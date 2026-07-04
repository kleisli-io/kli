(in-package #:kli/tools/lisp)

(defun read-source-file (path)
  (with-open-file (in path :direction :input :external-format :utf-8)
    (with-output-to-string (out)
      (loop for ch = (read-char in nil nil) while ch do (write-char ch out)))))

(defun truthy-p (value)
  (and value (not (member value '(nil :false :no "false" "no" "0") :test #'equalp))))

(defun ws-node-p (node)
  "A whitespace, newline, or comment leaf -- skipped when counting significant
forms or finding a form's head."
  (and (leaf-p node)
       (member (tok-type (leaf-tok node)) '(:ws :newline :comment))))

(defun top-form-label (group)
  "GROUP's head atom and the name atom after it, e.g. \"defun foo\" -- or just the
head, or nil when the head is not an atom."
  (let* ((sig (remove-if #'ws-node-p (group-children group)))
         (head (first sig))
         (name (second sig)))
    (when (and head (leaf-p head))
      (let ((head-text (tok-text (leaf-tok head)))
            (name-text (and name (leaf-p name) (tok-text (leaf-tok name)))))
        (if name-text
            (format nil "~A ~A" head-text name-text)
            head-text)))))

(defun top-form-spans (roots)
  "(start end . label) per top-level group form in ROOTS, absolute char offsets
matching the span map the match positions are measured in."
  (let ((pos 0) (result '()))
    (dolist (node roots (nreverse result))
      (let ((len (length (flatten (list node)))))
        (when (group-p node)
          (push (list* pos (+ pos len) (top-form-label node)) result))
        (incf pos len)))))

(defun span-positions (source spans)
  "For each (start . end) match span, \"L:C (in LABEL)\" naming the enclosing
top-level form, or plain \"L:C\" when the match sits outside an atom-headed form."
  (let ((forms (top-form-spans (parse-faithful (tokenize source)))))
    (mapcar (lambda (s)
              (multiple-value-bind (line col) (source-position source (car s))
                (let ((label (loop for (start end . label) in forms
                                   when (and (<= start (car s)) (< (car s) end))
                                     return label)))
                  (if label
                      (format nil "~D:~D (in ~A)" line col label)
                      (format nil "~D:~D" line col)))))
            spans)))

(defun scope-note (within-type within-name)
  (if (or within-type within-name)
      (format nil " within ~@[~A~]~@[ ~A~]" within-type within-name)
      ""))

(defun read-failure-report (text)
  "TEXT's first read error as \"line L col C: MESSAGE\" (message's first line);
empty string when TEXT reads clean."
  (multiple-value-bind (condition offset) (read-failure text)
    (if condition
        (let* ((msg (princ-to-string condition))
               (first-line (subseq msg 0 (or (position #\Newline msg) (length msg)))))
          (multiple-value-bind (line col) (source-position text (or offset 0))
            (format nil "line ~D col ~D: ~A" line col first-line)))
        "")))

(defun source-syntax-report (text diagnostics)
  "First CST diagnostic as \"line L col C: MESSAGE\"; empty string when clean."
  (if diagnostics
      (let ((diagnostic (first diagnostics)))
        (multiple-value-bind (line col)
            (source-position text (cst-diagnostic-start diagnostic))
          (format nil "line ~D col ~D: ~A" line col
                  (cst-diagnostic-message diagnostic))))
      ""))

(defun assert-source-syntax (text label)
  "Reject TEXT when the Common Lisp source-syntax CST reports diagnostics."
  (let ((diagnostics (source-syntax-diagnostics text)))
    (when diagnostics
      (error "edit-sexp: ~A has invalid Common Lisp source syntax or unbalanced delimiters: ~A"
             label (source-syntax-report text diagnostics))))
  text)

(defun significant-root-count (text)
  "Top-level Common Lisp source forms TEXT holds, whitespace and comments ignored."
  (source-form-count text))

(defun repair-incoming (text label)
  "Auto-repair TEXT the way the file is repaired, then validate source syntax.
An empty TEXT (delete) passes through."
  (multiple-value-bind (fixed repaired-p) (repair-if-needed text)
    (declare (ignore repaired-p))
    (assert-source-syntax fixed label)))

(defun line-change-region (old-lines new-lines)
  "Compare line vectors. Returns (values start end added removed): 1-based START
of the first differing line, END the last changed line in NEW-LINES (END < START
when nothing was added), and ADDED/REMOVED changed-line counts."
  (let* ((ol (length old-lines))
         (nl (length new-lines))
         (common (min ol nl))
         (prefix (loop for i below common
                       while (string= (svref old-lines i) (svref new-lines i))
                       count t))
         (suffix (loop for i below (- common prefix)
                       while (string= (svref old-lines (- ol 1 i))
                                      (svref new-lines (- nl 1 i)))
                       count t)))
    (values (1+ prefix) (- nl suffix)
            (- nl suffix prefix) (- ol suffix prefix))))

(defparameter *edit-diff-line-cap* 40
  "Most removed+added lines edit-sexp renders as a unified diff in its visible
result. A larger change shows only the (+N -M) summary; the full before/after
still rides :details.")

(defun render-edit-diff (old-lines new-lines start end added removed)
  "A bounded unified diff of the changed region: removed (-) OLD lines, then
added (+) NEW lines that keep their LINE:HH anchors for follow-up edits. START is
the 1-based first changed line, shared by both sides since the common prefix is
byte-identical; END the inclusive last NEW line. Returns NIL when the change is
empty or exceeds *edit-diff-line-cap* lines, so the caller falls back to the bare
(+N -M) summary."
  (when (and (plusp (+ added removed))
             (<= (+ added removed) *edit-diff-line-cap*))
    (let ((rows '())
          (old-end (+ start removed -1)))
      (loop for index from (1- start) below old-end
            do (push (format nil "- ~A" (render-truncate-front (svref old-lines index)))
                     rows))
      (loop for index from (1- start) below end
            for line = (svref new-lines index)
            do (push (format nil "+ ~D:~A ~A" (1+ index) (line-hash line)
                             (render-truncate-front line))
                     rows))
      (format nil "~{~A~^~%~}" (nreverse rows)))))

(defun splice-new-spans (edits new-len)
  "Map each working-coord (START . END) in EDITS to its NEW-SOURCE-coord span of
the spliced NEW text (length NEW-LEN), accounting for the cumulative length shift
of earlier edits. Document order."
  (let ((delta 0) (result '()))
    (dolist (e (sort (copy-list edits) #'< :key #'car) (nreverse result))
      (let ((start (+ (car e) delta)))
        (push (cons start (+ start new-len)) result)
        (incf delta (- new-len (- (cdr e) (car e))))))))

(defun continuation-ranges (source spans)
  "The continuation-line ranges of each splice (START . END) in SOURCE: a 1-based
inclusive (FIRST+1 . LAST) line range that excludes the splice's own first line,
whose leading indent is context-owned. Single-line splices contribute nothing."
  (loop for (s . e) in spans
        for first = (source-position source s)
        for last = (source-position source (1- e))
        when (> last first)
          collect (cons (1+ first) last)))

(defun reindent-splices (new-source edits new-repaired)
  "Reindent only the continuation lines of each splice of NEW-REPAIRED into
NEW-SOURCE, leaving every other byte (and each splice's first line) verbatim.
Returns NEW-SOURCE unchanged for a delete or a single-line splice."
  (if (plusp (length new-repaired))
      (let ((ranges (continuation-ranges
                     new-source
                     (splice-new-spans edits (length new-repaired)))))
        (if ranges (indent-region new-source :lines ranges) new-source))
      new-source))

(defun run-edit-sexp-tool (tool parameters context &key call-id on-update)
  "Structurally replace the s-expression SNODE= to match_form with new_form. An
unbalanced file is auto-repaired one-shot before the edit, with the repair diff
surfaced in this tool's own result; new_form's spliced continuation lines are
reindented to canonical columns, so new_form need not be pre-indented."
  (declare (ignore tool call-id on-update))
  (let* ((path (pathname (required-tool-parameter parameters :path)))
         (match (required-tool-parameter parameters :match_form))
         (new (required-tool-parameter parameters :new_form))
         (within-type (tool-parameter parameters :within_type))
         (within-name (tool-parameter parameters :within_name))
         (replace-all (truthy-p (tool-parameter parameters :replace_all)))
         (dry-run (truthy-p (tool-parameter parameters :dry_run))))
    (unless (cl-source-path-p path)
      (error "edit-sexp edits Common Lisp source (~{~A~^ ~}); ~A is not. Use edit."
             +cl-source-types+ path))
    (let* ((original (read-source-file path))
           (verdict (reader-verdict original)))
      (multiple-value-bind (working repaired-p) (repair-if-needed original)
        (progn
          (when (and (eq verdict :unbalanced) (not repaired-p))
            (let ((report (read-failure-report original)))
              (error "edit-sexp: ~A has unbalanced delimiters that could not be ~
auto-repaired.~@[ ~A~] Balance it, then retry." path
                     (and (plusp (length report)) report))))
          (assert-source-syntax working (namestring path)))
        (let ((new-repaired (repair-incoming new "new_form")))
          (let ((roots (significant-root-count new-repaired)))
            (when (> roots 1)
              (error "edit-sexp: new_form must be a single s-expression (got ~D); ~
to splice several, wrap them or use edit." roots)))
          (multiple-value-bind (new-source status info)
              (replace-source-form working
                                   (repair-incoming match "match_form")
                                   new-repaired
                                   :within-type within-type
                                   :within-name within-name
                                   :replace-all replace-all)
            (ecase status
              (:syntax-error
               (error "edit-sexp: source syntax validation failed: ~A"
                      (source-syntax-report working info)))
              (:bad-match
               (error "edit-sexp: match_form must be exactly one s-expression."))
              (:not-found
               (error "edit-sexp: no s-expression matches match_form~A in ~A."
                      (scope-note within-type within-name) path))
              (:ambiguous
               (error "edit-sexp: match_form matches ~D s-expressions in ~A (~{~A~^, ~}). ~
Pass replace_all true to change all, or scope with within_type/within_name."
                      (length info) path (span-positions working info)))
              (:within-not-found
               (error "edit-sexp: no top-level form matches~A in ~A."
                      (scope-note within-type within-name) path))
              (:within-ambiguous
               (error "edit-sexp: within_type/within_name matches ~D top-level forms in ~A (~{~A~^, ~}). Narrow it."
                      (length info) path (span-positions working info)))
              (:ok
               (let* ((final (assert-source-syntax
                              (reindent-splices new-source info new-repaired)
                              "final output"))
                      (old-lines (split-file-lines working))
                      (new-lines (split-file-lines final)))
                 (unless dry-run
                   (overwrite-file path final)
                   (remember-read (active-protocol context) path new-lines))
                 (multiple-value-bind (start end added removed)
                     (line-change-region old-lines new-lines)
                   (tool-text-result
                    (with-output-to-string (out)
                      (when repaired-p
                        (if dry-run
                            (format out "Note: the file has unbalanced delimiters; a ~
real edit would auto-repair it first.~%")
                            (format out "~A~%" (repair-note path original working))))
                      (format out "~:[Edited~;Dry run, nothing written:~] ~A (+~D -~D)"
                              dry-run path added removed)
                      (let ((diff (render-edit-diff old-lines new-lines
                                                    start end added removed)))
                        (cond
                          (diff
                           (terpri out)
                           (write-string diff out))
                          ((> (+ added removed) *edit-diff-line-cap*)
                           (format out "~%(diff omitted: +~D -~D lines exceed the ~
~D-line cap, so no line anchors are shown; re-read the file to refresh them)"
                                   added removed *edit-diff-line-cap*)))))
                    :details (if dry-run
                                 (list :dry-run-p t
                                       :path (namestring path)
                                       :preview-old original
                                       :preview-new final
                                       :added added
                                       :removed removed)
                                 (list :files
                                       (list (list :path (namestring path)
                                                   :old original
                                                   :new final
                                                   :added added
                                                   :removed removed)))))))))))))))
