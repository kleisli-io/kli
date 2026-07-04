(in-package #:kli/tools/filesystem)

(defstruct patch-op
  ":delete, or :replace. START-LINE is NIL for bare + or < (EOF/BOF).
  INDEX breaks position ties in patch order." 
  kind
  start-line
  start-hash
  end-line
  end-hash
  (payload '() :type list)
  (index 0 :type integer))

(defun parse-patch-anchor (text line)
  (flet ((bad ()
           (error "Cannot parse anchor ~S in ~S. Expected LINE:HH (e.g. 12:a3)."
                  text line)))
    (let* ((trimmed (string-right-trim "|" text))
           (colon (position #\: trimmed)))
      (unless colon (bad))
      (let ((digits (subseq trimmed 0 colon))
            (hash (subseq trimmed (1+ colon))))
        (unless (and (plusp (length digits))
                     (every #'digit-char-p digits)
                     (= 2 (length hash))
                     (every (lambda (ch) (find ch "0123456789abcdef")) hash))
          (bad))
        (values (parse-integer digits) hash)))))

(defun parse-patch-range (text line)
  (let ((dots (search ".." text)))
    (unless dots
      (error "Cannot parse range ~S in ~S. Expected A:HH..B:HH." text line))
    (multiple-value-bind (start-line start-hash)
        (parse-patch-anchor (subseq text 0 dots) line)
      (multiple-value-bind (end-line end-hash)
          (parse-patch-anchor (subseq text (+ dots 2)) line)
        (when (> start-line end-line)
          (error "Range ~S is reversed: start line ~D is after end line ~D."
                 text start-line end-line))
        (values start-line start-hash end-line end-hash)))))

(defun hashline-leading-whitespace-p (line)
  (and (plusp (length line))
       (member (char line 0) '(#\Space #\Tab))))

(defun hashline-marker-after-leading-whitespace (line)
  (when (hashline-leading-whitespace-p line)
    (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
      (cond
        ((zerop (length trimmed)) nil)
        ((and (>= (length trimmed) 2)
              (string= "@@" trimmed :end2 2))
         "@@")
        ((member (char trimmed 0) '(#\+ #\< #\- #\= #\~))
         (string (char trimmed 0)))))))

(defun hashline-payload-op-p (op)
  (and op (member (patch-op-kind op) '(:insert-after :insert-before :replace))))

(defun hashline-generic-parse-error (line)
  (error "Cannot parse line ~S. Expected \"@@ PATH\", \"+ [LINE:HH]\", \"< [LINE:HH]\", \"- A:HH..B:HH\", \"= A:HH..B:HH\", or \"~~payload\"."
         line))

(defun hashline-leading-whitespace-error (line marker)
  (error "Cannot parse line ~S. Hashline markers such as ~S must start in column 1; remove the leading whitespace. If this line is intended as file content, prefix it with ~S because payload lines use ~S."
         line marker "~" "~content"))

(defun hashline-missing-payload-marker-error (line)
  (error "Cannot parse line ~S as payload. Payload lines must start with ~S; prefix this literal line with ~S."
         line "~" "~"))

(defun parse-hashline-patch (input)
  "Parse INPUT into a list of (PATH . OPS) sections of patch-op structs.

Grammar:
  @@ PATH          section header (one or more sections per patch)
  + LINE:HH        insert payload AFTER anchored line
  +                insert payload at EOF (bare +)
  < LINE:HH        insert payload BEFORE anchored line
  <                insert payload at BOF (bare <)
  - A:HH..B:HH     delete inclusive range
  = A:HH..B:HH     replace range with payload (no payload = delete)
  ~content         payload line, everything after ~ is verbatim

Anchors are validated against disk content re-hashed at edit time. The
anchor cache only enforces read-before-edit (it holds what the model
last saw, so it cannot detect external drift). Any problem rejects the
whole patch before any file is written."
  (let ((sections '())
        (op-count 0))
    (flet ((add-op (op)
             (push op (cdr (first sections))))
           (last-op ()
             (first (cdr (first sections)))))
      (loop
        for line across (split-file-lines input)
        do (cond
             ((and (>= (length line) 3) (string= "@@ " line :end2 3))
              (let ((path (string-trim " " (subseq line 3))))
                (when (zerop (length path))
                  (error "Section header ~S names no path." line))
                (push (cons path '()) sections)))
             ((every (lambda (ch) (member ch '(#\Space #\Tab))) line))
             ((hashline-marker-after-leading-whitespace line)
              (hashline-leading-whitespace-error
               line (hashline-marker-after-leading-whitespace line)))
             ((null sections)
              (error "Patch must start with \"@@ PATH\", got ~S." line))
             ((char= (char line 0) #\~)
              (let ((op (last-op)))
                (unless op
                  (error "Payload line ~S has no preceding op." line))
                (when (eq (patch-op-kind op) :delete)
                  (error "Delete ops take no payload (line ~S). Use = to replace."
                         line))
                (push (subseq line 1) (patch-op-payload op))))
             ((member (char line 0) '(#\+ #\<))
              (let ((kind (if (char= (char line 0) #\+)
                              :insert-after
                              :insert-before))
                    (rest (string-trim " " (subseq line 1))))
                (if (zerop (length rest))
                    (add-op (make-patch-op :kind kind :index (incf op-count)))
                    (multiple-value-bind (anchor-line hash)
                        (parse-patch-anchor rest line)
                      (add-op (make-patch-op :kind kind
                                             :start-line anchor-line
                                             :start-hash hash
                                             :index (incf op-count)))))))
             ((member (char line 0) '(#\- #\=))
              (let ((kind (if (char= (char line 0) #\-) :delete :replace))
                    (rest (string-trim " " (subseq line 1))))
                (multiple-value-bind (a ah b bh)
                    (parse-patch-range rest line)
                  (add-op (make-patch-op :kind kind
                                         :start-line a :start-hash ah
                                         :end-line b :end-hash bh
                                         :index (incf op-count))))))
             ((hashline-payload-op-p (last-op))
              (hashline-missing-payload-marker-error line))
             (t
              (hashline-generic-parse-error line)))))
    (let ((ordered (nreverse sections)))
      (dolist (section ordered ordered)
        (setf (cdr section) (nreverse (cdr section)))
        (dolist (op (cdr section))
          (setf (patch-op-payload op) (nreverse (patch-op-payload op))))))))

(defstruct file-edit
  "Ops for one file. PATH is as named in the patch and echoed in
results, KEY its truename namestring, OLD-LINES a simple-vector."
  path
  key
  old-content
  old-lines
  ops)

(defun collect-file-edits (protocol sections)
  "Merge SECTIONS by truename and enforce read-before-edit.
Returns (values file-edits problems). An unread file contributes a
single problem and its anchors are not checked further."
  (let ((edits '())
        (problems '())
        (cache (anchor-cache protocol)))
    (dolist (section sections)
      (destructuring-bind (path . ops) section
        (handler-case
            (let ((key (anchor-cache-key path)))
              (if (not (nth-value 1 (gethash key cache)))
                  (push (format nil "~A has not been read this session. Read it first to obtain anchors."
                                path)
                        problems)
                  (let ((existing (find key edits
                                        :key #'file-edit-key
                                        :test #'string=)))
                    (if existing
                        (setf (file-edit-ops existing)
                              (append (file-edit-ops existing) ops))
                        (let ((content (read-file-string path)))
                          (push (make-file-edit :path path
                                                :key key
                                                :old-content content
                                                :old-lines (split-file-lines content)
                                                :ops ops)
                                edits))))))
          (file-error ()
            (push (format nil "~A does not exist." path) problems)))))
    (values (nreverse edits) (nreverse problems))))

(defun validate-file-edit (edit)
  "Problems for EDIT's ops, anchors re-hashed against its disk lines.
Twin references to one anchor (e.g. A:HH..A:HH) report once."
  (let* ((lines (file-edit-old-lines edit))
         (path (file-edit-path edit))
         (line-count (length lines))
         (problems '())
         (reported (make-hash-table :test #'equal)))
    (labels ((check-anchor (line hash role)
               (let ((key (list line hash)))
                 (unless (gethash key reported)
                   (cond ((not (<= 1 line line-count))
                          (setf (gethash key reported) t)
                          (push (format nil "Anchor ~D:~A references line ~D but ~A has only ~D line~:P. Re-read ~A and resend the patch."
                                        line hash line path line-count path)
                                problems))
                         ((not (string= hash (line-hash (svref lines (1- line)))))
                          (setf (gethash key reported) t)
                          (push (format nil "Anchor ~D:~A is stale (~A). Re-read ~A around line ~D and resend the patch."
                                        line hash role path line)
                                problems)))))))
      (dolist (op (file-edit-ops edit))
        (ecase (patch-op-kind op)
          ((:insert-after :insert-before)
           (let ((line (patch-op-start-line op)))
             (when line
               (check-anchor line (patch-op-start-hash op)
                             (if (eq (patch-op-kind op) :insert-after)
                                 "insert after"
                                 "insert before")))
             (unless (patch-op-payload op)
               (push (format nil "Insert op at ~A in ~A has no payload lines."
                             (cond (line (format nil "line ~D" line))
                                   ((eq (patch-op-kind op) :insert-after) "EOF")
                                   (t "BOF"))
                             path)
                     problems))))
          ((:delete :replace)
           (let ((role (if (eq (patch-op-kind op) :delete) "delete" "replace")))
             (check-anchor (patch-op-start-line op) (patch-op-start-hash op)
                           (format nil "~A start" role))
             (check-anchor (patch-op-end-line op) (patch-op-end-hash op)
                           (format nil "~A end" role))
             ;; A replace re-emits the whole line as payload; a line shown
             ;; truncated cannot be reconstructed, so its tail would be lost
             ;; silently. Delete/insert carry no such payload and stay free.
             (when (eq (patch-op-kind op) :replace)
               (loop for line from (patch-op-start-line op)
                       to (patch-op-end-line op)
                     when (and (<= 1 line line-count)
                               (> (length (svref lines (1- line)))
                                  *render-line-limit*))
                       do (push (format nil "Line ~D is ~D chars and was shown ~
truncated; replacing it would discard content you did not see. Rewrite the ~
file with the write tool instead."
                                        line (length (svref lines (1- line))))
                                problems)
                          (return)))))))
      (let ((ranges (loop for op in (file-edit-ops edit)
                          when (member (patch-op-kind op) '(:delete :replace))
                            collect (cons (patch-op-start-line op)
                                          (patch-op-end-line op)))))
        (loop for (range . rest) on ranges
              do (dolist (other rest)
                   (when (and (<= (car range) (cdr other))
                              (<= (car other) (cdr range)))
                     (push (format nil "Ops overlap on lines ~D..~D and ~D..~D in ~A. Split into separate edits."
                                   (car range) (cdr range)
                                   (car other) (cdr other) path)
                           problems))))
        (dolist (op (file-edit-ops edit))
          (let ((line (patch-op-start-line op)))
            (when (and line
                       (member (patch-op-kind op)
                               '(:insert-after :insert-before)))
              (dolist (range ranges)
                (when (<= (car range) line (cdr range))
                  (push (format nil "Insert at line ~D falls inside the ~D..~D range op in ~A. Split into separate edits."
                                line (car range) (cdr range) path)
                        problems))))))))
    (nreverse problems)))

(defun patch-op-sort-key (op line-count)
  "Insert-after N sits at N+1/2 and insert-before N at N-1/2, so one
sort orders all op kinds. Bare EOF/BOF resolve to the file bounds."
  (ecase (patch-op-kind op)
    (:insert-after (+ (or (patch-op-start-line op) line-count) 1/2))
    (:insert-before (- (or (patch-op-start-line op) 1) 1/2))
    ((:delete :replace) (patch-op-start-line op))))

(defun sort-patch-ops (ops line-count ascending)
  "Sort OPS by position. The index tiebreak keeps same-point inserts in
patch order whether ASCENDING or descending."
  (sort (copy-list ops)
        (lambda (a b)
          (let ((ka (patch-op-sort-key a line-count))
                (kb (patch-op-sort-key b line-count)))
            (if ascending
                (or (< ka kb)
                    (and (= ka kb)
                         (< (patch-op-index a) (patch-op-index b))))
                (or (> ka kb)
                    (and (= ka kb)
                         (> (patch-op-index a) (patch-op-index b)))))))))

(defun splice-lines (lines start-index remove-count payload)
  (concatenate 'simple-vector
               (subseq lines 0 start-index)
               (coerce payload 'simple-vector)
               (subseq lines (+ start-index remove-count))))

(defun patch-regions (ops line-count)
  "Changed regions as (START . COUNT) in post-edit numbering, ascending."
  (let ((shift 0)
        (regions '()))
    (dolist (op (sort-patch-ops ops line-count t) (nreverse regions))
      (let ((payload-length (length (patch-op-payload op))))
        (ecase (patch-op-kind op)
          (:insert-after
           (push (cons (+ (or (patch-op-start-line op) line-count) shift 1)
                       payload-length)
                 regions)
           (incf shift payload-length))
          (:insert-before
           (push (cons (+ (or (patch-op-start-line op) 1) shift)
                       payload-length)
                 regions)
           (incf shift payload-length))
          (:delete
           (decf shift (1+ (- (patch-op-end-line op)
                              (patch-op-start-line op)))))
          (:replace
           (let ((span (1+ (- (patch-op-end-line op)
                              (patch-op-start-line op)))))
             (when (plusp payload-length)
               (push (cons (+ (patch-op-start-line op) shift) payload-length)
                     regions))
             (incf shift (- payload-length span)))))))))

(defun apply-patch-ops (lines ops)
  "Apply OPS to LINES bottom-up so original positions stay valid.
Returns (values new-lines regions added removed)."
  (let ((line-count (length lines))
        (new-lines lines)
        (added 0)
        (removed 0))
    (dolist (op (sort-patch-ops ops line-count nil))
      (let ((payload (patch-op-payload op)))
        (ecase (patch-op-kind op)
          (:insert-after
           (incf added (length payload))
           (setf new-lines (splice-lines new-lines
                                         (or (patch-op-start-line op) line-count)
                                         0
                                         payload)))
          (:insert-before
           (incf added (length payload))
           (setf new-lines (splice-lines new-lines
                                         (1- (or (patch-op-start-line op) 1))
                                         0
                                         payload)))
          ((:delete :replace)
           (let ((span (1+ (- (patch-op-end-line op)
                              (patch-op-start-line op)))))
             (incf removed span)
             (incf added (length payload))
             (setf new-lines (splice-lines new-lines
                                           (1- (patch-op-start-line op))
                                           span
                                           payload)))))))
    (values new-lines (patch-regions ops line-count) added removed)))
