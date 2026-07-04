(in-package #:kli/tests)
(in-suite all)

(defun pr-test-path (name)
  (make-pathname :directory '(:absolute "tmp")
                 :name (format nil "kli-pr-test-~D-~A"
                               #+sbcl (sb-posix:getpid) #-sbcl 0 name)
                 :type "lisp"))

(defun pr-disk-content (path)
  (with-open-file (in path :external-format :utf-8)
    (let* ((buf (make-string (file-length in)))
           (n (read-sequence buf in)))
      (subseq buf 0 n))))

(defun pr-result-text (result)
  (apply #'concatenate 'string
         (loop for item in (ext:tool-result-content result)
               collect (or (getf item :text) ""))))


;;; ---- edit-sexp ----

(defun put-file (path content)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string content out)))

(defun install-edit-sexp-tools (context &rest extra)
  (apply #'install-extensions context
         event:*events-extension-manifest*
         tools-lisp:*edit-sexp-extension-manifest*
         extra))

(defun edit-sexp (protocol context path &rest params)
  (ext:invoke-tool protocol :edit-sexp
                   (list* :path (namestring path) params) context))

(test (edit-sexp-replaces-a-unique-form :fixture tool-authority)
  "A unique surface-structural match is spliced; untouched bytes are preserved
and the changed region's anchors are armed."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-unique")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path (format nil "(defun f ()~%  (+ 1 2))~%"))
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(+ 1 2)" :new_form "(* 3 4)"))
                  (disk (pr-disk-content path))
                  (text (pr-result-text result)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "(defun f ()~%  (* 3 4))~%") disk)
                 "the match is spliced byte-exact")
             (is (search "Edited" text))
             (is (eq :valid (tools-filesystem:anchor-known-p
                             protocol path 2
                             (tools-filesystem:line-hash "  (* 3 4))")))
                 "the changed region's anchors are armed for follow-up edits")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-rejects-an-ambiguous-match :fixture tool-authority)
  "More than one match without replace_all rejects, listing every position."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-ambiguous")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(list (a) (a))")
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(a)" :new_form "Z"))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "2 s-expressions" text) "the count is reported")
             (is (search "1:7" text) "the first position is listed")
             (is (search "1:11" text) "the second position is listed")
             (is (string= "(list (a) (a))" (pr-disk-content path))
                 "a rejected edit leaves the file untouched")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-replace-all :fixture tool-authority)
  "replace_all splices every match."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-replace-all")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(list (a) (a))")
           (let ((result (edit-sexp protocol context path
                                    :match_form "(a)" :new_form "Z"
                                    :replace_all t)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= "(list Z Z)" (pr-disk-content path)))))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-scopes-to-one-clos-method :fixture tool-authority)
  "within_type/within_name scope the edit to one CLOS method by its specializer."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-within"))
         (src "(defmethod area ((s square))
  (* (side s) (side s)))

(defmethod area ((s circle))
  (* pi (r s) (r s)))
"))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path src)
           (let ((result (edit-sexp protocol context path
                                    :match_form "(r s)" :new_form "RR"
                                    :within_type "defmethod"
                                    :within_name "area ((s circle))"
                                    :replace_all t)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= "(defmethod area ((s square))
  (* (side s) (side s)))

(defmethod area ((s circle))
  (* pi RR RR))
"
                          (pr-disk-content path))
                 "only the circle method changed, byte-exact elsewhere")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-deletes-with-a-seam :fixture tool-authority)
  "An empty new_form deletes the match and one adjacent separator."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-delete")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(a b c)")
           (let ((result (edit-sexp protocol context path
                                    :match_form "b" :new_form "")))
             (is (not (ext:tool-result-error-p result)))
             (is (string= "(a c)" (pr-disk-content path)))))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-repairs-an-unbalanced-file-then-edits :fixture tool-authority)
  "An unbalanced target file is auto-repaired one-shot, with the repair diff
surfaced, then the edit is applied."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-repair")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path (format nil "(defun f ()~%  (+ 1 2)"))   ; missing closer
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(+ 1 2)" :new_form "(* 3 4)"))
                  (disk (pr-disk-content path))
                  (text (pr-result-text result)))
             (is (not (ext:tool-result-error-p result)))
             (is (paren-repair:balanced-p disk) "the file is balanced after repair")
             (is (search "(* 3 4)" disk) "the edit was applied")
             (is (search "paren-repair" text) "the repair diff is surfaced")
             (is (search "Edited" text))))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-repairs-an-unbalanced-new-form :fixture tool-authority)
  "An unbalanced-but-fixable new_form is repaired before the splice, so the file
stays balanced."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-new-imbalance")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(defun f () (+ 1 2))")
           (let ((result (edit-sexp protocol context path
                                    :match_form "(+ 1 2)"
                                    :new_form "(* 3 4")))   ; missing closer
             (is (not (ext:tool-result-error-p result)))
             (is (paren-repair:balanced-p (pr-disk-content path)))
             (is (search "(* 3 4)" (pr-disk-content path)))))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-rejects-an-unreadable-new-form :fixture tool-authority)
      "A new_form with malformed source syntax is rejected before any write."
      (let* ((context (kli:make-kernel-host))
             (protocol (switch-to-extension-protocol context))
             (path (pr-test-path "edit-sexp-unreadable-new")))
        (unwind-protect
            (progn
              (install-edit-sexp-tools context)
              (put-file path "(defun f () (+ 1 2))")
              (let* ((result (edit-sexp protocol context path
                                        :match_form "(+ 1 2)"
                                        :new_form "#<unreadable>"))
                     (text (pr-result-text result)))
                (is (ext:tool-result-error-p result))
                (is (search "source syntax" text)
                    "the message names source syntax rather than reader/package evaluation")
                (is (not (search "does not read" text)))
                (is (string= "(defun f () (+ 1 2))" (pr-disk-content path))
                    "a rejected edit leaves the file untouched")))
          (ignore-errors (delete-file path)))))

(test (edit-sexp-allows-package-local-nickname-in-new-form :fixture tool-authority)
  "A package error from the scratch reader does not reject a structurally valid
new_form; target files may rely on package-local nicknames."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-local-nickname")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(defun f () (old))")
           (let ((result (edit-sexp protocol context path
                                    :match_form "(old)"
                                    :new_form "(cairn::cairn-extra-messages context)")))
             (is (not (ext:tool-result-error-p result)))
             (is (search "cairn::cairn-extra-messages" (pr-disk-content path)))))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-accepts-reader-syntax-match-and-new-form :fixture tool-authority)
  "Reader-prefix and dispatch forms are single source forms for matching and replacement."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-reader-syntax")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(defun f () (list '(a b) #P\"/tmp\"))")
           (let ((result (edit-sexp protocol context path
                                    :match_form "'(a b)"
                                    :new_form "#C(1 2)")))
             (is (not (ext:tool-result-error-p result)))
             (is (string= "(defun f () (list #C(1 2) #P\"/tmp\"))"
                          (pr-disk-content path))))
           (let ((result (edit-sexp protocol context path
                                    :match_form "#P\"/tmp\""
                                    :new_form "'(x y)")))
             (is (not (ext:tool-result-error-p result)))
             (is (string= "(defun f () (list #C(1 2) '(x y)))"
                          (pr-disk-content path)))))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-rejects-source-invalid-final-output :fixture tool-authority)
  "A malformed final source buffer is rejected before write in real and dry-run modes."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-final-syntax"))
         (src "(defun f () #'foo)"))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path src)
           (let* ((result (edit-sexp protocol context path
                                     :match_form "foo"
                                     :new_form ")"))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "final output" text))
             (is (search "source syntax" text))
             (is (string= src (pr-disk-content path))))
           (let* ((result (edit-sexp protocol context path
                                     :match_form "foo"
                                     :new_form ")"
                                     :dry_run t))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "final output" text))
             (is (search "source syntax" text))
             (is (string= src (pr-disk-content path)))))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-rejects-package-error-plus-delimiter-imbalance :fixture tool-authority)
  "Package-sensitive source with delimiter imbalance is rejected without writing."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-package-plus-imbalance"))
         (src "(defun f () (cairn::x"))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path src)
           (let* ((result (edit-sexp protocol context path
                                     :match_form "cairn::x"
                                     :new_form "ok"))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "source syntax" text))
             (is (string= src (pr-disk-content path)))))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-rejects-non-cl-source :fixture tool-authority)
  "A non-Common-Lisp file is refused with guidance to use edit."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (make-pathname :directory '(:absolute "tmp")
                              :name (format nil "kli-pr-test-~D-edit-sexp-boundary"
                                            #+sbcl (sb-posix:getpid) #-sbcl 0)
                              :type "txt")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(a b c)")
           (let* ((result (edit-sexp protocol context path
                                     :match_form "b" :new_form "x"))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "edit" text) "the message points at the text edit tool")
             (is (string= "(a b c)" (pr-disk-content path))
                 "the file is untouched")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-reindents-a-multiline-new-form :fixture tool-authority)
  "A dedented multi-line new_form is reindented to canonical columns in place, so
the model need not pre-indent it."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-reindent")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path (format nil "(defun foo ()~%  (old))~%"))
           (let ((result (edit-sexp protocol context path
                                    :match_form "(old)"
                                    :new_form (format nil "(let ((x 1))~%(when x~%(bar x)))"))))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "(defun foo ()~%  (let ((x 1))~%    (when x~%      (bar x))))~%")
                          (pr-disk-content path))
                 "the spliced continuation lines are canonically indented")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-reindent-preserves-siblings :fixture tool-authority)
  "Reindent touches only a splice's continuation lines: the splice's first line
keeps its context indent and sibling forms stay byte-exact."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-reindent-siblings")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path (format nil "(defun a () 1)~%(defun b ()~%  (c))~%"))
           (let ((result (edit-sexp protocol context path
                                    :match_form "(c)"
                                    :new_form (format nil "(let ((y 2))~%(g y))"))))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "(defun a () 1)~%(defun b ()~%  (let ((y 2))~%    (g y)))~%")
                          (pr-disk-content path))
                 "only b's body was reindented; (defun a () 1) is byte-exact")))
      (ignore-errors (delete-file path)))))


(test (edit-sexp-shows-a-unified-diff :fixture tool-authority)
  "A replace that deletes lines renders a unified diff in the visible result --
removed (-) old lines and added (+) new lines that keep their LINE:HH anchors --
so the change is legible without mining :details."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-diff")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path (format nil "(defun f (xs)~%  (let ((acc nil))~%    ~
(dolist (x xs)~%      (push x acc))~%    acc))~%"))
           (let* ((result (edit-sexp protocol context path
                                     :match_form
                                     "(let ((acc nil)) (dolist (x xs) (push x acc)) acc)"
                                     :new_form "(loop for x in xs collect x)"))
                  (text (pr-result-text result)))
             (is (not (ext:tool-result-error-p result)))
             (is (search "(+1 -4)" text) "the summary counts added and removed")
             (is (search (format nil "- ~A" "  (let ((acc nil))") text)
                 "a removed line is shown with a - marker")
             (is (search "(push x acc))" text)
                 "the rest of the removed body is shown")
             (is (search "+ 2:" text)
                 "the added line keeps its LINE:HH anchor under a + marker")
             (is (search "(loop for x in xs collect x)" text)
                 "the added line's content is shown")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-rejects-a-multi-form-new-form :fixture tool-authority)
  "new_form must be a single s-expression: several top-level forms are rejected
with the count, and the file is left untouched."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-multi-new")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(defun f () (+ 1 2))")
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(+ 1 2)"
                                     :new_form "(foo) (bar)"))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "single s-expression" text) "the message names the rule")
             (is (search "got 2" text) "the form count is reported")
             (is (string= "(defun f () (+ 1 2))" (pr-disk-content path))
                 "a rejected edit leaves the file untouched")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-dry-run-previews-without-writing :fixture tool-authority)
  "dry_run previews the splice -- the unified diff and the resulting content -- but
writes nothing and arms no anchors, so the file is byte-identical and the would-be
edited line is still unread."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-dry-run"))
         (src (format nil "(defun f ()~%  (+ 1 2))~%")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path src)
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(+ 1 2)" :new_form "(* 3 4)"
                                     :dry_run t))
                  (details (ext:tool-result-details result))
                  (text (pr-result-text result)))
             (is (not (ext:tool-result-error-p result)))
             (is (getf details :dry-run-p) "the result is marked a dry run")
             (is (search "Dry run" text) "the text says nothing was written")
             (is (search "(* 3 4)" text) "the diff shows the spliced line")
             (is (search "(* 3 4)" (getf details :preview-new))
                 "the preview content carries the spliced result")
             (is (string= src (pr-disk-content path))
                 "the file on disk is byte-identical")
             (is (eq :unread (tools-filesystem:anchor-known-p
                              protocol path 2
                              (tools-filesystem:line-hash "  (* 3 4))")))
                 "no anchor was armed for the previewed line")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-ambiguity-names-enclosing-forms :fixture tool-authority)
  "An ambiguous match names each match's enclosing top-level form -- head and name
-- so the model can pick a concrete within_name instead of re-reading by position."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-ambiguity-context")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path (format nil "(defun foo ()~%  (probe))~%~%~
(defun bar ()~%  (probe))~%"))
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(probe)" :new_form "Z"))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "2 s-expressions" text) "the count is reported")
             (is (search "(in defun foo)" text) "the first match names its form")
             (is (search "(in defun bar)" text) "the second match names its form")
             (is (string= (format nil "(defun foo ()~%  (probe))~%~%~
(defun bar ()~%  (probe))~%")
                          (pr-disk-content path))
                 "a rejected edit leaves the file untouched")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-unbalanced-unrepairable-reports-position :fixture tool-authority)
  "An unbalanced match_form that can't be auto-repaired is rejected with the
line:col of the failure, the way the indeterminate path already reports, and the
file is left untouched."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-unbalanced-pos")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path "(defun f () (+ 1 2))")
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(list :grad-3,-4 1))"
                                     :new_form "Z"))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "unbalanced delimiters" text) "the message names the cause")
             (is (search "col " text) "and carries a line:col position")
             (is (string= "(defun f () (+ 1 2))" (pr-disk-content path))
                 "a rejected edit leaves the file untouched")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-large-change-omits-diff-with-notice :fixture tool-authority)
  "A change past the diff line cap shows the (+N -M) summary plus a notice that the
diff and its anchors are omitted -- distinct from an empty change -- so the model
re-reads rather than reusing stale anchors."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-large")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path (format nil "(defun f ()~%  (old))~%"))
           (let* ((body (with-output-to-string (s)
                          (write-string "(progn" s)
                          (dotimes (i 60) (format s "~%(a~D)" i))
                          (write-string ")" s)))
                  (result (edit-sexp protocol context path
                                     :match_form "(old)" :new_form body))
                  (text (pr-result-text result)))
             (is (not (ext:tool-result-error-p result)))
             (is (search "diff omitted" text)
                 "the over-cap change names why the diff is suppressed")
             (is (search "re-read" text)
                 "and tells the model to re-read for fresh anchors")
             (is (not (search (format nil "~%+ ") text))
                 "no added-line anchors are shown over the cap")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-within-not-found-names-the-scope :fixture tool-authority)
  "A within_type/within_name that matches no top-level form is rejected naming the
searched scope, not the literal parameter tokens."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-within-missing")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path (format nil "(defun f ()~%  (+ 1 2))~%"))
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(+ 1 2)" :new_form "(* 3 4)"
                                     :within_type "defmethod"
                                     :within_name "nonesuch"))
                  (text (pr-result-text result)))
             (is (ext:tool-result-error-p result))
             (is (search "within defmethod nonesuch" text)
                 "the message names the searched scope")
             (is (not (search "within_type/within_name" text))
                 "not the literal parameter tokens")
             (is (string= (format nil "(defun f ()~%  (+ 1 2))~%")
                          (pr-disk-content path))
                 "a rejected edit leaves the file untouched")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-dry-run-on-unbalanced-file-is-future-tense :fixture tool-authority)
  "A dry_run on an unbalanced file announces that a real edit would auto-repair it
-- future tense -- rather than the past-tense repair note that implies the disk
already changed, and writes nothing."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-dry-unbalanced"))
         (src (format nil "(defun f ()~%  (+ 1 2)")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path src)
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(+ 1 2)" :new_form "(* 3 4)"
                                     :dry_run t))
                  (text (pr-result-text result)))
             (is (not (ext:tool-result-error-p result)))
             (is (search "would auto-repair" text)
                 "the dry-run notice is future tense")
             (is (not (search "paren-repair: balanced" text))
                 "not the past-tense repair note that implies the disk changed")
             (is (string= src (pr-disk-content path))
                 "the file on disk is byte-identical")))
      (ignore-errors (delete-file path)))))

(test (edit-sexp-repaired-file-edit-count-excludes-repair :fixture tool-authority)
  "When the target file is auto-repaired before the edit, the (+N -M) summary
measures the edit against the repaired text, so the repair's own change is not
double-counted -- the repair is announced separately by its note."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (pr-test-path "edit-sexp-repaired-count"))
         (src (format nil "(defun a (x)~%  (* x~%     x)~%(defun b ()~%  (target))~%")))
    (unwind-protect
         (progn
           (install-edit-sexp-tools context)
           (put-file path src)
           (let* ((result (edit-sexp protocol context path
                                     :match_form "(target)" :new_form "(replaced)"))
                  (text (pr-result-text result)))
             (is (not (ext:tool-result-error-p result)))
             (is (search "(+1 -1)" text)
                 "the summary counts only the edit, not the repair's added closer")
             (is (search "(replaced)" (pr-disk-content path))
                 "the edit applied")))
      (ignore-errors (delete-file path)))))
