(in-package #:kli/skills)

(defparameter +ignore-file-names+ '(".gitignore" ".ignore" ".fdignore")
  "Ignore files honored during skill discovery, matching pi.")

(defstruct (ignore-matcher (:constructor make-ignore-matcher ()))
  (rules '()))

(defstruct ignore-rule
  full-scanner
  prefix-scanner
  dir-only-p
  negated-p)

(defun prefix-ignore-line (line prefix)
  "Pi's per-directory pattern transform. Comments and blanks drop, the
negation marker is re-attached after prefixing, and a leading slash
anchors the pattern to its directory."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (when (or (zerop (length trimmed))
              (and (char= (char trimmed 0) #\#)
                   (not (uiop:string-prefix-p "\\#" trimmed))))
      (return-from prefix-ignore-line nil))
    (let ((pattern line)
          (negated nil))
      (cond
        ((uiop:string-prefix-p "!" pattern)
         (setf negated t
               pattern (subseq pattern 1)))
        ((uiop:string-prefix-p "\\!" pattern)
         (setf pattern (subseq pattern 1))))
      (let* ((anchored (uiop:string-prefix-p "/" pattern))
             (body (if anchored (subseq pattern 1) pattern))
             (prefix (or prefix ""))
             (prefixed (concatenate 'string prefix body))
             (final (if (and anchored (zerop (length prefix)))
                        (concatenate 'string "/" prefixed)
                        prefixed)))
        (if negated
            (concatenate 'string "!" final)
            final)))))

(defun strip-trailing-spaces (pattern)
  (let ((end (length pattern)))
    (loop while (and (plusp end)
                     (char= (char pattern (1- end)) #\Space)
                     (not (and (> end 1)
                               (char= (char pattern (- end 2)) #\\))))
          do (decf end))
    (subseq pattern 0 end)))

(defun write-quoted (string out)
  (write-string (cl-ppcre:quote-meta-chars string) out))

(defun glob-to-regex (glob)
  "One gitignore glob body as a cl-ppcre fragment. Stars stop at slashes,
double stars cross them, character classes pass through."
  (with-output-to-string (out)
    (let ((i 0)
          (n (length glob)))
      (loop
        while (< i n)
        do (let ((char (char glob i)))
             (cond
               ((and (char= char #\*)
                     (< (1+ i) n)
                     (char= (char glob (1+ i)) #\*))
                (let* ((j (loop for k from i below n
                                while (char= (char glob k) #\*)
                                finally (return k)))
                       (at-start (zerop i))
                       (before-slash (and (plusp i)
                                          (char= (char glob (1- i)) #\/)))
                       (after-slash (and (< j n)
                                         (char= (char glob j) #\/))))
                  (cond
                    ((and (or at-start before-slash) after-slash)
                     (write-string "(?:[^/]+/)*" out)
                     (setf i (1+ j)))
                    ((= j n)
                     (write-string ".*" out)
                     (setf i j))
                    (t
                     (write-string "[^/]*" out)
                     (setf i j)))))
               ((char= char #\*)
                (write-string "[^/]*" out)
                (incf i))
               ((char= char #\?)
                (write-string "[^/]" out)
                (incf i))
               ((char= char #\[)
                (let ((close (position #\] glob :start (1+ i))))
                  (if close
                      (progn
                        (write-string (subseq glob i (1+ close)) out)
                        (setf i (1+ close)))
                      (progn
                        (write-string "\\[" out)
                        (incf i)))))
               ((char= char #\\)
                (if (< (1+ i) n)
                    (progn
                      (write-quoted (string (char glob (1+ i))) out)
                      (incf i 2))
                    (progn
                      (write-string "\\\\" out)
                      (incf i))))
               (t
                (write-quoted (string char) out)
                (incf i))))))))

(defun compile-ignore-line (line)
  "An ignore-rule for a prefixed pattern line, or NIL when it compiles to
nothing. A pattern without a slash matches its basename at any depth."
  (let ((pattern line)
        (negated nil))
    (when (uiop:string-prefix-p "!" pattern)
      (setf negated t
            pattern (subseq pattern 1)))
    (setf pattern (strip-trailing-spaces pattern))
    (let* ((length (length pattern))
           (dir-only (and (plusp length)
                          (char= (char pattern (1- length)) #\/)))
           (body (if dir-only (subseq pattern 0 (1- length)) pattern))
           (anchored (uiop:string-prefix-p "/" body))
           (body (if anchored (subseq body 1) body)))
      (when (zerop (length body))
        (return-from compile-ignore-line nil))
      (let ((base (concatenate 'string
                               (if (or anchored (find #\/ body))
                                   ""
                                   "(?:[^/]+/)*")
                               (glob-to-regex body))))
        (make-ignore-rule
         :full-scanner (cl-ppcre:create-scanner
                        (format nil "^(?:~A)$" base))
         :prefix-scanner (cl-ppcre:create-scanner
                          (format nil "^(?:~A)/" base))
         :dir-only-p dir-only
         :negated-p negated)))))

(defun add-ignore-lines (matcher lines &key (prefix ""))
  "Append LINES as rules, each prefixed the way pi prefixes patterns from
ignore files found PREFIX deep into the walk. Returns MATCHER."
  (dolist (line lines matcher)
    (let* ((prefixed (prefix-ignore-line line prefix))
           (rule (and prefixed (compile-ignore-line prefixed))))
      (when rule
        (setf (ignore-matcher-rules matcher)
              (nconc (ignore-matcher-rules matcher) (list rule)))))))

(defun directory-relative-prefix (dir root)
  (let ((dir-namestring (namestring (uiop:ensure-directory-pathname dir)))
        (root-namestring (namestring (uiop:ensure-directory-pathname root))))
    (if (and (< (length root-namestring) (length dir-namestring))
             (string= root-namestring dir-namestring
                      :end2 (length root-namestring)))
        (subseq dir-namestring (length root-namestring))
        "")))

(defparameter *ignore-file-byte-limit* (* 2 1024 1024)
  "Largest ignore file the skill walk reads whole, in bytes. A pathological
ignore file would otherwise be split into an unbounded number of rules.")

(defun ignore-file-within-limit-p (path)
  "True when PATH is at most *IGNORE-FILE-BYTE-LIMIT* bytes. An unreadable
file is treated as over the limit so it is skipped."
  (handler-case
      (<= (with-open-file (stream path :element-type '(unsigned-byte 8))
            (file-length stream))
          *ignore-file-byte-limit*)
    (error () nil)))

(defun add-ignore-rules (matcher dir root)
  "Read the ignore files in DIR and append their rules relative to ROOT.
Unreadable and oversized files are skipped. Returns MATCHER."
  (let ((prefix (directory-relative-prefix dir root)))
    (dolist (name +ignore-file-names+ matcher)
      (let ((path (merge-pathnames name
                                   (uiop:ensure-directory-pathname dir))))
        (when (and (uiop:file-exists-p path)
                   (ignore-file-within-limit-p path))
          (handler-case
              (add-ignore-lines matcher
                                (uiop:split-string
                                 (uiop:read-file-string path)
                                 :separator '(#\Newline))
                                :prefix prefix)
            (error () nil)))))))

(defun path-ignored-p (matcher path)
  "Decide PATH against the accumulated rules, last match winning. A
trailing slash marks PATH as a directory for directory-only patterns.
Anything below a matched directory is ignored regardless."
  (let* ((length (length path))
         (dir-p (and (plusp length) (char= (char path (1- length)) #\/)))
         (clean (if dir-p (subseq path 0 (1- length)) path))
         (verdict nil))
    (dolist (rule (ignore-matcher-rules matcher) verdict)
      (cond
        ((cl-ppcre:scan (ignore-rule-prefix-scanner rule) clean)
         (setf verdict (not (ignore-rule-negated-p rule))))
        ((and (cl-ppcre:scan (ignore-rule-full-scanner rule) clean)
              (or (not (ignore-rule-dir-only-p rule)) dir-p))
         (setf verdict (not (ignore-rule-negated-p rule))))))))
