(in-package #:kli/prompts)

(defstruct (prompt-template (:constructor %make-prompt-template))
  name
  description
  argument-hint
  body
  path)

(defparameter +description-limit+ 60
  "Longest derived description before truncation, matching pi.")

(defun derive-description (frontmatter body)
  (let ((explicit (gethash "description" frontmatter)))
    (if (and explicit (plusp (length explicit)))
        explicit
        (let ((line (find-if (lambda (line)
                               (plusp (length (string-trim '(#\Space #\Tab)
                                                           line))))
                             (uiop:split-string body
                                                :separator '(#\Newline)))))
          (cond
            ((null line) "")
            ((> (length line) +description-limit+)
             (concatenate 'string (subseq line 0 +description-limit+) "..."))
            (t line))))))

(defparameter *prompt-template-file-byte-limit* (* 2 1024 1024)
  "Largest prompt template the prompts extension reads whole, in bytes. A
template body flows verbatim into the model context, so one oversized file at
discovery can exhaust the dynamic space.")

(defun prompt-template-file-byte-length (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun assert-prompt-template-file-within-limit (path)
  (let ((size (prompt-template-file-byte-length path)))
    (when (> size *prompt-template-file-byte-limit*)
      (error "~A is ~:D bytes, over the ~:D byte limit for a prompt template."
             path size *prompt-template-file-byte-limit*))))

(defun load-prompt-template (path)
  "Read PATH as a prompt template. The filename minus .md names it, the
description falls back to the first non-empty body line. Fail-soft: an
unreadable or oversized file yields NIL."
  (handler-case
      (multiple-value-bind (frontmatter body)
          (progn
            (assert-prompt-template-file-within-limit path)
            (parse-frontmatter (uiop:read-file-string path)))
        (%make-prompt-template
         :name (pathname-name path)
         :description (derive-description frontmatter body)
         :argument-hint (gethash "argument-hint" frontmatter)
         :body body
         :path path))
    (error () nil)))

(defun template-files (directory)
  (handler-case
      (sort (uiop:directory-files directory "*.md")
            #'string< :key #'namestring)
    (error () nil)))

(defun discover-prompt-templates (directories)
  "Templates from each directory's *.md files, sorted by filename within a
directory with directory order preserved. Non-recursive, matching pi."
  (loop for directory in directories
        append (loop for file in (template-files directory)
                     for template = (load-prompt-template file)
                     when template collect template)))
