;;;; claude-hooks - File I/O Utilities
;;;;
;;;; Common file operations for hooks managing session state.

(in-package :claude-hooks)

(defun append-line (path line)
  "Append LINE to file at PATH, creating parent directories as needed."
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (write-line line s)))

(defun append-line-unique (path line)
  "Append LINE to file at PATH only if not already present.
Creates the file if it does not exist."
  (let ((existing (read-lines path)))
    (unless (member line existing :test #'string=)
      (append-line path line))))

(defun read-lines (path)
  "Read all lines from file at PATH. Returns a list of strings.
Returns NIL if the file does not exist."
  (when (probe-file path)
    (with-open-file (s path :direction :input)
      (loop for line = (read-line s nil)
            while line
            collect line))))

(defun read-json-file (path &optional default)
  "Parse JSON from file at PATH. Returns DEFAULT if file is missing or parse fails."
  (if (probe-file path)
      (handler-case
          (with-open-file (s path :direction :input)
            (yason:parse s))
        (error () default))
      default))

(defun atomic-write-json (path value)
  "Write VALUE as JSON to PATH atomically via tmp file + rename.
Creates parent directories as needed."
  (ensure-directories-exist path)
  (let ((tmp (format nil "~a.tmp.~a" path (random 1000000))))
    (with-open-file (s tmp :direction :output :if-exists :supersede)
      (yason:encode value s))
    (rename-file tmp path)))
