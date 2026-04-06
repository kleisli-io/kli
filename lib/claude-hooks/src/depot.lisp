;;;; claude-hooks - Git Root Detection
;;;;
;;;; Find git repository root for consistent session state paths.

(in-package :claude-hooks)

;;; ---------------------------------------------------------------------------
;;; Git Root Detection
;;; ---------------------------------------------------------------------------

(defun find-git-root ()
  "Find git repository root via git rev-parse.
   Returns absolute path string or NIL if not in a git repo."
  (let ((result (ignore-errors
                  (uiop:run-program '("git" "rev-parse" "--show-toplevel")
                                    :output :string
                                    :ignore-error-status t))))
    (when (and result (not (string= result "")))
      (string-trim '(#\Newline #\Return #\Space) result))))
