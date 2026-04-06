;;;; discovery.lisp - Git root detection and coordination path utilities.

(in-package #:depot)

;;; ==========================================================================
;;; Utility Functions
;;; ==========================================================================

(defun strip-trailing-slash (path)
  "Remove trailing slash from path string if present."
  (if (and (> (length path) 1)
           (char= (char path (1- (length path))) #\/))
      (subseq path 0 (1- (length path)))
      path))

;;; ==========================================================================
;;; Git Detection
;;; ==========================================================================

(defun find-git-root ()
  "Find the git root directory from current working directory.
   Returns the git root path as a string, or NIL if not in a git repository."
  (let ((result (ignore-errors
                  (uiop:run-program '("git" "rev-parse" "--show-toplevel")
                                    :output :string
                                    :ignore-error-status t))))
    (when (and result (not (string= result "")))
      (string-trim '(#\Newline #\Return #\Space) result))))

(defun find-git-root-from (path)
  "Find git root by walking up from PATH looking for .git directory.
   Like find-git-root but starts from specified PATH instead of getcwd.
   Used by hooks which receive cwd as a parameter."
  (when path
    (loop with current = (uiop:ensure-directory-pathname path)
          while (and current
                     (not (equal (namestring current) "/"))
                     (not (equal current (user-homedir-pathname))))
          do (when (probe-file (merge-pathnames ".git" current))
               (return-from find-git-root-from
                 (strip-trailing-slash (namestring (truename current)))))
             (setf current (uiop:pathname-parent-directory-pathname current)))))

;;; ==========================================================================
;;; Coordination Root
;;; ==========================================================================

(defun coordination-root ()
  "Return the root directory for cross-session coordination files.

   Resolution order:
   1. KLI_COORDINATION_DIR env (explicit override)
   2. Git root
   3. CWD (fallback)

   This is used for:
     - .claude/active-pids/  (live Claude process tracking)
     - .claude/sessions/     (session state files)"
  (or (let ((override (uiop:getenv "KLI_COORDINATION_DIR")))
        (when (and override (not (string= override "")))
          override))
      (find-git-root)
      (namestring (uiop:getcwd))))

(defun coordination-root-from (path)
  "Return coordination root by walking up from PATH.
   This is the path-aware variant for hooks."
  (or (find-git-root-from path)
      (let ((override (uiop:getenv "KLI_COORDINATION_DIR")))
        (when (and override (not (string= override "")))
          override))))
