(in-package #:task)

;;; ==========================================================================
;;; Global State
;;; ==========================================================================

(defvar *tasks-root* nil
  "Root directory for task storage. Set by detect-tasks-root.")

;;; ==========================================================================
;;; Legacy Qualified ID Compat
;;; ==========================================================================

(defun strip-depot-prefix (id)
  "Strip legacy depot:prefix from task ID if present.
   Used to handle old events.jsonl entries that contain qualified IDs
   like \"kleisli:2026-02-10-task\"."
  (let ((pos (position #\: id)))
    (if pos
        (subseq id (1+ pos))
        id)))

;;; ==========================================================================
;;; Task Root Detection
;;; ==========================================================================

(defun detect-tasks-root ()
  "Detect tasks root directory.

   Resolution order:
   1. KLI_TASKS_DIR env (explicit override)
   2. ACE_TASKS_DIR env (backward compat)
   3. git-root/.kli/tasks/
   4. CWD/.kli/tasks/ (fallback)"
  (setf *tasks-root*
        (or (uiop:getenv "KLI_TASKS_DIR")
            (uiop:getenv "ACE_TASKS_DIR")
            (let ((git-root (ignore-errors
                              (funcall (find-symbol "FIND-GIT-ROOT" :depot)))))
              (when git-root
                (let ((candidate (format nil "~A/.kli/tasks/" git-root)))
                  (when (probe-file candidate)
                    candidate))))
            (let ((cwd (namestring (uiop:getcwd))))
              (let ((candidate (format nil "~A.kli/tasks/" cwd)))
                (when (probe-file candidate)
                  candidate)))
            (error "Cannot detect tasks root: set KLI_TASKS_DIR, or run 'kli init' in a git repository"))))

;;; ==========================================================================
;;; Task Paths
;;; ==========================================================================

(defun task-directory (task-id)
  "Return path to task directory."
  (format nil "~A~A/" *tasks-root* task-id))

(defun task-events-path (task-id)
  "Return path to task events.jsonl file."
  (format nil "~Aevents.jsonl" (task-directory task-id)))

(defun ensure-task-directory (task-id)
  "Create task directory and handoffs subdirectory if needed."
  (let ((dir (task-directory task-id)))
    (ensure-directories-exist (format nil "~Ahandoffs/" dir))
    dir))
