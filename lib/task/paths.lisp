(in-package #:task)

;;; ==========================================================================
;;; Global State
;;; ==========================================================================

(defvar *tasks-root* nil
  "Root directory for task storage. Set by detect-tasks-root.
   Used for single-depot mode and as default for unqualified IDs.")

(defvar *current-depot* nil
  "Current depot name for operations. NIL means use default depot.")

;;; ==========================================================================
;;; Qualified Task ID Functions
;;; ==========================================================================

(defun qualify-task-id (depot task-id)
  "Create a qualified task ID from depot name and task ID.
   Format: depot:task-id (e.g., core:2026-02-01-research)

   Arguments:
     depot   - Depot name string (e.g., \"core\", \"amti\")
     task-id - Task ID string (e.g., \"2026-02-01-research\")

   Returns:
     Qualified ID string."
  (format nil "~A:~A" depot task-id))

(defun parse-qualified-id (id)
  "Parse a qualified task ID into depot and task-id components.
   Format: depot:task-id

   If ID is unqualified (no colon), returns \"default\" as depot.

   Arguments:
     id - Task ID string, qualified or unqualified

   Returns:
     Two values: (depot-name task-id)"
  (let ((pos (position #\: id)))
    (if pos
        (values (subseq id 0 pos)
                (subseq id (1+ pos)))
        (values "default" id))))

(defun qualified-id-p (id)
  "Check if ID is a qualified task ID (contains depot prefix).

   Arguments:
     id - Task ID string

   Returns:
     T if qualified (contains colon), NIL otherwise."
  (and (stringp id)
       (position #\: id)
       t))

(defun detect-tasks-root ()
  "Detect tasks root directory.

   Resolution order:
   1. KLI_TASKS_DIR env (explicit override for kli standalone)
   2. ACE_TASKS_DIR env (backward compat with depot-of-depots)
   3. DEPOT_ROOT/.kli/tasks/ or DEPOT_ROOT/ace/tasks/ (.kli preferred)
   4. $GIT_ROOT/.kli/tasks/ (kli standalone default)
   5. CWD/.kli/tasks/ or CWD/ace/tasks/ (legacy fallback, .kli preferred)"
  (setf *tasks-root*
        (or (uiop:getenv "KLI_TASKS_DIR")
            (uiop:getenv "ACE_TASKS_DIR")
            (let ((depot (uiop:getenv "DEPOT_ROOT")))
              (when depot
                (let ((kli-candidate (format nil "~A/.kli/tasks/" depot)))
                  (if (probe-file kli-candidate)
                      kli-candidate
                      (let ((ace-candidate (format nil "~A/ace/tasks/" depot)))
                        (when (probe-file ace-candidate)
                          ace-candidate))))))
            ;; kli standalone: use git root + .kli/tasks/
            (let ((git-root (ignore-errors
                              (funcall (find-symbol "FIND-GIT-ROOT" :depot)))))
              (when git-root
                (let ((candidate (format nil "~A/.kli/tasks/" git-root)))
                  (when (probe-file candidate)
                    candidate))))
            ;; Legacy fallback: .kli/tasks/ or ace/tasks/ from CWD
            (let ((cwd (namestring (uiop:getcwd))))
              (let ((kli-candidate (format nil "~A.kli/tasks/" cwd)))
                (if (probe-file kli-candidate)
                    kli-candidate
                    (let ((ace-candidate (format nil "~Aace/tasks/" cwd)))
                      (when (probe-file ace-candidate)
                        ace-candidate)))))
            (error "Cannot detect tasks root: set KLI_TASKS_DIR, or run 'kli init' in a git repository"))))

(defvar *depot-tasks-roots* (make-hash-table :test 'equal)
  "Map of depot-name -> tasks-root-path for multi-depot support.
   Populated by detect-all-task-roots.")

;;; ==========================================================================
;;; Multi-Depot Task Root Detection
;;; ==========================================================================

(defun detect-all-task-roots ()
  "Detect task roots from all depots in the world.

   Uses depot:list-depots-with-tasks to find all depots that have ace/tasks/
   directories and populates *depot-tasks-roots* with the mappings.

   Also sets *tasks-root* to the current depot's task root for backward
   compatibility.

   Guards against missing :depot package (e.g., Swank/dev environments) and
   never clears *depot-tasks-roots* unless new data is ready to replace it.

   Returns:
     The *depot-tasks-roots* hash-table."
  (let ((depot-pkg (find-package :depot)))
    (if depot-pkg
        ;; Multi-depot mode: build into temp table, swap on success
        (let ((depot-fn (find-symbol "LIST-DEPOTS-WITH-TASKS" depot-pkg)))
          (if depot-fn
              (let ((new-roots (make-hash-table :test 'equal))
                    (depots-with-tasks (funcall depot-fn)))
                (dolist (entry depots-with-tasks)
                  (let ((depot-name (getf entry :depot))
                        (tasks-root (getf entry :tasks-root)))
                    (setf (gethash depot-name new-roots) tasks-root)))
                ;; Only replace when we have results
                (when (plusp (hash-table-count new-roots))
                  (clrhash *depot-tasks-roots*)
                  (maphash (lambda (k v)
                             (setf (gethash k *depot-tasks-roots*) v))
                           new-roots))
                ;; Set *tasks-root* and *current-depot* for backward compat
                (let* ((current-depot-fn (find-symbol "FIND-DEPOT-ROOT" depot-pkg))
                       (current-root (when current-depot-fn (funcall current-depot-fn))))
                  (when current-root
                    (let ((depot-name (car (last (pathname-directory
                                                  (pathname current-root))))))
                      (setf *current-depot* depot-name)
                      (setf *tasks-root* (gethash depot-name *depot-tasks-roots*))))))
              ;; Symbol not found in depot package — single depot fallback
              (when (zerop (hash-table-count *depot-tasks-roots*))
                (detect-tasks-root)
                (setf (gethash "default" *depot-tasks-roots*) *tasks-root*))))
        ;; No :depot package — single depot fallback
        (when (zerop (hash-table-count *depot-tasks-roots*))
          (detect-tasks-root)
          (setf (gethash "default" *depot-tasks-roots*) *tasks-root*))))
  *depot-tasks-roots*)

(defun task-directory (task-id)
  "Return path to task directory.

   Supports both qualified (depot:task-id) and unqualified (task-id) formats.
   - Qualified: looks up depot in *depot-tasks-roots*
   - Unqualified: uses *tasks-root* (backward compatible)

   Arguments:
     task-id - Task ID string, qualified or unqualified

   Returns:
     Path to task directory as string."
  (if (qualified-id-p task-id)
      ;; Qualified ID: use depot-specific root
      (multiple-value-bind (depot bare-id) (parse-qualified-id task-id)
        (let ((tasks-root (gethash depot *depot-tasks-roots*)))
          (if tasks-root
              (format nil "~A~A/" tasks-root bare-id)
              ;; Fallback: try to resolve via depot library if available
              (let ((depot-root (ignore-errors
                                  (funcall (find-symbol "DEPOT-TASKS-ROOT" :depot) depot))))
                (if depot-root
                    (format nil "~A~A/" depot-root bare-id)
                    (error "Unknown depot ~A in qualified ID ~A" depot task-id))))))
      ;; Unqualified ID: use default *tasks-root*
      (format nil "~A~A/" *tasks-root* task-id)))

(defun task-events-path (task-id)
  "Return path to task events.jsonl file."
  (format nil "~Aevents.jsonl" (task-directory task-id)))

(defun ensure-task-directory (task-id)
  "Create task directory and handoffs subdirectory if needed."
  (let ((dir (task-directory task-id)))
    (ensure-directories-exist (format nil "~Ahandoffs/" dir))
    dir))
