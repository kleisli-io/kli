;;;; session.lisp - Claude Code session utilities
;;;;
;;;; Provides utilities for Claude Code hooks to find and read session files.
;;;; These are Claude-specific (PID lookup, session file format) and don't
;;;; belong in the generic depot library.

(in-package #:claude-session)

;;; ==========================================================================
;;; PID Utilities
;;; ==========================================================================

(defun get-ppid ()
  "Get parent PID of current process.
   Returns integer PID, or NIL on non-SBCL implementations."
  #+sbcl (sb-posix:getppid)
  #-sbcl nil)

(defun read-active-pids (coord-root)
  "Read all PIDs from .claude/active-pids/ directory.

   Arguments:
     coord-root - Coordination root path (world or depot root)

   Returns:
     List of PID integers, or NIL if directory doesn't exist."
  (when coord-root
    (let ((active-dir (merge-pathnames ".claude/active-pids/"
                                       (uiop:ensure-directory-pathname coord-root)))
          (pids nil))
      (when (probe-file active-dir)
        (dolist (file (ignore-errors (directory (merge-pathnames "*" active-dir))))
          (let ((pid (ignore-errors (parse-integer (pathname-name file)))))
            (when pid (push pid pids)))))
      pids)))

(defun get-claude-pid (coord-root)
  "Get Claude Code's PID for session file lookup.

   Uses .claude/active-pids/ as source of truth (written by SessionStart hook).

   Algorithm:
     - Single PID in active-pids: use it directly
     - Multiple PIDs: prefer PPID if in list, else most recent by mtime
     - No active PIDs: fall back to PPID

   Arguments:
     coord-root - Coordination root path

   Returns:
     Integer PID, or NIL if cannot determine."
  (let ((active-pids (read-active-pids coord-root)))
    (cond
      ;; Single active session - use that PID
      ((= (length active-pids) 1)
       (first active-pids))
      ;; Multiple sessions - try PPID, check if it's in active list
      ((> (length active-pids) 1)
       (let ((ppid (get-ppid)))
         (if (member ppid active-pids)
             ppid
             ;; PPID not in list (intermediate shell) - use most recent by file mtime
             ;; but only consider PIDs that have a session file (avoids poison PIDs
             ;; that registered via SessionStart but never called task_set_current)
             (let ((active-dir (merge-pathnames ".claude/active-pids/"
                                                (uiop:ensure-directory-pathname coord-root)))
                   (sessions-dir (merge-pathnames ".claude/sessions/"
                                                  (uiop:ensure-directory-pathname coord-root))))
               (let ((sorted (sort (copy-list active-pids) #'>
                                   :key (lambda (p)
                                          (or (ignore-errors
                                               (file-write-date
                                                (merge-pathnames (format nil "~D" p) active-dir)))
                                              0)))))
                 (or (find-if (lambda (pid)
                                (probe-file (merge-pathnames
                                             (format nil "claude-~A.json" pid)
                                             sessions-dir)))
                              sorted)
                     (first sorted)))))))
      ;; No active PIDs - fall back to PPID
      (t (get-ppid)))))

;;; ==========================================================================
;;; Path Builders
;;; ==========================================================================

(defun session-file-path (coord-root &optional pid)
  "Build path to session file: {coord-root}/.claude/sessions/claude-{pid}.json

   Arguments:
     coord-root - Coordination root path
     pid - Optional PID (defaults to get-claude-pid result)

   Returns:
     Pathname object, or NIL if PID cannot be determined."
  (when coord-root
    (let ((claude-pid (or pid (get-claude-pid coord-root))))
      (when claude-pid
        (merge-pathnames (format nil ".claude/sessions/claude-~A.json" claude-pid)
                         (uiop:ensure-directory-pathname coord-root))))))

(defun active-pid-path (coord-root pid)
  "Build path to active-pid file: {coord-root}/.claude/active-pids/{pid}

   Arguments:
     coord-root - Coordination root path
     pid - Process ID

   Returns:
     Pathname object."
  (when (and coord-root pid)
    (merge-pathnames (format nil ".claude/active-pids/~A" pid)
                     (uiop:ensure-directory-pathname coord-root))))

;;; ==========================================================================
;;; Session File I/O
;;; ==========================================================================

(defun read-session-file (coord-root &optional pid)
  "Read and parse session JSON file.

   Arguments:
     coord-root - Coordination root path
     pid - Optional PID (defaults to get-claude-pid result)

   Returns:
     Hash-table of session data, or NIL if file not found or parse error."
  (let ((path (session-file-path coord-root pid)))
    (when (and path (probe-file path))
      (handler-case
          (with-open-file (s path)
            (yason:parse s))
        (error () nil)))))

(defun session-has-task-p (coord-root &optional pid)
  "Check if session has active task context.

   Arguments:
     coord-root - Coordination root path
     pid - Optional PID

   Returns:
     T if session has non-empty task_id, NIL otherwise."
  (let ((session (read-session-file coord-root pid)))
    (when session
      (let ((task-id (gethash "task_id" session)))
        (and task-id
             (stringp task-id)
             (> (length task-id) 0))))))

;;; ==========================================================================
;;; Session File Write (Hook-Side)
;;; ==========================================================================

(defun write-session-task-file (coord-root task-id session-id events-path)
  "Write claude-{PPID}.json with task context.

   Called from PostToolUse hooks after task_set_current/task_bootstrap.
   Uses get-ppid for filename — kernel-guaranteed correct PID of parent
   Claude Code process, unlike daemon-side write which relies on the
   racy *latest-registered-pid* global.

   Arguments:
     coord-root  - Coordination root (world or depot root)
     task-id     - Qualified task ID (e.g. 'core:my-task')
     session-id  - Task-mcp session ID (hex string from tool response)
     events-path - Path to task's events.jsonl

   Returns:
     Pathname of written session file, or NIL on failure."
  (handler-case
      (let* ((ppid (get-ppid))
             (sessions-dir (merge-pathnames
                            ".claude/sessions/"
                            (uiop:ensure-directory-pathname coord-root)))
             (session-file (merge-pathnames
                            (format nil "claude-~A.json" ppid)
                            sessions-dir))
             (ht (make-hash-table :test #'equal)))
        (setf (gethash "task_id" ht) task-id)
        (setf (gethash "session_id" ht) session-id)
        (setf (gethash "events_path" ht) events-path)
        (setf (gethash "claude_pid" ht) ppid)
        (setf (gethash "timestamp" ht) (get-universal-time))
        ;; Atomic write: temp file + rename
        (ensure-directories-exist session-file)
        (let ((temp-file (merge-pathnames
                          (format nil "claude-~A.json.tmp" ppid)
                          sessions-dir)))
          (with-open-file (s temp-file :direction :output :if-exists :supersede)
            (yason:encode ht s))
          (rename-file temp-file session-file))
        session-file)
    (error () nil)))

(defun delete-session-task-file (coord-root)
  "Delete claude-{PPID}.json session file.

   Called from PostToolUse hooks after task_release.
   Removes session file so PreToolUse hook blocks file operations again.

   Arguments:
     coord-root - Coordination root (world or depot root)

   Returns:
     T if file was deleted, NIL if not found or on error."
  (handler-case
      (let* ((ppid (get-ppid))
             (session-file (merge-pathnames
                            (format nil ".claude/sessions/claude-~A.json" ppid)
                            (uiop:ensure-directory-pathname coord-root))))
        (when (probe-file session-file)
          (delete-file session-file)
          t))
    (error () nil)))
