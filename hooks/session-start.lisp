;;;; session-start.lisp - SessionStart hook handler
;;;;
;;;; Provides:
;;;; 1. Stale PID cleanup (primary SessionEnd compensation)
;;;; 2. Session lock + active-PID tracking
;;;; 3. PID registration with task-mcp
;;;; 4. Git branch + short commit
;;;; 5. Parallel session detection
;;;; 6. Playbook session initialization (merged from playbook-session-start)
;;;;
;;;; Merged from: ace-session-start-v2/main.lisp + playbook-session-start.nix

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Path Helpers (session-specific, not in shared libraries)
;;; ---------------------------------------------------------------------------

(defun sessions-dir (coord-root)
  "Return .claude/sessions/ path under coordination root."
  (merge-pathnames ".claude/sessions/"
                   (uiop:ensure-directory-pathname coord-root)))

(defun session-lock-path (coord-root session-id)
  "Return .claude/sessions/{session-id}/session.lock path."
  (merge-pathnames (format nil "~A/session.lock" session-id)
                   (sessions-dir coord-root)))

(defun latest-session-path (coord-root)
  "Return .claude/latest-session path."
  (merge-pathnames ".claude/latest-session"
                   (uiop:ensure-directory-pathname coord-root)))

(defun active-pids-dir (coord-root)
  "Return .claude/active-pids/ path under coordination root."
  (merge-pathnames ".claude/active-pids/"
                   (uiop:ensure-directory-pathname coord-root)))

;;; ---------------------------------------------------------------------------
;;; Git State
;;; ---------------------------------------------------------------------------

(defun git-info ()
  "Return (values branch short-commit) from git."
  (flet ((git (&rest args)
           (handler-case
               (string-trim '(#\Newline #\Space)
                            (uiop:run-program (cons "git" args)
                                              :output '(:string :stripped t)
                                              :error-output nil))
             (error () ""))))
    (values (git "rev-parse" "--abbrev-ref" "HEAD")
            (git "rev-parse" "--short" "HEAD"))))

;;; ---------------------------------------------------------------------------
;;; Session Liveness Protocol
;;; ---------------------------------------------------------------------------

(defun pid-alive-p (pid)
  "Check if PID is alive via /proc/{pid}/status."
  (probe-file (format nil "/proc/~D/status" pid)))

(defun read-session-lock-pid (lock-path)
  "Read PID from a session.lock file. Returns integer or NIL."
  (handler-case
      (when (probe-file lock-path)
        (parse-integer
         (string-trim '(#\Newline #\Space)
                      (uiop:read-file-string lock-path))))
    (error () nil)))

(defun read-active-session-for-pid (coord-root pid)
  "Read which session is currently active for PID."
  (handler-case
      (let ((path (merge-pathnames (format nil "~D" pid)
                                   (active-pids-dir coord-root))))
        (when (probe-file path)
          (string-trim '(#\Newline #\Space)
                       (uiop:read-file-string path))))
    (error () nil)))

(defun session-live-p (coord-root session-name pid)
  "Check if a session is live using two-file liveness protocol."
  (and (pid-alive-p pid)
       (string= session-name
                (read-active-session-for-pid coord-root pid))))

;;; ---------------------------------------------------------------------------
;;; Stale PID Cleanup (Primary SessionEnd Compensation)
;;; ---------------------------------------------------------------------------

(defun cleanup-stale-pids (coord-root)
  "Remove active-pids/ entries and session files for dead PIDs.
   Called at startup to clean up after crashed sessions."
  (let ((dir (active-pids-dir coord-root))
        (sessions (sessions-dir coord-root)))
    (handler-case
        (when (uiop:directory-exists-p dir)
          (dolist (file (uiop:directory-files dir))
            (let* ((name (pathname-name file))
                   (pid (handler-case (parse-integer name)
                          (error () nil))))
              (when (and pid (not (pid-alive-p pid)))
                (delete-file file)
                (let ((session-file (merge-pathnames
                                     (format nil "claude-~D.json" pid)
                                     sessions)))
                  (when (probe-file session-file)
                    (ignore-errors (delete-file session-file))))))))
      (error () nil))))

;;; ---------------------------------------------------------------------------
;;; Parallel Session Detection
;;; ---------------------------------------------------------------------------

(defun count-parallel-sessions (coord-root current-session-id)
  "Count live parallel sessions (excluding current)."
  (let ((sdir (sessions-dir coord-root))
        (count 0))
    (handler-case
        (when (uiop:directory-exists-p sdir)
          (dolist (entry (uiop:subdirectories sdir))
            (let* ((name (first (last (pathname-directory entry))))
                   (lock (merge-pathnames "session.lock" entry))
                   (pid (read-session-lock-pid lock)))
              (when (and pid
                         (not (string= name current-session-id))
                         (session-live-p coord-root name pid))
                (incf count)))))
      (error () nil))
    count))

;;; ---------------------------------------------------------------------------
;;; PID Registration with task-mcp
;;; ---------------------------------------------------------------------------

(defun register-pid-with-task-mcp (session-id pid)
  "Register PID with task-mcp via HTTP POST.
   Uses dexador instead of shelling to curl."
  (handler-case
      (let ((url (format nil "http://127.0.0.1:~D/register-pid?session-id=~A&pid=~D"
                         *task-mcp-port* session-id pid)))
        (multiple-value-bind (body status)
            (dex:post url :connect-timeout 2 :read-timeout 2)
          (declare (ignore body))
          (and status (< status 300))))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; Session Setup
;;; ---------------------------------------------------------------------------

(defun write-session-lock (coord-root session-id)
  "Write session.lock, active-pids/{PID}, and .claude/latest-session."
  (let* ((ppid (get-ppid))
         (lock (session-lock-path coord-root session-id))
         (latest (latest-session-path coord-root))
         (active-pid (merge-pathnames (format nil "~D" ppid)
                                      (active-pids-dir coord-root))))
    ;; session.lock -> PID
    (ensure-directories-exist lock)
    (with-open-file (s lock :direction :output :if-exists :supersede)
      (format s "~D" ppid))
    ;; active-pids/{PID} -> session-id
    (ensure-directories-exist active-pid)
    (with-open-file (s active-pid :direction :output :if-exists :supersede)
      (write-string session-id s))
    ;; latest-session
    (ensure-directories-exist latest)
    (with-open-file (s latest :direction :output :if-exists :supersede)
      (write-string session-id s))))

;;; ---------------------------------------------------------------------------
;;; Playbook Session Init (merged from playbook-session-start)
;;; ---------------------------------------------------------------------------

(defun playbook-session-context (input cwd)
  "Initialize playbook session and return context string or NIL.
   Merged from playbook-session-start hook."
  (let ((pb-dir (ensure-session-dir input "playbook")))
    (when (and pb-dir cwd)
      (let* ((state-path (format nil "~a/.claude/playbook-state.json" cwd))
             (state (read-json-file state-path)))
        (when (and state (hash-table-p state))
          (let ((sessions (gethash "sessions" state)))
            (when (and sessions (listp sessions) (> (length sessions) 0))
              (format nil "~d prior session~:p with playbook data"
                      (length sessions)))))))))

;;; ---------------------------------------------------------------------------
;;; Handler Entry Point
;;; ---------------------------------------------------------------------------

(defun session-start-handler (input)
  "SessionStart handler: git state + parallel sessions + playbook init."
  (let ((session-id (jref input "session_id"))
        (cwd (jref input "cwd")))
    ;; Validate
    (unless (and session-id (plusp (length session-id))
                 cwd (plusp (length cwd)))
      (return-from session-start-handler (empty-response)))

    (let ((coord-root (depot:coordination-root-from cwd)))
      (unless coord-root
        (return-from session-start-handler (empty-response)))

      ;; Clean up stale PIDs from crashed sessions
      (cleanup-stale-pids coord-root)

      ;; Write session lock + active-pids + latest-session
      (write-session-lock coord-root session-id)

      ;; Register PID with task-mcp
      (register-pid-with-task-mcp session-id (get-ppid))

      ;; Gather context
      (multiple-value-bind (branch commit) (git-info)
        (let ((parallel (count-parallel-sessions coord-root session-id))
              (playbook-ctx (playbook-session-context input cwd)))
          ;; Compose output
          (session-start-context
           (with-output-to-string (s)
             (format s "# SessionStart: git state, parallel sessions, ACE tasks~%~%")
             (format s "SESSION[1]{id,git_branch,git_commit}:~% ~A,~A,~A~%~%"
                     session-id branch commit)
             (when (and parallel (> parallel 0))
               (format s "PARALLEL[1]{count,warning}:~% ~D,Another session active~%~%"
                       parallel))
             (format s "Use `task_bootstrap(task_id)` to load full task context.~%")
             (format s "Use `task_list()` to see all tasks.~%")
             (when playbook-ctx
               (format s "~%Playbook: ~a.~%" playbook-ctx)))))))))
