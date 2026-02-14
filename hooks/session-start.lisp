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
;;; Build-id (runtime lookup — kli package compiled after hooks)
;;; ---------------------------------------------------------------------------

(defun get-build-id ()
  "Get build-id from KLI package if loaded, else return unknown."
  (let ((pkg (find-package "KLI")))
    (if pkg
        (let ((sym (find-symbol "ENSURE-BUILD-ID" pkg)))
          (if (and sym (fboundp sym))
              (funcall sym)
              "unknown"))
        "unknown")))

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
;;; Daemon Lifecycle (ensure task-mcp HTTP daemon is running)
;;; ---------------------------------------------------------------------------

(defun task-mcp-health (&optional (port *task-mcp-port*))
  "Check daemon health and build version.
   Returns (values alive-p build-id).
   ALIVE-P is T if daemon responds on /health with 2xx.
   BUILD-ID is the daemon's build identifier string, or NIL."
  (handler-case
      (multiple-value-bind (body status)
          (dex:get (format nil "http://127.0.0.1:~D/health" port)
                   :connect-timeout 1 :read-timeout 1)
        (if (and status (< status 300))
            (values t (ignore-errors
                       (let ((json (yason:parse body)))
                         (when (hash-table-p json)
                           (gethash "build_id" json)))))
            (values nil nil)))
    (error () (values nil nil))))

(defun task-mcp-alive-p (&optional (port *task-mcp-port*))
  "Check if task-mcp HTTP daemon is alive via /health endpoint."
  (task-mcp-health port))

(defun request-daemon-shutdown (&optional (port *task-mcp-port*))
  "Request old daemon to shut down gracefully.
   Returns T if shutdown was acknowledged."
  (handler-case
      (multiple-value-bind (body status)
          (dex:post (format nil "http://127.0.0.1:~D/shutdown?build-id=~A"
                            port (get-build-id))
                    :connect-timeout 2 :read-timeout 2)
        (declare (ignore body))
        (and status (< status 300)))
    (error () nil)))

(defun wait-for-port-release (&optional (port *task-mcp-port*) (attempts 25))
  "Wait for port to become available (old daemon exited).
   Polls every 200ms, up to ATTEMPTS times (default 5s).
   Returns T if port was released."
  (loop repeat attempts
        unless (task-mcp-alive-p port) return t
        do (sleep 0.2)))

(defun start-task-mcp-daemon (&optional (port *task-mcp-port*))
  "Launch kli serve --task in background and wait for health.
   Returns T if daemon started successfully."
  (let ((log-file "/tmp/task-mcp-daemon.log"))
    (handler-case
        (progn
          (uiop:launch-program
           (list "kli" "serve" "--task")
           :output log-file :error-output log-file
           :if-output-exists :append
           :element-type 'character)
          ;; Wait for startup (up to 2 seconds)
          (loop repeat 10
                when (task-mcp-alive-p port) return t
                do (sleep 0.2))
          (task-mcp-alive-p port))
      (error () nil))))

(defun ensure-task-mcp-daemon (&optional (port *task-mcp-port*))
  "Start or replace task-mcp daemon with current build.
   Handles three cases:
   1. Daemon alive with matching build-id → reuse
   2. Daemon alive with different build-id → graceful takeover
   3. No daemon running → start fresh
   Returns T if daemon is available after this call."
  (get-build-id)
  (multiple-value-bind (alive-p daemon-build-id) (task-mcp-health port)
    (cond
      ;; Same build — reuse
      ((and alive-p daemon-build-id (string= daemon-build-id (get-build-id)))
       t)
      ;; Different build — takeover
      (alive-p
       (request-daemon-shutdown port)
       (unless (wait-for-port-release port)
         (return-from ensure-task-mcp-daemon nil))
       (start-task-mcp-daemon port))
      ;; Not running — start fresh
      (t
       (start-task-mcp-daemon port)))))

;;; ---------------------------------------------------------------------------
;;; PID Registration with task-mcp
;;; ---------------------------------------------------------------------------

(defun agent-team-env ()
  "Read Claude Code agent team env vars. Returns plist or NIL.
   Claude Code sets these on teammate processes after parsing CLI args
   (--agent-name, --team-name, --agent-type)."
  (let ((team-name (uiop:getenv "CLAUDE_CODE_TEAM_NAME"))
        (agent-name (uiop:getenv "CLAUDE_CODE_AGENT_NAME"))
        (agent-type (uiop:getenv "CLAUDE_CODE_AGENT_TYPE")))
    (when (and team-name (plusp (length team-name)))
      (list :team-name team-name
            :agent-name (when (and agent-name (plusp (length agent-name))) agent-name)
            :agent-type (when (and agent-type (plusp (length agent-type))) agent-type)))))

(defun register-pid-with-task-mcp (session-id pid &optional team-env)
  "Register PID with task-mcp via HTTP POST.
   When TEAM-ENV is non-nil, includes team metadata as query params."
  (handler-case
      (let ((base-url (format nil "http://127.0.0.1:~D/register-pid?session-id=~A&pid=~D"
                              *task-mcp-port* session-id pid)))
        (let ((url (if team-env
                       (format nil "~A&team-name=~A~@[&agent-name=~A~]~@[&agent-type=~A~]"
                               base-url
                               (getf team-env :team-name)
                               (getf team-env :agent-name)
                               (getf team-env :agent-type))
                       base-url)))
          (multiple-value-bind (body status)
              (dex:post url :connect-timeout 2 :read-timeout 2)
            (declare (ignore body))
            (and status (< status 300)))))
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

      ;; Ensure task-mcp HTTP daemon is running (starts if needed)
      (ensure-task-mcp-daemon)

      ;; Clean up stale PIDs from crashed sessions
      (cleanup-stale-pids coord-root)

      ;; Write session lock + active-pids + latest-session
      (write-session-lock coord-root session-id)

      ;; Detect agent team membership from env vars
      (let ((team-env (agent-team-env)))

        ;; Register PID with task-mcp (includes team metadata if present)
        (register-pid-with-task-mcp session-id (get-ppid) team-env)

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
               (when team-env
                 (format s "TEAM[1]{team_name,agent_name,agent_type}:~% ~A,~A,~A~%~%"
                         (getf team-env :team-name)
                         (or (getf team-env :agent-name) "")
                         (or (getf team-env :agent-type) "")))
               (when (and parallel (> parallel 0))
                 (format s "PARALLEL[1]{count,warning}:~% ~D,Another session active~%~%"
                         parallel))
               (format s "Use `task_bootstrap(task_id)` to load full task context.~%")
               (format s "Use `task_list()` to see all tasks.~%")
               (when playbook-ctx
                 (format s "~%Playbook: ~a.~%" playbook-ctx))))))))))
