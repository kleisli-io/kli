;;; kli — CLI dispatch entry point
;;;
;;; The kli binary bundles both task-mcp and playbook-mcp servers into a
;;; single SBCL image. Claude Code invokes it as an MCP server; the CLI
;;; selects which server to run.

(in-package #:kli)

(defvar *version* "0.1.0")

(defun print-help ()
  (format t "kli ~A — event-sourced task graphs for Claude Code~%~%" *version*)
  (format t "Usage: kli <command> [options]~%~%")
  (format t "Commands:~%")
  (format t "  serve [--task|--playbook]  Start MCP server (default: task)~%")
  (format t "  dashboard                  Start task graph dashboard~%")
  (format t "  hook <name>                Run Claude Code hook handler~%")
  (format t "  version                    Show version~%")
  (format t "  help                       Show this help~%")
  (format t "~%Environment:~%")
  (format t "  KLI_TASKS_DIR              Task storage (default: .kli/tasks/)~%")
  (format t "  PLAYBOOK_PATHS             Colon-separated playbook file paths~%")
  (format t "  OLLAMA_HOST                Ollama endpoint (default: http://localhost:11434)~%")
  (format t "  TASK_MCP_TRANSPORT         'http' for HTTP mode, else stdio~%"))

(defun main ()
  "Top-level entry point for the kli binary.
   Dispatches to task-mcp or playbook-mcp based on command-line arguments."
  (let* ((args (uiop:command-line-arguments))
         (command (first args)))
    (handler-case
        (cond
          ((or (null command) (string= command "help") (string= command "--help"))
           (print-help)
           (uiop:quit 0))
          ((string= command "version")
           (format t "kli ~A~%" *version*)
           (uiop:quit 0))
          ((string= command "serve")
           (let ((mode (second args)))
             (cond
               ((or (null mode) (string= mode "--task"))
                (task-mcp:main))
               ((string= mode "--playbook")
                (playbook-mcp:main))
               (t
                (format *error-output* "Unknown serve mode: ~A~%Try: kli help~%" mode)
                (uiop:quit 1)))))
          ((string= command "dashboard")
           (kli-dashboard:start)
           (format t "~&Press Ctrl-C to stop.~%")
           (handler-case (loop (sleep 60))
             (#+sbcl sb-sys:interactive-interrupt ()
               (kli-dashboard:stop))))
          ((string= command "hook")
           (let ((name (second args)))
             (if name
                 (progn (kli-hook:hook-main name) (uiop:quit 0))
                 (progn (format *error-output* "Usage: kli hook <name>~%") (uiop:quit 1)))))
          (t
           (format *error-output* "Unknown command: ~A~%Try: kli help~%" command)
           (uiop:quit 1)))
      (error (e)
        (format *error-output* "Fatal error: ~A~%" e)
        (uiop:quit 1)))))
