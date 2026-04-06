;;;; config.lisp - Shared configuration for all hook handlers

(in-package #:kli-hook)

(defvar *task-mcp-port*
  (or (parse-integer (or (uiop:getenv "TASK_MCP_PORT") "")
                     :junk-allowed t)
      8090)
  "HTTP port for unified task-mcp server (serves both task + playbook tools).
   Read from TASK_MCP_PORT env var.")
