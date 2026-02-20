;;;; package.lisp - Unified hook package for kli
;;;;
;;;; Single package for all Claude Code hook handlers. Each handler is a
;;;; plain defun that takes an INPUT hash-table and returns a hash-table
;;;; or nil. The dispatcher (dispatch.lisp) manages stdin/stdout/exit.
;;;;
;;;; :use claude-hooks for response builders, JSON utils, file I/O
;;;; :use claude-session for session file utilities
;;;; :use playbook-hooks for domain detection and co-application mining
;;;; Qualify depot: calls (collides with claude-hooks on find-depot-root etc.)

(defpackage #:kli-hook
  (:use #:cl #:claude-hooks #:claude-session #:playbook-hooks)
  (:export #:hook-main
           #:*task-mcp-port*))
