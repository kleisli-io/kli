;;;; package.lisp - Claude session utilities package definition

(defpackage #:claude-session
  (:use #:cl)
  (:export
   ;; PID utilities
   #:get-ppid
   #:read-active-pids
   #:get-claude-pid
   ;; Path builders
   #:session-file-path
   #:active-pid-path
   ;; Session file I/O
   #:read-session-file
   #:session-has-task-p
   #:write-session-task-file
   #:delete-session-task-file))
