;;;; KLI Dashboard — Server lifecycle

(in-package :kli-dashboard)

(defvar *server* nil "Hunchentoot acceptor instance.")
(defvar *port* 9091 "Dashboard port.")

(defun start (&key (port *port*))
  "Start the KLI dashboard server."
  (when *server*
    (stop))
  ;; Initialize design tokens
  (init-tokens)
  ;; Start server
  (setf *server* (start-server :port port :use-csrf nil))
  (format t "~&[kli-dashboard] Running on http://localhost:~D/~%" port)
  *server*)

(defun stop ()
  "Stop the KLI dashboard server."
  (when *server*
    (stop-server *server*)
    (setf *server* nil)
    (format t "~&[kli-dashboard] Stopped.~%")))

(defun reload ()
  "Invalidate all caches and reload."
  (invalidate-cache)
  (format t "~&[kli-dashboard] Caches cleared.~%"))
