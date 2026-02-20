;;;; KLI Dashboard — Server lifecycle

(in-package :kli-dashboard)

(defvar *server* nil "Hunchentoot acceptor instance.")
(defvar *port*
  (or (ignore-errors
        (parse-integer (uiop:getenv "KLI_DASHBOARD_PORT")))
      9091)
  "Dashboard port. Override via KLI_DASHBOARD_PORT env var.")

(defun start (&key (port *port*) (max-retries 10))
  "Start the KLI dashboard server. Tries successive ports on conflict."
  (when *server*
    (stop))
  ;; Initialize design tokens
  (init-tokens)
  ;; Start server, retrying on port conflict
  (loop for attempt from 0 below max-retries
        for try-port = (+ port attempt)
        for server = (start-server :port try-port :use-csrf nil)
        when server
          do (setf *server* server)
             (format t "~&[kli-dashboard] Running on http://localhost:~D/~%" try-port)
             (return server)
        finally (error "Could not start dashboard: ports ~D-~D all in use"
                       port (+ port max-retries -1))))

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
