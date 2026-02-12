;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Server-Sent Events (SSE) support for lol-reactive
;;;;
;;;; Provides SSE connection management and message broadcasting
;;;; for simpler server-to-client push scenarios.

(in-package :lol-reactive)

;;; ============================================================================
;;; CONNECTION REGISTRY
;;; ============================================================================

(defvar *sse-connections* (make-hash-table :test 'equal)
  "Active SSE connections indexed by channel ID.
   Each channel maps to a list of connection plists with :stream and :alive-p keys.")

(defvar *sse-lock* (bordeaux-threads:make-lock "sse-connections-lock")
  "Lock for thread-safe access to *sse-connections*.")

(defun sse-connection-count (&optional channel)
  "Return count of SSE connections.
   If CHANNEL is provided, count connections for that channel.
   Otherwise return total connection count."
  (bordeaux-threads:with-lock-held (*sse-lock*)
    (if channel
        (length (gethash channel *sse-connections*))
        (loop for conns being the hash-values of *sse-connections*
              sum (length conns)))))

(defun sse-channels ()
  "Return list of active SSE channel IDs."
  (bordeaux-threads:with-lock-held (*sse-lock*)
    (loop for channel being the hash-keys of *sse-connections*
          collect channel)))

;;; ============================================================================
;;; SSE MESSAGE FORMATTING
;;; ============================================================================

(defun format-sse-event (event-type data &key id retry)
  "Format an SSE event message according to the SSE specification.
   EVENT-TYPE: Event name (string)
   DATA: Event data (will be JSON-encoded if not a string)
   ID: Optional event ID for client reconnection
   RETRY: Optional retry interval in milliseconds"
  (with-output-to-string (s)
    (when id
      (format s "id: ~A~%" id))
    (when retry
      (format s "retry: ~A~%" retry))
    (when event-type
      (format s "event: ~A~%" event-type))
    ;; Data must be on its own line(s), each prefixed with "data: "
    (let ((data-str (if (stringp data)
                        data
                        (cl-json:encode-json-to-string data))))
      ;; Handle multi-line data
      (dolist (line (uiop:split-string data-str :separator '(#\Newline)))
        (format s "data: ~A~%" line)))
    ;; Empty line terminates the event
    (format s "~%")))

;;; ============================================================================
;;; SSE HANDLER CREATION
;;; ============================================================================

(defun make-sse-handler (channel &key on-connect on-disconnect)
  "Create an SSE handler for a channel.

   CHANNEL: String identifying the channel (e.g., \"updates\", \"notifications\")
   ON-CONNECT: Called with (conn) when connection opens
   ON-DISCONNECT: Called with (conn) when connection closes

   Returns a Clack application function suitable for routing."
  (lambda (env)
    (declare (ignore env))
    ;; Return delayed/streaming response
    (lambda (responder)
      (block sse-handler
        (let* ((writer (funcall responder
                                '(200 (:content-type "text/event-stream"
                                       :cache-control "no-cache"
                                       :connection "keep-alive"
                                       :x-accel-buffering "no"))))
               (conn (list :stream writer
                           :channel channel
                           :alive-p t
                           :created-at (get-universal-time)
                           :on-disconnect on-disconnect)))
          ;; Register connection
          (bordeaux-threads:with-lock-held (*sse-lock*)
            (push conn (gethash channel *sse-connections*)))

          ;; Send initial connection event
          (handler-case
              (progn
                (funcall writer (format-sse-event "connected"
                                                  `((:channel . ,channel)
                                                    (:timestamp . ,(get-universal-time)))
                                                  :retry 3000))
                ;; Call user handler
                (when on-connect
                  (funcall on-connect conn)))
            (error (e)
              (declare (ignore e))
              ;; Connection failed immediately
              (sse-remove-connection channel conn)
              (return-from sse-handler nil)))

          ;; BLOCKING: Keep connection alive by sending periodic pings
          ;; This blocks the worker thread to keep the connection open
          (unwind-protect
              (loop while (getf conn :alive-p)
                    do (handler-case
                           (progn
                             ;; Send keep-alive comment every 30 seconds
                             (funcall writer (format nil ": keepalive~%~%"))
                             (sleep 30))
                         (error (e)
                           (declare (ignore e))
                           ;; Connection closed
                           (setf (getf conn :alive-p) nil))))
            ;; Cleanup on exit
            (sse-remove-connection channel conn)))))))

(defun sse-remove-connection (channel conn)
  "Remove an SSE connection from the registry and call disconnect handler."
  (bordeaux-threads:with-lock-held (*sse-lock*)
    (setf (gethash channel *sse-connections*)
          (remove conn (gethash channel *sse-connections*) :test #'eq)))
  ;; Call disconnect handler if provided
  (let ((on-disconnect (getf conn :on-disconnect)))
    (when on-disconnect
      (handler-case
          (funcall on-disconnect conn)
        (error () nil)))))

;;; ============================================================================
;;; MESSAGE SENDING
;;; ============================================================================

(defun sse-send (conn event-type data &key id)
  "Send an SSE event to a specific connection.
   Returns T on success, NIL if connection is dead."
  (let ((stream (getf conn :stream)))
    (handler-case
        (progn
          (funcall stream (format-sse-event event-type data :id id))
          t)
      (error (e)
        (declare (ignore e))
        ;; Mark connection as dead
        (setf (getf conn :alive-p) nil)
        nil))))

(defun sse-send-comment (conn comment)
  "Send an SSE comment (keep-alive ping).
   Comments start with colon and are ignored by EventSource."
  (let ((stream (getf conn :stream)))
    (handler-case
        (progn
          (funcall stream (format nil ": ~A~%~%" comment))
          t)
      (error ()
        (setf (getf conn :alive-p) nil)
        nil))))

;;; ============================================================================
;;; BROADCASTING
;;; ============================================================================

(defun sse-broadcast (channel event-type data &key id)
  "Broadcast an SSE event to all connections on a channel.
   Returns count of connections that received the message.
   Dead connections are automatically removed."
  (let ((connections (bordeaux-threads:with-lock-held (*sse-lock*)
                       (copy-list (gethash channel *sse-connections*))))
        (sent 0)
        (dead nil))
    (dolist (conn connections)
      (if (sse-send conn event-type data :id id)
          (incf sent)
          (push conn dead)))
    ;; Clean up dead connections
    (when dead
      (bordeaux-threads:with-lock-held (*sse-lock*)
        (setf (gethash channel *sse-connections*)
              (set-difference (gethash channel *sse-connections*) dead :test #'eq))))
    sent))

(defun sse-broadcast-all (event-type data)
  "Broadcast an SSE event to ALL connections across all channels."
  (let ((total 0))
    (bordeaux-threads:with-lock-held (*sse-lock*)
      (maphash (lambda (channel connections)
                 (declare (ignore channel))
                 (dolist (conn connections)
                   (when (sse-send conn event-type data)
                     (incf total))))
               *sse-connections*))
    total))

(defun sse-ping-all ()
  "Send keep-alive ping to all SSE connections.
   Returns count of live connections."
  (let ((alive 0)
        (dead nil))
    (bordeaux-threads:with-lock-held (*sse-lock*)
      (maphash (lambda (channel connections)
                 (dolist (conn connections)
                   (if (sse-send-comment conn "ping")
                       (incf alive)
                       (push (cons channel conn) dead))))
               *sse-connections*))
    ;; Clean up dead connections
    (dolist (entry dead)
      (sse-remove-connection (car entry) (cdr entry)))
    alive))

;;; ============================================================================
;;; HTMX INTEGRATION
;;; ============================================================================

(defun sse-broadcast-html (channel target-id html &key (swap "innerHTML") id)
  "Broadcast HTML update to all SSE connections on a channel.
   Sends an 'update' event with target, html, and swap strategy."
  (sse-broadcast channel "update"
    `((:target . ,target-id)
      (:html . ,html)
      (:swap . ,swap))
    :id id))

(defun sse-broadcast-oob (channel updates &key id)
  "Broadcast out-of-band updates to all SSE connections on a channel.
   UPDATES is a list of (target-id html &key swap) specifications."
  (sse-broadcast channel "oob"
    `((:updates . ,(mapcar (lambda (u)
                             (destructuring-bind (target-id html &key (swap "outerHTML")) u
                               `((:target . ,target-id)
                                 (:html . ,html)
                                 (:swap . ,swap))))
                           updates)))
    :id id))

(defun sse-broadcast-trigger (channel event &optional detail)
  "Broadcast an event trigger to all SSE connections on a channel.
   CLIENT-SIDE: Will dispatch a CustomEvent with the given name and detail."
  (sse-broadcast channel "trigger"
    `((:event . ,event)
      ,@(when detail `((:detail . ,detail))))))

;;; ============================================================================
;;; ROUTE REGISTRATION HELPER
;;; ============================================================================

(defmacro defsse (path channel &key on-connect on-disconnect)
  "Define an SSE route.

   PATH: URL path for SSE endpoint (e.g., \"/sse/updates\")
   CHANNEL: Channel name for connection grouping
   ON-CONNECT: Handler called when connection opens (conn)
   ON-DISCONNECT: Handler called on disconnect (conn)

   Example:
     (defsse \"/sse/notifications\" \"notifications\"
       :on-connect (lambda (conn)
                     (log:info \"Client connected to notifications\")))"
  `(setf (gethash (cons :get ,path) *routes*)
         (make-sse-handler ,channel
                           :on-connect ,on-connect
                           :on-disconnect ,on-disconnect)))
