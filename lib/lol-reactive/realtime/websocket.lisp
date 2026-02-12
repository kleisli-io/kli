;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; WebSocket support for lol-reactive via websocket-driver
;;;;
;;;; Provides WebSocket connection management and message broadcasting
;;;; for real-time bidirectional communication.

(in-package :lol-reactive)

;;; ============================================================================
;;; CONNECTION REGISTRY
;;; ============================================================================

(defvar *ws-connections* (make-hash-table :test 'equal)
  "Active WebSocket connections indexed by channel ID.
   Each channel maps to a list of websocket-driver ws objects.")

(defvar *ws-lock* (bordeaux-threads:make-lock "ws-connections-lock")
  "Lock for thread-safe access to *ws-connections*.")

(defun ws-connection-count (&optional channel)
  "Return count of WebSocket connections.
   If CHANNEL is provided, count connections for that channel.
   Otherwise return total connection count."
  (bordeaux-threads:with-lock-held (*ws-lock*)
    (if channel
        (length (gethash channel *ws-connections*))
        (loop for conns being the hash-values of *ws-connections*
              sum (length conns)))))

(defun ws-channels ()
  "Return list of active channel IDs."
  (bordeaux-threads:with-lock-held (*ws-lock*)
    (loop for channel being the hash-keys of *ws-connections*
          collect channel)))

;;; ============================================================================
;;; WEBSOCKET HANDLER CREATION
;;; ============================================================================

(defun make-ws-handler (channel &key on-open on-message on-close on-error)
  "Create a WebSocket handler for a channel.

   CHANNEL: String identifying the channel (e.g., \"chat\", \"notifications\")
   ON-OPEN: Called with (ws) when connection opens
   ON-MESSAGE: Called with (ws message) when message received
   ON-CLOSE: Called with (ws &key code reason) when connection closes
   ON-ERROR: Called with (ws error) on protocol error

   Returns a Clack application function suitable for routing."
  (lambda (env)
    (let ((ws (websocket-driver.server:make-server env)))
      ;; Handle connection open
      (event-emitter:on :open ws
        (lambda ()
          ;; Register connection
          (bordeaux-threads:with-lock-held (*ws-lock*)
            (push ws (gethash channel *ws-connections*)))
          ;; Call user handler
          (when on-open
            (funcall on-open ws))))

      ;; Handle incoming messages
      (event-emitter:on :message ws
        (lambda (message)
          (when on-message
            (funcall on-message ws message))))

      ;; Handle connection close
      (event-emitter:on :close ws
        (lambda (&key code reason)
          ;; Unregister connection
          (bordeaux-threads:with-lock-held (*ws-lock*)
            (setf (gethash channel *ws-connections*)
                  (remove ws (gethash channel *ws-connections*))))
          ;; Call user handler
          (when on-close
            (funcall on-close ws :code code :reason reason))))

      ;; Handle protocol errors
      (event-emitter:on :error ws
        (lambda (error)
          (when on-error
            (funcall on-error ws error))))

      ;; Return streaming response starter
      (lambda (responder)
        (declare (ignore responder))
        ;; start-connection sends 101 Switching Protocols directly to socket
        ;; and enters blocking read loop until connection closes
        (websocket-driver.ws.base:start-connection ws)
        ;; Mark headers as sent so Hunchentoot doesn't try to send another response
        ;; after start-connection returns (when the WebSocket closes)
        (setf hunchentoot::*headers-sent* t)))))

;;; ============================================================================
;;; MESSAGE SENDING
;;; ============================================================================

(defun ws-send (ws message)
  "Send a message to a WebSocket connection.
   MESSAGE can be a string (sent as text) or byte vector (sent as binary)."
  (websocket-driver.ws.base:send ws message))

(defun ws-send-text (ws text)
  "Send a text message to a WebSocket connection."
  (websocket-driver.ws.base:send-text ws text))

(defun ws-send-binary (ws data)
  "Send binary data to a WebSocket connection."
  (websocket-driver.ws.base:send-binary ws data))

(defun ws-send-json (ws data)
  "Send data as JSON to a WebSocket connection."
  (ws-send-text ws (cl-json:encode-json-to-string data)))

(defun ws-close (ws &key code reason)
  "Close a WebSocket connection."
  (websocket-driver.ws.base:close-connection ws :code code :reason reason))

;;; ============================================================================
;;; BROADCASTING
;;; ============================================================================

(defun ws-broadcast (channel message)
  "Broadcast a message to all connections on a channel.
   Returns count of connections that received the message."
  (let ((connections (bordeaux-threads:with-lock-held (*ws-lock*)
                       (copy-list (gethash channel *ws-connections*))))
        (sent 0))
    (dolist (ws connections sent)
      (handler-case
          (progn
            (ws-send ws message)
            (incf sent))
        (error (e)
          (declare (ignore e))
          ;; Connection probably dead, will be cleaned up on close event
          nil)))))

(defun ws-broadcast-json (channel data)
  "Broadcast data as JSON to all connections on a channel."
  (ws-broadcast channel (cl-json:encode-json-to-string data)))

(defun ws-broadcast-all (message)
  "Broadcast a message to ALL WebSocket connections across all channels."
  (let ((total 0))
    (bordeaux-threads:with-lock-held (*ws-lock*)
      (maphash (lambda (channel connections)
                 (declare (ignore channel))
                 (dolist (ws connections)
                   (handler-case
                       (progn
                         (ws-send ws message)
                         (incf total))
                     (error () nil))))
               *ws-connections*))
    total))

;;; ============================================================================
;;; HTMX INTEGRATION
;;; ============================================================================

(defun ws-broadcast-html (channel target-id html &key (swap "innerHTML"))
  "Broadcast HTML update to all connections on a channel.
   Sends a JSON message with type, target, html, and swap strategy."
  (ws-broadcast-json channel
    `((:type . "html")
      (:target . ,target-id)
      (:html . ,html)
      (:swap . ,swap))))

(defun ws-broadcast-oob (channel updates)
  "Broadcast out-of-band updates to all connections on a channel.
   UPDATES is a list of (target-id html &key swap) specifications."
  (ws-broadcast-json channel
    `((:type . "oob")
      (:updates . ,(mapcar (lambda (u)
                             (destructuring-bind (target-id html &key (swap "outerHTML")) u
                               `((:target . ,target-id)
                                 (:html . ,html)
                                 (:swap . ,swap))))
                           updates)))))

(defun ws-broadcast-trigger (channel event &optional detail)
  "Broadcast an event trigger to all connections on a channel.
   CLIENT-SIDE: Will dispatch a CustomEvent with the given name and detail."
  (ws-broadcast-json channel
    `((:type . "trigger")
      (:event . ,event)
      ,@(when detail `((:detail . ,detail))))))

;;; ============================================================================
;;; ROUTE REGISTRATION HELPER
;;; ============================================================================

(defmacro defws (path channel &key on-open on-message on-close on-error)
  "Define a WebSocket route.

   PATH: URL path for WebSocket endpoint (e.g., \"/ws/chat\")
   CHANNEL: Channel name for connection grouping
   ON-OPEN: Handler called when connection opens (ws)
   ON-MESSAGE: Handler called on message (ws message)
   ON-CLOSE: Handler called on close (ws &key code reason)
   ON-ERROR: Handler called on error (ws error)

   Example:
     (defws \"/ws/notifications\" \"notifications\"
       :on-message (lambda (ws msg)
                     (let ((data (cl-json:decode-json-from-string msg)))
                       (handle-notification ws data))))"
  `(setf (gethash (cons :get ,path) *routes*)
         (make-ws-handler ,channel
                          :on-open ,on-open
                          :on-message ,on-message
                          :on-close ,on-close
                          :on-error ,on-error)))
