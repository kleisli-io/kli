;;; MCP HTTP Transport - Session management
;;;
;;; Implements MCP Streamable HTTP session lifecycle (spec 2025-03-26):
;;; - Session ID generation (cryptographically seeded, visible ASCII hex)
;;; - Session creation on InitializeResult
;;; - Session validation on subsequent requests
;;; - Session termination via DELETE

(in-package #:mcp-http)

;;; Session store

(defclass session-store ()
  ((sessions :initform (make-hash-table :test 'equal)
             :reader store-sessions)
   (lock :initform (make-lock "session-store")
         :reader store-lock))
  (:documentation "Thread-safe store for MCP sessions.
   Maps session ID strings to session metadata plists."))

(defun make-session-store ()
  "Create a new empty session store."
  (make-instance 'session-store))

;;; Session ID generation

(defun generate-session-id ()
  "Generate a cryptographically random session ID.
   Returns a 32-character lowercase hex string (128 bits).
   Uses SBCL's random state seeded from /dev/urandom.
   All characters are visible ASCII (0x30-0x66), satisfying MCP spec
   requirement of 0x21-0x7E."
  (let* ((state (sb-ext:seed-random-state t))
         (bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref bytes i) (random 256 state)))
    (with-output-to-string (s)
      (dotimes (i 16)
        (format s "~2,'0x" (aref bytes i))))))

;;; Session operations

(defun create-session (store)
  "Create a new session and return its ID. Thread-safe."
  (let ((id (generate-session-id)))
    (with-lock-held ((store-lock store))
      (setf (gethash id (store-sessions store))
            (list :created-at (get-universal-time))))
    id))

(defun valid-session-p (store session-id)
  "Return T if SESSION-ID is a valid active session. Thread-safe."
  (when session-id
    (with-lock-held ((store-lock store))
      (nth-value 1 (gethash session-id (store-sessions store))))))

(defun terminate-session (store session-id)
  "Remove a session from the store. Returns T if session existed. Thread-safe."
  (when session-id
    (with-lock-held ((store-lock store))
      (remhash session-id (store-sessions store)))))

;;; Origin validation

(defun valid-origin-p (origin host port)
  "Validate Origin header against server's host and port.
   Returns T if the origin is acceptable (prevents DNS rebinding).
   Accepts: no origin (same-origin/non-browser), or origin matching
   the server's host:port with common localhost variants."
  (cond
    ;; No Origin header — same-origin request (curl, non-browser clients)
    ((or (null origin) (string= origin "")) t)
    ;; Check against expected origins for this server
    (t (let ((expected (list (format nil "http://~A:~A" host port)
                             (format nil "https://~A:~A" host port)
                             (format nil "http://localhost:~A" port)
                             (format nil "https://localhost:~A" port)
                             (format nil "http://127.0.0.1:~A" port)
                             (format nil "https://127.0.0.1:~A" port))))
         (not (null (member origin expected :test #'string=)))))))
