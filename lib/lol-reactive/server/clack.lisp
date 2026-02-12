;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Clack request/response abstraction layer
;;;;
;;;; Provides a clean API over Clack's env plist for request handling
;;;; and standardized response builders.

(in-package :lol-reactive)

;;; ============================================================================
;;; REQUEST ENVIRONMENT
;;; ============================================================================

(defvar *env* nil
  "Current Clack request environment (plist).
   Bound during request handling.")

(defvar *response-headers* nil
  "Accumulated response headers for current request.
   Used by add-response-header and included in final response.")

;;; ============================================================================
;;; REQUEST ACCESSORS
;;; ============================================================================

(defun request-path ()
  "Get request path from Clack env.
   Returns the path portion of the URL (e.g., \"/api/users\")."
  (getf *env* :path-info "/"))

(defun request-method ()
  "Get request method as keyword (:GET, :POST, :PUT, :DELETE, etc.)."
  (getf *env* :request-method :get))

(defun request-query-string ()
  "Get raw query string (without leading ?)."
  (getf *env* :query-string ""))

(defun request-header (name)
  "Get request header by name (case-insensitive).
   NAME can be a string or keyword."
  (let ((headers (getf *env* :headers)))
    (when headers
      (gethash (string-downcase (string name)) headers))))

(defun request-content-type ()
  "Get Content-Type header value."
  (getf *env* :content-type))

(defun request-content-length ()
  "Get Content-Length as integer, or NIL if not present."
  (getf *env* :content-length))

(defun request-body ()
  "Get raw request body as string.
   Reads from the :raw-body stream if present."
  (let ((body-stream (getf *env* :raw-body)))
    (when body-stream
      (let ((content-length (request-content-length)))
        (if content-length
            ;; Read exact length if known
            (let ((octets (make-array content-length :element-type '(unsigned-byte 8))))
              (read-sequence octets body-stream)
              (babel:octets-to-string octets :encoding :utf-8))
            ;; Read until EOF
            (let ((octets (alexandria:read-stream-content-into-byte-vector body-stream)))
              (when (> (length octets) 0)
                (babel:octets-to-string octets :encoding :utf-8))))))))

(defun request-body-json ()
  "Parse request body as JSON.
   Returns NIL if body is empty or not valid JSON."
  (let ((body (request-body)))
    (when (and body (> (length body) 0))
      (handler-case
          (cl-json:decode-json-from-string body)
        (error () nil)))))

;;; ============================================================================
;;; PARAMETER ACCESSORS
;;; ============================================================================

(defun query-param (name)
  "Get query parameter by name.
   NAME is a string. Returns NIL if not found."
  (let ((params (getf *env* :query-parameters)))
    (cdr (assoc name params :test #'string=))))

(defun query-params ()
  "Get all query parameters as alist of (name . value)."
  (getf *env* :query-parameters))

(defun post-param (name)
  "Get POST parameter by name.
   NAME is a string. Returns NIL if not found."
  (let ((params (getf *env* :body-parameters)))
    (cdr (assoc name params :test #'string=))))

(defun post-params ()
  "Get all POST parameters as alist of (name . value)."
  (getf *env* :body-parameters))

(defun param (name)
  "Get parameter by name, checking POST first, then query string.
   NAME is a string."
  (or (post-param name)
      (query-param name)))

;;; ============================================================================
;;; RESPONSE BUILDERS
;;; ============================================================================

(defun response (status &key headers body content-type)
  "Build a Clack response list: (status headers-plist body-list).

   STATUS: HTTP status code (integer)
   HEADERS: Additional headers as plist
   BODY: Response body (string or list of strings)
   CONTENT-TYPE: Convenience for setting Content-Type header

   Accumulated headers from *response-headers* are included automatically."
  (let ((all-headers (append
                      (when content-type
                        (list :content-type content-type))
                      *response-headers*
                      headers)))
    (list status
          all-headers
          (if (listp body) body (list body)))))

(defun html-response (body &key (status 200) headers)
  "Build an HTML response with proper Content-Type.

   BODY: HTML string
   STATUS: HTTP status code (default 200)
   HEADERS: Additional headers"
  (response status
            :content-type "text/html; charset=utf-8"
            :headers headers
            :body body))

(defun json-response (data &key (status 200) headers)
  "Build a JSON response, encoding DATA to JSON string.

   DATA: Lisp data structure to encode
   STATUS: HTTP status code (default 200)
   HEADERS: Additional headers"
  (response status
            :content-type "application/json; charset=utf-8"
            :headers headers
            :body (cl-json:encode-json-to-string data)))

(defun text-response (body &key (status 200) headers)
  "Build a plain text response.

   BODY: Text string
   STATUS: HTTP status code (default 200)
   HEADERS: Additional headers"
  (response status
            :content-type "text/plain; charset=utf-8"
            :headers headers
            :body body))

(defun redirect-response (url &key (status 302) headers)
  "Build a redirect response.

   URL: Target URL for redirect
   STATUS: HTTP status (302 for temporary, 301 for permanent)
   HEADERS: Additional headers"
  (response status
            :headers (append (list :location url) headers)
            :body nil))

(defun error-response (status &key message headers content-type)
  "Build an error response.

   STATUS: HTTP error status code
   MESSAGE: Error message (optional)
   HEADERS: Additional headers
   CONTENT-TYPE: Response content type"
  (response status
            :content-type (or content-type "text/plain; charset=utf-8")
            :headers headers
            :body (or message (http-status-text status))))

;;; ============================================================================
;;; RESPONSE HEADER ACCUMULATION
;;; ============================================================================

(defmacro with-response-headers (() &body body)
  "Execute BODY with fresh response header accumulation.
   Headers added via add-response-header will be included in final response."
  `(let ((*response-headers* nil))
     ,@body))

(defun add-response-header (name value)
  "Add a header to the current response.
   NAME: Header name (string or keyword)
   VALUE: Header value (string)

   Must be called within with-response-headers context."
  ;; Headers are accumulated as plist, push in reverse order
  (push value *response-headers*)
  (push (if (keywordp name)
            name
            (intern (string-upcase name) :keyword))
        *response-headers*))

(defun get-response-headers ()
  "Get currently accumulated response headers as plist."
  *response-headers*)

;;; ============================================================================
;;; HTTP STATUS HELPERS
;;; ============================================================================

(defun http-status-text (code)
  "Get standard text for HTTP status code."
  (case code
    (200 "OK")
    (201 "Created")
    (204 "No Content")
    (301 "Moved Permanently")
    (302 "Found")
    (304 "Not Modified")
    (400 "Bad Request")
    (401 "Unauthorized")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (409 "Conflict")
    (422 "Unprocessable Entity")
    (429 "Too Many Requests")
    (500 "Internal Server Error")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (otherwise "Unknown Status")))

;;; ============================================================================
;;; SESSION ACCESSORS (Lack middleware integration)
;;; ============================================================================

(defun session-get (key)
  "Get value from Lack session.
   KEY can be a symbol or string.
   Returns NIL if session not available or key not found."
  (let ((session (getf *env* :lack.session)))
    (when session
      (gethash key session))))

(defun session-set (key value)
  "Set value in Lack session.
   KEY can be a symbol or string.
   VALUE is any serializable Lisp value.
   Returns VALUE, or NIL if session not available."
  (let ((session (getf *env* :lack.session)))
    (when session
      (setf (gethash key session) value))))

(defun session-delete (key)
  "Remove key from Lack session.
   Returns T if key was present, NIL otherwise."
  (let ((session (getf *env* :lack.session)))
    (when session
      (remhash key session))))

(defun session-clear ()
  "Clear all session data.
   Returns T if session was available, NIL otherwise."
  (let ((session (getf *env* :lack.session)))
    (when session
      (clrhash session)
      t)))

(defun session-keys ()
  "Get list of all session keys."
  (let ((session (getf *env* :lack.session)))
    (when session
      (loop for key being the hash-keys of session collect key))))

;;; ============================================================================
;;; CSRF INTEGRATION (Lack middleware)
;;; ============================================================================

(defun csrf-token ()
  "Get current CSRF token from session.
   Works with both Lack CSRF middleware and custom CSRF (security.lisp).
   Returns NIL if session not available."
  (session-get "csrf-token"))

(defun csrf-input ()
  "Generate hidden CSRF input field for forms.
   Uses unified 'csrf-token' field name (matching security.lisp and configured middleware).
   Returns HTML string or empty string if no token."
  (let ((token (csrf-token)))
    (if token
        (format nil "<input type=\"hidden\" name=\"csrf-token\" value=\"~A\">" token)
        "")))
