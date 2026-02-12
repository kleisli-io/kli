;;;; LOL-REACTIVE Security
;;;; Security headers, input sanitization, and CSRF protection
;;;;
;;;; Provides:
;;;; - add-security-headers: Standard security headers
;;;; - with-security: Wrap handlers with security
;;;; - sanitize-html: XSS prevention

(in-package :lol-reactive)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Security Headers
;;; ═══════════════════════════════════════════════════════════════════════════

(defun add-security-headers ()
  "Add standard security headers to the current response.
   Must be called within a with-response-headers context.

   Headers added:
   - X-Frame-Options: DENY - Prevent clickjacking
   - X-Content-Type-Options: nosniff - Prevent MIME sniffing
   - X-XSS-Protection: 1; mode=block - Enable XSS filter
   - Referrer-Policy: strict-origin-when-cross-origin - Control referrer info"
  (add-response-header "X-Frame-Options" "DENY")
  (add-response-header "X-Content-Type-Options" "nosniff")
  (add-response-header "X-XSS-Protection" "1; mode=block")
  (add-response-header "Referrer-Policy" "strict-origin-when-cross-origin"))

(defun add-csp-header (&key (default-src "'self'")
                            (script-src "'self' 'unsafe-inline' https://cdn.tailwindcss.com")
                            (style-src "'self' 'unsafe-inline' https://fonts.googleapis.com")
                            (font-src "'self' https://fonts.gstatic.com")
                            (img-src "'self' data: https:")
                            (connect-src "'self'"))
  "Add Content-Security-Policy header with customizable directives.
   Default allows Tailwind CDN and Google Fonts.
   Must be called within a with-response-headers context.

   Example:
     (add-csp-header :script-src \"'self'\" :style-src \"'self'\")"
  (add-response-header "Content-Security-Policy"
                       (format nil "default-src ~A; script-src ~A; style-src ~A; font-src ~A; img-src ~A; connect-src ~A"
                               default-src script-src style-src font-src img-src connect-src)))

(defmacro with-security (&body body)
  "Wrap handler with security headers.
   Convenience macro that adds security headers before executing body.

   Example:
     (defroute \"/api/data\" (:method :get)
       (with-security
         (get-data-json)))"
  `(progn
     (add-security-headers)
     ,@body))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Input Sanitization
;;; ═══════════════════════════════════════════════════════════════════════════

(defun sanitize-html (string)
  "Escape HTML special characters to prevent XSS.
   Converts & < > \" ' to their HTML entity equivalents.

   Example:
     (sanitize-html \"<script>alert('xss')</script>\")
     => \"&lt;script&gt;alert('xss')&lt;/script&gt;\""
  (when string
    (cl-who:escape-string string)))

(defun sanitize-attribute (string)
  "Escape string for use in HTML attributes.
   Escapes quotes and other special characters."
  (when string
    (with-output-to-string (out)
      (iter (for char in-string string)
            (case char
              (#\" (write-string "&quot;" out))
              (#\' (write-string "&#39;" out))
              (#\< (write-string "&lt;" out))
              (#\> (write-string "&gt;" out))
              (#\& (write-string "&amp;" out))
              (t (write-char char out)))))))

(defun sanitize-url (url)
  "Sanitize URL to prevent javascript: and data: injection.
   Returns NIL for dangerous URLs.

   Example:
     (sanitize-url \"javascript:alert(1)\") => NIL
     (sanitize-url \"https://example.com\") => \"https://example.com\""
  (when url
    (let ((lower-url (string-downcase (string-trim '(#\Space #\Tab #\Newline) url))))
      (cond
        ;; Block javascript: URLs
        ((cl-ppcre:scan "^javascript:" lower-url) nil)
        ;; Block data: URLs (can contain scripts)
        ((cl-ppcre:scan "^data:" lower-url) nil)
        ;; Block vbscript: URLs
        ((cl-ppcre:scan "^vbscript:" lower-url) nil)
        ;; Allow relative and absolute URLs
        (t url)))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; CSRF Protection
;;; ═══════════════════════════════════════════════════════════════════════════

(defun generate-csrf-token ()
  "Generate a random CSRF token (32 hex characters).
   Store in session and validate on form submission."
  (format nil "~32,'0X" (random (ash 1 128))))

(defun get-csrf-token ()
  "Get current CSRF token from session, creating if needed.
   Uses Lack session middleware via *env*.
   Note: Uses string key to match Lack CSRF middleware configuration."
  (or (session-get "csrf-token")
      (session-set "csrf-token" (generate-csrf-token))))

(defun validate-csrf-token (token)
  "Validate CSRF token from request matches session token.
   Returns T if valid, NIL if invalid.
   Note: Uses string key to match Lack CSRF middleware configuration."
  (when token
    (let ((session-token (session-get "csrf-token")))
      (and session-token
           (string= token session-token)))))

(defun csrf-token-input ()
  "Generate hidden input field with CSRF token.
   Include this in all forms that modify data.
   Uses html-attrs for proper attribute escaping.

   Example (in cl-who):
     (:form :method \"post\" :action \"/submit\"
       (who:str (csrf-token-input))
       ...)"
  (format nil "<input~A/>"
          (html-attrs "type" "hidden"
                      "name" "csrf-token"
                      "value" (get-csrf-token))))

(defmacro with-csrf-validation (&body body)
  "Wrap handler with CSRF token validation.
   Returns 403 Forbidden response if token is invalid.

   Example:
     (defroute \"/submit\" (:method :post)
       (with-csrf-validation
         (process-form)))"
  `(let ((token (post-param "csrf-token")))
     (if (validate-csrf-token token)
         (progn ,@body)
         (error-response 403
                         :content-type "text/html; charset=utf-8"
                         :message (minimal-error-html "403 Forbidden" "403" "Invalid or missing CSRF token")))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Rate Limiting (Simple In-Memory)
;;; ═══════════════════════════════════════════════════════════════════════════

(defvar *rate-limit-store* (make-hash-table :test 'equal)
  "In-memory store for rate limiting. Maps IP -> (count . timestamp)")

(defun check-rate-limit (ip &key (max-requests 100) (window-seconds 60))
  "Check if IP is within rate limit.
   Returns T if allowed, NIL if rate limited.
   Default: 100 requests per 60 seconds per IP."
  (let* ((now (get-universal-time))
         (entry (gethash ip *rate-limit-store*))
         (count (if entry (car entry) 0))
         (timestamp (if entry (cdr entry) now)))
    ;; Reset if window expired
    (when (> (- now timestamp) window-seconds)
      (setf count 0
            timestamp now))
    ;; Check limit
    (if (>= count max-requests)
        nil
        (progn
          (setf (gethash ip *rate-limit-store*) (cons (1+ count) timestamp))
          t))))

(defun get-client-ip ()
  "Get client IP address, checking X-Forwarded-For for proxied requests.
   Uses Clack *env* for request data."
  (or (request-header "X-Forwarded-For")
      (request-header "X-Real-IP")
      (getf *env* :remote-addr)))

(defmacro with-rate-limit ((&key (max-requests 100) (window-seconds 60)) &body body)
  "Wrap handler with rate limiting.
   Returns 429 Too Many Requests response if limit exceeded."
  `(let ((ip (get-client-ip)))
     (if (check-rate-limit ip :max-requests ,max-requests :window-seconds ,window-seconds)
         (progn ,@body)
         (error-response 429
                         :content-type "text/html; charset=utf-8"
                         :message (minimal-error-html "429 Too Many Requests" "429"
                                                      "You're making too many requests. Please slow down.")))))
