;;;; LOL-REACTIVE Error Handling
;;;; Production-grade error handling with logging and safe error pages
;;;;
;;;; Provides:
;;;; - with-error-handling: Macro for consistent error handling
;;;; - log-error: Structured error logging
;;;; - render-error-page: User-friendly error pages with fallbacks

(in-package :lol-reactive)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ═══════════════════════════════════════════════════════════════════════════

(defvar *debug-mode* nil
  "When T, show full backtraces in error responses and log detailed info.")

(defvar *error-log-path* "/tmp/lol-reactive-error.log"
  "Path to error log file. Set to NIL to disable file logging.")

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Error Logging
;;; ═══════════════════════════════════════════════════════════════════════════

(defun log-error (context error &key (stream *error-output*) (backtrace-count 30))
  "Log error with context, type, and optional backtrace.
   CONTEXT is a string describing where the error occurred (e.g., \"GET /home\").
   ERROR is the condition object.
   :stream - where to write (default *error-output*)
   :backtrace-count - number of stack frames to show (default 30)"
  ;; Console logging
  (format stream "~&========================================~%")
  (format stream "LOL-REACTIVE ERROR~%")
  (format stream "========================================~%")
  (format stream "Context: ~A~%" context)
  (format stream "Error: ~A~%" error)
  (format stream "Type: ~A~%" (type-of error))
  (format stream "Time: ~A~%" (get-universal-time))

  ;; Backtrace in debug mode
  (when *debug-mode*
    #+sbcl
    (handler-case
        (progn
          (format stream "~%BACKTRACE:~%")
          (sb-debug:print-backtrace :stream stream :count backtrace-count))
      (error () nil)))

  (format stream "========================================~%~%")
  (force-output stream)

  ;; File logging (always, if path configured)
  (when *error-log-path*
    (ignore-errors
      (with-open-file (log-stream *error-log-path*
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
        (format log-stream "~%[~A] ~A: ~A~%"
                (get-universal-time)
                context
                error)
        #+sbcl
        (handler-case
            (sb-debug:print-backtrace :stream log-stream :count backtrace-count)
          (error () nil))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Error Pages
;;; ═══════════════════════════════════════════════════════════════════════════

(defun minimal-error-html (title heading message &optional debug-info)
  "Generate minimal error page HTML that cannot fail (no external dependencies).
   Uses CSS variables from design tokens for visual consistency.
   Error pages are generic - apps can override *colors* and *typography* to
   change their appearance."
  (format nil "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>~A</title>
<style>
:root {
  --color-background: ~A;
  --color-surface: ~A;
  --color-text: ~A;
  --color-muted: ~A;
  --color-primary: ~A;
  --font-family: ~A;
}
body {
  background: var(--color-background);
  color: var(--color-text);
  font-family: var(--font-family);
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 100vh;
  margin: 0;
}
.container { text-align: center; max-width: 600px; padding: 2rem; }
h1 { color: var(--color-primary); font-size: 3rem; margin: 0 0 1rem 0; }
p { color: var(--color-muted); margin: 1rem 0; line-height: 1.6; }
a { color: var(--color-primary); text-decoration: none; }
a:hover { text-decoration: underline; }
pre {
  background: var(--color-surface);
  padding: 1rem;
  border-radius: 4px;
  overflow-x: auto;
  text-align: left;
  font-size: 0.85rem;
  color: var(--color-muted);
  margin-top: 2rem;
}
</style>
</head>
<body>
<div class=\"container\">
<h1>~A</h1>
<p>~A</p>
<a href=\"/\">Return to home</a>
~A
</div>
</body>
</html>"
          title
          ;; Inject token values as CSS variables
          (get-color :background)
          (get-color :surface)
          (get-color :text)
          (get-color :muted)
          (get-color :primary)
          (get-font :family)
          ;; Content
          heading
          message
          (if debug-info
              (format nil "<pre>~A</pre>" debug-info)
              "")))

(defun render-error-page (error &key (context "Unknown"))
  "Render a user-friendly error page.
   In debug mode, shows error details. Otherwise, shows generic message.
   Uses minimal HTML as fallback if complex rendering fails."
  (if *debug-mode*
      (minimal-error-html
       "Error - LOL-REACTIVE"
       "Something went wrong"
       (format nil "Context: ~A" context)
       (format nil "~A~%~%Type: ~A" error (type-of error)))
      (minimal-error-html
       "Error - LOL-REACTIVE"
       "Something went wrong"
       "We're working on it. Please try again later.")))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Error Handling Macro
;;; ═══════════════════════════════════════════════════════════════════════════

(defmacro with-error-handling (context &body body)
  "Wrap handler with consistent error handling, logging, and safe error pages.
   CONTEXT is a string describing the operation (e.g., \"GET /home\" or \"API /users\").

   Example:
     (with-error-handling \"GET /home\"
       (render-home-page))

   Behavior:
   1. Adds security headers to response
   2. Executes body
   3. On error:
      - Logs to console and file
      - Returns 500 error response
      - If error page rendering fails, returns minimal safe HTML"
  (let ((context-var (gensym "CONTEXT")))
    `(let ((,context-var ,context))
       (handler-case
           (progn
             ;; Add security headers before executing handler
             (add-security-headers)
             ,@body)
         (error (e)
           ;; Log the error
           (log-error ,context-var e)

           ;; Render error page with fallback, return as Clack response
           (error-response 500
                           :content-type "text/html; charset=utf-8"
                           :message (handler-case
                                        (render-error-page e :context ,context-var)
                                      (error (secondary-error)
                                        ;; If error page rendering fails, use absolute minimal HTML
                                        (format *error-output* "~%Secondary error rendering error page: ~A~%" secondary-error)
                                        (minimal-error-html
                                         "Error"
                                         "Error"
                                         (format nil "~A (error page also failed: ~A)" ,context-var secondary-error))))))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; HTTP Status Error Pages (for custom Hunchentoot acceptor)
;;; ═══════════════════════════════════════════════════════════════════════════

(defun render-404-page (&optional path)
  "Render custom 404 Not Found page."
  (minimal-error-html
   "404 - Not Found"
   "404"
   (if path
       (format nil "The page '~A' doesn't exist." path)
       "The page you're looking for doesn't exist.")))

(defun render-500-page (&optional error)
  "Render custom 500 Internal Server Error page."
  (if (and *debug-mode* error)
      (minimal-error-html
       "500 - Server Error"
       "500"
       "Something went wrong on our end."
       (format nil "~A" error))
      (minimal-error-html
       "500 - Server Error"
       "500"
       "Something went wrong on our end.")))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Debug Mode Control
;;; ═══════════════════════════════════════════════════════════════════════════

(defun enable-debug-mode ()
  "Enable debug mode - shows detailed errors, backtraces in responses."
  (setf *debug-mode* t)
  (format t "LOL-REACTIVE debug mode enabled~%"))

(defun disable-debug-mode ()
  "Disable debug mode - shows minimal error info to users."
  (setf *debug-mode* nil)
  (format t "LOL-REACTIVE debug mode disabled~%"))
