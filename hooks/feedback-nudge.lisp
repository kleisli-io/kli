;;;; feedback-nudge.lisp - Stop hook handler for pattern feedback reminders
;;;;
;;;; Reminds agents to give feedback on activated playbook patterns before
;;;; stopping. Uses feedback-state.json (written by playbook-mcp) as bridge
;;;; between MCP server memory and hook filesystem.
;;;;
;;;; Rate-limited to max 10 nudges per session via nudge-count file.

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defparameter *max-nudges* 10
  "Maximum number of feedback nudges per session before allowing stop.
   Set high so the hook persistently asks for feedback until all patterns
   are covered. The safety valve prevents infinite loops in edge cases.")

;;; ---------------------------------------------------------------------------
;;; Nudge Count (rate limiting)
;;; ---------------------------------------------------------------------------

(defun nudge-count-path (input)
  "Path to nudge count file in session directory."
  (session-file input "playbook" "feedback-nudge-count"))

(defun read-nudge-count (input)
  "Read current nudge count. Returns 0 if file doesn't exist."
  (let ((path (nudge-count-path input)))
    (if (and path (probe-file path))
        (handler-case
            (parse-integer (string-trim '(#\Space #\Newline #\Return)
                                        (uiop:read-file-string path))
                           :junk-allowed t)
          (error () 0))
        0)))

(defun increment-nudge-count (input)
  "Increment and persist nudge count."
  (let ((path (nudge-count-path input))
        (count (1+ (read-nudge-count input))))
    (when path
      (ensure-directories-exist path)
      (with-open-file (s path :direction :output :if-exists :supersede)
        (princ count s)))
    count))

;;; ---------------------------------------------------------------------------
;;; Feedback State Reading
;;; ---------------------------------------------------------------------------

(defun read-feedback-state (input)
  "Read feedback-state.json from session directory.
   Returns hash-table with activated, feedback_given, pending, count keys.
   Returns NIL if file doesn't exist or is unreadable."
  (let ((path (session-file input "playbook" "feedback-state.json")))
    (when (and path (probe-file path))
      (handler-case
          (yason:parse (uiop:read-file-string path))
        (error () nil)))))

;;; ---------------------------------------------------------------------------
;;; Reminder Formatting
;;; ---------------------------------------------------------------------------

(defun format-feedback-reminder (pending-ids)
  "Format a concise reminder listing pending pattern IDs."
  (with-output-to-string (s)
    (format s "~D pattern~:P need feedback: ~{~A~^, ~}. "
            (length pending-ids) pending-ids)
    (format s "Use (:feedback! :helpful \"evidence\") or (:feedback! :harmful \"reason\").")))

;;; ---------------------------------------------------------------------------
;;; Core Logic
;;; ---------------------------------------------------------------------------

(defun check-pending-feedback (input)
  "Check if there are pending patterns needing feedback.
   Returns (values decision reason) — :continue with reason, or :stop."
  (let ((state (read-feedback-state input)))
    (if (null state)
        (values :stop nil)
        (let ((pending (gethash "pending" state))
              (count (or (gethash "count" state) 0)))
          (if (or (null pending) (zerop count))
              (values :stop nil)
              (let ((nudge-count (read-nudge-count input)))
                (if (>= nudge-count *max-nudges*)
                    (values :stop nil)
                    (progn
                      (increment-nudge-count input)
                      (values :continue
                              (format-feedback-reminder
                               (coerce pending 'list)))))))))))

;;; ---------------------------------------------------------------------------
;;; Handler Entry Point
;;; ---------------------------------------------------------------------------

(defun feedback-nudge-handler (input)
  "Stop hook handler: nudge agent to give pattern feedback before stopping.
   Uses decision=block to force continuation. Rate-limited to 10 nudges.
   Checks stop_hook_active to prevent infinite loops."
  (handler-case
      (let ((stop-hook-active (jref input "stop_hook_active")))
        ;; If stop_hook_active is true, allow stop to prevent infinite loops
        (if (eql stop-hook-active t)
            (stop-allow)
            (multiple-value-bind (decision reason) (check-pending-feedback input)
              (if (eq decision :continue)
                  (block-response :reason reason)
                  (stop-allow)))))
    (error () (stop-allow))))
