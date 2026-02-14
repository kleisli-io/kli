;;;; feedback-nudge.lisp - Stop hook handler for pattern feedback reminders
;;;;
;;;; Reminds agents to give feedback on activated playbook patterns before
;;;; stopping. Uses feedback-state.json (written by playbook-mcp) as bridge
;;;; between MCP server memory and hook filesystem.
;;;;
;;;; Rate-limited to max 2 nudges per session via nudge-count file.

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defparameter *max-nudges* 2
  "Maximum number of feedback nudges per session before allowing stop.")

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
  "Format a reminder message listing pending pattern IDs with pq_query template."
  (with-output-to-string (s)
    (format s "~%You activated ~D playbook pattern~:P that ~[~;has~:;have~] not received feedback yet:~%~%"
            (length pending-ids) (length pending-ids))
    (dolist (id pending-ids)
      (format s "  - ~A~%" id))
    (format s "~%Please give feedback before stopping:~%")
    (format s "  pq_query('(-> (pattern \"<id>\") (:feedback! :helpful \"<evidence>\"))')~%")
    (format s "  pq_query('(-> (pattern \"<id>\") (:feedback! :harmful \"<reason>\"))')~%")))

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
   Only activates on stop_reason=end_turn. Rate-limited to 2 nudges."
  (handler-case
      (let ((stop-reason (jref input "stop_reason")))
        (if (string= stop-reason "end_turn")
            (multiple-value-bind (decision reason) (check-pending-feedback input)
              (if (eq decision :continue)
                  (stop-continue :reason reason)
                  (stop-allow)))
            (stop-allow)))
    (error () (stop-allow))))
