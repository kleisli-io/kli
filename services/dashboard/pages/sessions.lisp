;;;; KLI Dashboard — Sessions page
;;;; Active and historical session tracking.

(in-package :kli-dashboard)

;;; ============================================================
;;; SESSION HELPERS
;;; ============================================================

(defun format-duration (seconds)
  "Format duration in seconds as human-readable string."
  (cond ((< seconds 60) (format nil "~Ds" seconds))
        ((< seconds 3600) (format nil "~Dm" (floor seconds 60)))
        ((< seconds 86400) (format nil "~Dh ~Dm" (floor seconds 3600)
                                    (floor (mod seconds 3600) 60)))
        (t (format nil "~Dd ~Dh" (floor seconds 86400)
                    (floor (mod seconds 86400) 3600)))))

(defun format-ago (seconds)
  "Format seconds-ago as relative time string."
  (cond ((< seconds 60) "just now")
        ((< seconds 3600) (format nil "~Dm ago" (floor seconds 60)))
        ((< seconds 86400) (format nil "~Dh ago" (floor seconds 3600)))
        (t (format nil "~Dd ago" (floor seconds 86400)))))

(defun session-short-id (session-id)
  "Truncate session ID for display."
  (if (> (length session-id) 8)
      (subseq session-id 0 8)
      session-id))

;;; ============================================================
;;; SESSION ROWS
;;; ============================================================

(defun render-active-session (entry)
  "Render a single active session card."
  (let* ((sid (getf entry :session))
         (ago (getf entry :ago-seconds))
         (dur (getf entry :duration))
         (events (getf entry :event-count))
         (tasks (getf entry :tasks))
         (task-count (getf entry :task-count))
         (depots (getf entry :depots))
         (scolor (session-color sid)))
    (htm-str
      (:div :class "session-card active-session"
        (:div :class "session-header"
          (:span :class "session-id"
            :style (format nil "background:~A22;color:~A" scolor scolor)
            (cl-who:str (session-short-id sid)))
          (:span :class "session-status active" "active")
          (:span :class "session-ago" (cl-who:str (format-ago ago))))
        (:div :class "session-meta-row"
          (:span :class "session-metric"
            (cl-who:fmt "~:D events" events))
          (:span :class "session-metric"
            (cl-who:fmt "~D task~:P" task-count))
          (when (> dur 0)
            (cl-who:htm
              (:span :class "session-metric"
                (cl-who:str (format-duration dur))))))
        ;; Task list
        (when tasks
          (cl-who:htm
            (:div :class "session-tasks"
              (dolist (task-id (subseq tasks 0 (min 5 (length tasks))))
                (cl-who:htm
                  (:a :href (format nil "/task?id=~A" task-id)
                      :class "session-task-link"
                    (cl-who:str (humanize-task-name task-id)))))
              (when (> (length tasks) 5)
                (cl-who:htm
                  (:span :class "session-more"
                    (cl-who:fmt "+~D more" (- (length tasks) 5))))))))
        ;; Depot badges
        (when (> (length depots) 1)
          (cl-who:htm
            (:div :class "session-depots"
              (dolist (dep depots)
                (cl-who:htm
                  (:span :class "session-depot-badge" (cl-who:str dep)))))))))))

(defun render-history-session (entry)
  "Render a single historical session row."
  (let* ((sid (getf entry :session))
         (ago (getf entry :ago-seconds))
         (dur (getf entry :duration))
         (events (getf entry :event-count))
         (task-count (getf entry :task-count))
         (left (getf entry :left))
         (scolor (session-color sid)))
    (htm-str
      (:div :class "session-row"
        (:span :class "session-id small"
          :style (format nil "background:~A22;color:~A" scolor scolor)
          (cl-who:str (session-short-id sid)))
        (:span :class "session-row-tasks"
          (cl-who:fmt "~D task~:P" task-count))
        (:span :class "session-row-events"
          (cl-who:fmt "~:D events" events))
        (:span :class "session-row-duration"
          (cl-who:str (format-duration dur)))
        (:span :class "session-row-ago"
          (cl-who:str (format-ago ago)))
        (when left
          (cl-who:htm
            (:span :class "session-row-left" "left")))))))

;;; ============================================================
;;; SESSIONS PAGE
;;; ============================================================

(defun render-sessions (&optional depot)
  "Render the sessions page content."
  (let* ((data (get-session-data depot))
         (active (getf data :active))
         (history (getf data :history))
         (total (getf data :total-sessions)))
    (htm-str
      (:div :style "max-width: 900px; margin: 0 auto; padding: 2rem;"

        ;; Summary
        (:div :class "stats-summary"
          (:div :class "stats-card"
            (:div :class "stats-number" :style "color:var(--color-success)"
              (cl-who:fmt "~D" (length active)))
            (:div :class "stats-label" "active now"))
          (:div :class "stats-card"
            (:div :class "stats-number"
              (cl-who:fmt "~D" total))
            (:div :class "stats-label" "total sessions")))

        ;; Active sessions
        (:div :class "sessions-section"
          (:h3 :class "stats-heading"
            "Active Sessions"
            (:span :class "count" (cl-who:fmt " (~D)" (length active))))
          (if active
              (cl-who:htm
                (:div :class "active-sessions-grid"
                  (dolist (entry active)
                    (cl-who:str (render-active-session entry)))))
              (cl-who:htm
                (:div :class "empty-state"
                  "No active sessions in the last hour"))))

        ;; Session history
        (:div :class "sessions-section"
          (:h3 :class "stats-heading" "Recent Sessions")
          (if history
              (cl-who:htm
                (:div :class "session-history"
                  (:div :class "session-history-header"
                    (:span "Session")
                    (:span "Tasks")
                    (:span "Events")
                    (:span "Duration")
                    (:span "Last Seen")
                    (:span ""))
                  (dolist (entry history)
                    (cl-who:str (render-history-session entry)))))
              (cl-who:htm
                (:div :class "empty-state"
                  "No session history"))))))))

(defun render-sessions-page (&optional depot)
  "Render the sessions page with depot tabs and auto-refresh."
  (htm-str
    (:div :class "page-wrapper"
      (cl-who:str (render-depot-tabs depot "/sessions"))
      (cl-who:str (render-sessions depot)))))
