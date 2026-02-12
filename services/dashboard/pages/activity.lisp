;;;; KLI Dashboard — Activity page (real data)

(in-package :kli-dashboard)

;;; ============================================================
;;; EVENT HELPERS
;;; ============================================================

(defun event-category (type)
  "Map event type keyword to display category."
  (let ((name (if (keywordp type) (symbol-name type) (string type))))
    (cond ((search "SESSION" name) "session")
          ((search "OBSERVATION" name) "observation")
          ((search "HANDOFF" name) "handoff")
          (t "task"))))

(defun event-icon-char (category)
  (cond ((string= category "session") "S")
        ((string= category "observation") "O")
        ((string= category "handoff") "H")
        (t "T")))

(defun event-type-label (type)
  "Human-readable label for event type."
  (let ((name (if (keywordp type) (symbol-name type) (string type))))
    (cond ((string= name "SESSION.JOIN") "Session joined")
          ((string= name "SESSION.LEAVE") "Session left")
          ((string= name "SESSION.CLAIM") "Task claimed")
          ((string= name "OBSERVATION") "Observation")
          ((string= name "HANDOFF.CREATE") "Handoff created")
          ((search "LINK" name) "Edge created")
          ((search "SEVER" name) "Edge removed")
          ((search "COMPLETE" name) "Task completed")
          ((search "METADATA" name) "Metadata set")
          ((search "TASK.CREATE" name) "Task created")
          (t (string-downcase name)))))

(defun format-time (universal-time)
  "Format universal-time as HH:MM."
  (if (and universal-time (numberp universal-time) (> universal-time 0))
    (multiple-value-bind (s m h) (decode-universal-time universal-time)
      (declare (ignore s))
      (format nil "~2,'0D:~2,'0D" h m))
    "??:??"))

(defun format-date (universal-time)
  "Format universal-time as YYYY-MM-DD."
  (if (and universal-time (numberp universal-time) (> universal-time 0))
    (multiple-value-bind (s m h day month year) (decode-universal-time universal-time)
      (declare (ignore s m h))
      (format nil "~D-~2,'0D-~2,'0D" year month day))
    "unknown"))

(defun session-color (session-id)
  "Generate consistent color for a session ID."
  (if (and session-id (stringp session-id) (> (length session-id) 0))
    (let ((hash (sxhash session-id)))
      (format nil "hsl(~D, 60%, 65%)" (mod hash 360)))
    "#5a5a6e"))

(defun event-detail-text (data)
  "Extract a detail string from event data plist."
  (when (listp data)
    (or (getf data :text)
        (getf data :summary)
        (getf data :path)
        (let ((target (getf data :target-id))
              (edge (getf data :edge-type)))
          (when target
            (format nil "~@[~A ~]~A" edge target))))))

;;; ============================================================
;;; ACTIVITY PAGE
;;; ============================================================

(defun render-event-row (event)
  "Render a single event row."
  (let* ((task-id (getf event :task))
         (type (getf event :type))
         (timestamp (getf event :timestamp))
         (session (getf event :session))
         (data (getf event :data))
         (category (event-category type))
         (detail (event-detail-text data))
         (scolor (session-color session)))
    (htm-str
      (:div :class "event-row"
        (:div :class (format nil "event-icon ~A" category)
          (cl-who:str (event-icon-char category)))
        (:div :class "event-body"
          (:div :class "event-title"
            (cl-who:str (event-type-label type)) " in "
            (:a :href (format nil "/task?id=~A" task-id)
              (cl-who:str (or task-id "unknown"))))
          (when (and detail (> (length detail) 0))
            (cl-who:htm
              (:div :class "event-detail"
                (cl-who:str (truncate-string detail 120))))))
        (:div :class "event-meta"
          (:span :class "event-time" (cl-who:str (format-time timestamp)))
          (when session
            (cl-who:htm
              (:span :class "event-session"
                :style (format nil "background: ~A22; color: ~A" scolor scolor)
                (cl-who:str (subseq session 0 (min 6 (length session))))))))))))

(defun group-events-by-day (events)
  "Group events into ((date . events) ...) alist, newest first."
  (let ((groups nil))
    (dolist (ev events)
      (let* ((date (format-date (getf ev :timestamp)))
             (existing (assoc date groups :test #'string=)))
        (if existing
            (push ev (cdr existing))
            (push (cons date (list ev)) groups))))
    ;; Sort groups newest first
    (sort groups #'string> :key #'car)))

(defun render-activity ()
  "Render the activity page with real event data."
  (let* ((events (get-recent-events :limit 200))
         (day-groups (when events (group-events-by-day events)))
         (total (length events))
         (sessions (length (remove-duplicates
                             (mapcar (lambda (e) (getf e :session)) events)
                             :test #'equal)))
         (days (length day-groups))
         (obs-count (count-if (lambda (e)
                                (string= "observation" (event-category (getf e :type))))
                              events))
         (handoff-count (count-if (lambda (e)
                                    (string= "handoff" (event-category (getf e :type))))
                                  events)))
    (htm-str
      (:div :style "max-width: 860px; margin: 0 auto; padding: 2rem;"
        ;; Summary bar
        (:div :class "activity-summary"
          (:div :class "activity-stat"
            (:div :class "count" (cl-who:fmt "~D" total))
            (:div :class "label" "events"))
          (:div :class "activity-stat"
            (:div :class "count" (cl-who:fmt "~D" sessions))
            (:div :class "label" "sessions"))
          (:div :class "activity-stat"
            (:div :class "count" (cl-who:fmt "~D" days))
            (:div :class "label" "days"))
          (:div :class "activity-stat"
            (:div :class "count" :style "color: var(--color-success);"
              (cl-who:fmt "~D" obs-count))
            (:div :class "label" "observations"))
          (:div :class "activity-stat"
            (:div :class "count" :style "color: var(--color-accent);"
              (cl-who:fmt "~D" handoff-count))
            (:div :class "label" "handoffs")))
        ;; Day groups
        (if day-groups
          (dolist (group day-groups)
            (let ((date (car group))
                  (day-events (cdr group)))
              (cl-who:htm
                (:div :class "day-group reveal"
                  (:div :class "day-header"
                    (cl-who:fmt "~A (~D events)" date (length day-events)))
                  (:div :style "display: flex; flex-direction: column; gap: 2px;"
                    (dolist (ev day-events)
                      (cl-who:str (render-event-row ev))))))))
          (cl-who:htm
            (:div :class "empty-state"
              "No events found. Is task-mcp running?")))))))

(defun render-activity-page (&optional depot)
  "Render the activity page with depot tabs and reveal script."
  (htm-str
    (:div :class "page-wrapper"
      (cl-who:str (render-depot-tabs depot "/activity"))
      (cl-who:str (render-activity))
      (cl-who:str (render-reveal-script)))))
