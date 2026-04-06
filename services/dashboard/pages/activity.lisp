;;;; KLI Dashboard — Activity page (real data)
;;;; Event stream with depot filtering, category filters, infinite scroll.

(in-package :kli-dashboard)

;;; ============================================================
;;; CONFIG
;;; ============================================================

(defparameter *activity-initial-limit* 50
  "Number of events to render on initial page load.")

;;; ============================================================
;;; EVENT HELPERS
;;; ============================================================

(defun event-category (type)
  "Map event type keyword to display category string."
  (let ((name (if (keywordp type) (symbol-name type) (string type))))
    (cond ((search "SESSION" name) "session")
          ((search "OBSERVATION" name) "observation")
          ((search "HANDOFF" name) "handoff")
          ((search "ARTIFACT" name) "artifact")
          (t "task"))))

(defun event-icon-char (category)
  (cond ((string= category "session") "S")
        ((string= category "observation") "O")
        ((string= category "handoff") "H")
        ((string= category "artifact") "A")
        (t "T")))

(defun event-type-label (type)
  "Human-readable label for event type."
  (let ((name (if (keywordp type) (symbol-name type) (string type))))
    (cond ((string= name "SESSION.JOIN") "Session joined")
          ((string= name "SESSION.LEAVE") "Session left")
          ((string= name "SESSION.CLAIM") "Task claimed")
          ((string= name "SESSION.RELEASE") "Session released")
          ((string= name "OBSERVATION") "Observation")
          ((string= name "HANDOFF.CREATE") "Handoff created")
          ((string= name "ARTIFACT.CREATE") "Artifact registered")
          ((string= name "TASK.CREATE") "Task created")
          ((string= name "TASK.FORK") "Task forked")
          ((string= name "TASK.SPAWN") "Task spawned")
          ((search "LINK" name) "Edge created")
          ((search "SEVER" name) "Edge removed")
          ((search "RECLASSIFY" name) "Edge reclassified")
          ((search "METADATA" name) "Metadata set")
          ((search "UPDATE-STATUS" name) "Status changed")
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
;;; STATS
;;; ============================================================

(defun compute-activity-stats (events)
  "Compute activity stats from event list.
   Returns plist with :total, :sessions, :days, and per-category counts."
  (let ((sessions (make-hash-table :test #'equal))
        (dates (make-hash-table :test #'equal))
        (categories (make-hash-table :test #'equal)))
    (dolist (ev events)
      (let ((date (format-date (getf ev :timestamp)))
            (cat (event-category (getf ev :type)))
            (session (getf ev :session)))
        (setf (gethash date dates) t)
        (incf (gethash cat categories 0))
        (when session (setf (gethash session sessions) t))))
    (list :total (length events)
          :sessions (hash-table-count sessions)
          :days (hash-table-count dates)
          :observations (gethash "observation" categories 0)
          :handoffs (gethash "handoff" categories 0))))

;;; ============================================================
;;; EVENT ROWS
;;; ============================================================

(defun render-event-row (event)
  "Render a single event row with data-category for client-side filtering."
  (let* ((task-id (getf event :task))
         (type (getf event :type))
         (timestamp (getf event :timestamp))
         (session (getf event :session))
         (data (getf event :data))
         (category (event-category type))
         (detail (event-detail-text data))
         (scolor (session-color session)))
    (htm-str
      (:div :class "event-row" :data-category category
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
                (cl-who:str (cl-who:escape-string (truncate-string detail 120)))))))
        (:div :class "event-meta"
          (:span :class "event-time" (cl-who:str (format-time timestamp)))
          (when session
            (cl-who:htm
              (:span :class "event-session"
                :style (format nil "background: ~A22; color: ~A" scolor scolor)
                (cl-who:str (subseq session 0 (min 6 (length session))))))))))))

(defun render-paginated-events (items day-counts last-date)
  "Render paginated event items grouped by day with proper structure.
   Produces .day-group > (.day-header + .event-list) for new days, matching
   the initial page structure. Events continuing LAST-DATE get a bare
   .event-list wrapper (no duplicate day header).
   DAY-COUNTS is a hash-table of date->total-count for real totals.
   Returns HTML string."
  (let ((day-groups (group-events-by-day items)))
    (with-output-to-string (s)
      (dolist (group day-groups)
        (let ((date (car group))
              (events (cdr group)))
          (if (string= date last-date)
              ;; Continuing same day as previous page — no header, just event-list
              (write-string
               (htm-str
                 (:div :class "event-list"
                   (dolist (ev events)
                     (cl-who:str (render-event-row ev)))))
               s)
              ;; New day — full .day-group with header + event-list
              (write-string
               (htm-str
                 (:div :class "day-group"
                   (:div :class "day-header"
                     (cl-who:fmt "~A (~D event~:P)"
                                 date (gethash date day-counts 0)))
                   (:div :class "event-list"
                     (dolist (ev events)
                       (cl-who:str (render-event-row ev))))))
               s)))))))

;;; ============================================================
;;; INFINITE SCROLL
;;; ============================================================

(defun render-load-more-sentinel (offset limit &optional category depot last-date)
  "Render the HTMX infinite scroll sentinel.
   Uses hx-trigger='revealed once' to load more when scrolled into view.
   LAST-DATE carries the date of the last event so the next page can avoid
   duplicate day headers at the boundary."
  (let* ((cat-str (or category "all"))
         (base-url (format nil "/api/activity/events?offset=~D&limit=~D&category=~A"
                           offset limit cat-str))
         (url (if depot
                  (format nil "~A&depot=~A" base-url depot)
                  base-url))
         (url (if (and last-date (> (length last-date) 0))
                  (format nil "~A&last_date=~A" url last-date)
                  url)))
    (htm-str
      (:div :id "load-more-sentinel"
            :class "load-more-sentinel"
            :hx-get url
            :hx-trigger "revealed once"
            :hx-swap "outerHTML"
        (:div :class "loading-indicator"
          "Loading more events...")))))

(defun group-events-by-day (events)
  "Group events into ((date . events) ...) alist, days newest first,
   events within each day preserving input order (newest first)."
  (let ((groups nil))
    (dolist (ev events)
      (let* ((date (format-date (getf ev :timestamp)))
             (existing (assoc date groups :test #'string=)))
        (if existing
            (push ev (cdr existing))
            (push (cons date (list ev)) groups))))
    ;; Reverse within each group to undo push accumulation
    (dolist (g groups)
      (setf (cdr g) (nreverse (cdr g))))
    (sort groups #'string> :key #'car)))

;;; ============================================================
;;; ACTIVITY PAGE
;;; ============================================================

(defun count-events-per-day (events)
  "Count total events per date from EVENTS. Returns hash-table date->count."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (ev events)
      (let ((date (format-date (getf ev :timestamp))))
        (incf (gethash date counts 0))))
    counts))

(defun render-activity (&optional depot)
  "Render the activity page with depot filtering, category filters, and infinite scroll."
  (let* ((all-events (filter-events-by-depot (get-all-events) depot))
         (items (subseq all-events 0 (min *activity-initial-limit* (length all-events))))
         (stats (compute-activity-stats all-events))
         (total (getf stats :total))
         (day-counts (count-events-per-day all-events))
         (day-groups (when items (group-events-by-day items))))
    (htm-str
      (:div :style "max-width: 860px; margin: 0 auto; padding: 2rem;"
        ;; Summary bar — stats from ALL depot-filtered events
        (:div :class "activity-summary"
          (:div :class "activity-stat"
            (:div :class "count"
              (if (> total (length items))
                  (cl-who:fmt "~D+" total)
                  (cl-who:fmt "~D" (length items))))
            (:div :class "label" "events"))
          (:div :class "activity-stat"
            (:div :class "count" (cl-who:fmt "~D" (getf stats :sessions)))
            (:div :class "label" "sessions"))
          (:div :class "activity-stat"
            (:div :class "count" (cl-who:fmt "~D" (getf stats :days)))
            (:div :class "label" "days"))
          (:div :class "activity-stat"
            (:div :class "count" :style "color: var(--color-success);"
              (cl-who:fmt "~D" (getf stats :observations)))
            (:div :class "label" "observations"))
          (:div :class "activity-stat"
            (:div :class "count" :style "color: var(--color-accent);"
              (cl-who:fmt "~D" (getf stats :handoffs)))
            (:div :class "label" "handoffs")))

        ;; Filter buttons
        (:div :class "activity-filters"
          (:button :class "filter-btn active" :onclick "filterEvents('all')"
            "All")
          (:button :class "filter-btn" :onclick "filterEvents('session')"
            "Sessions")
          (:button :class "filter-btn" :onclick "filterEvents('observation')"
            "Observations")
          (:button :class "filter-btn" :onclick "filterEvents('handoff')"
            "Handoffs")
          (:button :class "filter-btn" :onclick "filterEvents('task')"
            "Tasks")
          (:button :class "filter-btn" :onclick "filterEvents('artifact')"
            "Artifacts"))

        ;; Hidden filter state input
        (:input :type "hidden" :id "current-filter" :name "category" :value "all")

        ;; Day groups
        (if day-groups
          (dolist (group day-groups)
            (let* ((date (car group))
                   (day-events (cdr group))
                   (real-count (gethash date day-counts 0)))
              (cl-who:htm
                (:div :class "day-group reveal"
                  (:div :class "day-header"
                    (cl-who:fmt "~A (~D event~:P)" date real-count))
                  (:div :class "event-list"
                    (dolist (ev day-events)
                      (cl-who:str (render-event-row ev))))))))
          (cl-who:htm
            (:div :class "empty-state"
              "No events found. Is task-mcp running?")))

        ;; Infinite scroll sentinel
        (when (> total (length items))
          (let ((last-date (format-date (getf (car (last items)) :timestamp))))
            (cl-who:str (render-load-more-sentinel
                         (length items) *activity-initial-limit* nil depot last-date))))))))

(defun render-activity-page (&optional depot)
  "Render the activity page with depot tabs and reveal script."
  (htm-str
    (:div :class "page-wrapper"
      (cl-who:str (render-depot-tabs depot "/activity"))
      (cl-who:str (render-activity depot))
      (cl-who:str (render-reveal-script)))))
