;;;; KLI Dashboard — Stats page
;;;; Aggregate statistics with inline SVG charts.

(in-package :kli-dashboard)

;;; ============================================================
;;; SVG CHART HELPERS
;;; ============================================================

(defun render-svg-bar-chart (data &key (width 700) (height 180) (bar-gap 2))
  "Render an inline SVG bar chart from ((label . value) ...) alist.
   Labels are displayed every 3rd bar to avoid crowding."
  (let* ((n (length data))
         (max-val (if data (reduce #'max data :key #'cdr :initial-value 1) 1))
         (bar-width (if (> n 0) (max 4 (floor (/ (- width (* bar-gap (1- n))) n))) 20))
         (chart-height (- height 30)))
    (htm-str
      (:svg :xmlns "http://www.w3.org/2000/svg"
            :width "100%" :height (format nil "~D" height)
            :viewBox (format nil "0 0 ~D ~D" width height)
            :class "bar-chart"
            :style "display:block"
        (loop for (label . value) in data
              for i from 0
              for x = (* i (+ bar-width bar-gap))
              for bar-h = (max 1 (round (* chart-height (/ value max-val))))
              for y = (- chart-height bar-h)
              do (cl-who:htm
                   (:g
                     (:title (cl-who:fmt "~A: ~:D events" label value))
                     (:rect :x (format nil "~D" x)
                            :y (format nil "~D" y)
                            :width (format nil "~D" bar-width)
                            :height (format nil "~D" bar-h)
                            :fill "var(--color-accent)"
                            :rx "1"))
                   (when (zerop (mod i (max 1 (ceiling n 10))))
                     (cl-who:htm
                       (:text :x (format nil "~D" (+ x (floor bar-width 2)))
                              :y (format nil "~D" (- height 2))
                              :text-anchor "middle"
                              :fill "var(--color-muted)"
                              :font-size "10"
                              :font-family "var(--font-mono)"
                              (cl-who:str (subseq label (min 5 (length label)))))))))))))

(defun render-status-ring (completed active dormant &key (size 120))
  "Render a donut chart showing task status breakdown."
  (let* ((total (+ completed active dormant))
         (r 50) (cx 60) (cy 60) (stroke 12)
         (circumference (* 2 pi r))
         (completed-pct (if (> total 0) (/ completed total) 0))
         (active-pct (if (> total 0) (/ active total) 0))
         (dormant-pct (if (> total 0) (/ dormant total) 0))
         (completed-len (* circumference completed-pct))
         (active-len (* circumference active-pct))
         (dormant-len (* circumference dormant-pct))
         (active-offset (- circumference completed-len))
         (dormant-offset (- circumference completed-len active-len)))
    (htm-str
      (:svg :xmlns "http://www.w3.org/2000/svg"
            :width (format nil "~D" size) :height (format nil "~D" size)
            :viewBox "0 0 120 120"
            :class "status-ring"
        ;; Background ring
        (:circle :cx (format nil "~D" cx) :cy (format nil "~D" cy)
                 :r (format nil "~D" r)
                 :fill "none" :stroke "var(--color-border)"
                 :stroke-width (format nil "~D" stroke))
        ;; Completed (green)
        (when (> completed 0)
          (cl-who:htm
            (:circle :cx (format nil "~D" cx) :cy (format nil "~D" cy)
                     :r (format nil "~D" r)
                     :fill "none" :stroke "var(--color-success)"
                     :stroke-width (format nil "~D" stroke)
                     :stroke-dasharray (format nil "~,1F ~,1F" completed-len
                                               (- circumference completed-len))
                     :stroke-dashoffset "0"
                     :transform (format nil "rotate(-90 ~D ~D)" cx cy))))
        ;; Active (accent)
        (when (> active 0)
          (cl-who:htm
            (:circle :cx (format nil "~D" cx) :cy (format nil "~D" cy)
                     :r (format nil "~D" r)
                     :fill "none" :stroke "var(--color-accent)"
                     :stroke-width (format nil "~D" stroke)
                     :stroke-dasharray (format nil "~,1F ~,1F" active-len
                                               (- circumference active-len))
                     :stroke-dashoffset (format nil "~,1F" active-offset)
                     :transform (format nil "rotate(-90 ~D ~D)" cx cy))))
        ;; Dormant (muted)
        (when (> dormant 0)
          (cl-who:htm
            (:circle :cx (format nil "~D" cx) :cy (format nil "~D" cy)
                     :r (format nil "~D" r)
                     :fill "none" :stroke "var(--color-muted)"
                     :stroke-width (format nil "~D" stroke)
                     :stroke-dasharray (format nil "~,1F ~,1F" dormant-len
                                               (- circumference dormant-len))
                     :stroke-dashoffset (format nil "~,1F" dormant-offset)
                     :transform (format nil "rotate(-90 ~D ~D)" cx cy))))
        ;; Center text
        (:text :x (format nil "~D" cx) :y (format nil "~D" (- cy 4))
               :text-anchor "middle"
               :fill "var(--color-heading)"
               :font-size "20" :font-family "var(--font-heading)"
               (cl-who:fmt "~D" total))
        (:text :x (format nil "~D" cx) :y (format nil "~D" (+ cy 12))
               :text-anchor "middle"
               :fill "var(--color-muted)"
               :font-size "10" :font-family "var(--font-mono)"
               "tasks")))))

;;; ============================================================
;;; CUMULATIVE AREA CHART
;;; ============================================================

(defun render-svg-cumulative-chart (series &key (width 700) (height 200))
  "Render inline SVG area chart with overlapping cumulative series.
   SERIES: ((name color ((date . value) ...)) ...)"
  (let* ((ml 45) (mr 40) (mt 16) (mb 25)
         (cw (- width ml mr)) (ch (- height mb mt))
         (max-val (max 1 (loop for (nil nil data) in series
                                when data
                                maximize (loop for (nil . v) in data
                                               maximize v))))
         (dates (mapcar #'car (third (first series))))
         (n (length dates)))
    (flet ((xp (i) (+ ml (if (> n 1)
                              (round (* cw (/ i (1- n))))
                              (floor cw 2))))
           (yp (v) (- height mb (round (* ch (/ v max-val))))))
      (htm-str
        (:svg :xmlns "http://www.w3.org/2000/svg"
              :width "100%" :height (format nil "~D" height)
              :viewBox (format nil "0 0 ~D ~D" width height)
              :class "cumulative-chart" :style "display:block"
          ;; Grid lines
          (loop for g from 0 to 4
                for val = (round (* max-val (/ g 4)))
                for y = (yp val)
                do (cl-who:htm
                     (:line :x1 (format nil "~D" ml)
                            :y1 (format nil "~D" y)
                            :x2 (format nil "~D" (- width mr))
                            :y2 (format nil "~D" y)
                            :stroke "var(--color-border)"
                            :stroke-dasharray "2,4")
                     (:text :x (format nil "~D" (- ml 4))
                            :y (format nil "~D" (+ y 3))
                            :text-anchor "end"
                            :fill "var(--color-muted)"
                            :font-size "9" :font-family "var(--font-mono)"
                            (cl-who:fmt "~D" val))))
          ;; Area fills + line strokes
          (dolist (s series)
            (destructuring-bind (name color data) s
              (declare (ignore name))
              (when data
                (let ((pts (loop for (nil . v) in data for i from 0
                                 collect (format nil "~D,~D" (xp i) (yp v))))
                      (last-v (cdr (car (last data))))
                      (last-i (1- (length data))))
                  (cl-who:htm
                    ;; Filled area
                    (:polygon
                      :points (format nil "~D,~D ~{~A ~}~D,~D"
                                      (xp 0) (yp 0) pts (xp last-i) (yp 0))
                      :fill color :fill-opacity "0.12" :stroke "none")
                    ;; Line
                    (:polyline
                      :points (format nil "~{~A ~}" pts)
                      :fill "none" :stroke color
                      :stroke-width "2" :stroke-linejoin "round")
                    ;; End value label
                    (:text :x (format nil "~D" (+ (xp last-i) 4))
                           :y (format nil "~D" (yp last-v))
                           :fill color
                           :font-size "10" :font-family "var(--font-mono)"
                           (cl-who:fmt "~D" last-v)))))))
          ;; X-axis date labels
          (let ((step (max 1 (ceiling n 8))))
            (loop for date in dates for i from 0
                  when (zerop (mod i step))
                  do (cl-who:htm
                       (:text :x (format nil "~D" (xp i))
                              :y (format nil "~D" (- height 4))
                              :text-anchor "middle"
                              :fill "var(--color-muted)"
                              :font-size "9" :font-family "var(--font-mono)"
                              (cl-who:str (subseq date (min 5 (length date))))))))
          ;; Legend
          (loop for (name color nil) in series for li from 0
                for lx = (+ ml (* li 120))
                do (cl-who:htm
                     (:rect :x (format nil "~D" lx) :y "3"
                            :width "12" :height "3"
                            :fill color :rx "1")
                     (:text :x (format nil "~D" (+ lx 16)) :y "8"
                            :fill "var(--color-muted)"
                            :font-size "9" :font-family "var(--font-mono)"
                            (cl-who:str name)))))))))

;;; ============================================================
;;; STATS PAGE
;;; ============================================================

(defun format-event-type-name (type)
  "Format event type keyword as readable string."
  (let ((name (string-downcase (symbol-name type))))
    (substitute #\Space #\. name)))

(defun render-stats (&optional depot)
  "Render the stats page content."
  (let* ((data (get-stats-data depot))
         (daily (getf data :daily))
         (types (getf data :types))
         (depots (getf data :depots))
         (top-tasks (getf data :top-tasks)))
    (htm-str
      (:div :style "max-width: 900px; margin: 0 auto; padding: 2rem;"

        ;; Summary row
        (:div :class "stats-summary"
          (:div :class "stats-card"
            (:div :class "stats-number" (cl-who:fmt "~:D" (getf data :total-events)))
            (:div :class "stats-label" "total events"))
          (:div :class "stats-card"
            (:div :class "stats-number" (cl-who:fmt "~D" (getf data :total-tasks)))
            (:div :class "stats-label" "tasks"))
          (:div :class "stats-card"
            (:div :class "stats-number" (cl-who:fmt "~D" (getf data :session-count)))
            (:div :class "stats-label" "sessions"))
          (:div :class "stats-card"
            (:div :class "stats-number" (cl-who:fmt "~D" (length daily)))
            (:div :class "stats-label" "active days")))

        ;; Events per day chart
        (:div :class "stats-section"
          (:h3 :class "stats-heading" "Events per Day")
          (if daily
              (cl-who:str (render-svg-bar-chart daily))
              (cl-who:htm (:div :class "empty-state" "No event data"))))

        ;; Cumulative progress
        (let ((cum-c (getf data :cumulative-creates))
              (cum-co (getf data :cumulative-completes)))
          (when (and cum-c (> (length cum-c) 1))
            (cl-who:htm
              (:div :class "stats-section"
                (:h3 :class "stats-heading" "Cumulative Progress")
                (cl-who:str
                  (render-svg-cumulative-chart
                    (list (list "created" "var(--color-accent)" cum-c)
                          (list "completed" "var(--color-success)" cum-co))))))))

        ;; Two-column layout: status ring + depot breakdown
        (:div :class "stats-columns"
          ;; Task status
          (:div :class "stats-section"
            (:h3 :class "stats-heading" "Task Status")
            (:div :class "stats-ring-row"
              (cl-who:str (render-status-ring
                            (getf data :completed)
                            (getf data :active)
                            (getf data :dormant)))
              (:div :class "stats-legend"
                (:div :class "legend-item"
                  (:span :class "legend-dot" :style "background:var(--color-success)")
                  (cl-who:fmt "~D completed" (getf data :completed)))
                (:div :class "legend-item"
                  (:span :class "legend-dot" :style "background:var(--color-accent)")
                  (cl-who:fmt "~D active" (getf data :active)))
                (:div :class "legend-item"
                  (:span :class "legend-dot" :style "background:var(--color-muted)")
                  (cl-who:fmt "~D dormant" (getf data :dormant))))))

          ;; Per-depot breakdown
          (when (> (length depots) 1)
            (cl-who:htm
              (:div :class "stats-section"
                (:h3 :class "stats-heading" "Events by Depot")
                (:div :class "stats-table"
                  (dolist (entry depots)
                    (let* ((dep (car entry))
                           (count (cdr entry))
                           (pct (if (> (getf data :total-events) 0)
                                    (round (* 100 (/ count (getf data :total-events))))
                                    0)))
                      (cl-who:htm
                        (:div :class "stats-table-row"
                          (:span :class "stats-table-label" (cl-who:str dep))
                          (:div :class "stats-bar-track"
                            (:div :class "stats-bar-fill"
                              :style (format nil "width:~D%" pct)))
                          (:span :class "stats-table-value"
                            (cl-who:fmt "~:D" count)))))))))))

        ;; Event types
        (:div :class "stats-section"
          (:h3 :class "stats-heading" "Event Types")
          (:div :class "stats-table"
            (dolist (entry (subseq types 0 (min 10 (length types))))
              (let* ((type (car entry))
                     (count (cdr entry))
                     (pct (if (> (getf data :total-events) 0)
                              (round (* 100 (/ count (getf data :total-events))))
                              0)))
                (cl-who:htm
                  (:div :class "stats-table-row"
                    (:span :class "stats-table-label"
                      (cl-who:str (format-event-type-name type)))
                    (:div :class "stats-bar-track"
                      (:div :class "stats-bar-fill"
                        :style (format nil "width:~D%" pct)))
                    (:span :class "stats-table-value"
                      (cl-who:fmt "~:D" count))))))))

        ;; Top tasks
        (:div :class "stats-section"
          (:h3 :class "stats-heading" "Most Active Tasks"
            " " (:span :class "count" "(by events)"))
          (:div :class "stats-table"
            (dolist (entry top-tasks)
              (let ((id (car entry))
                    (count (cdr entry)))
                (cl-who:htm
                  (:div :class "stats-table-row"
                    (:a :href (format nil "/task?id=~A" id)
                        :class "stats-table-label stats-task-link"
                      (cl-who:str (humanize-task-name id)))
                    (:span :class "stats-table-value"
                      :style "margin-left:auto"
                      (cl-who:fmt "~:D" count))))))))

        ;; Session summary
        (:div :class "stats-section"
          (:h3 :class "stats-heading" "Sessions")
          (:div :class "stats-summary" :style "margin-top: 0.5rem"
            (:div :class "stats-card"
              (:div :class "stats-number" (cl-who:fmt "~D" (getf data :session-count)))
              (:div :class "stats-label" "total"))
            (:div :class "stats-card"
              (:div :class "stats-number" :style "color:var(--color-accent)"
                (cl-who:fmt "~D" (getf data :builders)))
              (:div :class "stats-label" "builders"))
            (:div :class "stats-card"
              (:div :class "stats-number"
                (cl-who:fmt "~D" (getf data :observers)))
              (:div :class "stats-label" "observers"))))))))

(defun render-stats-page (&optional depot)
  "Render the stats page with depot tabs."
  (htm-str
    (:div :class "page-wrapper"
      (cl-who:str (render-depot-tabs depot "/stats"))
      (cl-who:str (render-stats depot)))))
