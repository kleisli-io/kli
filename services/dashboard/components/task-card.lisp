;;;; KLI Dashboard — Task card components

(in-package :kli-dashboard)

(defun status-class (status)
  (case status
    (:event-sourced "event-sourced")
    (:completed "completed")
    (:dormant "dormant")
    (t "dormant")))

(defun border-class (status)
  (case status
    (:event-sourced "border-sourced")
    (:completed "")
    (t "")))

(defun truncate-string (s max)
  (if (and s (> (length s) max))
      (concatenate 'string (subseq s 0 max) "...")
      (or s "")))

(defun render-task-card (task)
  "Render a full task card for the Pick Up section."
  (let ((id (getf task :id))
        (name (or (getf task :display-name) (getf task :bare-id) "Untitled"))
        (status (getf task :status))
        (depot (getf task :depot))
        (topic (getf task :topic))
        (goals (getf task :goals))
        (goal-count (or (getf task :goal-count) 0))
        (goals-done (or (getf task :goals-done) 0)))
    (htm-str
      (:div :class (format nil "task-card stagger ~A" (border-class status))
        ;; Header
        (:div :class "card-header"
          (:span :class (format nil "status-pip ~A" (status-class status)))
          (:a :href (format nil "/task?id=~A" id) :class "card-title"
            (cl-who:str name))
          (:span :class "tag" (cl-who:str depot)))
        ;; Goals progress
        (when (> goal-count 0)
          (cl-who:htm
            (:div :class "progress-track"
              (:div :class "progress-fill"
                :style (format nil "width: ~D%"
                         (if (zerop goal-count) 0
                             (round (* 100 (/ goals-done goal-count)))))))))
        ;; Footer
        (:div :class "card-footer"
          (:div :style "display: flex; align-items: center; gap: 0.75rem;"
            (when topic
              (cl-who:htm
                (:span :class "card-topic" (cl-who:str topic))))
            (:div :class "file-dots"
              (when (getf task :has-plan)
                (cl-who:htm (:span :class "file-dot plan" :title "plan")))
              (when (getf task :has-events)
                (cl-who:htm (:span :class "file-dot events" :title "events")))
              (when (getf task :has-handoffs)
                (cl-who:htm (:span :class "file-dot handoffs" :title "handoffs")))
              (when (getf task :has-research)
                (cl-who:htm (:span :class "file-dot research" :title "research")))))
          (when (> goal-count 0)
            (cl-who:htm
              (:span :class "card-stats"
                (cl-who:fmt "~D/~D goals" goals-done goal-count)))))))))

(defun render-ready-card (task)
  "Render a minimal card for the Ready section."
  (htm-str
    (:div :class "ready-card"
      (:a :href (format nil "/task?id=~A" (getf task :id))
        (cl-who:str (or (getf task :display-name)
                        (getf task :bare-id)
                        "Untitled"))))))
