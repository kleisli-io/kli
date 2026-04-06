;;;; KLI Dashboard — Section renderers

(in-package :kli-dashboard)

(defun render-section (title tasks &key (renderer #'render-task-card))
  "Render a titled section with a list of task cards."
  (htm-str
    (:section :class "reveal" :style "margin-bottom: var(--space-block);"
      (:div :class "section-head"
        (:div :class "accent-line")
        (:span :class "section-label" (cl-who:str title))
        (:span :class "section-count" (cl-who:fmt "(~D)" (length tasks))))
      (if tasks
        (cl-who:htm
          (:div :style "display: flex; flex-direction: column; gap: 0.6rem;"
            (dolist (tk tasks)
              (cl-who:str (funcall renderer tk)))))
        (cl-who:htm
          (:div :class "empty-state" "No tasks in this section"))))))

(defun render-ready-section (tasks-by-topic)
  "Render the Ready to Work section with topic groups."
  (htm-str
    (:section :class "reveal" :style "margin-bottom: var(--space-block);"
      (:div :class "section-head"
        (:div :class "accent-line")
        (:span :class "section-label" "ready to work")
        (:span :class "section-count"
          (cl-who:fmt "(~D)" (reduce #'+ tasks-by-topic
                                :key (lambda (g) (length (cdr g)))
                                :initial-value 0))))
      (if tasks-by-topic
        (dolist (group tasks-by-topic)
          (let ((topic (car group))
                (tasks (cdr group)))
            (cl-who:htm
              (:div :class "topic-group stagger"
                (:h3 :class "topic-name"
                  :style "color: var(--color-accent-dim)"
                  (cl-who:str topic)
                  (cl-who:fmt " (~D)" (length tasks)))
                (:div :class "topic-cards"
                  (dolist (tk tasks)
                    (cl-who:str (render-ready-card tk))))))))
        (cl-who:htm
          (:div :class "empty-state" "No tasks ready"))))))

(defun render-depot-tabs (active-depot available-depots)
  "Render depot filter tabs."
  (htm-str
    (:div :class "depot-tabs"
      (:a :href "/"
          :class (format nil "depot-tab~@[ active~]" (null active-depot))
          "all depots")
      (dolist (d available-depots)
        (cl-who:htm
          (:a :href (format nil "/?depot=~A" d)
              :class (format nil "depot-tab~@[ active~]"
                       (and active-depot (string-equal d active-depot)))
              (cl-who:str d)))))))
