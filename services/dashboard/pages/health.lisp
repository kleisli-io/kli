;;;; KLI Dashboard — Health page (real data)

(in-package :kli-dashboard)

(defun health-severity (count)
  (cond ((zerop count) :ok)
        ((< count 5) :warn)
        (t :error)))

(defun render-health-card (title icon count description)
  "Render a single health diagnostic card."
  (let ((severity (health-severity count)))
    (htm-str
      (:div :class (format nil "health-section stagger ~(~A~)" severity)
        (:div :class "health-header"
          (:span :class "health-icon" (cl-who:str icon))
          (:div :class "health-title"
            (:h3 (cl-who:fmt "~A (~D)" title count))
            (:div :class "health-desc" (cl-who:str description)))
          (:div :class "health-badge"
            :style (format nil "color: ~A; border-color: ~A"
                     (case severity (:ok "var(--color-success)") (:warn "var(--color-accent)") (:error "var(--color-error)"))
                     (case severity (:ok "var(--color-success)") (:warn "var(--color-accent)") (:error "var(--color-error)")))
            (if (zerop count) (cl-who:str "OK") (cl-who:fmt "~D" count))))))))

(defun render-health ()
  "Render the health page with real diagnostic data from task-mcp."
  (let* ((data (get-health-data))
         (total-issues (if data
                          (+ (or (getf data :stale-tasks) 0)
                             (or (getf data :dead-ends) 0)
                             (or (getf data :premature) 0))
                          0))
         (unexplored (or (and data (getf data :unexplored)) 0))
         (suggested (or (and data (getf data :suggested)) 0)))
    (htm-str
      (:div :style "max-width: 860px; margin: 0 auto; padding: 2rem;"
        ;; Summary
        (:div :class "health-summary"
          (:div :class "health-status"
            :style (format nil "color: ~A;"
                     (cond ((zerop total-issues) "var(--color-success)")
                           ((< total-issues 5) "var(--color-accent)")
                           (t "var(--color-error)")))
            (:span (cond ((zerop total-issues) (cl-who:str "All Clear"))
                         ((< total-issues 5) (cl-who:str "Minor Issues"))
                         (t (cl-who:str "Needs Attention")))))
          (:div :class "health-stats"
            (cl-who:htm
              (:span (cl-who:fmt "~D issues" total-issues))
              (:span (cl-who:fmt "~D unexplored" unexplored))
              (:span (cl-who:fmt "~D suggested" suggested)))))
        ;; Diagnostic cards
        (:div :class "reveal"
          (if data
            (cl-who:htm
              (cl-who:str (render-health-card "Stale Tasks" "&#x1F4A4;"
                            (getf data :stale-tasks)
                            "Created >3 days ago with minimal observations"))
              (cl-who:str (render-health-card "Dead Ends" "&#x1F6D1;"
                            (getf data :dead-ends)
                            "Active tasks with no events for >7 days"))
              (cl-who:str (render-health-card "Unlinked Roots" "&#x1F517;"
                            (getf data :orphans)
                            "Active tasks with no incoming edges"))
              (cl-who:str (render-health-card "Stale Claims" "&#x1F512;"
                            (getf data :stale-claims)
                            "Claimed but no activity for >4 hours"))
              (cl-who:str (render-health-card "Premature Completions" "&#x26A0;"
                            (getf data :premature)
                            "Marked complete but have incomplete children"))
              (cl-who:str (render-health-card "Convergent Clusters" "&#x1F300;"
                            (getf data :clusters)
                            "Topic groups with unlinked active tasks"))
              (cl-who:str (render-health-card "Suggested Edges" "&#x1F4A1;"
                            (getf data :suggested)
                            "Bidirectional references without declared edges"))
              (cl-who:str (render-health-card "Unexplored Frontier" "&#x1F30D;"
                            (getf data :unexplored)
                            "Active, unclaimed tasks available for work")))
            (cl-who:htm
              (:div :class "empty-state"
                "Could not connect to task-mcp for health data"))))))))

(defun render-health-page (&optional depot)
  "Render the health page with depot tabs and reveal script."
  (htm-str
    (:div :class "page-wrapper"
      (cl-who:str (render-depot-tabs depot "/health"))
      (cl-who:str (render-health))
      (cl-who:str (render-reveal-script)))))
