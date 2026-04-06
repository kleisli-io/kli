;;;; KLI Dashboard — Metrics bar component

(in-package :kli-dashboard)

(defun render-metrics (event-sourced completed dormant total)
  "Render the metrics bar with task counts."
  (htm-str
    (:div :class "metrics-bar"
      (:div :class "metric"
        (:span :class "metric-value" :style "color: var(--color-accent);"
          (cl-who:fmt "~D" (or event-sourced 0)))
        (:span :class "metric-label" "active"))
      (:div :class "metric"
        (:span :class "metric-value" :style "color: var(--color-success);"
          (cl-who:fmt "~D" (or completed 0)))
        (:span :class "metric-label" "completed"))
      (:div :class "metric"
        (:span :class "metric-value"
          (cl-who:fmt "~D" (or dormant 0)))
        (:span :class "metric-label" "dormant"))
      (:div :class "metric"
        (:span :class "metric-value"
          (cl-who:fmt "~D" (or total 0)))
        (:span :class "metric-label" "total")))))
