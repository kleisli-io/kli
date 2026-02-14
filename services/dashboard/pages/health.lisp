;;;; KLI Dashboard — Health page (real data)

(in-package :kli-dashboard)

(defun health-severity (count)
  (cond ((zerop count) :ok)
        ((< count 5) :warn)
        (t :error)))

(defun render-health-items (items &key format-fn)
  "Render a list of task IDs (or custom items via FORMAT-FN) as expandable content."
  (when items
    (htm-str
      (:ul :class "health-items"
        (dolist (item items)
          (cl-who:htm
            (:li :class "health-item"
              (if format-fn
                  (cl-who:str (funcall format-fn item))
                  (cl-who:htm
                    (:a :href (format nil "/task?id=~A" item)
                        :class "health-item-link"
                      (cl-who:str (task-display-name item))))))))))))

(defun task-display-name (id)
  "Extract a short display name from a qualified task ID."
  (let ((pos (position #\: id)))
    (if pos (subseq id (1+ pos)) id)))

(defun render-suggested-item (item)
  "Render a suggested-edge item with accept/dismiss actions."
  (let ((from (getf item :from))
        (to (getf item :to))
        (edge-type (string-downcase (symbol-name (or (getf item :suggested-type) :related-to)))))
    (htm-str
      (:div :class "health-suggested"
        (:div :class "health-suggested-pair"
          (:a :href (format nil "/task?id=~A" from)
              :class "health-item-link"
            (cl-who:str (task-display-name from)))
          (:span :class "health-arrow" " -> ")
          (:a :href (format nil "/task?id=~A" to)
              :class "health-item-link"
            (cl-who:str (task-display-name to)))
          (:span :class "health-edge-type" (cl-who:str edge-type)))
        (:div :class "health-actions"
          (:button :class "health-btn health-btn-accept"
                   :data-from from :data-to to :data-edge-type edge-type
                   :onclick "healthAcceptEdge(this)"
                   "Accept")
          (:button :class "health-btn health-btn-dismiss"
                   :data-from from :data-to to
                   :onclick "healthDismissEdge(this)"
                   "Dismiss"))))))

(defun render-health-card (title label count description items &key format-fn)
  "Render a single health diagnostic card with expandable item list."
  (let ((severity (health-severity count)))
    (htm-str
      (:div :class (format nil "health-section stagger ~(~A~)" severity)
        (if (and items (> count 0))
            (cl-who:htm
              (:details :class "health-details"
                (:summary :class "health-header"
                  (:span :class "health-label" (cl-who:str label))
                  (:div :class "health-title"
                    (:h3 (cl-who:fmt "~A (~D)" title count))
                    (:div :class "health-desc" (cl-who:str description)))
                  (:div :class "health-badge"
                    :style (format nil "color: ~A; border-color: ~A"
                             (case severity (:ok "var(--color-success)") (:warn "var(--color-accent)") (:error "var(--color-error)"))
                             (case severity (:ok "var(--color-success)") (:warn "var(--color-accent)") (:error "var(--color-error)")))
                    (cl-who:fmt "~D" count)))
                (:div :class "health-body"
                  (cl-who:str (render-health-items items :format-fn format-fn)))))
            (cl-who:htm
              (:div :class "health-header"
                (:span :class "health-label" (cl-who:str label))
                (:div :class "health-title"
                  (:h3 (cl-who:fmt "~A (~D)" title count))
                  (:div :class "health-desc" (cl-who:str description)))
                (:div :class "health-badge"
                  :style (format nil "color: ~A; border-color: ~A"
                           (case severity (:ok "var(--color-success)") (:warn "var(--color-accent)") (:error "var(--color-error)"))
                           (case severity (:ok "var(--color-success)") (:warn "var(--color-accent)") (:error "var(--color-error)")))
                  (cl-who:str "OK")))))))))

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
              (cl-who:str (render-health-card "Stale Tasks" "STALE"
                            (getf data :stale-tasks)
                            "Created >3 days ago with minimal observations"
                            (getf data :stale-items)))
              (cl-who:str (render-health-card "Dead Ends" "DEAD"
                            (getf data :dead-ends)
                            "Active tasks with no events for >7 days"
                            (getf data :dead-items)))
              (cl-who:str (render-health-card "Unlinked Roots" "UNLINKED"
                            (getf data :orphans)
                            "Active tasks with no incoming edges"
                            (getf data :orphan-items)))
              (cl-who:str (render-health-card "Stale Claims" "CLAIMED"
                            (getf data :stale-claims)
                            "Claimed but no activity for >4 hours"
                            (getf data :claim-items)))
              (cl-who:str (render-health-card "Premature Completions" "EARLY"
                            (getf data :premature)
                            "Marked complete but have incomplete children"
                            (getf data :premature-items)))
              (cl-who:str (render-health-card "Convergent Clusters" "CONVERGE"
                            (getf data :clusters)
                            "Topic groups with unlinked active tasks"
                            (getf data :cluster-items)))
              (cl-who:str (render-health-card "Suggested Edges" "SUGGEST"
                            (getf data :suggested)
                            "Bidirectional references without declared edges"
                            (getf data :suggested-items)
                            :format-fn #'render-suggested-item))
              (cl-who:str (render-health-card "Unexplored Frontier" "FRONTIER"
                            (getf data :unexplored)
                            "Active, unclaimed tasks available for work"
                            (getf data :frontier-items))))
            (cl-who:htm
              (:div :class "empty-state"
                "Could not connect to task-mcp for health data"))))))))

(defun render-health-scripts ()
  "Inline JS for health page edge actions."
  (htm-str
    (:script
      (cl-who:str "
function healthAcceptEdge(btn) {
  var from = btn.getAttribute('data-from');
  var to = btn.getAttribute('data-to');
  var edgeType = btn.getAttribute('data-edge-type');
  btn.disabled = true;
  btn.textContent = '...';
  fetch('/api/task/link', {
    method: 'POST',
    headers: {'Content-Type': 'application/json'},
    body: JSON.stringify({task_id: from, target_id: to, edge_type: edgeType})
  }).then(function(r) { return r.json(); })
    .then(function(d) {
      var item = btn.closest('.health-item');
      if (d.ok && item) item.style.display = 'none';
      else if (!d.ok) { btn.disabled = false; btn.textContent = 'Accept'; }
    }).catch(function() { btn.disabled = false; btn.textContent = 'Accept'; });
}
function healthDismissEdge(btn) {
  var from = btn.getAttribute('data-from');
  var to = btn.getAttribute('data-to');
  btn.disabled = true;
  btn.textContent = '...';
  fetch('/api/health/dismiss-edge', {
    method: 'POST',
    headers: {'Content-Type': 'application/json'},
    body: JSON.stringify({from: from, to: to})
  }).then(function(r) { return r.json(); })
    .then(function(d) {
      var item = btn.closest('.health-item');
      if (d.ok && item) item.style.display = 'none';
      else if (!d.ok) { btn.disabled = false; btn.textContent = 'Dismiss'; }
    }).catch(function() { btn.disabled = false; btn.textContent = 'Dismiss'; });
}
"))))

(defun render-health-page (&optional depot)
  "Render the health page with depot tabs and reveal script."
  (htm-str
    (:div :class "page-wrapper"
      (cl-who:str (render-depot-tabs depot "/health"))
      (cl-who:str (render-health))
      (cl-who:str (render-health-scripts))
      (cl-who:str (render-reveal-script)))))
