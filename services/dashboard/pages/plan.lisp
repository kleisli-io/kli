;;;; KLI Dashboard — Plan Page
;;;; Plan index (tasks with children) and plan detail with:
;;;;  - D3 DAG visualization (collapsible)
;;;;  - Frontier phases shown first (ready + in-progress)
;;;;  - Completed phases collapsed by default
;;;;  - Pending phases after frontier

(in-package :kli-dashboard)

;;; ============================================================
;;; PLAN INDEX (list of tasks with plan structure)
;;; ============================================================

(defun render-plan-index-card (p)
  "Render a single plan index card."
  (let* ((total (or (getf p :phases) 0))
         (done (or (getf p :done) 0))
         (pct (if (> total 0) (round (* 100 (/ done total))) 0)))
    (htm-str
      (:a :href (format nil "/plan?id=~A" (getf p :id))
          :class "plan-index-card"
        (:div :class (format nil "plan-index-status ~A"
                       (cond ((= done total) "completed")
                             ((> done 0) "ready")
                             (t "pending")))
          (:span :class "count"
            :style (format nil "color: ~A"
                     (cond ((= done total) "var(--color-success)")
                           ((> done 0) "#58a6ff")
                           (t "var(--color-muted)")))
            (cl-who:fmt "~D/~D" done total)))
        (:div :class "plan-index-info"
          (:div :class "plan-index-name"
            (cl-who:str (or (getf p :display-name)
                            (humanize-task-name (getf p :id)))))
          (:div :class "plan-index-meta"
            (cl-who:fmt "~D phase~:P" total)
            (when (getf p :date)
              (cl-who:htm
                (cl-who:fmt " &middot; ~A" (getf p :date))))))
        (:div :class "plan-index-bar"
          (:div :class "plan-progress-bar"
            (:div :class "plan-progress-fill"
              :style (format nil "width: ~D%" pct))))))))

(defun render-plan-index ()
  "Render listing of all tasks that have plan structure."
  (let* ((plans (plan-index-data))
         (active (remove-if (lambda (p)
                              (let ((total (or (getf p :phases) 0))
                                    (done (or (getf p :done) 0)))
                                (and (> total 0) (= done total))))
                            plans))
         (completed (remove-if-not (lambda (p)
                                     (let ((total (or (getf p :phases) 0))
                                           (done (or (getf p :done) 0)))
                                       (and (> total 0) (= done total))))
                                   plans)))
    (htm-str
      (:div :class "page-wrapper"
        (:div :class "plan-header"
          (:h1 :class "plan-title" "Plans"))
        (if plans
            (cl-who:htm
              ;; Active plans
              (when active
                (cl-who:htm
                  (:div :class "frontier-section"
                    (:h2 "Active Plans"
                      (:span :class "count"
                        (cl-who:fmt "(~D)" (length active))))
                    (:div :class "plan-phases"
                      (dolist (p active)
                        (cl-who:str (render-plan-index-card p)))))))
              ;; Completed plans — collapsed by default
              (when completed
                (cl-who:htm
                  (:div :class "plan-phases collapsed"
                    (:div :class "plan-phases-header"
                      :style "cursor:pointer"
                      :onclick "this.parentElement.classList.toggle('collapsed');this.querySelector('.toggle-icon').textContent=this.parentElement.classList.contains('collapsed')?'\\u25B8':'\\u25BE'"
                      (:h2 (cl-who:fmt "Completed (~D)" (length completed))
                        (:span :class "toggle-icon"
                          :style "margin-left:8px;font-size:10px;color:var(--color-muted)"
                          "&#x25B8;"))
                      (:span :style "font-family:var(--font-mono);font-size:0.6875rem;color:var(--color-success)"
                        "all phases done"))
                    (:div :class "plan-completed-body"
                      (dolist (p completed)
                        (cl-who:str (render-plan-index-card p))))))))
            (cl-who:htm
              (:div :class "empty-state"
                "No tasks with plan structure found.")))))))

;;; ============================================================
;;; PHASE CARD RENDERER
;;; ============================================================

(defun render-phase-card (phase &key (initially-open nil))
  "Render an expandable phase card. PHASE is a plist from plan-detail-data."
  (let* ((child-id (getf phase :id))
         (status-str (or (getf phase :status) "pending"))
         (is-ready (getf phase :ready))
         (display (getf phase :display))
         (description (getf phase :description))
         (obs-count (or (getf phase :obs-count) 0))
         (artifact-count (or (getf phase :artifact-count) 0))
         (session-count (or (getf phase :session-count) 0))
         (deps (getf phase :deps))
         (edges (getf phase :edges))
         (css-status (cond ((string-equal status-str "completed") "completed")
                           (is-ready "ready")
                           (t "pending")))
         (icon (cond ((string-equal status-str "completed") "&#x2713;")
                     (is-ready "&#x25B6;")
                     (t "&#x25CB;")))
         (safe-id (css-safe-id child-id)))
    (htm-str
      (:div :class (format nil "phase-card~@[ open~]" initially-open)
            :id (format nil "phase-~A" safe-id)
            :data-phase-id child-id
        ;; Header (always visible)
        (:div :class "phase-card-header"
              :onclick (format nil "togglePhase('~A')" safe-id)
          (:div :class (format nil "phase-status ~A" css-status)
            (cl-who:str icon))
          (:div :class "phase-info"
            (:div :class "phase-name"
              (cl-who:str (or display (humanize-task-name child-id))))
            (when description
              (cl-who:htm
                (:div :class "phase-desc-preview"
                  (cl-who:str (cl-who:escape-string description))))))
          (:div :class "phase-indicators"
            (when (> obs-count 0)
              (cl-who:htm
                (:span :class "phase-indicator"
                  (cl-who:fmt "~D obs" obs-count))))
            (when (> artifact-count 0)
              (cl-who:htm
                (:span :class "phase-indicator"
                  (cl-who:fmt "~D art" artifact-count))))
            (when (> session-count 0)
              (cl-who:htm
                (:span :class "phase-indicator"
                  (cl-who:fmt "~D sess" session-count))))
            (:span :class "phase-expand-icon" "&#x25B8;")))

        ;; Body (hidden by default unless initially-open)
        (:div :class "phase-card-body"
              :id (format nil "phase-body-~A" safe-id)
              :style (if initially-open "" "display:none")
          ;; Description
          (when description
            (cl-who:htm
              (:div :class "phase-section"
                (:div :class "phase-section-label" "Description")
                (:div :class "phase-description"
                  (cl-who:str (cl-who:escape-string description))))))

          ;; Dependencies
          (:div :class "phase-section"
            (:div :class "phase-section-label" "Dependencies")
            (if deps
                (cl-who:htm
                  (:div :class "phase-dep-list"
                    (dolist (dep deps)
                      (cl-who:htm
                        (:a :href (format nil "/task?id=~A" dep)
                            :class "phase-dep-tag"
                          (cl-who:str (humanize-task-name dep)))))))
                (cl-who:htm
                  (:div :class "phase-dep-empty" "No dependencies"))))

          ;; Non-dep edges
          (when edges
            (cl-who:htm
              (:div :class "phase-section"
                (:div :class "phase-section-label" "Edges")
                (:div :class "phase-edge-list"
                  (dolist (edge edges)
                    (cl-who:htm
                      (:span :class "phase-edge-tag"
                        (:span :class "edge-type"
                          (cl-who:str (getf edge :type)))
                        " "
                        (cl-who:str (humanize-task-name
                                      (getf edge :target))))))))))

          ;; Actions
          (:div :class "phase-actions"
            (:a :href (format nil "/task?id=~A" child-id)
                :class "phase-action-btn"
              "View Detail")
            (if (string-equal status-str "completed")
                (cl-who:htm
                  (:button :class "phase-action-btn"
                    :onclick (format nil "updatePhaseStatus('~A','active')" child-id)
                    "Reopen"))
                (cl-who:htm
                  (:button :class "phase-action-btn phase-action-complete"
                    :onclick (format nil "updatePhaseStatus('~A','completed')" child-id)
                    "Mark Complete")))))))))

;;; ============================================================
;;; PLAN DETAIL PAGE
;;; ============================================================

(defun render-plan-page (task-id)
  "Render plan page with DAG visualization, frontier-first layout."
  (when (or (null task-id) (string= task-id ""))
    (return-from render-plan-page (render-plan-index)))

  (let* ((data (plan-detail-data task-id))
         (display-name (or (getf data :display-name)
                           (humanize-task-name task-id))))
    (if (not (getf data :children))
        ;; No children = no plan structure
        (htm-str
          (:div :class "page-wrapper"
            (:div :class "plan-header"
              (:h1 :class "plan-title" (cl-who:str display-name)))
            (:div :class "empty-state"
              "This task has no plan phases (child tasks)."
              (:div :style "margin-top: 12px"
                (:a :href (format nil "/task?id=~A" task-id)
                    :class "plan-meta a"
                  "View task detail &rarr;")))))
        ;; Has children — render plan with sections
        (let* ((sorted (getf data :sorted))
               (total (or (getf data :total) 0))
               (done (or (getf data :done) 0))
               (ready-count (or (getf data :ready-count) 0))
               ;; Split into frontier / completed / pending
               (frontier nil)
               (completed nil)
               (pending nil))
          ;; Partition phases
          (dolist (phase sorted)
            (let ((status (getf phase :status))
                  (is-ready (getf phase :ready)))
              (cond ((string-equal status "completed")
                     (push phase completed))
                    (is-ready
                     (push phase frontier))
                    (t
                     (push phase pending)))))
          (setf frontier (nreverse frontier)
                completed (nreverse completed)
                pending (nreverse pending))

          (htm-str
            ;; D3 CDN
            (:script :src "https://d3js.org/d3.v7.min.js")
            (:script (cl-who:str (plan-js)))
            (:div :class "page-wrapper"
              ;; Header
              (:div :class "plan-header"
                (:h1 :class "plan-title"
                  (cl-who:fmt "Plan: ~A" display-name))
                (:div :class "plan-meta"
                  (:span (cl-who:fmt "~D phase~:P" total))
                  (:span (cl-who:fmt "~D completed" done))
                  (when (> ready-count 0)
                    (cl-who:htm
                      (:span :style "color: #58a6ff"
                        (cl-who:fmt "~D ready" ready-count))))
                  (:a :href (format nil "/task?id=~A" task-id)
                    "View task detail &rarr;")))

              ;; Progress bar
              (:div :class "plan-progress"
                (:div :class "plan-progress-stat"
                  (:div :class "count" :style "color: var(--color-success)"
                    (cl-who:fmt "~D" done))
                  (:div :class "label" "Done"))
                (:div :class "plan-progress-bar"
                  (:div :class "plan-progress-fill"
                    :style (format nil "width: ~D%"
                             (if (> total 0)
                                 (round (* 100 (/ done total)))
                                 0))))
                (:div :class "plan-progress-stat"
                  (:div :class "count" (cl-who:fmt "~D" total))
                  (:div :class "label" "Total"))
                (when (> ready-count 0)
                  (cl-who:htm
                    (:div :class "plan-progress-stat"
                      (:div :class "count" :style "color: #58a6ff"
                        (cl-who:fmt "~D" ready-count))
                      (:div :class "label" "Ready")))))

              ;; D3 DAG (collapsible, hidden by default)
              (:div :class "plan-graph-section"
                (:div :class "plan-graph-header"
                      :onclick "togglePlanGraph()"
                      :style "cursor:pointer;display:flex;align-items:center;gap:8px;padding:8px 12px;border:1px solid var(--color-border);border-radius:2px;background:var(--color-surface);margin-bottom:1.5rem"
                  (:span :id "graph-toggle-icon"
                    :style "color:var(--color-muted);font-size:10px" "&#x25B8;")
                  (:span :style "font-family:var(--font-mono);font-size:0.7rem;text-transform:uppercase;letter-spacing:0.1em;color:var(--color-muted)"
                    "Dependency Graph")
                  (:span :class "plan-graph-hint"
                    :style "font-family:var(--font-mono);font-size:0.6875rem;color:var(--color-muted)"
                    "(click to expand)"))
                (:div :id "plan-graph-container"
                      :style "display:none;border:1px solid var(--color-border);border-radius:2px;background:var(--color-surface);overflow:hidden"
                  (:svg :id "plan-svg" :width "100%" :height "500")))
              ;; Inject plan data as JSON for D3
              (:script :type "text/javascript"
                (cl-who:fmt "var planData = ~A;"
                  (or (plan-rich-json task-id) "{}")))

              ;; === FRONTIER: Ready Phases ===
              (when frontier
                (cl-who:htm
                  (:div :class "plan-phases"
                    (:div :class "plan-phases-header"
                      (:h2 (cl-who:fmt "Frontier (~D)" (length frontier)))
                      (:span :style "font-family:var(--font-mono);font-size:0.6875rem;color:#58a6ff"
                        "ready to work"))
                    (dolist (phase frontier)
                      (cl-who:str (render-phase-card phase :initially-open t))))))

              ;; === PENDING: Blocked Phases ===
              (when pending
                (cl-who:htm
                  (:div :class "plan-phases"
                    (:div :class "plan-phases-header"
                      (:h2 (cl-who:fmt "Pending (~D)" (length pending))))
                    (dolist (phase pending)
                      (cl-who:str (render-phase-card phase))))))

              ;; === COMPLETED: Collapsed ===
              (when completed
                (cl-who:htm
                  (:div :class "plan-phases"
                    (:div :class "plan-phases-header"
                      :style "cursor:pointer"
                      :onclick "this.parentElement.classList.toggle('collapsed');this.querySelector('.toggle-icon').textContent=this.parentElement.classList.contains('collapsed')?'\\u25B8':'\\u25BE'"
                      (:h2 (cl-who:fmt "Completed (~D)" (length completed))
                        (:span :class "toggle-icon"
                          :style "margin-left:8px;font-size:10px;color:var(--color-muted)" "&#x25BE;"))
                      (:span :style "font-family:var(--font-mono);font-size:0.6875rem;color:var(--color-success)"
                        (cl-who:fmt "~D% done"
                          (if (> total 0) (round (* 100 (/ done total))) 100))))
                    (dolist (phase completed)
                      (cl-who:str (render-phase-card phase))))))))))))

;;; ============================================================
;;; CLUSTERS PAGE
;;; ============================================================

(defun render-density-bar (density)
  "Render a small inline density indicator bar (0.0-1.0 scale)."
  (let* ((pct (min 100 (round (* density 100))))
         (color (cond ((> density 0.4) "var(--color-success)")
                      ((> density 0.15) "var(--color-accent)")
                      (t "var(--color-muted)"))))
    (htm-str
      (:div :class "density-bar"
        (:div :class "density-fill"
          :style (format nil "width:~D%;background:~A" pct color))))))

(defun cluster-status-class (status)
  "Map status keyword to CSS class."
  (cond ((eq status :completed) "completed")
        ((eq status :event-sourced) "active")
        (t "dormant")))

(defun render-cluster-group (group)
  "Render a single cluster group with metrics and expandable body."
  (let* ((topic (getf group :topic))
         (tasks (getf group :tasks))
         (task-count (or (getf group :task-count) (length tasks)))
         (density (or (getf group :density) 0.0))
         (completion (or (getf group :completion) 0.0))
         (cross-links (or (getf group :cross-links) 0))
         (completion-pct (round (* completion 100)))
         (total-obs (loop for tk in tasks sum (or (getf tk :obs-count) 0)))
         (total-sessions (loop for tk in tasks sum (or (getf tk :session-count) 0))))
    (htm-str
      (:div :class "cluster-group"
        (:div :class "cluster-header"
              :onclick "this.nextElementSibling.classList.toggle('open');this.querySelector('.cluster-toggle').textContent=this.nextElementSibling.classList.contains('open')?'\\u25BE':'\\u25B8'"
          (:div :class "cluster-color-bar"
            :style (format nil "background:~A" (topic-color topic)))
          (:div :class "cluster-identity"
            (:span :class "cluster-name" (cl-who:str topic))
            (:span :class "cluster-count" (cl-who:fmt "~D" task-count)))
          (:div :class "cluster-metrics"
            (:span :class "cluster-metric"
              :title "Completion"
              (cl-who:fmt "~D%" completion-pct))
            (cl-who:str (render-density-bar density))
            (when (> cross-links 0)
              (cl-who:htm
                (:span :class "cluster-metric cross"
                  (cl-who:fmt "~D &#8596;" cross-links)))))
          (:span :class "cluster-toggle" "&#x25B8;"))
        ;; Expandable body
        (:div :class "cluster-body"
          (:div :class "cluster-summary"
            (:span (cl-who:fmt "~D session~:P" total-sessions))
            (:span :class "sep" "&middot;")
            (:span (cl-who:fmt "~D observation~:P" total-obs))
            (:span :class "sep" "&middot;")
            (:span (cl-who:fmt "density ~,2F" density)))
          (dolist (tk tasks)
            (cl-who:htm
              (:div :class "cluster-task"
                (:div :class (format nil "cluster-task-status ~A"
                               (cluster-status-class (getf tk :status))))
                (:a :href (format nil "/task?id=~A" (getf tk :id))
                    :class "cluster-task-name"
                  (cl-who:str (or (getf tk :display-name) (getf tk :id))))
                (let ((obs (or (getf tk :obs-count) 0)))
                  (when (> obs 0)
                    (cl-who:htm
                      (:span :class "cluster-task-obs"
                        (cl-who:fmt "~D obs" obs)))))
                (:span :class "cluster-task-date"
                  (cl-who:str (or (getf tk :date) "")))))))))))

(defun render-clusters-page (&optional depot)
  "Render the clusters page with topic grouping and edge density."
  (let ((data (cluster-data depot)))
    (if (not data)
        (htm-str
          (:div :class "page-wrapper"
            (:div :class "empty-state" "Failed to load cluster data.")))
        (let ((groups (getf data :groups))
              (singletons (getf data :singletons))
              (total (or (getf data :total) 0))
              (cluster-count (or (getf data :cluster-count) 0))
              (singleton-count (or (getf data :singleton-count) 0)))
          (htm-str
            (:div :class "page-wrapper"
              (cl-who:str (render-depot-tabs depot "/clusters"))
              ;; Summary bar
              (:div :class "cluster-summary-bar"
                (:span (cl-who:fmt "~D tasks in ~D clusters" total cluster-count))
                (:span :class "sep" "&middot;")
                (:span (cl-who:fmt "~D singletons" singleton-count)))
              ;; Cluster groups
              (:section :class "frontier-section"
                (:h2 "Topic Clusters"
                  (:span :class "count"
                    (cl-who:fmt "(~D)" cluster-count)))
                (dolist (group groups)
                  (cl-who:str (render-cluster-group group))))
              ;; Singletons
              (when singletons
                (cl-who:htm
                  (:h3 :class "singletons-header"
                       :onclick "this.nextElementSibling.classList.toggle('open');this.querySelector('.cluster-toggle').textContent=this.nextElementSibling.classList.contains('open')?'\\u25BE':'\\u25B8'"
                    (cl-who:fmt "Singletons (~D) " singleton-count)
                    (:span :class "cluster-toggle" "&#x25B8;"))
                  (:div :class "singleton-grid"
                    (dolist (tk singletons)
                      (cl-who:htm
                        (:a :href (format nil "/task?id=~A" (getf tk :id))
                            :class "singleton-item"
                          (:span :class (format nil "cluster-task-status inline ~A"
                                          (cluster-status-class (getf tk :status))))
                          (cl-who:str (or (getf tk :display-name)
                                          (humanize-task-name (getf tk :id))))))))))))))))

;;; ============================================================
;;; PLAN SCRIPTS (Parenscript)
;;; ============================================================

(defun plan-js ()
  "JS for plan page: phase expand/collapse, status updates, and D3 DAG."
  (ps:ps
    (defun toggle-phase (safe-id)
      "Toggle a single phase card's expanded state."
      (let* ((card (ps:chain document (get-element-by-id (+ "phase-" safe-id))))
             (body (ps:chain document (get-element-by-id (+ "phase-body-" safe-id)))))
        (when (and card body)
          (ps:chain card class-list (toggle "open"))
          (if (= (ps:chain body style display) "none")
              (setf (ps:chain body style display) "")
              (setf (ps:chain body style display) "none")))))

    (defvar *all-expanded* false)

    (defun toggle-all-phases ()
      "Expand or collapse all phase cards."
      (let ((cards (ps:chain document (query-selector-all ".phase-card")))
            (btn (ps:chain document (get-element-by-id "toggle-all-btn"))))
        (setf *all-expanded* (not *all-expanded*))
        (ps:chain cards (for-each
          (lambda (card)
            (let ((body (ps:chain card (query-selector ".phase-card-body"))))
              (when body
                (if *all-expanded*
                    (progn
                      (ps:chain card class-list (add "open"))
                      (setf (ps:chain body style display) ""))
                    (progn
                      (ps:chain card class-list (remove "open"))
                      (setf (ps:chain body style display) "none"))))))))
        (when btn
          (setf (ps:chain btn text-content)
            (if *all-expanded* "Collapse All" "Expand All")))))

    (defun update-phase-status (task-id new-status)
      "Complete or reopen a phase via API."
      (let* ((url (if (= new-status "completed")
                      "/api/task/complete"
                      "/api/task/reopen"))
             (xhr (ps:new (-x-m-l-http-request))))
        (ps:chain xhr (open "POST" url true))
        (ps:chain xhr (set-request-header "Content-Type" "application/json"))
        (setf (ps:chain xhr onload)
          (lambda ()
            (let ((data (ps:chain -j-s-o-n (parse (ps:chain xhr response-text)))))
              (if (ps:@ data :ok)
                  (ps:chain window location (reload))
                  (alert (+ "Error: " (or (ps:@ data :error) "Unknown")))))))
        (setf (ps:chain xhr onerror)
          (lambda () (alert "Request failed")))
        (ps:chain xhr (send (ps:chain -j-s-o-n
                              (stringify (ps:create :task_id task-id)))))))

    ;; === D3 DAG Visualization ===
    (defvar *status-colors*
      (ps:create
        "completed" (ps:create "fill" "#1a2e1a" "stroke" "#3fb950" "text" "#3fb950"
                               "icon" (ps:lisp (string (code-char #x2713))))
        "active"    (ps:create "fill" "#1f1633" "stroke" "#a371f7" "text" "#a371f7"
                               "icon" (ps:lisp (string (code-char #x25B6))))
        "ready"     (ps:create "fill" "#0d2240" "stroke" "#58a6ff" "text" "#58a6ff"
                               "icon" (ps:lisp (string (code-char #x25CB))))
        "pending"   (ps:create "fill" "#161b22" "stroke" "#30363d" "text" "#8b949e"
                               "icon" (ps:lisp (string (code-char #x25CB))))))

    (defun status-prop (status prop)
      (let ((s (aref *status-colors* status)))
        (if s (aref s prop) "")))

    (defvar *graph-initialized* false)

    (defun toggle-plan-graph ()
      (let* ((container (ps:chain document (get-element-by-id "plan-graph-container")))
             (icon (ps:chain document (get-element-by-id "graph-toggle-icon")))
             (hint (ps:chain document (query-selector ".plan-graph-hint")))
             (is-hidden (= (ps:chain container style display) "none")))
        (if is-hidden
            (progn
              (setf (ps:chain container style display) "block")
              (when icon (setf (ps:chain icon inner-h-t-m-l) "&#x25BE;"))
              (when hint (setf (ps:chain hint text-content) "(click to collapse)"))
              (when (not *graph-initialized*)
                (init-plan-graph)
                (setf *graph-initialized* true)))
            (progn
              (setf (ps:chain container style display) "none")
              (when icon (setf (ps:chain icon inner-h-t-m-l) "&#x25B8;"))
              (when hint (setf (ps:chain hint text-content) "(click to expand)"))))))

    (defun init-plan-graph ()
      (let* ((data plan-data)
             (phases (ps:@ data phases))
             (container (ps:chain document (get-element-by-id "plan-graph-container"))))
        (when (or (not container) (not phases) (= (ps:@ phases length) 0))
          (return-from init-plan-graph))

        (let* ((width (ps:@ container offset-width))
               (node-w 260) (node-h 44)
               (svg (ps:chain d3 (select "#plan-svg")))
               (defs (ps:chain svg (append "defs")))
               (g (ps:chain svg (append "g"))))

          ;; Arrow marker
          (ps:chain defs (append "marker")
            (attr "id" "arrow") (attr "viewBox" "0 0 10 10")
            (attr "refX" 10) (attr "refY" 5)
            (attr "markerWidth" 6) (attr "markerHeight" 6)
            (attr "orient" "auto-start-reverse")
            (append "path") (attr "d" "M 0 0 L 10 5 L 0 10 z")
            (attr "fill" "#58a6ff"))

          ;; Zoom
          (let ((zoom-beh (ps:chain d3 (zoom) (scale-extent (array 0.3 3))
                            (on "zoom" (lambda (ev)
                              (ps:chain g (attr "transform" (ps:@ ev transform))))))))
            (ps:chain svg (call zoom-beh)))

          ;; Build node index (keyed by both qualified and bare ID for dep lookup)
          (let ((node-map (ps:create)))
            (ps:chain phases (for-each (lambda (p)
              (let ((id (ps:@ p id)))
                (setf (aref node-map id) p)
                ;; Also key by bare ID (strip "depot:" prefix)
                (let ((colon (ps:chain id (index-of ":"))))
                  (when (> colon -1)
                    (setf (aref node-map (ps:chain id (substring (+ colon 1)))) p))))))))

            ;; Topological levels
            (let ((levels (ps:create)))
              (labels ((compute-level (p-id)
                (let ((cached (aref levels p-id)))
                  (if (not (= cached undefined))
                      cached
                      (let* ((p (aref node-map p-id)))
                        (when (not p)
                          (setf (aref levels p-id) 0)
                          (return-from compute-level 0))
                        (let* ((deps (or (ps:@ p deps) (array)))
                               (lvl (if (= (ps:@ deps length) 0)
                                        0
                                        (+ 1 (ps:chain -math max (apply -math
                                          (ps:chain deps (map (lambda (d) (compute-level d))))))))))
                          (setf (aref levels p-id) lvl)
                          lvl))))))
                (ps:chain phases (for-each (lambda (p)
                  (compute-level (ps:@ p id))))))

              ;; Group by level
              (let ((by-level (ps:create)))
                (ps:chain phases (for-each (lambda (p)
                  (let ((lvl (aref levels (ps:@ p id))))
                    (when (= (aref by-level lvl) undefined)
                      (setf (aref by-level lvl) (array)))
                    (ps:chain (aref by-level lvl) (push p))))))

                ;; Layout
                (let ((start-y 30)
                      (row-gap 72)
                      (col-gap 16)
                      (max-level 0))
                  (ps:chain -object (keys by-level) (for-each (lambda (lvl-str)
                    (let* ((lvl (parse-int lvl-str))
                           (row (aref by-level lvl-str))
                           (row-width (+ (* (ps:@ row length) node-w)
                                         (* (- (ps:@ row length) 1) col-gap)))
                           (start-x (/ (- width row-width) 2))
                           (y (+ start-y (* lvl row-gap))))
                      (when (> lvl max-level) (setf max-level lvl))
                      (ps:chain row (for-each (lambda (p i)
                        (let ((x (+ start-x (* i (+ node-w col-gap)))))
                          (setf (ps:@ p x) x (ps:@ p y) y)))))))))

                  ;; Resize SVG height
                  (ps:chain svg (attr "height"
                    (+ start-y (* (+ max-level 1) row-gap) 20)))))

              ;; Draw edges
              (ps:chain phases (for-each (lambda (p)
                (when (and (ps:@ p deps) (> (ps:@ p deps length) 0))
                  (ps:chain (ps:@ p deps) (for-each (lambda (dep-id)
                    (let ((dep (aref node-map dep-id)))
                      (when dep
                        (ps:chain g (append "line")
                          (attr "x1" (+ (ps:@ dep x) (/ node-w 2)))
                          (attr "y1" (+ (ps:@ dep y) node-h))
                          (attr "x2" (+ (ps:@ p x) (/ node-w 2)))
                          (attr "y2" (ps:@ p y))
                          (attr "stroke" "#58a6ff")
                          (attr "stroke-opacity" 0.4)
                          (attr "stroke-width" 1)
                          (attr "marker-end" "url(#arrow)")))))))))))

              ;; Draw nodes
              (let ((nodes (ps:chain g (select-all "g.phase-node")
                             (data phases) (join "g")
                             (attr "class" "phase-node")
                             (attr "transform" (lambda (d)
                               (+ "translate(" (ps:@ d x) "," (ps:@ d y) ")"))))))

                ;; Background rect
                (ps:chain nodes (append "rect")
                  (attr "width" node-w) (attr "height" node-h)
                  (attr "rx" 2) (attr "ry" 2)
                  (attr "fill" (lambda (d) (status-prop (ps:@ d status) "fill")))
                  (attr "stroke" (lambda (d) (status-prop (ps:@ d status) "stroke")))
                  (attr "stroke-width" 1.5))

                ;; Status icon
                (ps:chain nodes (append "text")
                  (attr "x" 12) (attr "y" 27)
                  (attr "text-anchor" "middle")
                  (attr "fill" (lambda (d) (status-prop (ps:@ d status) "text")))
                  (attr "font-size" "11px")
                  (text (lambda (d) (status-prop (ps:@ d status) "icon"))))

                ;; Phase name
                (ps:chain nodes (append "text")
                  (attr "x" 24) (attr "y" 22)
                  (attr "fill" (lambda (d) (status-prop (ps:@ d status) "text")))
                  (attr "font-size" "11px") (attr "font-weight" "500")
                  (attr "font-family" "'Space Mono', monospace")
                  (text (lambda (d)
                    (let ((name (ps:@ d display)))
                      (if (> (ps:@ name length) 30)
                          (+ (ps:chain name (substring 0 28))
                             (ps:lisp (string (code-char #x2026))))
                          name)))))

                ;; Obs count
                (ps:chain nodes (append "text")
                  (attr "x" (- node-w 8)) (attr "y" 36)
                  (attr "text-anchor" "end")
                  (attr "fill" "#5a5a6e") (attr "font-size" "9px")
                  (attr "font-family" "'Space Mono', monospace")
                  (text (lambda (d)
                    (let ((obs-len (ps:@ d observations length)))
                      (if (> obs-len 0)
                          (+ obs-len " obs")
                          "")))))

                ;; Click to scroll to card
                (ps:chain nodes (style "cursor" "pointer")
                  (on "click" (lambda (ev d)
                    (let ((card (ps:chain document
                                  (query-selector (+ "[data-phase-id='" (ps:@ d id) "']")))))
                      (when card
                        ;; Expand if collapsed
                        (let ((body (ps:chain card (query-selector ".phase-card-body"))))
                          (when (and body (= (ps:chain body style display) "none"))
                            (setf (ps:chain body style display) "block")
                            (ps:chain card class-list (add "open"))))
                        (ps:chain card (scroll-into-view
                                         (ps:create "behavior" "smooth" "block" "center")))))))))))))))
