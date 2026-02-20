;;;; KLI Dashboard — Task Detail Page
;;;; Enriched view: header, description, metadata, fiber-grouped connections,
;;;; observations, handoffs, sessions. Includes scratchpad sidebar.

(in-package :kli-dashboard)

;;; ============================================================
;;; FIBER GROUPING
;;; ============================================================

(defparameter *fiber-order* '(:structural :causal :declared :inferred))

(defparameter *fiber-labels*
  '((:structural . "Structural")
    (:causal . "Causal")
    (:declared . "Declared")
    (:inferred . "Inferred")))

(defparameter *edge-to-fiber*
  '((:phase-of . :structural) (:spawned . :structural)
    (:depends-on . :causal) (:enables . :causal) (:blocks . :causal)
    (:related-to . :declared)
    (:topic . :inferred) (:reference . :inferred) (:references . :inferred)
    (:same-day . :inferred)))

(defun group-by-fiber (neighbors)
  "Group neighbors plist into fiber buckets. Returns hash-table fiber->list."
  (let ((grouped (make-hash-table :test 'eq)))
    (dolist (n neighbors)
      (let* ((edge-kw (getf n :edge))
             (fiber (or (cdr (assoc edge-kw *edge-to-fiber*)) :inferred)))
        (push n (gethash fiber grouped))))
    grouped))

;;; ============================================================
;;; SESSION COLOR (consistent hue from ID hash)
;;; ============================================================

(defun session-badge-color (session-id)
  "Generate consistent HSL color for a session ID."
  (if (and session-id (stringp session-id) (> (length session-id) 0))
      (format nil "hsl(~D, 55%, 60%)" (mod (sxhash session-id) 360))
      "#5a5a6e"))

;;; ============================================================
;;; TASK DETAIL RENDERER
;;; ============================================================

(defun render-task-detail (task-id)
  "Render the full task detail page body for TASK-ID."
  (when (or (null task-id) (string= task-id ""))
    (return-from render-task-detail
      (htm-str
        (:div :class "page-wrapper"
          (:div :class "empty-state" "No task specified.")))))
  (let* ((data (task-detail-data task-id))
         (desc (getf data :description))
         (goals (getf data :goals))
         (phase (getf data :phase))
         (tags (getf data :tags))
         (status (or (getf data :status) "active"))
         (is-completed (string-equal status "completed"))
         (children (getf data :children))
         (neighbors (getf data :neighbors))
         (sessions (getf data :sessions))
         (recent-obs (getf data :recent-obs))
         (obs-count (or (getf data :obs-count) 0))
         (events-count (or (getf data :events-count) 0))
         (handoff-count (or (getf data :handoff-count) 0))
         (handoffs (getf data :handoffs))
         (display-name (humanize-task-name task-id))
         ;; Derive topic color from bare-id
         (bare-id (let ((p (position #\: task-id)))
                    (if p (subseq task-id (1+ p)) task-id)))
         (topic (extract-topic bare-id))
         (topic-color (topic-color topic)))
    (htm-str
      ;; Scripts (expand toggle + scratchpad + detail actions)
      (:script (cl-who:str (expand-toggle-js)))
      (:script (cl-who:str (scratchpad-js)))
      (cl-who:str (render-scratchpad-sidebar))

      (:div :class "page-wrapper"
        ;; === HEADER ===
        (:div :class "detail-header"
          :style (format nil "border-left-color: ~A" topic-color)
          (:h1 :class "detail-title"
            :style (format nil "color: ~A" topic-color)
            (cl-who:str display-name))
          (:div :class "detail-id" (cl-who:str task-id))
          (:div :class "detail-meta"
            (:span :class "detail-meta-accent" (cl-who:str topic))
            (cl-who:htm
              (:span (cl-who:fmt "~D events" events-count))
              (:span (cl-who:fmt "~D observations" obs-count))
              (:span (cl-who:fmt "~D session~:P" (length sessions))))
            (when (and children (> (length children) 0))
              (cl-who:htm
                (:a :href (format nil "/plan?id=~A" task-id)
                    :class "detail-meta-link"
                  (cl-who:fmt "View Plan (~D phases) &rarr;" (length children)))))))

        ;; === ACTIONS ===
        (:div :class "detail-actions"
          (if is-completed
              (cl-who:htm
                (:span :class "detail-status-badge completed" "completed")
                (:button :class "detail-action-btn"
                  :onclick (format nil "reopenTask('~A')" task-id)
                  "Reopen"))
              (cl-who:htm
                (:button :class "detail-action-btn detail-complete-btn"
                  :onclick (format nil "completeTask('~A')" task-id)
                  "Complete")))
          (:button :class "scratchpad-open-btn detail-action-btn"
            :onclick (format nil "openScratchpad('~A','~A')"
                             task-id (cl-who:escape-string display-name))
            (cl-who:str "&#9998; Scratchpad")))

        ;; === DESCRIPTION & METADATA ===
        (when (or desc goals phase tags)
          (cl-who:htm
            (:div :class "detail-section"
              (when (and desc (> (length desc) 0))
                (cl-who:htm
                  (:div :class "detail-section-label" "Description")
                  (:div :class "detail-desc"
                    (cl-who:str (cl-who:escape-string desc)))))
              (when goals
                (cl-who:htm
                  (:div :class "detail-section-label"
                    :style "margin-top: 12px"
                    "Goals")
                  (:ul :class "detail-goals"
                    (dolist (g goals)
                      (cl-who:htm
                        (:li (cl-who:str (cl-who:escape-string g))))))))
              (when (or phase tags)
                (cl-who:htm
                  (:div :class "detail-metadata-row"
                    :style "margin-top: 12px"
                    (when phase
                      (cl-who:htm
                        (:div
                          (:span :class "meta-key" "Phase: ")
                          (:span :class "meta-val" (cl-who:str phase)))))
                    (when (and tags (> (length tags) 0))
                      (cl-who:htm
                        (:div
                          (:span :class "meta-key" "Tags: ")
                          (dolist (tag (uiop:split-string tags :separator ","))
                            (cl-who:htm
                              (:span :class "detail-tag"
                                (cl-who:str (string-trim " " tag))))))))))))))

        ;; === CONNECTIONS (fiber-grouped) ===
        (when neighbors
          (let ((grouped (group-by-fiber neighbors)))
            (cl-who:htm
              (:div :class "detail-section"
                (:div :class "detail-section-label"
                  "Connections"
                  (:span :class "detail-section-count"
                    (cl-who:fmt "(~D)" (length neighbors))))
                (dolist (fiber *fiber-order*)
                  (let ((items (nreverse (gethash fiber grouped))))
                    (when items
                      (cl-who:htm
                        (:div :class "connection-fiber"
                          (:div :class (format nil "fiber-label fiber-~(~A~)" fiber)
                            (cl-who:fmt "~A (~D)"
                              (cdr (assoc fiber *fiber-labels*))
                              (length items)))
                          (:ul :class "connections-list"
                            (dolist (n (subseq items 0 (min 20 (length items))))
                              (let ((edge-name (string-downcase
                                                 (symbol-name (getf n :edge)))))
                                (cl-who:htm
                                  (:li :class "connection-item"
                                    (:span :class (format nil "connection-layer ~A"
                                                          edge-name)
                                      (cl-who:str edge-name))
                                    (:a :class "connection-name"
                                        :href (format nil "/task?id=~A" (getf n :id))
                                      (cl-who:str (humanize-task-name (getf n :id))))
                                    (:span :class "connection-dir"
                                      (cl-who:str (string-downcase
                                                    (symbol-name (getf n :dir)))))))))
                            (when (> (length items) 20)
                              (cl-who:htm
                                (:li :class "connection-item"
                                  :style "color: var(--color-muted); font-size: 0.72rem"
                                  (cl-who:fmt "... and ~D more" (- (length items) 20)))))))))))))))

        ;; === OBSERVATIONS ===
        (when recent-obs
          (cl-who:htm
            (:div :class "detail-section"
              (:div :class "detail-section-label"
                "Recent Observations"
                (:span :class "detail-section-count"
                  (cl-who:fmt "(~D total)" obs-count)))
              (dolist (o recent-obs)
                (let ((text (getf o :text)))
                  (when text
                    (cl-who:htm
                      (:div :class "obs-item"
                        (:span :class "obs-time"
                          (cl-who:str (or (getf o :time) "")))
                        (:div :class "obs-text"
                          (cl-who:str (cl-who:escape-string
                                        (subseq text 0
                                          (min 500 (length text))))))))))))))

        ;; === SESSIONS ===
        (when sessions
          (cl-who:htm
            (:div :class "detail-section"
              (:div :class "detail-section-label"
                "Sessions"
                (:span :class "detail-section-count"
                  (cl-who:fmt "(~D)" (length sessions))))
              (:div :class "session-list"
                (dolist (s sessions)
                  (let ((scolor (session-badge-color s)))
                    (cl-who:htm
                      (:span :class "session-badge"
                        :style (format nil "background: ~A22; color: ~A" scolor scolor)
                        (cl-who:str (subseq s 0 (min 8 (length s))))))))))))

        ;; === HANDOFFS ===
        (when handoffs
          (cl-who:htm
            (:div :class "detail-section"
              (:div :class "detail-section-label"
                "Handoffs"
                (:span :class "detail-section-count"
                  (cl-who:fmt "(~D)" handoff-count)))
              (dolist (h handoffs)
                (let ((hname (getf h :name)))
                  (cl-who:htm
                    (:div :class "handoff-item"
                      (:button :class "expand-btn"
                               :hx-get (format nil "/api/handoff/expand?task_id=~A&name=~A"
                                               task-id hname)
                               :hx-target (format nil "#handoff-~A" (css-safe-id hname))
                               :hx-swap "innerHTML"
                               :hx-trigger "click once"
                               :onclick "toggleExpand(this)"
                               :hx-on-htmx-after-swap "afterExpand(event.detail.elt)"
                        (cl-who:fmt "&#9656; ~A" hname))
                      (:div :id (format nil "handoff-~A" (css-safe-id hname))
                            :class "expand-target"))))))))

        ;; === ACTION SCRIPTS ===
        (:script (cl-who:str (task-detail-js)))))))

;;; ============================================================
;;; HANDOFF EXPAND FRAGMENT
;;; ============================================================

(defun render-handoff-expand-fragment (task-id handoff-name)
  "Render an expandable handoff content panel."
  (let ((content (handoff-content task-id handoff-name)))
    (htm-str
      (:div :class "expand-panel"
        (if (and content (> (length content) 0))
            (cl-who:htm
              (:pre :class "expand-handoff"
                (cl-who:str (cl-who:escape-string content))))
            (cl-who:htm
              (:div :class "expand-desc" "No content available")))))))

;;; ============================================================
;;; TASK DETAIL SCRIPTS (Parenscript)
;;; ============================================================

(defun task-detail-js ()
  "JS for task complete/reopen actions."
  (ps:ps
    (defun task-action (url task-id)
      "POST to URL with task_id, reload on success."
      (let ((xhr (ps:new (-x-m-l-http-request))))
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

    (defun complete-task (task-id)
      (task-action "/api/task/complete" task-id))

    (defun reopen-task (task-id)
      (task-action "/api/task/reopen" task-id))))
