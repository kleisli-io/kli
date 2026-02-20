;;;; KLI Dashboard — Frontier page
;;;; 4-section layout: Pick Up / Pinned / Inbox / Ready to Work
;;;; with scratchpad sidebar, pin toggles, HTMX expand panels.

(in-package :kli-dashboard)

;;; ============================================================
;;; PIN TOGGLE (HTMX swap target)
;;; ============================================================

(defun render-pin-toggle (task-id depot pinned)
  "Render a pin/unpin button for HTMX swap."
  (if pinned
      (htm-str
        (:button :class "pin-btn pinned"
                 :hx-post (format nil "/api/unpin?task_id=~A&depot=~A"
                                  task-id (or depot ""))
                 :hx-swap "outerHTML"
                 :title "Unpin"
          (cl-who:str "&#9733;")))
      (htm-str
        (:button :class "pin-btn"
                 :hx-post (format nil "/api/pin?task_id=~A&depot=~A"
                                  task-id (or depot ""))
                 :hx-swap "outerHTML"
                 :title "Pin"
          (cl-who:str "&#9734;")))))

;;; ============================================================
;;; EXPAND PANEL RENDERING (HTMX fragment)
;;; ============================================================

(defun render-task-expand-fragment (task-id)
  "Render an expandable detail panel for a frontier card."
  (let* ((data (task-expand-data task-id))
         (desc (getf data :description))
         (goals (getf data :goals))
         (phase (getf data :phase))
         (children (getf data :children))
         (handoff-content (getf data :handoff-content))
         (recent-obs (getf data :recent-obs)))
    (htm-str
      (:div :class "expand-panel"
        (when (and desc (> (length desc) 0))
          (cl-who:htm
            (:div :class "expand-section"
              (:div :class "expand-label" "Description")
              (:div :class "expand-desc"
                (cl-who:str (cl-who:escape-string desc))))))
        (when goals
          (cl-who:htm
            (:div :class "expand-section"
              (:div :class "expand-label" "Goals")
              (:ul :class "expand-goals"
                (dolist (g goals)
                  (cl-who:htm
                    (:li (cl-who:str (cl-who:escape-string g)))))))))
        (when phase
          (cl-who:htm
            (:div :class "expand-section expand-inline"
              (:span :class "expand-label" "Phase: ")
              (:span :class "expand-phase" (cl-who:str phase)))))
        (when children
          (cl-who:htm
            (:div :class "expand-section"
              (:div :class "expand-label"
                (cl-who:fmt "Phases (~D)" (length children)))
              (:ul :class "expand-children"
                (dolist (c children)
                  (cl-who:htm
                    (:li (:a :href (format nil "/task?id=~A" c)
                           (cl-who:str (humanize-task-name c))))))))))
        (when handoff-content
          (cl-who:htm
            (:div :class "expand-section"
              (:div :class "expand-label" "Latest Handoff")
              (:pre :class "expand-handoff"
                (cl-who:str (cl-who:escape-string handoff-content))))))
        (when recent-obs
          (cl-who:htm
            (:div :class "expand-section"
              (:div :class "expand-label"
                (cl-who:fmt "Recent Observations (~D)" (length recent-obs)))
              (:div :class "expand-observations"
                (dolist (obs (subseq recent-obs 0
                                     (min 3 (length recent-obs))))
                  (cl-who:htm
                    (:div :class "expand-obs"
                      (:span :class "expand-obs-time"
                        (cl-who:str (or (getf obs :time) "")))
                      (:div :class "expand-obs-text"
                        (cl-who:str (cl-who:escape-string
                                      (let ((text (or (getf obs :text) "")))
                                        (if (> (length text) 200)
                                            (concatenate 'string
                                              (subseq text 0 200) "...")
                                            text))))))))))))
        (:div :class "expand-footer"
          (:a :href (format nil "/task?id=~A" task-id)
              :class "expand-detail-link"
            (cl-who:str "View full detail &#8594;")))))))

;;; ============================================================
;;; CARD RENDERERS
;;; ============================================================

(defun render-scratchpad-btn (task-id task-name)
  "Render a scratchpad open button for a task card."
  (htm-str
    (:button :class "scratchpad-open-btn"
             :title "Scratchpad"
             :onclick (format nil "openScratchpad('~A','~A')"
                              task-id (cl-who:escape-string task-name))
      (cl-who:str "&#9998;"))))

(defun render-pickup-card (tk active-depot)
  "Render a card for the 'Pick Up Where You Left Off' section."
  (let ((id (getf tk :id))
        (name (or (getf tk :display-name) (getf tk :bare-id) (getf tk :id)))
        (depot (getf tk :depot))
        (topic (getf tk :topic)))
    (htm-str
      (:div :class "pickup-card"
        (:div :class "pickup-header"
          (:a :href (format nil "/task?id=~A" id)
              :class "pickup-title"
            (cl-who:str name))
          (:span :class "pickup-actions"
            (when depot
              (cl-who:htm
                (:span :class "depot-badge" (cl-who:str depot))))
            (cl-who:str (render-scratchpad-btn id name))
            (cl-who:str (render-pin-toggle id (or active-depot depot)
                                           (pinned-p id (or active-depot depot))))))
        (:div :class "pickup-meta"
          (when topic
            (cl-who:htm
              (:span (cl-who:str topic))))
          (cl-who:htm
            (:span (cl-who:fmt "~D sessions" (or (getf tk :session-count) 0)))
            (:span (cl-who:fmt "~D obs" (or (getf tk :obs-count) 0)))))
        ;; Expand toggle
        (:button :class "expand-btn"
                 :hx-get (format nil "/api/task/expand?id=~A" id)
                 :hx-target (format nil "#expand-pickup-~A" (css-safe-id id))
                 :hx-swap "innerHTML"
                 :hx-trigger "click once"
                 :onclick "toggleExpand(this)"
                 :hx-on-htmx-after-swap "afterExpand(event.detail.elt)"
          (cl-who:str "&#9656; Details"))
        (:div :id (format nil "expand-pickup-~A" (css-safe-id id))
              :class "expand-target")))))

(defun render-pin-card (tk active-depot)
  "Render a card for the 'Pinned' section."
  (let ((id (getf tk :id))
        (name (or (getf tk :display-name) (getf tk :bare-id) (getf tk :id))))
    (htm-str
      (:div :class "pin-card"
        (:a :href (format nil "/task?id=~A" id)
            :class "pin-title"
          (cl-who:str name))
        (cl-who:str (render-pin-toggle id active-depot t))))))

(defun render-inbox-card (tk active-depot)
  "Render a card for the 'Inbox' section."
  (let ((id (getf tk :id))
        (depot (getf tk :depot))
        (name (or (getf tk :display-name) (getf tk :bare-id) (getf tk :id))))
    (htm-str
      (:div :class "inbox-card"
        (:div :class "inbox-header"
          (:a :href (format nil "/task?id=~A" id)
              :class "inbox-title"
            (cl-who:str name))
          (:span :class "inbox-actions"
            (cl-who:str (render-scratchpad-btn id name))
            (cl-who:str (render-pin-toggle id (or active-depot depot)
                                           (pinned-p id (or active-depot depot))))))
        (:div :class "inbox-meta"
          (cl-who:fmt "~D obs · ~D session~:P"
                      (or (getf tk :obs-count) 0)
                      (or (getf tk :session-count) 0)))
        (:button :class "expand-btn"
                 :hx-get (format nil "/api/task/expand?id=~A" id)
                 :hx-target (format nil "#expand-inbox-~A" (css-safe-id id))
                 :hx-swap "innerHTML"
                 :hx-trigger "click once"
                 :onclick "toggleExpand(this)"
                 :hx-on-htmx-after-swap "afterExpand(event.detail.elt)"
          (cl-who:str "&#9656; Details"))
        (:div :id (format nil "expand-inbox-~A" (css-safe-id id))
              :class "expand-target")))))

;;; ============================================================
;;; SCRATCHPAD SIDEBAR (per-task)
;;; ============================================================

(defun render-scratchpad-sidebar ()
  "Render the per-task scratchpad sidebar. Content loaded dynamically via HTMX."
  (htm-str
    ;; Sidebar panel (content loaded on open via JS)
    (:div :id "scratchpad" :class "scratchpad-sidebar"
      (:div :class "scratchpad-header"
        (:span :id "scratchpad-title" "Scratchpad")
        (:button :class "scratchpad-close"
                 :onclick "closeScratchpad()"
          (cl-who:str "&#10005;")))
      (:form :id "scratchpad-form"
        (:textarea :name "content"
                   :id "scratchpad-textarea"
                   :class "scratchpad-textarea"
                   :placeholder "Notes, todos, quick thoughts..."
                   :spellcheck "false"))
      (:div :id "scratchpad-status" :class "scratchpad-status"))))

;;; ============================================================
;;; FRONTIER SCRIPTS (Parenscript)
;;; ============================================================

(defun expand-toggle-js ()
  "JS for expand/collapse toggle on frontier cards."
  (ps:ps
    (defun toggle-expand (btn)
      (if (ps:chain btn class-list (contains "loaded"))
          ;; Already loaded — toggle visibility
          (progn
            (ps:chain btn class-list (toggle "open"))
            (let ((target (ps:chain btn next-element-sibling)))
              (when target
                (if (= (ps:chain target style display) "none")
                    (setf (ps:chain target style display) "")
                    (setf (ps:chain target style display) "none")))))
          ;; First click — show loading state
          (ps:chain btn class-list (add "loading"))))

    (defun after-expand (elt)
      ;; elt is the triggering button (event.detail.elt)
      (ps:chain elt class-list (remove "loading"))
      (ps:chain elt class-list (add "loaded"))
      (ps:chain elt class-list (add "open")))))

(defun scratchpad-js ()
  "JS for per-task scratchpad sidebar with click-outside-to-close and autosave."
  (ps:ps
    (defvar *scratchpad-task-id* nil)
    (defvar *scratchpad-dirty* false)

    (defun save-scratchpad ()
      "Save current scratchpad content if dirty."
      (when (and *scratchpad-task-id* *scratchpad-dirty*)
        (let ((ta (ps:chain document (get-element-by-id "scratchpad-textarea"))))
          (when ta
            (let ((xhr (ps:new (-x-m-l-http-request))))
              (ps:chain xhr (open "POST"
                (+ "/api/scratchpad?task_id=" (encode-u-r-i-component *scratchpad-task-id*))
                true))
              (ps:chain xhr (set-request-header "Content-Type" "text/plain"))
              (setf (ps:chain xhr onload)
                (lambda ()
                  (let ((status-el (ps:chain document (get-element-by-id "scratchpad-status"))))
                    (when status-el
                      (setf (ps:chain status-el inner-h-t-m-l)
                        (if (= (ps:chain xhr status) 200)
                            (ps:chain xhr response-text)
                            "<span class=\"scratchpad-error\">Save failed</span>"))))))
              (ps:chain xhr (send (ps:chain ta value))))
            (setf *scratchpad-dirty* false)))))

    (defun open-scratchpad (task-id task-name)
      "Open scratchpad for a specific task."
      (let ((panel (ps:chain document (get-element-by-id "scratchpad"))))
        (when panel
          ;; Save previous task's content if dirty
          (save-scratchpad)
          ;; Set new task context
          (setf *scratchpad-task-id* task-id)
          (setf *scratchpad-dirty* false)
          ;; Update header
          (let ((title-el (ps:chain document (get-element-by-id "scratchpad-title"))))
            (when title-el
              (setf (ps:chain title-el text-content) task-name)))
          ;; Load content
          (let ((ta (ps:chain document (get-element-by-id "scratchpad-textarea")))
                (xhr (ps:new (-x-m-l-http-request))))
            ;; Ensure input listener is bound (idempotent via flag)
            (when (and ta (not (ps:@ ta :_scratchpad-bound)))
              (ps:chain ta (add-event-listener "input"
                (lambda () (setf *scratchpad-dirty* true))))
              (setf (ps:@ ta :_scratchpad-bound) true))
            (ps:chain xhr (open "GET"
              (+ "/api/scratchpad?task_id=" (encode-u-r-i-component task-id))
              true))
            (setf (ps:chain xhr onload)
              (lambda ()
                (when ta
                  (setf (ps:chain ta value)
                    (if (= (ps:chain xhr status) 200)
                        (ps:chain xhr response-text)
                        ""))
                  (ps:chain ta (focus)))))
            (ps:chain xhr (send)))
          ;; Clear status
          (let ((status-el (ps:chain document (get-element-by-id "scratchpad-status"))))
            (when status-el (setf (ps:chain status-el inner-h-t-m-l) "")))
          ;; Open panel
          (ps:chain panel class-list (add "open")))))

    (defun close-scratchpad ()
      "Close scratchpad with autosave."
      (let ((panel (ps:chain document (get-element-by-id "scratchpad"))))
        (when panel
          (save-scratchpad)
          (ps:chain panel class-list (remove "open"))
          (setf *scratchpad-task-id* nil))))

    ;; Click-outside-to-close
    (ps:chain document (add-event-listener "click"
      (lambda (e)
        (let ((panel (ps:chain document (get-element-by-id "scratchpad"))))
          (when (and panel (ps:chain panel class-list (contains "open")))
            ;; Close if click is outside the scratchpad panel and not on a scratchpad-open button
            (unless (or (ps:chain panel (contains (ps:chain e target)))
                        (ps:chain (ps:chain e target) (closest ".scratchpad-open-btn")))
              (close-scratchpad)))))))))

;;; ============================================================
;;; DEPOT TABS
;;; ============================================================

(defun render-depot-tabs (&optional active-depot (base-url "/"))
  "Render depot selector tabs."
  (let ((depots (get-available-depots)))
    (when (> (length depots) 1)
      (htm-str
        (:div :class "depot-tabs"
          (:a :href base-url
              :class (if (null active-depot) "depot-tab active" "depot-tab")
              "All depots")
          (dolist (d depots)
            (cl-who:htm
              (:a :href (format nil "~A?depot=~A" base-url d)
                  :class (if (and active-depot (string= active-depot d))
                             "depot-tab active"
                             "depot-tab")
                  (cl-who:str d)))))))))

;;; ============================================================
;;; FRONTIER PAGE RENDERER
;;; ============================================================

(defun render-frontier-page (&optional depot)
  "Render the 4-section frontier with depot tabs, scratchpad, and HTMX."
  (let ((data (frontier-data :depot depot)))
    (htm-str
      ;; Inline scripts for expand toggle
      (:script (cl-who:str (expand-toggle-js)))
      ;; Page wrapper
      (:div :class "page-wrapper"
        ;; Depot tabs
        (cl-who:str (render-depot-tabs depot))
        ;; Metrics bar
        (cl-who:str (render-metrics
                      (getf data :event-sourced)
                      (getf data :completed)
                      (getf data :dormant)
                      (getf data :total)))
        ;; Sections
        (:div :class "frontier-sections"
          ;; Section 1: Pick Up
          (:section :class "frontier-section"
            (:h2 "Pick Up Where You Left Off"
              (:span :class "count"
                (cl-who:fmt "(~D)" (length (getf data :pick-up)))))
            (:div :class "section-cards"
              (let ((picks (getf data :pick-up)))
                (if picks
                    (dolist (tk picks)
                      (cl-who:str (render-pickup-card tk depot)))
                    (cl-who:htm
                      (:div :class "empty-state" "No recent work to resume"))))))

          ;; Section 2: Pinned
          (let ((pins (getf data :pinned)))
            (when pins
              (cl-who:htm
                (:section :class "frontier-section"
                  (:h2 "Pinned"
                    (:span :class "count"
                      (cl-who:fmt "(~D)" (length pins))))
                  (:div :class "section-cards"
                    (dolist (tk pins)
                      (cl-who:str (render-pin-card tk depot))))))))

          ;; Section 3: Inbox
          (:section :class "frontier-section"
            (:h2 "Needs Attention"
              (:span :class "count"
                (cl-who:fmt "(~D)" (length (getf data :inbox)))))
            (:div :class "section-cards"
              (let ((items (getf data :inbox)))
                (if items
                    (dolist (tk items)
                      (cl-who:str (render-inbox-card tk depot)))
                    (cl-who:htm
                      (:div :class "empty-state" "Inbox empty"))))))

          ;; Section 4: Ready to Work
          (:section :class "frontier-section"
            (:h2 "Ready to Work"
              (:span :class "count"
                (cl-who:fmt "(~D)" (length (getf data :ready)))))
            (let ((groups (getf data :ready)))
              (if groups
                  (dolist (group groups)
                    (let ((topic (car group))
                          (tasks (cdr group)))
                      (cl-who:htm
                        (:div :class "topic-group"
                          (:h3 :class "topic-name"
                            (cl-who:str topic)
                            (cl-who:fmt " (~D)" (length tasks)))
                          (:div :class "topic-cards"
                            (dolist (tk tasks)
                              (cl-who:htm
                                (:div :class "ready-card"
                                  (:a :href (format nil "/task?id=~A" (getf tk :id))
                                    (cl-who:str (or (getf tk :display-name)
                                                    (getf tk :bare-id)
                                                    (getf tk :id))))
                                  (:button :class "expand-btn expand-btn-sm"
                                           :hx-get (format nil "/api/task/expand?id=~A"
                                                     (getf tk :id))
                                           :hx-target (format nil "#expand-ready-~A"
                                                        (css-safe-id (getf tk :id)))
                                           :hx-swap "innerHTML"
                                           :hx-trigger "click once"
                                           :onclick "toggleExpand(this)"
                                           :hx-on-htmx-after-swap "afterExpand(event.detail.elt)"
                                    (cl-who:str "&#9656;"))
                                  (:div :id (format nil "expand-ready-~A"
                                              (css-safe-id (getf tk :id)))
                                        :class "expand-target")))))))))
                  (cl-who:htm
                    (:div :class "empty-state" "No tasks ready"))))))))))
