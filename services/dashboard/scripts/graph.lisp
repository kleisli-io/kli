;;;; KLI Dashboard — Graph Page WebGL Renderer (Parenscript)
;;;; Pixi.js-based rendering for high-performance force-directed graph.
;;;; D3 force simulation for physics, Pixi.js 8.x for WebGL rendering.
;;;;
;;;; Ported from old dashboard composition-webgl.lisp with:
;;;; - KLI token convention (--color-* instead of --bg/--border/etc)
;;;; - Fixed "references" layer name (plural, matching graph JSON data)
;;;; - Void-black background (#08080a instead of #0d1117)
;;;; - KLI typography (Space Mono / Inter)

(in-package :kli-dashboard)

(defun graph-js ()
  "Client-side JS for WebGL graph using Pixi.js + D3 force simulation."
  (ps:ps
    ;; Edge layer visual styles (decimal colors for Pixi)
    ;; Note: "references" (plural) matches graph JSON from task:graph-to-dashboard-json
    (defvar *layer-styles*
      (ps:create
        "references"        (ps:create "color" 5809919 "width" 1.5 "opacity" 0.5)   ; #58a6ff
        "topic"             (ps:create "color" 4176208 "width" 1.0 "opacity" 0.3)   ; #3fb950
        "same-day"          (ps:create "color" 13801762 "width" 0.8 "opacity" 0.2)  ; #d29922
        "declared-dep"      (ps:create "color" 16274761 "width" 2.5 "opacity" 0.9)  ; #f85149
        "phase-of"          (ps:create "color" 10711543 "width" 2.5 "opacity" 0.85) ; #a371f7
        "depends-on"        (ps:create "color" 15763518 "width" 2.0 "opacity" 0.8)  ; #f0883e
        "related-to"        (ps:create "color" 13867298 "width" 1.5 "opacity" 0.6)  ; #d39922 amber-ish
        "blocks"            (ps:create "color" 16743282 "width" 3.0 "opacity" 0.9)  ; #ff7b72
        "discovered-during" (ps:create "color" 7979263 "width" 1.2 "opacity" 0.5))) ; #79c0ff

    (defvar *layer-visible*
      (ps:create "references" nil "topic" nil "same-day" nil "declared-dep" nil
                 "phase-of" t "depends-on" t "related-to" t
                 "blocks" nil "discovered-during" nil))

    (defvar *structural-layers*
      (ps:create "phase-of" t "depends-on" t "related-to" t
                 "blocks" t "discovered-during" t))

    (defvar *pixi-state* nil)
    (defvar *selected-node* nil)
    (defvar *show-labels* t)
    (defvar *current-scale* 1)
    (defvar *force-params*
      (ps:create
        "charge" -200
        "link-dist-struct" 60
        "link-dist-know" 120
        "link-str-struct" 0.3
        "link-str-know" 0.08
        "center-y" 0.03
        "collide-struct" 18
        "collide-know" 12))

    (defvar *lod-labels-threshold* 0.4)
    (defvar *lod-detail-threshold* 0.25)

    (defun layer-prop (layer prop fallback)
      (let ((s (aref *layer-styles* layer)))
        (if s (or (aref s prop) fallback) fallback)))

    (defun edge-visible (d)
      (aref *layer-visible* (ps:@ d layer)))

    ;; --- Detail panel & highlighting ---

    ;; (build-edge-summary removed — replaced by build-edge-detail with sever buttons)

    ;; --- Mutation helpers ---

    (defun post-json (url body on-success)
      "POST JSON to URL, call ON-SUCCESS on 200 with parsed response."
      (let ((xhr (ps:new (-x-m-l-http-request))))
        (ps:chain xhr (open "POST" url true))
        (ps:chain xhr (set-request-header "Content-Type" "application/json"))
        (setf (ps:chain xhr onload)
          (lambda ()
            (let ((data (ps:chain -j-s-o-n (parse (ps:chain xhr response-text)))))
              (if (ps:@ data :ok)
                  (when on-success (funcall on-success data))
                  (alert (+ "Error: " (or (ps:@ data :error) "Unknown")))))))
        (setf (ps:chain xhr onerror)
          (lambda () (alert "Request failed")))
        (ps:chain xhr (send (ps:chain -j-s-o-n (stringify body))))))

    (defun graph-complete-task (task-id)
      "Mark a task as completed from graph panel."
      (post-json "/api/task/complete"
        (ps:create :task_id task-id)
        (lambda (data)
          ;; Update node status in graph data and refresh panel
          (let ((node (ps:chain (ps:@ *pixi-state* data nodes)
                        (find (lambda (n) (= (ps:@ n id) task-id))))))
            (when node
              (setf (ps:@ node status) "completed")
              (update-panel node))))))

    (defun graph-reopen-task (task-id)
      "Reopen a completed task from graph panel."
      (post-json "/api/task/reopen"
        (ps:create :task_id task-id)
        (lambda (data)
          (let ((node (ps:chain (ps:@ *pixi-state* data nodes)
                        (find (lambda (n) (= (ps:@ n id) task-id))))))
            (when node
              (setf (ps:@ node status) "active")
              (update-panel node))))))

    (defun graph-sever-edge (source-id target-id edge-type)
      "Sever an edge from source to target."
      (post-json "/api/task/sever"
        (ps:create :task_id source-id :target_id target-id :edge_type edge-type)
        (lambda (data)
          ;; Remove edge from graph data
          (setf (ps:@ *pixi-state* data edges)
                (ps:chain (ps:@ *pixi-state* data edges) (filter
                  (lambda (e)
                    (let ((src (if (ps:@ e source id) (ps:@ e source id) (ps:@ e source)))
                          (tgt (if (ps:@ e target id) (ps:@ e target id) (ps:@ e target))))
                      (not (and (= src source-id) (= tgt target-id) (= (ps:@ e layer) edge-type))))))))
          ;; Redraw edges and refresh panel
          (draw-edges (ps:@ *pixi-state* edges-graphics) (ps:@ *pixi-state* data))
          (let ((node (ps:chain (ps:@ *pixi-state* data nodes)
                        (find (lambda (n) (= (ps:@ n id) source-id))))))
            (when node (update-panel node))))))

    ;; Expose mutation functions to window for onclick handlers
    (setf (ps:@ window graph-complete-task) graph-complete-task)
    (setf (ps:@ window graph-reopen-task) graph-reopen-task)
    (setf (ps:@ window graph-sever-edge) graph-sever-edge)

    ;; --- Detail panel ---

    (defun build-edge-detail (node-id edges)
      "Build HTML for edges connected to a node, with sever buttons."
      (let ((incoming (ps:create))
            (outgoing (ps:create))
            (html ""))
        (ps:chain edges (for-each (lambda (e)
          (let ((src (if (ps:@ e source id) (ps:@ e source id) (ps:@ e source)))
                (tgt (if (ps:@ e target id) (ps:@ e target id) (ps:@ e target)))
                (layer (ps:@ e layer)))
            (when (edge-visible e)
              (cond
                ((= src node-id)
                 (unless (aref outgoing layer)
                   (setf (aref outgoing layer) (array)))
                 (ps:chain (aref outgoing layer) (push (ps:create "id" tgt "layer" layer))))
                ((= tgt node-id)
                 (unless (aref incoming layer)
                   (setf (aref incoming layer) (array)))
                 (ps:chain (aref incoming layer) (push (ps:create "id" src "layer" layer))))))))))
        ;; Render outgoing with sever buttons
        (let ((keys (ps:chain -object (keys outgoing))))
          (when (> (ps:@ keys length) 0)
            (setf html (+ html "<div class='edge-section'><span class='edge-dir'>&rarr; outgoing</span>"))
            (ps:chain keys (for-each (lambda (layer)
              (let ((style (aref *layer-styles* layer))
                    (targets (aref outgoing layer)))
                (setf html (+ html "<div class='edge-group-detail'>"))
                (setf html (+ html "<span class='edge-layer' style='color:#"
                               (ps:chain (aref style "color") (to-string 16) (pad-start 6 "0"))
                               "'>" layer "</span>"))
                (ps:chain targets (for-each (lambda (t-obj)
                  (let ((tid (ps:@ t-obj id))
                        (lyr (ps:@ t-obj layer)))
                    (setf html (+ html "<div class='edge-target-row'>"
                                   "<a href='/task?id=" tid "' class='edge-target-link'>"
                                   (ps:chain tid (replace (ps:new (-reg-exp "^[^:]+:" "")) ""))
                                   "</a>"
                                   "<button class='edge-sever-btn' onclick=\"graphSeverEdge('"
                                   node-id "','" tid "','" lyr "')\""
                                   " title='Sever edge'>&times;</button>"
                                   "</div>"))))))
                (setf html (+ html "</div>"))))))
            (setf html (+ html "</div>"))))
        ;; Render incoming (read-only — sever must be done from source)
        (let ((keys (ps:chain -object (keys incoming))))
          (when (> (ps:@ keys length) 0)
            (setf html (+ html "<div class='edge-section'><span class='edge-dir'>&larr; incoming</span>"))
            (ps:chain keys (for-each (lambda (layer)
              (let ((style (aref *layer-styles* layer))
                    (sources (aref incoming layer)))
                (setf html (+ html "<div class='edge-group-detail'>"))
                (setf html (+ html "<span class='edge-layer' style='color:#"
                               (ps:chain (aref style "color") (to-string 16) (pad-start 6 "0"))
                               "'>" layer "</span>"))
                (ps:chain sources (for-each (lambda (s-obj)
                  (let ((sid (ps:@ s-obj id)))
                    (setf html (+ html "<div class='edge-target-row'>"
                                   "<a href='/task?id=" sid "' class='edge-target-link'>"
                                   (ps:chain sid (replace (ps:new (-reg-exp "^[^:]+:" "")) ""))
                                   "</a></div>"))))))
                (setf html (+ html "</div>"))))))
            (setf html (+ html "</div>"))))
        html))

    (defun update-panel (d)
      "Update detail panel with node information and mutation controls."
      (let* ((panel (ps:chain document (get-element-by-id "detail-panel")))
             (all-edges (ps:@ *pixi-state* data edges))
             (edge-html (build-edge-detail (ps:@ d id) all-edges))
             (status (ps:@ d status))
             (is-completed (= status "completed"))
             (action-html
               (if (ps:@ d has-events)
                 (+ "<div class='panel-actions'>"
                    (if is-completed
                      (+ "<button class='panel-action-btn' onclick=\"graphReopenTask('"
                         (ps:@ d id) "')\">Reopen</button>")
                      (+ "<button class='panel-action-btn panel-complete-btn' onclick=\"graphCompleteTask('"
                         (ps:@ d id) "')\">Complete</button>"))
                    "</div>")
                 "")))
        (setf (ps:@ panel style display) "block"
              (ps:@ panel inner-h-t-m-l)
              (+ "<button onclick='window.closePanel()' class='detail-close'>&times;</button>"
                 "<h3>" (ps:@ d label) "</h3>"
                 "<div class='card-id'>" (ps:@ d id) "</div>"
                 "<div class='detail-row'><span class='detail-label'>Status</span>"
                 "<span class='status-badge status-" status "'>" status "</span></div>"
                 "<div class='detail-row'><span class='detail-label'>Date</span>"
                 "<span class='detail-value'>" (or (ps:@ d date) "—") "</span></div>"
                 "<div class='detail-row'><span class='detail-label'>Topic</span>"
                 "<span style='color:" (ps:@ d color) "'>" (ps:@ d topic) "</span></div>"
                 (if (ps:@ d has-events)
                   "<div class='detail-row'><span class='detail-label'>Type</span><span class='status-badge status-event-sourced'>event-sourced</span></div>"
                   "")
                 action-html
                 (if (> (ps:chain edge-html length) 0)
                   (+ "<div class='detail-edges'><div class='detail-label'>Connections</div>" edge-html "</div>")
                   "")
                 "<a href='/task?id=" (ps:@ d id) "' class='detail-link'>Open task detail &rarr;</a>"))))

    (defun highlight-node (d)
      "Highlight a node and dim unconnected nodes/edges in WebGL."
      (let ((connected (ps:create))
            (node-id (ps:@ d id))
            (all-edges (ps:@ *pixi-state* data edges))
            (node-sprites (ps:@ *pixi-state* node-sprites))
            (labels (ps:@ *pixi-state* labels))
            (nodes (ps:@ *pixi-state* data nodes)))
        (setf (aref connected node-id) t)
        ;; Find all connected nodes via visible edges
        (ps:chain all-edges (for-each (lambda (e)
          (let ((src (if (ps:@ e source id) (ps:@ e source id) (ps:@ e source)))
                (tgt (if (ps:@ e target id) (ps:@ e target id) (ps:@ e target))))
            (when (edge-visible e)
              (when (= src node-id) (setf (aref connected tgt) t))
              (when (= tgt node-id) (setf (aref connected src) t)))))))
        ;; Dim unconnected node sprites
        (ps:chain nodes (for-each (lambda (node i)
          (let ((sprite (aref node-sprites i))
                (label (aref labels i))
                (is-connected (aref connected (ps:@ node id))))
            (when sprite
              (setf (ps:@ sprite alpha) (if is-connected 1.0 0.15)))
            (when label
              (setf (ps:@ label visible) is-connected))))))))

    (defun clear-highlight ()
      "Restore all nodes to default appearance."
      (let ((node-sprites (ps:@ *pixi-state* node-sprites))
            (labels (ps:@ *pixi-state* labels))
            (labels-container (ps:@ *pixi-state* labels-container)))
        (setf *selected-node* nil)
        ;; Hide panel
        (ps:chain (ps:chain document (get-element-by-id "detail-panel"))
          (set-attribute "style" "display:none"))
        ;; Restore node alpha
        (ps:chain node-sprites (for-each (lambda (sprite)
          (when sprite (setf (ps:@ sprite alpha) 1.0)))))
        ;; Restore individual label visibility
        (when labels
          (ps:chain labels (for-each (lambda (label)
            (when label (setf (ps:@ label visible) t))))))
        ;; Restore container visibility based on LOD
        (let ((show-labels (and *show-labels* (> *current-scale* *lod-labels-threshold*))))
          (when labels-container
            (setf (ps:@ labels-container visible) show-labels)))
        ;; Redraw edges with normal opacity
        (draw-edges (ps:@ *pixi-state* edges-graphics) (ps:@ *pixi-state* data))))

    (defun handle-node-click (node)
      "Handle click on a node sprite."
      (if (and *selected-node* (= (ps:@ *selected-node* id) (ps:@ node id)))
        ;; Clicking selected node clears selection
        (clear-highlight)
        ;; Select new node
        (progn
          (setf *selected-node* node)
          (update-panel node)
          (highlight-node node)
          (draw-edges (ps:@ *pixi-state* edges-graphics) (ps:@ *pixi-state* data)))))

    (defun close-panel ()
      (clear-highlight))

    (setf (ps:@ window close-panel) close-panel)

    ;; --- Search ---

    (defun node-matches-query (d q)
      (or (> (ps:chain (ps:@ d id) (to-lower-case) (index-of q)) -1)
          (> (ps:chain (ps:@ d label) (to-lower-case) (index-of q)) -1)
          (> (ps:chain (ps:@ d topic) (to-lower-case) (index-of q)) -1)))

    (defun handle-search-input ()
      (let* ((search-el (ps:chain document (get-element-by-id "graph-search")))
             (q (ps:chain (ps:@ search-el value) (trim) (to-lower-case)))
             (node-sprites (ps:@ *pixi-state* node-sprites))
             (labels (ps:@ *pixi-state* labels))
             (nodes (ps:@ *pixi-state* data nodes)))
        (if (= q "")
          ;; Clear search — restore all
          (progn
            (ps:chain node-sprites (for-each (lambda (s)
              (when s (setf (ps:@ s alpha) 1.0)))))
            (ps:chain labels (for-each (lambda (l)
              (when l (setf (ps:@ l visible) t)))))
            (draw-edges (ps:@ *pixi-state* edges-graphics) (ps:@ *pixi-state* data)))
          ;; Filter by query
          (let ((match-set (ps:create)))
            (ps:chain nodes (for-each (lambda (n)
              (when (node-matches-query n q)
                (setf (aref match-set (ps:@ n id)) t)))))
            (ps:chain nodes (for-each (lambda (n i)
              (let ((matched (aref match-set (ps:@ n id))))
                (when (aref node-sprites i)
                  (setf (ps:@ (aref node-sprites i) alpha)
                        (if matched 1.0 0.1)))
                (when (aref labels i)
                  (setf (ps:@ (aref labels i) visible) matched))))))
            (draw-edges (ps:@ *pixi-state* edges-graphics) (ps:@ *pixi-state* data))))))

    (defun handle-search-keydown (ev)
      (when (= (ps:@ ev key) "Enter")
        (let* ((search-el (ps:chain document (get-element-by-id "graph-search")))
               (q (ps:chain (ps:@ search-el value) (trim) (to-lower-case)))
               (nodes (ps:@ *pixi-state* data nodes))
               (stage (ps:@ *pixi-state* stage))
               (width (ps:@ *pixi-state* width))
               (height (ps:@ *pixi-state* height))
               (match (ps:chain nodes (find (lambda (n) (node-matches-query n q))))))
          (when match
            ;; Zoom to matched node
            (let ((scale 1.5))
              (setf *current-scale* scale)
              (setf (ps:@ stage scale x) scale)
              (setf (ps:@ stage scale y) scale)
              (setf (ps:@ stage x) (- (/ width 2) (* scale (ps:@ match x))))
              (setf (ps:@ stage y) (- (/ height 2) (* scale (ps:@ match y))))
              (update-lod))))))

    ;; --- Pixi rendering functions ---

    (defun create-node-graphic (node degrees)
      "Create a Pixi Graphics object for a node."
      (let* ((g (ps:new (ps:chain -p-i-x-i -graphics)))
             (deg (or (aref degrees (ps:@ node id)) 0))
             (radius (if (ps:@ node has-events)
                       (+ 6 (min 8 (* deg 0.5)))
                       (+ 4 (min 5 (* deg 0.3)))))
             (color (parse-int (ps:chain (ps:@ node color) (replace "#" "")) 16))
             (stroke-color (if (ps:@ node has-events) 10711543 1973800)) ; #a371f7 / #1e1e28
             (stroke-width (if (ps:@ node has-events) 2 1)))
        (ps:chain g (circle 0 0 radius) (fill (ps:create "color" color "alpha" 0.85)))
        (ps:chain g (circle 0 0 radius) (stroke (ps:create "color" stroke-color "width" stroke-width)))
        (setf (ps:@ g node-data) node)
        (setf (ps:@ g node-radius) radius)
        (setf (ps:@ g event-mode) "static")
        (setf (ps:@ g cursor) "pointer")
        ;; Explicit hit area — minimum 14px radius for usable click targets
        (setf (ps:@ g hit-area) (ps:new ((ps:chain -p-i-x-i -circle) 0 0 (max radius 14))))
        g))

    (defun create-label (node)
      "Create a Pixi Text object for a node label.
       Rendered at 3x resolution for crisp text when zoomed in."
      (let* ((font-size (if (ps:@ node has-events) 10 8))
             (render-scale 3)
             (opts (ps:create
                     "text" (ps:@ node label)
                     "style" (ps:create
                               "fontFamily" "Space Mono, monospace"
                               "fontSize" (* font-size render-scale)
                               "fill" (if (ps:@ node has-events) "#e8e8ee" "#7b7b89")
                               "align" "center")))
             (text (ps:new ((ps:chain -p-i-x-i -text) opts))))
        (setf (ps:@ text anchor x) 0.5)
        (setf (ps:@ text anchor y) 0)
        ;; Scale down to display size — texture stays high-res for zoom
        (setf (ps:@ text scale x) (/ 1 render-scale))
        (setf (ps:@ text scale y) (/ 1 render-scale))
        text))

    (defun draw-edges (graphics data)
      "Draw all visible edges using Pixi Graphics."
      (ps:chain graphics (clear))
      (let ((sel-id (when *selected-node* (ps:@ *selected-node* id))))
        (ps:chain (ps:@ data edges) (for-each (lambda (e)
          (when (edge-visible e)
            (let* ((src (ps:@ e source))
                   (tgt (ps:@ e target))
                   (src-id (if (ps:@ src id) (ps:@ src id) src))
                   (tgt-id (if (ps:@ tgt id) (ps:@ tgt id) tgt))
                   (style (aref *layer-styles* (ps:@ e layer)))
                   (color (or (aref style "color") 1973800))
                   (width (or (aref style "width") 1))
                   (base-alpha (or (aref style "opacity") 0.5))
                   ;; Dim edges not connected to selected node
                   (alpha (if sel-id
                            (if (or (= src-id sel-id) (= tgt-id sel-id))
                              1.0
                              0.04)
                            base-alpha)))
              (ps:chain graphics
                (move-to (ps:@ src x) (ps:@ src y))
                (line-to (ps:@ tgt x) (ps:@ tgt y))
                (stroke (ps:create "color" color "width" width "alpha" alpha))))))))))

    (defun update-node-positions ()
      "Update Pixi sprite positions from D3 simulation data."
      (let ((node-sprites (ps:@ *pixi-state* node-sprites))
            (labels (ps:@ *pixi-state* labels))
            (data (ps:@ *pixi-state* data)))
        (ps:chain (ps:@ data nodes) (for-each (lambda (node i)
          (let ((sprite (aref node-sprites i))
                (label (aref labels i)))
            (when sprite
              (setf (ps:@ sprite x) (ps:@ node x))
              (setf (ps:@ sprite y) (ps:@ node y)))
            (when label
              (setf (ps:@ label x) (ps:@ node x))
              (setf (ps:@ label y) (+ (ps:@ node y)
                                       (or (ps:@ sprite node-radius) 8)
                                       4)))))))))

    (defun update-lod ()
      "Update level-of-detail based on current zoom."
      (let ((labels-container (ps:@ *pixi-state* labels-container))
            (show-labels (and *show-labels* (> *current-scale* *lod-labels-threshold*))))
        (when labels-container
          (setf (ps:@ labels-container visible) show-labels))))

    ;; --- Zoom/Pan handling ---

    (defun setup-zoom-pan (app stage)
      "Setup zoom and pan via mouse wheel and drag."
      (let ((dragging nil)
            (drag-start (ps:create "x" 0 "y" 0))
            (stage-start (ps:create "x" 0 "y" 0)))

        (ps:chain (ps:@ app canvas) (add-event-listener "wheel" (lambda (ev)
          (ps:chain ev (prevent-default))
          (let* ((delta (if (> (ps:@ ev delta-y) 0) 0.95 1.05))
                 (new-scale (* *current-scale* delta))
                 (clamped (min 8 (max 0.02 new-scale)))
                 (mouse-x (ps:@ ev offset-x))
                 (mouse-y (ps:@ ev offset-y))
                 (world-x (/ (- mouse-x (ps:@ stage x)) *current-scale*))
                 (world-y (/ (- mouse-y (ps:@ stage y)) *current-scale*)))
            (setf *current-scale* clamped)
            (setf (ps:@ stage scale x) clamped)
            (setf (ps:@ stage scale y) clamped)
            (setf (ps:@ stage x) (- mouse-x (* world-x clamped)))
            (setf (ps:@ stage y) (- mouse-y (* world-y clamped)))
            (update-lod)))))

        (ps:chain (ps:@ app canvas) (add-event-listener "mousedown" (lambda (ev)
          (setf dragging t)
          (setf (ps:@ drag-start x) (ps:@ ev client-x))
          (setf (ps:@ drag-start y) (ps:@ ev client-y))
          (setf (ps:@ stage-start x) (ps:@ stage x))
          (setf (ps:@ stage-start y) (ps:@ stage y)))))

        (ps:chain (ps:@ app canvas) (add-event-listener "mousemove" (lambda (ev)
          (when dragging
            (let ((dx (* 0.7 (- (ps:@ ev client-x) (ps:@ drag-start x))))
                  (dy (* 0.7 (- (ps:@ ev client-y) (ps:@ drag-start y)))))
              (setf (ps:@ stage x) (+ (ps:@ stage-start x) dx))
              (setf (ps:@ stage y) (+ (ps:@ stage-start y) dy)))))))

        (ps:chain (ps:@ app canvas) (add-event-listener "mouseup" (lambda (ev)
          (setf dragging nil))))

        (ps:chain (ps:@ app canvas) (add-event-listener "mouseleave" (lambda (ev)
          (setf dragging nil))))))

    ;; --- Force simulation helpers ---

    (defun date-to-x (date-str min-ts max-ts width)
      (if (or (not date-str) (= date-str ""))
        (/ width 2)
        (let* ((ts (ps:chain -date (parse date-str)))
               (range (max 1 (- max-ts min-ts)))
               (pct (/ (- ts min-ts) range)))
          (+ 100 (* pct (- width 200))))))

    (defun compute-node-degree (nodes edges)
      (let ((deg (ps:create)))
        (ps:chain nodes (for-each (lambda (n) (setf (aref deg (ps:@ n id)) 0))))
        (ps:chain edges (for-each (lambda (e)
          (let ((src (ps:@ e source))
                (tgt (ps:@ e target)))
            (when (aref deg src) (setf (aref deg src) (+ (aref deg src) 1)))
            (when (aref deg tgt) (setf (aref deg tgt) (+ (aref deg tgt) 1)))))))
        deg))

    ;; --- Slider handlers ---

    (defun update-slider-value (slider-id value)
      (let ((val-el (ps:chain document (get-element-by-id (+ slider-id "-val")))))
        (when val-el
          (setf (ps:@ val-el text-content) value))))

    (defun handle-charge-change (ev)
      (let* ((val (parse-int (ps:@ ev target value)))
             (sim (ps:@ *pixi-state* sim)))
        (setf (aref *force-params* "charge") val)
        (update-slider-value "slider-charge" val)
        (ps:chain sim (force "charge") (strength val))
        (ps:chain sim (alpha 0.3) (restart))))

    (defun handle-link-dist-change (ev)
      (let* ((val (parse-int (ps:@ ev target value)))
             (sim (ps:@ *pixi-state* sim)))
        (setf (aref *force-params* "link-dist-struct") val)
        (setf (aref *force-params* "link-dist-know") (* val 2))
        (update-slider-value "slider-link-dist" val)
        (ps:chain sim (force "link")
          (distance (lambda (d)
            (if (aref *structural-layers* (ps:@ d layer))
              (aref *force-params* "link-dist-struct")
              (aref *force-params* "link-dist-know")))))
        (ps:chain sim (alpha 0.3) (restart))))

    (defun handle-link-str-change (ev)
      (let* ((val (/ (parse-int (ps:@ ev target value)) 100))
             (sim (ps:@ *pixi-state* sim)))
        (setf (aref *force-params* "link-str-struct") val)
        (setf (aref *force-params* "link-str-know") (/ val 4))
        (update-slider-value "slider-link-str" (ps:chain val (to-fixed 2)))
        (ps:chain sim (force "link")
          (strength (lambda (d)
            (if (aref *structural-layers* (ps:@ d layer))
              (aref *force-params* "link-str-struct")
              (aref *force-params* "link-str-know")))))
        (ps:chain sim (alpha 0.3) (restart))))

    (defun handle-center-y-change (ev)
      (let* ((val (/ (parse-int (ps:@ ev target value)) 1000))
             (sim (ps:@ *pixi-state* sim)))
        (setf (aref *force-params* "center-y") val)
        (update-slider-value "slider-center-y" (ps:chain val (to-fixed 3)))
        (ps:chain sim (force "y") (strength val))
        (ps:chain sim (alpha 0.3) (restart))))

    (defun handle-collide-change (ev)
      (let* ((val (parse-int (ps:@ ev target value)))
             (sim (ps:@ *pixi-state* sim)))
        (setf (aref *force-params* "collide-struct") val)
        (setf (aref *force-params* "collide-know") (ps:chain -math (round (* val 0.67))))
        (update-slider-value "slider-collide" val)
        (ps:chain sim (force "collide")
          (radius (lambda (d)
            (if (ps:@ d has-events)
              (aref *force-params* "collide-struct")
              (aref *force-params* "collide-know")))))
        (ps:chain sim (alpha 0.3) (restart))))

    (defun setup-controls ()
      "Setup UI control event listeners."
      ;; Layer toggles
      (ps:chain document (query-selector-all ".layer-toggles input[type=checkbox]")
        (for-each (lambda (cb)
          (ps:chain cb (add-event-listener "change" (lambda ()
            (setf (aref *layer-visible* (ps:@ cb dataset layer)) (ps:@ cb checked))
            (draw-edges (ps:@ *pixi-state* edges-graphics) (ps:@ *pixi-state* data))))))))

      ;; Sliders
      (let ((charge-el (ps:chain document (get-element-by-id "slider-charge")))
            (link-dist-el (ps:chain document (get-element-by-id "slider-link-dist")))
            (link-str-el (ps:chain document (get-element-by-id "slider-link-str")))
            (center-y-el (ps:chain document (get-element-by-id "slider-center-y")))
            (collide-el (ps:chain document (get-element-by-id "slider-collide"))))
        (when charge-el
          (ps:chain charge-el (add-event-listener "input" handle-charge-change)))
        (when link-dist-el
          (ps:chain link-dist-el (add-event-listener "input" handle-link-dist-change)))
        (when link-str-el
          (ps:chain link-str-el (add-event-listener "input" handle-link-str-change)))
        (when center-y-el
          (ps:chain center-y-el (add-event-listener "input" handle-center-y-change)))
        (when collide-el
          (ps:chain collide-el (add-event-listener "input" handle-collide-change))))

      ;; Fit button
      (let ((fit-btn (ps:chain document (get-element-by-id "btn-fit"))))
        (when fit-btn
          (ps:chain fit-btn (add-event-listener "click" (lambda ()
            (let* ((data (ps:@ *pixi-state* data))
                   (stage (ps:@ *pixi-state* stage))
                   (width (ps:@ *pixi-state* width))
                   (height (ps:@ *pixi-state* height))
                   (min-x 99999) (max-x -99999)
                   (min-y 99999) (max-y -99999))
              (ps:chain (ps:@ data nodes) (for-each (lambda (n)
                (when (< (ps:@ n x) min-x) (setf min-x (ps:@ n x)))
                (when (> (ps:@ n x) max-x) (setf max-x (ps:@ n x)))
                (when (< (ps:@ n y) min-y) (setf min-y (ps:@ n y)))
                (when (> (ps:@ n y) max-y) (setf max-y (ps:@ n y))))))
              (let* ((pad 60)
                     (gw (+ (- max-x min-x) (* 2 pad)))
                     (gh (+ (- max-y min-y) (* 2 pad)))
                     (scale (min (/ width gw) (/ height gh) 1.5))
                     (cx (/ (+ min-x max-x) 2))
                     (cy (/ (+ min-y max-y) 2)))
                (setf *current-scale* scale)
                (setf (ps:@ stage scale x) scale)
                (setf (ps:@ stage scale y) scale)
                (setf (ps:@ stage x) (- (/ width 2) (* scale cx)))
                (setf (ps:@ stage y) (- (/ height 2) (* scale cy)))
                (update-lod))))))))

      ;; Labels toggle
      (let ((labels-btn (ps:chain document (get-element-by-id "btn-labels"))))
        (when labels-btn
          (ps:chain labels-btn (add-event-listener "click" (lambda ()
            (setf *show-labels* (not *show-labels*))
            (if *show-labels*
              (ps:chain labels-btn class-list (add "active"))
              (ps:chain labels-btn class-list (remove "active")))
            (update-lod))))))

      ;; Search
      (let ((search-el (ps:chain document (get-element-by-id "graph-search"))))
        (when search-el
          (ps:chain search-el (add-event-listener "input" handle-search-input))
          (ps:chain search-el (add-event-listener "keydown" handle-search-keydown)))))

    ;; --- Main init (async) ---

    (defun init-pixi-app (app data container width height min-ts max-ts degrees)
      "Initialize Pixi app after async init completes."
      (ps:chain container (append-child (ps:@ app canvas)))
      ;; Set CSS dimensions explicitly so Pixi's internal resolution stays crisp
      (setf (ps:@ (ps:@ app canvas) style width) (+ width "px"))
      (setf (ps:@ (ps:@ app canvas) style height) (+ height "px"))

        ;; Create stage container for zoom/pan
        (let ((stage (ps:new (ps:chain -p-i-x-i -container))))
          (ps:chain (ps:@ app stage) (add-child stage))

          ;; Make stage interactive to catch clicks on empty space
          (setf (ps:@ stage event-mode) "static")
          (setf (ps:@ stage hit-area) (ps:new ((ps:chain -p-i-x-i -rectangle) (- 0 5000) (- 0 5000) 10000 10000)))
          (ps:chain stage (on "pointerdown" (lambda (ev)
            (when *selected-node*
              (clear-highlight)))))

          ;; Create edges layer
          (let ((edges-graphics (ps:new (ps:chain -p-i-x-i -graphics))))
            (ps:chain stage (add-child edges-graphics))

            ;; Create nodes and labels containers
            (let ((nodes-container (ps:new (ps:chain -p-i-x-i -container)))
                  (labels-container (ps:new (ps:chain -p-i-x-i -container)))
                  (node-sprites (array))
                  (labels (array)))
              (ps:chain stage (add-child nodes-container))
              (ps:chain stage (add-child labels-container))

              ;; Create node sprites and labels
              (ps:chain (ps:@ data nodes) (for-each (lambda (node)
                (let ((sprite (create-node-graphic node degrees))
                      (label (create-label node)))
                  ;; Attach click handler
                  (ps:chain sprite (on "pointerdown" (lambda (ev)
                    (when (and ev (ps:@ ev stop-propagation))
                      (ps:chain ev (stop-propagation)))
                    (handle-node-click (ps:@ sprite node-data)))))
                  (ps:chain node-sprites (push sprite))
                  (ps:chain labels (push label))
                  (ps:chain nodes-container (add-child sprite))
                  (ps:chain labels-container (add-child label))))))

              ;; Create D3 force simulation
              (let ((sim (ps:chain d3 (force-simulation (ps:@ data nodes))
                           (alpha-decay 0.05)
                           (alpha-min 0.001)
                           (force "link" (ps:chain d3 (force-link (ps:@ data edges))
                                           (id (lambda (d) (ps:@ d id)))
                                           (distance (lambda (d)
                                             (if (aref *structural-layers* (ps:@ d layer)) 60 120)))
                                           (strength (lambda (d)
                                             (if (aref *structural-layers* (ps:@ d layer)) 0.3 0.08)))))
                           (force "charge" (ps:chain d3 (force-many-body)
                                             (strength -200) (distance-max 600)))
                           (force "x" (ps:chain d3 (force-x)
                                        (x (lambda (d) (date-to-x (ps:@ d date) min-ts max-ts width)))
                                        (strength 0.12)))
                           (force "y" (ps:chain d3 (force-y (/ height 2)) (strength 0.03)))
                           (force "collide" (ps:chain d3 (force-collide)
                                              (radius (lambda (d)
                                                (if (ps:@ d has-events) 18 12))))))))

                ;; Store state
                (setf *pixi-state* (ps:create
                                     "app" app
                                     "stage" stage
                                     "nodesContainer" nodes-container
                                     "labelsContainer" labels-container
                                     "edgesGraphics" edges-graphics
                                     "nodeSprites" node-sprites
                                     "labels" labels
                                     "data" data
                                     "sim" sim
                                     "width" width
                                     "height" height))

                ;; D3 tick handler
                (ps:chain sim (on "tick" (lambda ()
                  (draw-edges edges-graphics data)
                  (update-node-positions))))

                ;; Setup zoom/pan and controls
                (setup-zoom-pan app stage)
                (setup-controls)

                ;; Build cluster legend
                (let ((legend-el (ps:chain document (get-element-by-id "cluster-legend"))))
                  (when (and legend-el (ps:@ data clusters))
                    (setf (ps:@ legend-el inner-h-t-m-l)
                          (+ "<h4>Clusters</h4>"
                             (ps:chain (ps:@ data clusters)
                               (map (lambda (c)
                                      (+ "<div class='legend-item'><span class='legend-dot' style='background:"
                                         (ps:@ c color) "'></span>" (ps:@ c name)
                                         " (" (ps:@ c count) ")</div>")))
                               (join "")))))))))))

    (defun filter-graph-by-depot (data depot)
      "Filter graph data to only include nodes/edges for the given depot."
      (when (and depot (> (ps:@ depot length) 0))
        (let ((prefix (+ depot ":")))
          (setf (ps:@ data nodes)
                (ps:chain (ps:@ data nodes) (filter (lambda (n)
                  (ps:chain (ps:@ n id) (starts-with prefix))))))
          (let ((node-ids (ps:create)))
            (ps:chain (ps:@ data nodes) (for-each (lambda (n)
              (setf (aref node-ids (ps:@ n id)) t))))
            (setf (ps:@ data edges)
                  (ps:chain (ps:@ data edges) (filter (lambda (e)
                    (and (aref node-ids (ps:@ e source))
                         (aref node-ids (ps:@ e target))))))))))
      data)

    (defun init-webgl-graph ()
      "Initialize the WebGL graph renderer."
      (let* ((container (ps:chain document (get-element-by-id "graph-container")))
             (depot (ps:chain container (get-attribute "data-depot")))
             (data (filter-graph-by-depot graph-data depot))
             (nav-h 48)
             (tabs-el (ps:chain document (query-selector ".depot-tabs")))
             (tabs-h (if tabs-el (ps:@ tabs-el offset-height) 0))
             (width (ps:@ window inner-width))
             (height (- (ps:@ window inner-height) nav-h tabs-h))
             (min-ts (ps:chain -date (parse (ps:@ data min-date))))
             (max-ts (ps:chain -date (parse (ps:@ data max-date))))
             (degrees (compute-node-degree (ps:@ data nodes) (ps:@ data edges)))
             (app (ps:new (ps:chain -p-i-x-i -application))))

        ;; Pixi.js 8.x async init
        (ps:chain app (init (ps:create
                              "width" width
                              "height" height
                              "background" 526346   ; #08080a — KLI void-black
                              "antialias" t
                              "resolution" (ps:@ window device-pixel-ratio)))
          (then (lambda () (init-pixi-app app data container width height min-ts max-ts degrees))))))

    (setf (ps:@ window onload) init-webgl-graph)))
