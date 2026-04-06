;;;; KLI Dashboard — Graph Page (WebGL/Pixi.js)
;;;; High-performance graph visualization using WebGL rendering.
;;;; D3 force simulation for physics, Pixi.js for rendering.

(in-package :kli-dashboard)

;;; ============================================================
;;; Parenscript Output Fixups
;;; ============================================================

(defun fix-ps-output (js)
  "Fix known Parenscript output issues (bangequals → !==)."
  (let ((result js))
    (loop for pos = (search "bangequals(" result)
          while pos
          do (let* ((args-start (+ pos (length "bangequals(")))
                    (comma (position #\, result :start args-start))
                    (close (position #\) result :start args-start))
                    (arg1 (string-trim " " (subseq result args-start comma)))
                    (arg2 (string-trim " " (subseq result (1+ comma) close))))
               (setf result (concatenate 'string
                 (subseq result 0 pos)
                 arg1 " !== " arg2
                 (subseq result (1+ close))))))
    result))

;;; ============================================================
;;; Graph Page Renderer (WebGL)
;;; ============================================================

(defun render-graph-page (&optional depot)
  "Render the full-bleed WebGL graph page with Pixi.js rendering.
   DEPOT filters to a specific depot (nil = all)."
  (let ((graph-json (get-graph-json)))
    (htm-str
      (:div :class "board graph-board"
        (cl-who:str (or (render-depot-tabs depot "/graph") ""))
        (:div :id "graph-container"
              :data-depot (or depot ""))
        (:div :id "graph-controls"
          (:div :class "graph-search"
            (:input :type "text" :id "graph-search" :placeholder "Search tasks..."))
          (:div :class "layer-toggles"
            (:div :class "toggle-group-label" "Knowledge")
            (:label (:input :type "checkbox" :data-layer "references") " Reference")
            (:label (:input :type "checkbox" :data-layer "topic") " Topic")
            (:label (:input :type "checkbox" :data-layer "same-day") " Same-day")
            (:label (:input :type "checkbox" :data-layer "declared-dep") " Declared")
            (:div :class "toggle-group-label" "Structural")
            (:label (:input :type "checkbox" :checked "checked" :data-layer "phase-of") " Phase-of")
            (:label (:input :type "checkbox" :checked "checked" :data-layer "depends-on") " Depends-on")
            (:label (:input :type "checkbox" :checked "checked" :data-layer "related-to") " Related-to")
            (:label (:input :type "checkbox" :data-layer "blocks") " Blocks")
            (:label (:input :type "checkbox" :data-layer "discovered-during") " Discovered"))
          (:div :class "graph-actions"
            (:button :id "btn-fit" :class "graph-btn" "Fit")
            (:button :id "btn-labels" :class "graph-btn active" "Labels"))
          (:div :class "force-sliders"
            (:div :class "toggle-group-label" "Forces")
            (:div :class "slider-row"
              (:label :for "slider-charge" "Repel")
              (:input :type "range" :id "slider-charge" :min "-500" :max "-50" :value "-200")
              (:span :id "slider-charge-val" :class "slider-val" "-200"))
            (:div :class "slider-row"
              (:label :for "slider-link-dist" "Distance")
              (:input :type "range" :id "slider-link-dist" :min "20" :max "200" :value "60")
              (:span :id "slider-link-dist-val" :class "slider-val" "60"))
            (:div :class "slider-row"
              (:label :for "slider-link-str" "Link Pull")
              (:input :type "range" :id "slider-link-str" :min "5" :max "80" :value "30")
              (:span :id "slider-link-str-val" :class "slider-val" "0.30"))
            (:div :class "slider-row"
              (:label :for "slider-center-y" "Center Y")
              (:input :type "range" :id "slider-center-y" :min "0" :max "100" :value "30")
              (:span :id "slider-center-y-val" :class "slider-val" "0.030"))
            (:div :class "slider-row"
              (:label :for "slider-collide" "Spread")
              (:input :type "range" :id "slider-collide" :min "5" :max "40" :value "18")
              (:span :id "slider-collide-val" :class "slider-val" "18")))
)
        (:div :id "detail-panel")
        (:div :id "cluster-legend")
        (:script :type "text/javascript"
          (cl-who:fmt "var graphData = ~A;" graph-json))
        (:script :type "text/javascript"
          (cl-who:str (fix-ps-output (graph-js))))))))
