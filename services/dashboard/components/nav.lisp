;;;; KLI Dashboard — Navigation component

(in-package :kli-dashboard)

(defun render-nav (&optional active-route &key depot)
  "Render the top navigation bar with depot counts."
  (let ((depots (get-available-depots))
        (tasks (or (get-enriched-tasks) nil)))
    (flet ((nav-href (path)
             (if depot (format nil "~A?depot=~A" path depot) path)))
      (htm-str
        (:nav :class "dashboard-nav"
          (:div :class "nav-left"
            (:span :class "nav-brand" "kli")
            (:div :class "nav-links"
              (:a :href (nav-href "/")
                  :class (format nil "nav-link~@[ active~]" (string= active-route "/"))
                  "frontier")
              (:a :href (nav-href "/activity")
                  :class (format nil "nav-link~@[ active~]" (string= active-route "/activity"))
                  "activity")
              (:a :href (nav-href "/stats")
                  :class (format nil "nav-link~@[ active~]" (string= active-route "/stats"))
                  "stats")
              (:a :href (nav-href "/sessions")
                  :class (format nil "nav-link~@[ active~]" (string= active-route "/sessions"))
                  "sessions")
              (:a :href "/plan"
                  :class (format nil "nav-link~@[ active~]" (string= active-route "/plan"))
                  "plans")
              (:a :href (nav-href "/clusters")
                  :class (format nil "nav-link~@[ active~]" (string= active-route "/clusters"))
                  "clusters")
              (:a :href (nav-href "/graph")
                  :class (format nil "nav-link~@[ active~]" (string= active-route "/graph"))
                  "graph")
              (:a :href (nav-href "/health")
                  :class (format nil "nav-link~@[ active~]" (string= active-route "/health"))
                  "health")))
          (:div :class "nav-right"
            (:button :class "scratchpad-open-btn nav-scratchpad-btn"
                     :title "Global Scratchpad"
                     :onclick (format nil "openScratchpad('~A:__global__','Global Scratchpad')"
                                      (or depot (first depots) "core"))
              (cl-who:str "&#9998;"))
            (dolist (d depots)
              (cl-who:htm
                (:span :class "nav-stat"
                  (cl-who:fmt "~A ~D" d
                    (count d tasks
                      :key (lambda (tk) (getf tk :depot))
                      :test #'string-equal)))))))))))
