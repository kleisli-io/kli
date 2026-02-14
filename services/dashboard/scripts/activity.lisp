;;;; KLI Dashboard — Activity Page Scripts
;;;; Client-side filtering for event categories + HTMX sentinel management.

(in-package :kli-dashboard)

(defun activity-filter-script ()
  "Generate JavaScript for event category filtering.
   Updates DOM visibility, hidden input, and re-registers sentinel for lazy loading."
  (ps:ps
    (defun update-sentinel-category (category)
      "Update the load-more sentinel's URL and re-register with HTMX.
       The lol-reactive HTMX runtime captures hx-get at processElement time,
       so we must clone/replace the sentinel to trigger re-registration."
      (let ((sentinel (ps:chain document (get-element-by-id "load-more-sentinel"))))
        (when sentinel
          (let* ((current-url (ps:chain sentinel (get-attribute "hx-get")))
                 (base-url (ps:chain current-url (split "?") 0))
                 (params (ps:new (-u-r-l-search-params
                                  (or (ps:chain current-url (split "?") 1) "")))))
            ;; Update category param
            (ps:chain params (set "category" category))
            (let ((new-url (+ base-url "?" (ps:chain params (to-string)))))
              ;; Update attribute
              (ps:chain sentinel (set-attribute "hx-get" new-url))
              ;; Clone and replace to trigger HTMX re-registration
              (let ((clone (ps:chain sentinel (clone-node t))))
                (ps:chain sentinel parent-node (replace-child clone sentinel))
                ;; Re-process with HTMX
                (when (ps:@ window htmx)
                  (ps:chain window htmx (process clone)))))))))

    (defun filter-events (category)
      "Filter visible event rows by category and update UI state."
      (let ((rows (ps:chain document (query-selector-all ".event-row")))
            (buttons (ps:chain document (query-selector-all ".filter-btn")))
            (filter-input (ps:chain document (get-element-by-id "current-filter"))))
        ;; Update hidden input
        (when filter-input
          (setf (ps:@ filter-input value) category))
        ;; Update sentinel with new category
        (update-sentinel-category category)
        ;; Update active button
        (ps:chain buttons (for-each (lambda (btn)
          (ps:chain btn class-list (remove "active")))))
        ;; Mark clicked button as active (use event.target)
        (when (ps:@ event target)
          (ps:chain event target class-list (add "active")))
        ;; Show/hide rows
        (ps:chain rows (for-each (lambda (row)
          (let ((row-cat (ps:chain row (get-attribute "data-category"))))
            (setf (ps:@ row style display)
                  (if (or (= category "all") (= category row-cat))
                    "" "none"))))))))))
