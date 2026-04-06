;;;; kli dashboard — Route definitions
;;;; All lol-reactive defroute registrations.

(in-package :kli-dashboard)

;;; ============================================================
;;; PAGE ROUTES
;;; ============================================================

(lol-reactive:defroute "/" (:method :get)
  "Frontier — 4-section task dashboard"
  (let ((depot (hunchentoot:parameter "depot")))
    (html-page
      :title "kli"
      :include-tailwind nil
      :include-htmx nil
      :head-extra (htm-str
                    (:script (cl-who:str (lol-reactive:htmx-runtime-js))))
      :body (format nil "~A~A~A~A"
                    (render-nav "/" :depot depot)
                    (render-frontier-page depot)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

(lol-reactive:defroute "/health" (:method :get)
  "Health diagnostics"
  (let ((depot (hunchentoot:parameter "depot")))
    (html-page
      :title "Health — kli"
      :include-tailwind nil
      :include-htmx nil
      :body (format nil "~A~A~A~A"
                    (render-nav "/health" :depot depot)
                    (render-health-page depot)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

(lol-reactive:defroute "/activity" (:method :get)
  "Activity stream with category filters and infinite scroll"
  (let ((depot (hunchentoot:parameter "depot")))
    (html-page
      :title "Activity — kli"
      :include-tailwind nil
      :include-htmx nil
      :head-extra (htm-str
                    (:script (cl-who:str (activity-filter-script)))
                    (:script (cl-who:str (lol-reactive:htmx-runtime-js))))
      :body (format nil "~A~A~A~A"
                    (render-nav "/activity" :depot depot)
                    (render-activity-page depot)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

(lol-reactive:defroute "/stats" (:method :get)
  "Aggregate statistics with charts"
  (let ((depot (hunchentoot:parameter "depot")))
    (html-page
      :title "Stats — kli"
      :include-tailwind nil
      :include-htmx nil
      :body (format nil "~A~A~A~A"
                    (render-nav "/stats" :depot depot)
                    (render-stats-page depot)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

(lol-reactive:defroute "/sessions" (:method :get)
  "Active and historical sessions"
  (let ((depot (hunchentoot:parameter "depot")))
    (html-page
      :title "Sessions — kli"
      :include-tailwind nil
      :include-htmx nil
      :head-extra (htm-str
                    (:meta :http-equiv "refresh" :content "30"))
      :body (format nil "~A~A~A~A"
                    (render-nav "/sessions" :depot depot)
                    (render-sessions-page depot)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

(lol-reactive:defroute "/task" (:method :get)
  "Task detail view"
  (let ((task-id (hunchentoot:parameter "id")))
    (html-page
      :title (if (and task-id (> (length task-id) 0))
                 (format nil "~A — kli" (humanize-task-name task-id))
                 "Task — kli")
      :include-tailwind nil
      :include-htmx nil
      :head-extra (htm-str
                    (:script (cl-who:str (lol-reactive:htmx-runtime-js)))
                    (:style (cl-who:str ".expand-target{display:none}.expand-btn.open+.expand-target{display:block}.expand-btn{background:none;border:none;color:var(--color-text);font-family:var(--font-mono);font-size:0.78rem;cursor:pointer;padding:4px 0}.expand-btn:hover{color:var(--color-accent)}.expand-btn.loading{opacity:0.5}.expand-panel{padding:8px 0 4px 16px;font-size:0.78rem;border-left:2px solid var(--color-border)}.expand-section{margin-bottom:8px}.expand-label{font-family:var(--font-mono);font-size:0.6875rem;color:var(--color-muted);text-transform:uppercase;letter-spacing:0.08em;margin-bottom:2px}.expand-desc{color:var(--color-text);line-height:1.5}.expand-handoff{font-size:0.72rem;color:var(--color-text);line-height:1.5;white-space:pre-wrap;max-height:400px;overflow-y:auto;background:var(--color-surface-2);padding:8px;border-radius:2px;border:1px solid var(--color-border)}}")))
      :body (format nil "~A~A~A~A"
                    (render-nav nil)
                    (render-task-detail task-id)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

(lol-reactive:defroute "/plan" (:method :get)
  "Plan view — phase DAG with expandable cards"
  (let ((task-id (hunchentoot:parameter "id")))
    (html-page
      :title (if (and task-id (> (length task-id) 0))
                 (format nil "Plan: ~A — kli" (humanize-task-name task-id))
                 "Plans — kli")
      :include-tailwind nil
      :include-htmx nil
      :body (format nil "~A~A~A~A"
                    (render-nav "/plan")
                    (render-plan-page task-id)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

(lol-reactive:defroute "/clusters" (:method :get)
  "Topic cluster groupings"
  (let ((depot (hunchentoot:parameter "depot")))
    (html-page
      :title "Clusters — kli"
      :include-tailwind nil
      :include-htmx nil
      :body (format nil "~A~A~A~A"
                    (render-nav "/clusters" :depot depot)
                    (render-clusters-page depot)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

(lol-reactive:defroute "/graph" (:method :get)
  "Full-bleed WebGL force graph (Pixi.js + D3)"
  (let ((depot (hunchentoot:parameter "depot")))
    (html-page
      :title "Graph — kli"
      :include-tailwind nil
      :include-htmx nil
      :head-extra (htm-str
                    (:script :src "https://d3js.org/d3.v7.min.js")
                    (:script :src "https://cdn.jsdelivr.net/npm/pixi.js@8.2.6/dist/pixi.min.js"))
      :body (format nil "~A~A~A~A"
                    (render-nav "/graph" :depot depot)
                    (render-graph-page depot)
                    (htm-str (:script (cl-who:str (scratchpad-js))))
                    (render-scratchpad-sidebar)))))

;;; ============================================================
;;; ACTIVITY PAGINATION API
;;; ============================================================

(lol-reactive:defroute "/api/activity/events" (:method :get
                                                :content-type "text/html")
  "Paginated activity events as HTML fragments for infinite scroll.
   Supports category and depot filters via query params.
   Returns event rows grouped by day with headers, plus next sentinel when more exist."
  (let* ((offset (parse-integer (or (hunchentoot:parameter "offset") "0") :junk-allowed t))
         (limit (parse-integer (or (hunchentoot:parameter "limit") "50") :junk-allowed t))
         (category (hunchentoot:parameter "category"))
         (depot (hunchentoot:parameter "depot"))
         (last-date (or (hunchentoot:parameter "last_date") ""))
         (offset (or offset 0))
         (limit (or limit 50))
         (all-events (filter-events-by-depot (get-all-events) depot))
         (day-counts (count-events-per-day all-events))
         (items (get-events-range all-events offset limit category))
         (next-offset (+ offset (length items))))
    (if (null items)
        ""
        (let* ((last-ev-date (format-date (getf (car (last items)) :timestamp)))
               (body (render-paginated-events items day-counts last-date)))
          (if (= (length items) limit)
              (concatenate 'string body
                (render-load-more-sentinel next-offset limit category depot last-ev-date))
              body)))))

;;; ============================================================
;;; HTMX FRAGMENT ROUTES
;;; ============================================================

(lol-reactive:defroute "/api/task/expand" (:method :get
                                           :content-type "text/html")
  "Return HTML fragment for expanding a frontier card."
  (let ((task-id (hunchentoot:parameter "id")))
    (if (and task-id (> (length task-id) 0))
        (handler-case
          (render-task-expand-fragment task-id)
          (error (e)
            (setf (hunchentoot:return-code*) 500)
            (htm-str (:div :class "expand-error"
                       (cl-who:fmt "Error: ~A" (princ-to-string e))))))
        (progn
          (setf (hunchentoot:return-code*) 400)
          ""))))

;;; ============================================================
;;; HANDOFF EXPAND API
;;; ============================================================

(lol-reactive:defroute "/api/handoff/expand" (:method :get
                                               :content-type "text/html")
  "Return HTML fragment for expanding a handoff in task detail."
  (let ((task-id (hunchentoot:parameter "task_id"))
        (name (hunchentoot:parameter "name")))
    (if (and task-id name)
        (handler-case
          (render-handoff-expand-fragment task-id name)
          (error (e)
            (setf (hunchentoot:return-code*) 500)
            (htm-str (:div :class "expand-error"
                       (cl-who:fmt "Error: ~A" (princ-to-string e))))))
        (progn
          (setf (hunchentoot:return-code*) 400)
          ""))))

;;; ============================================================
;;; PIN API
;;; ============================================================

(lol-reactive:defroute "/api/pin" (:method :post)
  "Pin a task. Returns HTMX fragment."
  (let* ((task-id (hunchentoot:parameter "task_id"))
         (depot (let ((d (hunchentoot:parameter "depot")))
                  (when (and d (> (length d) 0)) d))))
    (if task-id
        (handler-case
          (progn
            (pin-task task-id depot)
            (render-pin-toggle task-id depot t))
          (error (e)
            (setf (hunchentoot:return-code*) 500)
            (format nil "Error: ~A" (princ-to-string e))))
        (progn
          (setf (hunchentoot:return-code*) 400)
          "Missing task_id"))))

(lol-reactive:defroute "/api/unpin" (:method :post)
  "Unpin a task. Returns HTMX fragment."
  (let* ((task-id (hunchentoot:parameter "task_id"))
         (depot (let ((d (hunchentoot:parameter "depot")))
                  (when (and d (> (length d) 0)) d))))
    (if task-id
        (handler-case
          (progn
            (unpin-task task-id depot)
            (render-pin-toggle task-id depot nil))
          (error (e)
            (setf (hunchentoot:return-code*) 500)
            (format nil "Error: ~A" (princ-to-string e))))
        (progn
          (setf (hunchentoot:return-code*) 400)
          "Missing task_id"))))

;;; ============================================================
;;; SCRATCHPAD API
;;; ============================================================

(lol-reactive:defroute "/api/scratchpad" (:method :get
                                          :content-type "text/plain")
  "Get scratchpad content for a task."
  (let ((task-id (let ((t-id (hunchentoot:parameter "task_id")))
                   (when (and t-id (> (length t-id) 0)) t-id))))
    (if task-id
        (load-scratchpad task-id)
        (progn (setf (hunchentoot:return-code*) 400) ""))))

(lol-reactive:defroute "/api/scratchpad" (:method :post)
  "Save scratchpad for a task. Body is raw text content."
  (let ((task-id (let ((t-id (hunchentoot:parameter "task_id")))
                   (when (and t-id (> (length t-id) 0)) t-id)))
        (content (let ((stream (hunchentoot:raw-post-data :force-text nil)))
                   (if (streamp stream)
                       (let ((buf (make-string 65536)))
                         (let ((n (read-sequence buf stream)))
                           (subseq buf 0 n)))
                       (or stream "")))))
    (if task-id
        (handler-case
          (progn
            (save-scratchpad content task-id)
            (htm-str (:span :class "scratchpad-status" "Saved")))
          (error ()
            (setf (hunchentoot:return-code*) 500)
            (htm-str (:span :class "scratchpad-status scratchpad-error" "Save failed"))))
        (progn
          (setf (hunchentoot:return-code*) 400)
          "Missing task_id"))))

;;; ============================================================
;;; TASK ACTION API
;;; ============================================================

(defun parse-json-body ()
  "Parse JSON POST body into an alist. Returns nil on failure.
   Uses lol-reactive:request-body which reads from the Clack env stream."
  (handler-case
    (let ((text (lol-reactive:request-body)))
      (when (and text (> (length text) 0))
        (yason:parse text :object-as :alist)))
    (error () nil)))

(lol-reactive:defroute "/api/task/complete" (:method :post
                                              :content-type "application/json")
  "Mark a task as completed via task-mcp RPC.
   Emits a :task.update-status event directly into the task's event log."
  (let* ((body (parse-json-body))
         (task-id (cdr (assoc "task_id" body :test #'string=))))
    (if (and task-id (> (length task-id) 0))
        (handler-case
          (progn
            (emit-task-event task-id :task.update-status
                             (list :status "completed"))
            (yason:with-output-to-string* ()
              (yason:encode-alist '(("ok" . t)))))
          (error (e)
            (lol-reactive:response 500
              :content-type "application/json"
              :body (yason:with-output-to-string* ()
                      (yason:encode-alist
                        `(("ok" . nil)
                          ("error" . ,(princ-to-string e))))))))
        (lol-reactive:response 400
          :content-type "application/json"
          :body (yason:with-output-to-string* ()
                  (yason:encode-alist '(("ok" . nil) ("error" . "Missing task_id"))))))))

(lol-reactive:defroute "/api/task/reopen" (:method :post
                                            :content-type "application/json")
  "Reopen a completed task.
   Emits a :task.update-status event with status active."
  (let* ((body (parse-json-body))
         (task-id (cdr (assoc "task_id" body :test #'string=))))
    (if (and task-id (> (length task-id) 0))
        (handler-case
          (progn
            (emit-task-event task-id :task.update-status
                             (list :status "active"))
            (yason:with-output-to-string* ()
              (yason:encode-alist '(("ok" . t)))))
          (error (e)
            (lol-reactive:response 500
              :content-type "application/json"
              :body (yason:with-output-to-string* ()
                      (yason:encode-alist
                        `(("ok" . nil)
                          ("error" . ,(princ-to-string e))))))))
        (lol-reactive:response 400
          :content-type "application/json"
          :body (yason:with-output-to-string* ()
                  (yason:encode-alist '(("ok" . nil) ("error" . "Missing task_id"))))))))

;;; ============================================================
;;; EDGE MUTATION API (link, sever, reclassify)
;;; ============================================================

(defun emit-edge-event (source-task-id event-type data)
  "Emit an edge mutation event to SOURCE-TASK-ID's event log.
   EVENT-TYPE is :task.link, :task.sever, or :task.reclassify.
   DATA is a plist with the event-specific fields."
  (emit-task-event source-task-id event-type data))

(lol-reactive:defroute "/api/task/link" (:method :post
                                          :content-type "application/json")
  "Create a typed edge from source task to target task via RPC."
  (let* ((body (parse-json-body))
         (task-id (cdr (assoc "task_id" body :test #'string=)))
         (target-id (cdr (assoc "target_id" body :test #'string=)))
         (edge-type (cdr (assoc "edge_type" body :test #'string=))))
    (if (and task-id target-id edge-type
             (> (length task-id) 0)
             (> (length target-id) 0)
             (> (length edge-type) 0))
        (handler-case
          (progn
            (emit-edge-event task-id :task.link
              (list :target-id target-id :edge-type edge-type))
            (yason:with-output-to-string* ()
              (yason:encode-alist '(("ok" . t)))))
          (error (e)
            (lol-reactive:response 500
              :content-type "application/json"
              :body (yason:with-output-to-string* ()
                      (yason:encode-alist
                        `(("ok" . nil)
                          ("error" . ,(princ-to-string e))))))))
        (lol-reactive:response 400
          :content-type "application/json"
          :body (yason:with-output-to-string* ()
                  (yason:encode-alist
                    '(("ok" . nil) ("error" . "Missing task_id, target_id, or edge_type"))))))))

(lol-reactive:defroute "/api/task/sever" (:method :post
                                           :content-type "application/json")
  "Remove a typed edge from source task to target task via RPC."
  (let* ((body (parse-json-body))
         (task-id (cdr (assoc "task_id" body :test #'string=)))
         (target-id (cdr (assoc "target_id" body :test #'string=)))
         (edge-type (cdr (assoc "edge_type" body :test #'string=))))
    (if (and task-id target-id edge-type
             (> (length task-id) 0)
             (> (length target-id) 0)
             (> (length edge-type) 0))
        (handler-case
          (progn
            (emit-edge-event task-id :task.sever
              (list :target-id target-id :edge-type edge-type))
            (yason:with-output-to-string* ()
              (yason:encode-alist '(("ok" . t)))))
          (error (e)
            (lol-reactive:response 500
              :content-type "application/json"
              :body (yason:with-output-to-string* ()
                      (yason:encode-alist
                        `(("ok" . nil)
                          ("error" . ,(princ-to-string e))))))))
        (lol-reactive:response 400
          :content-type "application/json"
          :body (yason:with-output-to-string* ()
                  (yason:encode-alist
                    '(("ok" . nil) ("error" . "Missing task_id, target_id, or edge_type"))))))))

(lol-reactive:defroute "/api/task/reclassify" (:method :post
                                                :content-type "application/json")
  "Change the type of an edge from source task to target task via RPC."
  (let* ((body (parse-json-body))
         (task-id (cdr (assoc "task_id" body :test #'string=)))
         (target-id (cdr (assoc "target_id" body :test #'string=)))
         (old-type (cdr (assoc "old_type" body :test #'string=)))
         (new-type (cdr (assoc "new_type" body :test #'string=))))
    (if (and task-id target-id old-type new-type
             (> (length task-id) 0)
             (> (length target-id) 0)
             (> (length old-type) 0)
             (> (length new-type) 0))
        (handler-case
          (progn
            (emit-edge-event task-id :task.reclassify
              (list :target-id target-id :old-type old-type :new-type new-type))
            (yason:with-output-to-string* ()
              (yason:encode-alist '(("ok" . t)))))
          (error (e)
            (lol-reactive:response 500
              :content-type "application/json"
              :body (yason:with-output-to-string* ()
                      (yason:encode-alist
                        `(("ok" . nil)
                          ("error" . ,(princ-to-string e))))))))
        (lol-reactive:response 400
          :content-type "application/json"
          :body (yason:with-output-to-string* ()
                  (yason:encode-alist
                    '(("ok" . nil)
                      ("error" . "Missing task_id, target_id, old_type, or new_type"))))))))

;;; ============================================================
;;; HEALTH SUGGESTION ACTIONS
;;; ============================================================

(lol-reactive:defroute "/api/health/dismiss-edge" (:method :post
                                                    :content-type "application/json")
  "Dismiss a suggested edge so it no longer appears in health diagnostics."
  (let* ((body (parse-json-body))
         (from (cdr (assoc "from" body :test #'string=)))
         (to (cdr (assoc "to" body :test #'string=))))
    (if (and from to (> (length from) 0) (> (length to) 0))
        (handler-case
          (progn
            (dismiss-edge-suggestion from to)
            (yason:with-output-to-string* ()
              (yason:encode-alist '(("ok" . t)))))
          (error (e)
            (lol-reactive:response 500
              :content-type "application/json"
              :body (yason:with-output-to-string* ()
                      (yason:encode-alist
                        `(("ok" . nil)
                          ("error" . ,(princ-to-string e))))))))
        (lol-reactive:response 400
          :content-type "application/json"
          :body (yason:with-output-to-string* ()
                  (yason:encode-alist
                    '(("ok" . nil) ("error" . "Missing from or to"))))))))
