(in-package #:task-mcp)

;;; Task Graph — MCP tool layer over core.lib.task graph primitives.
;;;
;;; All graph structure, queries, formatting, and enrichment live in
;;; the task package (core.lib.task). This file provides:
;;;   - get-or-build-graph: cached graph construction (with metadata edges)
;;;   - task_graph MCP tool (8 query modes)
;;;
;;; Removed modes (zero callers, use TQ instead):
;;;   neighbors → (-> (node "id") :edges)
;;;   ego       → (-> (node "id") (:follow :phase-of) ...)
;;;   upstream  → (-> (node "id") (:back :depends-on) ...)
;;;   downstream → (-> (node "id") (:follow :depends-on) ...)

;;; ============================================================
;;; GRAPH CONSTRUCTION
;;; ============================================================

(defun get-or-build-graph ()
  "Build the full task graph from all configured depots.
   Returns cached multi-depot graph with qualified IDs (depot:task-id).
   State edges (from OR-Set) are always included."
  (task:get-cached-multi-depot-graph))

;;; ============================================================
;;; task_graph MCP TOOL
;;; 8 query modes: stats, frontier, temporal, knowledge,
;;; plan, plan-frontier, plan-markdown, plan-json
;;; ============================================================

(defvar *ts-edges-cache* nil "Cached temporal edges (expensive to compute).")
(defvar *ts-edges-cache-time* 0 "Universal time of last ts-edges computation.")
(defparameter *ts-edges-cache-ttl* 300 "TTL in seconds for temporal edge cache (5 min).")

(defun get-cached-ts-edges ()
  "Get temporal edges with caching. Recomputes if cache is stale (> 5 min)."
  (let ((now (get-universal-time)))
    (when (or (null *ts-edges-cache*)
              (> (- now *ts-edges-cache-time*) *ts-edges-cache-ttl*))
      (let ((task-infos (task:get-cached-task-infos)))
        (setf *ts-edges-cache*
              (loop for depot being the hash-keys of task:*depot-tasks-roots*
                    using (hash-value root)
                    append (let ((depot-infos
                                   (mapcar (lambda (info)
                                             (list* :id (or (getf info :bare-id)
                                                             (getf info :id))
                                                    (alexandria:remove-from-plist info :id :bare-id)))
                                           (remove-if-not
                                            (lambda (info) (string= (getf info :depot) depot))
                                            task-infos))))
                             (task:extract-timestamped-reference-edges
                              depot-infos (namestring root))))
              *ts-edges-cache-time* now))))
  *ts-edges-cache*)

(define-session-tool task_graph
    ((query string "Query type: stats, frontier, temporal, knowledge, plan, plan-frontier, plan-markdown, plan-json" "stats")
     (task_id string "Task ID (default: current task)" nil))
  "Query the task relationship graph.
Supports: stats (global summary), frontier (ready tasks),
temporal (timestamped references), knowledge (transitive reachability),
plan (phase DAG with status), plan-frontier (ready phases in plan),
plan-markdown (generated plan.md), plan-json (plan as JSON for dashboard)."
  (let* ((graph (get-or-build-graph))
         (id (or (when (and task_id (> (length task_id) 0)) task_id)
                 *current-task-id*)))
    (make-text-content "~A"
     (cond
       ((string= query "stats")
        (task:format-graph-stats graph))

       ((string= query "frontier")
        (let ((frontier (task:compute-frontier graph)))
          (with-output-to-string (s)
            (format s "Frontier (~D tasks ready):~%~%" (length frontier))
            (dolist (fid (subseq frontier 0 (min 20 (length frontier))))
              (format s "  ~A~%" fid))
            (when (> (length frontier) 20)
              (format s "  ... and ~D more~%" (- (length frontier) 20))))))

       ((string= query "temporal")
        (let ((ts-edges (get-cached-ts-edges)))
          (with-output-to-string (s)
            (format s "Temporal references for ~A:~%~%" (or id "all"))
            (let ((relevant (if id
                                (remove-if-not
                                 (lambda (e) (or (equal (getf e :from) id)
                                                 (equal (getf e :to) id)))
                                 ts-edges)
                                (last ts-edges 20))))
              (format s "~D references:~%" (length relevant))
              (dolist (e relevant)
                (format s "  [~A] ~A -> ~A~%"
                        (getf e :time) (getf e :from) (getf e :to)))))))

       ((string= query "knowledge")
        (let* ((ts-edges (get-cached-ts-edges))
               (reachable (task:reachable-at-time ts-edges (or id "")
                                                   "2026-12-31_23-59-59")))
          (with-output-to-string (s)
            (format s "Knowledge graph from ~A:~%~%" (or id "(none)"))
            (format s "Transitively reachable: ~D tasks~%" (length reachable))
            (dolist (r reachable)
              (format s "  ~A~%" r)))))

       ((string= query "plan")
        (task:format-plan (or id "")))

       ((string= query "plan-frontier")
        (let ((frontier (task:plan-frontier (or id ""))))
          (with-output-to-string (s)
            (if frontier
                (let ((scored nil))
                  ;; Compute scores for each frontier phase
                  (dolist (fid frontier)
                    (handler-case
                        (let* ((elog (task:elog-load (task:task-events-path fid)))
                               (events (task:event-log-events elog))
                               (state (task:compute-state events))
                               (obs-count (crdt:gs-count (task:task-state-observations state)))
                               (sess-count (crdt:gs-count (task:task-state-sessions state)))
                               (claim (crdt:lww-value (task:task-state-claim state)))
                               (affinity (if (> (length events) 0)
                                             (handler-case (task:affinity-score events state)
                                               (error () 0.0))
                                             0.0)))
                          (push (list :id fid :obs obs-count :sessions sess-count
                                      :claim claim :affinity affinity)
                                scored))
                      (error () (push (list :id fid :obs 0 :sessions 0
                                            :claim nil :affinity 0.0)
                                      scored))))
                  ;; Sort by affinity descending
                  (setf scored (sort scored #'> :key (lambda (s) (getf s :affinity))))
                  (format s "Plan frontier for ~A (~D phases ready):~%~%"
                          (or id "?") (length scored))
                  (loop for entry in scored
                        for first = t then nil
                        do (format s "  ~A~A  [~D obs, ~D sessions~A]~%"
                                   (if first "(recommended next) " "")
                                   (getf entry :id) (getf entry :obs) (getf entry :sessions)
                                   (if (getf entry :claim)
                                       (format nil ", claimed: ~A" (getf entry :claim))
                                       ""))))
                (let ((children (task:task-children (or id ""))))
                  (if children
                      (format s "No ready phases — all ~D phases completed or blocked.~%"
                              (length children))
                      (format s "Task ~A has no plan (no children spawned).~%"
                              (or id "?"))))))))

       ((string= query "plan-markdown")
        (or (task:generate-plan-markdown (or id ""))
            (format nil "Task ~A has no plan (no children spawned).~%" (or id "?"))))

       ((string= query "plan-json")
        (task:plan-to-json (or id "")))

       (t
        (format nil "Unknown: ~A~%Valid: stats, frontier, temporal, knowledge, plan, plan-frontier, plan-markdown, plan-json"
                query))))))

(define-tool task_health ()
  "Task health report: stale forks, dead ends, unlinked roots."
  (make-text-content "~A" (task:task-health-report)))

;;; task_enriched_query removed — use TQ: (-> (node "id") :enrich)
;;; or call task:compute-enriched-query directly from bootstrap-task.
