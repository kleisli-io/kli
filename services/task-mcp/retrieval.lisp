;;; Task MCP Server - Retrieval Pipeline
;;; Spreading activation, text search, RRF fusion, enriched context, quality scoring

(in-package #:task-mcp)

;;; Spreading activation

(defun obs-spread (graph seed-weights &key (decay 0.7) (iterations 3)
                                           (incoming-factor 0.5))
  "Spreading activation on observation KNN graph.
   SEED-WEIGHTS: alist of (text-hash . weight).
   Returns alist of (text-hash . activation) sorted descending."
  (bt:with-lock-held ((obs-graph-lock graph))
    (let ((activation (make-hash-table :test 'eql)))
      (dolist (pair seed-weights)
        (setf (gethash (car pair) activation)
              (coerce (cdr pair) 'double-float)))
      (dotimes (i iterations)
        (let ((new-act (make-hash-table :test 'eql)))
          (maphash (lambda (k v) (setf (gethash k new-act) v)) activation)
          (maphash (lambda (node act)
                     (dolist (pair (gethash node (obs-graph-forward graph)))
                       (incf (gethash (car pair) new-act 0.0d0)
                             (* act decay (cdr pair))))
                     (dolist (pair (gethash node (obs-graph-reverse-idx graph)))
                       (incf (gethash (car pair) new-act 0.0d0)
                             (* act decay incoming-factor (cdr pair)))))
                   activation)
          (maphash (lambda (k v)
                     (when (< v 0.0d0) (setf (gethash k new-act) 0.0d0)))
                   new-act)
          (setf activation new-act)))
      (let (result)
        (maphash (lambda (k v) (push (cons k v) result)) activation)
        (sort result #'> :key #'cdr)))))

;;; Text search

(defun obs-text-search (graph query &key (limit 20))
  "Case-insensitive substring search over observation texts.
   Score based on fraction of query terms matched."
  (let ((terms (cl-ppcre:split "\\s+" (string-downcase query)))
        (results nil))
    (bt:with-lock-held ((obs-graph-lock graph))
      (maphash (lambda (hash record)
                 (let* ((text-down (string-downcase (obs-record-text record)))
                        (matches (count-if (lambda (term)
                                             (search term text-down))
                                           terms)))
                   (when (plusp matches)
                     (push (cons hash (/ (coerce matches 'double-float)
                                         (length terms)))
                           results))))
               (obs-graph-records graph)))
    (let ((sorted (sort results #'> :key #'cdr)))
      (subseq sorted 0 (min limit (length sorted))))))

;;; Reciprocal Rank Fusion

(defun rrf-fuse (ranked-lists &key (k 60))
  "Reciprocal Rank Fusion across multiple ranked lists.
   Each list is an alist of (key . score), sorted by score descending."
  (let ((scores (make-hash-table :test 'eql)))
    (dolist (ranked ranked-lists)
      (loop for (key . _score) in ranked
            for rank from 1
            do (incf (gethash key scores 0.0d0)
                     (/ 1.0d0 (+ k rank)))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) scores)
      (sort result #'> :key #'cdr))))

;;; Full retrieval pipeline

(defun obs-retrieve (graph query &key (k 10) (spread-iterations 3))
  "Full retrieval: KNN + text search + spreading activation, fused via RRF.
   Returns list of (obs-record . rrf-score), limited to k results."
  (let* ((query-emb (get-embedding query))
         (knn-results (when query-emb
                        (obs-graph-query graph query-emb :k (* 2 k))))
         (text-results (obs-text-search graph query :limit (* 2 k)))
         (spread-results (when knn-results
                           (obs-spread graph
                                       (subseq knn-results
                                               0 (min 5 (length knn-results)))
                                       :iterations spread-iterations)))
         (fused (rrf-fuse (remove nil (list knn-results text-results spread-results))))
         (top-k (subseq fused 0 (min k (length fused)))))
    (bt:with-lock-held ((obs-graph-lock graph))
      (mapcar (lambda (pair)
                (cons (gethash (car pair) (obs-graph-records graph))
                      (cdr pair)))
              top-k))))

;;; Enriched context (task-graph-aware seeding)

(defun task-graph-neighbor-ids (task-graph task-id)
  "Get neighbor task IDs with edge types from task graph.
   Returns list of (neighbor-id relation-keyword) from both directions."
  (let ((result nil))
    (dolist (edge (gethash task-id (task:task-graph-forward task-graph)))
      (push (list (first edge) (second edge)) result))
    (dolist (edge (gethash task-id (task:task-graph-reverse task-graph)))
      (push (list (first edge) (second edge)) result))
    result))

(defun compute-obs-seeds (task-id graph)
  "Compute observation seed weights from task graph context.
   Own-task: 1.0, structural neighbor: 0.7, related: 0.5, other: 0.3."
  (let ((seeds (make-hash-table :test 'eql))
        (task-graph (or graph (get-or-build-graph))))
    (bt:with-lock-held ((obs-graph-lock *obs-graph*))
      (maphash (lambda (hash record)
                 (when (string= (obs-record-task-id record) task-id)
                   (setf (gethash hash seeds) 1.0d0)))
               (obs-graph-records *obs-graph*)))
    (dolist (neighbor (task-graph-neighbor-ids task-graph task-id))
      (let ((nid (first neighbor))
            (etype (second neighbor)))
        (let ((weight (cond
                        ((member etype '(:phase-of :forked-from)) 0.7d0)
                        ((eq etype :related-to) 0.5d0)
                        (t 0.3d0))))
          (bt:with-lock-held ((obs-graph-lock *obs-graph*))
            (maphash (lambda (hash record)
                       (when (string= (obs-record-task-id record) nid)
                         (setf (gethash hash seeds)
                               (max weight (gethash hash seeds 0.0d0)))))
                     (obs-graph-records *obs-graph*))))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) seeds)
      result)))

(defun adaptive-alpha (pattern-scores obs-scores)
  "Adaptive mixing weight alpha for pattern vs observation results.
   Alpha in [0.3, 0.8]: favors patterns when they match strongly."
  (let ((p-max (if pattern-scores
                   (reduce #'max pattern-scores :key #'cdr :initial-value 0.0d0)
                   0.0d0))
        (o-max (if obs-scores
                   (reduce #'max obs-scores :key #'cdr :initial-value 0.0d0)
                   0.0d0)))
    (cond
      ((zerop o-max) 0.8d0)
      ((zerop p-max) 0.3d0)
      (t (+ 0.3d0 (* 0.5d0 (/ p-max (+ p-max o-max))))))))

(defun enriched-retrieve (task-id &key (k 10) (graph nil))
  "Task-graph-aware retrieval: enriched query + observation seeds + RRF.
   Returns (values obs-results enriched-query alpha)."
  (let* ((task-graph (or graph (get-or-build-graph)))
         (enriched-query (task:compute-enriched-query task-id task-graph))
         (neighbor-ids (mapcar #'first (task-graph-neighbor-ids task-graph task-id))))
    ;; Index current + neighbor tasks
    (index-task-observations task-id)
    (dolist (nid neighbor-ids)
      (ignore-errors (index-task-observations nid)))
    (let* ((seeds (compute-obs-seeds task-id task-graph))
           (obs-results (when enriched-query
                          (obs-retrieve *obs-graph* enriched-query :k k)))
           (spread-from-seeds (when seeds
                                (obs-spread *obs-graph* seeds :iterations 2)))
           (query-ranked (mapcar (lambda (pair)
                                   (cons (sxhash (obs-record-text (car pair)))
                                         (cdr pair)))
                                 obs-results))
           (fused (rrf-fuse (remove nil (list query-ranked spread-from-seeds))))
           (top-k (subseq fused 0 (min k (length fused))))
           (results (bt:with-lock-held ((obs-graph-lock *obs-graph*))
                      (mapcar (lambda (pair)
                                (cons (gethash (car pair)
                                               (obs-graph-records *obs-graph*))
                                      (cdr pair)))
                              top-k))))
      (values results enriched-query (adaptive-alpha nil results)))))

;;; Quality scoring

(defun quality-update (current outcome &key (alpha 0.3))
  "Bayesian quality update. OUTCOME: :success or :failure.
   Converges to true success rate in ~7 updates."
  (ecase outcome
    (:success (+ current (* alpha (- 1.0d0 current))))
    (:failure (- current (* alpha current)))))

(defun obs-temporal-decay (age-seconds quality &key (half-life (* 7 86400))
                                                     (min-factor 0.1d0))
  "Temporal decay modulated by quality. 7-day half-life, 0.1 floor."
  (+ min-factor
     (* (- 1.0d0 min-factor) quality
        (exp (- (/ (coerce age-seconds 'double-float) half-life))))))

(defun promotion-score (cluster-size mean-quality n-tasks age-days)
  "Geometric mean promotion score. Single-task clusters always score 0."
  (let ((size-f (min 1.0d0 (/ (max 0 (- cluster-size 1)) 4.0d0)))
        (quality-f (max 0.0d0 (min 1.0d0 (/ (- mean-quality 0.3d0) 0.5d0))))
        (diversity-f (min 1.0d0 (/ (max 0 (- n-tasks 1)) 2.0d0)))
        (age-f (min 1.0d0 (/ (coerce age-days 'double-float) 5.0d0))))
    (expt (* size-f quality-f diversity-f age-f) 0.25d0)))

;;; Formatting

(defun format-obs-results (results)
  "Format retrieval results for MCP tool response.
   Ranked list without scores — agents don't need retrieval internals."
  (with-output-to-string (s)
    (if (null results)
        (format s "No matching observations found.")
        (loop for (record . score) in results
              for i from 1
              when record
              do (format s "~D. (~A, ~A)~%   ~A~%~%"
                         i
                         (obs-record-task-id record)
                         (obs-record-session record)
                         (obs-record-text record))))))
