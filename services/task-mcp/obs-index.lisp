;;; Task MCP Server - Observation Index
;;; KNN graph + metadata store for semantic observation retrieval

(in-package #:task-mcp)

;;; Observation record (external index pattern — G-Set stays as bare strings)

(defstruct obs-record
  "Observation with metadata, keyed by text hash."
  (text "" :type string)
  (text-hash 0 :type fixnum)
  (task-id "" :type string)
  (session "" :type string)
  (timestamp 0 :type integer)
  (embedding nil :type (or null (simple-array double-float (*)))))

;;; KNN observation graph

(defstruct obs-graph
  "KNN observation graph with incremental updates."
  (records (make-hash-table :test 'eql))     ; text-hash -> obs-record
  (embeddings (make-hash-table :test 'eql))  ; text-hash -> embedding vector
  (forward (make-hash-table :test 'eql))     ; text-hash -> ((other-hash . sim) ...)
  (reverse-idx (make-hash-table :test 'eql)) ; text-hash -> ((other-hash . sim) ...)
  (k 10 :type fixnum)
  (lock (bt:make-lock "obs-graph") :type t))

(defvar *obs-graph* (make-obs-graph)
  "Global observation index.")

;;; Quality tracking (external hash-table per lisp-264491)

(defvar *obs-quality* (make-hash-table :test 'eql)
  "Observation quality scores, keyed by text-hash. Default 0.5.")

(defvar *obs-quality-lock* (bt:make-lock "obs-quality")
  "Lock for *obs-quality* hash table.")

(defun obs-quality (text-hash)
  "Get quality score for observation. Default 0.5."
  (bt:with-lock-held (*obs-quality-lock*)
    (gethash text-hash *obs-quality* 0.5d0)))

(defun (setf obs-quality) (value text-hash)
  "Set quality score for observation."
  (bt:with-lock-held (*obs-quality-lock*)
    (setf (gethash text-hash *obs-quality*) value)))

;;; Graph operations

(defun obs-k (graph)
  "Adaptive k: max(10, ceil(log(n)))."
  (let ((n (hash-table-count (obs-graph-records graph))))
    (max (obs-graph-k graph) (ceiling (log (max 2 n))))))

(defun obs-graph-add (graph record)
  "Add observation record to KNN graph. O(n*d) per insertion.
   Thread-safe. Returns text-hash."
  (bt:with-lock-held ((obs-graph-lock graph))
    (let ((k (obs-k graph))
          (hash (obs-record-text-hash record))
          (emb (obs-record-embedding record)))
      ;; Store record and embedding
      (setf (gethash hash (obs-graph-records graph)) record)
      (when emb
        (setf (gethash hash (obs-graph-embeddings graph)) emb)
        ;; Compute similarities to all existing embeddings
        (let ((sims nil))
          (maphash (lambda (other-hash other-emb)
                     (unless (= other-hash hash)
                       (push (cons other-hash (vec-dot emb other-emb)) sims)))
                   (obs-graph-embeddings graph))
          ;; Set forward edges (top-k nearest)
          (let ((top-k (subseq (sort sims #'> :key #'cdr)
                               0 (min k (length sims)))))
            (setf (gethash hash (obs-graph-forward graph)) top-k)
            ;; Update reverse index
            (dolist (pair top-k)
              (push (cons hash (cdr pair))
                    (gethash (car pair) (obs-graph-reverse-idx graph)))))))
      hash)))

(defun obs-graph-query (graph query-embedding &key (k 10))
  "Find k nearest observations to query embedding. O(n*d) brute-force.
   Returns list of (text-hash . similarity) pairs, sorted descending."
  (bt:with-lock-held ((obs-graph-lock graph))
    (let ((sims nil))
      (maphash (lambda (hash emb)
                 (push (cons hash (vec-dot query-embedding emb)) sims))
               (obs-graph-embeddings graph))
      (subseq (sort sims #'> :key #'cdr)
              0 (min k (length sims))))))

(defun obs-graph-search (graph query-text &key (k 10))
  "Semantic search: embed query, find k nearest observations.
   Returns list of (obs-record . similarity) pairs."
  (let ((query-emb (get-embedding query-text)))
    (when query-emb
      (mapcar (lambda (pair)
                (cons (gethash (car pair) (obs-graph-records graph))
                      (cdr pair)))
              (obs-graph-query graph query-emb :k k)))))

;;; Structural edges (same-task, same-session)

(defun obs-add-structural-edges (graph)
  "Add structural edges between observations sharing task or session.
   Same-task weight: 0.6, same-session weight: 0.5.
   Thread-safe."
  (bt:with-lock-held ((obs-graph-lock graph))
    (let ((by-task (make-hash-table :test 'equal))
          (by-session (make-hash-table :test 'equal)))
      ;; Group by task and session
      (maphash (lambda (hash record)
                 (push hash (gethash (obs-record-task-id record) by-task))
                 (push hash (gethash (obs-record-session record) by-session)))
               (obs-graph-records graph))
      ;; Add edges
      (let ((edges-added 0))
        (flet ((add-edges (groups weight)
                 (maphash (lambda (key members)
                            (declare (ignore key))
                            (loop for (a . rest) on members
                                  do (dolist (b rest)
                                       (unless (assoc b (gethash a (obs-graph-forward graph)))
                                         (push (cons b weight) (gethash a (obs-graph-forward graph)))
                                         (push (cons a weight) (gethash b (obs-graph-forward graph)))
                                         (incf edges-added)))))
                          groups)))
          (add-edges by-task 0.6d0)
          (add-edges by-session 0.5d0))
        edges-added))))

;;; Cluster detection

(defun obs-find-clusters (graph &key (threshold 0.55))
  "Connected components on symmetrized KNN graph above threshold.
   Returns list of (cluster-id . (text-hash ...)) sorted by size descending."
  (bt:with-lock-held ((obs-graph-lock graph))
    (let ((visited (make-hash-table :test 'eql))
          (components nil)
          (comp-id 0))
      (maphash (lambda (hash _)
                 (declare (ignore _))
                 (unless (gethash hash visited)
                   (let ((queue (list hash))
                         (component nil))
                     (loop while queue
                           for current = (pop queue)
                           unless (gethash current visited)
                           do (setf (gethash current visited) t)
                              (push current component)
                              ;; Traverse forward edges above threshold
                              (dolist (pair (gethash current (obs-graph-forward graph)))
                                (when (and (> (cdr pair) threshold)
                                           (not (gethash (car pair) visited)))
                                  (push (car pair) queue)))
                              ;; Traverse reverse edges above threshold
                              (dolist (pair (gethash current (obs-graph-reverse-idx graph)))
                                (when (and (> (cdr pair) threshold)
                                           (not (gethash (car pair) visited)))
                                  (push (car pair) queue))))
                     (push (cons (incf comp-id) (nreverse component)) components))))
               (obs-graph-records graph))
      (sort components #'> :key (lambda (c) (length (cdr c)))))))

;;; Auto-indexing from event log

(defun index-observation-event (event task-id)
  "Index an observation from a task event. Embeds and adds to *obs-graph*.
   Fails gracefully if Ollama is down (skips embedding)."
  (let* ((text (getf (task:event-data event) :text))
         (hash (sxhash text))
         (existing (gethash hash (obs-graph-records *obs-graph*))))
    ;; Skip if already indexed
    (when existing
      (return-from index-observation-event nil))
    (let* ((emb (get-embedding text))
           (record (make-obs-record
                    :text text
                    :text-hash hash
                    :task-id task-id
                    :session (task:event-session event)
                    :timestamp (task:event-timestamp event)
                    :embedding emb)))
      (obs-graph-add *obs-graph* record)
      hash)))

(defun index-task-observations (task-id)
  "Index all observations from a task's event log. Idempotent.
   Uses ollama-embed-batch for efficiency (single API call for all texts).
   Returns count of newly indexed observations."
  (let* ((path (task:task-events-path task-id))
         (log (task:elog-load path))
         (events (reverse (task:event-log-events log)))
         (pending nil))  ; (event . text) pairs needing indexing
    ;; Collect un-indexed observation events
    (dolist (ev events)
      (when (eq (task:event-type ev) :observation)
        (let* ((text (getf (task:event-data ev) :text))
               (hash (sxhash text)))
          (unless (gethash hash (obs-graph-records *obs-graph*))
            (push (cons ev text) pending)))))
    (when (null pending)
      (return-from index-task-observations 0))
    (setf pending (nreverse pending))
    ;; Batch embed all texts in one API call
    (let* ((texts (mapcar #'cdr pending))
           (embeddings (ollama-embed-batch texts))
           (count 0))
      ;; Populate embedding cache with batch results
      (when embeddings
        (bt:with-lock-held (*embedding-cache-lock*)
          (loop for text in texts
                for emb in embeddings
                when emb
                do (setf (gethash text *embedding-cache*) emb)
                   (incf *embedding-cache-dirty*)))
        ;; Persist to disk after batch indexing
        (maybe-save-embedding-cache))
      ;; Create records and add to graph
      (loop for (ev . text) in pending
            for emb in (or embeddings (make-list (length pending)))
            do (let ((record (make-obs-record
                              :text text
                              :text-hash (sxhash text)
                              :task-id task-id
                              :session (task:event-session ev)
                              :timestamp (task:event-timestamp ev)
                              :embedding emb)))
                 (obs-graph-add *obs-graph* record)
                 (incf count)))
      count)))

(defun obs-graph-stats ()
  "Return stats about the observation graph."
  (bt:with-lock-held ((obs-graph-lock *obs-graph*))
    (let ((n-records (hash-table-count (obs-graph-records *obs-graph*)))
          (n-embedded (hash-table-count (obs-graph-embeddings *obs-graph*)))
          (n-edges 0))
      (maphash (lambda (k v) (declare (ignore k)) (incf n-edges (length v)))
               (obs-graph-forward *obs-graph*))
      (list :records n-records
            :embedded n-embedded
            :edges n-edges
            :k (obs-k *obs-graph*)))))
