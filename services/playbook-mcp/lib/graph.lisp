;;; Playbook MCP Server - Graph Rebuild Pipeline
;;; Builds complete graph from manual edges, co-application, and embedding proximity.
;;; Atomic swap ensures readers always see a consistent graph.
;;;
;;; ARCHITECTURE: Three-Source Edge Merge
;;;
;;; The graph is built from three independent sources during rebuild-graph:
;;;
;;; 1. Manual Edges (git-tracked curation)
;;;    Source: ace/playbook-manual-edges.json
;;;    Relations: :requires, :supersedes, :conflicts
;;;    Written by: playbook_link MCP tool (filters out computed edges before save)
;;;    Persistence: Git-tracked, survives cache deletion
;;;
;;; 2. Co-Application Ledger (git-tracked usage data)
;;;    Source: ace/playbook-co-applications.json
;;;    Relations: :co-applied (bidirectional)
;;;    Written by: SessionEnd hook (increments pair counts)
;;;    Transform: Raw counts → weighted edges via co-application-weight function
;;;    Persistence: Git-tracked, accumulates over time
;;;
;;; 3. Proximity Edges (computed from embeddings)
;;;    Source: .playbook-cache/embeddings.json (ephemeral)
;;;    Relations: :similar
;;;    Computed by: rebuild-graph via compute-proximity-edges
;;;    Transform: Pairwise cosine similarity → weighted edges
;;;    Persistence: Never persisted, recomputed fresh on every rebuild
;;;
;;; No edges cache file needed. All edges are either:
;;; - Curated (manual edges file)
;;; - Derived from ledger (co-app counts → edges)
;;; - Computed from embeddings (proximity on demand)
;;;
;;; Rebuild triggers: Manual edge added, pattern evolved, feedback given (stale flag)

(in-package #:playbook-mcp)

;;; Graph state

(defvar *current-graph* (make-hash-table :test 'equal)
  "The live graph edge store (adjacency list). Swapped atomically during rebuild.")

(defvar *incoming-edges* (make-hash-table :test 'equal)
  "Reverse index: node-id → list of incoming edges.
   Built by build-incoming-index, used for bidirectional spreading.")

(defparameter *incoming-decay-factor* 0.5
  "Extra decay for incoming edges (backward spreading is weaker than forward).")

(defvar *graph-stale* nil
  "When T, graph needs rebuild before next activation query.")

(defvar *last-rebuild-time* nil
  "Universal time of last successful graph rebuild.")

(defvar *manual-edges-path* nil
  "Path to git-tracked manual edges file (ace/playbook-manual-edges.json).
   Contains only :requires, :supersedes, :conflicts relations.
   Set via PLAYBOOK_EDGES_PATH env var or defaults to ace/playbook-manual-edges.json.")

;;; Co-application ledger

(defvar *co-app-ledger* (make-hash-table :test 'equal)
  "Co-application ledger: canonical-pair-key → count.")

(defvar *co-app-ledger-lock* (make-lock "co-app-ledger")
  "Lock for thread-safe co-app ledger access.")

(defun co-app-key (id-a id-b)
  "Canonical sorted key for a co-application pair."
  (if (string< id-a id-b)
      (format nil "~A:~A" id-a id-b)
      (format nil "~A:~A" id-b id-a)))

(defun parse-co-app-key (key)
  "Split a co-app key back to a cons pair of (id-a . id-b)."
  (let ((pos (position #\: key)))
    (cons (subseq key 0 pos)
          (subseq key (1+ pos)))))

;;; Co-application ledger I/O

(defun load-co-app-ledger (path)
  "Load co-application ledger from JSON file into *co-app-ledger*.
   File format: {\"id-a:id-b\": count, ...}. No-op if file missing."
  (when (probe-file path)
    (let ((ht (yason:parse (alexandria:read-file-into-string path))))
      (with-lock-held (*co-app-ledger-lock*)
        (clrhash *co-app-ledger*)
        (maphash (lambda (k v)
                   (setf (gethash k *co-app-ledger*) v))
                 ht)))))

(defun merge-co-app-ledger (path)
  "Additive merge of co-app ledger from PATH into *co-app-ledger*.
   Unlike load-co-app-ledger, does NOT clrhash — safe to call for multiple depots."
  (handler-case
      (when (probe-file path)
        (let ((parsed (yason:parse (alexandria:read-file-into-string path))))
          (with-lock-held (*co-app-ledger-lock*)
            (maphash (lambda (k v) (incf (gethash k *co-app-ledger* 0) v)) parsed))))
    (error (e) (format *error-output* "Warning: merge-co-app-ledger ~A: ~A~%" path e))))

(defun save-co-app-ledger (path)
  "Save *co-app-ledger* to JSON file (atomic write)."
  (let* ((abs-path (namestring (merge-pathnames path)))
         (temp-path (format nil "~A.tmp" abs-path)))
    (with-lock-held (*co-app-ledger-lock*)
      (with-open-file (s temp-path :direction :output :if-exists :supersede)
        (yason:encode *co-app-ledger* s)))
    (rename-file temp-path abs-path)))

;;; Co-application pair generation

(defun generate-co-app-pairs (pattern-ids)
  "Generate all unique pairs from a list of pattern IDs.
   Returns list of (id-a . id-b) cons pairs with canonical ordering."
  (let (pairs)
    (loop for (a . rest) on pattern-ids
          do (loop for b in rest
                   do (push (cons (if (string< a b) a b)
                                  (if (string< a b) b a))
                            pairs)))
    pairs))

(defun update-co-app-ledger-from-session (pattern-ids)
  "Increment co-app ledger counts for all pairs of activated patterns.
   Returns the number of pairs updated."
  (let ((pairs (generate-co-app-pairs pattern-ids)))
    (with-lock-held (*co-app-ledger-lock*)
      (dolist (pair pairs)
        (let ((key (co-app-key (car pair) (cdr pair))))
          (incf (gethash key *co-app-ledger* 0)))))
    (length pairs)))

(defun atomic-update-co-app-ledger (ledger-path pattern-ids)
  "Atomically update co-app ledger with file-level locking.

   1. Acquire exclusive file lock
   2. Load current state fresh from disk (ignores in-memory state)
   3. Generate pairs and increment counts
   4. Write back atomically (temp + rename)
   5. Release lock

   Returns the number of pairs updated."
  (when (< (length pattern-ids) 2)
    (return-from atomic-update-co-app-ledger 0))

  (with-file-lock (ledger-path :wait t)
    ;; Load fresh from disk within lock
    (let ((ledger (if (probe-file ledger-path)
                      (yason:parse (alexandria:read-file-into-string ledger-path))
                      (make-hash-table :test 'equal)))
          (pairs (generate-co-app-pairs pattern-ids)))
      ;; Update counts
      (dolist (pair pairs)
        (let ((key (co-app-key (car pair) (cdr pair))))
          (incf (gethash key ledger 0))))
      ;; Atomic write via temp + rename
      (let ((temp-path (format nil "~A.tmp" (namestring ledger-path))))
        (with-open-file (s temp-path :direction :output :if-exists :supersede)
          (yason:encode ledger s))
        (rename-file temp-path ledger-path))
      (length pairs))))

(defun prune-co-app-ledger (ledger-path valid-pattern-ids)
  "Remove co-app pairs containing patterns not in VALID-PATTERN-IDS.
   Uses file-level locking for safe concurrent access.
   Returns count of removed pairs."
  (unless (probe-file ledger-path)
    (return-from prune-co-app-ledger 0))

  (with-file-lock (ledger-path :wait t)
    (let ((ledger (yason:parse (alexandria:read-file-into-string ledger-path)))
          (valid-set (make-hash-table :test 'equal))
          (to-remove nil))
      ;; Build valid set
      (dolist (id valid-pattern-ids)
        (setf (gethash id valid-set) t))
      ;; Find orphaned pairs
      (maphash (lambda (key count)
                 (declare (ignore count))
                 (let ((pair (parse-co-app-key key)))
                   (unless (and (gethash (car pair) valid-set)
                                (gethash (cdr pair) valid-set))
                     (push key to-remove))))
               ledger)
      ;; Remove them
      (dolist (key to-remove)
        (remhash key ledger))
      ;; Write back if changed
      (when to-remove
        (let ((temp-path (format nil "~A.tmp" (namestring ledger-path))))
          (with-open-file (s temp-path :direction :output :if-exists :supersede)
            (yason:encode ledger s))
          (rename-file temp-path ledger-path)))
      (length to-remove))))

;;; Co-application edges from ledger

(defun ledger-to-co-app-edges ()
  "Convert co-app ledger counts to weighted bidirectional edges.
   Only pairs with count >= 2 produce edges."
  (let (edges)
    (with-lock-held (*co-app-ledger-lock*)
      (maphash (lambda (key count)
                 (when (>= count 2)
                   (let* ((pair (parse-co-app-key key))
                          (w (co-application-weight count)))
                     (push (make-edge :source (car pair) :target (cdr pair)
                                      :relation :co-applied :weight w
                                      :evidence (format nil "co-applied ~D times" count))
                           edges)
                     (push (make-edge :source (cdr pair) :target (car pair)
                                      :relation :co-applied :weight w
                                      :evidence (format nil "co-applied ~D times" count))
                           edges))))
               *co-app-ledger*))
    edges))

;;; Proximity edges from embeddings

(defun compute-proximity-edges (patterns &key (threshold 0.5))
  "Compute embedding proximity edges between all pattern pairs.
   Returns bidirectional edges for pairs with cosine similarity above THRESHOLD."
  (let ((edges nil)
        (with-embeddings (remove-if-not #'pattern-embedding patterns)))
    (loop for (p1 . rest) on with-embeddings
          do (loop for p2 in rest
                   for sim = (cosine-similarity (pattern-embedding p1)
                                                (pattern-embedding p2))
                   when (> sim threshold)
                   do (let ((w (embedding-proximity-weight sim)))
                        (push (make-edge :source (pattern-id p1)
                                         :target (pattern-id p2)
                                         :relation :similar
                                         :weight w
                                         :evidence (format nil "cosine=~,2F" sim))
                              edges)
                        (push (make-edge :source (pattern-id p2)
                                         :target (pattern-id p1)
                                         :relation :similar
                                         :weight w
                                         :evidence (format nil "cosine=~,2F" sim))
                              edges))))
    edges))

;;; Graph rebuild

(defun build-edge-store (edge-list)
  "Build an adjacency-list hash-table from a flat list of edges."
  (let ((store (make-hash-table :test 'equal)))
    (dolist (e edge-list)
      (push e (gethash (edge-source e) store)))
    store))

(defun rebuild-graph (&key manual-edges-path)
  "Rebuild the graph from all edge sources. Atomic swap.
   1. Load manual edges from file (if path provided and file exists)
   2. Include in-memory edges from *edge-store* (from playbook_link)
   3. Convert co-app ledger to weighted edges
   4. Build new store and atomic swap
   Returns a status string."
  ;; Embeddings are computed lazily by ensure-pattern-embedding on first
  ;; semantic search query. No eager Ollama call needed here — proximity
  ;; edges are disabled, and the graph structure uses only co-app + manual edges.
  (let ((file-edges (when manual-edges-path
                      (load-edges-file manual-edges-path)))
        (co-app-edges (ledger-to-co-app-edges))
        (proximity-edges nil)
        (edge-count 0))
    ;; NOTE: We use file-edges, not *edge-store*, to avoid double-counting.
    ;; After rebuild, *edge-store* will contain ALL edges (manual+co-app+proximity).
    ;; Manual edges come from file (git-tracked), co-app from ledger, proximity computed fresh.
    ;; On server start, *edge-store* is populated from the manual edges file (server.lisp:54-57).
    (let* ((all-edges (append (or file-edges nil)
                              co-app-edges
                              proximity-edges))
           (new-store (build-edge-store all-edges)))
      ;; Atomic swap
      (setf *current-graph* new-store)
      ;; Build reverse index for bidirectional traversal
      (let ((incoming-count (build-incoming-index)))
        (setf *graph-stale* nil)
        (setf *last-rebuild-time* (get-universal-time))
        (maphash (lambda (k v) (declare (ignore k)) (incf edge-count (length v))) new-store)
        (format nil "Graph rebuilt: ~D nodes, ~D edges (~D file, ~D co-app, ~D proximity), ~D with incoming"
                (hash-table-count new-store) edge-count
                (length file-edges)
                (length co-app-edges) (length proximity-edges)
                incoming-count)))))

(defun ensure-graph-fresh (&key (manual-edges-path *manual-edges-path*))
  "If graph is stale, rebuild. Otherwise no-op. Returns T if rebuilt."
  (when *graph-stale*
    (rebuild-graph :manual-edges-path manual-edges-path)
    t))

;;; After-mutation events

(defun after-pattern-mutation (event-type pattern-id)
  "Single dispatch for graph/embedding updates after pattern mutations.
   EVENT-TYPE: :feedback, :evolve, :devolve, :add"
  (case event-type
    (:feedback
     ;; Weights may change from effectiveness shift → stale graph
     (setf *graph-stale* t))
    ((:evolve :devolve)
     ;; Content changed → invalidate embedding, mark graph stale
     (let ((pattern (get-pattern pattern-id)))
       (when pattern
         (setf (pattern-embedding pattern) nil)))
     (setf *graph-stale* t))
    (:add
     ;; New pattern → compute embedding if possible, mark stale
     (let ((pattern (get-pattern pattern-id)))
       (when pattern
         (ensure-pattern-embedding pattern)))
     (setf *graph-stale* t))))

;;; In-process co-application recording

(defun co-app-ledger-path ()
  "Return path to co-app ledger file in depot's metadata directory (.kli/ or ace/).
   Returns NIL if depot not detected."
  (let ((meta-path (detect-depot-meta-path)))
    (when meta-path
      (namestring (merge-pathnames "playbook-co-applications.json" meta-path)))))

(defun flush-co-app-ledger ()
  "Save in-memory co-app ledger to disk. No-op if path not detected."
  (let ((path (co-app-ledger-path)))
    (when path
      (save-co-app-ledger path))))

(defun record-activation-and-update-graph (session-id pattern-ids)
  "Record activated patterns and update co-app ledger in-process.
   Tracks per-session activated set, computes only NEW pairs
   (within-new + cross new×prior), updates in-memory ledger,
   flushes to disk, marks graph stale.
   Returns count of new pairs added."
  (when (< (length pattern-ids) 1)
    (return-from record-activation-and-update-graph 0))
  (let* ((session (get-or-create-session session-id))
         (prior (session-state-activated-patterns session))
         (new-ids (set-difference pattern-ids prior :test #'string=)))
    ;; Record all pattern IDs in session state
    (dolist (id pattern-ids)
      (record-activation session-id id))
    ;; Compute only NEW pairs to avoid double-counting
    (when (and new-ids (>= (+ (length new-ids) (length prior)) 2))
      (let ((pair-count 0))
        (with-lock-held (*co-app-ledger-lock*)
          ;; Within-new pairs
          (loop for (a . rest) on new-ids
                do (loop for b in rest
                         do (let ((key (co-app-key a b)))
                              (incf (gethash key *co-app-ledger* 0))
                              (incf pair-count))))
          ;; Cross pairs: new × prior
          (dolist (new-id new-ids)
            (dolist (old-id prior)
              (let ((key (co-app-key new-id old-id)))
                (incf (gethash key *co-app-ledger* 0))
                (incf pair-count)))))
        ;; Flush to disk and mark graph stale
        (when (> pair-count 0)
          (flush-co-app-ledger)
          (setf *graph-stale* t))
        pair-count))))

;;; Graph query helpers

(defun graph-outgoing (node-id)
  "Return outgoing edges for NODE-ID from the current graph."
  (gethash node-id *current-graph*))

(defun graph-incoming (node-id)
  "Return incoming edges for NODE-ID from the reverse index."
  (gethash node-id *incoming-edges*))

(defun graph-node-count ()
  "Number of nodes with outgoing edges in the current graph."
  (hash-table-count *current-graph*))

(defun graph-edge-count ()
  "Total number of edges in the current graph."
  (let ((count 0))
    (maphash (lambda (k v) (declare (ignore k)) (incf count (length v)))
             *current-graph*)
    count))

;;; Incoming edges index

(defun build-incoming-index ()
  "Build reverse index from current graph. Must be called after graph rebuild.
   Returns the count of nodes with incoming edges."
  (clrhash *incoming-edges*)
  (maphash (lambda (source edges)
             (declare (ignore source))
             (dolist (edge edges)
               (push edge (gethash (edge-target edge) *incoming-edges*))))
           *current-graph*)
  (hash-table-count *incoming-edges*))
