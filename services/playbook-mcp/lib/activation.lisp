;;; Playbook MCP Server - Spreading Activation
;;; Context-driven pattern retrieval via spreading activation on the semantic graph.

(in-package #:playbook-mcp)

;;; Core algorithm

(defun spread-activation (seeds &key (decay 0.7) (iterations 3) (direction :outgoing))
  "Spreading activation on *current-graph*.
   SEEDS is an alist of (node-id . initial-activation).
   DIRECTION: :outgoing (default), :incoming, or :both
   Uses relation-specific weights from *relation-weights*.
   Incoming edges use *incoming-decay-factor* additional decay.
   Conflict edges produce negative spread, floored at 0.
   Returns sorted alist of (node-id . final-activation)."
  (let ((activation (make-hash-table :test 'equal)))
    ;; Initialize seeds
    (loop for (id . act) in seeds
          do (setf (gethash id activation) (coerce act 'single-float)))
    ;; Spread for N iterations
    (dotimes (i iterations)
      (let ((new-activation (make-hash-table :test 'equal)))
        ;; Copy current activations
        (maphash (lambda (k v) (setf (gethash k new-activation) v)) activation)
        ;; Spread from each active node
        (maphash (lambda (node act)
                   ;; Forward spreading (outgoing edges)
                   (when (member direction '(:outgoing :both))
                     (dolist (edge (graph-outgoing node))
                       (let* ((target (edge-target edge))
                              (rel-weight (relation-weight (edge-relation edge)))
                              (spread (* act decay (edge-weight edge) rel-weight)))
                         (incf (gethash target new-activation 0.0) spread))))
                   ;; Backward spreading (incoming edges)
                   (when (member direction '(:incoming :both))
                     (dolist (edge (graph-incoming node))
                       (let* ((source (edge-source edge))
                              (rel-weight (relation-weight (edge-relation edge)))
                              ;; Incoming edges get additional decay factor
                              (spread (* act decay *incoming-decay-factor*
                                        (edge-weight edge) rel-weight)))
                         (incf (gethash source new-activation 0.0) spread)))))
                 activation)
        ;; Floor negative activations at 0 (from conflict inhibition)
        (maphash (lambda (k v)
                   (when (< v 0.0)
                     (setf (gethash k new-activation) 0.0)))
                 new-activation)
        (setf activation new-activation)))
    ;; Convert to sorted alist
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) activation)
      (sort result #'> :key #'cdr))))

;;; Temporal decay tracking

(defvar *pattern-last-used* (make-hash-table :test 'equal)
  "Pattern ID → universal time of last use. Used for temporal decay.")

(defparameter *recency-half-life* (* 30 24 60 60)
  "Half-life for temporal decay in seconds (30 days).")

(defparameter *temporal-min-factor* 0.3
  "Minimum decay factor for never-used patterns.")

(defvar *temporal-data-path* nil
  "Path to temporal data JSON file for persistence.")

(defun mark-pattern-used (pattern-id)
  "Record that a pattern was used now. Returns the timestamp."
  (setf (gethash pattern-id *pattern-last-used*) (get-universal-time)))

(defun temporal-decay (pattern-id)
  "Compute decay factor [0.3, 1.0] based on recency.
   Just-used = 1.0, 30 days old = ~0.56, never used = 0.3"
  (let ((last-used (gethash pattern-id *pattern-last-used*)))
    (if (null last-used)
        *temporal-min-factor*
        (let* ((age (- (get-universal-time) last-used))
               (decay (exp (- (/ age *recency-half-life*)))))
          (+ *temporal-min-factor* (* (- 1.0 *temporal-min-factor*) decay))))))

(defun apply-temporal-decay-to-seeds (seeds)
  "Multiply each seed's activation by its temporal decay factor.
   Returns new alist with decayed activations."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (coerce (* (cdr pair) (temporal-decay (car pair))) 'single-float)))
          seeds))

;;; Explainability - activation path tracking

(defstruct activation-path
  "Tracks how activation reached a node."
  (node-id "" :type string)
  (total-activation 0.0 :type single-float)
  (contributions nil :type list))  ; ((source relation amount) ...)

(defun spread-activation-with-paths (seeds &key (decay 0.7) (iterations 3) (direction :both))
  "Spreading activation that tracks contribution paths.
   Returns hash-table of node-id → activation-path struct."
  (let ((paths (make-hash-table :test 'equal)))
    ;; Initialize seeds as direct matches
    (loop for (id . act) in seeds
          do (setf (gethash id paths)
                   (make-activation-path
                    :node-id id
                    :total-activation (coerce act 'single-float)
                    :contributions (list (list "query" :direct act)))))
    ;; Spread for N iterations
    (dotimes (i iterations)
      ;; Spread from each active node
      (let ((updates nil))
        (maphash (lambda (node path)
                   (let ((act (activation-path-total-activation path)))
                     ;; Forward spreading
                     (when (member direction '(:outgoing :both))
                       (dolist (edge (graph-outgoing node))
                         (let* ((target (edge-target edge))
                                (rel-weight (relation-weight (edge-relation edge)))
                                (spread (* act decay (edge-weight edge) rel-weight)))
                           (when (> spread 0.0)
                             (push (list target node (edge-relation edge) spread) updates)))))
                     ;; Backward spreading
                     (when (member direction '(:incoming :both))
                       (dolist (edge (graph-incoming node))
                         (let* ((source (edge-source edge))
                                (rel-weight (relation-weight (edge-relation edge)))
                                (spread (* act decay *incoming-decay-factor*
                                          (edge-weight edge) rel-weight)))
                           (when (> spread 0.0)
                             (push (list source node (edge-relation edge) spread) updates)))))))
                 paths)
        ;; Apply updates
        (dolist (update updates)
          (destructuring-bind (target source relation amount) update
            (let ((existing (gethash target paths)))
              (if existing
                  (progn
                    (incf (activation-path-total-activation existing) amount)
                    (push (list source relation amount)
                          (activation-path-contributions existing)))
                  (setf (gethash target paths)
                        (make-activation-path
                         :node-id target
                         :total-activation (coerce amount 'single-float)
                         :contributions (list (list source relation amount))))))))))
    paths))

(defun explain-activation (paths pattern-id &key (max-contributors 5))
  "Generate human-readable explanation for a pattern's activation."
  (let ((path (gethash pattern-id paths)))
    (if (null path)
        (format nil "~A: no activation" pattern-id)
        (let* ((total (activation-path-total-activation path))
               (contribs (activation-path-contributions path))
               (sorted (sort (copy-list contribs) #'> :key #'third))
               (top (subseq sorted 0 (min max-contributors (length sorted)))))
          (with-output-to-string (s)
            (format s "~A received ~,2F activation:~%" pattern-id total)
            (dolist (c top)
              (destructuring-bind (source relation amount) c
                (format s "  * ~,2F from ~A (~A)~%" amount source relation))))))))

(defun explain-top-activations (paths top-k &key (max-contributors 3))
  "Get explanations for top-K activated patterns.
   Returns list of (pattern-id activation explanation-string)."
  (let ((sorted nil))
    (maphash (lambda (id path)
               (push (cons id (activation-path-total-activation path)) sorted))
             paths)
    (setf sorted (sort sorted #'> :key #'cdr))
    (mapcar (lambda (pair)
              (list (car pair)
                    (cdr pair)
                    (explain-activation paths (car pair)
                                        :max-contributors max-contributors)))
            (subseq sorted 0 (min top-k (length sorted))))))

;;; Temporal data persistence

(defun save-temporal-data (path)
  "Save temporal data to JSON file (atomic write).
   Format: {\"pattern-id\": unix-timestamp, ...}"
  (let* ((abs-path (namestring (merge-pathnames path)))
         (temp-path (format nil "~A.tmp" abs-path))
         (ht (make-hash-table :test 'equal)))
    ;; Convert to JSON-friendly format
    (maphash (lambda (id timestamp)
               (setf (gethash id ht) timestamp))
             *pattern-last-used*)
    (with-open-file (s temp-path :direction :output :if-exists :supersede)
      (yason:encode ht s))
    (rename-file temp-path abs-path)
    (hash-table-count ht)))

(defun load-temporal-data (path)
  "Load temporal data from JSON file into *pattern-last-used*.
   Returns count of patterns loaded, or NIL if file missing."
  (when (probe-file path)
    (let ((ht (yason:parse (alexandria:read-file-into-string path))))
      (clrhash *pattern-last-used*)
      (maphash (lambda (id timestamp)
                 (setf (gethash id *pattern-last-used*) (truncate timestamp)))
               ht)
      (hash-table-count *pattern-last-used*))))

;;; Full activation pipeline

(defun activate-patterns (query domains
                          &key task-type recent-patterns (top-k 5)
                               manual-edges-path)
  "Context-driven pattern retrieval via spreading activation.
   QUERY: natural language query string
   DOMAINS: list of domain strings to seed from
   TASK-TYPE: optional task type keyword (unused for now, reserved)
   RECENT-PATTERNS: list of pattern IDs to exclude (already surfaced)
   TOP-K: number of results to return
   MANUAL-EDGES-PATH: path to manual edges file for graph freshness"
  (declare (ignore task-type))
  ;; Ensure graph is fresh
  (ensure-graph-fresh :manual-edges-path manual-edges-path)
  ;; Build seeds from domain match + effectiveness
  (let ((seeds nil)
        (all-patterns (list-patterns)))
    ;; Seed 1: domain-matching patterns
    (dolist (domain domains)
      (dolist (p (patterns-by-domain domain))
        (let* ((id (pattern-id p))
               (activation (pattern-beta-score p)))
          (push (cons id (coerce activation 'single-float)) seeds))))
    ;; Seed 2: embedding similarity to query (when Ollama available)
    (let ((query-embedding (get-embedding query)))
      (when query-embedding
        (dolist (p all-patterns)
          (let ((p-embedding (pattern-embedding p)))
            (when p-embedding
              (let ((sim (cosine-similarity query-embedding p-embedding)))
                (when (> sim 0.3)
                  ;; Add to existing seed or create new
                  (let ((existing (assoc (pattern-id p) seeds :test #'string=)))
                    (if existing
                        (incf (cdr existing) (* 0.5 sim))
                        (push (cons (pattern-id p) (coerce (* 0.5 sim) 'single-float))
                              seeds))))))))))
    ;; Spread activation
    (let ((activated (spread-activation seeds)))
      ;; Filter out recent patterns
      (when recent-patterns
        (setf activated
              (remove-if (lambda (pair)
                           (member (car pair) recent-patterns :test #'string=))
                         activated)))
      ;; Return top-K with pattern objects
      (let ((top (subseq activated 0 (min top-k (length activated)))))
        (mapcar (lambda (pair)
                  (cons (get-pattern (car pair)) (cdr pair)))
                ;; Filter out patterns not in store (edge-only nodes)
                (remove-if-not (lambda (pair) (get-pattern (car pair))) top))))))

(defun activate-patterns-full (query domains
                                &key task-type recent-patterns (top-k 5)
                                     manual-edges-path
                                     (direction :both)
                                     (use-temporal-decay t)
                                     (explain nil))
  "Full-featured pattern activation with all graph enhancements.
   QUERY: natural language query string
   DOMAINS: list of domain strings to seed from
   TASK-TYPE: optional task type keyword (reserved)
   RECENT-PATTERNS: list of pattern IDs to exclude (already surfaced)
   TOP-K: number of results to return
   MANUAL-EDGES-PATH: path to manual edges file for graph freshness
   DIRECTION: :outgoing (default preserves old behavior), :incoming, or :both
   USE-TEMPORAL-DECAY: when T, apply recency decay to seeds
   EXPLAIN: when T, include explanation strings in output

   Returns: ((pattern . score) ...) when explain=nil (backward compatible)
            ((pattern score explanation) ...) when explain=t"
  (declare (ignore task-type))
  ;; Ensure graph is fresh
  (ensure-graph-fresh :manual-edges-path manual-edges-path)
  ;; Build seeds from domain match + effectiveness
  (let ((seeds nil)
        (all-patterns (list-patterns)))
    ;; Seed 1: domain-matching patterns
    (dolist (domain domains)
      (dolist (p (patterns-by-domain domain))
        (let* ((id (pattern-id p))
               (eff (pattern-effectiveness p))
               (activation (+ 0.5 (* 0.5 (/ (max eff 0) (max 1 (+ eff 1)))))))
          (push (cons id (coerce activation 'single-float)) seeds))))
    ;; Seed 2: embedding similarity to query (when Ollama available)
    (let ((query-embedding (get-embedding query)))
      (when query-embedding
        (dolist (p all-patterns)
          (let ((p-embedding (pattern-embedding p)))
            (when p-embedding
              (let ((sim (cosine-similarity query-embedding p-embedding)))
                (when (> sim 0.3)
                  (let ((existing (assoc (pattern-id p) seeds :test #'string=)))
                    (if existing
                        (incf (cdr existing) (* 0.5 sim))
                        (push (cons (pattern-id p) (coerce (* 0.5 sim) 'single-float))
                              seeds))))))))))
    ;; Apply temporal decay to seeds if enabled
    (when use-temporal-decay
      (setf seeds (apply-temporal-decay-to-seeds seeds)))
    ;; Choose spreading strategy based on explain flag
    (if explain
        ;; Path-tracking spread for explainability
        (let* ((paths (spread-activation-with-paths seeds :direction direction))
               (explained (explain-top-activations paths top-k)))
          ;; Filter by recent and valid patterns
          (let ((result nil))
            (dolist (entry explained)
              (destructuring-bind (id score explanation) entry
                (when (and (get-pattern id)
                           (not (member id recent-patterns :test #'string=)))
                  (push (list (get-pattern id) score explanation) result))))
            (nreverse (subseq result 0 (min top-k (length result))))))
        ;; Standard spread (faster, backward compatible)
        (let ((activated (spread-activation seeds :direction direction)))
          (when recent-patterns
            (setf activated
                  (remove-if (lambda (pair)
                               (member (car pair) recent-patterns :test #'string=))
                             activated)))
          (let ((top (subseq activated 0 (min top-k (length activated)))))
            (mapcar (lambda (pair)
                      (cons (get-pattern (car pair)) (cdr pair)))
                    (remove-if-not (lambda (pair) (get-pattern (car pair))) top)))))))

;;; ============================================================
;;; ADAPTIVE DOMAIN-AGNOSTIC ACTIVATION
;;; ============================================================

(defparameter *min-edge-density-for-spreading* 0.05
  "Minimum edge density (edges/patterns) to use spreading activation.
   Below this threshold, pure semantic search is used to avoid
   sparse graph distortion.")

(defun graph-edge-density ()
  "Compute edge density as edges/patterns.
   Returns 0.0 if no patterns exist."
  (let ((pattern-count (length (list-patterns)))
        (edge-count (hash-table-count *current-graph*)))
    (if (zerop pattern-count)
        0.0
        (/ (float edge-count) pattern-count))))

(defun activate-patterns-hybrid (query &key boost recent-patterns (top-k 5))
  "Semantic-primary activation with co-application boost.

   1. Get semantic results at 3x top-k (wide net)
   2. For each result, check co-app neighbors in the graph
   3. Boost neighbors that also appear in results by 10% of edge-weight × neighbor-score
   4. Re-sort and return top-k

   This avoids hub dominance from spreading activation while still leveraging
   co-application signal to promote contextually related patterns."
  (let* ((wide-k (* 3 top-k))
         (semantic (activate-patterns-semantic query
                                              :boost boost
                                              :recent-patterns recent-patterns
                                              :top-k wide-k))
         ;; Build score table for efficient lookup
         (scores (make-hash-table :test 'equal)))
    ;; Index scores by pattern ID
    (dolist (pair semantic)
      (setf (gethash (pattern-id (car pair)) scores) (cdr pair)))
    ;; Apply co-app boost: for each result, boost its co-app neighbors
    (dolist (pair semantic)
      (let* ((id (pattern-id (car pair)))
             (score (cdr pair))
             (neighbors (graph-outgoing id)))
        (dolist (edge neighbors)
          (when (eq :co-applied (edge-relation edge))
            (let ((neighbor-score (gethash (edge-target edge) scores)))
              (when neighbor-score
                ;; Boost neighbor by 10% of edge-weight × this pattern's score
                (incf (gethash (edge-target edge) scores)
                      (* 0.1 (edge-weight edge) score))))))))
    ;; Rebuild results with boosted scores and re-sort
    (let ((boosted (mapcar (lambda (pair)
                             (cons (car pair)
                                   (gethash (pattern-id (car pair)) scores)))
                           semantic)))
      (setf boosted (sort boosted #'> :key #'cdr))
      (subseq boosted 0 (min top-k (length boosted))))))

(defun activate-patterns-adaptive (query &key boost recent-patterns (top-k 5)
                                              manual-edges-path)
  "Domain-agnostic pattern activation with adaptive strategy.

   QUERY: natural language query string
   BOOST: optional list of domain strings to boost (not filter)
   RECENT-PATTERNS: list of pattern IDs to exclude
   TOP-K: number of results to return
   MANUAL-EDGES-PATH: path to manual edges file

   Strategy:
   - When co-app edges exist: hybrid (semantic + co-app boost)
   - Otherwise: pure semantic search

   Returns: list of (pattern . score) pairs"
  (ensure-graph-fresh :manual-edges-path manual-edges-path)
  (if (> (hash-table-count *co-app-ledger*) 0)
      ;; Co-app data available: use hybrid (semantic + co-app boost)
      (activate-patterns-hybrid query
                                :boost boost
                                :recent-patterns recent-patterns
                                :top-k top-k)
      ;; No co-app data: pure semantic search
      (activate-patterns-semantic query
                                  :boost boost
                                  :recent-patterns recent-patterns
                                  :top-k top-k)))

(defun activate-patterns-semantic (query &key boost recent-patterns (top-k 5))
  "Pure semantic search activation (no spreading).
   Used when graph is too sparse for spreading to add value.

   BOOST: optional list of domain strings to increase weight of matching patterns"
  (let* ((query-embedding (get-embedding query))
         (all-patterns (list-patterns))
         (results nil))
    (when query-embedding
      ;; Score all patterns by embedding similarity
      (dolist (p all-patterns)
        (let ((p-embedding (ensure-pattern-embedding p)))
          (when p-embedding
            (let* ((sim (cosine-similarity query-embedding p-embedding))
                   (domain (pattern-domain p))
                   ;; Apply boost if domain matches
                   (boosted-sim (if (and boost (member domain boost :test #'string-equal))
                                    (* sim 1.5)  ; 50% boost for matching domains
                                    sim)))
              (when (> boosted-sim 0.3)
                (push (cons p boosted-sim) results)))))))
    ;; Sort by score descending
    (setf results (sort results #'> :key #'cdr))
    ;; Filter recent patterns
    (when recent-patterns
      (setf results
            (remove-if (lambda (pair)
                         (member (pattern-id (car pair)) recent-patterns :test #'string=))
                       results)))
    ;; Return top-K
    (subseq results 0 (min top-k (length results)))))

(defun activate-patterns-with-spreading (query &key boost recent-patterns (top-k 5))
  "Semantic + spreading activation for rich graphs.

   Seeds from embedding similarity, spreads via graph edges.
   BOOST domains get additional seed weight."
  (let* ((query-embedding (get-embedding query))
         (all-patterns (list-patterns))
         (seeds nil))
    ;; Build seeds from embedding similarity
    (when query-embedding
      (dolist (p all-patterns)
        (let ((p-embedding (ensure-pattern-embedding p)))
          (when p-embedding
            (let* ((sim (cosine-similarity query-embedding p-embedding))
                   (id (pattern-id p))
                   (domain (pattern-domain p)))
              (when (> sim 0.3)
                ;; Base seed from similarity
                (let ((seed-weight (* 0.5 sim)))
                  ;; Apply boost if domain matches
                  (when (and boost (member domain boost :test #'string-equal))
                    (incf seed-weight 0.3))
                  (push (cons id (coerce seed-weight 'single-float)) seeds))))))))
    ;; Spread activation
    (let ((activated (spread-activation seeds :direction :both)))
      ;; Filter recent patterns
      (when recent-patterns
        (setf activated
              (remove-if (lambda (pair)
                           (member (car pair) recent-patterns :test #'string=))
                         activated)))
      ;; Return top-K with pattern objects
      (let ((top (subseq activated 0 (min top-k (length activated)))))
        (mapcar (lambda (pair)
                  (cons (get-pattern (car pair)) (cdr pair)))
                (remove-if-not (lambda (pair) (get-pattern (car pair))) top))))))
