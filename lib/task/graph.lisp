(in-package #:task)

;;; Task Graph — Shared graph structure and queries for the task system.
;;;
;;; The task graph has four edge layers:
;;;   1. Topic edges — tasks sharing a topic cluster (inferred from names)
;;;   2. Same-day edges — tasks created on the same day (inferred from dates)
;;;   3. Reference edges — cross-task citations in handoff documents
;;;   4. Metadata edges — declared relationships from event-sourced metadata
;;;
;;; This module provides:
;;;   - Task info extraction (topic, date, scan)
;;;   - Graph structure (bidirectional adjacency)
;;;   - Edge extraction (four layers)
;;;   - Graph queries (frontier, upstream, downstream)
;;;   - Enriched query computation (Theorem 9: cross-graph naturality)
;;;   - Dashboard rendering (enriched task view, rich event formatting)

;;; ============================================================
;;; TASK INFO EXTRACTION
;;; Derive everything from directory names and filesystem.
;;; ============================================================

(defun extract-date-prefix (dirname)
  "Extract YYYY-MM-DD prefix from task directory name."
  (when (and (>= (length dirname) 10)
             (char= (char dirname 4) #\-)
             (char= (char dirname 7) #\-))
    (subseq dirname 0 10)))

(defun strip-date-prefix (name)
  "Strip leading YYYY-MM-DD- prefix from NAME if present."
  (if (and (> (length name) 11)
           (digit-char-p (char name 0))
           (char= (char name 4) #\-)
           (digit-char-p (char name 5))
           (char= (char name 7) #\-)
           (digit-char-p (char name 8))
           (char= (char name 10) #\-))
      (subseq name 11)
      name))

(defun strip-phase-prefix (name)
  "Strip leading phase-N- or phase-Na- prefix from NAME if present.
   Handles phase-1-, phase-2a-, phase-12- etc."
  (if (and (>= (length name) 7)
           (string= "phase-" (subseq name 0 6)))
      (let ((rest (subseq name 6)))
        (loop for i from 0 below (length rest)
              for c = (char rest i)
              while (or (digit-char-p c) (alpha-char-p c))
              finally (return (if (and (< i (length rest))
                                       (char= (char rest i) #\-))
                                  (subseq rest (1+ i))
                                  name))))
      name))

(defparameter *topic-noise-words*
  '("and" "for" "the" "with" "from" "into"
    "implementation" "exploration" "research"
    "fix" "update" "add" "new")
  "Words that carry no topic-discriminating information.")

(defun extract-topic (dirname)
  "Extract topic cluster from directory name.
   Strips date prefixes (including double-dates), phase-N- prefixes,
   and skips noise words to find the first 2 meaningful words.
   The Galois connection f: Tasks → Topics."
  (let* ((name (strip-date-prefix dirname))
         (name (strip-date-prefix name))     ; double-dated tasks
         (name (strip-phase-prefix name))
         (parts (uiop:split-string name :separator "-")))
    (let ((meaningful (loop for p in parts
                           when (not (member p *topic-noise-words*
                                             :test #'string-equal))
                           collect p into acc
                           when (= (length acc) 2) do (loop-finish)
                           finally (return acc))))
      (if meaningful
          (format nil "~{~A~^-~}" meaningful)
          (or (first parts) "unknown")))))

(defun humanize-task-name (dirname)
  "Convert directory name to human-readable display name."
  (let* ((name (strip-date-prefix dirname))
         (name (strip-date-prefix name))     ; double-dated tasks
         (words (uiop:split-string name :separator "-")))
    (format nil "~{~:(~A~)~^ ~}" words)))

(defun extract-goals-from-plan (plan-path)
  "Extract goals with completion status from plan.md.
   Scans for ## Phase N or ## Step N headers and detects completion markers."
  (handler-case
    (let ((goals nil))
      (with-open-file (s plan-path :direction :input)
        (loop for line = (read-line s nil nil)
              while line
              do (let ((trimmed (string-left-trim '(#\Space #\#) line)))
                   (when (and (>= (length line) 4)
                              (string= "## " line :end2 3)
                              (or (and (>= (length trimmed) 5)
                                       (string-equal "Phase" trimmed :end2 5))
                                  (and (>= (length trimmed) 4)
                                       (string-equal "Step" trimmed :end2 4)
                                       (or (= (length trimmed) 4)
                                           (not (alpha-char-p (char trimmed 4)))))))
                     (let ((done-p (or (search "✓" trimmed)
                                       (search "[x]" trimmed :test #'char-equal)
                                       (search "complete" trimmed :test #'char-equal)
                                       (search "done" trimmed :test #'char-equal))))
                       (push (list :text trimmed :done (not (null done-p)))
                             goals))))))
      (nreverse goals))
    (error () nil)))

(defun extract-tags (dirname)
  "Extract meaningful keyword tags from task directory name."
  (let* ((name (if (and (> (length dirname) 11)
                        (char= (char dirname 4) #\-)
                        (char= (char dirname 7) #\-))
                   (subseq dirname 11)
                   dirname))
         (words (uiop:split-string name :separator "-"))
         (noise '("the" "a" "an" "and" "or" "for" "with" "in" "of" "to"
                  "implementation" "research" "exploration" "integration"
                  "improvement" "removal" "clean" "break" "new" "add"
                  "update" "fix" "based" "task")))
    (remove-if (lambda (w) (or (< (length w) 3)
                               (member w noise :test #'string-equal)
                               (every #'digit-char-p w)))
               words)))

(defun file-mod-time (path)
  "Get file modification time as universal-time, or 0."
  (handler-case
    (file-write-date (pathname path))
    (error () 0)))

(defun scan-task-infos (&optional (tasks-root *tasks-root*))
  "Scan task filesystem and extract enriched info plists for all tasks.
   Returns list of plists with :id :display-name :topic :date :has-events
   :has-plan :has-handoffs :has-research :goals :goal-count :goals-done
   :tags :latest-mod :status. Both event-sourced and legacy tasks included."
  (unless tasks-root
    (detect-tasks-root))
  (let ((dirs (directory (merge-pathnames "*/" (pathname (or tasks-root *tasks-root*)))))
        (infos nil))
    (dolist (dir dirs)
      (let* ((id (car (last (pathname-directory dir))))
             (events-path (merge-pathnames "events.jsonl" dir))
             (plan-path (merge-pathnames "plan.md" dir))
             (research-path (merge-pathnames "research.md" dir))
             (has-events (probe-file events-path))
             (has-plan (probe-file plan-path))
             (has-handoffs (uiop:directory-exists-p
                            (merge-pathnames "handoffs/" dir)))
             (has-research (probe-file research-path))
             (topic (extract-topic id))
             (date (extract-date-prefix id))
             (goals (when has-plan
                      (extract-goals-from-plan (namestring plan-path))))
             (tags (extract-tags id))
             (latest-mod (reduce #'max
                           (mapcar #'file-mod-time
                             (remove nil (list (namestring plan-path)
                                              (namestring events-path)
                                              (namestring research-path))))
                           :initial-value 0)))
        (push (list :id id
                    :display-name (humanize-task-name id)
                    :topic topic
                    :date date
                    :has-events has-events
                    :has-plan has-plan
                    :has-handoffs has-handoffs
                    :has-research has-research
                    :goals goals
                    :goal-count (length goals)
                    :goals-done (count-if (lambda (g) (getf g :done)) goals)
                    :tags tags
                    :latest-mod latest-mod
                    :status (cond
                              (has-events :event-sourced)
                              ((and (> latest-mod 0)
                                    (< (- (get-universal-time) latest-mod)
                                       (* 72 3600)))
                               :recent)
                              (t :dormant)))
              infos)))
    (nreverse infos)))

;;; ============================================================
;;; MULTI-DEPOT TASK SCANNING
;;; Scan tasks across multiple depots with qualified IDs.
;;; ============================================================

(defun scan-depot-tasks (depot-name)
  "Scan tasks from a specific depot, returning infos with qualified IDs.

   Arguments:
     depot-name - Name of depot to scan (e.g., \"core\", \"amti\")

   Returns:
     List of task info plists with :id as qualified ID (depot:task-id)
     and :depot field added."
  (let ((tasks-root (gethash depot-name *depot-tasks-roots*)))
    (unless tasks-root
      ;; Try to resolve via depot library (guard against missing package)
      (let* ((depot-pkg (find-package :depot))
             (depot-fn (when depot-pkg (find-symbol "DEPOT-TASKS-ROOT" depot-pkg))))
        (when depot-fn
          (setf tasks-root (funcall depot-fn depot-name)))))
    (when tasks-root
      (let ((infos (scan-task-infos tasks-root)))
        ;; Qualify IDs and add depot field
        (mapcar (lambda (info)
                  (let ((bare-id (getf info :id)))
                    (list* :id (qualify-task-id depot-name bare-id)
                           :depot depot-name
                           :bare-id bare-id
                           (alexandria:remove-from-plist info :id))))
                infos)))))

(defun scan-all-depot-tasks ()
  "Scan tasks from all depots, returning infos with qualified IDs.

   Requires *depot-tasks-roots* to be populated (via detect-all-task-roots).
   If empty, falls back to single-depot scan.

   Returns:
     List of task info plists from all depots, with :id as qualified ID
     (depot:task-id) and :depot field."
  ;; Ensure we have depot roots
  (when (zerop (hash-table-count *depot-tasks-roots*))
    (detect-all-task-roots))

  (if (zerop (hash-table-count *depot-tasks-roots*))
      ;; Fallback to single depot
      (scan-task-infos)
      ;; Multi-depot scan
      (let ((all-tasks '()))
        (maphash (lambda (depot-name tasks-root)
                   (declare (ignore tasks-root))
                   (let ((depot-tasks (scan-depot-tasks depot-name)))
                     (setf all-tasks (nconc all-tasks depot-tasks))))
                 *depot-tasks-roots*)
        all-tasks)))

;;; ============================================================
;;; GRAPH STRUCTURE
;;; Bidirectional adjacency for actionable queries.
;;; ============================================================

(defstruct task-graph
  "Graph of task relationships with forward and reverse adjacency."
  (nodes (make-hash-table :test 'equal))    ; id → plist of properties
  (forward (make-hash-table :test 'equal))  ; id → ((target relation weight) ...)
  (reverse (make-hash-table :test 'equal))) ; id → ((source relation weight) ...)

(defun graph-add-node (graph id &rest props)
  "Add a task node with properties to GRAPH."
  (setf (gethash id (task-graph-nodes graph)) props)
  graph)

(defun graph-node-props (graph id)
  "Get properties plist for node ID."
  (gethash id (task-graph-nodes graph)))

(defun graph-add-edge (graph from to relation &optional weight)
  "Add a directed edge with automatic reverse index."
  (push (list to relation weight) (gethash from (task-graph-forward graph)))
  (push (list from relation weight) (gethash to (task-graph-reverse graph)))
  graph)

;;; ============================================================
;;; THREE-LAYER EDGE EXTRACTION
;;; ============================================================

(defun extract-topic-edges (task-infos)
  "Layer 1 (inferred): tasks sharing topic cluster. Groups 2-5 only."
  (let ((by-topic (make-hash-table :test 'equal)))
    (dolist (info task-infos)
      (let ((topic (getf info :topic)))
        (when topic (push (getf info :id) (gethash topic by-topic)))))
    (let ((edges nil))
      (maphash (lambda (topic members)
                 (declare (ignore topic))
                 (when (<= 2 (length members) 5)
                   (loop for (a . rest) on members
                         do (dolist (b rest)
                              (push (list a b :topic) edges)))))
               by-topic)
      edges)))

;;; Token-overlap topic edges (5.15x improvement over basic topic extraction)

(defparameter *token-stopwords*
  '("mcp" "swank" "research" "phase" "depot" "lisp" "task" "implementation"
    "fix" "investigate" "add" "update" "test" "explore" "create" "the" "and"
    "for" "with" "from" "into" "core" "server" "client")
  "Common words to filter from token-overlap matching.")

(defun extract-task-tokens (task-id)
  "Extract meaningful tokens from task ID for similarity matching.
   Strips date prefix and filters stopwords."
  (let* ((name (strip-date-prefix task-id))
         (name (strip-date-prefix name))  ; double-dated tasks
         (name (strip-phase-prefix name))
         (parts (uiop:split-string name :separator "-")))
    (remove-if (lambda (tok)
                 (or (< (length tok) 3)
                     (member tok *token-stopwords* :test #'string-equal)))
               parts)))

(defun extract-token-overlap-edges (task-infos &key (min-overlap 2))
  "Extract edges between tasks sharing MIN-OVERLAP or more tokens.
   Stopword-filtered for meaningful connections."
  (let ((task-tokens (make-hash-table :test 'equal))
        (edges nil))
    ;; Build token sets for each task
    (dolist (info task-infos)
      (let ((id (getf info :id)))
        (setf (gethash id task-tokens) (extract-task-tokens id))))
    ;; Find pairs with sufficient overlap
    (let ((ids (loop for id being the hash-keys of task-tokens collect id)))
      (loop for i from 0 below (length ids)
            for id1 = (nth i ids)
            for tok1 = (gethash id1 task-tokens)
            do (loop for j from (1+ i) below (length ids)
                     for id2 = (nth j ids)
                     for tok2 = (gethash id2 task-tokens)
                     for shared = (intersection tok1 tok2 :test #'string-equal)
                     when (>= (length shared) min-overlap)
                     do (push (list id1 id2 :topic) edges))))
    edges))

(defun extract-topic-edges-enhanced (task-infos &key (min-token-overlap 2))
  "Extract topic edges using both methods:
   1. Original 2-word topic clustering (groups 2-5)
   2. Token overlap with stopword filtering (min 2 shared tokens)
   Returns union of both edge sets (5.15x improvement over basic)."
  (let* ((topic-edges (extract-topic-edges task-infos))
         (token-edges (extract-token-overlap-edges task-infos
                                                    :min-overlap min-token-overlap))
         ;; Dedupe using sorted pairs
         (seen (make-hash-table :test 'equal))
         (combined nil))
    ;; Add all edges, deduping
    (dolist (e (append topic-edges token-edges))
      (let* ((a (first e)) (b (second e))
             (key (if (string< a b) (list a b) (list b a))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push e combined))))
    combined))

(defun extract-same-day-edges (task-infos)
  "Layer 1 (inferred): tasks created on same day within the same depot.
   Groups by (depot, date). Small groups (2-6) get full cliques.
   Larger groups get a chain (each node linked to next) to stay O(n)."
  (let ((by-depot-date (make-hash-table :test 'equal)))
    (dolist (info task-infos)
      (let ((date (getf info :date))
            (depot (or (getf info :depot) "default")))
        (when date
          (let ((key (format nil "~A:~A" depot date)))
            (push (getf info :id) (gethash key by-depot-date))))))
    (let ((edges nil))
      (maphash (lambda (key members)
                 (declare (ignore key))
                 (when (>= (length members) 2)
                   (if (<= (length members) 6)
                       ;; Small group: full clique
                       (loop for (a . rest) on members
                             do (dolist (b rest)
                                  (push (list a b :same-day) edges)))
                       ;; Large group: chain connectivity (O(n) edges)
                       (loop for (a b) on members
                             while b
                             do (push (list a b :same-day) edges)))))
               by-depot-date)
      edges)))

;;; ============================================================
;;; STATE EDGE EXTRACTION (Coalgebraic Layer)
;;; Edges from OR-Set in task state — replaces metadata edges.
;;; ============================================================

(defun try-parse-json-array (str)
  "Try to parse STR as a JSON array. Returns list or NIL."
  (handler-case
      (when (and (> (length str) 0) (char= (char str 0) #\[))
        (let ((result (yason:parse str)))
          (when (listp result) result)))
    (error () nil)))

(defun extract-state-edges (task-id)
  "Extract edges from a task's OR-Set as (from to type) triples.
   Replays event log, builds state, decodes edge members.
   Returns raw target IDs as stored in events (may be qualified or bare).
   Callers handle ID qualification as needed for their graph mode."
  (let ((path (task-events-path task-id)))
    (when (probe-file path)
      (handler-case
          (let* ((log (elog-load path))
                 (events (reverse (event-log-events log)))
                 (state (when events (compute-state events))))
            (when state
              (let ((result nil))
                (dolist (member (ors-members (task-state-edges state)))
                  (let ((decoded (decode-edge member)))
                    (when decoded
                      (push (list task-id (car decoded) (cdr decoded))
                            result))))
                (nreverse result))))
        (error () nil)))))

(defun extract-all-state-edges (&optional (tasks-root *tasks-root*))
  "Extract declared edges from all event-sourced tasks in a single depot.
   Returns list of (from to type) triples with bare IDs.
   Used as building block by health query functions per-depot."
  (unless tasks-root (detect-tasks-root))
  (let ((edges nil))
    (dolist (dir (uiop:subdirectories (or tasks-root *tasks-root*)))
      (let* ((id (first (last (pathname-directory dir))))
             (task-edges (extract-state-edges id)))
        (dolist (edge task-edges)
          ;; Normalize target to bare ID for single-depot graph consistency
          (let ((target (multiple-value-bind (depot bare)
                            (parse-qualified-id (second edge))
                          (declare (ignore depot))
                          bare)))
            (push (list (first edge) target (third edge)) edges)))))
    (nreverse edges)))

(defun extract-reference-edges (task-infos tasks-root)
  "Layer 2 (natural): cross-task citations in handoff documents.
   Used as building block by per-depot health queries.

   Optimized with cl-ppcre: builds a single regex matching all task IDs,
   then scans each handoff file once to find all references."
  (let* ((all-ids (mapcar (lambda (info) (getf info :id)) task-infos))
         (id-set (make-hash-table :test 'equal))
         (edges nil))
    (dolist (id all-ids)
      (setf (gethash id id-set) t))
    (let* ((pattern (format nil "\\b(~{~A~^|~})\\b"
                            (mapcar #'cl-ppcre:quote-meta-chars all-ids)))
           (scanner (cl-ppcre:create-scanner pattern)))
      (dolist (info task-infos)
        (let* ((id (getf info :id))
               (handoff-dir (format nil "~A~A/handoffs/" tasks-root id))
               (handoffs (when (uiop:directory-exists-p handoff-dir)
                           (directory (merge-pathnames "*.md" (pathname handoff-dir))))))
          (dolist (hf handoffs)
            (handler-case
                (let ((content (uiop:read-file-string hf)))
                  (cl-ppcre:do-matches-as-strings (match scanner content)
                    (when (and (gethash match id-set)
                               (not (equal match id)))
                      (pushnew (list id match :reference) edges :test #'equal))))
              (error () nil))))))
    edges))

(defun build-task-graph (&key (tasks-root *tasks-root*))
  "Build a task graph for a single depot root.
   Used as building block by per-depot health queries (find-bidirectional-refs etc.).
   For the primary multi-depot graph, use build-multi-depot-task-graph instead."
  (unless tasks-root (detect-tasks-root))
  (let* ((root (or tasks-root *tasks-root*))
         (infos (scan-task-infos root))
         (graph (make-task-graph)))
    (dolist (info infos)
      (apply #'graph-add-node graph (getf info :id)
             (list :display-name (getf info :display-name)
                   :topic (getf info :topic)
                   :status (getf info :status)
                   :has-plan (getf info :has-plan)
                   :has-events (getf info :has-events))))
    (dolist (edge (extract-topic-edges-enhanced infos))
      (graph-add-edge graph (first edge) (second edge) (third edge) (fourth edge)))
    (dolist (edge (extract-same-day-edges infos))
      (graph-add-edge graph (first edge) (second edge) (third edge)))
    (dolist (edge (extract-reference-edges infos (namestring root)))
      (graph-add-edge graph (first edge) (second edge) (third edge)))
    (dolist (edge (extract-all-state-edges root))
      (graph-add-edge graph (first edge) (second edge) (third edge)))
    graph))

;;; ============================================================
;;; GRAPH CACHING LAYER
;;; ============================================================

(defvar *graph-cache* (make-hash-table :test 'equal)
  "Cache for task graphs. Key: depot name or tasks-root path, Value: (timestamp . graph)")

(defvar *graph-cache-ttl* 3600
  "TTL for graph cache in seconds. Default 3600 seconds (1 hour).
   With mutation-based invalidation in emit-event, TTL is a safety net.")

(defvar *graph-cache-lock* (bt:make-lock "graph-cache")
  "Mutex preventing concurrent graph builds (thundering herd).")

(defun get-cached-multi-depot-graph ()
  "Get cached multi-depot graph or build fresh. Uses TTL-based invalidation.
   Key is 'multi-depot' in the graph cache.
   Lock-free fast path for cache hits; lock only held during rebuild
   to prevent thundering herd from concurrent Hunchentoot worker threads."
  (let* ((key "multi-depot")
         (entry (gethash key *graph-cache*))
         (now (get-universal-time))
         (valid (and entry
                     (< (- now (car entry)) *graph-cache-ttl*))))
    (if valid
        (cdr entry)
        ;; Cache miss — acquire lock, double-check, rebuild if needed
        (bt:with-lock-held (*graph-cache-lock*)
          (let* ((entry2 (gethash key *graph-cache*))
                 (valid2 (and entry2
                              (< (- (get-universal-time) (car entry2))
                                 *graph-cache-ttl*))))
            (if valid2
                (cdr entry2)
                (let ((graph (build-multi-depot-task-graph)))
                  (setf (gethash key *graph-cache*) (cons (get-universal-time) graph))
                  graph)))))))

(defun clear-graph-cache ()
  "Clear all cached graphs."
  (clrhash *graph-cache*))

(defun invalidate-graph-cache (key)
  "Invalidate cache for a specific key."
  (remhash key *graph-cache*))

;;; --- Task Infos Cache ---
;;; Caches the result of scan-all-depot-tasks so callers that only need
;;; task infos (not the full graph) avoid a redundant 3s filesystem scan.
;;; Populated as a side-effect of build-multi-depot-task-graph.

(defvar *infos-cache* nil
  "Cached task infos from last graph build. Value: (timestamp . infos-list) or NIL.")

(defun get-cached-task-infos ()
  "Return cached task infos, triggering a graph build if cache is cold or expired.
   This is the preferred entry point for code that needs task infos —
   it avoids redundant scans by reusing the infos computed during graph construction."
  (let ((now (get-universal-time)))
    (if (and *infos-cache*
             (< (- now (car *infos-cache*)) *graph-cache-ttl*))
        (cdr *infos-cache*)
        ;; Graph build populates *infos-cache* as a side-effect
        (progn
          (get-cached-multi-depot-graph)
          (if *infos-cache*
              (cdr *infos-cache*)
              ;; Fallback: direct scan (should not normally happen)
              (scan-all-depot-tasks))))))

(defun clear-infos-cache ()
  "Clear the cached task infos."
  (setf *infos-cache* nil))

;;; ============================================================
;;; GRAPH CONSTRUCTION PIPELINE
;;; ============================================================


(defun build-multi-depot-task-graph ()
  "Build unified task graph across all depots.

   Uses scan-all-depot-tasks for qualified IDs.
   Creates cross-depot edges via topic matching.

   Returns:
     task-graph with nodes from all depots and cross-depot edges."
  ;; Ensure depot roots are detected
  (when (zerop (hash-table-count *depot-tasks-roots*))
    (detect-all-task-roots))

  (let* ((infos (scan-all-depot-tasks))
             (graph (make-task-graph)))
        ;; Populate infos cache as side-effect of graph build
        (setf *infos-cache* (cons (get-universal-time) infos))
        ;; Add all nodes with depot metadata
        (dolist (info infos)
          (apply #'graph-add-node graph (getf info :id)
                 (list :display-name (getf info :display-name)
                       :topic (getf info :topic)
                       :depot (getf info :depot)
                       :bare-id (getf info :bare-id)
                       :status (getf info :status)
                       :has-plan (getf info :has-plan)
                       :has-events (getf info :has-events))))

        ;; Layer 1: inferred edges (works across depots via topic/token matching)
        (dolist (edge (extract-topic-edges-enhanced infos))
          (graph-add-edge graph (first edge) (second edge) (third edge) (fourth edge)))
        (dolist (edge (extract-same-day-edges infos))
          (graph-add-edge graph (first edge) (second edge) (third edge)))

        ;; Layer 2: reference edges (per-depot, qualified)
        (maphash (lambda (depot-name tasks-root)
                   (let ((depot-edges (extract-reference-edges-for-depot
                                       depot-name tasks-root infos)))
                     (dolist (edge depot-edges)
                       (graph-add-edge graph (first edge) (second edge) (third edge)))))
                 *depot-tasks-roots*)

        ;; Layer 3: state edges (per-depot, qualified)
        (maphash (lambda (depot-name tasks-root)
                   (let ((depot-edges (extract-state-edges-for-depot depot-name tasks-root)))
                     (dolist (edge depot-edges)
                       (graph-add-edge graph (first edge) (second edge) (third edge)))))
                 *depot-tasks-roots*)

        graph))

(defun extract-reference-edges-for-depot (depot-name tasks-root infos)
  "Extract reference edges for a specific depot with qualified IDs.

   Finds references to other tasks in handoff files, qualifying with depot.
   Optimized with cl-ppcre regex for single-pass scanning."
  (let* ((depot-infos (remove-if-not (lambda (i) (string= (getf i :depot) depot-name)) infos))
         (bare-ids (remove nil (mapcar (lambda (i) (getf i :bare-id)) depot-infos)))
         (id-to-qualified (make-hash-table :test 'equal))
         (edges nil))
    ;; Build bare-id -> qualified-id lookup
    (dolist (info depot-infos)
      (let ((bare (getf info :bare-id))
            (qual (getf info :id)))
        (when bare
          (setf (gethash bare id-to-qualified) qual))))
    ;; Build regex for all bare IDs
    (when bare-ids
      (let* ((pattern (format nil "\\b(~{~A~^|~})\\b"
                              (mapcar #'cl-ppcre:quote-meta-chars bare-ids)))
             (scanner (cl-ppcre:create-scanner pattern)))
        (dolist (dir (uiop:subdirectories tasks-root))
          (let* ((bare-id (first (last (pathname-directory dir))))
                 (source-id (qualify-task-id depot-name bare-id))
                 (handoffs-dir (merge-pathnames "handoffs/" dir)))
            (when (uiop:directory-exists-p handoffs-dir)
              (dolist (handoff (directory (merge-pathnames "*.md" handoffs-dir)))
                (let ((content (ignore-errors (uiop:read-file-string handoff))))
                  (when content
                    ;; Find all task references in one regex scan
                    (cl-ppcre:do-matches-as-strings (match scanner content)
                      (when (and (not (string= bare-id match))
                                 (gethash match id-to-qualified))
                        (pushnew (list source-id (gethash match id-to-qualified) :references)
                                 edges :test #'equal)))))))))))
    edges))

(defun extract-state-edges-for-depot (depot-name tasks-root)
  "Extract state edges for a specific depot with qualified IDs.

   Returns list of (from to type) triples with qualified IDs."
  (let ((edges nil)
        ;; Bind *tasks-root* so bare-ID resolution inside extract-state-edges
        ;; points to this depot's tasks directory (not the server's default).
        (*tasks-root* tasks-root))
    (dolist (dir (uiop:subdirectories tasks-root))
      (let* ((bare-id (first (last (pathname-directory dir))))
             (source-id (qualify-task-id depot-name bare-id))
             (task-edges (extract-state-edges bare-id)))
        (dolist (edge task-edges)
          ;; Qualify the target ID if it's not already qualified
          (let ((target (second edge))
                (edge-type (third edge)))
            (push (list source-id
                        (if (qualified-id-p target)
                            target
                            (qualify-task-id depot-name target))
                        edge-type)
                  edges)))))
    (nreverse edges)))

;;; ============================================================
;;; GRAPH QUERIES
;;; ============================================================

(defun compute-frontier (graph)
  "Tasks ready to work on: not complete, all dependencies satisfied.
   Checks both :declared-dep (Layer 3 metadata) and :depends-on (Layer 4 OR-Set)."
  (let ((frontier nil))
    (maphash
     (lambda (id props)
       (let ((status (getf props :status))
             (all-deps-met t))
         (dolist (edge (gethash id (task-graph-forward graph)))
           (when (member (second edge) '(:declared-dep :depends-on))
             (let ((dep-props (gethash (first edge) (task-graph-nodes graph))))
               (unless (and dep-props
                            (string= (getf dep-props :status) "completed"))
                 (setf all-deps-met nil)))))
         (when (and (not (string= status "completed")) all-deps-met)
           (push id frontier))))
     (task-graph-nodes graph))
    (nreverse frontier)))

(defun graph-upstream (graph id)
  "Tasks that must complete before ID can start. BFS through forward depends-on."
  (let ((visited (make-hash-table :test 'equal))
        (queue (list id))
        (result nil))
    (loop while queue do
      (let ((current (pop queue)))
        (unless (gethash current visited)
          (setf (gethash current visited) t)
          (unless (equal current id) (push current result))
          (dolist (edge (gethash current (task-graph-forward graph)))
            (when (eq (second edge) :declared-dep)
              (push (first edge) queue))))))
    (nreverse result)))

(defun graph-downstream (graph id)
  "Tasks transitively unblocked by completing ID. BFS through reverse depends-on."
  (let ((visited (make-hash-table :test 'equal))
        (queue (list id))
        (result nil))
    (loop while queue do
      (let ((current (pop queue)))
        (unless (gethash current visited)
          (setf (gethash current visited) t)
          (unless (equal current id) (push current result))
          (dolist (edge (gethash current (task-graph-reverse graph)))
            (when (eq (second edge) :declared-dep)
              (push (first edge) queue))))))
    (nreverse result)))

;;; ============================================================
;;; HEALTH QUERIES
;;; Self-healing observation: the graph reports its own gaps.
;;; ============================================================

(defun children-complete-p (task-id)
  "Check if all currently-connected children of TASK-ID are complete.
   Returns T if task has no children or all children are completed."
  (let ((children (task-children task-id)))
    (or (null children)
        (let ((states (load-child-states children)))
          (every (lambda (child-id)
                   (let ((state (gethash child-id states)))
                     (and state
                          (string= (lww-value (task-state-status state))
                                   "completed"))))
                 children)))))

(defun incomplete-descendants (task-id)
  "Return alist of (child-id . status) for all incomplete descendants.
   Uses all-descendants for recursive traversal. Returns NIL if all
   descendants are complete or task has no children."
  (let ((descs (all-descendants task-id)))
    (when descs
      (let ((states (load-child-states descs))
            (incomplete nil))
        (dolist (id descs)
          (let* ((state (gethash id states))
                 (status (if state
                             (lww-value (task-state-status state))
                             "unknown")))
            (unless (string= status "completed")
              (push (cons id status) incomplete))))
        (nreverse incomplete)))))

(defun %find-stale-tasks (tasks-root &key (min-age-days 3))
  "Per-depot worker: find tasks with events but <=1 observation and age >= MIN-AGE-DAYS.
   TASKS-ROOT is required. Returns bare IDs."
  (let ((root tasks-root)
        (stale nil)
        (now (get-universal-time))
        (day-seconds (* 24 60 60)))
    (dolist (dir (uiop:subdirectories root))
      (let* ((id (first (last (pathname-directory dir))))
             (events-path (format nil "~Aevents.jsonl" (namestring dir))))
        (when (probe-file events-path)
          (handler-case
              (let* ((log (elog-load events-path))
                     (events (reverse (event-log-events log)))
                     (state (when events (compute-state events))))
                (when (and state
                           (<= (length (gs-members (task-state-observations state))) 1)
                           (not (string= (lww-value (task-state-status state)) "completed")))
                  (let ((create-event (find :task.create events :key #'event-type)))
                    (when (and create-event
                               (> (- now (event-timestamp create-event))
                                  (* min-age-days day-seconds)))
                      (push id stale)))))
            (error () nil)))))
    (nreverse stale)))

(defun %find-dead-ends (tasks-root &key (min-idle-days 7))
  "Per-depot worker: find active tasks with no events for >= MIN-IDLE-DAYS.
   TASKS-ROOT is required. Returns bare IDs."
  (let ((root tasks-root)
        (dead nil)
        (now (get-universal-time))
        (day-seconds (* 24 60 60)))
    (dolist (dir (uiop:subdirectories root))
      (let* ((id (first (last (pathname-directory dir))))
             (events-path (format nil "~Aevents.jsonl" (namestring dir))))
        (when (probe-file events-path)
          (handler-case
              (let* ((log (elog-load events-path))
                     (events (reverse (event-log-events log)))
                     (state (when events (compute-state events))))
                (when (and state
                           (not (string= (lww-value (task-state-status state)) "completed"))
                           events)
                  (let ((last-event (car (last events))))
                    (when (and last-event
                               (> (- now (event-timestamp last-event))
                                  (* min-idle-days day-seconds)))
                      (push id dead)))))
            (error () nil)))))
    (nreverse dead)))

(defun %find-declared-orphans (tasks-root)
  "Per-depot worker: find active tasks with no incoming declared edges.
   TASKS-ROOT is required. Returns bare IDs."
  (let* ((root tasks-root)
         (all-edges (extract-all-state-edges root))
         (targets (make-hash-table :test 'equal))
         (active-ids nil))
    ;; Collect all edge targets
    (dolist (edge all-edges)
      (setf (gethash (second edge) targets) t))
    ;; Find active tasks not targeted by any edge
    (dolist (dir (uiop:subdirectories root))
      (let* ((id (first (last (pathname-directory dir))))
             (events-path (format nil "~Aevents.jsonl" (namestring dir))))
        (when (probe-file events-path)
          (handler-case
              (let* ((log (elog-load events-path))
                     (events (reverse (event-log-events log)))
                     (state (when events (compute-state events))))
                (when (and state
                           (not (string= (lww-value (task-state-status state)) "completed"))
                           (not (gethash id targets)))
                  (push id active-ids)))
            (error () nil)))))
    (nreverse active-ids)))

(defun %find-stale-claims (tasks-root &key (timeout-hours 4))
  "Per-depot worker: find tasks with stale claims.
   TASKS-ROOT is required. Returns bare IDs."
  (let ((root tasks-root)
        (stale nil)
        (now (get-universal-time))
        (timeout-seconds (* timeout-hours 60 60)))
    (dolist (dir (uiop:subdirectories root))
      (let* ((id (first (last (pathname-directory dir))))
             (events-path (format nil "~Aevents.jsonl" (namestring dir))))
        (when (probe-file events-path)
          (handler-case
              (let* ((log (elog-load events-path))
                     (events (reverse (event-log-events log)))
                     (state (when events (compute-state events))))
                (when state
                  (let ((claim (lww-value (task-state-claim state))))
                    (when (and claim (> (length claim) 0))
                      (let ((last-from-claimer
                              (find claim (reverse events)
                                    :key #'event-session :test #'string=)))
                        (when (and last-from-claimer
                                   (> (- now (event-timestamp last-from-claimer))
                                      timeout-seconds))
                          (push id stale)))))))
            (error () nil)))))
    (nreverse stale)))

(defun %find-unexplored-frontier (tasks-root)
  "Per-depot worker: find active, unclaimed tasks.
   TASKS-ROOT is required. Returns bare IDs."
  (let ((root tasks-root)
        (frontier nil))
    (dolist (dir (uiop:subdirectories root))
      (let* ((id (first (last (pathname-directory dir))))
             (events-path (format nil "~Aevents.jsonl" (namestring dir))))
        (when (probe-file events-path)
          (handler-case
              (let* ((log (elog-load events-path))
                     (events (reverse (event-log-events log)))
                     (state (when events (compute-state events))))
                (when state
                  (let ((status (lww-value (task-state-status state)))
                        (claim (lww-value (task-state-claim state))))
                    (when (and (string= status "active")
                               (or (null claim) (string= claim "")))
                      (push id frontier)))))
            (error () nil)))))
    (nreverse frontier)))

(defun %find-convergent-clusters (tasks-root &key (min-cluster-size 2))
  "Per-depot worker: find topic clusters with multiple active tasks but no declared edges.
   TASKS-ROOT is required. Returns bare IDs in cluster structure."
  (let* ((root tasks-root)
         (infos (scan-task-infos root))
         (all-edges (extract-all-state-edges root))
         (edge-pairs (make-hash-table :test 'equal))
         (topics (make-hash-table :test 'equal))
         (result nil))
    (dolist (e all-edges)
      (setf (gethash (cons (first e) (second e)) edge-pairs) t)
      (setf (gethash (cons (second e) (first e)) edge-pairs) t))
    (dolist (info infos)
      (when (string= (getf info :status) "active")
        (push (getf info :id) (gethash (getf info :topic) topics))))
    (maphash (lambda (topic ids)
               (when (>= (length ids) min-cluster-size)
                 (let ((unlinked 0))
                   (loop for (a . rest) on ids
                         do (dolist (b rest)
                              (unless (or (gethash (cons a b) edge-pairs)
                                          (gethash (cons b a) edge-pairs))
                                (incf unlinked))))
                   (when (> unlinked 0)
                     (push (list topic ids unlinked) result)))))
             topics)
    (sort result #'> :key #'third)))

(defun %find-premature-completions (tasks-root)
  "Per-depot worker: find tasks marked completed but with incomplete descendants.
   TASKS-ROOT is required. Returns bare IDs in plist structure."
  (let ((root tasks-root)
        (premature nil))
    (dolist (dir (uiop:subdirectories root))
      (let* ((id (first (last (pathname-directory dir))))
             (events-path (task-events-path id)))
        (when (probe-file events-path)
          (handler-case
              (let* ((log (elog-load events-path))
                     (events (reverse (event-log-events log)))
                     (state (when events (compute-state events))))
                (when (and state
                           (string= "completed"
                                    (lww-value (task-state-status state))))
                  (let ((incomplete (incomplete-descendants id)))
                    (when incomplete
                      (push (list :id id
                                  :incomplete-count (length incomplete)
                                  :children (mapcar #'car incomplete))
                            premature)))))
            (error () nil)))))
    (nreverse premature)))

(defun find-bidirectional-refs-with-graph (graph)
  "Find task pairs that reference each other using pre-built graph.
   Returns list of sorted pairs (task-a task-b) where A < B alphabetically."
  (let* ((fwd (task-graph-forward graph))
         (pairs nil))
    (maphash (lambda (from edges)
               (dolist (edge edges)
                 ;; Edge format is (target type weight)
                 (when (eq (second edge) :reference)
                   (let* ((to (first edge))
                          (reverse-edges (gethash to fwd))
                          (has-reverse (find-if (lambda (e)
                                                  (and (eq (second e) :reference)
                                                       (equal (first e) from)))
                                                reverse-edges)))
                     (when has-reverse
                       ;; Store sorted pair to avoid duplicates
                       (let ((pair (if (string< from to)
                                       (list from to)
                                       (list to from))))
                         (pushnew pair pairs :test 'equal)))))))
             fwd)
    pairs))

(defun %find-bidirectional-refs (tasks-root)
  "Per-depot worker: find task pairs that reference each other.
   TASKS-ROOT is required. Returns bare ID pairs."
  (find-bidirectional-refs-with-graph (build-task-graph :tasks-root tasks-root)))

(defun find-unlinked-bidirectional-refs-with-graph (graph)
  "Find unlinked bidirectional refs using pre-built graph.
   Returns list of (:from A :to B :reason \"bidirectional-reference\" :suggested-type :related-to)."
  (let* ((fwd (task-graph-forward graph))
         (declared-types '(:phase-of :depends-on :related-to :discovered-during :enables :blocks))
         (bidir-pairs (find-bidirectional-refs-with-graph graph))
         (unlinked nil))
    ;; For each bidirectional pair, check if any declared edge exists
    (dolist (pair bidir-pairs)
      (let* ((a (first pair))
             (b (second pair))
             (a-edges (gethash a fwd))
             (b-edges (gethash b fwd))
             (has-declared nil))
        ;; Check A->B declared
        (dolist (e a-edges)
          (when (and (equal (first e) b)
                     (member (second e) declared-types))
            (setf has-declared t)))
        ;; Check B->A declared
        (dolist (e b-edges)
          (when (and (equal (first e) a)
                     (member (second e) declared-types))
            (setf has-declared t)))
        (unless has-declared
          (push (list :from a :to b
                      :reason "bidirectional-reference"
                      :suggested-type :related-to)
                unlinked))))
    unlinked))

(defun %find-unlinked-bidirectional-refs (tasks-root)
  "Per-depot worker: find task pairs that reference each other but have no declared edge.
   TASKS-ROOT is required. Returns bare IDs in plist structure."
  (find-unlinked-bidirectional-refs-with-graph (build-task-graph :tasks-root tasks-root)))

;;; ============================================================
;;; Health Functions (Multi-Depot Primary API)
;;; These call %find-*-per-depot workers above, qualify IDs.
;;; ============================================================

(defun find-stale-tasks (&key (min-age-days 3))
  "Find stale tasks across all depots. Returns qualified IDs."
  (detect-all-task-roots)
  (let ((results nil))
    (maphash (lambda (depot root)
               (dolist (id (%find-stale-tasks root :min-age-days min-age-days))
                 (push (qualify-task-id depot id) results)))
             *depot-tasks-roots*)
    (nreverse results)))

(defun find-dead-ends (&key (min-idle-days 7))
  "Find dead-end tasks across all depots. Returns qualified IDs."
  (detect-all-task-roots)
  (let ((results nil))
    (maphash (lambda (depot root)
               (dolist (id (%find-dead-ends root :min-idle-days min-idle-days))
                 (push (qualify-task-id depot id) results)))
             *depot-tasks-roots*)
    (nreverse results)))

(defun find-declared-orphans ()
  "Find declared orphans across all depots. Returns qualified IDs."
  (detect-all-task-roots)
  (let ((results nil))
    (maphash (lambda (depot root)
               (dolist (id (%find-declared-orphans root))
                 (push (qualify-task-id depot id) results)))
             *depot-tasks-roots*)
    (nreverse results)))

(defun find-stale-claims (&key (timeout-hours 4))
  "Find stale claims across all depots. Returns qualified IDs."
  (detect-all-task-roots)
  (let ((results nil))
    (maphash (lambda (depot root)
               (dolist (id (%find-stale-claims root :timeout-hours timeout-hours))
                 (push (qualify-task-id depot id) results)))
             *depot-tasks-roots*)
    (nreverse results)))

(defun find-unexplored-frontier ()
  "Find unexplored frontier across all depots. Returns qualified IDs."
  (detect-all-task-roots)
  (let ((results nil))
    (maphash (lambda (depot root)
               (dolist (id (%find-unexplored-frontier root))
                 (push (qualify-task-id depot id) results)))
             *depot-tasks-roots*)
    (nreverse results)))

(defun find-convergent-clusters ()
  "Find convergent clusters across all depots."
  (detect-all-task-roots)
  (let ((results nil))
    (maphash (lambda (depot root)
               (dolist (cluster (%find-convergent-clusters root))
                 (let ((topic (first cluster))
                       (ids (second cluster))
                       (unlinked (third cluster)))
                   (push (list topic
                               (mapcar (lambda (id) (qualify-task-id depot id)) ids)
                               unlinked)
                         results))))
             *depot-tasks-roots*)
    (nreverse results)))

(defun find-premature-completions ()
  "Find premature completions across all depots."
  (detect-all-task-roots)
  (let ((results nil))
    (maphash (lambda (depot root)
               (dolist (item (%find-premature-completions root))
                 (let ((id (getf item :id))
                       (count (getf item :incomplete-count))
                       (children (getf item :children)))
                   (push (list :id (qualify-task-id depot id)
                               :incomplete-count count
                               :children (mapcar (lambda (c) (qualify-task-id depot c)) children))
                         results))))
             *depot-tasks-roots*)
    (nreverse results)))

(defun find-unlinked-bidirectional-refs ()
  "Find unlinked bidirectional refs across all depots."
  (detect-all-task-roots)
  (let ((results nil))
    (maphash (lambda (depot root)
               (dolist (item (%find-unlinked-bidirectional-refs root))
                 (let ((from (getf item :from))
                       (to (getf item :to))
                       (reason (getf item :reason))
                       (suggested (getf item :suggested-type)))
                   (push (list :from (qualify-task-id depot from)
                               :to (qualify-task-id depot to)
                               :reason reason
                               :suggested-type suggested)
                         results))))
             *depot-tasks-roots*)
    (nreverse results)))

(defun find-missing-markov-edges ()
  "Find task pairs with repeated off-edge session transitions (Markov analysis).
   Returns list of plists (:from :to :count) sorted by count descending."
  (handler-case
      (let ((all-tasks (cached-all-tasks)))
        (when (plusp (hash-table-count all-tasks))
          (find-missing-edges all-tasks)))
    (error () nil)))

(defun find-unorganized-tasks (&key (min-events 20) (critical-memory 5))
  "Find tasks with many events but below the organization threshold.
   These are active tasks that could benefit from more observations.
   Returns list of plists (:id :events :obs-count :deficit)."
  (handler-case
      (let ((all-tasks (cached-all-tasks :min-events min-events))
            (results nil))
        (maphash (lambda (name data)
                   (let* ((events (getf data :events))
                          (state (getf data :state))
                          (org (organization-indicator events state
                                                      :critical-memory critical-memory)))
                     (unless (getf org :organized)
                       (push (list :id name
                                   :events (length events)
                                   :obs-count (getf org :obs-count)
                                   :deficit (getf org :deficit))
                             results))))
                 all-tasks)
        (sort results #'> :key (lambda (r) (getf r :events))))
    (error () nil)))

(defun task-health-data ()
  "Return structured health data across all depots.
   Keys: :stale-forks, :dead-ends, :unlinked-roots, :stale-claims,
         :unexplored-frontier, :convergent-clusters, :premature-completions,
         :suggested-edges, :missing-edges, :unorganized-tasks.
   All IDs are qualified with depot prefix."
  (list
   (cons :stale-forks (find-stale-tasks))
   (cons :dead-ends (find-dead-ends))
   (cons :unlinked-roots (find-declared-orphans))
   (cons :stale-claims (find-stale-claims))
   (cons :unexplored-frontier (find-unexplored-frontier))
   (cons :convergent-clusters (find-convergent-clusters))
   (cons :premature-completions (find-premature-completions))
   (cons :suggested-edges (find-unlinked-bidirectional-refs))
   (cons :missing-edges (find-missing-markov-edges))
   (cons :unorganized-tasks (find-unorganized-tasks))))

(defun format-health-report (data &optional (stream nil))
  "Format health data alist as human-readable report.
   DATA should be from TASK-HEALTH-DATA.
   Returns string if STREAM is nil, otherwise writes to STREAM."
  (format stream "Task Health Report~%~%")
  (format stream "Stale forks (~D): tasks created >3 days ago with <=1 observation~%"
          (length (cdr (assoc :stale-forks data))))
  (dolist (id (cdr (assoc :stale-forks data)))
    (format stream "  - ~A~%" id))
  (format stream "~%Dead ends (~D): active tasks with no events for >7 days~%"
          (length (cdr (assoc :dead-ends data))))
  (dolist (id (cdr (assoc :dead-ends data)))
    (format stream "  - ~A~%" id))
  (format stream "~%Unlinked roots (~D): active tasks with no incoming edges~%"
          (length (cdr (assoc :unlinked-roots data))))
  (dolist (id (cdr (assoc :unlinked-roots data)))
    (format stream "  - ~A~%" id))
  (format stream "~%Stale claims (~D): claimed but no activity for >4 hours~%"
          (length (cdr (assoc :stale-claims data))))
  (dolist (id (cdr (assoc :stale-claims data)))
    (format stream "  - ~A~%" id))
  (format stream "~%Unexplored frontier (~D): active, unclaimed tasks~%"
          (length (cdr (assoc :unexplored-frontier data))))
  (dolist (id (cdr (assoc :unexplored-frontier data)))
    (format stream "  - ~A~%" id))
  (format stream "~%Convergent clusters (~D): topic groups with unlinked active tasks~%"
          (length (cdr (assoc :convergent-clusters data))))
  (dolist (cluster (cdr (assoc :convergent-clusters data)))
    (format stream "  - ~A (~D unlinked pairs): ~{~A~^, ~}~%"
            (first cluster) (third cluster) (second cluster)))
  (format stream "~%Premature completions (~D): completed tasks with incomplete children~%"
          (length (cdr (assoc :premature-completions data))))
  (dolist (p (cdr (assoc :premature-completions data)))
    (format stream "  - ~A (~D incomplete): ~{~A~^, ~}~%"
            (getf p :id) (getf p :incomplete-count) (getf p :children)))
  (format stream "~%Suggested edges (~D): bidirectional references without declared edges~%"
          (length (cdr (assoc :suggested-edges data))))
  (dolist (s (cdr (assoc :suggested-edges data)))
    (format stream "  - ~A ↔ ~A (suggest: ~A)~%"
            (getf s :from) (getf s :to) (getf s :suggested-type)))
  ;; Markov-derived health checks
  (let ((missing (cdr (assoc :missing-edges data))))
    (format stream "~%Missing edges (~D): task pairs with repeated off-edge session transitions~%"
            (length missing))
    (dolist (m (subseq missing 0 (min 10 (length missing))))
      (format stream "  - ~A → ~A (~D transitions)~%"
              (getf m :from) (getf m :to) (getf m :count))))
  (let ((unorg (cdr (assoc :unorganized-tasks data))))
    (format stream "~%Unorganized tasks (~D): >20 events but below observation threshold~%"
            (length unorg))
    (dolist (u (subseq unorg 0 (min 10 (length unorg))))
      (format stream "  - ~A (~D events, ~D obs, needs ~D more)~%"
              (getf u :id) (getf u :events) (getf u :obs-count)
              (max 0 (getf u :deficit))))))

(defun task-health-report (&key raw)
  "Combined health report across all depots: stale forks, dead ends,
   unlinked roots, stale claims, unexplored frontier, convergent clusters,
   premature completions.
   With :RAW t, returns structured alist for programmatic use.
   Without :RAW (default), returns formatted string."
  (let ((data (task-health-data)))
    (if raw
        data
        (with-output-to-string (s)
          (format-health-report data s)))))

;;; ============================================================
;;; ENRICHED QUERY (Cross-graph naturality — Theorem 9)
;;; The enriched functor: own topic + 1-hop reference neighbor
;;; topics. Achieves 0% zero-overlap on reference edges (vs 80.2%
;;; for basic functor). Necessary and sufficient for naturality.
;;; ============================================================

(defun compute-enriched-query (task-id &optional graph)
  "Compute enriched playbook query for TASK-ID.
   Combines own topic with 1-hop reference neighbor topics and tags.
   Returns a space-separated string suitable for playbook_activate query.
   Mathematical foundation: Theorem 9 — enriched functor achieves
   0% zero-overlap on cross-topic reference edges."
  (let* ((g (or graph (build-task-graph)))
         (nodes (task-graph-nodes g))
         (forward (task-graph-forward g))
         (reverse-g (task-graph-reverse g))
         (props (gethash task-id nodes))
         (own-topic (getf props :topic))
         (topics nil))
    ;; Start with own topic
    (when own-topic
      (push own-topic topics))
    ;; Add reference neighbor topics (forward)
    (dolist (e (gethash task-id forward))
      (when (eq (second e) :reference)
        (let ((tp (getf (gethash (first e) nodes) :topic)))
          (when tp (pushnew tp topics :test #'string=)))))
    ;; Add reference neighbor topics (reverse)
    (dolist (e (gethash task-id reverse-g))
      (when (eq (second e) :reference)
        (let ((tp (getf (gethash (first e) nodes) :topic)))
          (when tp (pushnew tp topics :test #'string=)))))
    ;; Also include tags from event-sourced metadata if available
    (handler-case
        (let* ((events-path (task-events-path task-id))
               (tags (when (probe-file events-path)
                       (let* ((log (elog-load events-path))
                              (events (reverse (event-log-events log)))
                              (state (when events (compute-state events))))
                         (when state
                           (crdt:lwwm-get (task-state-metadata state) "tags"))))))
          (when (and tags (> (length tags) 0))
            (dolist (tag (uiop:split-string tags :separator ","))
              (let ((trimmed (string-trim " " tag)))
                (when (> (length trimmed) 0)
                  (pushnew trimmed topics :test #'string=))))))
      (error () nil))
    (format nil "~{~A~^ ~}" (nreverse topics))))

;;; ============================================================
;;; CYCLE DETECTION AND SCC
;;; ============================================================

(defun has-cycle-p (graph &optional (layer :reference))
  "Check if the graph has a cycle in the specified edge layer."
  (let ((forward (task-graph-forward graph))
        (color (make-hash-table :test 'equal))
        (found-cycle nil))
    (labels ((dfs (node)
               (setf (gethash node color) :gray)
               (dolist (e (gethash node forward))
                 (when (and (not found-cycle)
                            (eq (second e) layer))
                   (let ((tgt (first e)))
                     (case (gethash tgt color :white)
                       (:gray (setf found-cycle t))
                       (:white (dfs tgt))))))
               (setf (gethash node color) :black)))
      (maphash (lambda (src edges)
                 (declare (ignore edges))
                 (when (and (not found-cycle)
                            (eq (gethash src color :white) :white))
                   (dfs src)))
               forward))
    found-cycle))

(defun find-cycles (graph &optional (layer :reference))
  "Find all cycle-participating nodes in the specified edge layer."
  (let ((forward (task-graph-forward graph))
        (color (make-hash-table :test 'equal))
        (cycle-nodes nil)
        (path nil))
    (labels ((dfs (node)
               (push node path)
               (setf (gethash node color) :gray)
               (dolist (e (gethash node forward))
                 (when (eq (second e) layer)
                   (let ((tgt (first e)))
                     (case (gethash tgt color :white)
                       (:gray
                        (let ((cycle-start (member tgt path :test #'equal)))
                          (dolist (cn cycle-start)
                            (pushnew cn cycle-nodes :test #'equal))))
                       (:white (dfs tgt))))))
               (setf (gethash node color) :black)
               (pop path)))
      (maphash (lambda (src edges)
                 (declare (ignore edges))
                 (when (eq (gethash src color :white) :white)
                   (dfs src)))
               forward))
    cycle-nodes))

(defun tarjan-scc (graph &optional (layer :reference))
  "Compute strongly connected components using Tarjan's algorithm."
  (let ((forward (task-graph-forward graph))
        (index-counter 0)
        (index-of (make-hash-table :test 'equal))
        (lowlink (make-hash-table :test 'equal))
        (on-stack (make-hash-table :test 'equal))
        (stack nil)
        (sccs nil))
    (labels ((strongconnect (v)
               (setf (gethash v index-of) index-counter
                     (gethash v lowlink) index-counter)
               (incf index-counter)
               (push v stack)
               (setf (gethash v on-stack) t)
               (dolist (e (gethash v forward))
                 (when (eq (second e) layer)
                   (let ((w (first e)))
                     (cond
                       ((not (gethash w index-of nil))
                        (strongconnect w)
                        (setf (gethash v lowlink)
                              (min (gethash v lowlink) (gethash w lowlink))))
                       ((gethash w on-stack)
                        (setf (gethash v lowlink)
                              (min (gethash v lowlink) (gethash w index-of))))))))
               (when (= (gethash v lowlink) (gethash v index-of))
                 (let ((scc nil))
                   (loop (let ((w (pop stack)))
                           (setf (gethash w on-stack) nil)
                           (push w scc)
                           (when (equal w v) (return))))
                   (push scc sccs)))))
      (maphash (lambda (src edges)
                 (declare (ignore edges))
                 (unless (gethash src index-of nil)
                   (strongconnect src)))
               forward))
    sccs))

(defun condense (graph &optional (layer :reference))
  "Build the condensation graph — quotient by SCCs.
   Each SCC becomes a single node. The result is always a DAG."
  (let* ((sccs (tarjan-scc graph layer))
         (node-to-scc (make-hash-table :test 'equal))
         (scc-members (make-hash-table))
         (forward (task-graph-forward graph))
         (condensed-edges (make-hash-table))
         (condensed-count (length sccs)))
    (loop for scc in sccs
          for idx from 0
          do (setf (gethash idx scc-members) scc)
             (dolist (node scc)
               (setf (gethash node node-to-scc) idx)))
    (maphash (lambda (src edges)
               (let ((src-scc (gethash src node-to-scc)))
                 (when src-scc
                   (dolist (e edges)
                     (when (eq (second e) layer)
                       (let ((tgt-scc (gethash (first e) node-to-scc)))
                         (when (and tgt-scc (/= src-scc tgt-scc))
                           (pushnew tgt-scc
                                    (gethash src-scc condensed-edges)))))))))
             forward)
    (list :scc-count condensed-count
          :non-trivial-sccs
          (loop for idx being the hash-keys of scc-members
                  using (hash-value members)
                when (> (length members) 1)
                  collect (list :scc idx :size (length members) :members members))
          :condensed-edges
          (let ((edge-count 0))
            (maphash (lambda (k v) (declare (ignore k))
                       (incf edge-count (length v)))
                     condensed-edges)
            edge-count)
          :is-dag t)))

(defun topo-sort-condensation (graph &optional (layer :reference))
  "Topological sort of the condensation graph.
   Returns list of SCCs in dependency order (dependencies first)."
  (let* ((sccs (tarjan-scc graph layer))
         (n (length sccs))
         (node-to-scc (make-hash-table :test 'equal))
         (forward (task-graph-forward graph))
         (adj (make-array n :initial-element nil))
         (in-degree (make-array n :initial-element 0)))
    (loop for scc in sccs
          for idx from 0
          do (dolist (node scc)
               (setf (gethash node node-to-scc) idx)))
    (maphash (lambda (src edges)
               (let ((src-scc (gethash src node-to-scc)))
                 (when src-scc
                   (dolist (e edges)
                     (when (eq (second e) layer)
                       (let ((tgt-scc (gethash (first e) node-to-scc)))
                         (when (and tgt-scc (/= src-scc tgt-scc))
                           (pushnew tgt-scc (aref adj src-scc)))))))))
             forward)
    (loop for idx below n
          do (dolist (tgt (aref adj idx))
               (incf (aref in-degree tgt))))
    (let ((queue (loop for idx below n
                       when (= (aref in-degree idx) 0)
                         collect idx))
          (result nil))
      (loop while queue
            do (let ((node (pop queue)))
                 (push node result)
                 (dolist (tgt (aref adj node))
                   (decf (aref in-degree tgt))
                   (when (= (aref in-degree tgt) 0)
                     (push tgt queue)))))
      (values (mapcar (lambda (idx) (nth idx sccs))
                      (nreverse result))
              n))))

;;; ============================================================
;;; SUBGRAPH AND EGO-GRAPH
;;; ============================================================

(defun graph-subgraph (graph pred)
  "Extract subgraph containing only nodes satisfying PRED.
   Edges are kept iff both endpoints satisfy PRED."
  (let ((new-graph (make-task-graph))
        (old-nodes (task-graph-nodes graph))
        (old-forward (task-graph-forward graph)))
    (maphash (lambda (id props)
               (when (funcall pred id props)
                 (setf (gethash id (task-graph-nodes new-graph)) props)))
             old-nodes)
    (let ((new-nodes (task-graph-nodes new-graph)))
      (maphash (lambda (src edges)
                 (when (gethash src new-nodes)
                   (let ((kept (remove-if-not
                                (lambda (e) (gethash (first e) new-nodes))
                                edges)))
                     (when kept
                       (setf (gethash src (task-graph-forward new-graph)) kept)
                       (dolist (e kept)
                         (push (list src (second e) (third e))
                               (gethash (first e)
                                        (task-graph-reverse new-graph))))))))
               old-forward))
    new-graph))

(defun graph-ego (graph center-id &optional (hops 2))
  "Extract the N-hop ego graph around CENTER-ID.
   Returns subgraph of all nodes within HOPS distance (both directions)."
  (let ((visited (make-hash-table :test 'equal))
        (forward (task-graph-forward graph))
        (reverse-g (task-graph-reverse graph))
        (queue (list (cons center-id 0))))
    (setf (gethash center-id visited) t)
    (loop while queue do
      (let* ((item (pop queue))
             (node (car item))
             (dist (cdr item)))
        (when (< dist hops)
          (dolist (e (gethash node forward))
            (unless (gethash (first e) visited)
              (setf (gethash (first e) visited) t)
              (push (cons (first e) (1+ dist)) queue)))
          (dolist (e (gethash node reverse-g))
            (unless (gethash (first e) visited)
              (setf (gethash (first e) visited) t)
              (push (cons (first e) (1+ dist)) queue))))))
    (graph-subgraph graph (lambda (id props)
                            (declare (ignore props))
                            (gethash id visited)))))

;;; ============================================================
;;; TEMPORAL EDGES
;;; Timestamped reference graph is a DAG (handoff immutability).
;;; ============================================================

(defun extract-timestamped-reference-edges (task-infos tasks-root)
  "Extract reference edges WITH handoff timestamps.
   Returns plists (:time :from :to :handoff). Since handoffs are
   immutable, the resulting graph is a DAG."
  (let ((all-ids (mapcar (lambda (info) (getf info :id)) task-infos))
        (edges nil))
    (dolist (info task-infos)
      (let* ((id (getf info :id))
             (handoff-dir (format nil "~A~A/handoffs/" tasks-root id))
             (handoffs (when (uiop:directory-exists-p handoff-dir)
                         (directory (merge-pathnames "*.md"
                                                     (pathname handoff-dir))))))
        (dolist (hf handoffs)
          (let* ((fname (pathname-name hf))
                 (ts-str (when (>= (length fname) 19)
                           (subseq fname 0 19))))
            (handler-case
                (with-open-file (s hf)
                  (loop for line = (read-line s nil nil)
                        while line
                        do (dolist (other-id all-ids)
                             (when (and (not (equal other-id id))
                                        (search other-id line))
                               (pushnew (list :time ts-str :from id
                                              :to other-id :handoff fname)
                                        edges :test
                                        (lambda (a b)
                                          (and (equal (getf a :time) (getf b :time))
                                               (equal (getf a :from) (getf b :from))
                                               (equal (getf a :to) (getf b :to)))))))))
              (error () nil))))))
    (sort edges #'string< :key (lambda (e) (or (getf e :time) "")))))

(defun temporal-chain-edges (task-infos)
  "Alternative same-day layer: connect temporally adjacent tasks.
   Each task links to the next task created on the same day.
   O(n) edges per day instead of O(n^2)."
  (let ((by-date (make-hash-table :test 'equal)))
    (dolist (info task-infos)
      (let ((date (getf info :date)))
        (when date (push info (gethash date by-date)))))
    (let ((edges nil))
      (maphash (lambda (date members)
                 (declare (ignore date))
                 (when (>= (length members) 2)
                   (let ((sorted (sort (copy-list members) #'string<
                                       :key (lambda (info) (getf info :id)))))
                     (loop for (a b) on sorted
                           while b
                           do (push (list (getf a :id) (getf b :id) :temporal)
                                    edges)))))
               by-date)
      edges)))

;;; ============================================================
;;; REACHABILITY (temporal)
;;; ============================================================

(defun reachable-at-time (timestamped-edges task-id cutoff-time)
  "Tasks transitively reachable from TASK-ID following only edges
   with timestamp <= CUTOFF-TIME.
   Answers: 'What did task X know about at time T?'"
  (let ((adj (make-hash-table :test 'equal))
        (visited (make-hash-table :test 'equal))
        (result nil))
    (dolist (e timestamped-edges)
      (when (string<= (or (getf e :time) "") cutoff-time)
        (push (getf e :to) (gethash (getf e :from) adj))))
    (let ((queue (list task-id)))
      (setf (gethash task-id visited) t)
      (loop while queue
            do (let* ((node (pop queue))
                      (neighbors (gethash node adj)))
                 (dolist (n neighbors)
                   (unless (gethash n visited)
                     (setf (gethash n visited) t)
                     (push n result)
                     (push n queue))))))
    (nreverse result)))

(defun graph-at-time (timestamped-edges cutoff)
  "Build the reference graph as it existed at CUTOFF time.
   Returns plist with :nodes :edges :unique-pairs counts."
  (let ((filtered (remove-if (lambda (e)
                               (string> (or (getf e :time) "") cutoff))
                             timestamped-edges))
        (nodes (make-hash-table :test 'equal)))
    (dolist (e filtered)
      (setf (gethash (getf e :from) nodes) t)
      (setf (gethash (getf e :to) nodes) t))
    (list :nodes (hash-table-count nodes)
          :edges (length filtered)
          :unique-pairs
          (let ((pairs (make-hash-table :test 'equal)))
            (dolist (e filtered)
              (setf (gethash (cons (getf e :from) (getf e :to)) pairs) t))
            (hash-table-count pairs)))))

;;; ============================================================
;;; FORMATTING
;;; ============================================================

(defun format-graph-context (graph task-id)
  "Format the graph neighborhood of TASK-ID for task_get output.
   Shows: topic cluster, connected topics (1-hop reference neighbors),
   outgoing/incoming references, same-day and topic peer counts.
   Returns NIL for isolated nodes (no edges)."
  (let* ((nodes (task-graph-nodes graph))
         (forward (task-graph-forward graph))
         (reverse-g (task-graph-reverse graph))
         (fwd (gethash task-id forward))
         (rev (gethash task-id reverse-g))
         (props (gethash task-id nodes)))
    (when (or fwd rev)
      (let ((own-topic (getf props :topic))
            (ref-out nil)
            (ref-in nil)
            (neighbor-topics nil)
            (same-day-count 0)
            (topic-count 0))
        (dolist (e fwd)
          (case (second e)
            (:reference
             (push (first e) ref-out)
             (let ((tp (getf (gethash (first e) nodes) :topic)))
               (when tp (pushnew tp neighbor-topics :test #'string=))))
            (:same-day (incf same-day-count))
            (:topic (incf topic-count))))
        (dolist (e rev)
          (case (second e)
            (:reference
             (push (first e) ref-in)
             (let ((tp (getf (gethash (first e) nodes) :topic)))
               (when tp (pushnew tp neighbor-topics :test #'string=))))
            (:same-day (incf same-day-count))
            (:topic (incf topic-count))))
        (setf neighbor-topics
              (remove own-topic neighbor-topics :test #'string=))
        (with-output-to-string (s)
          (format s "Graph Context:~%")
          (when own-topic
            (format s "  Topic cluster: ~A~%" own-topic))
          (when neighbor-topics
            (format s "  Connected topics: ~{~A~^, ~}~%" neighbor-topics))
          (when ref-out
            (format s "  References (~D):~%" (length ref-out))
            (dolist (rid ref-out)
              (format s "    -> ~A~%" rid)))
          (when ref-in
            (format s "  Referenced by (~D):~%" (length ref-in))
            (dolist (rid ref-in)
              (format s "    <- ~A~%" rid)))
          (when (> same-day-count 0)
            (format s "  Same-day peers: ~D~%" same-day-count))
          (when (> topic-count 0)
            (format s "  Topic peers: ~D~%" topic-count)))))))

(defun format-graph-stats (graph)
  "Format global graph statistics."
  (let ((nodes (task-graph-nodes graph))
        (forward (task-graph-forward graph))
        (total 0)
        (layer-counts (make-hash-table :test 'equal))
        (max-out 0) (max-out-id nil)
        (max-in 0) (max-in-id nil)
        (reverse-g (task-graph-reverse graph)))
    (maphash (lambda (src edges)
               (let ((n (length edges)))
                 (when (> n max-out)
                   (setf max-out n max-out-id src))
                 (dolist (e edges)
                   (incf total)
                   (incf (gethash (second e) layer-counts 0)))))
             forward)
    (maphash (lambda (tgt edges)
               (let ((n (length edges)))
                 (when (> n max-in)
                   (setf max-in n max-in-id tgt))))
             reverse-g)
    (with-output-to-string (s)
      (format s "Graph: ~D nodes, ~D edges~%~%" (hash-table-count nodes) total)
      (format s "Edge layers:~%")
      (let ((pairs nil))
        (maphash (lambda (k v) (push (cons k v) pairs)) layer-counts)
        (dolist (p (sort pairs #'> :key #'cdr))
          (format s "  ~A: ~D~%" (car p) (cdr p))))
      (format s "~%Hub (out-degree): ~A (~D edges)~%" max-out-id max-out)
      (format s "Hub (in-degree): ~A (~D edges)~%" max-in-id max-in)
      (format s "~%Frontier: ~D tasks ready~%"
              (length (compute-frontier graph))))))

(defun format-task-neighbors (graph task-id)
  "Format direct neighbors of a task across all edge layers."
  (let ((forward (task-graph-forward graph))
        (reverse-g (task-graph-reverse graph)))
    (let ((outgoing (gethash task-id forward))
          (incoming (gethash task-id reverse-g)))
      (with-output-to-string (s)
        (format s "Neighbors of ~A:~%~%" task-id)
        (when outgoing
          (format s "Outgoing (~D):~%" (length outgoing))
          (dolist (e outgoing)
            (format s "  -> ~A [~A]~%" (first e) (second e))))
        (when incoming
          (format s "~%Incoming (~D):~%" (length incoming))
          (dolist (e incoming)
            (format s "  <- ~A [~A]~%" (first e) (second e))))
        (unless (or outgoing incoming)
          (format s "  (isolated node -- no edges)~%"))))))

(defun format-task-list-grouped (task-infos)
  "Format task list grouped by topic cluster (Galois connection quotient).
   Non-trivial clusters (size > 1) get headers with member count.
   Singletons listed in a compact section at the end.
   Sorted by cluster size (largest first)."
  (let ((by-topic (make-hash-table :test 'equal)))
    (dolist (info task-infos)
      (let ((topic (getf info :topic)))
        (when topic
          (push info (gethash topic by-topic)))))
    (let ((clusters nil)
          (singletons nil))
      (maphash (lambda (topic members)
                 (if (> (length members) 1)
                     (push (cons topic (nreverse members)) clusters)
                     (push (first members) singletons)))
               by-topic)
      (setf clusters (sort clusters #'> :key (lambda (c) (length (cdr c)))))
      (setf singletons (sort singletons #'string>
                              :key (lambda (info) (or (getf info :date) ""))))
      (with-output-to-string (s)
        (format s "Tasks (~D in ~D clusters):~%~%"
                (length task-infos) (hash-table-count by-topic))
        (dolist (cluster clusters)
          (let ((topic (car cluster))
                (members (cdr cluster)))
            (format s "== ~A (~D) ==~%" topic (length members))
            (dolist (info members)
              (let ((id (getf info :id))
                    (status (getf info :status))
                    (events-p (getf info :has-events)))
                (format s "  ~A~A~%"
                        id
                        (cond
                          ((and status (not (eq status :dormant)))
                           (format nil " (~(~A~))" status))
                          (events-p " (tracked)")
                          (t "")))))
            (format s "~%")))
        (when singletons
          (format s "== singletons (~D) ==~%" (length singletons))
          (dolist (info singletons)
            (format s "  ~A~%" (getf info :id))))))))

;;; ============================================================
;;; DETERMINISTIC COLOR GENERATION
;;; ============================================================

(defun djb2-hash (str)
  "DJB2 hash — good distribution, deterministic, fast."
  (let ((hash 5381))
    (loop for c across (string str)
          do (setf hash (logand (+ (* hash 33) (char-code c)) #xFFFFFFFF)))
    hash))

(defun string-to-hue (str)
  "Deterministic hue 0-360 from string via DJB2."
  (coerce (mod (djb2-hash str) 360) 'single-float))

(defun hsl-to-hex (h s l)
  "Convert HSL (h:0-360, s:0-1, l:0-1) to CSS hex color string."
  (let* ((c (* (- 1.0 (abs (- (* 2.0 l) 1.0))) s))
         (x (* c (- 1.0 (abs (- (mod (/ h 60.0) 2.0) 1.0)))))
         (m (- l (/ c 2.0))))
    (multiple-value-bind (r1 g1 b1)
        (cond ((< h 60)  (values c x 0.0))
              ((< h 120) (values x c 0.0))
              ((< h 180) (values 0.0 c x))
              ((< h 240) (values 0.0 x c))
              ((< h 300) (values x 0.0 c))
              (t         (values c 0.0 x)))
      (format nil "#~2,'0x~2,'0x~2,'0x"
              (round (* 255 (+ r1 m)))
              (round (* 255 (+ g1 m)))
              (round (* 255 (+ b1 m)))))))

(defun string-to-color (str &key (saturation 0.65) (lightness 0.6))
  "Deterministic CSS hex color from any string."
  (hsl-to-hex (string-to-hue str) saturation lightness))

(defun string-to-oklch (str &key (lightness 70) (chroma 0.15))
  "Deterministic OKLCH CSS color string. Perceptually uniform."
  (format nil "oklch(~D% ~,2F ~D)" lightness chroma (round (string-to-hue str))))

;;; ============================================================
;;; DASHBOARD RENDERING
;;; Rich event formatting and enriched task views for dashboard.
;;; ============================================================

(defparameter *event-type-display*
  '(("task.create"        :icon "+" :color "#58a6ff" :label "Created")
    ("session.join"       :icon ">" :color "#3fb950" :label "Session joined")
    ("session.team-join"  :icon "T" :color "#3fb950" :label "Teammate joined")
    ("observation"        :icon "o" :color "#d2a8ff" :label "Observed")
    ("artifact.create"    :icon "@" :color "#d29922" :label "Artifact")
    ("artifact.delete"    :icon "x" :color "#f85149" :label "Artifact removed")
    ("task.set-metadata"  :icon "#" :color "#79c0ff" :label "Metadata")
    ("task.update-status" :icon "*" :color "#3fb950" :label "Status")
    ("task.spawn"         :icon "^" :color "#a371f7" :label "Spawned")
    ("handoff"            :icon "~" :color "#8b949e" :label "Handoff")
    ("pattern.activate"   :icon "d" :color "#d2a8ff" :label "Pattern"))
  "Display properties for each event type.")

(defun event-display-info (type-string)
  "Get display info plist for an event type string.
   Returns (:icon ... :color ... :label ...)."
  (let ((entry (assoc type-string *event-type-display* :test #'string=)))
    (if entry
        (cdr entry)
        (list :icon "." :color "#8b949e" :label type-string))))

(defun universal-time-to-iso (ut)
  "Convert universal-time integer to ISO 8601 string."
  (multiple-value-bind (sec min hr day mon yr)
      (decode-universal-time ut 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" yr mon day hr min sec)))

(defun relative-time (ut)
  "Human-readable relative time from universal-time integer."
  (let* ((now (get-universal-time))
         (diff (- now ut)))
    (cond
      ((< diff 60) "just now")
      ((< diff 3600) (format nil "~Dm ago" (floor diff 60)))
      ((< diff 86400) (format nil "~Dh ago" (floor diff 3600)))
      (t (format nil "~Dd ago" (floor diff 86400))))))

(defun format-event-rich (event-alist)
  "Format a parsed event alist into a rich display plist.
   Input: an alist from (yason:parse line :object-as :alist).
   Output: plist with :type :icon :color :label :timestamp :iso
           :relative :session :content."
  (let* ((type-str (cdr (assoc "type" event-alist :test #'string=)))
         (ts (cdr (assoc "timestamp" event-alist :test #'string=)))
         (session (cdr (assoc "session" event-alist :test #'string=)))
         (data (cdr (assoc "data" event-alist :test #'string=)))
         (display (event-display-info type-str)))
    (list :type type-str
          :icon (getf display :icon)
          :color (getf display :color)
          :label (getf display :label)
          :timestamp ts
          :iso (when (numberp ts) (universal-time-to-iso ts))
          :relative (when (numberp ts) (relative-time ts))
          :session session
          :content (cond
                     ((string= type-str "observation")
                      (cdr (assoc "text" data :test #'string=)))
                     ((string= type-str "task.create")
                      (format nil "~A" (cdr (assoc "name" data :test #'string=))))
                     ((string= type-str "artifact.create")
                      (cdr (assoc "path" data :test #'string=)))
                     ((string= type-str "task.set-metadata")
                      (format nil "~A = ~A"
                              (cdr (assoc "key" data :test #'string=))
                              (cdr (assoc "value" data :test #'string=))))
                     ((string= type-str "task.update-status")
                      (cdr (assoc "status" data :test #'string=)))
                     ((string= type-str "task.spawn")
                      (cdr (assoc "child-id" data :test #'string=)))
                     (data (format nil "~A" data))
                     (t "")))))

(defun enriched-task-view (task-id &optional tasks-root)
  "Rich task view with recent observations, metadata, event log.
   Works for both event-sourced and legacy tasks.
   Returns a plist with :id :display-name :topic :color :source and
   either event-sourced fields or legacy filesystem fields.
   TASKS-ROOT is deprecated; task-directory handles qualified IDs."
  (declare (ignore tasks-root))
  (let* ((task-dir (task-directory task-id))
         (events-path (format nil "~Aevents.jsonl" task-dir))
         (has-events (probe-file events-path)))
    (if has-events
        ;; Event-sourced task: read events
        (handler-case
            (with-open-file (s events-path)
              (let* ((lines (loop for line = (read-line s nil nil)
                                  while line collect line))
                     (events (mapcar (lambda (l) (yason:parse l :object-as :alist)) lines))
                     (rich-events (mapcar #'format-event-rich events))
                     (observations (remove-if-not
                                    (lambda (ev) (string= (getf ev :type) "observation"))
                                    rich-events))
                     ;; Compute CRDT state from same lines (single I/O)
                     (structured-events
                      (loop for l in lines
                            for ev = (handler-case (json-string-to-event l)
                                       (error () nil))
                            when ev collect ev))
                     (computed-state (when structured-events
                                       (compute-state (reverse structured-events))))
                     (task-status (when computed-state
                                    (crdt:lww-value (task-state-status computed-state))))
                     (task-description (when computed-state
                                         (crdt:lww-value (task-state-description computed-state))))
                     (task-metadata (when computed-state
                                      (task-state-metadata computed-state)))
                     ;; Extract file paths from tool.call events (Edit/Write)
                     ;; Also includes legacy file.touch events for backward compat
                     (files-touched
                      (remove-duplicates
                       (loop for ev in events
                             for type = (cdr (assoc "type" ev :test #'string=))
                             for data-alist = (cdr (assoc "data" ev :test #'string=))
                             append
                             (cond
                               ;; New: tool.call with Edit/Write → extract args.file_path
                               ((and (string= type "tool.call")
                                     (member (cdr (assoc "tool" data-alist :test #'string=))
                                             '("Edit" "Write") :test #'string=))
                                (let* ((args (cdr (assoc "args" data-alist :test #'string=)))
                                       (path (when args (cdr (assoc "file_path" args :test #'string=)))))
                                  (when path (list path))))
                               ;; Legacy: file.touch → extract data.path
                               ((string= type "file.touch")
                                (let ((path (cdr (assoc "path" data-alist :test #'string=))))
                                  (when path (list path))))
                               (t nil)))
                       :test #'equal))
                     (sessions (remove-duplicates
                                (mapcar (lambda (ev) (getf ev :session)) rich-events)
                                :test #'equal)))
                (list :id task-id
                      :display-name (humanize-task-name task-id)
                      :topic (extract-topic task-id)
                      :color (string-to-color (extract-topic task-id))
                      :source :event-sourced
                      :events-count (length events)
                      :sessions sessions
                      :observations-count (length observations)
                      :recent-observations
                      (mapcar (lambda (obs)
                                (list :time (getf obs :relative)
                                      :text (getf obs :content)))
                              (reverse (last observations 5)))
                      :files-touched-count (length files-touched)
                      :files-touched files-touched
                      :event-log (reverse rich-events)
                      :status task-status
                      :description task-description
                      :metadata task-metadata)))
          (error () (list :id task-id :source :error)))
        ;; Legacy task: infer from filesystem
        (list :id task-id
              :display-name (humanize-task-name task-id)
              :topic (extract-topic task-id)
              :color (string-to-color (extract-topic task-id))
              :source :legacy
              :has-plan (when (probe-file (format nil "~Aplan.md" task-dir)) t)
              :has-handoffs (when (uiop:directory-exists-p
                                   (format nil "~Ahandoffs/" task-dir)) t)))))

;;; ============================================================
;;; PLAN-AS-GRAPH (Theorem 10: Structural Forest)
;;; Plan(T) = slice(TaskGraph, children(T))
;;; Plans are queries over the existing graph, not new types.
;;; ============================================================

(defun task-children (task-id)
  "Return child task IDs for TASK-ID from edges OR-Set.
   Only follows structural edge types (*child-bearing-edge-types*) that define
   the plan fibration. Lateral edges (depends-on, etc.) cross fibers.
   Bare (unqualified) child IDs are qualified using the parent's depot."
  (let ((events-path (task-events-path task-id)))
    (when (probe-file events-path)
      (handler-case
          (let* ((log (elog-load events-path))
                 (events (reverse (event-log-events log)))
                 (state (when events (compute-state events)))
                 (edges (when state (task-state-edges state)))
                 (parent-depot (when (qualified-id-p task-id)
                                 (parse-qualified-id task-id))))
            (when edges
              (let ((children nil))
                (dolist (type *child-bearing-edge-types*)
                  (dolist (target (edge-targets edges type))
                    (let ((qualified (if (and parent-depot
                                             (not (qualified-id-p target)))
                                        (qualify-task-id parent-depot target)
                                        target)))
                      (pushnew qualified children :test #'string=))))
                (nreverse children))))
        (error (e)
          (warn "task-children: error loading children for ~A: ~A" task-id e)
          nil)))))

(defun all-descendants (task-id &optional (visited (make-hash-table :test 'equal)))
  "Recursively collect all descendants of TASK-ID (forest traversal).
   VISITED hash-table prevents infinite loops from circular edges."
  (setf (gethash task-id visited) t)
  (let ((children (remove-if (lambda (id) (gethash id visited))
                             (task-children task-id))))
    (when children
      (append children
              (loop for child in children
                    append (or (all-descendants child visited) nil))))))

(defun load-child-states (children)
  "Load CRDT states for a list of child task IDs. Returns hash-table id->state.
   Only reads event logs for the given children, not the full task corpus."
  (let ((states (make-hash-table :test 'equal)))
    (dolist (id children states)
      (let ((events-path (task-events-path id)))
        (when (probe-file events-path)
          (handler-case
              (let* ((log (elog-load events-path))
                     (events (reverse (event-log-events log)))
                     (state (when events (compute-state events))))
                (when state
                  (setf (gethash id states) state)))
            (error () nil)))))))

(defun build-plan-graph (children states)
  "Build a plan-local graph from children and their pre-loaded states.
   Avoids scanning the entire task corpus — only processes the given children.
   Nodes get accurate CRDT status. Edges come from each child's edges OR-Set."
  (let ((graph (make-task-graph))
        (child-set (make-hash-table :test 'equal)))
    ;; Build lookup set
    (dolist (id children)
      (setf (gethash id child-set) t))
    ;; Add nodes with accurate status
    (dolist (id children)
      (let ((state (gethash id states)))
        (graph-add-node graph id
          :display-name (if state
                            (or (lwwm-get (task-state-metadata state) "display-name") id)
                            id)
          :topic (extract-topic id)
          :status (if state
                      (or (lww-value (task-state-status state)) "unknown")
                      "unknown"))))
    ;; Add intra-plan edges from each child's edges OR-Set
    (dolist (id children)
      (let ((state (gethash id states)))
        (when state
          (dolist (member (ors-members (task-state-edges state)))
            (let ((decoded (decode-edge member)))
              (when (and decoded (gethash (car decoded) child-set))
                (graph-add-edge graph id (car decoded) (cdr decoded))))))))
    graph))

(defun plan-subgraph (task-id)
  "Build the plan subgraph for TASK-ID directly from children's event logs.
   Returns NIL if TASK-ID has no children.
   O(children) not O(all-tasks) — reads only the relevant event logs."
  (let ((children (task-children task-id)))
    (when children
      (build-plan-graph children (load-child-states children)))))

(defun plan-frontier (task-id)
  "Compute frontier scoped to a plan: children whose declared-dep are all completed.
   Returns NIL if TASK-ID has no children."
  (let ((sub (plan-subgraph task-id)))
    (when sub
      (compute-frontier sub))))

(defun format-plan (task-id)
  "Render a task's plan as structured text showing phases, status, and dependencies."
  (let ((children (task-children task-id)))
    (unless children
      (return-from format-plan
        (format nil "Task ~A has no plan (no children spawned).~%" task-id)))
    (let* ((states (load-child-states children))
           (graph (build-plan-graph children states))
           (frontier (compute-frontier graph))
           (frontier-set (make-hash-table :test 'equal)))
      (dolist (f frontier) (setf (gethash f frontier-set) t))
      (with-output-to-string (s)
        (format s "Plan for ~A (~D phases):~%~%" task-id (length children))
        ;; Show each child — state already loaded, no re-reading
        (let ((done-count 0))
          (dolist (child-id children)
            (let* ((state (gethash child-id states))
                   (status (if state
                               (or (lww-value (task-state-status state)) "unknown")
                               "unknown"))
                   (display-name
                     (if state
                         (let ((dn (lwwm-get (task-state-metadata state) "display-name")))
                           (if (and dn (> (length dn) 0)) dn child-id))
                         child-id))
                   (deps (when state
                           (let ((dj (lwwm-get (task-state-metadata state) "depends-on")))
                             (when dj (try-parse-json-array dj))))))
              (when (string= status "completed") (incf done-count))
              (format s "  ~A ~A  ~A~%"
                      (cond ((string= status "completed") "[done]")
                            ((gethash child-id frontier-set) "[READY]")
                            ((string= status "active") "[active]")
                            (t (format nil "[~A]" status)))
                      display-name
                      (if deps
                          (format nil "(depends: ~{~A~^, ~})" deps)
                          ""))))
          (format s "~%Progress: ~D/~D phases done, ~D ready~%"
                  done-count (length children) (length frontier)))))))

;;; ============================================================
;;; PLAN TIER 3: Generated Artifacts + Self-Attention
;;; Graph is source of truth → markdown/JSON are read projections.
;;; Self-attention: phase task_get auto-injects plan context.
;;; ============================================================

(defun stable-topo-sort (children states)
  "Sort children so dependencies come before dependents.
   Uses Kahn's algorithm with edges from OR-Set.
   Falls back to input order for disconnected nodes."
  (let ((in-degree (make-hash-table :test 'equal))
        (deps-of (make-hash-table :test 'equal))
        (child-set (make-hash-table :test 'equal)))
    (dolist (c children)
      (setf (gethash c child-set) t)
      (setf (gethash c in-degree) 0))
    ;; Build dependency graph from edges OR-Set
    (dolist (c children)
      (let ((state (gethash c states)))
        (when state
          (dolist (member (ors-members (task-state-edges state)))
            (let ((decoded (decode-edge member)))
              (when (and decoded (gethash (car decoded) child-set))
                ;; c has an edge to (car decoded) — c depends on it
                (push c (gethash (car decoded) deps-of))
                (incf (gethash c in-degree))))))))
    (let ((queue (remove-if-not (lambda (c) (zerop (gethash c in-degree))) children))
          (result nil))
      (loop while queue do
        (let ((node (pop queue)))
          (push node result)
          (dolist (dependent (gethash node deps-of))
            (decf (gethash dependent in-degree))
            (when (zerop (gethash dependent in-degree))
              (push dependent queue)))))
      (nreverse result))))

(defparameter *phase-metadata-render-order*
  '("objective" "acceptance" "steps" "context" "constraints")
  "Metadata keys to render in plan output, in display order.
   Keys like 'await' and 'ephemeral' are control flags, not rendered.")

(defun unescape-metadata-value (val)
  "Replace literal two-character \\n sequences with actual newlines.
   TQ's safe-read-query preserves \\n as literal characters in strings."
  (with-output-to-string (s)
    (loop with i = 0
          while (< i (length val))
          do (if (and (< (1+ i) (length val))
                      (char= (char val i) #\\)
                      (char= (char val (1+ i)) #\n))
                 (progn (write-char #\Newline s)
                        (incf i 2))
                 (progn (write-char (char val i) s)
                        (incf i))))))

(defun render-phase-metadata (meta stream)
  "Render phase metadata keys from LWW-Map META to STREAM as markdown.
   Only renders keys in *phase-metadata-render-order* that have values."
  (dolist (key *phase-metadata-render-order*)
    (let ((val (lwwm-get meta key)))
      (when (and val (> (length val) 0))
        (format stream "~%**~:(~A~)**: ~A~%" key (unescape-metadata-value val))))))

(defun generate-plan-markdown (task-id)
  "Generate a plan.md-style markdown document from the plan graph.
   The graph is the source of truth; this is a read projection.
   Returns NIL if the task has no children."
  (let ((children (task-children task-id)))
    (unless children (return-from generate-plan-markdown nil))
    (let* ((states (load-child-states children))
           (graph (build-plan-graph children states))
           (frontier (compute-frontier graph))
           (frontier-set (make-hash-table :test 'equal))
           (sorted (stable-topo-sort children states)))
      (dolist (f frontier) (setf (gethash f frontier-set) t))
      (with-output-to-string (s)
        (format s "# Plan: ~A~%~%" (humanize-task-name task-id))
        (format s "| Phase | Status | Dependencies |~%")
        (format s "|-------|--------|--------------|~%")
        (let ((done-count 0))
          (dolist (id sorted)
            (let* ((state (gethash id states))
                   (status (if state (or (lww-value (task-state-status state)) "unknown") "unknown"))
                   (display (if state (or (lwwm-get (task-state-metadata state) "display-name") id) id))
                   (deps (mapcar #'first
                           (remove-if-not (lambda (e) (eq (second e) :depends-on))
                                          (gethash id (task-graph-forward graph))))))
              (when (string= status "completed") (incf done-count))
              (format s "| ~A | ~A | ~A |~%"
                      display
                      (cond ((string= status "completed") "done")
                            ((gethash id frontier-set) "**READY**")
                            (t status))
                      (if deps (format nil "~{~A~^, ~}" deps) ""))))
          (format s "~%**Progress**: ~D/~D phases~%~%---~%~%" done-count (length children)))
        (dolist (id sorted)
          (let* ((state (gethash id states))
                 (display (if state (or (lwwm-get (task-state-metadata state) "display-name") id) id))
                 (desc (when state (lww-value (task-state-description state))))
                 (status (if state (or (lww-value (task-state-status state)) "unknown") "unknown"))
                 (meta (when state (task-state-metadata state))))
            (format s "## ~A ~A~%~%~A~%"
                    display
                    (if (string= status "completed") "✓" "")
                    (or desc ""))
            ;; Render phase metadata if present
            (when meta
              (render-phase-metadata meta s))
            (format s "~%")))))))

(defun extract-parent (task-id)
  "Extract parent task ID from the task.create event, if any.
   Returns NIL for top-level tasks."
  (let ((path (task-events-path task-id)))
    (when (probe-file path)
      (handler-case
          (let* ((log (elog-load path))
                 (events (reverse (event-log-events log)))
                 (create-event (find :task.create events :key #'event-type)))
            (when create-event
              (getf (event-data create-event) :parent)))
        (error () nil)))))

(defun plan-context (phase-id parent-id)
  "Compute contextual information for working on PHASE-ID within PARENT-ID's plan.
   Returns a plist of context sources: progress, frontier, dep observations, parent observations."
  (let* ((children (task-children parent-id))
         (states (load-child-states children))
         (phase-state (gethash phase-id states))
         (parent-path (task-events-path parent-id))
         (parent-state (when (probe-file parent-path)
                         (handler-case
                           (let* ((log (elog-load parent-path))
                                  (evts (reverse (event-log-events log))))
                             (when evts (compute-state evts)))
                           (error () nil))))
         (parent-obs (when parent-state
                       (gs-members (task-state-observations parent-state))))
         (dep-json (when phase-state
                     (lwwm-get (task-state-metadata phase-state) "depends-on")))
         (dep-ids (when dep-json (try-parse-json-array dep-json)))
         (dep-observations nil)
         (graph (build-plan-graph children states))
         (frontier (compute-frontier graph))
         (done-count (count-if (lambda (c)
                                 (let ((s (gethash c states)))
                                   (when s (string= (lww-value (task-state-status s))
                                                    "completed"))))
                               children)))
    (dolist (dep-id dep-ids)
      (let ((dep-state (gethash dep-id states)))
        (when dep-state
          (let ((obs (gs-members (task-state-observations dep-state))))
            (when obs
              (push (list :phase dep-id :observations obs) dep-observations))))))
    (list :phase phase-id
          :parent parent-id
          :plan-progress (format nil "~D/~D phases done, ~D ready"
                                 done-count (length children) (length frontier))
          :frontier frontier
          :parent-observations-count (length parent-obs)
          :parent-observations-sample (subseq parent-obs 0 (min 3 (length parent-obs)))
          :dep-observations (nreverse dep-observations)
          :phase-metadata (when phase-state
                            (let ((meta (task-state-metadata phase-state))
                                  (result nil))
                              (dolist (key *phase-metadata-render-order*)
                                (let ((val (lwwm-get meta key)))
                                  (when (and val (> (length val) 0))
                                    (push (cons key val) result))))
                              (nreverse result))))))

(defun format-plan-context (phase-id parent-id)
  "Format plan context as text for Claude's consumption.
   Concise summary of what's relevant when starting work on a phase."
  (let ((ctx (plan-context phase-id parent-id)))
    (with-output-to-string (s)
      (format s "~%## Plan Context: ~A~%~%" phase-id)
      ;; Phase metadata (objective, acceptance, etc.)
      (let ((phase-meta (getf ctx :phase-metadata)))
        (when phase-meta
          (dolist (pair phase-meta)
            (format s "**~:(~A~)**: ~A~%" (car pair) (unescape-metadata-value (cdr pair))))))
      (format s "**Progress**: ~A~%" (getf ctx :plan-progress))
      (let ((frontier (getf ctx :frontier)))
        (when frontier
          (format s "**Ready next**: ~{~A~^, ~}~%" frontier)))
      (let ((dep-obs (getf ctx :dep-observations)))
        (when dep-obs
          (format s "~%### From Dependencies~%~%")
          (dolist (dep dep-obs)
            (format s "**~A** (~D observations):~%"
                    (getf dep :phase) (length (getf dep :observations)))
            (dolist (o (subseq (getf dep :observations) 0
                               (min 3 (length (getf dep :observations)))))
              (format s "- ~A~%" (subseq o 0 (min 200 (length o))))))))
      (let ((parent-obs (getf ctx :parent-observations-sample)))
        (when parent-obs
          (format s "~%### Parent Task Observations (~D total)~%~%"
                  (getf ctx :parent-observations-count))
          (dolist (o parent-obs)
            (format s "- ~A~%~%" (subseq o 0 (min 200 (length o))))))))))

(defun auto-plan-context (task-id)
  "If TASK-ID is a phase (has a parent), return formatted plan context.
   Returns NIL for top-level tasks. Self-attention hook for task_get."
  (let ((parent (extract-parent task-id)))
    (when parent
      (format-plan-context task-id parent))))

(defun plan-to-json (task-id)
  "Convert plan graph to JSON for dashboard D3 rendering.
   Nodes are phases with status/position. Edges are dependencies."
  (let ((children (task-children task-id)))
    (unless children (return-from plan-to-json "{}"))
    (let* ((states (load-child-states children))
           (graph (build-plan-graph children states))
           (frontier (compute-frontier graph))
           (frontier-set (make-hash-table :test 'equal))
           (sorted (stable-topo-sort children states)))
      (dolist (f frontier) (setf (gethash f frontier-set) t))
      (with-output-to-string (s)
        (format s "{\"task\":~S,\"phases\":[" task-id)
        (let ((first-node t))
          (dolist (id sorted)
            (let* ((state (gethash id states))
                   (status (if state (or (lww-value (task-state-status state)) "unknown") "unknown"))
                   (display (if state (or (lwwm-get (task-state-metadata state) "display-name") id) id))
                   (deps (mapcar #'first
                           (remove-if-not (lambda (e) (eq (second e) :depends-on))
                                          (gethash id (task-graph-forward graph)))))
                   (obs-count (if state (length (gs-members (task-state-observations state))) 0)))
              (unless first-node (format s ","))
              (setf first-node nil)
              (format s "{\"id\":~S,\"display\":~S,\"status\":~S,\"ready\":~A,\"deps\":[~{~S~^,~}],\"observations\":~D}"
                      id display status
                      (if (gethash id frontier-set) "true" "false")
                      (or deps nil)
                      obs-count))))
        (format s "],\"progress\":{\"done\":~D,\"total\":~D,\"ready\":~D}}"
                (count-if (lambda (c)
                            (let ((s (gethash c states)))
                              (when s (string= (lww-value (task-state-status s)) "completed"))))
                          children)
                (length children)
                (length frontier))))))

(defun extract-phase-details (child-id events state frontier-set)
  "Extract rich details for a plan phase from its events and CRDT state.
   Returns a plist with all data needed for the rich plan view.
   Reads event log once, extracts both CRDT-derived and raw event data."
  (let ((description nil)
        (observations nil)
        (artifacts nil)
        (edges nil)
        (goals nil)
        (sessions (make-hash-table :test 'equal))
        (last-activity 0)
        (status "unknown")
        (display (humanize-task-name child-id))
        (deps nil))
    ;; Extract from raw events
    (dolist (ev events)
      (let ((ts (event-timestamp ev))
            (typ (event-type ev))
            (data (event-data ev)))
        (when (> ts last-activity) (setf last-activity ts))
        (case typ
          (:task.create
           (setf description (getf data :description)))
          (:observation
           (push (list :text (getf data :text) :timestamp ts
                       :session (event-session ev))
                 observations))
          (:session.join
           (setf (gethash (event-session ev) sessions) t))
          (:session.team-join
           (setf (gethash (event-session ev) sessions) t)))))
    ;; Extract from CRDT state
    (when state
      (setf status (or (lww-value (task-state-status state)) "unknown"))
      (let ((meta (task-state-metadata state)))
        (setf display (or (lwwm-get meta "display-name")
                          (humanize-task-name child-id)))
        (let ((goals-json (lwwm-get meta "goals")))
          (when goals-json (setf goals (try-parse-json-array goals-json))))
        (let ((dep-json (lwwm-get meta "depends-on")))
          (when dep-json (setf deps (try-parse-json-array dep-json)))))
      ;; Deps from edges OR-Set (authoritative, overrides metadata)
      (let ((edge-deps nil))
        (dolist (member (ors-members (task-state-edges state)))
          (let ((decoded (decode-edge member)))
            (when (and decoded (eq (cdr decoded) :depends-on))
              (pushnew (car decoded) edge-deps :test #'string=))))
        (when edge-deps (setf deps edge-deps)))
      ;; Artifacts from OR-Set
      (maphash (lambda (k v) (declare (ignore v)) (push k artifacts))
               (slot-value (task-state-artifacts state) 'crdt::elements))
      ;; Edges from OR-Set
      (dolist (member (ors-members (task-state-edges state)))
        (let ((decoded (decode-edge member)))
          (when decoded
            (push (list :target (car decoded)
                        :type (string-downcase (symbol-name (cdr decoded))))
                  edges)))))
    (list :id child-id
          :display display
          :status status
          :ready (if (gethash child-id frontier-set) t nil)
          :description (or description "")
          :observations (nreverse observations)
          :artifacts (nreverse artifacts)
          :edges (nreverse edges)
          :goals (or goals nil)
          :deps (or deps nil)
          :sessions (hash-table-count sessions)
          :last-activity last-activity)))

(defun json-string (str)
  "Encode STR as a JSON string with proper escaping.
   Uses yason for correct handling of newlines, quotes, etc."
  (with-output-to-string (s)
    (yason:encode (or str "") s)))

(defun plan-to-rich-json (task-id)
  "Convert plan to rich JSON with full phase details for dashboard.
   Unlike plan-to-json, includes descriptions, observation text,
   artifacts, edges, goals, session counts, and last activity.
   O(children) — reads each child's event log once."
  (let ((children (task-children task-id)))
    (unless children (return-from plan-to-rich-json "{}"))
    (let* ((states (load-child-states children))
           (graph (build-plan-graph children states))
           (frontier (compute-frontier graph))
           (frontier-set (make-hash-table :test 'equal))
           (sorted (stable-topo-sort children states)))
      (dolist (f frontier) (setf (gethash f frontier-set) t))
      (with-output-to-string (s)
        (format s "{\"task\":~A,\"phases\":[" (json-string task-id))
        (let ((first-node t))
          (dolist (id sorted)
            (let* ((events-path (task-events-path id))
                   (events (when (probe-file events-path)
                             (handler-case
                               (reverse (event-log-events (elog-load events-path)))
                               (error () nil))))
                   (state (gethash id states))
                   (details (extract-phase-details id (or events nil) state frontier-set)))
              (unless first-node (format s ","))
              (setf first-node nil)
              (format s "{\"id\":~A,\"display\":~A,\"status\":~A,\"ready\":~A"
                      (json-string (getf details :id))
                      (json-string (getf details :display))
                      (json-string (getf details :status))
                      (if (getf details :ready) "true" "false"))
              (format s ",\"description\":~A" (json-string (getf details :description)))
              ;; Observations array
              (format s ",\"observations\":[")
              (let ((first-obs t))
                (dolist (obs (getf details :observations))
                  (unless first-obs (format s ","))
                  (setf first-obs nil)
                  (format s "{\"text\":~A,\"timestamp\":~D,\"session\":~A}"
                          (json-string (getf obs :text))
                          (getf obs :timestamp)
                          (json-string (or (getf obs :session) "")))))
              (format s "]")
              ;; Artifacts array
              (format s ",\"artifacts\":[")
              (let ((first-art t))
                (dolist (art (getf details :artifacts))
                  (unless first-art (format s ","))
                  (setf first-art nil)
                  (format s "~A" (json-string art))))
              (format s "]")
              ;; Edges array
              (format s ",\"edges\":[")
              (let ((first-edge t))
                (dolist (edge (getf details :edges))
                  (unless first-edge (format s ","))
                  (setf first-edge nil)
                  (format s "{\"target\":~A,\"type\":~A}"
                          (json-string (getf edge :target))
                          (json-string (getf edge :type)))))
              (format s "]")
              ;; Deps, goals
              (format s ",\"deps\":[")
              (let ((first-dep t))
                (dolist (dep (or (getf details :deps) nil))
                  (unless first-dep (format s ","))
                  (setf first-dep nil)
                  (format s "~A" (json-string dep))))
              (format s "]")
              (format s ",\"goals\":[")
              (let ((first-goal t))
                (dolist (goal (or (getf details :goals) nil))
                  (unless first-goal (format s ","))
                  (setf first-goal nil)
                  (format s "~A" (json-string goal))))
              (format s "]")
              (format s ",\"sessions\":~D" (getf details :sessions))
              (format s ",\"lastActivity\":~D}" (getf details :last-activity)))))
        (format s "],\"progress\":{\"done\":~D,\"total\":~D,\"ready\":~D}}"
                (count-if (lambda (c)
                            (let ((s (gethash c states)))
                              (when s (string= (lww-value (task-state-status s)) "completed"))))
                          children)
                (length children)
                (length frontier))))))

(defun dashboard-emit-event (task-id event-type data)
  "Emit an event to a task's event log from the dashboard.
   Session is tagged 'dashboard' to distinguish from Claude sessions."
  (let* ((events-path (task-events-path task-id))
         (event (make-event
                 :id (format nil "dash-~A" (get-universal-time))
                 :timestamp (get-universal-time)
                 :session "dashboard"
                 :type event-type
                 :data data)))
    (elog-append-event events-path event)
    event))

;;; ============================================================
;;; DASHBOARD JSON PROJECTION
;;; Graph → JSON for D3 visualization. Read projection with
;;; edge layer metadata for visual differentiation.
;;; ============================================================

(defun graph-to-dashboard-json (graph task-infos)
  "Convert task graph + infos to JSON for D3 dashboard rendering.
   Nodes carry: id, label, topic, color, date, status.
   Edges carry: source, target, layer (reference/topic/same-day/declared-dep).
   Also computes cluster summaries and date range."
  (let ((nodes-json nil)
        (edges-json nil)
        (info-map (make-hash-table :test 'equal))
        (cluster-counts (make-hash-table :test 'equal))
        (dates nil))
    ;; Build info lookup
    (dolist (info task-infos)
      (setf (gethash (getf info :id) info-map) info))
    ;; Nodes from graph
    (maphash (lambda (id props)
               (declare (ignore props))
               (let* ((info (gethash id info-map))
                      (topic (or (getf info :topic) (extract-topic id)))
                      (date (or (getf info :date) (extract-date-prefix id)))
                      (display (or (getf info :display-name) (humanize-task-name id)))
                      (status (or (getf info :status) :legacy))
                      (has-events (getf info :has-events)))
                 (when date (push date dates))
                 (incf (gethash topic cluster-counts 0))
                 (push (format nil "{\"id\":~S,\"label\":~S,\"topic\":~S,\"color\":~S,\"date\":~S,\"status\":~S,\"hasEvents\":~A}"
                               id display topic (string-to-color topic)
                               (or date "")
                               (string-downcase (princ-to-string status))
                               (if has-events "true" "false"))
                       nodes-json)))
             (task-graph-nodes graph))
    ;; Edges from forward adjacency (only emit if both endpoints exist)
    (let ((node-set (task-graph-nodes graph)))
      (maphash (lambda (src edges)
                 (when (gethash src node-set)
                   (dolist (e edges)
                     (let ((tgt (first e)))
                       (when (gethash tgt node-set)
                         (push (format nil "{\"source\":~S,\"target\":~S,\"layer\":~S}"
                                       src tgt
                                       (string-downcase (princ-to-string (second e))))
                               edges-json))))))
               (task-graph-forward graph)))
    ;; Cluster summaries (sorted by count desc)
    (let ((clusters nil))
      (maphash (lambda (k v) (push (cons k v) clusters)) cluster-counts)
      (setf clusters (sort clusters #'> :key #'cdr))
      ;; Date range
      (let* ((sorted-dates (sort (remove nil dates) #'string<))
             (min-date (or (first sorted-dates) "2026-01-01"))
             (max-date (or (car (last sorted-dates)) "2026-01-30")))
        (format nil "{\"nodes\":[~{~A~^,~}],\"edges\":[~{~A~^,~}],\"clusters\":[~{~A~^,~}],\"minDate\":~S,\"maxDate\":~S}"
                (nreverse nodes-json)
                (nreverse edges-json)
                (mapcar (lambda (c)
                          (format nil "{\"name\":~S,\"count\":~D,\"color\":~S}"
                                  (car c) (cdr c)
                                  (string-to-color (car c))))
                        (subseq clusters 0 (min 20 (length clusters))))
                min-date max-date)))))
