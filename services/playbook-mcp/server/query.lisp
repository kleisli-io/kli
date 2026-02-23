(in-package #:playbook-mcp)

;;; PQ Query Tool
;;;
;;; Thin MCP wrapper over lib/playbook/query.lisp (the :pq package).
;;; Binds dynamic context and provides error handling.
;;; Provides mutation handler for ! operations.

;;; ============================================================
;;; MUTATION HANDLER
;;; ============================================================

(defun normalize-feedback-type (feedback-type)
  "Normalize feedback input to a keyword: :helpful, :harmful, :not-relevant, or NIL.
   Accepts keywords, strings, and the :irrelevant alias."
  (cond ((or (eq feedback-type :helpful)
             (string-equal feedback-type "helpful"))
         :helpful)
        ((or (eq feedback-type :harmful)
             (string-equal feedback-type "harmful"))
         :harmful)
        ((or (eq feedback-type :not-relevant)
             (eq feedback-type :irrelevant)
             (string-equal feedback-type "not-relevant")
             (string-equal feedback-type "irrelevant"))
         :not-relevant)
        (t nil)))

(defun pq-mutation-handler (op pattern-id &rest args)
  "Execute a PQ mutation operation.
   OP is a keyword like :feedback, :evolve, :link, :add.
   PATTERN-ID is nil for :add, otherwise the target pattern ID.
   Returns result string."
  (case op
    (:feedback
     (let* ((feedback-type (first args))
            (evidence (second args))
            (pattern (get-pattern pattern-id))
            (type-kw (normalize-feedback-type feedback-type)))
       (unless pattern
         (error "Pattern not found: ~A" pattern-id))
       (unless type-kw
         (error "Invalid feedback type: ~A (use :helpful, :harmful, or :not-relevant)"
                feedback-type))
       (cond
         ;; Quality path: helpful/harmful → update counters + playbook.md
         ((member type-kw '(:helpful :harmful))
          (save-pattern-feedback pattern-id type-kw evidence)
          (after-pattern-mutation :feedback pattern-id))
         ;; Relevance path: not-relevant → sidecar store (no graph staleness)
         ((eq type-kw :not-relevant)
          (let* ((session-id (current-session-id))
                 (domains (when session-id
                            (let ((session (get-session session-id)))
                              (when session
                                (session-state-active-domains session))))))
            (save-pattern-relevance-feedback pattern-id domains evidence))))
       ;; Common tail: record in session + update feedback state file
       (let ((session-id (current-session-id)))
         (when session-id
           (record-feedback session-id pattern-id type-kw)
           (let ((cwd (or (mcp-find-depot-root) (namestring (uiop:getcwd)))))
             (write-feedback-state-file cwd session-id))))
       (format nil "Recorded ~A for ~A" type-kw pattern-id)))

    (:evolve
     (let* ((content (first args))
            (reason (or (getf args :reason) "PQ mutation"))
            (pattern (get-pattern pattern-id)))
       (if pattern
           (progn
             (save-pattern-evolution pattern-id content reason)
             (after-pattern-mutation :evolve pattern-id)
             (format nil "Evolved ~A: ~A" pattern-id reason))
           (error "Pattern not found: ~A" pattern-id))))

    (:link
     (let* ((target (first args))
            (relation (or (second args) :requires))
            (weight (or (third args) 1.0)))
       (unless (get-pattern pattern-id)
         (error "Source pattern not found: ~A" pattern-id))
       (unless (get-pattern target)
         (error "Target pattern not found: ~A" target))
       (let* ((rel-str (if (keywordp relation)
                           (string-downcase (symbol-name relation))
                           relation))
              (rel-kw (intern (string-upcase rel-str) :keyword))
              (edge (make-edge :source pattern-id
                               :target target
                               :relation rel-kw
                               :weight (coerce weight 'single-float)
                               :evidence "")))
         (add-edge edge)
         (setf *graph-stale* t)
         (when *manual-edges-path*
           (let ((manual-edges (remove-if-not
                                (lambda (e)
                                  (member (edge-relation e)
                                          '(:requires :supersedes :conflicts)))
                                (all-edges))))
             (save-edges-file *manual-edges-path* manual-edges)))
         (rebuild-graph :manual-edges-path *manual-edges-path*)
         (format nil "Linked ~A -> ~A (~A)" pattern-id target relation))))

    (:add
     (let* ((domain (getf args :domain))
            (content (getf args :content)))
       (unless domain
         (error ":add requires :domain"))
       (unless content
         (error ":add requires :content"))
       (let ((meta-path (detect-depot-meta-path)))
         (if meta-path
             (let* ((id (generate-pattern-id domain))
                    (pattern (make-pattern :id id
                                           :domain domain
                                           :content content
                                           :helpful 0
                                           :harmful 0))
                    (target-file (merge-pathnames "playbook.md" meta-path)))
               (append-pattern-to-file target-file pattern)
               (after-pattern-mutation :add id)
               id)
             (error "Cannot add pattern: No depot found")))))

    (:devolve
     (let ((pattern (get-pattern pattern-id)))
       (if pattern
           (let ((history (pattern-evolution-history pattern)))
             (if history
                 (let* ((previous (pop (pattern-evolution-history pattern)))
                        (old-content (third previous)))
                   (setf (pattern-content pattern) old-content)
                   (when (pattern-source-file pattern)
                     (update-pattern-in-file
                      (pattern-source-file pattern)
                      pattern-id
                      (lambda (line) (declare (ignore line)) (pattern-to-line pattern))))
                   (after-pattern-mutation :devolve pattern-id)
                   (format nil "Rolled back ~A. ~D versions remain."
                           pattern-id (length (pattern-evolution-history pattern))))
                 (error "No evolution history for ~A" pattern-id)))
           (error "Pattern not found: ~A" pattern-id))))

    (otherwise
     (error "Unknown mutation operation: ~A" op))))

;;; ============================================================
;;; CONTEXT BINDING FUNCTIONS
;;; ============================================================

(defun maybe-auto-activate (session-id cwd)
  "Silently activate patterns on the first pq_query of a session.
   Reads domains from .claude/sessions/{id}/playbook/domains.txt (written by UserPromptSubmit hook).
   No-op if: session already has activations, no domains file, or no patterns loaded."
  (when (and session-id cwd (> (pattern-count) 0))
    (let ((session (get-or-create-session session-id)))
      (when (null (session-state-activated-patterns session))
        (let ((domains-file (merge-pathnames
                             (format nil ".claude/sessions/~A/playbook/domains.txt" session-id)
                             (uiop:ensure-directory-pathname cwd))))
          (when (probe-file domains-file)
            (let ((domains (with-open-file (f domains-file)
                             (loop for line = (read-line f nil nil) while line
                                   for trimmed = (string-trim '(#\Space #\Return #\Newline) line)
                                   when (string/= trimmed "") collect trimmed))))
              (when domains
                (pq-activate-fn (format nil "~{~A~^ ~}" domains)
                                :boost domains :top-k 5)))))))))

(defun pq-search-fn (query &key (limit 10))
  "Semantic search wrapper for PQ."
  (let ((results (semantic-search-patterns query :limit limit)))
    ;; semantic-search-patterns returns (pattern . similarity) pairs
    ;; PQ expects just patterns
    (mapcar #'car results)))

(defun pq-activate-fn (query &key boost (top-k 5))
  "Activation wrapper for PQ (domain-agnostic).
   Records activations for in-process co-app mining and temporal decay."
  (let* ((results (activate-patterns-adaptive query :boost boost :top-k top-k))
         (patterns (mapcar #'car results))
         (pattern-ids (mapcar #'pattern-id patterns)))
    ;; Record activations for in-process co-app mining and temporal decay
    (when pattern-ids
      (let ((session-id (current-session-id)))
        (when session-id
          (record-activation-and-update-graph session-id pattern-ids)
          ;; Belt+suspenders: also write to session state file for SessionEnd hook
          (let ((cwd (or (mcp-find-depot-root) (namestring (uiop:getcwd)))))
            (write-activation-to-mcp-state cwd pattern-ids)
            ;; Write feedback state for Stop hook to read
            (write-feedback-state-file cwd session-id))))
      ;; Mark patterns used for temporal decay
      (dolist (id pattern-ids)
        (mark-pattern-used id)))
    ;; Return patterns for PQ (strip scores)
    patterns))

(defun pq-edges-fn (pattern-id)
  "Get edges for a pattern."
  (let ((outgoing (graph-outgoing pattern-id))
        (incoming nil))
    ;; Collect incoming edges
    (maphash (lambda (k edges)
               (declare (ignore k))
               (dolist (e edges)
                 (when (string= (edge-target e) pattern-id)
                   (push e incoming))))
             *current-graph*)
    (append (mapcar (lambda (e)
                      (list :out (edge-target e) (edge-relation e) (edge-weight e)))
                    outgoing)
            (mapcar (lambda (e)
                      (list :in (edge-source e) (edge-relation e) (edge-weight e)))
                    incoming))))

(defun pq-history-fn (pattern-id)
  "Get evolution history for a pattern."
  (let ((pattern (get-pattern pattern-id)))
    (when pattern
      (pattern-evolution-history pattern))))

;;; ============================================================
;;; MCP TOOL DEFINITION
;;; ============================================================

(define-tool pq_query
    ((query string "PQ query as S-expression string"))
  "Execute a PQ (Playbook Query) expression against the pattern graph.

PQ is a pipeline-based query language for exploring and mutating patterns.
Queries are S-expressions that start with a source and apply transformations.

## Starting Expressions
- :all - All patterns
- (pattern \"id\") - Single pattern by ID
- (search \"query\") - Semantic similarity search
- (proven :min N) - Patterns with helpful >= N (default 3)
- (warnings) - Patterns with harmful > 0
- (activate \"query\") - Domain-agnostic graph-powered retrieval
- (activate \"query\" :boost (lisp nix)) - With optional domain boost
- (query \"name\") - Execute a named query

## Pipeline Steps
Use (-> source step1 step2 ...) to chain transformations:
- (:where predicate) - Filter patterns
- (:sort :field) - Sort by field (descending)
- (:take n) - Limit to first n results
- :ids - Extract just pattern IDs (list of strings)
- :count - Count patterns (integer)
- :edges - Get graph edges for each pattern
- :history - Get evolution history for each pattern
- :full - Show full pattern content (no truncation)
- (:group-by :field) - Group by field value

## Named Queries
Pre-defined queries accessible via (query \"name\"):
- \"proven\" - Patterns with helpful >= 3
- \"warnings\" - Patterns marked harmful
- \"orphans\" - Patterns with no graph edges
- \"embedded\" - Patterns with embeddings
- \"unembedded\" - Patterns without embeddings
- \"lisp\" - Patterns in lisp domain
- \"nix\" - Patterns in nix domain
- \"ace\" - Patterns in ace domain
- \"nixos\" - Patterns in nixos domain

## Predicates (for :where)
- (domain= :lisp) - Domain equals value
- (> :helpful N) - Field greater than value
- (< :harmful N) - Field less than value
- (>= :helpful N) - Field greater or equal
- (has :embedding) - Field exists and is non-nil
- (matches \"text\") - Content contains substring
- (and pred1 pred2) - Both predicates true
- (or pred1 pred2) - Either predicate true
- (not pred) - Negate predicate

## Examples

Count all patterns:
  (-> :all :count)

Find lisp patterns:
  (-> :all (:where (domain= :lisp)) :ids)

Get top helpful patterns:
  (-> :all (:sort :helpful) (:take 5))

Search for DSL patterns:
  (-> (search \"DSL interpreter\") (:take 3))

Complex filter:
  (-> :all (:where (and (domain= :lisp) (> :helpful 0))) :ids)

Group by domain:
  (-> :all (:group-by :domain))

Get pattern edges:
  (-> (pattern \"lisp-000001\") :edges)

## Mutation Steps (! suffix)
Apply mutations to patterns in the pipeline result:
- (:feedback! :helpful \"evidence\") - Record positive feedback
- (:feedback! :harmful \"why\") - Record negative feedback
- (:evolve! \"new content\" :reason \"why\") - Update pattern content
- (:devolve!) - Rollback to previous version from history
- (:link! \"target-id\" :requires) - Create graph edge

Mutation examples:

Record helpful feedback:
  (-> (pattern \"lisp-001\") (:feedback! :helpful \"REPL verified\"))

Evolve a pattern:
  (-> (pattern \"nix-001\") (:evolve! \"Updated content\" :reason \"Clarified\"))

## Add New Pattern
(add! :domain :lisp :content \"Pattern content here\")
Note: add! is a starting expression, not a pipeline step."
  (let ((sid (current-session-id))
        (cwd (or (mcp-find-depot-root) (namestring (uiop:getcwd)))))
    (maybe-auto-activate sid cwd))
  (handler-case
      (let* ((form (pq:safe-read-query query))
             (result (pq:with-patterns
                         (:patterns #'list-patterns
                          :mutation-handler #'pq-mutation-handler
                          :search-fn #'pq-search-fn
                          :activate-fn #'pq-activate-fn
                          :edges-fn #'pq-edges-fn
                          :history-fn #'pq-history-fn)
                       (pq:interpret-query form))))
        (make-text-content (pq:format-query-result result)))
    (pq:pq-parse-error (c)
      (make-text-content (format nil "Parse error: ~A" (pq:pq-error-message c))))
    (pq:pq-error (c)
      (make-text-content (format nil "Query error: ~A" (pq:pq-error-message c))))
    (error (c)
      (make-text-content (format nil "Error: ~A" c)))))
