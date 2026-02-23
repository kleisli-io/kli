;;; TQ — Task Query Language
;;;
;;; A Lisp-native query language for the task graph.
;;; Two interfaces:
;;;   1. `->` macro: for Lisp programmers, uses function predicates
;;;   2. `interpret-query`: for MCP/Claude, safely interprets S-expressions
;;;
;;; The interpreter uses read-from-string with *read-eval* nil to get the
;;; S-expression AST, then walks it — never calling eval.

(defpackage #:tq
  (:use #:cl)
  (:export
   ;; Dynamic context
   #:*graph*
   #:*mutation-handler*
   #:*current-task-id*
   #:with-graph
   #:current-graph

   ;; Starting expressions (produce node-sets)
   #:all-nodes
   #:node
   #:active
   #:dormant
   #:query

   ;; Named queries
   #:*named-queries*
   #:define-query
   #:register-query
   #:list-queries

   ;; Pipeline macro (for Lisp programmers)
   #:->

   ;; Pipeline primitives
   #:follow
   #:back
   #:where-step
   #:select-fields
   #:sort-by-field
   #:take-n
   #:ids-step
   #:count-step
   #:enrich-step
   #:skip-step
   #:edges-step
   #:group-by-step

   ;; Predicates (function-based, for -> macro)
   #:prop=
   #:prop>
   #:prop<
   #:prop>=
   #:has-prop
   #:name-matches
   #:pred-and
   #:pred-or
   #:pred-not

   ;; Set operations
   #:node-union
   #:node-intersection
   #:node-difference

   ;; Safe interpreter (for MCP)
   #:safe-read-query
   #:interpret-query
   #:format-query-result

   ;; Mutation results
   #:mutation-log
   #:mutation-log-p
   #:mutation-log-entries
   #:mutation-log-skipped
   #:mutation-log-errors
   #:mutation-entry
   #:mutation-entry-node-id
   #:mutation-entry-operation
   #:mutation-entry-result
   #:mutation-entry-message

   ;; Group-by results
   #:group-by-result
   #:group-by-result-p
   #:group-by-result-groups
   #:group-by-result-field

   ;; Scaffolding results
   #:scaffold-result
   #:scaffold-result-p
   #:scaffold-result-created
   #:scaffold-result-edges
   #:scaffold-result-parent
   #:scaffold-result-metadata
   #:*phase-metadata-keys*
   #:parse-phase-spec

   ;; Conditions
   #:tq-error
   #:tq-error-message
   #:tq-parse-error
   #:mutation-without-handler
   #:mutation-safety-exceeded

   ;; Safety configuration
   #:*mutation-safety-limit*

   ;; Exact node lookup
   #:exact-node))

(in-package #:tq)

;;; ============================================================
;;; DYNAMIC CONTEXT
;;; ============================================================

(defvar *graph* nil
  "Current task-graph for queries. Bind with WITH-GRAPH.")

(defvar *mutation-handler* nil
  "Function (op &rest args) for executing mutations.
   Nil in lib/task, bound by task-mcp server.")

(defvar *current-task-id* nil
  "Current task ID for (current) expression. Bound by task-mcp.")

(defparameter *mutation-safety-limit* 20
  "Maximum number of tasks a single mutation step can affect.
   Use (:take N) before a mutation step to explicitly narrow the set,
   or use (query \"plan\") to target current task's phases.")

(defparameter *enrich-required-fields*
  '(:crdt-status :obs-count :edge-count :session-count :alpha :entropy :organized :affinity)
  "Fields that only exist after enrich-step. Used to warn users.")

(defparameter *phase-metadata-keys*
  '(:objective :acceptance :steps :context :constraints :await :ephemeral)
  "Recognized metadata keywords for scaffold-plan! phase specs.")

(defun current-graph ()
  "Return the current graph, building if needed."
  (or *graph* (task:build-multi-depot-task-graph)))

(defmacro with-graph ((&optional graph) &body body)
  "Execute BODY with *graph* bound. Builds graph if not provided."
  `(let ((*graph* (or ,graph *graph* (task:build-multi-depot-task-graph))))
     ,@body))

;;; ============================================================
;;; NODE-SET TYPE
;;; A node-set is a list of (id . props) pairs.
;;; Props is a plist: (:display-name "..." :topic "..." :status ...)
;;; ============================================================

(defun make-node-set (items)
  "Create a node-set from a list of (id . props) pairs."
  items)

(defun node-set-items (node-set)
  "Get the items from a node-set."
  node-set)

(defun node-set-count (node-set)
  "Count nodes in a node-set."
  (length node-set))

(defun node-set-ids (node-set)
  "Extract just the IDs from a node-set."
  (mapcar #'car node-set))

;;; ============================================================
;;; STARTING EXPRESSIONS
;;; These produce initial node-sets from the graph.
;;; ============================================================

(defun all-nodes ()
  "Return all nodes in the graph as a node-set."
  (let ((result nil)
        (nodes (task:task-graph-nodes (current-graph))))
    (maphash (lambda (id props)
               (push (cons id props) result))
             nodes)
    result))

(defun node (pattern)
  "Return nodes whose ID contains PATTERN as a substring.
   Rejects nil or empty-string patterns (which would match everything)."
  (when (or (null pattern) (and (stringp pattern) (zerop (length pattern))))
    (error 'tq-error :message "(node) requires a non-empty pattern"))
  (let ((result nil)
        (nodes (task:task-graph-nodes (current-graph))))
    (maphash (lambda (id props)
               (when (search pattern id :test #'char-equal)
                 (push (cons id props) result)))
             nodes)
    result))

(defun exact-node (id)
  "Return exactly the node with ID using exact string= match.
   Returns a node-set of zero or one elements."
  (let* ((nodes (task:task-graph-nodes (current-graph)))
         (props (gethash id nodes)))
    (when props
      (list (cons id props)))))

(defun active ()
  "Return event-sourced tasks (have events)."
  (remove-if-not (lambda (entry)
                   (eq (getf (cdr entry) :status) :event-sourced))
                 (all-nodes)))

(defun dormant ()
  "Return dormant tasks (no events)."
  (remove-if-not (lambda (entry)
                   (eq (getf (cdr entry) :status) :dormant))
                 (all-nodes)))

;;; ============================================================
;;; NAMED QUERIES
;;; Pre-defined queries accessible by name via (query "name").
;;; ============================================================

(defvar *named-queries* (make-hash-table :test 'equal)
  "Registry of named queries. Maps name -> thunk returning node-set.")

(defmacro define-query (name &body body)
  "Define a named query. BODY should return a node-set."
  `(setf (gethash ,name *named-queries*)
         (lambda () ,@body)))

(defun register-query (name thunk)
  "Register a named query function."
  (setf (gethash name *named-queries*) thunk))

(defun query (name)
  "Execute a named query by NAME. Returns a node-set."
  (let ((thunk (gethash name *named-queries*)))
    (unless thunk
      (error 'tq-error :message (format nil "Unknown query: ~A" name)))
    (funcall thunk)))

(defun list-queries ()
  "List all registered query names."
  (let ((names nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k names))
             *named-queries*)
    (sort names #'string<)))

;; Pre-registered queries
(define-query "active-roots"
  "Event-sourced tasks that have no parent (not a phase of another task)."
  (let ((all-active (active))
        (has-parent (make-hash-table :test 'equal)))
    ;; Mark all tasks that are phases of something
    (dolist (entry all-active)
      (let ((children (follow (list entry) :phase-of)))
        (dolist (child children)
          (setf (gethash (car child) has-parent) t))))
    ;; Return those without parents
    (remove-if (lambda (entry)
                 (gethash (car entry) has-parent))
               all-active)))

(define-query "orphans"
  "Tasks with no edges (neither incoming nor outgoing)."
  (let ((graph (current-graph)))
    (remove-if-not
     (lambda (entry)
       (let ((id (car entry)))
         (and (null (gethash id (task:task-graph-forward graph)))
              (null (gethash id (task:task-graph-reverse graph))))))
     (all-nodes))))

(define-query "leaf-tasks"
  "Tasks that have no children (phases)."
  (remove-if-not
   (lambda (entry)
     (null (follow (list entry) :phase-of)))
   (active)))

(define-query "stale-phases"
  "Phases that are still active but their parent is completed."
  (let ((result nil))
    (dolist (entry (enrich-step (active)))
      (let* ((id (car entry))
             (props (cdr entry))
             (status (getf props :crdt-status)))
        ;; Only check non-completed tasks
        (unless (equal status "completed")
          ;; Check if any parent is completed (enrich parents too)
          (let ((parents (enrich-step (back (list entry) :phase-of))))
            (dolist (parent parents)
              (let ((parent-status (getf (cdr parent) :crdt-status)))
                (when (equal parent-status "completed")
                  (push entry result))))))))
    result))

(define-query "orphaned-phases"
  "Phases claimed by sessions that have departed (session.leave event emitted).
   These represent work that may need pickup by another session."
  (let ((result nil))
    (dolist (entry (enrich-step (active)))
      (let* ((id (car entry))
             (props (cdr entry))
             (status (getf props :crdt-status))
             ;; Get claim from enriched state
             (events-path (task-events-path id)))
        ;; Only check non-completed tasks
        (unless (equal status "completed")
          ;; Load task state to check claim
          (when (probe-file events-path)
            (handler-case
                (let* ((log (elog-load events-path))
                       (events (reverse (event-log-events log)))
                       (state (compute-state events))
                       (claim (crdt:lww-value (task-state-claim state))))
                  ;; If task is claimed and claimer has departed
                  (when (and claim (> (length claim) 0))
                    (let ((departure (session-has-left-p id claim)))
                      (when departure
                        (push entry result)))))
              (error () nil))))))
    result))

(defun get-plan-phases ()
  "Get phases for plan query. If current task has phases, return them.
   If current task IS a phase (no sub-phases but has parent), return siblings."
  (let ((my-phases (follow (exact-node *current-task-id*) :phase-of)))
    (if my-phases
        my-phases
        ;; No sub-phases - check if I'm a phase myself
        (let ((parents (back (exact-node *current-task-id*) :phase-of)))
          (when parents
            ;; I'm a phase - show siblings (parent's phases)
            (follow parents :phase-of))))))

(define-query "plan"
  "Phases of current task (or sibling phases if current is a phase)."
  (if *current-task-id*
      (enrich-step (or (get-plan-phases) nil))
      (error 'tq-error :message "\"plan\" requires task context")))

(define-query "plan-ready"
  "Ready phases (non-completed) of current task or siblings."
  (if *current-task-id*
      (let ((phases (enrich-step (or (get-plan-phases) nil))))
        (remove-if (lambda (entry)
                     (equal (getf (cdr entry) :crdt-status) "completed"))
                   phases))
      (error 'tq-error :message "\"plan-ready\" requires task context")))

(define-query "recent"
  "Recently active tasks (by session count, descending)."
  (take-n (sort-by-field (enrich-step (active)) :session-count) 20))

(define-query "busy"
  "Tasks with most observations (descending)."
  (take-n (sort-by-field (enrich-step (active)) :obs-count) 20))

(define-query "hub-tasks"
  "Tasks with most edges (graph hubs, descending)."
  (take-n (sort-by-field (enrich-step (active)) :edge-count) 20))

(define-query "health"
  "Combined health report: stale phases, orphans, and leaf tasks."
  (let ((stale (query "stale-phases"))
        (orphans (query "orphans"))
        (leaves (query "leaf-tasks")))
    (node-union (node-union stale orphans) leaves)))

;;; ============================================================
;;; PIPELINE PRIMITIVES
;;; These transform node-sets.
;;; ============================================================

(defun follow (node-set edge-type)
  "Traverse forward edges of EDGE-TYPE from nodes in NODE-SET.
   Warns on dangling edges (target exists in edges but not in graph nodes)."
  (let ((graph (current-graph))
        (result nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (entry node-set)
      (let* ((id (car entry))
             (edges (gethash id (task:task-graph-forward graph))))
        (dolist (edge edges)
          (when (eq (second edge) edge-type)
            (let ((target-id (first edge)))
              (unless (gethash target-id seen)
                (setf (gethash target-id seen) t)
                (let ((props (task:graph-node-props graph target-id)))
                  (if props
                      (push (cons target-id props) result)
                      (warn "TQ follow: dangling ~A edge from ~A to ~A (target not in graph)"
                            edge-type id target-id)))))))))
    (nreverse result)))

(defun back (node-set edge-type)
  "Traverse backward (incoming) edges of EDGE-TYPE to nodes in NODE-SET.
   Warns on dangling edges (source exists in edges but not in graph nodes)."
  (let ((graph (current-graph))
        (result nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (entry node-set)
      (let* ((id (car entry))
             (edges (gethash id (task:task-graph-reverse graph))))
        (dolist (edge edges)
          (when (eq (second edge) edge-type)
            (let ((source-id (first edge)))
              (unless (gethash source-id seen)
                (setf (gethash source-id seen) t)
                (let ((props (task:graph-node-props graph source-id)))
                  (if props
                      (push (cons source-id props) result)
                      (warn "TQ back: dangling ~A edge to ~A from ~A (source not in graph)"
                            edge-type id source-id)))))))))
    (nreverse result)))

(defun where-step (node-set predicate)
  "Filter NODE-SET to nodes satisfying PREDICATE.
   PREDICATE is a function (id props) -> boolean."
  (remove-if-not (lambda (entry)
                   (funcall predicate (car entry) (cdr entry)))
                 node-set))

(defun select-fields (node-set &rest fields)
  "Project only specified FIELDS from each node's props."
  (mapcar (lambda (entry)
            (let ((id (car entry))
                  (props (cdr entry))
                  (new-props nil))
              (dolist (field fields)
                (let ((val (getf props field)))
                  (when val
                    (setf (getf new-props field) val))))
              (cons id new-props)))
          node-set))

(defun sort-by-field (node-set field &key (descending t))
  "Sort NODE-SET by FIELD value. Detects numeric fields for proper ordering.
   Scans all non-nil values for type detection (not just the first node)."
  (when (null node-set) (return-from sort-by-field nil))
  (let* ((numeric-p (loop for entry in node-set
                          for val = (getf (cdr entry) field)
                          when val return (numberp val))))
    (sort (copy-list node-set)
          (if numeric-p
              (if descending #'> #'<)
              (if descending #'string> #'string<))
          :key (lambda (entry)
                 (let ((val (getf (cdr entry) field)))
                   (if numeric-p
                       (or val 0)
                       (if (stringp val) val (princ-to-string (or val "")))))))))

(defun take-n (node-set n)
  "Return first N nodes from NODE-SET."
  (subseq node-set 0 (min n (length node-set))))

(defun ids-step (node-set)
  "Extract just the IDs from NODE-SET. Returns a list of strings."
  (node-set-ids node-set))

(defun count-step (node-set)
  "Count nodes in NODE-SET. Returns an integer."
  (node-set-count node-set))

(defun skip-step (node-set n)
  "Skip first N nodes from NODE-SET (for pagination)."
  (nthcdr n node-set))

(defun edges-step (node-set)
  "Get edges for each node in NODE-SET.
   Returns alist of (id . edges) where each edge is (direction type target-id)."
  (let ((graph (current-graph)))
    (mapcar (lambda (entry)
              (let* ((id (car entry))
                     (fwd (gethash id (task:task-graph-forward graph)))
                     (rev (gethash id (task:task-graph-reverse graph))))
                (cons id (append
                          (mapcar (lambda (e) (list :forward (second e) (first e))) fwd)
                          (mapcar (lambda (e) (list :reverse (second e) (first e))) rev)))))
            node-set)))

(defun enrich-step (node-set)
  "Enrich NODE-SET with full CRDT state data and Markov metrics.
   Adds: :crdt-status, :obs-count, :edge-count, :session-count, :display-name,
         :alpha, :entropy, :organized, :affinity."
  (mapcar (lambda (entry)
            (let* ((id (car entry))
                   (props (cdr entry))
                   (path (handler-case (task:task-events-path id)
                           (error () nil))))
              (if (and path (probe-file path))
                  (handler-case
                      (let* ((log (task:elog-load path))
                             (events (reverse (task:event-log-events log)))
                             (state (task:compute-state events))
                             (new-props (copy-list props)))
                        ;; Add CRDT-derived fields
                        (setf (getf new-props :crdt-status)
                              (crdt:lww-value (task:task-state-status state)))
                        (setf (getf new-props :obs-count)
                              (hash-table-count
                               (slot-value (task:task-state-observations state)
                                           'crdt::elements)))
                        (setf (getf new-props :edge-count)
                              (length (crdt:ors-members
                                       (task:task-state-edges state))))
                        (setf (getf new-props :session-count)
                              (hash-table-count
                               (slot-value (task:task-state-sessions state)
                                           'crdt::elements)))
                        ;; Display name and metadata promotion
                        (let ((meta (task:task-state-metadata state)))
                          (setf (getf new-props :display-name)
                                (or (crdt:lwwm-get meta "display-name") id))
                          ;; Promote all metadata keys to node properties
                          (dolist (key (crdt:lwwm-keys meta))
                            (unless (string= key "display-name")
                              (let ((kw (intern (string-upcase key) :keyword))
                                    (val (crdt:lwwm-get meta key)))
                                (when val
                                  (setf (getf new-props kw) val))))))
                        ;; Markov enrichment fields
                        (when events
                          (setf (getf new-props :alpha)
                                (task:action-functor events state))
                          (setf (getf new-props :entropy)
                                (task:event-type-entropy events))
                          (setf (getf new-props :organized)
                                (getf (task:organization-indicator events state)
                                      :organized))
                          (setf (getf new-props :affinity)
                                (task:affinity-score events state)))
                        (cons id new-props))
                    (error (c)
                      (warn "TQ enrich failed for ~A: ~A" id c)
                      entry))
                  entry)))
          node-set))

(defstruct group-by-result
  "Result of a group-by operation. Wraps alist to avoid format heuristic ambiguity."
  (groups nil :type list)    ; alist of (value . nodes)
  (field nil :type symbol))  ; field grouped by

(defun group-by-step (node-set field)
  "Group NODE-SET by FIELD value. Returns a group-by-result struct."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (entry node-set)
      (let ((val (or (getf (cdr entry) field) :nil)))
        (push entry (gethash val groups))))
    ;; Convert to sorted alist
    (let ((result nil))
      (maphash (lambda (k v)
                 (push (cons k (nreverse v)) result))
               groups)
      (make-group-by-result
       :groups (sort result #'> :key (lambda (g) (length (cdr g))))
       :field field))))

;;; ============================================================
;;; PREDICATES (function-based, for -> macro)
;;; These return functions suitable for WHERE-STEP.
;;; ============================================================

(defun prop= (field value)
  "Return predicate: property FIELD equals VALUE."
  (lambda (id props)
    (declare (ignore id))
    (equal (getf props field) value)))

(defun prop> (field value)
  "Return predicate: numeric property FIELD > VALUE."
  (lambda (id props)
    (declare (ignore id))
    (let ((v (getf props field)))
      (and (numberp v) (> v value)))))

(defun prop< (field value)
  "Return predicate: numeric property FIELD < VALUE."
  (lambda (id props)
    (declare (ignore id))
    (let ((v (getf props field)))
      (and (numberp v) (< v value)))))

(defun prop>= (field value)
  "Return predicate: numeric property FIELD >= VALUE."
  (lambda (id props)
    (declare (ignore id))
    (let ((v (getf props field)))
      (and (numberp v) (>= v value)))))

(defun has-prop (field)
  "Return predicate: property FIELD exists and is non-nil."
  (lambda (id props)
    (declare (ignore id))
    (getf props field)))

(defun name-matches (pattern)
  "Return predicate: node ID contains PATTERN."
  (lambda (id props)
    (declare (ignore props))
    (search pattern id :test #'char-equal)))

(defun pred-and (&rest predicates)
  "Return predicate: all PREDICATES are true."
  (lambda (id props)
    (every (lambda (p) (funcall p id props)) predicates)))

(defun pred-or (&rest predicates)
  "Return predicate: any PREDICATE is true."
  (lambda (id props)
    (some (lambda (p) (funcall p id props)) predicates)))

(defun pred-not (predicate)
  "Return predicate: PREDICATE is false."
  (lambda (id props)
    (not (funcall predicate id props))))

;;; ============================================================
;;; SET OPERATIONS
;;; ============================================================

(defun node-union (set1 set2)
  "Union of two node-sets."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (entry set1)
      (setf (gethash (car entry) seen) entry))
    (dolist (entry set2)
      (unless (gethash (car entry) seen)
        (setf (gethash (car entry) seen) entry)))
    (maphash (lambda (id entry)
               (declare (ignore id))
               (push entry result))
             seen)
    result))

(defun node-intersection (set1 set2)
  "Intersection of two node-sets."
  (let ((ids2 (make-hash-table :test 'equal)))
    (dolist (entry set2)
      (setf (gethash (car entry) ids2) t))
    (remove-if-not (lambda (entry)
                     (gethash (car entry) ids2))
                   set1)))

(defun node-difference (set1 set2)
  "Difference: nodes in SET1 but not in SET2."
  (let ((ids2 (make-hash-table :test 'equal)))
    (dolist (entry set2)
      (setf (gethash (car entry) ids2) t))
    (remove-if (lambda (entry)
                 (gethash (car entry) ids2))
               set1)))

;;; ============================================================
;;; PIPELINE MACRO (for Lisp programmers)
;;; ============================================================

(defmacro -> (start &rest steps)
  "Thread START through pipeline STEPS.
   Each step is either:
   - A bare keyword like :ids, :count, :enrich
   - A form like (:follow :phase-of), (:where predicate), (:group-by :field)"
  (if (null steps)
      start
      (let ((step (first steps))
            (rest (rest steps)))
        (cond
          ;; Bare keywords
          ((eq step :ids)
           `(-> (ids-step ,start) ,@rest))
          ((eq step :count)
           `(-> (count-step ,start) ,@rest))
          ((eq step :enrich)
           `(-> (enrich-step ,start) ,@rest))
          ((eq step :edges)
           `(-> (edges-step ,start) ,@rest))
          ;; List forms
          ((and (listp step) (eq (car step) :follow))
           `(-> (follow ,start ,(second step)) ,@rest))
          ((and (listp step) (eq (car step) :back))
           `(-> (back ,start ,(second step)) ,@rest))
          ((and (listp step) (eq (car step) :where))
           `(-> (where-step ,start ,(second step)) ,@rest))
          ((and (listp step) (eq (car step) :select))
           `(-> (apply #'select-fields ,start ',(cdr step)) ,@rest))
          ((and (listp step) (eq (car step) :sort))
           `(-> (sort-by-field ,start ,(second step)) ,@rest))
          ((and (listp step) (eq (car step) :take))
           `(-> (take-n ,start ,(second step)) ,@rest))
          ((and (listp step) (eq (car step) :skip))
           `(-> (skip-step ,start ,(second step)) ,@rest))
          ((and (listp step) (eq (car step) :group-by))
           `(-> (group-by-step ,start ,(second step)) ,@rest))
          (t
           (error "Unknown pipeline step: ~S" step))))))

;;; ============================================================
;;; CONDITIONS
;;; ============================================================

(define-condition tq-error (error)
  ((message :initarg :message :reader tq-error-message))
  (:report (lambda (c s)
             (format s "TQ error: ~A" (tq-error-message c)))))

(define-condition tq-parse-error (tq-error)
  ()
  (:report (lambda (c s)
             (format s "TQ parse error: ~A" (tq-error-message c)))))

(define-condition mutation-without-handler (tq-error)
  ((operation :initarg :operation :reader mutation-operation))
  (:report (lambda (c s)
             (format s "Mutation ~A requires *mutation-handler* binding"
                     (mutation-operation c)))))

(define-condition mutation-safety-exceeded (tq-error)
  ((operation :initarg :operation :reader mutation-operation)
   (count :initarg :count :reader mutation-count)
   (limit :initarg :limit :reader mutation-limit))
  (:report (lambda (c s)
             (format s "Mutation ~A would affect ~D tasks (limit: ~D). ~
                        Narrow with (:where ...) or (:take ~D) first, ~
                        or use (query \"plan\") to target current task's phases."
                     (mutation-operation c)
                     (mutation-count c)
                     (mutation-limit c)
                     (mutation-count c)))))

;;; ============================================================
;;; SAFE INTERPRETER (for MCP/Claude)
;;; ============================================================

(defun safe-read-query (query-string)
  "Safely read a query string into an S-expression.
   Disables *read-eval* to prevent #. attacks."
  (let ((*read-eval* nil)
        (*package* (find-package :tq)))
    (handler-case
        (read-from-string query-string)
      (error (c)
        (error 'tq-parse-error :message (format nil "~A" c))))))

(defun interpret-query (form)
  "Interpret a TQ query form. Returns a node-set, list, or scalar."
  (with-graph ()
    (interpret-expr form)))

(defun sym= (sym name)
  "Check if SYM has the given NAME (case-insensitive, ignoring package)."
  (and (symbolp sym)
       (string-equal (symbol-name sym) name)))

(defun interpret-expr (form)
  "Interpret a single expression."
  (cond
    ;; Keywords
    ((eq form :all) (all-nodes))

    ;; Atoms - return as-is
    ((atom form) form)

    ;; Pipeline
    ((sym= (car form) "->")
     (interpret-pipeline (cdr form)))

    ;; Starting expressions
    ((sym= (car form) "NODE")
     ;; Support multiple patterns: (node "a" "b" "c") unions all matches
     (if (cddr form)
         (reduce #'node-union (mapcar #'node (cdr form)))
         (node (second form))))
    ((sym= (car form) "ACTIVE")
     (active))
    ((sym= (car form) "DORMANT")
     (dormant))
    ((sym= (car form) "CURRENT")
     (if *current-task-id*
         (exact-node *current-task-id*)
         (error 'tq-error :message "(current) requires task context")))
    ((sym= (car form) "QUERY")
     (query (second form)))

    ;; Set operations
    ((sym= (car form) "UNION")
     (node-union (interpret-expr (second form))
                 (interpret-expr (third form))))
    ((sym= (car form) "MINUS")
     (node-difference (interpret-expr (second form))
                      (interpret-expr (third form))))
    ((sym= (car form) "INTERSECT")
     (node-intersection (interpret-expr (second form))
                        (interpret-expr (third form))))

    ;; Scaffolding forms
    ((sym= (car form) "SCAFFOLD-PLAN!")
     (execute-scaffold-plan (cdr form)))
    ((sym= (car form) "SCAFFOLD-CHAIN!")
     (execute-scaffold-chain (cdr form)))

    (t
     (error 'tq-error :message (format nil "Unknown expression: ~S" form)))))

;;; ============================================================
;;; MUTATION EXECUTION
;;; ============================================================

(defstruct mutation-log
  "Result of mutation execution."
  (entries nil :type list)     ; list of mutation-entry
  (skipped 0 :type integer)    ; count of completed tasks skipped
  (errors 0 :type integer))    ; count of errors

(defstruct mutation-entry
  "Single mutation result."
  node-id
  operation
  args
  result    ; :success, :skipped, or error message
  message)  ; human-readable message

(defun execute-mutation-step (node-set step)
  "Execute a mutation step on all nodes in NODE-SET.
   Checks *mutation-safety-limit* before executing.
   Returns a mutation-log with results for each node."
  (unless *mutation-handler*
    (error 'mutation-without-handler :operation (car step)))
  (let ((count (length node-set)))
    (when (and *mutation-safety-limit* (> count *mutation-safety-limit*))
      (error 'mutation-safety-exceeded
             :operation (car step)
             :count count
             :limit *mutation-safety-limit*
             :message (format nil "Mutation ~A would affect ~D tasks (limit: ~D)"
                              (car step) count *mutation-safety-limit*))))
  (let* ((op (car step))
         (args (cdr step))
         (op-name (normalize-mutation-op op))
         (log (make-mutation-log)))
    (dolist (entry node-set)
      (let ((node-id (car entry)))
        (handler-case
            (let ((result (apply *mutation-handler* node-id op-name args)))
              (push (make-mutation-entry
                     :node-id node-id
                     :operation op-name
                     :args args
                     :result (if (eq result :skipped) :skipped :success)
                     :message (if (eq result :skipped)
                                  "skipped (completed)"
                                  (or result "ok")))
                    (mutation-log-entries log))
              (when (eq result :skipped)
                (incf (mutation-log-skipped log))))
          (error (c)
            (push (make-mutation-entry
                   :node-id node-id
                   :operation op-name
                   :args args
                   :result :error
                   :message (format nil "~A" c))
                  (mutation-log-entries log))
            (incf (mutation-log-errors log))))))
    (setf (mutation-log-entries log) (nreverse (mutation-log-entries log)))
    log))

(defun normalize-mutation-op (op)
  "Convert :complete! keyword to :complete symbol for handler dispatch."
  (let ((name (symbol-name op)))
    ;; Strip trailing !
    (intern (subseq name 0 (1- (length name))) :keyword)))

;;; ============================================================
;;; DAG SCAFFOLDING
;;; ============================================================

(defstruct scaffold-result
  "Result of scaffold operation."
  (created nil :type list)       ; list of (local-name . task-id)
  (edges nil :type list)         ; list of (from to edge-type)
  (parent nil :type (or null string))
  (metadata nil :type list))     ; list of (local-name . key-count)

(defun maybe-improve-name (local-name description)
  "If LOCAL-NAME fails validation but DESCRIPTION exists, generate better name.
   Returns the final name to use (possibly improved)."
  (let ((validation (task-validation:validate-task-name local-name)))
    (if (task-validation:validation-result-valid-p validation)
        ;; Already valid - use as-is
        local-name
        ;; Invalid - try to generate from description
        (if (and description (> (length description) 0))
            (let ((generated (task-validation:suggest-name-from-description description)))
              (if (and generated
                       (> (length generated) 0)
                       ;; Also validate the generated name
                       (task-validation:validation-result-valid-p
                        (task-validation:validate-task-name generated)))
                  generated
                  ;; Generation failed or invalid, use original
                  local-name))
            ;; No description to generate from
            local-name))))

(defun parse-phase-spec (spec)
  "Parse a scaffold-plan! phase spec into components.
   SPEC is (name \"description\" [:after dep ...] [:keyword value] ...).
   Returns (values after-deps metadata-plist) where metadata-plist
   contains recognized *phase-metadata-keys* as keyword-value pairs."
  (let ((after-deps nil)
        (metadata nil)
        (i 2)
        (len (length spec)))
    (loop while (< i len)
          for item = (nth i spec)
          do (cond
               ;; :after — collect subsequent non-keyword symbols as dep refs
               ((eq item :after)
                (incf i)
                (loop while (< i len)
                      for dep = (nth i spec)
                      while (and (symbolp dep) (not (keywordp dep)))
                      do (push dep after-deps)
                         (incf i)))
               ;; Recognized metadata keyword — take next item as value
               ((and (keywordp item) (member item *phase-metadata-keys*))
                (if (< (1+ i) len)
                    (progn
                      (push item metadata)
                      (push (nth (1+ i) spec) metadata)
                      (incf i 2))
                    (incf i)))
               ;; Unknown — skip
               (t (incf i))))
    (values (nreverse after-deps) (nreverse metadata))))

(defun validate-scaffold-plan-refs (phase-specs)
  "Pre-validate all :after references in PHASE-SPECS before creating any tasks.
   Returns T if valid, signals tq-error if any :after reference is invalid."
  (let ((defined-names (make-hash-table :test 'equal)))
    ;; Collect all defined names (both raw and improved)
    (dolist (spec phase-specs)
      (let* ((raw-name (symbol-name (first spec)))
             (description (second spec))
             (local-name (maybe-improve-name raw-name description)))
        (setf (gethash raw-name defined-names) t)
        (setf (gethash local-name defined-names) t)))
    ;; Check all :after references (using parse-phase-spec for correct extraction)
    (dolist (spec phase-specs t)
      (multiple-value-bind (after-deps metadata) (parse-phase-spec spec)
        (declare (ignore metadata))
        (dolist (dep after-deps)
          (let ((dep-name (symbol-name dep)))
            (unless (gethash dep-name defined-names)
              (let ((available (loop for k being the hash-keys of defined-names collect k)))
                (error 'tq-error
                       :message (format nil "scaffold-plan!: :after reference ~S not found in plan. Defined: ~{~A~^, ~}"
                                        dep-name available))))))))))

(defun execute-scaffold-plan (phase-specs)
  "Execute (scaffold-plan! (p1 \"desc\") (p2 \"desc\" :after p1) ...).
   Three-pass: create all phases, wire dependencies, set metadata.
   Uses *current-task-id* as parent.
   Auto-improves short/invalid names using descriptions when available.
   Pre-validates all :after references before creating any tasks.

   Phase specs support metadata keywords alongside :after:
     (p1 \"Setup database\"
       :objective \"Create schema with migrations\"
       :acceptance \"Tables exist, migrations pass\"
       :steps \"1. Define schema\\n2. Run migrations\"
       :after p2)

   Recognized metadata keywords: :objective, :acceptance, :steps,
   :context, :constraints, :await, :ephemeral."
  (unless *mutation-handler*
    (error 'mutation-without-handler :operation 'scaffold-plan!))
  (unless *current-task-id*
    (error 'tq-error :message "scaffold-plan! requires current task context"))
  ;; Pre-validate: fail early before creating any tasks
  (validate-scaffold-plan-refs phase-specs)
  (let ((result (make-scaffold-result :parent *current-task-id*))
        (name-to-id (make-hash-table :test 'equal))    ; local-name -> created task-id
        (name-to-meta (make-hash-table :test 'equal)))  ; local-name -> metadata plist
    ;; Pass 1: Create all phases, extract metadata
    (dolist (spec phase-specs)
      (let* ((raw-name (symbol-name (first spec)))
             (description (second spec))
             (local-name (maybe-improve-name raw-name description)))
        (multiple-value-bind (after-deps metadata) (parse-phase-spec spec)
          (let ((created-id (funcall *mutation-handler*
                                     *current-task-id*
                                     :fork
                                     local-name
                                     description
                                     "phase-of")))
            (setf (gethash local-name name-to-id) created-id)
            (unless (string= raw-name local-name)
              (setf (gethash raw-name name-to-id) created-id))
            (push (cons local-name created-id) (scaffold-result-created result))
            ;; Store metadata for pass 3
            (when metadata
              (setf (gethash local-name name-to-meta) metadata))
            ;; Store dependencies for pass 2
            (dolist (dep after-deps)
              (push (list local-name (symbol-name dep) :depends-on)
                    (scaffold-result-edges result)))))))
    ;; Reverse to maintain order
    (setf (scaffold-result-created result)
          (nreverse (scaffold-result-created result)))
    (setf (scaffold-result-edges result)
          (nreverse (scaffold-result-edges result)))
    ;; Pass 2: Wire dependencies (all refs pre-validated, should not fail)
    (dolist (edge-spec (scaffold-result-edges result))
      (destructuring-bind (from-local to-local edge-type) edge-spec
        (let ((from-id (gethash from-local name-to-id))
              (to-id (gethash to-local name-to-id)))
          (when (and from-id to-id)
            (funcall *mutation-handler* from-id :link to-id edge-type)))))
    ;; Pass 3: Set metadata on created phases
    (maphash (lambda (local-name metadata)
               (let ((task-id (gethash local-name name-to-id)))
                 (when task-id
                   (let ((key-count 0))
                     (loop for (key value) on metadata by #'cddr
                           do (funcall *mutation-handler* task-id :set-meta key value)
                              (incf key-count))
                     (push (cons local-name key-count)
                           (scaffold-result-metadata result))))))
             name-to-meta)
    (setf (scaffold-result-metadata result)
          (nreverse (scaffold-result-metadata result)))
    result))

(defun max-existing-phase-number (task-id)
  "Find the highest phase-N number among existing children of TASK-ID.
   Returns 0 if no phase-N children exist."
  (let ((children (task:task-children task-id))
        (max-n 0))
    (dolist (child children max-n)
      (multiple-value-bind (depot bare) (task:parse-qualified-id child)
        (declare (ignore depot))
        ;; Match "phase-N" or "YYYY-MM-DD-phase-N" patterns in bare ID
        (let ((pos (search "phase-" bare :test #'char-equal)))
          (when pos
            (let* ((num-start (+ pos 6))
                   (num-str (subseq bare num-start))
                   ;; Extract leading digits
                   (end (position-if-not #'digit-char-p num-str))
                   (digits (subseq num-str 0 end)))
              (when (> (length digits) 0)
                (let ((n (parse-integer digits :junk-allowed t)))
                  (when (and n (> n max-n))
                    (setf max-n n)))))))))))

(defun execute-scaffold-chain (descriptions)
  "Execute (scaffold-chain! \"Phase 1\" \"Phase 2\" \"Phase 3\").
   Creates linear dependency chain: p1 -> p2 -> p3.
   Uses *current-task-id* as parent.
   Starts numbering after existing phase-N children to avoid collisions."
  (unless *mutation-handler*
    (error 'mutation-without-handler :operation 'scaffold-chain!))
  (unless *current-task-id*
    (error 'tq-error :message "scaffold-chain! requires current task context"))
  (let ((result (make-scaffold-result :parent *current-task-id*))
        (prev-id nil)
        (start-n (1+ (max-existing-phase-number *current-task-id*))))
    (loop for desc in descriptions
          for i from start-n
          do (let* ((local-name (format nil "phase-~D" i))
                    (created-id (funcall *mutation-handler*
                                         *current-task-id*
                                         :fork
                                         local-name
                                         desc
                                         "phase-of")))
               (push (cons local-name created-id) (scaffold-result-created result))
               ;; Wire dependency to previous
               (when prev-id
                 (funcall *mutation-handler* created-id :link prev-id :depends-on)
                 (push (list local-name prev-id :depends-on)
                       (scaffold-result-edges result)))
               (setf prev-id created-id)))
    (setf (scaffold-result-created result)
          (nreverse (scaffold-result-created result)))
    (setf (scaffold-result-edges result)
          (nreverse (scaffold-result-edges result)))
    result))

(defun interpret-pipeline (forms)
  "Interpret a pipeline: (start step1 step2 ...)"
  (let ((result (interpret-expr (first forms))))
    (dolist (step (rest forms))
      (setf result (interpret-step result step)))
    result))

;;; Enrich-required field checking

(defun extract-pred-fields (pred-form)
  "Extract field keywords from a predicate form for enrich checking."
  (cond
    ((atom pred-form) nil)
    ((and (listp pred-form) (or (sym= (car pred-form) "=")
                                 (sym= (car pred-form) "HAS")
                                 (sym= (car pred-form) ">")
                                 (sym= (car pred-form) "<")
                                 (sym= (car pred-form) ">=")))
     (let ((field (second pred-form)))
       (when (keywordp field) (list field))))
    ((and (listp pred-form) (or (sym= (car pred-form) "AND")
                                 (sym= (car pred-form) "OR")))
     (mapcan #'extract-pred-fields (cdr pred-form)))
    ((and (listp pred-form) (sym= (car pred-form) "NOT"))
     (extract-pred-fields (second pred-form)))
    (t nil)))

(defun needs-enrichment-p (node-set)
  "Check if NODE-SET lacks enrichment data."
  (and node-set
       (not (some (lambda (f) (getf (cdr (first node-set)) f))
                  *enrich-required-fields*))))

(defun auto-enrich-if-needed (node-set)
  "Automatically enrich NODE-SET if it lacks enrichment data."
  (if (needs-enrichment-p node-set)
      (enrich-step node-set)
      node-set))

(defun auto-enrich-for-fields (node-set fields)
  "Auto-enrich NODE-SET if any FIELDS require enrichment."
  (if (and fields
           (intersection fields *enrich-required-fields*)
           (needs-enrichment-p node-set))
      (enrich-step node-set)
      node-set))

(defun interpret-step (node-set step)
  "Interpret a single pipeline step."
  (cond
    ;; Bare keywords
    ((eq step :ids) (ids-step node-set))
    ((eq step :count) (count-step node-set))
    ((eq step :enrich) (enrich-step node-set))
    ((eq step :edges) (edges-step node-set))

    ;; List forms with keyword car
    ((and (listp step) (eq (car step) :follow))
     (follow node-set (second step)))
    ((and (listp step) (eq (car step) :back))
     (back node-set (second step)))
    ((and (listp step) (eq (car step) :where))
     (let ((enriched (auto-enrich-for-fields node-set
                                             (extract-pred-fields (second step)))))
       (where-step enriched (interpret-where (second step)))))
    ((and (listp step) (eq (car step) :select))
     (apply #'select-fields node-set (cdr step)))
    ((and (listp step) (eq (car step) :sort))
     (let ((field (second step)))
       (sort-by-field (auto-enrich-for-fields node-set (list field))
                      field)))
    ((and (listp step) (eq (car step) :take))
     (take-n node-set (second step)))
    ((and (listp step) (eq (car step) :skip))
     (skip-step node-set (second step)))
    ((and (listp step) (eq (car step) :group-by))
     (let ((field (second step)))
       (group-by-step (auto-enrich-for-fields node-set (list field))
                      field)))

    ;; Mutation steps - execute via handler
    ((and (listp step) (mutation-step-p (car step)))
     (execute-mutation-step node-set step))

    (t
     (error 'tq-error :message (format nil "Unknown step: ~S" step)))))

(defun mutation-step-p (sym)
  "Check if SYM is a mutation step (ends with !)."
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 0)
              (char= (char name (1- (length name))) #\!)))))

(defun interpret-where (pred-form)
  "Convert a declarative predicate form to a function."
  (cond
    ;; (= :field value)
    ((and (listp pred-form) (sym= (car pred-form) "="))
     (prop= (second pred-form) (third pred-form)))

    ;; (> :field value)
    ((and (listp pred-form) (sym= (car pred-form) ">"))
     (prop> (second pred-form) (third pred-form)))

    ;; (< :field value)
    ((and (listp pred-form) (sym= (car pred-form) "<"))
     (prop< (second pred-form) (third pred-form)))

    ;; (>= :field value)
    ((and (listp pred-form) (sym= (car pred-form) ">="))
     (prop>= (second pred-form) (third pred-form)))

    ;; (has :field)
    ((and (listp pred-form) (sym= (car pred-form) "HAS"))
     (has-prop (second pred-form)))

    ;; (matches "pattern")
    ((and (listp pred-form) (sym= (car pred-form) "MATCHES"))
     (name-matches (second pred-form)))

    ;; (and pred1 pred2 ...)
    ((and (listp pred-form) (sym= (car pred-form) "AND"))
     (apply #'pred-and (mapcar #'interpret-where (cdr pred-form))))

    ;; (or pred1 pred2 ...)
    ((and (listp pred-form) (sym= (car pred-form) "OR"))
     (apply #'pred-or (mapcar #'interpret-where (cdr pred-form))))

    ;; (not pred)
    ((and (listp pred-form) (sym= (car pred-form) "NOT"))
     (pred-not (interpret-where (second pred-form))))

    (t
     (error 'tq-error
            :message (format nil "Unknown predicate: ~S" pred-form)))))

;;; ============================================================
;;; RESULT FORMATTING (for MCP response)
;;; ============================================================

(defun format-query-result (result)
  "Format a query result for MCP text response."
  (cond
    ;; Integer (from :count)
    ((integerp result)
     (format nil "~D" result))

    ;; Mutation log
    ((mutation-log-p result)
     (format-mutation-log result))

    ;; Scaffold result
    ((scaffold-result-p result)
     (format-scaffold-result result))

    ;; List of strings (from :ids)
    ((and (listp result) (every #'stringp result))
     (format nil "~D task~:P:~%~{- ~A~%~}"
             (length result) result))

    ;; Group-by result (struct-based, no heuristic needed)
    ((group-by-result-p result)
     (let ((groups (group-by-result-groups result)))
       (with-output-to-string (s)
         (format s "~D group~:P:~%" (length groups))
         (dolist (group groups)
           (let ((key (car group))
                 (nodes (cdr group)))
             (format s "~%## ~A (~D)~%" key (length nodes))
             (dolist (entry nodes)
               (format s "~A~%" (format-node-entry entry))))))))

    ;; Edges result: ((id . ((:forward type target) ...)) ...)
    ((and (listp result) (every #'consp result)
          (let ((first-val (cdar result)))
            (and (listp first-val) (consp (car first-val))
                 (member (caar first-val) '(:forward :reverse)))))
     (with-output-to-string (s)
       (format s "~D node~:P:~%" (length result))
       (dolist (entry result)
         (format s "~%~A:~%" (car entry))
         (dolist (edge (cdr entry))
           (format s "  ~A ~A -> ~A~%" (first edge) (second edge) (third edge))))))

    ;; Node-set
    ((and (listp result) (every #'consp result))
     (format nil "~D task~:P:~%~{~A~%~}"
             (length result)
             (mapcar #'format-node-entry result)))

    ;; Other
    (t
     (format nil "~S" result))))

(defun format-mutation-log (log)
  "Format a mutation-log for display."
  (with-output-to-string (s)
    (let ((entries (mutation-log-entries log))
          (skipped (mutation-log-skipped log))
          (errors (mutation-log-errors log)))
      (format s "Mutation log: ~D operation~:P" (length entries))
      (when (> skipped 0)
        (format s ", ~D skipped" skipped))
      (when (> errors 0)
        (format s ", ~D error~:P" errors))
      (format s "~%")
      (dolist (entry entries)
        (format s "  ~A ~A: ~A~%"
                (if (eq (mutation-entry-result entry) :success) "✓"
                    (if (eq (mutation-entry-result entry) :skipped) "○" "✗"))
                (mutation-entry-node-id entry)
                (mutation-entry-message entry))))))

(defun format-scaffold-result (result)
  "Format a scaffold-result for display."
  (with-output-to-string (s)
    (format s "Scaffolded ~D phase~:P under ~A:~%"
            (length (scaffold-result-created result))
            (scaffold-result-parent result))
    (dolist (pair (scaffold-result-created result))
      (format s "  ✓ ~A -> ~A~%" (car pair) (cdr pair)))
    (when (scaffold-result-edges result)
      (format s "~%Dependencies wired:~%")
      (dolist (edge (scaffold-result-edges result))
        (format s "  ~A -> ~A (~A)~%"
                (first edge) (second edge) (third edge))))
    (when (scaffold-result-metadata result)
      (format s "~%Metadata set:~%")
      (dolist (pair (scaffold-result-metadata result))
        (format s "  ~A: ~D key~:P~%" (car pair) (cdr pair))))))

(defun format-node-entry (entry)
  "Format a single node entry for display."
  (let ((id (car entry))
        (props (cdr entry)))
    (with-output-to-string (s)
      (format s "- ~A" id)
      ;; Show display-name if different from id
      (let ((display (getf props :display-name)))
        (when (and display (not (string= display id)))
          (format s " [~A]" display)))
      ;; Show status
      (let ((status (or (getf props :crdt-status) (getf props :status))))
        (when status (format s " (~A)" status)))
      ;; Show counts if enriched - only print fields that are present
      (let ((obs (getf props :obs-count))
            (edges (getf props :edge-count))
            (sessions (getf props :session-count)))
        (when obs (format s " obs=~D" obs))
        (when edges (format s " edges=~D" edges))
        (when sessions (format s " sessions=~D" sessions)))
      ;; Show extra metadata properties not in the standard set
      (let ((standard '(:display-name :crdt-status :status :obs-count
                        :edge-count :session-count :alpha :entropy
                        :organized :affinity :topic)))
        (loop for (key val) on props by #'cddr
              when (and val (not (member key standard)))
              do (format s "~%    ~(~A~): ~A" key val))))))
