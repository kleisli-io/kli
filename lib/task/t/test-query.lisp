(in-package #:task-tests)

;;; ============================================================
;;; TQ QUERY LANGUAGE TESTS
;;; Tests use mock task-graphs to avoid filesystem dependency.
;;; ============================================================

(def-suite :tq-tests
  :description "Tests for TQ query language"
  :in :task-tests)

(in-suite :tq-tests)

;;; --- Test Helpers ---

(defun make-test-graph (&key nodes edges)
  "Build a task-graph from specs.
   NODES: list of (id &rest props)
   EDGES: list of (from to edge-type-keyword)
   Nodes without explicit props get (:status :event-sourced) as default
   so graph-node-props returns non-nil (required by follow/back)."
  (let ((node-ht (make-hash-table :test 'equal))
        (forward (make-hash-table :test 'equal))
        (reverse-ht (make-hash-table :test 'equal)))
    (dolist (spec nodes)
      (setf (gethash (first spec) node-ht)
            (or (rest spec) (list :status :event-sourced))))
    (dolist (spec edges)
      (destructuring-bind (from to edge-type) spec
        (push (list to edge-type) (gethash from forward))
        (push (list from edge-type) (gethash to reverse-ht))))
    (make-task-graph :nodes node-ht :forward forward :reverse reverse-ht)))

(defmacro with-test-graph ((&key nodes edges (current nil current-p)) &body body)
  "Execute BODY with *graph* bound to a mock graph."
  `(let* ((*graph* (make-test-graph :nodes ',nodes :edges ',edges))
          ,@(when current-p `((*current-task-id* ,current))))
     ,@body))

(defun ids-of (node-set)
  "Extract sorted IDs from a node-set for comparison."
  (sort (mapcar #'car node-set) #'string<))

;;; --- Starting Expressions ---

(test tq-all-nodes
  "all-nodes returns all nodes in the graph."
  (with-test-graph (:nodes (("a" :topic "x") ("b" :topic "y") ("c" :topic "z")))
    (is (= 3 (length (all-nodes))))
    (is (equal '("a" "b" "c") (ids-of (all-nodes))))))

(test tq-node-pattern-match
  "node returns nodes matching substring pattern."
  (with-test-graph (:nodes (("core:fix-bug" :topic "fix")
                            ("core:fix-typo" :topic "fix")
                            ("core:add-feature" :topic "add")))
    (is (= 2 (length (node "fix"))))
    (is (= 1 (length (node "feature"))))
    (is (= 3 (length (node "core"))))))

(test tq-node-multi-pattern
  "node with multiple patterns unions results."
  (with-test-graph (:nodes (("a" :topic "x") ("b" :topic "y") ("c" :topic "z")))
    (let ((result (interpret-query (safe-read-query "(node \"a\" \"c\")"))))
      (is (= 2 (length result)))
      (is (member "a" (mapcar #'car result) :test #'string=))
      (is (member "c" (mapcar #'car result) :test #'string=)))))

(test tq-active-dormant
  "active and dormant filter by :status property."
  (with-test-graph (:nodes (("a" :status :event-sourced)
                            ("b" :status :dormant)
                            ("c" :status :event-sourced)))
    (is (= 2 (length (active))))
    (is (= 1 (length (dormant))))
    (is (equal '("b") (ids-of (dormant))))))

;;; --- Pipeline Primitives ---

(test tq-where-step
  "where-step filters by predicate."
  (with-test-graph (:nodes (("a" :topic "lisp") ("b" :topic "nix") ("c" :topic "lisp")))
    (let ((result (where-step (all-nodes)
                              (lambda (id props)
                                (declare (ignore id))
                                (equal (getf props :topic) "lisp")))))
      (is (= 2 (length result)))
      (is (equal '("a" "c") (ids-of result))))))

(test tq-select-fields
  "select-fields projects only specified fields."
  (with-test-graph (:nodes (("a" :topic "lisp" :count 5 :extra "x")))
    (let* ((result (select-fields (all-nodes) :topic :count))
           (props (cdr (first result))))
      (is (equal "lisp" (getf props :topic)))
      (is (= 5 (getf props :count)))
      (is (null (getf props :extra))))))

(test tq-sort-by-field-numeric
  "sort-by-field correctly sorts numeric values descending."
  (with-test-graph (:nodes (("a" :count 5) ("b" :count 100) ("c" :count 10)))
    (let ((result (sort-by-field (all-nodes) :count)))
      (is (equal '("b" "c" "a") (mapcar #'car result))))))

(test tq-sort-by-field-nil-first-node
  "sort-by-field handles NIL as first node value (regression: Bug 1)."
  (with-test-graph (:nodes (("a" :count nil) ("b" :count 10) ("c" :count 5) ("d" :count 100)))
    (let ((result (sort-by-field (all-nodes) :count)))
      ;; Numeric detection scans past nil to find numberp value
      ;; Descending: 100, 10, 5, nil(→0)
      (is (equal "d" (car (first result))))
      (is (equal "a" (car (fourth result)))))))

(test tq-sort-by-field-all-nil
  "sort-by-field with all nil values doesn't error."
  (with-test-graph (:nodes (("a" :count nil) ("b" :count nil)))
    (is (= 2 (length (sort-by-field (all-nodes) :count))))))

(test tq-sort-by-field-string
  "sort-by-field sorts string values."
  (with-test-graph (:nodes (("a" :topic "nix") ("b" :topic "ace") ("c" :topic "lisp")))
    (let ((result (sort-by-field (all-nodes) :topic :descending t)))
      (is (equal "nix" (getf (cdr (first result)) :topic))))))

(test tq-sort-by-field-ascending
  "sort-by-field ascending with nil puts nil (0) first."
  (with-test-graph (:nodes (("a" :count nil) ("b" :count 10) ("c" :count 5)))
    (let ((result (sort-by-field (all-nodes) :count :descending nil)))
      (is (equal "a" (car (first result))))
      (is (equal "c" (car (second result)))))))

(test tq-sort-by-field-empty
  "sort-by-field on empty node-set returns nil."
  (is (null (sort-by-field nil :count))))

(test tq-take-n
  "take-n limits result size."
  (with-test-graph (:nodes (("a") ("b") ("c") ("d")))
    (is (= 2 (length (take-n (all-nodes) 2))))
    (is (= 4 (length (take-n (all-nodes) 10))))))

(test tq-ids-step
  "ids-step extracts just IDs."
  (with-test-graph (:nodes (("a" :topic "x") ("b" :topic "y")))
    (let ((ids (ids-step (all-nodes))))
      (is (every #'stringp ids))
      (is (= 2 (length ids))))))

(test tq-count-step
  "count-step returns integer count."
  (with-test-graph (:nodes (("a") ("b") ("c")))
    (is (= 3 (count-step (all-nodes))))
    (is (= 0 (count-step nil)))))

;;; --- Graph Traversal ---

(test tq-follow
  "follow traverses forward edges."
  (with-test-graph (:nodes (("parent") ("child-1") ("child-2") ("other"))
                   :edges (("parent" "child-1" :phase-of)
                           ("parent" "child-2" :phase-of)
                           ("parent" "other" :depends-on)))
    (let ((phases (follow (node "parent") :phase-of)))
      (is (= 2 (length phases)))
      (is (equal '("child-1" "child-2") (ids-of phases))))
    (let ((deps (follow (node "parent") :depends-on)))
      (is (= 1 (length deps)))
      (is (equal '("other") (ids-of deps))))))

(test tq-back
  "back traverses reverse edges."
  (with-test-graph (:nodes (("parent") ("child"))
                   :edges (("parent" "child" :phase-of)))
    (let ((parents (back (node "child") :phase-of)))
      (is (= 1 (length parents)))
      (is (equal '("parent") (ids-of parents))))))

(test tq-follow-no-matching-edges
  "follow returns empty for non-matching edge types."
  (with-test-graph (:nodes (("a") ("b"))
                   :edges (("a" "b" :phase-of)))
    (is (null (follow (node "a") :depends-on)))))

;;; --- Group-by ---

(test tq-group-by-step-basic
  "group-by-step returns group-by-result struct."
  (with-test-graph (:nodes (("a" :topic "lisp") ("b" :topic "nix") ("c" :topic "lisp")))
    (let ((result (group-by-step (all-nodes) :topic)))
      (is (group-by-result-p result))
      (is (= 2 (length (group-by-result-groups result))))
      ;; "lisp" group has 2 items, "nix" has 1 — sorted by size descending
      (let ((first-group (first (group-by-result-groups result))))
        (is (equal "lisp" (car first-group)))
        (is (= 2 (length (cdr first-group))))))))

(test tq-group-by-step-nil-field
  "group-by-step uses :nil for missing fields."
  (with-test-graph (:nodes (("a" :topic "lisp") ("b")))
    (let* ((result (group-by-step (all-nodes) :topic))
           (groups (group-by-result-groups result))
           (nil-group (find :nil groups :key #'car)))
      (is (not (null nil-group)))
      (is (= 1 (length (cdr nil-group)))))))

(test tq-group-by-step-empty
  "group-by-step on empty node-set returns empty result."
  (let ((result (group-by-step nil :topic)))
    (is (group-by-result-p result))
    (is (null (group-by-result-groups result)))))

;;; --- Set Operations ---

(test tq-node-union
  "node-union combines two node-sets, deduplicating."
  (with-test-graph (:nodes (("a") ("b") ("c")))
    (let ((result (node-union (node "a") (node "b"))))
      (is (= 2 (length result))))
    ;; Union with overlap deduplicates
    (let ((result (node-union (all-nodes) (node "a"))))
      (is (= 3 (length result))))))

(test tq-node-intersection
  "node-intersection returns common nodes."
  (with-test-graph (:nodes (("a" :status :event-sourced) ("b" :status :dormant) ("c" :status :event-sourced)))
    (let ((result (node-intersection (active) (node "a"))))
      (is (= 1 (length result)))
      (is (equal '("a") (ids-of result))))))

(test tq-node-difference
  "node-difference removes second set from first."
  (with-test-graph (:nodes (("a") ("b") ("c")))
    (let ((result (node-difference (all-nodes) (node "b"))))
      (is (= 2 (length result)))
      (is (not (member "b" (mapcar #'car result) :test #'string=))))))

;;; --- Interpreter (safe-read-query + interpret-query) ---

(test tq-interpret-pipeline
  "Full pipeline via interpreter."
  (with-test-graph (:nodes (("a" :topic "lisp" :count 10)
                            ("b" :topic "nix" :count 20)
                            ("c" :topic "lisp" :count 5)))
    (is (= 3 (interpret-query (safe-read-query "(-> :all :count)"))))
    (is (= 2 (length (interpret-query (safe-read-query "(-> :all (:where (= :topic \"lisp\")))")))))
    (let ((sorted (interpret-query (safe-read-query "(-> :all (:sort :count) (:take 2))"))))
      (is (= 2 (length sorted)))
      (is (equal "b" (car (first sorted)))))))

(test tq-interpret-set-operations
  "Set operations via interpreter."
  (with-test-graph (:nodes (("a") ("b") ("c")))
    (is (= 2 (length (interpret-query (safe-read-query "(union (node \"a\") (node \"b\"))")))))
    (is (= 2 (length (interpret-query (safe-read-query "(minus :all (node \"c\"))")))))
    (is (= 1 (length (interpret-query (safe-read-query "(intersect (node \"a\") :all)")))))))

(test tq-interpret-where-predicates
  "All predicate types via interpreter."
  (with-test-graph (:nodes (("core:fix-bug" :topic "lisp" :count 5)
                            ("core:add-feat" :topic "nix")
                            ("core:fix-typo" :topic "lisp" :count 10)))
    ;; (= :field val)
    (is (= 2 (length (interpret-query (safe-read-query "(-> :all (:where (= :topic \"lisp\")))")))))
    ;; (has :field)
    (is (= 2 (length (interpret-query (safe-read-query "(-> :all (:where (has :count)))")))))
    ;; (matches "pattern")
    (is (= 2 (length (interpret-query (safe-read-query "(-> :all (:where (matches \"fix\")))")))))
    ;; (and ...)
    (is (= 1 (length (interpret-query (safe-read-query "(-> :all (:where (and (= :topic \"lisp\") (matches \"bug\"))))")))))
    ;; (or ...)
    (is (= 3 (length (interpret-query (safe-read-query "(-> :all (:where (or (= :topic \"lisp\") (= :topic \"nix\"))))")))))
    ;; (not ...)
    (is (= 1 (length (interpret-query (safe-read-query "(-> :all (:where (not (= :topic \"lisp\"))))")))))))

(test tq-interpret-group-by
  "group-by via interpreter returns group-by-result."
  (with-test-graph (:nodes (("a" :topic "lisp") ("b" :topic "nix") ("c" :topic "lisp")))
    (let ((result (interpret-query (safe-read-query "(-> :all (:group-by :topic))"))))
      (is (group-by-result-p result)))))

;;; --- Error Handling ---

(test tq-error-unknown-query
  "Unknown named query signals tq-error."
  (with-test-graph (:nodes (("a")))
    (signals tq-error
      (interpret-query (safe-read-query "(query \"nonexistent\")")))))

(test tq-error-unknown-expression
  "Unknown expression form signals tq-error."
  (with-test-graph (:nodes (("a")))
    (signals tq-error
      (interpret-query (safe-read-query "(bogus-form)")))))

(test tq-error-unknown-step
  "Unknown pipeline step signals tq-error."
  (with-test-graph (:nodes (("a")))
    (signals tq-error
      (interpret-query (safe-read-query "(-> :all (:unknown-step))")))))

(test tq-error-parse
  "Malformed query string signals tq-parse-error."
  (signals tq-parse-error
    (safe-read-query "(unclosed paren")))

(test tq-error-mutation-without-handler
  "Mutation without *mutation-handler* signals condition."
  (with-test-graph (:nodes (("a")))
    (let ((*mutation-handler* nil))
      (signals mutation-without-handler
        (interpret-query (safe-read-query "(-> (node \"a\") (:complete!))"))))))

(test tq-error-current-without-context
  "(current) without *current-task-id* signals tq-error."
  (with-test-graph (:nodes (("a")))
    (let ((*current-task-id* nil))
      (signals tq-error
        (interpret-query (safe-read-query "(current)"))))))

;;; --- Format Results ---

(test tq-format-integer
  "format-query-result formats integer (from :count)."
  (is (string= "3" (format-query-result 3))))

(test tq-format-ids
  "format-query-result formats list of strings (from :ids)."
  (let ((result (format-query-result '("a" "b" "c"))))
    (is (search "3 task" result))
    (is (search "- a" result))))

(test tq-format-node-set
  "format-query-result formats node-set."
  (let ((result (format-query-result '(("core:task-a" :status :event-sourced)))))
    (is (search "1 task" result))
    (is (search "core:task-a" result))))

(test tq-format-group-by-result
  "format-query-result formats group-by-result struct (regression: Bug 3)."
  (let* ((gbr (tq::make-group-by-result
               :groups '(("lisp" . (("a" :topic "lisp") ("b" :topic "lisp")))
                          ("nix" . (("c" :topic "nix"))))
               :field :topic))
         (result (format-query-result gbr)))
    (is (search "2 group" result))
    (is (search "## lisp (2)" result))
    (is (search "## nix (1)" result))))

(test tq-format-empty-group-by
  "format-query-result formats empty group-by-result."
  (let* ((gbr (tq::make-group-by-result :groups nil :field :topic))
         (result (format-query-result gbr)))
    (is (search "0 group" result))))

(test tq-format-empty-node-set
  "format-query-result formats empty node-set."
  (is (search "0 task" (format-query-result nil))))

;;; --- Enrich Step: Markov Fields ---

(test tq-enrich-adds-markov-fields
  "enrich-step adds :alpha :entropy :organized :affinity fields."
  (with-task-roots
    (let* ((all (task:scan-all-depot-tasks))
           (target (loop for info in all
                         when (getf info :has-events) return (getf info :id))))
      (when target
        (let* ((node-set (list (cons target nil)))
               (enriched (enrich-step node-set))
               (props (cdr (first enriched))))
          ;; Pre-existing fields
          (is (getf props :crdt-status))
          (is (numberp (getf props :obs-count)))
          ;; New markov fields
          (is (numberp (getf props :alpha))
              "enrich-step should add :alpha")
          (is (numberp (getf props :entropy))
              "enrich-step should add :entropy")
          (is (member (getf props :organized) '(t nil))
              "enrich-step should add :organized")
          (is (numberp (getf props :affinity))
              "enrich-step should add :affinity"))))))

(test tq-enrich-missing-task-graceful
  "enrich-step returns entry unchanged for non-existent task."
  ;; Bare ID — resolves to path but file doesn't exist
  (let* ((node-set (list (cons "nonexistent-task-99999" nil)))
         (enriched (enrich-step node-set)))
    (is (= 1 (length enriched)))
    (is (equal "nonexistent-task-99999" (car (first enriched))))
    (is (null (cdr (first enriched)))))
  ;; Qualified ID with unknown depot — path resolution errors gracefully
  (let* ((node-set (list (cons "bogus:nonexistent-task-99999" nil)))
         (enriched (enrich-step node-set)))
    (is (= 1 (length enriched)))
    (is (equal "bogus:nonexistent-task-99999" (car (first enriched))))))

;;; --- Mutation Safety (blast radius regression tests) ---

(test tq-mutation-safety-limit-blocks-large-sets
  "Mutation exceeding *mutation-safety-limit* signals mutation-safety-exceeded."
  (with-test-graph (:nodes (("a") ("b") ("c") ("d") ("e")
                            ("f") ("g") ("h") ("i") ("j") ("k")))
    (let ((*mutation-handler* (lambda (&rest args) (declare (ignore args)) "ok"))
          (tq:*mutation-safety-limit* 5))
      ;; 11 nodes > limit of 5
      (signals tq:mutation-safety-exceeded
        (interpret-query (safe-read-query "(-> :all (:complete!))"))))))

(test tq-mutation-safety-limit-allows-small-sets
  "Mutation within *mutation-safety-limit* proceeds normally."
  (with-test-graph (:nodes (("a") ("b") ("c")))
    (let ((*mutation-handler* (lambda (&rest args) (declare (ignore args)) "ok"))
          (tq:*mutation-safety-limit* 5))
      ;; 3 nodes < limit of 5
      (let ((result (interpret-query (safe-read-query "(-> :all (:complete!))"))))
        (is (tq:mutation-log-p result))
        (is (= 3 (length (tq:mutation-log-entries result))))))))

(test tq-mutation-safety-limit-nil-disables
  "Setting *mutation-safety-limit* to nil disables the check."
  (with-test-graph (:nodes (("a") ("b") ("c") ("d") ("e")
                            ("f") ("g") ("h") ("i") ("j") ("k")))
    (let ((*mutation-handler* (lambda (&rest args) (declare (ignore args)) "ok"))
          (tq:*mutation-safety-limit* nil))
      ;; 11 nodes, no limit
      (let ((result (interpret-query (safe-read-query "(-> :all (:complete!))"))))
        (is (tq:mutation-log-p result))
        (is (= 11 (length (tq:mutation-log-entries result))))))))

;;; --- Exact Node ---

(test tq-exact-node-single-match
  "exact-node returns exactly one node for exact ID match."
  (with-test-graph (:nodes (("core:2026-02-01-A" :topic "x")
                            ("core:2026-02-01-ABC" :topic "y")
                            ("core:2026-02-01-ABCD" :topic "z")))
    ;; Substring would match all 3, exact-node matches only 1
    (is (= 1 (length (exact-node "core:2026-02-01-A"))))
    (is (equal "core:2026-02-01-A" (car (first (exact-node "core:2026-02-01-A")))))))

(test tq-exact-node-no-match
  "exact-node returns empty for non-existent ID."
  (with-test-graph (:nodes (("a") ("b")))
    (is (null (exact-node "c")))
    (is (null (exact-node "ab")))))

(test tq-current-uses-exact-match
  "(current) returns exactly one node even when ID is prefix of others."
  (with-test-graph (:nodes (("core:task-A" :topic "x")
                            ("core:task-ABC" :topic "y")
                            ("core:task-ABCDEF" :topic "z"))
                   :current "core:task-A")
    (let ((result (interpret-query (safe-read-query "(current)"))))
      (is (= 1 (length result)))
      (is (equal "core:task-A" (car (first result)))))))

;;; --- Node nil/empty guard ---

(test tq-node-rejects-nil-pattern
  "(node nil) signals tq-error instead of matching everything."
  (with-test-graph (:nodes (("a") ("b") ("c")))
    (signals tq-error (node nil))))

(test tq-node-rejects-empty-pattern
  "(node \"\") signals tq-error instead of matching everything."
  (with-test-graph (:nodes (("a") ("b") ("c")))
    (signals tq-error (node ""))))
