(in-package #:pq-tests)

;;; ============================================================
;;; PQ QUERY LANGUAGE TESTS
;;; Tests use mock patterns via kli-pattern::make-pattern.
;;; ============================================================

;;; --- Test Helpers ---

(defun make-test-pattern (&key id domain content (helpful 0) (harmful 0))
  "Create a mock pattern for testing."
  (kli-pattern:make-pattern
   :id id :domain domain
   :content (or content "")
   :helpful helpful :harmful harmful))

(defun make-test-patterns ()
  "Standard set of test patterns."
  (list (make-test-pattern :id "lisp-001" :domain "lisp" :content "REPL pattern" :helpful 5)
        (make-test-pattern :id "nix-001" :domain "nix" :content "Flake pattern" :helpful 2)
        (make-test-pattern :id "ace-001" :domain "ace" :content "Research pattern" :helpful 0)
        (make-test-pattern :id "lisp-002" :domain "lisp" :content "Macro pattern" :helpful 10 :harmful 1)))

(defmacro with-test-patterns ((&optional patterns) &body body)
  "Execute BODY with *patterns* bound to test patterns."
  `(let ((pq:*patterns* ,(or patterns '(make-test-patterns))))
     ,@body))

;;; --- Sort Tests ---

(test pq-sort-numeric
  "sort-by-field correctly sorts numeric values descending."
  (with-test-patterns ()
    (let ((result (pq::sort-by-field pq:*patterns* :helpful)))
      (is (= 4 (length result)))
      (is (equal "lisp-002" (pq::pattern-id-accessor (first result))))
      (is (equal "ace-001" (pq::pattern-id-accessor (fourth result)))))))

(test pq-sort-string
  "sort-by-field correctly sorts string values (regression: was TYPE-ERROR)."
  (with-test-patterns ()
    (let ((result (pq::sort-by-field pq:*patterns* :domain :descending t)))
      (is (= 4 (length result)))
      ;; "nix" > "lisp" > "ace" alphabetically
      (is (equal "nix" (pq::pattern-domain-accessor (first result)))))))

(test pq-sort-ascending
  "sort-by-field ascending puts smallest first."
  (with-test-patterns ()
    (let ((result (pq::sort-by-field pq:*patterns* :helpful :descending nil)))
      (is (equal "ace-001" (pq::pattern-id-accessor (first result))))
      (is (equal "lisp-002" (pq::pattern-id-accessor (fourth result)))))))

(test pq-sort-empty
  "sort-by-field on empty set returns nil."
  (is (null (pq::sort-by-field nil :helpful))))

;;; --- Group-by Tests ---

(test pq-group-by-struct
  "group-by-step returns group-by-result struct."
  (with-test-patterns ()
    (let ((result (pq::group-by-step pq:*patterns* :domain)))
      (is (pq:group-by-result-p result))
      (is (= 3 (length (pq:group-by-result-groups result))))
      ;; "lisp" group has 2 items, sorted first by size
      (let ((first-group (first (pq:group-by-result-groups result))))
        (is (equal "lisp" (car first-group)))
        (is (= 2 (length (cdr first-group))))))))

;;; --- Pipeline Primitive Tests ---

(test pq-take-n
  "take-n limits result size."
  (with-test-patterns ()
    (is (= 2 (length (pq::take-n pq:*patterns* 2))))
    (is (= 4 (length (pq::take-n pq:*patterns* 10))))))

(test pq-ids-step
  "ids-step extracts just IDs as strings."
  (with-test-patterns ()
    (let ((ids (pq::ids-step pq:*patterns*)))
      (is (every #'stringp ids))
      (is (= 4 (length ids))))))

(test pq-count-step
  "count-step returns integer count."
  (with-test-patterns ()
    (is (= 4 (pq::count-step pq:*patterns*)))
    (is (= 0 (pq::count-step nil)))))

;;; --- Predicate Tests ---

(test pq-predicate-domain=
  "domain= filters by domain."
  (with-test-patterns ()
    (let ((result (pq::where-step pq:*patterns* (pq:domain= :lisp))))
      (is (= 2 (length result))))))

(test pq-predicate-field>
  "field> filters by numeric threshold."
  (with-test-patterns ()
    (let ((result (pq::where-step pq:*patterns* (pq::field> :helpful 3))))
      (is (= 2 (length result))))))

(test pq-predicate-has-field
  "has-field checks for non-nil field."
  (with-test-patterns ()
    (let ((result (pq::where-step pq:*patterns* (pq::has-field :embedding))))
      (is (= 0 (length result))))))

(test pq-predicate-content-matches
  "content-matches does substring search."
  (with-test-patterns ()
    (let ((result (pq::where-step pq:*patterns* (pq::content-matches "pattern"))))
      (is (= 4 (length result))))))

;;; --- Format Tests ---

(test pq-format-count
  "format-query-result formats integer."
  (is (equal "4" (pq:format-query-result 4))))

(test pq-format-ids
  "format-query-result formats ID list."
  (let ((result (pq:format-query-result (list "a" "b" "c"))))
    (is (search "3 pattern" result))))

(test pq-format-group-by
  "format-query-result formats group-by-result struct."
  (with-test-patterns ()
    (let* ((grouped (pq::group-by-step pq:*patterns* :domain))
           (result (pq:format-query-result grouped)))
      (is (search "3 group" result))
      (is (search "lisp" result)))))
