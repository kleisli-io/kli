(in-package #:task-tests)

;;; --- Markov Category Enrichment Tests ---
;;;
;;; Tests for lib/task/markov.lisp functions.
;;; Uses synthetic event streams for deterministic testing,
;;; plus real depot data for integration tests.

;;; Helper: build a minimal event stream with known properties
(defun make-test-events (&key (obs-count 3) (tool-count 5) (fork-count 1) (sessions '("s1")))
  "Build a deterministic event stream for testing markov functions.
   Returns list of events with known type distribution."
  (let ((events nil)
        (ts 100)
        (session (first sessions)))
    ;; Task creation
    (push (make-event :id "e-create" :timestamp ts :session session
                      :clock (make-vector-clock) :type :task.create
                      :data (list :name "test-task" :description "test"))
          events)
    (incf ts 10)
    ;; Session joins for all sessions
    (dolist (s sessions)
      (push (make-event :id (format nil "e-join-~A" s) :timestamp ts :session s
                        :clock (make-vector-clock) :type :session.join)
            events)
      (incf ts 10))
    ;; Tool calls
    (dotimes (i tool-count)
      (push (make-event :id (format nil "e-tool-~D" i) :timestamp ts :session session
                        :clock (make-vector-clock) :type :tool.call
                        :data (list :tool "Edit" :file-path (format nil "src/file-~D.lisp" i)))
            events)
      (incf ts 10))
    ;; Observations
    (dotimes (i obs-count)
      (push (make-event :id (format nil "e-obs-~D" i) :timestamp ts :session session
                        :clock (make-vector-clock) :type :observation
                        :data (list :text (format nil "finding ~D" i)))
            events)
      (incf ts 10))
    ;; Forks
    (dotimes (i fork-count)
      (push (make-event :id (format nil "e-fork-~D" i) :timestamp ts :session session
                        :clock (make-vector-clock) :type :task.fork
                        :data (list :child-id (format nil "child-~D" i) :edge-type "phase-of"))
            events)
      (incf ts 10))
    (nreverse events)))

;;; --- Action Functor ---

(test action-functor-basic
  "Action functor computes α = φ/Q correctly."
  (let* ((events (make-test-events :obs-count 3 :tool-count 5 :fork-count 2))
         (state (compute-state events)))
    (multiple-value-bind (alpha phi q) (action-functor events state)
      ;; Q = total events
      (is (= q (length events)))
      ;; phi = obs + files + edges
      (is (= phi (+ (gs-count (task-state-observations state))
                     (length (ors-members (task-state-files-touched state)))
                     (length (ors-members (task-state-edges state))))))
      ;; alpha is phi/q
      (is (< (abs (- alpha (float (/ phi q)))) 0.001))
      ;; alpha should be positive
      (is (> alpha 0.0)))))

(test action-functor-empty-events
  "Action functor handles single-event case without division by zero."
  (let* ((events (list (make-event :id "e1" :timestamp 100 :session "s1"
                                   :clock (make-vector-clock) :type :task.create
                                   :data (list :name "minimal" :description "t"))))
         (state (compute-state events)))
    (multiple-value-bind (alpha phi q) (action-functor events state)
      (is (= q 1))
      (is (>= alpha 0.0))
      (is (>= phi 0)))))

;;; --- Event Markov Kernel ---

(test compute-event-markov-kernel-basic
  "Markov kernel computes transition probabilities summing to 1 per source."
  (let* ((events (make-test-events :obs-count 3 :tool-count 5 :fork-count 1))
         (kernel (compute-event-markov-kernel events))
         (source-sums (make-hash-table :test 'equal)))
    ;; Kernel should be non-empty
    (is (plusp (hash-table-count kernel)))
    ;; Sum probabilities per source type
    (maphash (lambda (pair prob)
               (incf (gethash (car pair) source-sums 0.0) prob))
             kernel)
    ;; Each source type's outgoing probabilities should sum to ~1.0
    (maphash (lambda (source total)
               (declare (ignore source))
               (is (< (abs (- total 1.0)) 0.01)
                   "Outgoing probabilities should sum to 1.0"))
             source-sums)))

(test compute-event-markov-kernel-empty
  "Markov kernel on empty or single event returns empty hash."
  (let ((kernel (compute-event-markov-kernel nil)))
    (is (zerop (hash-table-count kernel))))
  (let* ((events (list (make-event :id "e1" :timestamp 100 :session "s1"
                                   :clock (make-vector-clock) :type :task.create
                                   :data (list :name "t" :description "t"))))
         (kernel (compute-event-markov-kernel events)))
    (is (zerop (hash-table-count kernel)))))

;;; --- Shannon Entropy ---

(test event-type-entropy-basic
  "Entropy is non-negative and bounded by log₂(|types|)."
  (let* ((events (make-test-events :obs-count 3 :tool-count 5 :fork-count 2))
         (h (event-type-entropy events)))
    (is (>= h 0.0))
    ;; Entropy bounded by log₂ of number of distinct types
    (let ((n-types (length (remove-duplicates (mapcar #'event-type events)))))
      (is (<= h (log n-types 2.0))))))

(test event-type-entropy-uniform
  "Entropy is maximized when all types are equally likely."
  ;; Build events with exactly 2 types, equally distributed
  (let* ((events (list (make-event :id "e1" :timestamp 100 :session "s1"
                                   :clock (make-vector-clock) :type :observation)
                       (make-event :id "e2" :timestamp 200 :session "s1"
                                   :clock (make-vector-clock) :type :tool.call)))
         (h (event-type-entropy events)))
    ;; For 2 equally likely types, H = 1.0 bit
    (is (< (abs (- h 1.0)) 0.01))))

(test event-type-entropy-empty
  "Entropy of empty events is 0."
  (is (= 0.0 (event-type-entropy nil))))

;;; --- Mutual Information ---

(test mutual-information-non-negative
  "Mutual information is always non-negative."
  (let* ((events (make-test-events :obs-count 5 :tool-count 10 :fork-count 2))
         (mi (mutual-information-bigrams events)))
    (is (>= mi 0.0))))

(test mutual-information-empty
  "MI of empty or single event is 0."
  (is (= 0.0 (mutual-information-bigrams nil)))
  (is (= 0.0 (mutual-information-bigrams
               (list (make-event :id "e1" :timestamp 100 :session "s1"
                                 :clock (make-vector-clock) :type :task.create
                                 :data (list :name "t" :description "t")))))))

;;; --- Organization Indicator ---

(test organization-indicator-below-threshold
  "Task with fewer than M_c observations is not organized."
  (let* ((events (make-test-events :obs-count 2 :tool-count 5))
         (state (compute-state events))
         (result (organization-indicator events state :critical-memory 5)))
    (is (not (getf result :organized)))
    (is (plusp (getf result :deficit)))))

(test organization-indicator-above-threshold
  "Task with >= M_c observations is organized."
  (let* ((events (make-test-events :obs-count 8 :tool-count 5))
         (state (compute-state events))
         (result (organization-indicator events state :critical-memory 5)))
    (is-true (getf result :organized))
    (is (minusp (getf result :deficit)))
    (is (= 8 (getf result :obs-count)))))

(test organization-indicator-has-entropy
  "Organization indicator includes entropy and MI."
  (let* ((events (make-test-events :obs-count 5 :tool-count 5))
         (state (compute-state events))
         (result (organization-indicator events state)))
    (is (numberp (getf result :entropy)))
    (is (numberp (getf result :mi)))
    (is (>= (getf result :entropy) 0.0))
    (is (>= (getf result :mi) 0.0))))

;;; --- Task Features ---

(test task-features-returns-plist
  "task-features returns plist with all expected keys."
  (let* ((events (make-test-events :obs-count 3 :tool-count 5 :fork-count 2))
         (state (compute-state events))
         (feats (task-features events state)))
    (is (numberp (getf feats :alpha)))
    (is (numberp (getf feats :obs-density)))
    (is (numberp (getf feats :sessions)))
    (is (numberp (getf feats :edges)))
    (is (numberp (getf feats :phase-of-ratio)))))

(test task-features-phase-of-ratio
  "phase-of-ratio correctly reflects edge composition."
  ;; All edges are phase-of from make-test-events
  (let* ((events (make-test-events :fork-count 3))
         (state (compute-state events))
         (feats (task-features events state)))
    ;; All forks create phase-of edges
    (is (= 1.0 (getf feats :phase-of-ratio)))))

;;; --- Affinity Score ---

(test affinity-score-in-range
  "Affinity score is in [0, 1]."
  (let* ((events (make-test-events :obs-count 5 :tool-count 10 :fork-count 3))
         (state (compute-state events))
         (score (affinity-score events state)))
    (is (>= score 0.0))
    (is (<= score 1.0))))

(test affinity-score-custom-weights
  "Affinity score respects custom weights."
  (let* ((events (make-test-events :obs-count 5 :tool-count 10 :fork-count 3))
         (state (compute-state events))
         ;; Weight only alpha at 100%
         (score-alpha (affinity-score events state
                                      '((:alpha . 1.0) (:edges . 0.0)
                                        (:phase-of-ratio . 0.0) (:obs-density . 0.0))))
         ;; Weight only edges at 100%
         (score-edges (affinity-score events state
                                      '((:alpha . 0.0) (:edges . 1.0)
                                        (:phase-of-ratio . 0.0) (:obs-density . 0.0)))))
    ;; Different weights should produce different scores
    (is (/= score-alpha score-edges))))

;;; --- Cosine Similarity ---

(test cosine-similarity-identical
  "Identical vectors have similarity 1.0."
  (is (< (abs (- 1.0 (cosine-similarity (vector 1.0 2.0 3.0) (vector 1.0 2.0 3.0)))) 0.001)))

(test cosine-similarity-orthogonal
  "Orthogonal vectors have similarity 0.0."
  (is (< (abs (cosine-similarity (vector 1.0 0.0) (vector 0.0 1.0))) 0.001)))

(test cosine-similarity-parallel
  "Parallel vectors (scaled) have similarity 1.0."
  (is (< (abs (- 1.0 (cosine-similarity (vector 1.0 2.0 3.0) (vector 2.0 4.0 6.0)))) 0.001)))

(test cosine-similarity-zero-vector
  "Zero vector returns 0.0 (no division by zero)."
  (is (= 0.0 (cosine-similarity (vector 0.0 0.0) (vector 1.0 1.0)))))

;;; --- Session Fingerprinting ---

(test fingerprint-to-vector-dimensions
  "Fingerprint vector has 6 dimensions."
  (let ((fp (list :tasks '("t1")
                  :type-counts (let ((ht (make-hash-table :test 'equal)))
                                 (setf (gethash :tool.call ht) 10
                                       (gethash :observation ht) 5)
                                 ht)
                  :total 15 :obs 5 :forks 0)))
    (let ((vec (fingerprint-to-vector fp)))
      (is (= 6 (length vec)))
      ;; All values should be non-negative
      (dotimes (i 6)
        (is (>= (aref vec i) 0.0))))))

(test classify-session-archetype-builder
  "High tool% classified as builder."
  (let ((fp (list :tasks '("t1")
                  :type-counts (let ((ht (make-hash-table :test 'equal)))
                                 (setf (gethash :tool.call ht) 50
                                       (gethash :observation ht) 5
                                       (gethash :session.join ht) 2)
                                 ht)
                  :total 57 :obs 5 :forks 0)))
    (is (eq :builder (classify-session-archetype fp)))))

(test classify-session-archetype-observer
  "Low tool% classified as observer."
  (let ((fp (list :tasks '("t1")
                  :type-counts (let ((ht (make-hash-table :test 'equal)))
                                 (setf (gethash :tool.call ht) 5
                                       (gethash :observation ht) 20
                                       (gethash :session.join ht) 10)
                                 ht)
                  :total 35 :obs 20 :forks 0)))
    (is (eq :observer (classify-session-archetype fp)))))

;;; --- Bisimulation Quotient ---

(test bisimulation-quotient-identical-sessions
  "Identical sessions collapse to one equivalence class."
  (let ((fps (make-hash-table :test 'equal)))
    ;; Two sessions with identical fingerprints
    (let ((fp-template (list :tasks '("t1")
                             :type-counts (let ((ht (make-hash-table :test 'equal)))
                                            (setf (gethash :tool.call ht) 50
                                                  (gethash :observation ht) 10)
                                            ht)
                             :total 60 :obs 10 :forks 0)))
      (setf (gethash "s1" fps) fp-template)
      ;; Same counts → same fingerprint → same class
      (let ((fp2 (list :tasks '("t1")
                       :type-counts (let ((ht (make-hash-table :test 'equal)))
                                      (setf (gethash :tool.call ht) 50
                                            (gethash :observation ht) 10)
                                      ht)
                       :total 60 :obs 10 :forks 0)))
        (setf (gethash "s2" fps) fp2)))
    (let ((result (bisimulation-quotient fps :threshold 0.99)))
      (is (= 1 (getf result :classes)))
      (is (= 2 (getf result :total-sessions)))
      (is (= 2.0 (getf result :reduction))))))

(test bisimulation-quotient-different-sessions
  "Very different sessions form separate classes."
  (let ((fps (make-hash-table :test 'equal)))
    ;; Builder session: high tool%
    (setf (gethash "builder" fps)
          (list :tasks '("t1")
                :type-counts (let ((ht (make-hash-table :test 'equal)))
                               (setf (gethash :tool.call ht) 90
                                     (gethash :observation ht) 5
                                     (gethash :session.join ht) 5)
                               ht)
                :total 100 :obs 5 :forks 0))
    ;; Observer session: high obs%
    (setf (gethash "observer" fps)
          (list :tasks '("t1" "t2" "t3" "t4" "t5")
                :type-counts (let ((ht (make-hash-table :test 'equal)))
                               (setf (gethash :tool.call ht) 2
                                     (gethash :observation ht) 80
                                     (gethash :session.join ht) 18)
                               ht)
                :total 100 :obs 80 :forks 0))
    (let ((result (bisimulation-quotient fps :threshold 0.95)))
      (is (= 2 (getf result :classes)))
      (is (= 1.0 (getf result :reduction))))))

;;; --- Load All Tasks ---

(test load-all-tasks-returns-hash
  "load-all-tasks returns hash and count."
  (with-task-roots
    (multiple-value-bind (tasks count) (load-all-tasks :min-events 10)
      (is (hash-table-p tasks))
      (is (= count (hash-table-count tasks)))
      (is (plusp count))
      ;; Each entry has :events :state :depot
      (let ((first-val nil))
        (maphash (lambda (k v) (declare (ignore k)) (unless first-val (setf first-val v))) tasks)
        (is (listp (getf first-val :events)))
        (is (not (null (getf first-val :state))))
        (is (stringp (getf first-val :depot)))))))

;;; --- TTL Cache ---

(test cache-stores-and-retrieves
  "get-cached-markov-data stores and retrieves values."
  (clear-markov-cache)
  (let ((calls 0))
    (get-cached-markov-data :test-key (lambda () (incf calls) 42))
    (is (= 1 calls))
    ;; Second call should use cache
    (let ((result (get-cached-markov-data :test-key (lambda () (incf calls) 99))))
      (is (= 1 calls) "Compute function should not be called again")
      (is (= 42 result) "Should return cached value, not recomputed")))
  (clear-markov-cache))

(test cache-invalidation-selective
  "invalidate-markov-cache removes specific key."
  (clear-markov-cache)
  (get-cached-markov-data :key-a (lambda () 1))
  (get-cached-markov-data :key-b (lambda () 2))
  (is (= 2 (hash-table-count *markov-cache*)))
  (invalidate-markov-cache :key-a)
  (is (= 1 (hash-table-count *markov-cache*)))
  ;; key-b still present
  (is (= 2 (get-cached-markov-data :key-b (lambda () 99))))
  (clear-markov-cache))

(test cache-clear-removes-all
  "clear-markov-cache removes all entries."
  (clear-markov-cache)
  (get-cached-markov-data :x (lambda () 1))
  (get-cached-markov-data :y (lambda () 2))
  (clear-markov-cache)
  (is (zerop (hash-table-count *markov-cache*))))

(test cache-ttl-respected
  "Expired entries are recomputed."
  (clear-markov-cache)
  (let ((*markov-cache-ttl* 0))  ;; 0 second TTL = always expired
    (let ((calls 0))
      (get-cached-markov-data :ttl-test (lambda () (incf calls) :v1))
      (get-cached-markov-data :ttl-test (lambda () (incf calls) :v2))
      ;; Both calls should have computed (TTL=0 means always stale)
      (is (= 2 calls))))
  (clear-markov-cache))

(test cached-task-metrics-structure
  "cached-task-metrics returns hash with expected metric keys."
  (with-task-roots
    (clear-markov-cache)
    (let ((metrics (cached-task-metrics)))
      (is (hash-table-p metrics))
      (is (plusp (hash-table-count metrics)))
      ;; Check a sample entry has all expected keys
      (let ((sample nil))
        (maphash (lambda (k v) (declare (ignore k)) (unless sample (setf sample v))) metrics)
        (is (numberp (getf sample :alpha)))
        (is (numberp (getf sample :entropy)))
        (is (numberp (getf sample :affinity)))
        (is (member (getf sample :organized) '(t nil)))))
    (clear-markov-cache)))

;;; --- Integration: Real Depot Data ---

(test markov-functions-with-real-data
  "Markov functions work on real depot data without error."
  (with-task-roots
    (let* ((all (scan-all-depot-tasks))
           ;; Pick first active task with events
           (target (loop for info in all
                         for id = (getf info :id)
                         when (getf info :has-events) return id)))
      (when target
        (let* ((ep (merge-pathnames "events.jsonl" (task-directory target)))
               (elog (elog-load ep))
               (events (event-log-events elog))
               (state (compute-state events)))
          (when (> (length events) 5)
            ;; All functions should return without error
            (multiple-value-bind (alpha phi q) (action-functor events state)
              (is (numberp alpha))
              (is (integerp phi))
              (is (integerp q)))
            (let ((h (event-type-entropy events)))
              (is (>= h 0.0)))
            (let ((mi (mutual-information-bigrams events)))
              (is (>= mi 0.0)))
            (let ((org (organization-indicator events state)))
              (is (listp org))
              (is (member :organized org)))
            (let ((feats (task-features events state)))
              (is (numberp (getf feats :alpha))))
            (let ((score (affinity-score events state)))
              (is (>= score 0.0))
              (is (<= score 1.0)))
            (let ((kernel (compute-event-markov-kernel events)))
              (is (hash-table-p kernel)))))))))
