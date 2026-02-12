;;;; markov.lisp — Markov category enrichment for task system
;;;;
;;;; Provides probabilistic and information-theoretic metrics over tasks:
;;;; - Action functor: efficiency α = φ/Q
;;;; - Event Markov kernel: P(event-type-j | event-type-i)
;;;; - Shannon entropy and mutual information
;;;; - Organization indicator (M_c phase transition)
;;;; - Affinity scoring with learned weights
;;;; - Coalgebraic bisimulation quotient for sessions
;;;; - Active inference: missing edge detection + free energy reduction
;;;;
;;;; All functions operate on task events and state from the CRDT layer.
;;;; Validated against live data: 164 tasks, 7163 events, 214 sessions.

(in-package #:task)

;;; ============================================================
;;; Action Functor: A: I(d) → R+
;;; Maps tasks to efficiency α = φ/Q
;;; ============================================================

(defun action-functor (events state)
  "Compute action efficiency α = φ/Q for a task.
   φ = observations + files-touched + edges (meaningful output).
   Q = total events (action cost proxy).
   Returns (values alpha phi q)."
  (let* ((q (max (length events) 1))
         (obs (gs-count (task-state-observations state)))
         (files (length (ors-members (task-state-files-touched state))))
         (edges (length (ors-members (task-state-edges state))))
         (phi (+ obs files edges)))
    (values (float (/ phi q)) phi q)))

;;; ============================================================
;;; Event Markov Kernel: P(event-type-j | event-type-i)
;;; First-order transition matrix over event types within a task
;;; ============================================================

(defun compute-event-markov-kernel (events)
  "Compute first-order Markov transition matrix from event stream.
   Returns hash-table: (type-from . type-to) → probability."
  (let ((transitions (make-hash-table :test 'equal))
        (counts (make-hash-table :test 'equal))
        (sorted (sort (copy-list events) #'< :key #'event-timestamp)))
    (loop for (e1 e2) on sorted while e2
          for t1 = (event-type e1)
          for t2 = (event-type e2)
          do (incf (gethash (cons t1 t2) transitions 0))
             (incf (gethash t1 counts 0)))
    (let ((kernel (make-hash-table :test 'equal)))
      (maphash (lambda (pair count)
                 (setf (gethash pair kernel)
                       (float (/ count (gethash (car pair) counts 1)))))
               transitions)
      kernel)))

;;; ============================================================
;;; Shannon Channel Capacity
;;; Event type entropy, mutual information, organization indicator
;;; ============================================================

(defun event-type-entropy (events)
  "Compute Shannon entropy H of event type distribution in bits.
   H = -Σ p(t) log₂ p(t). Max ≈ 3.0 for ~8 event types."
  (let* ((types (mapcar #'event-type events))
         (total (float (max (length types) 1)))
         (type-counts (make-hash-table :test 'equal))
         (h 0.0))
    (dolist (ty types) (incf (gethash ty type-counts 0)))
    (maphash (lambda (ty cnt)
               (declare (ignore ty))
               (let ((p (/ cnt total)))
                 (when (> p 0) (decf h (* p (log p 2.0))))))
             type-counts)
    h))

(defun mutual-information-bigrams (events)
  "Compute mutual information I(X_t; X_{t+1}) for event type bigrams in bits.
   High MI indicates organized (predictable) session behavior."
  (let ((joint (make-hash-table :test 'equal))
        (marginal-x (make-hash-table :test 'equal))
        (marginal-y (make-hash-table :test 'equal))
        (total 0))
    (let ((sorted (sort (copy-list events) #'< :key #'event-timestamp)))
      (loop for (e1 e2) on sorted while e2
            for t1 = (event-type e1)
            for t2 = (event-type e2)
            do (incf (gethash (cons t1 t2) joint 0))
               (incf (gethash t1 marginal-x 0))
               (incf (gethash t2 marginal-y 0))
               (incf total)))
    (when (zerop total) (return-from mutual-information-bigrams 0.0))
    (let ((mi 0.0) (ftotal (float total)))
      (maphash (lambda (pair count)
                 (let* ((pxy (/ count ftotal))
                        (px (/ (gethash (car pair) marginal-x 1) ftotal))
                        (py (/ (gethash (cdr pair) marginal-y 1) ftotal)))
                   (when (and (> pxy 0) (> px 0) (> py 0))
                     (incf mi (* pxy (log (/ pxy (* px py)) 2.0))))))
               joint)
      mi)))

(defun organization-indicator (events state &key (critical-memory 5))
  "Determine if a task has passed the Shannon phase transition.
   CRITICAL-MEMORY (M_c) is the observation threshold.
   Returns plist (:organized t/nil :obs-count N :entropy H :mi MI :deficit D)
   where deficit = M_c - obs-count (negative means past transition)."
  (let* ((obs-count (gs-count (task-state-observations state)))
         (h (event-type-entropy events))
         (mi (mutual-information-bigrams events))
         (organized (>= obs-count critical-memory)))
    (list :organized organized
          :obs-count obs-count
          :entropy h
          :mi mi
          :deficit (- critical-memory obs-count))))

;;; ============================================================
;;; Affinity Scoring
;;; Feature extraction and weighted scoring for frontier ranking
;;; ============================================================

(defun task-features (events state)
  "Extract feature vector from task state for affinity scoring.
   Returns plist (:alpha :obs-density :sessions :edges :phase-of-ratio)."
  (let* ((q (max (length events) 1))
         (obs (gs-count (task-state-observations state)))
         (files (length (ors-members (task-state-files-touched state))))
         (edges-raw (ors-members (task-state-edges state)))
         (edge-types (mapcar (lambda (e) (cdr (decode-edge e))) edges-raw))
         (sessions (gs-count (task-state-sessions state)))
         (phi (+ obs files (length edges-raw)))
         (alpha (float (/ phi q)))
         (obs-density (float (* 100 (/ (count :observation (mapcar #'event-type events)) q))))
         (phase-of-ratio (if (plusp (length edges-raw))
                             (float (/ (count :phase-of edge-types) (length edges-raw)))
                             0.0)))
    (list :alpha alpha :obs-density obs-density :sessions sessions
          :edges (length edges-raw) :phase-of-ratio phase-of-ratio)))

(defun affinity-score (events state &optional weights)
  "Compute affinity score for a task using learned or provided weights.
   WEIGHTS is alist of (feature . weight). If nil, uses default weights.
   Features are normalized to [0,1] using empirical maxima.
   Returns score in [0, 1]."
  (let ((w (or weights '((:phase-of-ratio . 0.40) (:edges . 0.22)
                          (:alpha . 0.21) (:obs-density . 0.17))))
        (feats (task-features events state))
        (score 0.0)
        (norms '((:phase-of-ratio . 1.0) (:edges . 20.0)
                 (:alpha . 1.0) (:obs-density . 50.0))))
    (dolist (wt w)
      (let* ((key (car wt))
             (weight (cdr wt))
             (raw (or (getf feats key) 0.0))
             (max-val (or (cdr (assoc key norms)) 1.0))
             (normalized (min 1.0 (/ raw max-val))))
        (incf score (* weight normalized))))
    score))

;;; ============================================================
;;; Coalgebraic Bisimulation Quotient
;;; Session fingerprinting and equivalence class computation
;;; ============================================================

(defun cosine-similarity (v1 v2)
  "Cosine similarity between two vectors. Returns value in [-1, 1]."
  (let ((dot 0.0) (mag1 0.0) (mag2 0.0))
    (dotimes (i (length v1))
      (incf dot (* (aref v1 i) (aref v2 i)))
      (incf mag1 (* (aref v1 i) (aref v1 i)))
      (incf mag2 (* (aref v2 i) (aref v2 i))))
    (if (or (zerop mag1) (zerop mag2)) 0.0
        (float (/ dot (* (sqrt mag1) (sqrt mag2)))))))

(defun build-session-fingerprints (all-tasks &key (min-events 10))
  "Build behavioral fingerprints for all sessions across tasks.
   ALL-TASKS is hash: task-name → (:events events :state state :depot depot-name).
   Returns hash: session-id → (:tasks list :type-counts hash :total N :obs N :forks N).
   Sessions with fewer than MIN-EVENTS total events are excluded."
  (let ((fingerprints (make-hash-table :test 'equal)))
    (maphash (lambda (name data)
               (dolist (e (getf data :events))
                 (let ((session (event-session e)))
                   (when session
                     (let ((fp (or (gethash session fingerprints)
                                   (setf (gethash session fingerprints)
                                         (list :tasks nil
                                               :type-counts (make-hash-table :test 'equal)
                                               :total 0 :obs 0 :forks 0)))))
                       (pushnew name (getf fp :tasks) :test #'equal)
                       (incf (gethash (event-type e) (getf fp :type-counts) 0))
                       (incf (getf fp :total))
                       (when (eq (event-type e) :observation) (incf (getf fp :obs)))
                       (when (eq (event-type e) :task.fork) (incf (getf fp :forks))))))))
             all-tasks)
    (let ((filtered (make-hash-table :test 'equal)))
      (maphash (lambda (id fp)
                 (when (>= (getf fp :total) min-events)
                   (setf (gethash id filtered) fp)))
               fingerprints)
      filtered)))

(defun fingerprint-to-vector (fp)
  "Convert fingerprint plist to normalized 6-dimensional feature vector.
   Dimensions: [tool% obs% fork% join% task-count/10 total/200]."
  (let* ((tc (getf fp :type-counts))
         (total (float (max (getf fp :total) 1)))
         (tool% (/ (gethash :tool.call tc 0) total))
         (obs% (/ (gethash :observation tc 0) total))
         (fork% (/ (gethash :task.fork tc 0) total))
         (join% (/ (gethash :session.join tc 0) total))
         (task-count (length (getf fp :tasks))))
    (vector (float tool%) (float obs%) (float fork%) (float join%)
            (float (/ task-count 10.0))
            (float (/ (getf fp :total) 200.0)))))

(defun bisimulation-quotient (fingerprints &key (threshold 0.95))
  "Compute bisimulation quotient Agent/~ using cosine similarity threshold.
   FINGERPRINTS is hash from build-session-fingerprints.
   Returns plist (:total-sessions N :classes N :reduction X
                  :class-sizes list :class-profiles list)."
  (let ((sessions nil)
        (parent (make-hash-table :test 'equal)))
    (maphash (lambda (id fp)
               (push (cons id (fingerprint-to-vector fp)) sessions)
               (setf (gethash id parent) id))
             fingerprints)
    (labels ((find-root (x)
               (if (equal x (gethash x parent)) x
                   (let ((r (find-root (gethash x parent))))
                     (setf (gethash x parent) r) r)))
             (union-sets (a b)
               (let ((ra (find-root a)) (rb (find-root b)))
                 (unless (equal ra rb) (setf (gethash ra parent) rb)))))
      (let ((n (length sessions)))
        (loop for i from 0 below n
              for (id-a . vec-a) = (nth i sessions)
              do (loop for j from (1+ i) below n
                       for (id-b . vec-b) = (nth j sessions)
                       when (>= (cosine-similarity vec-a vec-b) threshold)
                       do (union-sets id-a id-b)))
        (let ((classes (make-hash-table :test 'equal)))
          (dolist (s sessions)
            (push s (gethash (find-root (car s)) classes nil)))
          (let ((profiles nil))
            (maphash (lambda (root members)
                       (declare (ignore root))
                       (let* ((size (length members))
                              (vecs (mapcar #'cdr members))
                              (mean-vec (make-array 6 :initial-element 0.0)))
                         (dolist (v vecs)
                           (dotimes (i 6) (incf (aref mean-vec i) (aref v i))))
                         (dotimes (i 6) (setf (aref mean-vec i) (/ (aref mean-vec i) size)))
                         (push (list :size size
                                     :tool% (float (aref mean-vec 0))
                                     :obs% (float (aref mean-vec 1))
                                     :fork% (float (aref mean-vec 2))
                                     :join% (float (aref mean-vec 3)))
                               profiles)))
                     classes)
            (let ((sorted-profiles (sort profiles #'> :key (lambda (p) (getf p :size)))))
              (list :total-sessions n
                    :classes (hash-table-count classes)
                    :reduction (if (plusp (hash-table-count classes))
                                   (float (/ n (hash-table-count classes)))
                                   0.0)
                    :class-sizes (mapcar (lambda (p) (getf p :size)) sorted-profiles)
                    :class-profiles sorted-profiles))))))))

(defun classify-session-archetype (fp &key (builder-tool-threshold 0.30))
  "Classify a session fingerprint as :builder or :observer.
   Builders have tool% > threshold; observers do not."
  (let* ((vec (fingerprint-to-vector fp))
         (tool% (aref vec 0)))
    (if (> tool% builder-tool-threshold) :builder :observer)))

;;; ============================================================
;;; Active Inference: Missing Edge Detection + Free Energy
;;; ============================================================

(defun find-missing-edges (all-tasks &key (min-count 2))
  "Find task pairs with repeated off-edge session transitions.
   ALL-TASKS is hash: task-name → (:events events :state state :depot depot-name).
   Returns list of (:from X :to Y :count N) sorted by count descending."
  (let ((off-edges (make-hash-table :test 'equal))
        (session-tasks (make-hash-table :test 'equal))
        (graph-edges (make-hash-table :test 'equal)))
    ;; Collect session flows and graph edges
    (maphash (lambda (name data)
               (let* ((events (getf data :events))
                      (state (getf data :state)))
                 (dolist (e events)
                   (when (eq (event-type e) :session.join)
                     (let ((session (event-session e)))
                       (when session
                         (push (cons (event-timestamp e) name)
                               (gethash session session-tasks nil))))))
                 (dolist (edge (ors-members (task-state-edges state)))
                   (let* ((decoded (decode-edge edge))
                          (target (car decoded)))
                     ;; Strip depot qualifier if present
                     (when (find #\: target)
                       (setf target (subseq target (1+ (position #\: target)))))
                     (setf (gethash (cons name target) graph-edges) t)
                     (setf (gethash (cons target name) graph-edges) t)))))
             all-tasks)
    ;; Find off-edge transitions
    (maphash (lambda (session tasks)
               (declare (ignore session))
               (let ((sorted (sort (copy-list tasks) #'< :key #'car)))
                 (when (> (length sorted) 1)
                   ;; Deduplicate consecutive same-task visits
                   (let ((deduped (list (first sorted))))
                     (dolist (tk (rest sorted))
                       (unless (equal (cdr tk) (cdr (first deduped)))
                         (push tk deduped)))
                     (setf deduped (nreverse deduped))
                     (loop for (a b) on deduped while b
                           for from = (cdr a) for to = (cdr b)
                           unless (gethash (cons from to) graph-edges)
                           do (incf (gethash (cons from to) off-edges 0)))))))
             session-tasks)
    ;; Return pairs above threshold
    (let (pairs)
      (maphash (lambda (pair count)
                 (when (>= count min-count)
                   (push (list :from (car pair) :to (cdr pair) :count count) pairs)))
               off-edges)
      (sort pairs #'> :key (lambda (x) (getf x :count))))))

(defun free-energy-reduction (all-tasks)
  "Compute coherence improvement from adding suggested missing edges.
   ALL-TASKS is hash: task-name → (:events events :state state :depot depot-name).
   Returns plist (:current-coherence X :new-coherence Y :improvement Z
                  :suggested-edges N :transitions-fixed N)."
  (let ((session-tasks (make-hash-table :test 'equal))
        (graph-edges (make-hash-table :test 'equal))
        (off-edge-counts (make-hash-table :test 'equal)))
    ;; Collect flows and edges
    (maphash (lambda (name data)
               (let* ((events (getf data :events))
                      (state (getf data :state)))
                 (dolist (e events)
                   (when (eq (event-type e) :session.join)
                     (let ((session (event-session e)))
                       (when session
                         (push (cons (event-timestamp e) name)
                               (gethash session session-tasks nil))))))
                 (dolist (edge (ors-members (task-state-edges state)))
                   (let* ((decoded (decode-edge edge))
                          (target (car decoded)))
                     (when (find #\: target)
                       (setf target (subseq target (1+ (position #\: target)))))
                     (setf (gethash (cons name target) graph-edges) t)
                     (setf (gethash (cons target name) graph-edges) t)))))
             all-tasks)
    ;; Count transitions
    (let ((total 0) (off 0) (would-fix 0))
      (maphash (lambda (session tasks)
                 (declare (ignore session))
                 (let ((sorted (sort (copy-list tasks) #'< :key #'car)))
                   (when (> (length sorted) 1)
                     (let ((deduped (list (first sorted))))
                       (dolist (tk (rest sorted))
                         (unless (equal (cdr tk) (cdr (first deduped)))
                           (push tk deduped)))
                       (setf deduped (nreverse deduped))
                       (loop for (a b) on deduped while b
                             for from = (cdr a) for to = (cdr b)
                             do (incf total)
                                (unless (gethash (cons from to) graph-edges)
                                  (incf off)
                                  (incf (gethash (cons from to) off-edge-counts 0))))))))
               session-tasks)
      ;; Count how many off-transitions would be fixed by adding suggested edges
      (maphash (lambda (session tasks)
                 (declare (ignore session))
                 (let ((sorted (sort (copy-list tasks) #'< :key #'car)))
                   (when (> (length sorted) 1)
                     (let ((deduped (list (first sorted))))
                       (dolist (tk (rest sorted))
                         (unless (equal (cdr tk) (cdr (first deduped)))
                           (push tk deduped)))
                       (setf deduped (nreverse deduped))
                       (loop for (a b) on deduped while b
                             for from = (cdr a) for to = (cdr b)
                             unless (gethash (cons from to) graph-edges)
                             when (>= (gethash (cons from to) off-edge-counts 0) 2)
                             do (incf would-fix))))))
               session-tasks)
      (let ((suggested (let ((n 0))
                         (maphash (lambda (pair count)
                                    (declare (ignore pair))
                                    (when (>= count 2) (incf n)))
                                  off-edge-counts)
                         n))
            (current-coherence (float (/ (- total off) (max total 1))))
            (new-coherence (float (/ (- total (- off would-fix)) (max total 1)))))
        (list :current-coherence current-coherence
              :new-coherence new-coherence
              :improvement (- new-coherence current-coherence)
              :suggested-edges suggested
              :transitions-fixed would-fix)))))

;;; ============================================================
;;; Batch Loading
;;; ============================================================

(defun load-all-tasks (&key (min-events 15))
  "Load all tasks with >= MIN-EVENTS events from all depots.
   Returns (values hash count) where hash maps task-name to
   (:events events :state state :depot depot-name)."
  (let ((result (make-hash-table :test 'equal))
        (count 0))
    (maphash (lambda (depot-name root)
               (dolist (dir (directory (merge-pathnames "*/" (pathname root))))
                 (let ((ep (merge-pathnames "events.jsonl" dir)))
                   (when (probe-file ep)
                     (handler-case
                         (let* ((elog (elog-load ep))
                                (events (event-log-events elog))
                                (task-name (car (last (pathname-directory dir)))))
                           (when (> (length events) min-events)
                             (let ((state (compute-state events)))
                               (setf (gethash task-name result)
                                     (list :events events :state state :depot depot-name))
                               (incf count))))
                       (error () nil))))))
             *depot-tasks-roots*)
    (values result count)))

;;; ============================================================
;;; TTL Cache for Batch Computations
;;; Follows the pattern from graph.lisp (*graph-cache*)
;;; ============================================================

(defvar *markov-cache* (make-hash-table :test 'equal)
  "Cache for markov batch computations. Key: keyword, Value: (timestamp . result)")

(defvar *markov-cache-ttl* 300
  "TTL for markov cache in seconds. Default 300 (5 minutes).
   Longer than graph-cache (60s) because batch computations are expensive.")

(defun get-cached-markov-data (key compute-fn)
  "Get cached markov data for KEY or compute fresh via COMPUTE-FN.
   COMPUTE-FN is a thunk (zero-arg function) that produces the value.
   Returns the cached or freshly computed value."
  (let* ((entry (gethash key *markov-cache*))
         (now (get-universal-time))
         (valid (and entry
                     (< (- now (car entry)) *markov-cache-ttl*))))
    (if valid
        (cdr entry)
        (let ((result (funcall compute-fn)))
          (setf (gethash key *markov-cache*) (cons now result))
          result))))

(defun clear-markov-cache ()
  "Clear all cached markov data."
  (clrhash *markov-cache*))

(defun invalidate-markov-cache (&optional key)
  "Invalidate markov cache for KEY, or clear all if KEY is nil."
  (if key
      (remhash key *markov-cache*)
      (clrhash *markov-cache*)))

;;; Convenience accessors for common cached computations

(defun cached-all-tasks (&key (min-events 15))
  "Get cached all-tasks hash or load fresh."
  (get-cached-markov-data
   :all-tasks
   (lambda () (load-all-tasks :min-events min-events))))

(defun cached-task-metrics ()
  "Get cached per-task metrics (alpha, entropy, affinity) for all tasks.
   Returns hash: task-name → plist of metrics."
  (get-cached-markov-data
   :task-metrics
   (lambda ()
     (let ((all-tasks (cached-all-tasks))
           (metrics (make-hash-table :test 'equal)))
       (maphash (lambda (name data)
                  (let* ((events (getf data :events))
                         (state (getf data :state)))
                    (multiple-value-bind (alpha phi q) (action-functor events state)
                      (let ((h (event-type-entropy events))
                            (mi (mutual-information-bigrams events))
                            (aff (affinity-score events state))
                            (org (organization-indicator events state)))
                        (setf (gethash name metrics)
                              (list :alpha alpha :phi phi :q q
                                    :entropy h :mi mi
                                    :affinity aff
                                    :organized (getf org :organized)
                                    :obs-deficit (getf org :deficit)))))))
                all-tasks)
       metrics))))

(defun cached-session-fingerprints ()
  "Get cached session fingerprints for all sessions."
  (get-cached-markov-data
   :session-fingerprints
   (lambda () (build-session-fingerprints (cached-all-tasks)))))

(defun cached-missing-edges ()
  "Get cached missing edges analysis."
  (get-cached-markov-data
   :missing-edges
   (lambda () (find-missing-edges (cached-all-tasks)))))

(defun cached-free-energy ()
  "Get cached free energy reduction analysis."
  (get-cached-markov-data
   :free-energy
   (lambda () (free-energy-reduction (cached-all-tasks)))))
