(in-package #:kli-bench)

;;; Event Log Benchmarks
;;; elog-load, compute-state, elog-merge at varying event counts.

(defvar *event-scales* '(100 500 1000 5000)
  "Event counts for event log benchmarks.")

(defvar *event-types*
  '(:task.create :session.join :observation :task.set-metadata
    :task.link :task.fork :tool.call :artifact.create)
  "Event types to generate for realistic benchmarks.")

;;; Synthetic event generation

(defun make-synthetic-event (index session-id)
  "Generate a synthetic event with realistic data payload."
  (let* ((ts (+ 1700000000 (* index 60)))  ; 1-minute spacing
         (etype (nth (mod index (length *event-types*)) *event-types*))
         (vc (make-vector-clock)))
    (vc-increment vc session-id)
    (make-event
     :id (format nil "evt-~36R" (+ (* index 97) 12345))
     :timestamp ts
     :session session-id
     :clock vc
     :type etype
     :data (ecase etype
             (:task.create (list :name "bench-task" :description "Benchmark task"))
             (:session.join nil)
             (:observation (list :text (format nil "Observation ~D: benchmark data point" index)))
             (:task.set-metadata (list :key "phase" :value "implementation"))
             (:task.link (list :target-id "other-task" :edge-type "related-to"))
             (:task.fork (list :child-id (format nil "child-~D" index) :edge-type "phase-of"))
             (:tool.call (list :tool "Edit" :args (list :file-path (format nil "/src/file-~D.lisp" (mod index 10)))))
             (:artifact.create (list :path (format nil "/artifacts/doc-~D.md" index)))))))

(defun generate-events-file (path n-events &key (n-sessions 3))
  "Generate a JSONL events file with N-EVENTS synthetic events."
  (with-open-file (s path :direction :output :if-exists :supersede)
    (dotimes (i n-events)
      (let* ((session-id (format nil "session-~D" (mod i n-sessions)))
             (ev (make-synthetic-event i session-id)))
        (write-string (event-to-json-string ev) s)
        (terpri s))))
  path)

;;; Benchmarks

(defun bench-elog-load (n &key (iterations 50))
  "Benchmark loading an N-event JSONL file."
  (let ((path (merge-pathnames (format nil "bench-~D.jsonl" n)
                               (uiop:temporary-directory))))
    (generate-events-file path n)
    (prog1 (run-benchmark (format nil "elog-load ~D events" n)
             :iterations iterations :warmup 3
             :body (elog-load path))
      (delete-file path))))

(defun bench-compute-state (n &key (iterations 50))
  "Benchmark computing task state from N events."
  (let* ((events (loop for i below n
                       for sid = (format nil "session-~D" (mod i 3))
                       collect (make-synthetic-event i sid))))
    (run-benchmark (format nil "compute-state ~D events" n)
      :iterations iterations :warmup 3
      :body (compute-state events))))

(defun bench-elog-merge (n &key (iterations 50))
  "Benchmark merging two N-event logs."
  (let ((path1 (merge-pathnames "bench-merge-1.jsonl" (uiop:temporary-directory)))
        (path2 (merge-pathnames "bench-merge-2.jsonl" (uiop:temporary-directory))))
    (generate-events-file path1 n :n-sessions 2)
    (generate-events-file path2 n :n-sessions 2)
    (let ((log1 (elog-load path1))
          (log2 (elog-load path2)))
      (prog1 (run-benchmark (format nil "elog-merge ~D+~D events" n n)
               :iterations iterations :warmup 3
               :body (elog-merge log1 log2))
        (delete-file path1)
        (delete-file path2)))))

(defun bench-event-serialize (n &key (iterations 50))
  "Benchmark JSON serialization of N events (round-trip)."
  (let* ((events (loop for i below n
                       for sid = (format nil "session-~D" (mod i 3))
                       collect (make-synthetic-event i sid))))
    (run-benchmark (format nil "event-json-roundtrip ~D" n)
      :iterations iterations :warmup 3
      :body (dolist (ev events)
              (json-string-to-event (event-to-json-string ev))))))

;;; Suite

(defun run-event-log ()
  "Run all event log benchmarks."
  (let ((results nil))
    (dolist (n *event-scales*)
      (push (bench-elog-load n) results)
      (push (bench-compute-state n) results)
      (push (bench-elog-merge n) results)
      (push (bench-event-serialize n) results))
    (format-results (nreverse results) :title "Event Log")))

(define-suite event-log (run-event-log))
