(in-package #:kli-bench)

;;; Real-Task Benchmarks
;;; Measure elog-load + compute-state on actual task event logs.
;;; Paths are passed at runtime — nothing hardcoded.

(defvar *bucket-thresholds*
  '((:small    0  50)
    (:medium  50 200)
    (:large  200 500)
    (:xlarge 500 nil))
  "Size bucket definitions: (name min-inclusive max-exclusive-or-nil).")

(defstruct task-file
  "A real task event log with its metadata."
  path
  event-count
  bucket)

(defun classify-bucket (n)
  "Return the bucket keyword for N events."
  (loop for (name lo hi) in *bucket-thresholds*
        when (and (>= n lo) (or (null hi) (< n hi)))
          return name))

(defun discover-task-files (roots)
  "Scan ROOTS for events.jsonl files. Returns list of task-file structs."
  (let ((results nil))
    (dolist (root (mapcar #'pathname roots))
      (dolist (path (directory (merge-pathnames "*/events.jsonl" root)))
        (handler-case
            (let* ((elog (elog-load path))
                   (n (length (event-log-events elog))))
              (when (> n 0)
                (push (make-task-file :path path
                                      :event-count n
                                      :bucket (classify-bucket n))
                      results)))
          (error () nil))))
    (sort results #'< :key #'task-file-event-count)))

(defun select-representatives (task-files &key (per-bucket 3))
  "Select PER-BUCKET representative tasks from each size bucket.
   Picks min, median, and max from each bucket."
  (let ((buckets (make-hash-table)))
    (dolist (tf task-files)
      (push tf (gethash (task-file-bucket tf) buckets)))
    (let ((selected nil))
      (dolist (spec *bucket-thresholds*)
        (let* ((name (first spec))
               (members (sort (copy-list (gethash name buckets)) #'<
                              :key #'task-file-event-count))
               (n (length members)))
          (when (> n 0)
            (cond
              ((<= n per-bucket)
               (dolist (m members) (push m selected)))
              (t
               (push (first members) selected)
               (push (nth (floor n 2) members) selected)
               (push (car (last members)) selected))))))
      (sort (nreverse selected) #'< :key #'task-file-event-count))))

(defun bench-real-elog-load (task-file &key (iterations 30))
  "Benchmark elog-load on a real task event log."
  (let ((path (task-file-path task-file))
        (n (task-file-event-count task-file)))
    (run-benchmark (format nil "real elog-load ~D (~A)" n (task-file-bucket task-file))
      :iterations iterations :warmup 3
      :body (elog-load path))))

(defun bench-real-compute-state (task-file &key (iterations 30))
  "Benchmark compute-state on a real task's events."
  (let* ((n (task-file-event-count task-file))
         (events (event-log-events (elog-load (task-file-path task-file)))))
    (run-benchmark (format nil "real compute-state ~D (~A)" n (task-file-bucket task-file))
      :iterations iterations :warmup 3
      :body (compute-state events))))

;;; Environment metadata (no paths, no task names)

(defun format-environment (stream task-files representatives)
  "Print anonymized environment metadata for reproducibility."
  (let ((bucket-counts (make-hash-table)))
    (dolist (tf task-files)
      (incf (gethash (task-file-bucket tf) bucket-counts 0)))
    (format stream "~%Environment:~%")
    (format stream "  SBCL ~A on ~A ~A~%"
            (lisp-implementation-version)
            (software-type) (machine-type))
    (format stream "  Time units/sec: ~D~%" internal-time-units-per-second)
    (format stream "  Tasks scanned: ~D~%"
            (length task-files))
    (format stream "  Distribution: ~{~A=~D~^, ~}~%"
            (loop for (name . nil) in *bucket-thresholds*
                  append (list name (gethash name bucket-counts 0))))
    (format stream "  Representatives: ~D (~{~D~^ ~} events)~%"
            (length representatives)
            (mapcar #'task-file-event-count representatives))
    (terpri stream)))

;;; Suite

(defun run-real-tasks (&rest roots)
  "Run benchmarks on real task event logs.
   ROOTS are directories containing task subdirs with events.jsonl files.
   Example: (run-real-tasks #P\"/path/to/depot1/ace/tasks/\" #P\"/path/to/depot2/ace/tasks/\")"
  (when (null roots)
    (error "run-real-tasks requires at least one directory root.~%~
            Usage: (run-real-tasks #P\"/path/to/ace/tasks/\" ...)"))
  (format t "~%Scanning for event logs...~%")
  (let* ((all-files (discover-task-files roots))
         (representatives (select-representatives all-files))
         (results nil))
    (format-environment t all-files representatives)
    (dolist (tf representatives)
      (push (bench-real-elog-load tf) results)
      (push (bench-real-compute-state tf) results))
    (format-results (nreverse results) :title "Real Task Replay")))

;; No define-suite for real-tasks — requires caller-supplied roots.
;; Use (run-real-tasks #P"/path/to/ace/tasks/" ...) directly,
;; or (run-benchmarks :real-task-roots '(...)).

;;; Unified entry point

(defun run-benchmarks (&key real-task-roots)
  "Run all benchmark suites and print combined results.
   REAL-TASK-ROOTS: list of directories for real-task benchmarks.
   Example: (run-benchmarks :real-task-roots '(#P\"/path/to/ace/tasks/\"))"
  (let ((all-results nil))
    (flet ((run-one (name fn)
             (format t "~%~V,,,'-A~%" 80 "")
             (format t "=== ~A ===~%" name)
             (let ((results (funcall fn)))
               (setf all-results (append all-results
                                         (if (and results (getf (first results) :name))
                                             results
                                             nil))))))
      (run-one "CRDT" #'run-crdt)
      (run-one "Event Log (Synthetic)" #'run-event-log)
      (when real-task-roots
        (run-one "Real Task Replay"
                 (lambda () (apply #'run-real-tasks real-task-roots)))))
    (format t "~%~V,,,'-A~%" 80 "")
    (format-results all-results :title "Combined Results")
    all-results))
