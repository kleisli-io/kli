(defpackage #:kli-bench
  (:use #:cl #:crdt #:task)
  (:export
   ;; Harness
   #:run-benchmark
   #:format-results
   ;; Benchmark suites
   #:run-all
   #:run-crdt
   #:run-event-log
   #:run-real-tasks
   #:run-benchmarks))
