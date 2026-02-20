(defsystem "kli-bench"
  :description "Benchmark suite for KLI subsystems"
  :version "0.1.0"
  :license "MIT"
  :depends-on ("kli-crdt" "kli-task" "alexandria" "bordeaux-threads" "yason" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "harness")))
