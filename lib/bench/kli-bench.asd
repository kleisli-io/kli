(defsystem "kli-bench"
  :description "Benchmark suite for KLI subsystems"
  :version (:read-file-form "../../version.sexp")
  :license "MIT"
  :depends-on ("kli-crdt" "kli-task" "alexandria" "bordeaux-threads" "yason" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "harness")))
