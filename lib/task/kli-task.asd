(defsystem "kli-task"
  :description "Event-sourced task management with CRDT merge semantics"
  :version "0.1.0"
  :license "MIT"
  :depends-on ("kli-crdt" "alexandria" "cl-ppcre" "yason" "uiop")
  :serial t
  :components ((:file "package")
               (:file "validation")
               (:file "event")
               (:file "log")
               (:file "paths")
               (:file "state")
               (:file "graph")
               (:file "query")
               (:file "markov"))
  :in-order-to ((test-op (test-op "kli-task/tests"))))

(defsystem "kli-task/tests"
  :depends-on ("kli-task" "fiveam" "bordeaux-threads")
  :serial t
  :components ((:module "t"
                :components ((:file "package")
                             (:file "tests")
                             (:file "test-query")
                             (:file "test-markov"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :task-tests)))
