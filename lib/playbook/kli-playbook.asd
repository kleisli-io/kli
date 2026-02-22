(defsystem "kli-playbook"
  :description "PQ query language — safe interpreter for playbook pattern graph"
  :version "0.2.0"
  :license "MIT"
  :depends-on ("uiop")
  :serial t
  :components ((:file "query"))
  :in-order-to ((test-op (test-op "kli-playbook/tests"))))

(defsystem "kli-playbook/tests"
  :depends-on ("kli-playbook" "fiveam")
  :serial t
  :components ((:module "t"
                :components ((:file "test-patterns")
                             (:file "package")
                             (:file "test-query"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :pq-tests)))
