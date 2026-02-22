(defsystem "kli-crdt"
  :description "CRDT primitives — join-semilattice types for conflict-free merge"
  :version "0.2.0"
  :license "MIT"
  :depends-on ("alexandria")
  :serial t
  :components ((:file "package")
               (:file "vector-clock")
               (:file "g-set")
               (:file "pn-counter")
               (:file "lww-register")
               (:file "or-set")
               (:file "lww-map"))
  :in-order-to ((test-op (test-op "kli-crdt/tests"))))

(defsystem "kli-crdt/tests"
  :depends-on ("kli-crdt" "fiveam")
  :serial t
  :components ((:module "t"
                :components ((:file "package")
                             (:file "tests"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :crdt-tests)))
