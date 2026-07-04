(in-package #:kli/tests)

(in-suite all)

;;; Constraint lattice.

(test constraint-any-covers-everything
  (is (ext:constraint-covers-p (ext:constraint-any) (ext:constraint-any)))
  (is (ext:constraint-covers-p (ext:constraint-any) (ext:constraint-none)))
  (is (ext:constraint-covers-p (ext:constraint-any)
                               (ext:path-prefix-constraint "/home"))))

(test constraint-none-is-bottom
  (is (ext:constraint-covers-p (ext:path-prefix-constraint "/home")
                               (ext:constraint-none)))
  (is (null (ext:constraint-covers-p (ext:constraint-none)
                                     (ext:path-prefix-constraint "/home")))))

(test path-prefix-order
  (is (ext:constraint-covers-p (ext:path-prefix-constraint "/home")
                               (ext:path-prefix-constraint "/home/user")))
  (is (null (ext:constraint-covers-p (ext:path-prefix-constraint "/home/user")
                                     (ext:path-prefix-constraint "/home"))))
  (is (null (ext:constraint-covers-p (ext:path-prefix-constraint "/etc")
                                     (ext:path-prefix-constraint "/home")))))

(test path-prefix-meet
  (is (equal (ext:path-prefix-constraint "/home/user")
             (ext:constraint-meet (ext:path-prefix-constraint "/home")
                                  (ext:path-prefix-constraint "/home/user"))))
  (is (eq :none
          (ext:constraint-meet (ext:path-prefix-constraint "/home")
                               (ext:path-prefix-constraint "/etc")))))

(test numeric-bound-order
  (is (ext:constraint-covers-p (ext:numeric-bound-constraint :low 0 :high 10)
                               (ext:numeric-bound-constraint :low 2 :high 8)))
  (is (null (ext:constraint-covers-p
             (ext:numeric-bound-constraint :low 2 :high 8)
             (ext:numeric-bound-constraint :low 0 :high 10))))
  ;; An open side covers a bounded one but not vice versa.
  (is (ext:constraint-covers-p (ext:numeric-bound-constraint :low 0)
                               (ext:numeric-bound-constraint :low 0 :high 10)))
  (is (null (ext:constraint-covers-p (ext:numeric-bound-constraint :low 0 :high 10)
                                     (ext:numeric-bound-constraint :low 0)))))

(test numeric-bound-meet
  (let ((m (ext:constraint-meet (ext:numeric-bound-constraint :low 0 :high 10)
                                (ext:numeric-bound-constraint :low 5 :high 20))))
    (is (ext:constraint-covers-p (ext:numeric-bound-constraint :low 0 :high 10) m))
    (is (ext:constraint-covers-p (ext:numeric-bound-constraint :low 5 :high 20) m))
    (is (ext:constraint-covers-p m (ext:numeric-bound-constraint :low 5 :high 10))))
  (is (eq :none
          (ext:constraint-meet (ext:numeric-bound-constraint :low 0 :high 3)
                               (ext:numeric-bound-constraint :low 5 :high 9)))))

(test enumerated-order-and-meet
  (is (ext:constraint-covers-p (ext:enumerated-constraint '(:a :b :c))
                               (ext:enumerated-constraint '(:a :b))))
  (is (null (ext:constraint-covers-p (ext:enumerated-constraint '(:a :b))
                                     (ext:enumerated-constraint '(:a :b :c)))))
  (let ((m (ext:constraint-meet (ext:enumerated-constraint '(:a :b :c))
                                (ext:enumerated-constraint '(:b :c :d)))))
    (is (ext:constraint-covers-p (ext:enumerated-constraint '(:b :c)) m))
    (is (ext:constraint-covers-p m (ext:enumerated-constraint '(:b :c)))))
  (is (eq :none (ext:constraint-meet (ext:enumerated-constraint '(:a))
                                     (ext:enumerated-constraint '(:b))))))

(test cross-kind-constraints-are-incomparable
  (is (eq :none (ext:constraint-meet (ext:path-prefix-constraint "/home")
                                     (ext:numeric-bound-constraint :low 0 :high 1))))
  (is (null (ext:constraint-covers-p (ext:path-prefix-constraint "/home")
                                     (ext:numeric-bound-constraint :low 0 :high 1)))))

;;; Grant lattice.

(defun lattice-test-grant (&rest capabilities)
  (ext:make-grant :capabilities capabilities))

(test grant-top-covers-all
  (is (ext:grant-covers-p (ext:grant-top) (ext:grant-top)))
  (is (ext:grant-covers-p (ext:grant-top) (ext:grant-bottom)))
  (is (ext:grant-covers-p (ext:grant-top) (lattice-test-grant :a :b))))

(test grant-bottom-covered-by-all
  (is (ext:grant-covers-p (lattice-test-grant :a :b) (ext:grant-bottom)))
  (is (ext:grant-covers-p (ext:grant-bottom) (ext:grant-bottom)))
  (is (null (ext:grant-covers-p (ext:grant-bottom) (lattice-test-grant :a)))))

(test grant-meet-is-greatest-lower-bound
  (let* ((a (lattice-test-grant :a :b))
         (b (lattice-test-grant :b :c))
         (m (ext:grant-meet a b)))
    (is (ext:grant-covers-p a m))
    (is (ext:grant-covers-p b m))
    (is (ext:grant-equiv-p m (lattice-test-grant :b)))))

(test grant-meet-idempotent
  (let ((a (lattice-test-grant :a :b)))
    (is (ext:grant-equiv-p a (ext:grant-meet a a)))))

(test grant-meet-commutative
  (let ((a (lattice-test-grant :a :b))
        (b (lattice-test-grant :b :c)))
    (is (ext:grant-equiv-p (ext:grant-meet a b) (ext:grant-meet b a)))))

(test grant-meet-associative
  (let ((a (lattice-test-grant :a :b))
        (b (lattice-test-grant :b :c))
        (c (lattice-test-grant :b :d)))
    (is (ext:grant-equiv-p (ext:grant-meet (ext:grant-meet a b) c)
                           (ext:grant-meet a (ext:grant-meet b c))))))

(test grant-top-is-meet-identity
  (let ((a (lattice-test-grant :a :b)))
    (is (ext:grant-equiv-p a (ext:grant-meet (ext:grant-top) a)))
    (is (ext:grant-equiv-p a (ext:grant-meet a (ext:grant-top))))))

(test grant-bottom-is-meet-absorbing
  (let ((a (lattice-test-grant :a :b)))
    (is (ext:grant-equiv-p (ext:grant-bottom)
                           (ext:grant-meet (ext:grant-bottom) a)))
    (is (ext:grant-equiv-p (ext:grant-bottom)
                           (ext:grant-meet a (ext:grant-bottom))))))

(test grant-meet-never-rises
  ;; Monotonicity: attenuating by meet can only descend the order. This is the
  ;; structural no-escalation property.
  (let* ((holder (lattice-test-grant :a :b))
         (other (ext:make-grant
                 :atoms (list (cons :a (ext:path-prefix-constraint "/home")))))
         (m (ext:grant-meet holder other)))
    (is (ext:grant-covers-p holder m))
    (is (null (ext:grant-covers-p m holder)))))

(test keyword-bag-embeds-as-powerset
  (is (ext:grant-covers-p (lattice-test-grant :a :b :c) (lattice-test-grant :a :c)))
  (is (null (ext:grant-covers-p (lattice-test-grant :a :c)
                                (lattice-test-grant :a :b :c))))
  (is (ext:grant-equiv-p (lattice-test-grant :b)
                         (ext:grant-meet (lattice-test-grant :a :b)
                                         (lattice-test-grant :b :c)))))

(test constrained-grant-covers-narrower-request
  (let ((holder (ext:make-grant
                 :atoms (list (cons :file/read
                                    (ext:path-prefix-constraint "/home"))))))
    (is (ext:grant-covers-p
         holder
         (ext:make-grant :atoms (list (cons :file/read
                                            (ext:path-prefix-constraint "/home/user"))))))
    (is (null (ext:grant-covers-p
               holder
               (ext:make-grant :atoms (list (cons :file/read
                                                  (ext:path-prefix-constraint "/etc")))))))
    ;; A bare unconstrained request demands more than a constrained grant gives.
    (is (null (ext:grant-covers-p holder (lattice-test-grant :file/read))))))

(test attenuated-grant-cannot-be-widened
  (let* ((holder (lattice-test-grant :file/read :file/write))
         (attenuated (ext:grant-meet
                      holder
                      (ext:make-grant
                       :atoms (list (cons :file/read
                                          (ext:path-prefix-constraint "/home")))))))
    (is (ext:grant-covers-p holder attenuated))
    (is (null (ext:grant-covers-p attenuated holder)))
    ;; Further meet only descends.
    (is (ext:grant-covers-p
         attenuated
         (ext:grant-meet attenuated
                         (ext:make-grant
                          :atoms (list (cons :file/read
                                             (ext:path-prefix-constraint "/home/user")))))))))

;;; Subject embedding into the lattice.

(test subject-grant-reflects-capabilities
  (let ((s (ext:make-subject :capabilities '(:a :b))))
    (is (ext:grant-covers-p (ext:subject-grant s) (lattice-test-grant :a)))
    (is (ext:grant-covers-p (ext:subject-grant s) (lattice-test-grant :b)))
    (is (null (ext:grant-covers-p (ext:subject-grant s) (lattice-test-grant :c))))))

(test system-subject-grant-is-top
  (is (ext:grant-covers-p (ext:subject-grant (ext:make-system-subject))
                          (lattice-test-grant :anything/at-all))))

(test implications-are-an-order-fragment
  (let ((s (ext:make-subject :capabilities '(:tools/standard))))
    (is (ext:check-capability s :file/read))
    (is (ext:check-capability s :process/exec))
    (is (null (ext:check-capability s :image/eval)))))
