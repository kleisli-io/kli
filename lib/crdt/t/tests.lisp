(in-package #:crdt-tests)

;;; --- Vector Clock ---

(test vc-increment-and-get
  (let ((vc (make-vector-clock)))
    (vc-increment vc "s1")
    (vc-increment vc "s1")
    (vc-increment vc "s2")
    (is (= 2 (vc-get vc "s1")))
    (is (= 1 (vc-get vc "s2")))
    (is (= 0 (vc-get vc "s3")))))

(test vc-merge-pointwise-max
  (let ((vc1 (make-vector-clock))
        (vc2 (make-vector-clock)))
    (vc-increment vc1 "s1")
    (vc-increment vc1 "s1")
    (vc-increment vc2 "s2")
    (let ((merged (vc-merge vc1 vc2)))
      (is (= 2 (vc-get merged "s1")))
      (is (= 1 (vc-get merged "s2"))))))

(test vc-semilattice-commutative
  (let ((a (make-vector-clock))
        (b (make-vector-clock)))
    (vc-increment a "s1")
    (vc-increment b "s2")
    (is (vc-equal-p (vc-merge a b) (vc-merge b a)))))

(test vc-semilattice-associative
  (let ((a (make-vector-clock))
        (b (make-vector-clock))
        (c (make-vector-clock)))
    (vc-increment a "s1")
    (vc-increment b "s2")
    (vc-increment c "s3")
    (is (vc-equal-p (vc-merge (vc-merge a b) c)
                    (vc-merge a (vc-merge b c))))))

(test vc-semilattice-idempotent
  (let ((a (make-vector-clock)))
    (vc-increment a "s1")
    (is (vc-equal-p (vc-merge a a) a))))

(test vc-happened-before
  (let ((a (make-vector-clock))
        (b (make-vector-clock)))
    (vc-increment a "s1")
    ;; b has everything a has and more
    (vc-increment b "s1")
    (vc-increment b "s2")
    (is (vc-happened-before-p a b))
    (is (not (vc-happened-before-p b a)))))

(test vc-concurrent-detection
  (let ((a (make-vector-clock))
        (b (make-vector-clock)))
    (vc-increment a "s1")
    (vc-increment b "s2")
    (is (vc-concurrent-p a b))
    (is (not (vc-happened-before-p a b)))
    (is (not (vc-happened-before-p b a)))))

;;; --- G-Set ---

(test gs-add-idempotent
  (let ((gs (make-g-set)))
    (gs-add gs "a")
    (gs-add gs "a")
    (is (= 1 (gs-count gs)))
    (is (gs-contains-p gs "a"))))

(test gs-merge-union
  (let ((gs1 (make-g-set))
        (gs2 (make-g-set)))
    (gs-add gs1 "a")
    (gs-add gs1 "b")
    (gs-add gs2 "b")
    (gs-add gs2 "c")
    (let ((merged (gs-merge gs1 gs2)))
      (is (= 3 (gs-count merged)))
      (is (gs-contains-p merged "a"))
      (is (gs-contains-p merged "b"))
      (is (gs-contains-p merged "c")))))

;;; --- PN-Counter ---

(test pnc-increment-decrement
  (let ((pnc (make-pn-counter)))
    (pnc-increment pnc "s1" 5)
    (pnc-decrement pnc "s1" 2)
    (is (= 3 (pnc-value pnc)))))

(test pnc-multi-session
  (let ((pnc (make-pn-counter)))
    (pnc-increment pnc "s1" 3)
    (pnc-increment pnc "s2" 2)
    (pnc-decrement pnc "s1" 1)
    (is (= 4 (pnc-value pnc)))))

(test pnc-merge-max-per-session
  (let ((a (make-pn-counter))
        (b (make-pn-counter)))
    (pnc-increment a "s1" 5)
    (pnc-increment b "s1" 3)
    (pnc-increment b "s2" 2)
    (let ((merged (pnc-merge a b)))
      ;; s1: max(5,3)=5, s2: max(0,2)=2, no neg
      (is (= 7 (pnc-value merged))))))

;;; --- LWW-Register ---

(test lww-timestamp-wins
  (let ((reg (make-lww-register)))
    (lww-set reg "first" 100 "s1")
    (lww-set reg "second" 200 "s2")
    (is (string= "second" (lww-value reg)))
    ;; Older write ignored
    (lww-set reg "old" 50 "s3")
    (is (string= "second" (lww-value reg)))))

(test lww-merge-higher-wins
  (let ((a (make-lww-register :value "a" :timestamp 100 :session "s1"))
        (b (make-lww-register :value "b" :timestamp 200 :session "s2")))
    (let ((merged (lww-merge a b)))
      (is (string= "b" (lww-value merged)))
      (is (= 200 (lww-timestamp merged))))))

;;; --- OR-Set ---

(test ors-add-remove
  (let ((ors (make-or-set)))
    (ors-add ors "file.lisp" "s1:1")
    (is (ors-contains-p ors "file.lisp"))
    (ors-remove ors "file.lisp")
    (is (not (ors-contains-p ors "file.lisp")))))

(test ors-concurrent-add-remove
  "Concurrent add survives a concurrent remove (key OR-Set property)."
  (let ((ors1 (make-or-set))
        (ors2 (make-or-set)))
    ;; Both add with different tags
    (ors-add ors1 "file.lisp" "s1:1")
    (ors-add ors2 "file.lisp" "s2:1")
    ;; Session 1 removes (tombstones only s1:1)
    (ors-remove ors1 "file.lisp")
    ;; Merge: s2:1 not tombstoned -> file.lisp survives
    (let ((merged (ors-merge ors1 ors2)))
      (is (ors-contains-p merged "file.lisp")))))

(test ors-members-filters-tombstoned
  (let ((ors (make-or-set)))
    (ors-add ors "a" "t1")
    (ors-add ors "b" "t2")
    (ors-remove ors "a")
    (let ((members (ors-members ors)))
      (is (= 1 (length members)))
      (is (member "b" members :test #'equal)))))

;;; --- LWW-Map ---

(test lwwm-set-get
  (let ((m (make-lww-map)))
    (lwwm-set m "status" "active" 100 "s1")
    (is (string= "active" (lwwm-get m "status")))
    (lwwm-set m "status" "done" 200 "s2")
    (is (string= "done" (lwwm-get m "status")))
    ;; Older write ignored
    (lwwm-set m "status" "old" 50 "s3")
    (is (string= "done" (lwwm-get m "status")))))

(test lwwm-merge-per-key
  (let ((m1 (make-lww-map))
        (m2 (make-lww-map)))
    (lwwm-set m1 "status" "active" 100 "s1")
    (lwwm-set m2 "status" "done" 200 "s2")
    (lwwm-set m1 "desc" "first" 300 "s1")
    (let ((merged (lwwm-merge m1 m2)))
      (is (string= "done" (lwwm-get merged "status")))
      (is (string= "first" (lwwm-get merged "desc"))))))
