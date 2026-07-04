(in-package #:kli/tests)

(in-suite all)

(test system-subject-passes-any-capability
  (let ((subject (ext:make-system-subject)))
    (is (ext:check-capability subject :any/cap))
    (is (ext:check-capability subject :totally/made-up))
    (is (ext:check-capability subject :image/eval))))

(test default-subject-rejects-unlisted-capability
  (let ((subject (ext:make-subject :capabilities '())))
    (is (null (ext:check-capability subject :anything)))
    (is (null (ext:check-capability subject :image/eval)))))

(test default-subject-accepts-listed-capability
  (let ((subject (ext:make-subject :capabilities '(:foo :bar))))
    (is (ext:check-capability subject :foo))
    (is (ext:check-capability subject :bar))
    (is (null (ext:check-capability subject :baz)))))

(test make-subject-expands-implications
  (let ((subject (ext:make-subject :capabilities '(:image/recode))))
    (is (ext:check-capability subject :image/recode))
    (is (ext:check-capability subject :manifest/install))
    (is (ext:check-capability subject :manifest/retract))))

(test make-subject-implication-fixed-point
  (let* ((kli/ext::*capability-implications*
           '(:a (:b)
             :b (:c)))
         (subject (ext:make-subject :capabilities '(:a))))
    (is (ext:check-capability subject :a))
    (is (ext:check-capability subject :b))
    (is (ext:check-capability subject :c))))

(test require-capability-signals-capability-denied
  (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
    (signals ext:capability-denied
      (ext:require-capability :foo))))

(test require-capability-passes-under-system-subject
  (let ((ext:*call-subject* (ext:make-system-subject)))
    (finishes (ext:require-capability :foo))
    (finishes (ext:require-capability :image/eval))))

(test capability-denied-carries-subject-and-capability
  (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
    (handler-case (ext:require-capability :missing/cap)
      (ext:capability-denied (condition)
        (is (eq :missing/cap
                (ext:capability-denied-capability condition)))
        (is (eq ext:*call-subject*
                (ext:capability-denied-subject condition)))))))

(test call-subject-default-denies-gated-capability
  (is (not (typep ext:*call-subject* 'ext:system-subject)))
  (is (null (ext:check-capability ext:*call-subject* :file/write)))
  (signals ext:capability-denied (ext:require-capability :file/write)))

(test make-default-subject-denies-everything
  (let ((subject (ext:make-default-subject)))
    (is (not (typep subject 'ext:system-subject)))
    (is (null (ext:check-capability subject :file/write)))
    (is (null (ext:check-capability subject :image/eval)))))

(test with-system-authority-takes-the-full-lattice
  (ext:with-system-authority
    (is (typep ext:*call-subject* 'ext:system-subject))
    (is (ext:check-capability ext:*call-subject* :file/write))))

(test unrestricted-subject-passes-every-capability-without-being-system
  "The unrestricted root fallback is an ordinary subject over the lattice top: it
passes every capability check yet is not a `system-subject`, so it is a policy
grant the usual attenuation can narrow, not the substrate authority."
  (let ((subject (ext:make-unrestricted-subject)))
    (is (not (typep subject 'ext:system-subject)))
    (dolist (cap '(:file/read :file/write :process/exec :cairn/read
                   :image/eval :totally/made-up))
      (is (ext:check-capability subject cap)
          "the unrestricted subject confers ~A" cap))
    (is (eq :universal
            (getf (ext:grant-report (ext:subject-grant subject)) :scope)))))

(test unrestricted-subject-narrows-under-meet
  "Meeting the unrestricted subject with a bounded one yields exactly the bounded
authority -- the top attenuates like any grant rather than staying universal."
  (let* ((bounded (ext:make-subject :capabilities '(:file/read)))
         (met (ext:subject-meet (ext:make-unrestricted-subject) bounded)))
    (is (ext:check-capability met :file/read))
    (is (null (ext:check-capability met :process/exec)))))
