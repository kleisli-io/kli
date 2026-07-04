(in-package #:kli/tests)

(in-suite all)

;;; Grant serialization round-trips through the snapshot serializer.

(defun grant-datum-round-trips-p (grant)
  (ext:grant-equiv-p grant (ext:datum->grant (ext:grant->datum grant))))

(test grant-datum-round-trips-all-constraint-kinds
  (is (grant-datum-round-trips-p (ext:grant-top)))
  (is (grant-datum-round-trips-p (ext:grant-bottom)))
  (is (grant-datum-round-trips-p
       (ext:grant-without-atoms (ext:grant-top)
                                '(:file/write :file/edit :process/exec))))
  (is (grant-datum-round-trips-p (ext:make-grant :capabilities '(:a :b))))
  (is (grant-datum-round-trips-p
       (ext:make-grant
        :atoms (list (cons :file/read (ext:path-prefix-constraint "/home"))))))
  (is (grant-datum-round-trips-p
       (ext:make-grant
        :atoms (list (cons :proc/nice
                           (ext:numeric-bound-constraint :low 0 :high 19))))))
  (is (grant-datum-round-trips-p
       (ext:make-grant
        :atoms (list (cons :net/host
                           (ext:enumerated-constraint '("a" "b"))))))))

(test grant-datum-survives-snapshot-serializer
  ;; The load-bearing property at the value level: the datum is serializable
  ;; (proper lists, no dotted pairs), so the real serializer keeps it whole
  ;; rather than skipping it as it does structs and dotted pairs.
  (let* ((grant (ext:make-grant
                 :atoms (list (cons :file/read (ext:path-prefix-constraint "/home"))
                              (cons :proc/nice
                                    (ext:numeric-bound-constraint :low 0 :high 19)))))
         (datum (ext:grant->datum grant))
         (round (snapshot::deserialize-snapshot-value
                 (snapshot::serialize-snapshot-value datum))))
    (is (equal datum round))
    (is (ext:grant-equiv-p grant (ext:datum->grant round)))))

;;; Co-finite grants: the top minus a finite set of atoms, the only shape able
;;; to represent "every authority but these actuators".

(test grant-without-atoms-on-top-yields-co-finite-authority
  "Removing atoms from the universal grant keeps every other capability and
denies exactly the removed ones -- not the bottom-collapse a positive subtraction
from an un-enumerable top would give."
  (let ((g (ext:grant-without-atoms (ext:grant-top)
                                    '(:file/write :file/edit :process/exec))))
    (is (ext:grant-covers-p g (ext:make-grant :capabilities '(:file/read))))
    (is (ext:grant-covers-p g (ext:make-grant :capabilities '(:cairn/read))))
    (is (null (ext:grant-covers-p g (ext:make-grant :capabilities '(:file/write)))))
    (is (null (ext:grant-covers-p g (ext:make-grant :capabilities '(:process/exec)))))
    (is (eq :universal-except
            (getf (ext:grant-report g) :scope)))
    (is (equal '("file/edit" "file/write" "process/exec")
               (getf (ext:grant-report g) :excluded)))))

(test co-finite-grant-sits-strictly-below-the-top
  "A co-finite grant is covered by the top and covers the bottom, but the top is
not covered by it -- it is a proper intermediate element."
  (let ((g (ext:grant-without-atoms (ext:grant-top) '(:process/exec))))
    (is (ext:grant-covers-p (ext:grant-top) g))
    (is (ext:grant-covers-p g (ext:grant-bottom)))
    (is (null (ext:grant-covers-p g (ext:grant-top))))
    (is (null (ext:grant-equiv-p g (ext:grant-top))))))

(test co-finite-meet-unions-exclusions
  "Meeting two co-finite grants withholds the union of their exclusions, so the
meet is covered by each input."
  (let* ((a (ext:grant-without-atoms (ext:grant-top) '(:file/write)))
         (b (ext:grant-without-atoms (ext:grant-top) '(:process/exec)))
         (m (ext:grant-meet a b)))
    (is (ext:grant-covers-p a m))
    (is (ext:grant-covers-p b m))
    (is (ext:grant-covers-p m (ext:make-grant :capabilities '(:file/read))))
    (is (null (ext:grant-covers-p m (ext:make-grant :capabilities '(:file/write)))))
    (is (null (ext:grant-covers-p m (ext:make-grant :capabilities '(:process/exec)))))))

(test co-finite-meet-bounded-drops-excluded-atoms
  "Meeting a co-finite grant with a bounded one keeps the bounded atoms the
co-finite side does not withhold and drops the rest."
  (let* ((co (ext:grant-without-atoms (ext:grant-top) '(:process/exec)))
         (bounded (ext:make-grant :capabilities '(:file/read :process/exec)))
         (m (ext:grant-meet co bounded)))
    (is (ext:grant-covers-p m (ext:make-grant :capabilities '(:file/read))))
    (is (null (ext:grant-covers-p m (ext:make-grant :capabilities '(:process/exec)))))
    (is (ext:grant-covers-p bounded m))))

(test top-minus-nothing-is-the-top
  "Removing no atoms leaves the universal grant the top, serialized as :universal."
  (let ((g (ext:grant-without-atoms (ext:grant-top) '())))
    (is (ext:grant-equiv-p g (ext:grant-top)))
    (is (equal '(:universal) (ext:grant->datum g)))
    (is (eq :universal (getf (ext:grant-report g) :scope)))))

;;; Conferral attenuates; escalation is unrepresentable.

(test confer-attenuates-and-rejects-escalation
  (let* ((holder (ext:make-grant :capabilities '(:file/read :file/write)))
         (delegated (ext:confer holder (ext:make-grant :capabilities '(:file/read)))))
    (is (ext:grant-covers-p holder delegated))
    (is (ext:check-capability (ext:make-subject :grant delegated) :file/read))
    (is (null (ext:check-capability (ext:make-subject :grant delegated) :file/write)))
    (signals ext:grant-escalation
      (ext:confer holder
                  (ext:make-grant :capabilities '(:file/read :process/exec))))))

;;; Granting and revoking are the install/retract morphism on the grant-set.

(test grant-install-enforces-and-retract-revokes
  (let* ((protocol (ext:make-extension-protocol))
         (contribution (ext:make-grant-contribution
                        :principal :planner
                        :grant (ext:make-grant :capabilities '(:file/read)))))
    (ext:install-contribution protocol contribution nil)
    (is (ext:check-authority protocol :planner :file/read))
    (is (null (ext:check-authority protocol :planner :file/write)))
    (ext:retract-contribution protocol contribution nil)
    (is (null (ext:check-authority protocol :planner :file/read)))
    (is (null (ext:protocol-grant-set protocol)))))

;;; The dynamic guard and the grant-set are one lattice: a principal-subject
;;; bound to *call-subject* gates identically to check-authority.

(ext:defextension test-planner-grant
  (:provides
   (grant planner
     (ext:make-grant :capabilities '(:file/read :file/write)))))

(test (grant-manifest-binds-guard-to-grant-set :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (handle (install-extension context *test-planner-grant-extension-manifest*)))
    (is (ext:check-authority protocol :planner :file/read))
    (is (ext:check-authority protocol :planner :file/write))
    (is (null (ext:check-authority protocol :planner :file/delete)))
    (let ((ext:*call-subject* (ext:principal-subject protocol :planner)))
      (is (ext:check-capability ext:*call-subject* :file/read))
      (is (null (ext:check-capability ext:*call-subject* :file/delete))))
    (ext:retract-manifest handle protocol context)
    (is (null (ext:check-authority protocol :planner :file/read)))))

;;; The grant-set snapshots through protocol storage and restores into a fresh
;;; protocol with identical authority -- nothing silently skipped.

(defun read-constraint-request (capability prefix)
  (ext:make-grant
   :atoms (list (cons capability (ext:path-prefix-constraint prefix)))))

(test grant-set-snapshot-restores-identical-authority
  (let* ((source (ext:make-extension-protocol))
         (planner (ext:make-grant :capabilities '(:file/read :file/write)))
         (triager (ext:grant-meet
                   planner
                   (read-constraint-request :file/read "/home"))))
    (ext:install-contribution
     source (ext:make-grant-contribution :principal :planner :grant planner) nil)
    (ext:install-contribution
     source (ext:make-grant-contribution :principal :triager :grant triager) nil)
    (multiple-value-bind (captured skipped)
        (snapshot::snapshot-protocol-storage source)
      (is (null skipped))
      (let ((restored (ext:make-extension-protocol)))
        (snapshot::rehydrate-protocol-storage restored (list :storage captured))
        (is (ext:grant-equiv-p planner (ext:grant-set-lookup restored :planner)))
        (is (ext:grant-equiv-p triager (ext:grant-set-lookup restored :triager)))
        ;; planner is unconstrained; the attenuated triager is confined to /home
        (is (ext:check-authority restored :planner
                                 (read-constraint-request :file/read "/etc")))
        (is (null (ext:check-authority restored :triager
                                       (read-constraint-request :file/read "/etc"))))
        (is (ext:check-authority restored :triager
                                 (read-constraint-request :file/read "/home/user")))))))
