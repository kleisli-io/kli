(in-package #:kli/tests)

(in-suite all)

;;; Isolated-server snapshot/restore: a lifted MCP extension rides its pin only.
;;; Restore re-spawns and re-handshakes the server; a server that cannot re-spawn
;;; surfaces a non-fatal :isolated-server-unavailable gap. The grant-set rides
;;; protocol storage and restores for free. Reuses the sh fixture from
;;; t/mcp-client and cross-image-gap-reasons from t/cross-image-snapshot.
;;; Spawning runs in the nix sandbox only.

(defun isolated-fixture-spec ()
  (list :command "sh"
        :arguments (list "-c" (mcp-fixture-script))
        :id :fixture
        :timeout 5))

(defun isolated-fixture-grant ()
  (ext:lifted-server-grant :fixture '("echo")))

(defun confer-fixture-grant (protocol context)
  (ext:install-contribution
   protocol
   (ext:make-grant-contribution :principal :fixture :grant (isolated-fixture-grant))
   context))

(defun isolated-pin-with-command (pin command)
  (loop for (key value) on pin by #'cddr
        append (list key (if (eq key :command) command value))))

(defun id-in-snapshot-isolated-set-p (id snapshot)
  (member (ext:normalize-extension-id id)
          (mapcar #'ext:normalize-extension-id
                  (getf snapshot :isolated-server-ids))))

(test (isolated-install-records-serializable-pin :fixture restore-authority)
  "Installing a lifted server records an :isolated-server pin that rides the
snapshot whole, and the snapshot tags the server id for the tolerant restore."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (multiple-value-bind (state handle)
        (app:install-isolated-extension (isolated-fixture-spec) protocol context
                                        :confirm-fn (lambda (stage card)
                                                      (declare (ignore stage card)) t))
      (is (eq :installed state) "the fixture server installs")
      (confer-fixture-grant protocol context)
      (let ((pin (gethash "fixture" (app:remote-install-pins protocol))))
        (is (not (null pin)) "install records a pin")
        (is (eq :isolated-server (getf pin :source-kind)))
        (is (string= "sh" (getf pin :command))))
      (let* ((snapshot (snapshot:snapshot-context context))
             (pins (app::snapshot-remote-install-pins snapshot))
             (pin (find "fixture" pins :key (lambda (p) (getf p :id)) :test #'string=)))
        (is (not (null pin)) "the pin round-trips through the snapshot")
        (is (eq :isolated-server (getf pin :source-kind)))
        (is (ext:grant-equiv-p (isolated-fixture-grant)
                               (ext:grant-set-lookup protocol :fixture))
            "the conferred grant is live before the snapshot")
        (is (id-in-snapshot-isolated-set-p :fixture snapshot)
            "the snapshot tags the isolated server id"))
      (ext:retract-manifest handle protocol context))))

(test (isolated-restore-present-server-relifts-and-keeps-grant :fixture restore-authority)
  "Restore with the server present re-spawns it: the lifted tool is live and
invocable again, the grant-set survives, and no gap is recorded."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (multiple-value-bind (state handle)
        (app:install-isolated-extension (isolated-fixture-spec) protocol context
                                        :confirm-fn (lambda (stage card)
                                                      (declare (ignore stage card)) t))
      (is (eq :installed state))
      (confer-fixture-grant protocol context)
      (let ((snapshot (snapshot:snapshot-context context)))
        (ext:retract-manifest handle protocol context)
        (let* ((target-context (kli:make-kernel-host))
               (target (switch-to-extension-protocol target-context)))
          (finishes (snapshot::apply-snapshot target target-context snapshot))
          (is (not (null (ext:find-tool target "echo")))
              "a present-server restore re-lifts the tool")
          (is (null (cross-image-gap-reasons target))
              "a successful re-spawn records no gap")
          (is (ext:grant-equiv-p (isolated-fixture-grant)
                                 (ext:grant-set-lookup target :fixture))
              "the grant-set rides the snapshot and restores")
          (let* ((ext:*call-subject* (ext:make-subject
                                      :grant (isolated-fixture-grant)))
                 (result (ext:invoke-tool target "echo" nil target-context)))
            (is (not (ext:tool-result-error-p result))
                "the re-lifted tool is invocable")))))))

(test (isolated-restore-absent-server-defers-non-fatally :fixture restore-authority)
  "Restore with the server absent does not crash: the re-spawn is deferred and an
:isolated-server-unavailable gap is recorded that survives the full restore."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (multiple-value-bind (state handle)
        (app:install-isolated-extension (isolated-fixture-spec) protocol context
                                        :confirm-fn (lambda (stage card)
                                                      (declare (ignore stage card)) t))
      (is (eq :installed state))
      (let ((pin (gethash "fixture" (app:remote-install-pins protocol))))
        (app:record-remote-install-pin
         protocol (isolated-pin-with-command pin "kli-no-such-mcp-server")))
      (let ((snapshot (snapshot:snapshot-context context)))
        (ext:retract-manifest handle protocol context)
        (let* ((target-context (kli:make-kernel-host))
               (target (switch-to-extension-protocol target-context)))
          (finishes (snapshot::apply-snapshot target target-context snapshot))
          (is (member :isolated-server-unavailable (cross-image-gap-reasons target))
              "an un-re-spawnable server surfaces a non-fatal gap")
          (is (null (ext:find-tool target "echo"))
              "no tool is live when the server could not re-spawn"))))))

(defun bounded-install-grant ()
  "The bounded authority a lifted install runs under -- install plus spawn,
implication-closed -- as a grant, matching what the production install binds."
  (ext:subject-grant
   (ext:make-subject
    :capabilities '(:manifest/install :extension/spawn-process))))

(test isolated-pin-carries-bounded-install-grant
  "build-isolated-pin records the bounded install grant as a serializable datum
that rides the pin whitelist and reconstructs to an equal grant; a pin built
without that slot still validates, so older pins ride unchanged."
  (let* ((grant (bounded-install-grant))
         (pin (app::build-isolated-pin :fixture :fixture
                                       (isolated-fixture-spec) grant)))
    (is (app::pin-value-serializable-p (getf pin :install-grant))
        "the carried grant datum rides the pin whitelist")
    (finishes (app::validate-pin pin))
    (is (ext:grant-equiv-p grant (ext:datum->grant (getf pin :install-grant)))
        "the carried datum reconstructs to the original bounded grant")
    (let ((legacy (loop for (key value) on pin by #'cddr
                        unless (eq key :install-grant) append (list key value))))
      (finishes (app::validate-pin legacy)))))

(test (isolated-install-pin-grant-round-trips-through-snapshot
       :fixture restore-authority)
  "An installed lifted server records its bounded install grant in the pin, and
that grant rides the snapshot serializer intact, reconstructing to the authority
the install ran under."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (multiple-value-bind (state handle)
        (app:install-isolated-extension (isolated-fixture-spec) protocol context
                                        :confirm-fn (lambda (stage card)
                                                      (declare (ignore stage card)) t))
      (is (eq :installed state))
      (let ((pin (gethash "fixture" (app:remote-install-pins protocol))))
        (is (ext:grant-equiv-p (bounded-install-grant)
                               (ext:datum->grant (getf pin :install-grant)))
            "the recorded pin carries the bounded install grant"))
      (let* ((snapshot (snapshot:snapshot-context context))
             (pins (app::snapshot-remote-install-pins snapshot))
             (pin (find "fixture" pins
                        :key (lambda (p) (getf p :id)) :test #'string=)))
        (is (not (null pin)) "the pin rides the snapshot")
        (is (ext:grant-equiv-p (bounded-install-grant)
                               (ext:datum->grant (getf pin :install-grant)))
            "the carried install grant round-trips through the snapshot intact"))
      (ext:retract-manifest handle protocol context))))

(defun isolated-pin-with-grant (pin grant)
  "PIN with its carried install grant replaced by GRANT's datum -- builds a pin
whose recorded authority is narrower than a production install, to witness that
restore is bounded by the recorded grant, never the restorer's spare authority."
  (loop for (key value) on pin by #'cddr
        append (list key (if (eq key :install-grant)
                             (ext:grant->datum grant) value))))

(test (isolated-restore-without-spawn-authority-defers :fixture restore-authority)
  "A restorer lacking spawn authority cannot re-spawn an isolated server: the
re-spawn is denied before any subprocess starts, an :isolated-server-unavailable
gap is recorded, and restore completes -- restore confers no spawn authority the
restoring subject never held."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (multiple-value-bind (state handle)
        (app:install-isolated-extension (isolated-fixture-spec) protocol context
                                        :confirm-fn (lambda (stage card)
                                                      (declare (ignore stage card)) t))
      (is (eq :installed state))
      (let ((snapshot (snapshot:snapshot-context context)))
        (ext:retract-manifest handle protocol context)
        (let ((target-context (kli:make-kernel-host))
              (restored nil))
          (switch-to-extension-protocol target-context)
          (finishes
            (let ((ext:*call-subject*
                    (ext:make-subject
                     :capabilities '(:protocol/restore :manifest/install))))
              (setf restored
                    (snapshot:restore-active-protocol target-context snapshot))))
          (is (member :isolated-server-unavailable
                      (cross-image-gap-reasons restored))
              "the deferred re-spawn surfaces a non-fatal gap")
          (is (null (ext:find-tool restored "echo"))
              "no tool is live when the restorer lacks spawn authority"))))))

(test (isolated-restore-with-covering-authority-relifts :fixture restore-authority)
  "A restorer covering the pin's recorded authority re-spawns the server under the
meet of its grant and the pin's: the lifted tool is live and invocable and no gap
is recorded."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (multiple-value-bind (state handle)
        (app:install-isolated-extension (isolated-fixture-spec) protocol context
                                        :confirm-fn (lambda (stage card)
                                                      (declare (ignore stage card)) t))
      (is (eq :installed state))
      (confer-fixture-grant protocol context)
      (let ((snapshot (snapshot:snapshot-context context)))
        (ext:retract-manifest handle protocol context)
        (let ((target-context (kli:make-kernel-host))
              (restored nil))
          (switch-to-extension-protocol target-context)
          (finishes
            (let ((ext:*call-subject*
                    (ext:make-subject
                     :capabilities '(:protocol/restore :manifest/install
                                     :extension/spawn-process))))
              (setf restored
                    (snapshot:restore-active-protocol target-context snapshot))))
          (is (not (null (ext:find-tool restored "echo")))
              "a covering restorer re-lifts the tool")
          (is (null (cross-image-gap-reasons restored))
              "a successful re-spawn records no gap")
          (let* ((ext:*call-subject* (ext:make-subject
                                      :grant (isolated-fixture-grant)))
                 (result (ext:invoke-tool restored "echo" nil target-context)))
            (is (not (ext:tool-result-error-p result))
                "the re-lifted tool is invocable")))))))

(test (isolated-restore-bounded-by-recorded-grant-defers :fixture restore-authority)
  "Even a fully authorized restorer cannot re-spawn beyond the pin's recorded
grant: a pin whose recorded authority omits spawn defers under the meet, records
the non-fatal gap, and starts no subprocess -- restore restores recorded
authority, it does not fabricate it."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (multiple-value-bind (state handle)
        (app:install-isolated-extension (isolated-fixture-spec) protocol context
                                        :confirm-fn (lambda (stage card)
                                                      (declare (ignore stage card)) t))
      (is (eq :installed state))
      (let ((pin (gethash "fixture" (app:remote-install-pins protocol)))
            (install-only (ext:subject-grant
                           (ext:make-subject :capabilities '(:manifest/install)))))
        (app:record-remote-install-pin
         protocol (isolated-pin-with-grant pin install-only)))
      (let ((snapshot (snapshot:snapshot-context context)))
        (ext:retract-manifest handle protocol context)
        (let ((target-context (kli:make-kernel-host))
              (restored nil))
          (switch-to-extension-protocol target-context)
          (finishes
            (let ((ext:*call-subject*
                    (ext:make-subject
                     :capabilities '(:protocol/restore :manifest/install
                                     :extension/spawn-process))))
              (setf restored
                    (snapshot:restore-active-protocol target-context snapshot))))
          (is (member :isolated-server-unavailable
                      (cross-image-gap-reasons restored))
              "a pin recording no spawn authority cannot re-spawn, even for a ~
               covering restorer")
          (is (null (ext:find-tool restored "echo"))
              "no tool is live when the recorded grant omits spawn"))))))

(test (isolated-restore-without-pins-stays-clean :fixture restore-authority)
  "The restorer-grant hook is threaded even with no isolated pins to replay: a
snapshot carrying none restores without an isolated-server gap, confirming the
extended hook contract did not regress the ordinary path."
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((snapshot (snapshot:snapshot-context context)))
      (is (null (getf snapshot :isolated-server-ids))
          "the snapshot carries no isolated pins")
      (let ((target-context (kli:make-kernel-host))
            (restored nil))
        (switch-to-extension-protocol target-context)
        (finishes
          (let ((ext:*call-subject*
                  (ext:make-subject :capabilities '(:protocol/restore))))
            (setf restored
                  (snapshot:restore-active-protocol target-context snapshot))))
        (is (null (cross-image-gap-reasons restored))
            "no gap is recorded when there are no isolated pins")))))
