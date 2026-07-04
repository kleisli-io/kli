(in-package #:kli/tests)
(in-suite all)

(defun cross-image-install-non-root (protocol context id)
  "Install ID as a non-root extension child -- the shape a nix-declared baseline
takes. It enters protocol-extensions (so it rides the snapshot :objects) but not
protocol-root-activations (so the snapshot :extensions never names it), which is
why a plain-core restore cannot reconstruct it from manifests."
  (let ((ext::*activating-extension* :nix-profile-root))
    (with-extension-load-authority
      (ext:install-manifest (lambda () (ext:make-extension :id id))
                            protocol context))))

(defun cross-image-pin-entry (&key (id :demo) (version "1.0.0"))
  (app::make-user-extension-entry
   :id id
   :manifest (lambda () (ext:make-extension :id id))
   :metadata (list :version version)))

(defun cross-image-gap-reasons (protocol)
  (mapcar (lambda (gap) (getf gap :reason))
          (app:restore-version-gaps protocol)))

(test (snapshot-tags-nix-baseline-ids-from-image :fixture interactive-authority)
  "snapshot-context records the running image's baked nix-declared baseline ids
in a dedicated :nix-baseline-ids field, read live through the install-set hook --
empty on plain core, the baked set on a configured image."
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (is (null (getf (snapshot:snapshot-context context) :nix-baseline-ids))
        "plain core tags an empty baseline")
    (let ((profiles:*nix-declared-baseline-ids* (list "fixture-ext")))
      (is (equal (list "fixture-ext")
                 (getf (snapshot:snapshot-context context) :nix-baseline-ids))
          "a configured image tags its baked baseline"))))

(test (cross-image-configured-to-plain-defers-not-crashes :fixture restore-authority)
  "A snapshot from a configured image -- a nix-declared baseline child captured in
:objects -- restored on plain core does not hard-crash rehydrate-live-objects. The
absent baseline owner is deferred and a :cross-image-baseline-gap is recorded."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (cross-image-install-non-root protocol context :fixture-ext)
    (is (not (member :fixture-ext (ext:protocol-root-activations protocol)))
        "the baseline child is non-root")
    (let ((snapshot (let ((profiles:*nix-declared-baseline-ids* (list "fixture-ext")))
                      (snapshot:snapshot-context context))))
      (is (member "fixture-ext" (getf snapshot :nix-baseline-ids) :test #'string=)
          "the source image tagged the baseline")
      (let* ((target-context (kli:make-kernel-host))
             (target (switch-to-extension-protocol target-context)))
        (finishes (snapshot::apply-snapshot target target-context snapshot))
        (is (member :cross-image-baseline-gap (cross-image-gap-reasons target))
            "the gap is diagnosed, not fatal")
        (let ((gap (find :cross-image-baseline-gap (app:restore-version-gaps target)
                         :key (lambda (gap) (getf gap :reason)))))
          (is (equal "fixture-ext" (getf gap :id))
              "the gap names the absent baseline extension"))))))

(test (cross-image-plain-to-configured-restores-clean :fixture restore-authority)
  "The reverse direction: a plain-core snapshot, whose :nix-baseline-ids is empty,
restored where the image bakes a baseline reconstructs without a cross-image gap.
The image's extra baseline is additive and the snapshot's own objects reconstruct."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (app::install-user-extension protocol (cross-image-pin-entry) context)
    (app:record-remote-install-pin
     protocol (list :id "demo" :source-kind :registry :version "1.0.0"))
    (let ((snapshot (snapshot:snapshot-context context)))
      (is (null (getf snapshot :nix-baseline-ids))
          "a plain-core snapshot carries no baseline")
      (let* ((target-context (kli:make-kernel-host))
             (target (switch-to-extension-protocol target-context)))
        (setf (gethash :demo (app::available-extensions target)) (cross-image-pin-entry))
        (let ((profiles:*nix-declared-baseline-ids* (list "image-baseline")))
          (finishes (snapshot::apply-snapshot target target-context snapshot)))
        (is (null (cross-image-gap-reasons target))
            "no cross-image gap when the snapshot carried no baseline")))))

(test cross-image-non-baseline-unreconstructed-still-errors
  "Safety preserved: an un-reconstructed :objects id that is not a snapshot
baseline is still a hard error, never silently deferred."
  (let ((snapshot (list :format-version 1 :kind :extension-protocol
                        :active-protocol :ghost-protocol
                        :extensions nil :unrestorable-extensions nil
                        :nix-baseline-ids nil :storage nil
                        :objects (list (list :id (snapshot::serialize-snapshot-value
                                                  :ghost)
                                             :slots nil :skipped-slots nil)))))
    (let ((context (kli:make-kernel-host)))
      (switch-to-extension-protocol context)
      (signals error (snapshot:restore-active-protocol context snapshot)))))
