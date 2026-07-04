(in-package #:kli/profiles)

(defparameter +profile-activations-key+ :kli/profiles/activations
  "Per-protocol storage key for the profile-activation table. Profiles are an
open contribution kind, a named ordered group of extension manifests installed
as a unit. The kernel never learns about profiles, installation routes through
the shared manifest-group primitives, and activation records live in keyed
storage rather than a protocol slot.")

(defstruct (profile-activation (:constructor make-profile-activation))
  id
  extensions
  seam)

(defun protocol-profile-activations (protocol)
  "Per-protocol hash mapping profile id → `profile-activation`. Lazily created."
  (ensure-protocol-storage protocol +profile-activations-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun protocol-profile-activation (protocol id)
  (gethash (normalize-extension-id id)
           (protocol-profile-activations protocol)))

(defclass profile-contribution (contribution)
  ((manifest-group
    :initarg :manifest-group
    :initform '()
    :reader contribution-manifest-group)
   (seam
    :initarg :seam
    :initform '()
    :reader contribution-seam
    :documentation "Capabilities a user supplies to complete this profile,
declared but never provided by the profile itself. Lets a stub name its
extension points.")))

(defun make-profile-contribution (&key name manifest-group seam source)
  (make-instance 'profile-contribution
                 :kind :profile
                 :name (normalize-extension-id name)
                 :manifest-group manifest-group
                 :seam seam
                 :source source))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution profile-contribution)
                                 context)
  (let ((handles (install-manifest-list
                  (contribution-manifest-group contribution)
                  protocol
                  context)))
    (setf (gethash (contribution-name contribution)
                   (protocol-profile-activations protocol))
          (make-profile-activation :id (contribution-name contribution)
                                   :extensions handles
                                   :seam (contribution-seam contribution)))
    (push contribution (protocol-installed-contributions protocol))
    contribution))

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution profile-contribution)
                                 context)
  (let* ((activations (protocol-profile-activations protocol))
         (id (contribution-name contribution))
         (record (gethash id activations)))
    (when record
      (retract-installed-extensions (profile-activation-extensions record)
                                    protocol
                                    context)
      (remhash id activations)))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defcontribution-kind :profile (extension-id form)
  (destructuring-bind (_ name group-form &key seam) form
    (declare (ignore _))
    `(make-profile-contribution
      :name ',(normalize-extension-id name)
      :manifest-group ,group-form
      :seam ',seam
      :source ',extension-id)))
