(in-package #:kli/profiles)

(define-condition unknown-profile (error)
  ((name :initarg :name :reader unknown-profile-name)
   (known :initarg :known :reader unknown-profile-known))
  (:report (lambda (condition stream)
             (format stream "Unknown profile ~S. Known profiles: ~{~S~^, ~}."
                     (unknown-profile-name condition)
                     (unknown-profile-known condition)))))

(defun profile-registry ()
  "Alist of profile id keyword → its extension manifest factory."
  (list (cons :headless *headless-extension-manifest*)
        (cons :print *print-extension-manifest*)
        (cons :interactive-terminal *interactive-terminal-extension-manifest*)
        (cons :human-in-loop *human-in-loop-extension-manifest*)
        (cons :autonomous *autonomous-extension-manifest*)))

(defun known-profile-names ()
  (mapcar #'car (profile-registry)))

(defun find-profile-manifest (name)
  "Resolve a profile NAME (keyword, string, or symbol) to its extension
manifest, signalling UNKNOWN-PROFILE when nothing matches."
  (let* ((id (normalize-extension-id name))
         (entry (assoc id (profile-registry))))
    (if entry
        (cdr entry)
        (error 'unknown-profile :name id :known (known-profile-names)))))

(defparameter +active-profile-key+ :kli/profiles/active-profile
  "Per-protocol storage key for the resolved profile recorded at boot. The
deltas gate the user-extension install phase and the settings overlay rides
along for the config layer, while the kernel never learns about profiles.")

(defun record-active-profile (protocol resolved)
  "Record RESOLVED as PROTOCOL's active profile. Returns RESOLVED."
  (setf (protocol-storage protocol +active-profile-key+) resolved))

(defun protocol-active-profile (protocol)
  "The resolved profile recorded for PROTOCOL, or NIL when none was."
  (protocol-storage protocol +active-profile-key+))
