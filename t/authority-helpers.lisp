(in-package #:kli/tests)

;;; Test-tier authority. Production establishes authority at named seams: the
;;; interactive surface runs under the interactive principal, extension loading
;;; under the extension-load principal, a model tool under the per-call agent
;;; grant a profile confers. A test that drives that substrate in isolation must
;;; establish the SAME bounded authority. Two anti-patterns these wrappers exist
;;; to remove: wrapping the runner (or a test) in the full lattice, which vacates
;;; every capability-gate assertion; and minting a broad ad-hoc driver subject,
;;; which lets test authority drift from production. The interactive and
;;; extension-load wrappers inherit the production principals verbatim, so the
;;; surface a test is allowed to reach tracks the surface production grants.
;;;
;;; Each is exposed twice: a macro, for a substrate helper function that drives a
;;; gated primitive; and a fixture of the same intent, so a test takes its
;;; authority through a one-line `:fixture` header rather than an indented body
;;; wrap.

(defmacro with-interactive-authority (&body body)
  "Drive first-party interactive, session, context, and auth substrate under the
production interactive principal."
  `(let ((ext:*call-subject* ext:*ui-subject*))
     ,@body))

(defmacro with-extension-load-authority (&body body)
  "Drive extension install, retract, and source-evaluation substrate under the
production extension-load principal."
  `(let ((ext:*call-subject* ext:*install-subject*))
     ,@body))

(defmacro with-granted-authority ((&rest capabilities) &body body)
  "Bind a subject granted exactly CAPABILITIES (closed under their implications)
-- the explicit-grant tier. Use when the authority is the thing under test: a
model tool driven as a profile would confer it, or a substrate primitive whose
own capability is exercised. Never ambient, never the full lattice. Defaults to
the standard tool grant when none are named."
  `(let ((ext:*call-subject*
           (ext:make-subject :capabilities ',(or capabilities '(:tools/standard)))))
     ,@body))

(def-fixture interactive-authority ()
  (with-interactive-authority (&body)))

(def-fixture extension-load-authority ()
  (with-extension-load-authority (&body)))

(def-fixture tool-authority ()
  (with-granted-authority (:tools/standard :image/eval) (&body)))

(def-fixture recode-authority ()
  (with-granted-authority (:image/recode) (&body)))

;;; Substrate that crosses more than one production seam in a single test.
;;; Restore replays installs, so it needs the install surface plus the snapshot
;;; and restore protocol authority. A lifted-tool test confers and then invokes
;;; an isolated server's atom, so the installer must hold that atom to grant it.
;;; A holistic image test drives the interactive session surface and also recodes
;;; or evaluates the image. Each grant is bounded and named -- never the lattice.

(def-fixture restore-authority ()
  (with-granted-authority (:manifest/install :manifest/retract :manifest/install-remote
                           :image/eval :extension/spawn-process
                           :protocol/snapshot :protocol/restore
                           :auth/register-reference)
    (&body)))

(def-fixture lifted-tool-authority ()
  (with-granted-authority (:extension/spawn-process :manifest/install :manifest/retract
                           :isolated/fixture/echo)
    (&body)))

(def-fixture image-session-authority ()
  (with-granted-authority (:agent/session/submit :agent/session/switch :agent/session/branch
                           :agent/session/listen :agent/session/compact
                           :context/read :context/stage-edit :context/commit-edit :context/seal
                           :auth/register-reference :auth/resolve-secret
                           :behavior/hotpatch :behavior/state
                           :manifest/install :manifest/retract
                           :image/eval :image/recode)
    (&body)))
