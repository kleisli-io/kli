(in-package #:kli/tests)

(in-suite all)

;;; The bounded named principals. These tests pin the interactive and
;;; extension-load authority surfaces so that widening either one -- or letting
;;; a new gated operation slip in unaccounted-for -- turns the suite red instead
;;; of a user's session. They are the standing forcing function behind "no
;;; principal outside boot holds the full lattice."

(defparameter *interactive-surface-atoms*
  '(:agent/session/submit
    :agent/session/switch
    :agent/session/branch
    :agent/session/listen
    :agent/session/compact
    :agent/session/focus
    :context/read
    :context/stage-edit
    :context/commit-edit
    :context/seal
    :auth/register-reference
    :auth/forget-secret
    :auth/resolve-secret
    :auth/read-metadata
    :protocol/create
    :protocol/switch
    :protocol/rollback
    :protocol/recover
    :protocol/snapshot
    :protocol/restore
    :manifest/install
    :manifest/retract
    :behavior/hotpatch
    :behavior/state)
  "Every authority the interactive first-party command and construction surface
exercises. The interactive subject must cover exactly these.")

(defparameter *model-actuator-atoms*
  '(:file/read :file/write :file/edit :process/exec :image/eval :image/recode
    :manifest/install-remote :extension/spawn-process)
  "Authority conferred per tool call on a narrower subject, never ambient in the
interactive surface. The interactive subject must deny all of these.")

(defparameter *extension-load-atoms*
  '(:manifest/install :manifest/retract :image/eval
    :extension/spawn-process :manifest/install-remote
    :auth/register-reference)
  "Authority the extension-load boundary needs and nothing more. Provider
extensions register a credential reference at install time, so the load boundary
covers that registration alongside install and source-evaluation.")

(test bounded-principals-are-not-the-system-subject
  "The interactive and extension-load principals are bounded grants, not the top
of the lattice."
  (is (not (typep ext:*ui-subject* 'ext:system-subject)))
  (is (not (typep ext:*install-subject* 'ext:system-subject))))

(test interactive-subject-covers-the-whole-command-surface
  "Every authority a first-party command or the construction path reaches is
covered, so a new command that needs new authority fails here rather than at a
user's boot."
  (dolist (atom *interactive-surface-atoms*)
    (is (ext:check-capability ext:*ui-subject* atom)
        "interactive subject must cover ~S" atom)))

(test interactive-subject-denies-model-actuators
  "The interactive surface confers none of the model's own actuators, so a
worker that inherits it cannot write, run a process, or evaluate."
  (dolist (atom *model-actuator-atoms*)
    (is (not (ext:check-capability ext:*ui-subject* atom))
        "interactive subject must deny ~S" atom)))

(test extension-load-subject-covers-and-bounds
  "The extension-load principal covers the install and source-evaluation
authority a load needs. Evaluating source is first-party image authority, so it
also confers the file and process atoms eval implies; it still grants no
interactive session authority and is not the lattice top."
  (dolist (atom *extension-load-atoms*)
    (is (ext:check-capability ext:*install-subject* atom)
        "extension-load subject must cover ~S" atom))
  (dolist (atom '(:file/write :file/edit :process/exec))
    (is (ext:check-capability ext:*install-subject* atom)
        "evaluating source confers ~S through the eval implication" atom))
  (dolist (atom '(:agent/session/switch :agent/session/submit :context/seal))
    (is (not (ext:check-capability ext:*install-subject* atom))
        "extension-load subject must deny ~S" atom)))

(test interactive-boot-self-establishes-authority-under-narrow-ambient
  "Driving the real interactive boot under an empty ambient principal must
succeed: production self-establishes the authority each stage needs. A boot
stage that fails to take its own authority would deny here, so this is the
standing regression net for the launch path."
  (let ((ext:*call-subject* (ext:make-default-subject)))
    (let* ((settings (config:load-settings))
           (context (app:main :profile :interactive-terminal :settings settings)))
      (app:boot-user-extensions context)
      (let ((app (app:build-tui-app context)))
        (is (typep app 'tui-app:tui-app))
        (is (not (typep ext:*call-subject* 'ext:system-subject))
            "ambient principal stays narrow after a self-establishing boot")))))
