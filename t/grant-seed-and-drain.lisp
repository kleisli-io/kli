(in-package #:kli/tests)

(in-suite all)

;;; Seeding the grant-set with each agent's own principal, carrying the actor on
;;; the model request, draining a delegation by its own owner, draining an
;;; agent's runs on deregister, and validating a contribution before installing
;;; it. Five independent reversibility/attribution seams, all pure except the
;;; ones that need a kernel host for the live-object registry.

;;; --- C1: grant-set principal reachability ----------------------------------

(test seeded-agent-principal-projects-without-widening
  "A constructed agent carries a principal; seeding records its own configured
authority in the grant-set so agent-call-subject projects through the map, and
an un-delegated agent gates identically -- the seed is not a widening."
  (let* ((protocol (ext:make-extension-protocol))
         (a-read (ext:lifted-tool-atom :fs "read"))
         (a-write (ext:lifted-tool-atom :fs "write"))
         (subject (ext:make-subject :capabilities (list a-read)))
         (agent (agents:make-agent nil nil nil nil nil
                                   :id :seed-agent :subject subject)))
    (is (agents:agent-principal agent)
        "every constructed agent carries a real principal")
    (is (eq (agents::agent-call-subject agent protocol) subject)
        "before seeding the grant-set is not consulted -- the slot subject gates")
    (is (null (ext:grant-set-has-p protocol (agents:agent-principal agent)))
        "nothing is recorded before seeding")
    (agents:seed-agent-principal-grant agent protocol nil)
    (is (ext:grant-set-has-p protocol (agents:agent-principal agent))
        "seeding records the agent's principal in the grant-set")
    (let ((projected (agents::agent-call-subject agent protocol)))
      (is (ext:check-capability projected a-read)
          "the projected subject keeps the configured read authority")
      (is (null (ext:check-capability projected a-write))
          "seeding does not widen -- write stays denied"))))

;;; --- C2: delegation drain --------------------------------------------------

(defun drain-delegation-owner (protocol owner-id)
  "Retract every contribution attributed to OWNER-ID: the grant-set drain
deactivate-extension performs, isolated from the live-object registry so the
test needs no kernel host."
  (let ((owner (ext:make-extension :id owner-id)))
    (dolist (contribution (ext::installed-contributions-for-extension protocol
                                                                      owner))
      (ext:retract-contribution protocol contribution nil))))

(defun seed-parent-grant (protocol parent capabilities)
  (ext:install-contribution
   protocol
   (ext:make-grant-contribution :principal parent
                                :grant (ext:make-grant :capabilities capabilities))
   nil))

(test delegation-stamps-a-drainable-owner
  "A delegated grant is attributed to its per-delegation owner, so draining that
owner removes exactly the delegated grant and leaves no orphan contribution."
  (let ((protocol (ext:make-extension-protocol)))
    (seed-parent-grant protocol :planner '(:x))
    (ext:delegate-grant protocol :planner :triager
                        (ext:make-grant :capabilities '(:x)))
    (is (ext:grant-set-has-p protocol :triager) "the delegation is recorded")
    (drain-delegation-owner protocol (ext:delegation-owner-id :planner :triager))
    (is (null (ext:grant-set-has-p protocol :triager))
        "draining the delegation owner removes the delegated grant")
    (is (ext:grant-set-has-p protocol :planner)
        "the parent's own grant is untouched")
    (is (null (ext::installed-contributions-for-extension
               protocol
               (ext:make-extension :id (ext:delegation-owner-id :planner
                                                                :triager))))
        "no orphan contribution survives the drain")))

(test delegation-drain-is-sibling-independent
  "Each delegation owns its own drain key, so revoking one leaves the other
delegations from the same parent intact."
  (let ((protocol (ext:make-extension-protocol)))
    (seed-parent-grant protocol :planner '(:x))
    (ext:delegate-grant protocol :planner :triager-a
                        (ext:make-grant :capabilities '(:x)))
    (ext:delegate-grant protocol :planner :triager-b
                        (ext:make-grant :capabilities '(:x)))
    (drain-delegation-owner protocol
                            (ext:delegation-owner-id :planner :triager-a))
    (is (null (ext:grant-set-has-p protocol :triager-a))
        "the drained delegation is gone")
    (is (ext:grant-set-has-p protocol :triager-b)
        "the sibling delegation survives")))

;;; --- C3: run-lifecycle drain -----------------------------------------------

(defun track-registered-run (agent registry &key (state :completed))
  "Register a run live and track it on AGENT, the way run-agent-loop does."
  (let ((run (agents::make-agent-run agent)))
    (setf (agents::agent-run-state run) state)
    (kli:register-live-object registry run)
    (sb-thread:with-mutex ((agents::agent-loop-runs-lock agent))
      (push run (agents:agent-loop-runs agent)))
    run))

(test agent-loop-runs-drain-on-deregister
  "Runs created over a session are tracked on the agent, so deregister-agent
removes every one from the live registry instead of leaking it."
  (let* ((context (agent-loop-test-context))
         (selection (agent-loop-register-model context "fake-provider"
                                               "fake-model"))
         (agent (make-agent-loop-session-agent context selection
                                               :id :drain-agent))
         (registry (kli:context-registry context))
         (runs (loop repeat 3 collect (track-registered-run agent registry))))
    (is (every (lambda (run)
                 (kli:find-live-object registry (kli:object-id run)))
               runs)
        "the runs are live before deregister")
    (agents:deregister-agent (agent-loop-service context) agent context)
    (is (notany (lambda (run)
                  (kli:find-live-object registry (kli:object-id run)))
                runs)
        "deregister drains every tracked run from the registry")
    (is (null (agents:agent-loop-runs agent))
        "no run remains tracked on the agent")))

(test agent-loop-in-flight-run-survives-deregister
  "A run still in flight is not yanked from under a live turn -- the drain skips
a :running run and keeps it tracked."
  (let* ((context (agent-loop-test-context))
         (selection (agent-loop-register-model context "fake-provider"
                                               "fake-model"))
         (agent (make-agent-loop-session-agent context selection
                                               :id :inflight-agent))
         (registry (kli:context-registry context))
         (live (track-registered-run agent registry :state :running)))
    (agents:deregister-agent (agent-loop-service context) agent context)
    (is (kli:find-live-object registry (kli:object-id live))
        "the in-flight run stays live in the registry")
    (is (member live (agents:agent-loop-runs agent))
        "the in-flight run stays tracked on the agent")))

;;; --- C4: :actor dead-seam carrier ------------------------------------------

(test (make-model-request-carries-the-actor :fixture interactive-authority)
  "make-model-request slots the actor into the request metadata instead of
declaring it ignored, so per-request authority has a carrier; with no actor the
key is simply absent."
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "fake-provider" "fake-model"
                                  :auth-required-p nil)
        (declare (ignore _provider _model))
        (let ((runtime (model-runtime-service context)))
          (let ((with-actor (rt:make-model-request runtime selection
                                                   sealed-context context
                                                   :id :req-with-actor
                                                   :actor :agent/seven)))
            (is (eq :agent/seven
                    (getf (rt::model-request-metadata with-actor) :actor))
                "the actor rides on the request metadata"))
          (let ((without (rt:make-model-request runtime selection
                                                sealed-context context
                                                :id :req-no-actor)))
            (is (null (getf (rt::model-request-metadata without) :actor))
                "no :actor key when none is supplied")))))))

;;; --- C5: contribution lifecycle check-then-mutate --------------------------

(test activate-extension-validates-before-mutating
  "A contribution whose precondition fails aborts activation before any mutation:
the extension never registers and no earlier contribution installs."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (good (ext:make-grant-contribution
                :principal :seed-principal
                :grant (ext:make-grant :capabilities '(:x))))
         (bad (ext:make-tool-contribution :name :broken :tool :not-a-tool))
         (extension (ext:make-extension :id :partial-ext
                                        :contributions (list good bad))))
    (ext:with-system-authority
      (signals error (ext:activate-extension protocol extension context)))
    (is (null (kli:find-live-object (kli:context-registry context) :partial-ext))
        "the extension is not registered when a precondition fails")
    (is (null (ext:grant-set-has-p protocol :seed-principal))
        "the earlier valid contribution left no partial state")))

(test activate-extension-installs-all-valid-contributions
  "When every precondition passes, activation installs all contributions."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (c1 (ext:make-grant-contribution
              :principal :p-one :grant (ext:make-grant :capabilities '(:x))))
         (c2 (ext:make-grant-contribution
              :principal :p-two :grant (ext:make-grant :capabilities '(:y))))
         (extension (ext:make-extension :id :all-good-ext
                                        :contributions (list c1 c2))))
    (ext:with-system-authority
      (ext:activate-extension protocol extension context))
    (is (kli:find-live-object (kli:context-registry context) :all-good-ext)
        "the extension registers")
    (is (and (ext:grant-set-has-p protocol :p-one)
             (ext:grant-set-has-p protocol :p-two))
        "every valid contribution installs")))
