(in-package #:kli/agent/session)

(defun make-agent-session-contract ()
  (make-provider-contract
   :id :agent/session/v1
   :capability :agent/session
   :required-entries
   '(:submit-prompt
     :steer-session
     :follow-up-session
     :switch-session
     :branch-session
     :reset-session
     :clear-session
     :session-context
     :register-listener
     :unregister-listener
     :set-model
     :inspect-agent-session
     :recode-event-persistence-policy
     :recode-prompt-expansion-policy
     :recode-context-transform-policy
     :recode-retry-policy
     :recode-compaction-policy)))

(defun make-agent-session-provider ()
  (make-provider
   :id :agent-session-provider
   :capability :agent/session
   :contracts '(:agent/session/v1)
   :entries
   (list :submit-prompt           #'submit-agent-session-prompt
         :steer-session           #'steer-agent-session
         :follow-up-session       #'follow-up-agent-session
         :switch-session          #'switch-agent-session
         :branch-session          #'branch-agent-session
         :reset-session           #'reset-agent-session
         :clear-session           #'clear-agent-session
         :session-context         #'agent-session-context
         :register-listener       #'register-session-event-listener
         :unregister-listener     #'unregister-session-event-listener
         :set-model               #'set-agent-session-model
         :set-option              #'set-agent-session-option
         :inspect-agent-session   #'inspect-agent-session
         :recode-event-persistence-policy #'recode-event-persistence-policy
         :recode-prompt-expansion-policy  #'recode-prompt-expansion-policy
         :recode-context-transform-policy #'recode-context-transform-policy
         :recode-retry-policy             #'recode-retry-policy
         :recode-compaction-policy        #'recode-compaction-policy)))

(defextension agent-session
  (:requires
   (capability events           :contract events/v1)
   (capability session/log      :contract session/log/v1)
   (capability session/entries  :contract session/entries/v1)
   (capability context/lens     :contract context/lens/v1)
   (capability agent/loop       :contract agent/loop/v1)
   (capability model/runtime    :contract model/runtime/v1)
   (capability model/registry   :contract model/registry/v1)
   (capability auth             :contract auth/v1))
  (:provides
   (contract agent/session/v1
     (make-agent-session-contract))
   (capability agent/session
     (make-agent-session-provider))
   (live-object agent-session
     (make-agent-session-service))
   (event-type :session-switch)
   (event-type :session-branch)
   (event-type :session-reset)
   (event-type :session-cleared)
   (event-type :session-focus)
   (event-type :listener-registered)
   (event-type :listener-unregistered)
   (event-type :model-change)
   (event-type :option-change)
   (event-type :session-compaction-needed)
   (event-type :session-compaction-started)
   (event-type :session-compaction-finished)
   (event-type :agent/retry)
   (event-handler record-model-visible-commands
     :event-type :command/result
     :handler (lambda (event context)
                (let ((service (find-live-object (context-registry context)
                                                 :agent-session-service)))
                  (when service
                    (record-pending-command service
                                            (kli/event:event-payload event))))))
   (method kli/event:dispatch-event
       (:after)
       (kli/ext:extension-protocol t t)
       (protocol event context)
     "Composes with the existing primary on dispatch-event so event-handler contributions continue to fire."
     (declare (ignore protocol))
     (let ((service (find-live-object (context-registry context)
                                      :agent-session-service)))
       (when service
         (persist-agent-event service event context))))))

;;;; Durable session identity: per mode, {mode-id, session-id, leaf-id}. Model
;;;; selection round-trips via the session log (switch recovers it); everything
;;;; else is rebuilt by service construction + switch. The default method would
;;;; slot-scrape all of it by accident.

(defmethod kli/runtime/snapshot:snapshot-representation ((s agent-session))
  (let ((modes '()))
    (maphash
     (lambda (mode-id binding)
       (let ((session-binding (mode-binding-session-binding binding)))
         (when session-binding
           (push (list :mode-id
                       (kli/runtime/snapshot:serialize-snapshot-value mode-id)
                       :session-id
                       (kli/runtime/snapshot:serialize-snapshot-value
                        (session-binding-session-id session-binding))
                       :leaf-id
                       (kli/runtime/snapshot:serialize-snapshot-value
                        (session-binding-leaf-id session-binding)))
                 modes))))
     (session-mode-bindings s))
    (list :modes (sort modes #'string<
                       :key (lambda (mode)
                              (princ-to-string (getf mode :mode-id))))
          :active-mode (kli/runtime/snapshot:serialize-snapshot-value
                        (session-active-mode-id s))
          ;; Not carried -- all reconstructed (construction, switch, TUI).
          :omitted '("event-listeners" "source->mode"
                     "event-persistence-policy" "prompt-expansion-policy"
                     "context-transform-policy" "retry-policy"
                     "compaction-policy" "pending-command-records"))))

(defun restore-session-mode (agent-session mode-id session-id leaf-id context)
  "Rebind MODE-ID onto SESSION-ID at the captured LEAF-ID via the canonical
switch, loading from disk if absent. A missing session file is skipped, not
fatal."
  (when (agent-session-busy-p agent-session mode-id context)
    (error 'agent-session-error :reason :busy :provider-id mode-id))
  (let* ((store (session-store-of context))
         (log   (session-log-provider-of context))
         (session (provider-call log :find-session store session-id)))
    (unless session
      (unless (typep store 'kli/session/log:file-session-store)
        (return-from restore-session-mode nil))
      (let ((path (kli/session/log:session-file-path store session-id)))
        (unless (probe-file path)
          (return-from restore-session-mode nil))
        (kli/session/log:load-session-file store path context)
        (setf session (provider-call log :find-session store session-id))))
    (when (and session
               leaf-id
               (not (eql leaf-id (kli/session/log:session-leaf-id session))))
      (provider-call log :repoint-session-leaf store session leaf-id context))
    (switch-agent-session agent-session mode-id session-id context)))

(defmethod kli/runtime/snapshot:restore-representation
    ((s agent-session) datum context)
  ;; Restore re-establishes the interactive session: run the gated switch under
  ;; the bounded UI subject, not full system authority. After every mode is
  ;; rebound, focus the captured active mode LAST so the registry's global
  ;; selection -- which seeds newly constructed agents -- follows the focused
  ;; mode, via the same focus op live focus uses. A missing active mode (its
  ;; session file was absent cross-image, so it never bound) is recorded as an
  ;; unfocused loss rather than focusing a phantom.
  (let ((kli/ext:*call-subject* kli/ext:*ui-subject*))
    (dolist (mode (getf datum :modes))
      (restore-session-mode
       s
       (kli/runtime/snapshot:deserialize-snapshot-value (getf mode :mode-id))
       (kli/runtime/snapshot:deserialize-snapshot-value (getf mode :session-id))
       (kli/runtime/snapshot:deserialize-snapshot-value (getf mode :leaf-id))
       context))
    ;; Clear the pointer first so focus-at-end always re-seeds the registry. On
    ;; an in-place restore the pointer can already equal the captured active
    ;; mode; focus's idempotent no-op would then skip the reseed, leaving the
    ;; registry global on whatever mode the loop above rebound last.
    (setf (session-active-mode-id s) nil
          (session-restore-unfocused-mode s) nil)
    (let ((active (kli/runtime/snapshot:deserialize-snapshot-value
                   (getf datum :active-mode))))
      (cond
        ((null active))
        ((gethash active (session-mode-bindings s))
         (focus-agent-session-mode s active context))
        ;; The captured focused mode never rebound -- record the loss rather than
        ;; focus a phantom.
        (t (setf (session-restore-unfocused-mode s) active))))))
