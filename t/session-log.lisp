(in-package #:kli/tests)

(defun session-log-provider (protocol)
  (ext:require-capability-provider protocol
                                   :session/log
                                   :contract :session/log/v1))

(defun session-log-store (context)
  (kli:find-live-object (kli:context-registry context)
                        :session-store))

(test session-log-registers-store-and-provider
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (is (typep (session-log-store context) 'sess:session-store))
    (is (ext:find-capability-provider protocol
                                      :session/log
                                      :contract :session/log/v1))))

(test session-log-appends-messages-and-builds-context
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (let* ((provider (session-log-provider protocol))
           (store (session-log-store context))
           (session (ext:provider-call provider
                                       :create-session
                                       store
                                       context
                                       :id :test-session))
           (user-entry (sess:make-message-entry
                        (sess:make-user-message "hello" :id :user-message)
                        :id :entry-user))
           (assistant-entry (sess:make-message-entry
                             (sess:make-assistant-message "hi"
                                                          :id :assistant-message)
                             :id :entry-assistant)))
      (is (eq session
              (kli:find-live-object (kli:context-registry context)
                                    :test-session)))
      (ext:provider-call provider
                         :append-session-entry
                         store
                         session
                         user-entry
                         context)
      (ext:provider-call provider
                         :append-session-entry
                         store
                         session
                         assistant-entry
                         context)
      (is (eq :entry-assistant (sess:session-leaf-id session)))
      (is (eq user-entry (sess:session-entry-by-id store
                                                   session
                                                   :entry-user)))
      (is (equal '(:entry-user :entry-assistant)
                 (mapcar #'kli:object-id
                         (sess:session-branch store session nil))))
      (let ((built (sess:build-session-context store session)))
        (is (equal '(:entry-user :entry-assistant)
                   (mapcar #'kli:object-id
                           (sess:session-context-entries built))))
        (is (equal '(:user :assistant)
                   (mapcar #'sess:message-role
                           (sess:session-context-messages built))))
        (is (equal '("hello" "hi")
                   (mapcar #'sess:message-content
                           (sess:session-context-messages built))))))))

(test session-log-supports-branches-from-earlier-entry
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (let* ((store (session-log-store context))
           (session (sess:create-session store context :id :branch-session))
           (root (sess:make-message-entry
                  (sess:make-user-message "root")
                  :id :root))
           (left (sess:make-message-entry
                  (sess:make-assistant-message "left")
                  :id :left))
           (right (sess:make-message-entry
                   (sess:make-assistant-message "right")
                   :id :right
                   :parent-id :root)))
      (sess:append-session-entry store session root context)
      (sess:append-session-entry store session left context)
      (sess:append-session-entry store session right context)
      (is (equal '(:root :left)
                 (mapcar #'kli:object-id
                         (sess:session-branch store session :left))))
      (is (equal '(:root :right)
                 (mapcar #'kli:object-id
                         (sess:session-branch store session :right))))
      (is (equal '("root" "right")
                 (mapcar #'sess:message-content
                         (sess:session-context-messages
                          (sess:build-session-context store
                                                      session
                                                      :leaf-id :right))))))))

(test session-log-branch-does-not-orphan-tool-calls
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (let* ((store (session-log-store context))
           (session (sess:create-session store context :id :tool-branch-session))
           (user (sess:make-message-entry
                  (sess:make-user-message "run a tool")
                  :id :tool-branch-user))
           (assistant (sess:make-message-entry
                       (sess:make-assistant-message
                        ""
                        :metadata
                        (list :tool-calls
                              (list (list :id "call_branch"
                                          :name "read"
                                          :arguments-json "{}"))))
                       :id :tool-branch-assistant))
           (result (sess:make-message-entry
                    (sess:make-tool-result-message
                     "tool output" :tool-call-id "call_branch")
                    :id :tool-branch-result)))
      (dolist (entry (list user assistant result))
        (sess:append-session-entry store session entry context))
      (let* ((full (sess:build-session-context store session))
             (full-wire
               (transports:convert-responses-input
                (mapcar #'rt::convert-agent-message
                        (sess:session-context-messages full)))))
        (is (string= "function_call"
                     (gethash "type" (aref full-wire 1))))
        (is (string= "function_call_output"
                     (gethash "type" (aref full-wire 2)))))
      (let* ((branch (sess:branch-session-at-entry
                      store session :tool-branch-assistant context))
             (branched (sess:build-session-context store branch))
             (wire (transports:convert-responses-input
                    (mapcar #'rt::convert-agent-message
                            (sess:session-context-messages branched)))))
        (is (eq :tool-branch-user (sess:session-leaf-id branch)))
        (is (equal '(:user)
                   (mapcar #'sess:message-role
                           (sess:session-context-messages branched))))
        (is (= 1 (length wire)))
        (is (string= "message" (gethash "type" (aref wire 0))))))))

(test session-log-distinguishes-custom-entries-and-custom-messages
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (let* ((store (session-log-store context))
           (session (sess:create-session store context :id :custom-session))
           (user (sess:make-message-entry
                  (sess:make-user-message "visible")
                  :id :visible))
           (custom-data (sess:make-custom-entry :ui-state
                                                :id :ui-state
                                                :data '(:scroll 10)))
           (custom-message (sess:make-custom-message-entry :note
                                                           "include me"
                                                           :id :note)))
      (dolist (entry (list user custom-data custom-message))
        (sess:append-session-entry store session entry context))
      (let ((built (sess:build-session-context store session)))
        (is (equal '(:visible :ui-state :note)
                   (mapcar #'kli:object-id
                           (sess:session-context-entries built))))
        (is (equal '("visible" "include me")
                   (mapcar #'sess:message-content
                           (sess:session-context-messages built)))))
      (let ((filtered (sess:build-session-context
                       store
                       session
                       :policy (lambda (entry)
                                 (not (typep entry
                                             'sess:custom-message-entry))))))
        (is (equal '("visible")
                   (mapcar #'sess:message-content
                           (sess:session-context-messages filtered))))))))

(test session-log-repoint-leaf-retracts-unanswered-turn
  "Repointing the leaf to a captured pre-turn tip drops later entries from model
   context while leaving them in the append-only table."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (let* ((provider (session-log-provider protocol))
           (store (session-log-store context))
           (session (ext:provider-call provider :create-session store context
                                       :id :retract-session)))
      (flet ((append-entry (message)
               (ext:provider-call provider :append-session-entry store session
                                  (sess:make-message-entry message) context))
             (contents ()
               (mapcar #'sess:message-content
                       (sess:session-context-messages
                        (ext:provider-call provider :build-session-context
                                           store session)))))
        (append-entry (sess:make-user-message "first"))
        (append-entry (sess:make-assistant-message "reply one"))
        (let ((pre-turn-leaf (sess:session-leaf-id session))
              (retracted (sess:make-message-entry
                          (sess:make-user-message "RETRACT-ME"))))
          (ext:provider-call provider :append-session-entry store session
                             retracted context)
          (is (equal '("first" "reply one" "RETRACT-ME") (contents)))
          (ext:provider-call provider :repoint-session-leaf store session
                             pre-turn-leaf context)
          (is (equal '("first" "reply one") (contents)))
          (is (eq pre-turn-leaf (sess:session-leaf-id session)))
          (is (eq retracted
                  (ext:provider-call provider :session-entry-by-id store session
                                     (kli:object-id retracted)))
              "the retracted entry survives, orphaned"))))))

(test session-log-harness-context-message-coerces-trust-fail-safe
  "Trust is honored only as an explicit known class; absent/unknown lowers to
:reference. Ephemeral folds into metadata; role is :harness-context."
  (is (eq :operator (sess:message-trust (sess:make-harness-context-message "x" :trust :operator))))
  (is (eq :reference (sess:message-trust (sess:make-harness-context-message "x" :trust :reference))))
  (is (eq :reference (sess:message-trust (sess:make-harness-context-message "x")))
      "absent trust lowers to :reference")
  (is (eq :reference (sess:message-trust (sess:make-harness-context-message "x" :trust :sysadmin)))
      "unknown trust lowers to :reference")
  (is (eq :harness-context (sess:message-role (sess:make-harness-context-message "x" :trust :operator))))
  (is (eq t (getf (sess:message-metadata (sess:make-harness-context-message "x" :ephemeral t)) :ephemeral)))
  (is (null (getf (sess:message-metadata (sess:make-harness-context-message "x")) :ephemeral))))

(test session-log-serializes-and-restores-harness-context-message
  "SERIALIZE-RECORD round-trips a harness-context-message through its tagged
record, so a durable resume block (cairn:resume's :ephemeral nil injection)
reloads as the same class with content, role, and trust class intact. Without
the method the durable commit errors \"Cannot serialize HARNESS-CONTEXT-MESSAGE\"."
  (dolist (trust '(:operator :reference))
    (let* ((message (sess:make-harness-context-message "durable resume block"
                                                       :trust trust))
           (record (sess:serialize-record message))
           (restored (sess:deserialize-value record)))
      (is (eq :harness-context-message (sess:record-type record)))
      (is (typep restored 'sess:harness-context-message))
      (is (eq (kli:object-id message) (kli:object-id restored)))
      (is (string= "durable resume block" (sess:message-content restored)))
      (is (eq :harness-context (sess:message-role restored)))
      (is (eq trust (sess:message-trust restored))
          "trust class round-trips so a reloaded session lowers to the same channel")))
  (let ((restored (sess:deserialize-value
                   (sess:serialize-record
                    (sess:make-harness-context-message "x" :ephemeral t)))))
    (is (eq t (getf (sess:message-metadata restored) :ephemeral))
        "the ephemeral flag survives the round-trip in metadata")))

(defun %all-subclasses (class)
  "Every transitive subclass of CLASS, derived from the live hierarchy."
  (let ((seen '()))
    (labels ((walk (c)
               (dolist (sub (sb-mop:class-direct-subclasses c))
                 (unless (member sub seen)
                   (push sub seen)
                   (walk sub)))))
      (walk class))
    seen))

(test session-log-serialize-record-total-over-agent-message-subclasses
  "Totality guard: every AGENT-MESSAGE subclass round-trips through its tagged
record. The subclass set is DERIVED from the live class hierarchy, so adding a
subclass without a SERIALIZE-RECORD/DESERIALIZE-RECORD pair (or without a sample
here) fails this test rather than silently dropping to the \"Cannot serialize\"
fallback when a durable record is written. The per-subclass serializer obligation
is thereby enforced, not merely remembered."
  (let* ((samples
           (list (sess:make-user-message "u")
                 (sess:make-assistant-message "a")
                 (sess:make-tool-result-message "r"
                                                :tool-call-id "call_1"
                                                :tool-name "read"
                                                :error-p t)
                 (sess:make-custom-agent-message :note "c")
                 (sess:make-harness-context-message "h" :trust :operator)))
         (covered (mapcar #'class-of samples))
         (concrete (%all-subclasses (find-class 'sess:agent-message))))
    (is (null (set-difference concrete covered))
        "every AGENT-MESSAGE subclass has a serializer round-trip sample")
    (is (null (set-difference covered concrete))
        "no stale sample for a removed AGENT-MESSAGE subclass")
    (dolist (message samples)
      (let* ((record (sess:serialize-record message))
             (restored (sess:deserialize-value record)))
        (is (eq (class-of message) (class-of restored))
            "~A round-trips to its own class" (class-name (class-of message)))
        (is (eq (kli:object-id message) (kli:object-id restored)))
        (is (eq (sess:message-role message) (sess:message-role restored)))
        (is (equal (sess:message-content message)
                   (sess:message-content restored)))
        (typecase restored
          (sess:tool-result-message
           (is (string= "call_1" (sess:tool-call-id restored)))
           (is (string= "read" (sess:tool-name restored)))
           (is (eq t (sess:tool-error-p restored))))
          (sess:custom-agent-message
           (is (eq :note (sess:custom-message-type restored))))
          (sess:harness-context-message
           (is (eq :operator (sess:message-trust restored)))))))))
