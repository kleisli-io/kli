(in-package #:kli/tests)

(in-suite all)

(defun session-entries-provider (protocol)
  (ext:require-capability-provider protocol
                                   :session/entries
                                   :contract :session/entries/v1))

(test session-entries-exposes-compaction-branch-and-custom
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (let* ((provider (session-entries-provider protocol))
           (comp (ext:provider-call provider :make-compaction-entry
                                    "summary" :first-kept :id :c1))
           (bs (ext:provider-call provider :make-branch-summary-entry
                                  :from "branch summary" :id :b1))
           (cm (ext:provider-call provider :make-custom-agent-message
                                  :note "hello"))
           (custom (ext:provider-call provider :make-custom-entry :ctype)))
      (is (typep comp 'sess:compaction-entry))
      (is (string= "summary" (sess:entry-summary comp)))
      (is (eq :first-kept (sess:entry-first-kept-entry-id comp)))
      (is (typep bs 'sess:branch-summary-entry))
      (is (eq :from (sess:entry-from-id bs)))
      (is (string= "branch summary" (sess:entry-summary bs)))
      (is (eq :custom (sess:message-role cm)))
      (is (string= "hello" (sess:message-content cm)))
      (is-true (ext:provider-call provider :custom-entry-p custom))
      (is-false (ext:provider-call provider :custom-entry-p comp)))))

(defun compaction-test-log (context protocol)
  (values (ext:require-capability-provider protocol :session/log
                                           :contract :session/log/v1)
          (kli:find-live-object (kli:context-registry context)
                                :session-store)))

(test build-session-context-replaces-summarized-prefix
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (multiple-value-bind (provider store) (compaction-test-log context protocol)
      (let ((session (ext:provider-call provider :create-session store context
                                        :id :compact-session)))
        (flet ((append! (entry)
                 (ext:provider-call provider :append-session-entry
                                    store session entry context)))
          (append! (sess:make-message-entry
                    (sess:make-user-message "old1") :id :e-old1))
          (append! (sess:make-message-entry
                    (sess:make-assistant-message "old2") :id :e-old2))
          (append! (sess:make-message-entry
                    (sess:make-user-message "keep1") :id :e-keep1))
          (append! (sess:make-message-entry
                    (sess:make-assistant-message "keep2") :id :e-keep2))
          (append! (sess:make-compaction-entry "SUM" :e-keep1 :id :e-comp)))
        (let* ((built (ext:provider-call provider :build-session-context
                                         store session))
               (messages (sess:session-context-messages built)))
          (is (= 3 (length messages)))
          (is (eq :user (sess:message-role (first messages))))
          (is (string= "SUM" (sess:message-content (first messages))))
          (is (string= "keep1" (sess:message-content (second messages))))
          (is (string= "keep2" (sess:message-content (third messages)))))))))

(defun sized-message-entry (role chars id)
  (let ((content (make-string chars :initial-element #\x)))
    (sess:make-message-entry
     (ecase role
       (:user (sess:make-user-message content))
       (:assistant (sess:make-assistant-message content))
       (:tool (sess:make-tool-result-message content)))
     :id id)))

(test cut-point-keeps-recent-user-turn
  (let* ((entries (list (sized-message-entry :user 400 :e1)
                        (sized-message-entry :assistant 400 :e2)
                        (sized-message-entry :user 40 :e3)
                        (sized-message-entry :assistant 40 :e4)))
         (prep (sess:prepare-session-compaction entries 15)))
    (is (eq :e3 (sess:compaction-preparation-first-kept-id prep)))
    (is-false (sess:compaction-preparation-split-turn-p prep))
    (is (= 2 (length (sess:compaction-preparation-messages-to-summarize prep))))))

(test cut-point-never-lands-on-tool-result
  (let* ((entries (list (sized-message-entry :user 40 :t1)
                        (sized-message-entry :assistant 40 :t2)
                        (sized-message-entry :tool 40 :t3)
                        (sized-message-entry :user 40 :t4)
                        (sized-message-entry :assistant 40 :t5)))
         (prep (sess:prepare-session-compaction entries 25)))
    (is (eq :t4 (sess:compaction-preparation-first-kept-id prep)))
    (is-false (sess:compaction-preparation-split-turn-p prep))))

(test cut-point-splits-mid-turn
  (let* ((entries (list (sized-message-entry :user 40 :s1)
                        (sized-message-entry :assistant 400 :s2)
                        (sized-message-entry :assistant 40 :s3)))
         (prep (sess:prepare-session-compaction entries 15)))
    (is (eq :s2 (sess:compaction-preparation-first-kept-id prep)))
    (is-true (sess:compaction-preparation-split-turn-p prep))
    (is (= 1 (length (sess:compaction-preparation-turn-prefix-messages prep))))
    (is (= 0 (length (sess:compaction-preparation-messages-to-summarize prep))))))

(test cut-point-nil-when-leaf-is-compaction
  (let ((entries (list (sized-message-entry :user 40 :z1)
                       (sess:make-compaction-entry "s" :z1 :id :z2))))
    (is-false (sess:prepare-session-compaction entries 15))))

(test cut-point-snaps-backward-when-trailing-results-exceed-keep-recent
  "Trailing tool results that alone exceed keep-recent leave no cut point at or
after the boundary; the backward snap lands on the assistant owning them, so
older history still gets summarized instead of nothing."
  (let* ((entries (list (sized-message-entry :user 40 :bf-u1)
                        (sized-message-entry :assistant 40 :bf-a0)
                        (sized-message-entry :user 40 :bf-u2)
                        (sized-message-entry :assistant 40 :bf-a1)
                        (sized-message-entry :tool 400 :bf-r1)))
         (prep (sess:prepare-session-compaction entries 50)))
    (is (eq :bf-a1 (sess:compaction-preparation-first-kept-id prep)))
    (is (= 2 (length (sess:compaction-preparation-messages-to-summarize prep))))
    (is-true (sess:compaction-preparation-split-turn-p prep))
    (is (= 1 (length (sess:compaction-preparation-turn-prefix-messages prep))))))

(test prepare-compaction-nil-when-no-reducible-history
  "When the backward snap lands at the start with nothing older and no split
prefix, there is nothing to reduce; the guard returns nil rather than summarize
an empty set over the whole history."
  (let ((entries (list (sized-message-entry :assistant 40 :gd-a1)
                       (sized-message-entry :tool 400 :gd-r1))))
    (is-false (sess:prepare-session-compaction entries 5))))

(test prepare-compaction-counts-tool-call-arguments
  "Assistant tool-call arguments are provider-visible context. A large write
argument must therefore make the turn eligible for compaction even when the
assistant message text is empty."
  (let* ((large-content (make-string 240000 :initial-element #\x))
         (arguments-json (format nil "{\"path\":\"huge.txt\",\"content\":\"~A\"}"
                                 large-content))
         (entries
           (list
            (sess:make-message-entry
             (sess:make-user-message "write the generated artifact")
             :id :large-arg-user)
            (sess:make-message-entry
             (sess:make-assistant-message
              ""
              :metadata (list :tool-calls
                              (list (list :id "call_large_write"
                                          :name "write"
                                          :arguments-json arguments-json))))
             :id :large-arg-assistant)
            (sess:make-message-entry
             (sess:make-tool-result-message
              "Wrote 240000 characters to huge.txt."
              :tool-call-id "call_large_write"
              :tool-name "write")
             :id :large-arg-result)))
         (prep (sess:prepare-session-compaction entries 20000)))
    (is (not (null prep))
        "provider-visible tool-call arguments must count toward compaction")
    (when prep
      (is (eq :large-arg-assistant
              (sess:compaction-preparation-first-kept-id prep)))
      (is (= 1
             (length
              (sess:compaction-preparation-turn-prefix-messages prep)))))))

(defun compaction-bound-agent (service context &optional (mode-id :default-mode))
  (let* ((binding (gethash mode-id
                           (agent-session:session-mode-bindings service)))
         (agent-id (and binding (agent-session:mode-binding-agent-id binding))))
    (and agent-id (kli:find-live-object (kli:context-registry context)
                                        agent-id))))

(defun provider-accounting-for-test (service context)
  (let ((agent (compaction-bound-agent service context)))
    (agent-session::rebuild-context agent context)
    (agent-session:account-provider-replay
     (agents:agent-context agent)
     (agents:agent-model-selection agent)
     context)))

(test (provider-accounting-reports-upper-bound-and-item-attribution
       :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (service session) (bind-agent-session-mode context)
      (let ((store (session-log-store context)))
        (flet ((append! (entry)
                 (sess:append-session-entry store session entry context)))
          (append! (sess:make-message-entry
                    (sess:make-user-message "write a file" :id :pa-user-msg)
                    :id :pa-user-entry))
          (append! (sess:make-message-entry
                    (sess:make-assistant-message
                     ""
                     :id :pa-assistant-msg
                     :metadata
                     (list :tool-calls
                           (list (list :id "call-pa"
                                       :name "write"
                                       :arguments-json "{\"path\":\"a\"}"))))
                    :id :pa-assistant-entry))
          (append! (sess:make-message-entry
                    (sess:make-tool-result-message
                     "ok"
                     :id :pa-result-msg
                     :tool-call-id "call-pa"
                     :tool-name "write")
                    :id :pa-result-entry)))
        (let* ((accounting (provider-accounting-for-test service context))
               (attrs (agent-session:provider-accounting-result-attributions
                       accounting)))
          (is (eq :tokens
                  (agent-session:provider-accounting-result-units accounting)))
          (is-false
           (agent-session:provider-accounting-result-exact-p accounting))
          (is-true
           (agent-session:provider-accounting-result-upper-bound-p accounting))
          (is (< 0 (agent-session:provider-accounting-result-total
                    accounting)))
          (is (some (lambda (attr)
                      (member '(:message :pa-assistant-msg)
                              (getf attr :item-ids)
                              :test #'equal))
                    attrs))
          (is (some (lambda (attr)
                      (member :pa-assistant-entry
                              (getf attr :entry-ids)
                              :test #'equal))
                    attrs)))))))

(test (provider-accounted-cut-planning-counts-wire-tool-arguments
       :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (service session) (bind-agent-session-mode context)
      (let* ((store (session-log-store context))
             (large-content (make-string 240000 :initial-element #\x))
             (arguments-json
               (format nil "{\"path\":\"huge.txt\",\"content\":\"~A\"}"
                       large-content)))
        (flet ((append! (entry)
                 (sess:append-session-entry store session entry context)))
          (append! (sess:make-message-entry
                    (sess:make-user-message "write the artifact")
                    :id :pac-user))
          (append! (sess:make-message-entry
                    (sess:make-assistant-message
                     ""
                     :metadata
                     (list :tool-calls
                           (list (list :id "call-provider-large"
                                       :name "write"
                                       :arguments-json arguments-json))))
                    :id :pac-assistant))
          (append! (sess:make-message-entry
                    (sess:make-tool-result-message
                     "wrote huge.txt"
                     :tool-call-id "call-provider-large"
                     :tool-name "write")
                    :id :pac-result)))
        (let* ((accounting (provider-accounting-for-test service context))
               (entry-token-fn
                 (agent-session::provider-accounting-entry-token-fn
                  accounting))
               (entries (sess:session-branch store session
                                             (sess:session-leaf-id session)))
               (prep (sess:prepare-session-compaction
                      entries 20000
                      :entry-token-fn entry-token-fn)))
          (is (< 20000
                 (agent-session:provider-accounting-result-total accounting)))
          (is (not (null prep)))
          (is (eq :pac-assistant
                  (sess:compaction-preparation-first-kept-id prep))))))))

(test (provider-accounting-materializes-repairs-as-provider-results
       :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (service session) (bind-agent-session-mode context)
      (let ((store (session-log-store context)))
        (flet ((append! (entry)
                 (sess:append-session-entry store session entry context)))
          (append! (sess:make-message-entry
                    (sess:make-user-message "call the tool")
                    :id :repair-user))
          (append! (sess:make-message-entry
                    (sess:make-assistant-message
                     ""
                     :metadata
                     (list :tool-calls
                           (list (list :id "call-missing"
                                       :name "read"
                                       :arguments-json "{\"path\":\"a\"}"))))
                    :id :repair-assistant)))
        (let* ((accounting (provider-accounting-for-test service context))
               (attrs (agent-session:provider-accounting-result-attributions
                       accounting)))
          (is (some (lambda (attr)
                      (some (lambda (item-id)
                              (and (consp item-id)
                                   (eq (first item-id) :repair)))
                            (getf attr :item-ids)))
                    attrs))
          (is (< 0 (agent-session:provider-accounting-result-total
                    accounting))))))))

(defun responses-wire-item-types (items)
  (loop for item across items collect (gethash "type" item)))

(test compacted-provider-context-drops-completed-tool-turns
  "Compaction must not leave completed tool turns in provider replay. Counting
or truncating tool-call arguments is not enough: once a file mutation succeeded,
the compacted provider context should carry the summary, not the original
function_call/function_call_output pair."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (content (make-string 240000 :initial-element #\x))
         (arguments-json (format nil "{\"path\":\"huge.txt\",\"content\":\"~A\"}"
                                 content)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (multiple-value-bind (provider store) (compaction-test-log context protocol)
      (let ((session (ext:provider-call provider :create-session store context
                                        :id :compacted-tool-turn-session)))
        (flet ((append! (entry)
                 (ext:provider-call provider :append-session-entry
                                    store session entry context)))
          (append! (sess:make-message-entry
                    (sess:make-user-message "write the generated artifact")
                    :id :ct-user))
          (append! (sess:make-message-entry
                    (sess:make-assistant-message
                     ""
                     :metadata (list :tool-calls
                                     (list (list :id "call_large_write"
                                                 :name "write"
                                                 :arguments-json arguments-json))))
                    :id :ct-assistant))
          (append! (sess:make-message-entry
                    (sess:make-tool-result-message
                     "Wrote huge.txt."
                     :tool-call-id "call_large_write"
                     :tool-name "write"
                     :metadata (list :details
                                     (list :path "huge.txt"
                                           :added 1
                                           :removed 0
                                           :changed-ranges nil
                                           :new-sha256 "abc")))
                    :id :ct-result))
          (append! (sess:make-compaction-entry
                    "The artifact was written to huge.txt."
                    :ct-assistant
                    :id :ct-compaction)))
        (let* ((messages (sess:session-context-messages
                          (ext:provider-call provider :build-session-context
                                             store session)))
               (items (transports:convert-responses-input
                       (rt::convert-messages messages)))
               (types (responses-wire-item-types items))
               (wire (com.inuoe.jzon:stringify items)))
          (is (equal '("message") types)
              "compacted provider context must contain only summary/message items")
          (is (not (search "function_call" wire))
              "completed function calls must not survive compaction replay")
          (is (not (search content wire))
              "compaction replay must not contain the original write body"))))))

(defun runtime-request-count (runtime)
  (hash-table-count (rt:runtime-requests runtime)))

(test (compaction-executor-appends-summary-and-shortens-context :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :metadata (list :fake-deltas '("ok")))
    (let* ((service (agent-session-service context))
           (store (session-log-store context))
           (session (sess:find-session store :agent-session-test-session)))
      (flet ((append! (entry)
               (sess:append-session-entry store session entry context)))
        (append! (sized-message-entry :user 400 :x-u1))
        (append! (sized-message-entry :assistant 400 :x-a1))
        (append! (sized-message-entry :user 40 :x-u2))
        (append! (sized-message-entry :assistant 40 :x-a2)))
      (agent-session:recode-compaction-policy
       service :keep-recent-tokens 15
       :summarizer (lambda (&key messages &allow-other-keys)
                     (declare (ignore messages))
                     (values "SUMMARY OF OLD" (list :read-files (list "a.txt")))))
      (agent-session:execute-session-compaction
       service
       (event:make-event :session-compaction-needed
                         :payload (list :mode :default-mode))
       context)
      (let* ((leaf (sess:session-leaf-entry store session))
             (built (sess:build-session-context store session))
             (messages (sess:session-context-messages built)))
        (is (typep leaf 'sess:compaction-entry))
        (is (string= "SUMMARY OF OLD" (sess:entry-summary leaf)))
        (is (eq :x-u2 (sess:entry-first-kept-entry-id leaf)))
        (is (equal '(:read-files ("a.txt")) (sess:entry-data leaf)))
        (is (= 3 (length messages)))
        (is (eq :user (sess:message-role (first messages))))
        (is (string= "SUMMARY OF OLD"
                     (sess:message-content (first messages))))))))

(test (compaction-executor-retries-when-summary-exceeds-reserve
       :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :metadata (list :fake-deltas '("ok")))
    (let* ((service (agent-session-service context))
           (store (session-log-store context))
           (session (sess:find-session store :agent-session-test-session))
           (calls 0))
      (flet ((append! (entry)
               (sess:append-session-entry store session entry context)))
        (append! (sized-message-entry :user 400 :rs-u1))
        (append! (sized-message-entry :assistant 400 :rs-a1))
        (append! (sized-message-entry :user 400 :rs-u2))
        (append! (sized-message-entry :assistant 400 :rs-a2))
        (append! (sized-message-entry :user 40 :rs-u3))
        (append! (sized-message-entry :assistant 40 :rs-a3)))
      (agent-session:recode-compaction-policy
       service
       :keep-recent-tokens 150
       :summary-reserve-tokens 5
       :summarizer (lambda (&key messages turn-prefix-messages
                              &allow-other-keys)
                     (declare (ignore messages turn-prefix-messages))
                     (incf calls)
                     (values (make-string 400 :initial-element #\s) nil)))
      (agent-session:execute-session-compaction
       service
       (event:make-event :session-compaction-needed
                         :payload (list :mode :default-mode))
       context)
      (let ((leaf (sess:session-leaf-entry store session)))
        (is (= 2 calls))
        (is (typep leaf 'sess:compaction-entry))
        (is (eq :rs-a2 (sess:entry-first-kept-entry-id leaf)))))))

(test (compaction-executor-default-summarizer-appends-isolated-summary :fixture interactive-authority)
  "The default summarizer runs an isolated completion, so compaction appends one
compaction entry carrying the summary text while registering no loop request and
leaking no assistant turn into the session."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context
                             :metadata (list :fake-deltas '("STRUCTURED SUMMARY")))
    (let* ((service (agent-session-service context))
           (store (session-log-store context))
           (session (sess:find-session store :agent-session-test-session))
           (runtime (kli:find-live-object (kli:context-registry context)
                                          :model-runtime-service)))
      (flet ((append! (entry)
               (sess:append-session-entry store session entry context)))
        (append! (sized-message-entry :user 400 :d-u1))
        (append! (sized-message-entry :assistant 400 :d-a1))
        (append! (sized-message-entry :user 40 :d-u2)))
      (agent-session:recode-compaction-policy service :keep-recent-tokens 5)
      (let ((requests-before (runtime-request-count runtime))
            (entries-before (length (sess:session-branch
                                     store session
                                     (sess:session-leaf-id session)))))
        (agent-session:execute-session-compaction
         service
         (event:make-event :session-compaction-needed
                           :payload (list :mode :default-mode))
         context)
        (let* ((leaf (sess:session-leaf-entry store session))
               (branch (sess:session-branch store session
                                            (sess:session-leaf-id session)))
               (messages (sess:session-context-messages
                          (sess:build-session-context store session))))
          (is (typep leaf 'sess:compaction-entry))
          (is (string= "STRUCTURED SUMMARY" (sess:entry-summary leaf)))
          (is (eq :d-u2 (sess:entry-first-kept-entry-id leaf)))
          (is (= requests-before (runtime-request-count runtime)))
          (is (= (1+ entries-before) (length branch)))
          (is (= 2 (length messages)))
          (is (string= "STRUCTURED SUMMARY"
                       (sess:message-content (first messages)))))))))

(defun watch-compaction-events (service context)
  "Register a listener collecting (type . payload) for compaction lifecycle
events. Returns a thunk yielding them oldest-first."
  (let ((seen '()))
    (agent-session:register-session-event-listener
     service
     (agent-session:make-session-event-listener
      :compaction-event-watcher
      (lambda (event mode-id ctx)
        (declare (ignore mode-id ctx))
        (let ((type (event:event-type event)))
          (when (member type '(:session-compaction-started
                               :session-compaction-finished))
            (push (cons type (event:event-payload event)) seen)))))
     context)
    (lambda () (reverse seen))))

(defun compaction-test-session (context &key (prefix "c"))
  "Seed the bound test session with a compactable history. Returns
(values service store session)."
  (let* ((service (agent-session-service context))
         (store (session-log-store context))
         (session (sess:find-session store :agent-session-test-session)))
    (flet ((id (suffix)
             (intern (format nil "~:@(~A-~A~)" prefix suffix) :keyword)))
      (sess:append-session-entry
       store session (sized-message-entry :user 400 (id "u1")) context)
      (sess:append-session-entry
       store session (sized-message-entry :assistant 400 (id "a1")) context)
      (sess:append-session-entry
       store session (sized-message-entry :user 40 (id "u2")) context))
    (values service store session)))

(test (compaction-failure-distinct-from-nothing-to-compact :fixture interactive-authority)
  "A summarizer fault returns :failed with the condition and reports through
the finished event -- previously it collapsed to NIL, indistinguishable from
an up-to-date session. The fault policy binds NIL to exercise the production
barrier instead of the suite-wide :escalate."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (multiple-value-bind (service store session)
        (compaction-test-session context :prefix "cf")
      (let ((events (watch-compaction-events service context)))
        (agent-session:recode-compaction-policy
         service :keep-recent-tokens 5
         :summarizer (lambda (&rest args)
                       (declare (ignore args))
                       (error "summarizer exploded")))
        (let ((ext:*extension-fault-policy* nil))
          (multiple-value-bind (entry status failure)
              (agent-session:compact-agent-session service :default-mode context)
            (is (null entry))
            (is (eq :failed status))
            (is (typep failure 'error))))
        (is (not (typep (sess:session-leaf-entry store session)
                        'sess:compaction-entry))
            "a failed compaction must not append an entry")
        (let* ((seen (funcall events))
               (finished (find :session-compaction-finished seen :key #'car)))
          (is (= 2 (length seen)))
          (is (eq :session-compaction-started (car (first seen))))
          (is (eq :failed (getf (cdr finished) :status)))
          (is (search "summarizer exploded" (getf (cdr finished) :error))))))))

(test (compaction-abort-discards-summary-and-reports-aborted :fixture interactive-authority)
  "An abort during the summarizer -- Esc or quit -- must discard the result:
an aborted stream still returns its partial text, which must never be
committed as the summary. Status and the finished event say :aborted,
distinguishable from both :failed and :compacted."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (multiple-value-bind (service store session)
        (compaction-test-session context :prefix "ca")
      (let ((events (watch-compaction-events service context)))
        (agent-session:recode-compaction-policy
         service :keep-recent-tokens 5
         :summarizer (lambda (&rest args)
                       (declare (ignore args))
                       (agent-session:abort-agent-session
                        service :default-mode context)
                       "PARTIAL SUMMARY"))
        (multiple-value-bind (entry status)
            (agent-session:compact-agent-session service :default-mode context)
          (is (null entry))
          (is (eq :aborted status)))
        (is (not (typep (sess:session-leaf-entry store session)
                        'sess:compaction-entry))
            "an aborted compaction must not append the partial summary")
        (let* ((seen (funcall events))
               (finished (find :session-compaction-finished seen :key #'car)))
          (is (eq :aborted (getf (cdr finished) :status))))
        (is (null (agent-session:mode-binding-compaction-interrupt
                   (gethash :default-mode
                            (agent-session:session-mode-bindings service))))
            "the interrupt thunk is cleared once the compaction unwinds")))))

(test (compaction-abort-shuts-down-the-tracked-summarizer-request :fixture interactive-authority)
  "abort-agent-session interrupts the summarizer's in-flight model request
through the binding's interrupt thunk -- abort-agent alone only reaches the
current turn's request, and a compaction has none. The request is what a
worker sits parked on, so shutting it down is what lets Esc and quit unblock
an in-flight compaction."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (multiple-value-bind (service store session)
        (compaction-test-session context :prefix "cr")
      (declare (ignore store session))
      (let ((captured nil))
        (agent-session:recode-compaction-policy
         service :keep-recent-tokens 5
         :summarizer (lambda (&key on-request lens-provider runtime-provider
                                   runtime selection agent-context context
                              &allow-other-keys)
                       (let* ((message (sess:make-user-message "probe"))
                              (sealed (ext:provider-call
                                       lens-provider :seal-range-projection
                                       agent-context (list message) context))
                              (request (ext:provider-call
                                        runtime-provider :make-model-request
                                        runtime selection sealed context)))
                         (setf captured request)
                         (funcall on-request request)
                         (agent-session:abort-agent-session
                          service :default-mode context)
                         "PARTIAL")))
        (multiple-value-bind (entry status)
            (agent-session:compact-agent-session service :default-mode context)
          (is (null entry))
          (is (eq :aborted status)))
        (is (eq :aborted (rt:model-request-state captured))
            "the interrupt aborted the tracked request")))))

(test (compaction-nil-summary-reports-failed :fixture interactive-authority)
  "The default summarizer returns NIL when the model seam is unavailable --
that is a failure to report, not \"nothing to compact\"."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (multiple-value-bind (service store session)
        (compaction-test-session context :prefix "cn")
      (declare (ignore store session))
      (let ((events (watch-compaction-events service context)))
        (agent-session:recode-compaction-policy
         service :keep-recent-tokens 5
         :summarizer (lambda (&rest args) (declare (ignore args)) nil))
        (multiple-value-bind (entry status failure)
            (agent-session:compact-agent-session service :default-mode context)
          (is (null entry))
          (is (eq :failed status))
          (is (null failure)))
        (let ((finished (find :session-compaction-finished (funcall events)
                              :key #'car)))
          (is (eq :failed (getf (cdr finished) :status)))
          (is (search "no summary" (getf (cdr finished) :error))))))))

(test (compaction-success-emits-started-and-finished :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (multiple-value-bind (service store session)
        (compaction-test-session context :prefix "ce")
      (declare (ignore store session))
      (let ((events (watch-compaction-events service context)))
        (agent-session:recode-compaction-policy
         service :keep-recent-tokens 5
         :summarizer (lambda (&rest args) (declare (ignore args)) "SUM"))
        (multiple-value-bind (entry status)
            (agent-session:compact-agent-session service :default-mode context)
          (is (typep entry 'sess:compaction-entry))
          (is (eq :compacted status)))
        (let ((seen (funcall events)))
          (is (equal '(:session-compaction-started :session-compaction-finished)
                     (mapcar #'car seen)))
          (is (eq :compacted (getf (cdr (second seen)) :status)))
          (is (eq :manual (getf (cdr (second seen)) :trigger))))))))

(test (compaction-nothing-to-compact-emits-finished-only :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (events (watch-compaction-events service context)))
      (multiple-value-bind (entry status)
          (agent-session:compact-agent-session service :default-mode context)
        (is (null entry))
        (is (eq :nothing-to-compact status)))
      (let ((seen (funcall events)))
        (is (= 1 (length seen)))
        (is (eq :session-compaction-finished (car (first seen))))
        (is (eq :nothing-to-compact (getf (cdr (first seen)) :status)))
        (is (eq :manual (getf (cdr (first seen)) :trigger)))))))

(test (compaction-holds-agent-busy-and-restores-idle :fixture interactive-authority)
  "The agent sits in :compacting -- an active state -- while the summarizer
runs, so the busy indicator stays live and a concurrent submit steers into the
queue instead of spawning a racing turn."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (multiple-value-bind (service store session)
        (compaction-test-session context :prefix "cb")
      (declare (ignore store session))
      (let* ((binding (gethash :default-mode
                               (agent-session:session-mode-bindings service)))
             (agent (kli:find-live-object
                     (kli:context-registry context)
                     (agent-session:mode-binding-agent-id binding)))
             (busy-inside nil)
             (state-inside nil))
        (agent-session:recode-compaction-policy
         service :keep-recent-tokens 5
         :summarizer (lambda (&rest args)
                       (declare (ignore args))
                       (setf busy-inside (agent-session:agent-session-busy-p
                                          service :default-mode context)
                             state-inside (agents:agent-state-value
                                           (agents:agent-state agent)))
                       "SUM"))
        (agent-session:compact-agent-session service :default-mode context)
        (is (eq t busy-inside))
        (is (eq :compacting state-inside))
        (is (agents:agent-idle-p agent))
        (is (not (agent-session:agent-session-busy-p service :default-mode
                                                     context)))))))

(test (compaction-drains-steering-queued-during-compaction :fixture interactive-authority)
  "A submit landing mid-compaction steers into the queue (the agent is not
idle) and runs as a full turn once compaction finishes, instead of racing the
context rebuild or stranding in the queue."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :deltas '("queued reply"))
    (multiple-value-bind (service store session)
        (compaction-test-session context :prefix "cd")
      (declare (ignore store session))
      (let* ((binding (gethash :default-mode
                               (agent-session:session-mode-bindings service)))
             (agent (kli:find-live-object
                     (kli:context-registry context)
                     (agent-session:mode-binding-agent-id binding))))
        (agent-session:recode-compaction-policy
         service :keep-recent-tokens 5
         :summarizer (lambda (&rest args)
                       (declare (ignore args))
                       (agent-session:steer-agent-session
                        service :default-mode "queued during compaction" context)
                       "SUM"))
        (multiple-value-bind (entry status)
            (agent-session:compact-agent-session service :default-mode context)
          (is (typep entry 'sess:compaction-entry))
          (is (eq :compacted status)))
        (is (agents:agent-idle-p agent))
        (let ((contents (agent-session-message-contents agent)))
          (is (find "queued during compaction" contents :test #'string=)
              "the queued steer reached the session")
          (is (find "queued reply" contents :test #'string=)
              "the drained steer ran a model turn"))))))

(test build-session-context-without-compaction-keeps-all
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context sess:*session-log-extension-manifest*)
    (multiple-value-bind (provider store) (compaction-test-log context protocol)
      (let ((session (ext:provider-call provider :create-session store context
                                        :id :plain-session)))
        (flet ((append! (entry)
                 (ext:provider-call provider :append-session-entry
                                    store session entry context)))
          (append! (sess:make-message-entry
                    (sess:make-user-message "a") :id :p-a))
          (append! (sess:make-message-entry
                    (sess:make-assistant-message "b") :id :p-b)))
        (let* ((built (ext:provider-call provider :build-session-context
                                         store session))
               (messages (sess:session-context-messages built)))
          (is (= 2 (length messages)))
          (is (string= "a" (sess:message-content (first messages))))
          (is (string= "b" (sess:message-content (second messages)))))))))

(test produce-branch-summary-appends-entry-at-target-leaf
  (let* ((store (sess:make-session-store :id :bs-store-1))
         (session (sess:make-session :id :bs-session-1)))
    (sess:store-session store session nil)
    (flet ((append! (entry)
             (sess:append-session-entry store session entry nil)))
      (append! (sess:make-message-entry (sess:make-user-message "r1") :id :bs1-r1))
      (append! (sess:make-message-entry (sess:make-assistant-message "r2") :id :bs1-r2))
      (append! (sess:make-message-entry (sess:make-user-message "old") :id :bs1-old))
      (setf (sess:session-leaf-id session) :bs1-r2)
      (append! (sess:make-message-entry (sess:make-assistant-message "target")
                                        :id :bs1-target)))
    (let ((entry (sess:produce-branch-summary
                  store session :bs1-old :bs1-target
                  :summarizer (lambda (&key messages &allow-other-keys)
                                (declare (ignore messages))
                                "BRANCH SUMMARY"))))
      (is (typep entry 'sess:branch-summary-entry))
      (is (eq :bs1-r2 (sess:entry-from-id entry)))
      (is (string= "BRANCH SUMMARY" (sess:entry-summary entry)))
      (is (eq :bs1-target (sess:entry-parent-id entry)))
      (is (eq (kli:object-id entry) (sess:session-leaf-id session))))))

(test produce-branch-summary-accumulates-tracked-files
  "A prior summary on the old branch seeds cumulative file tracking. The
produced entry, file lists and all, survives a serialize round-trip."
  (let* ((store (sess:make-session-store :id :bs-store-2))
         (session (sess:make-session :id :bs-session-2)))
    (sess:store-session store session nil)
    (flet ((append! (entry)
             (sess:append-session-entry store session entry nil)))
      (append! (sess:make-message-entry (sess:make-user-message "r1") :id :bs2-r1))
      (append! (sess:make-branch-summary-entry
                :root "prior" :id :bs2-prior
                :data (list :read-files (list "old.txt"))))
      (append! (sess:make-message-entry (sess:make-user-message "old") :id :bs2-old))
      (setf (sess:session-leaf-id session) :bs2-r1)
      (append! (sess:make-message-entry (sess:make-assistant-message "target")
                                        :id :bs2-target)))
    (let ((entry (sess:produce-branch-summary
                  store session :bs2-old :bs2-target
                  :summarizer (lambda (&key messages &allow-other-keys)
                                (declare (ignore messages))
                                (values "BRANCH SUMMARY"
                                        (list :read-files (list "new.txt")
                                              :modified-files (list "mod.txt")))))))
      (is (equal '("old.txt" "new.txt")
                 (getf (sess:entry-data entry) :read-files)))
      (is (equal '("mod.txt")
                 (getf (sess:entry-data entry) :modified-files)))
      (let* ((form (sess:serialize-record entry))
             (restored (sess:deserialize-value
                        (with-standard-io-syntax
                          (let ((*read-eval* nil))
                            (read-from-string (prin1-to-string form)))))))
        (is (equal form (sess:serialize-record restored)))))))

(test produce-branch-summary-default-appends-nothing
  (let* ((store (sess:make-session-store :id :bs-store-3))
         (session (sess:make-session :id :bs-session-3)))
    (sess:store-session store session nil)
    (flet ((append! (entry)
             (sess:append-session-entry store session entry nil)))
      (append! (sess:make-message-entry (sess:make-user-message "r1") :id :bs3-r1))
      (append! (sess:make-message-entry (sess:make-user-message "old") :id :bs3-old))
      (setf (sess:session-leaf-id session) :bs3-r1)
      (append! (sess:make-message-entry (sess:make-assistant-message "target")
                                        :id :bs3-target)))
    (is-false (sess:produce-branch-summary store session :bs3-old :bs3-target))
    (is (eq :bs3-target (sess:session-leaf-id session)))
    (is-false (typep (sess:session-leaf-entry store session)
                     'sess:branch-summary-entry))))

(test serialize-conversation-renders-roles-thinking-tool-calls-and-results
  (let* ((user (sess:make-user-message "hello"))
         (assistant (sess:make-assistant-message
                     "done"
                     :metadata
                     (list :thinking-blocks (list (list :thinking "pondering"))
                           :tool-calls (list (list :id :c1 :name "read"
                                                   :arguments-json
                                                   "{\"path\":\"a.lisp\"}")))))
         (result (sess:make-tool-result-message "file body"))
         (text (agent-session:serialize-conversation
                (list user assistant result))))
    (is (search "[User]: hello" text))
    (is (search "[Assistant thinking]: pondering" text))
    (is (search "[Assistant]: done" text))
    (is (search "[Assistant tool calls]: read({\"path\":\"a.lisp\"})" text))
    (is (search "[Tool result]: file body" text))))

(test serialize-conversation-truncates-long-tool-results
  (let* ((long (make-string 2500 :initial-element #\a))
         (result (sess:make-tool-result-message long))
         (text (agent-session:serialize-conversation (list result))))
    (is (search "more characters truncated" text))
    (is (< (length text) (length long)))))

(test (compaction-default-summarizer-threads-custom-instructions :fixture interactive-authority)
  "Custom instructions append a focus line to the framed prompt, and the isolated
request carries the summarization system prompt."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :metadata (list :fake-deltas '("ignored")))
    (let ((captured nil))
      (rt:register-model-stream-adapter
       (kli:find-live-object (kli:context-registry context)
                             :model-runtime-service)
       :fake
       (lambda (provider request context &key emit)
         (declare (ignore provider context))
         (setf captured
               (list :instructions (rt:model-request-instructions request)
                     :messages (rt:model-request-model-messages request)))
         (funcall emit (rt:make-assistant-delta "SUMMARY")))
       context)
      (let* ((service (agent-session-service context))
             (store (session-log-store context))
             (session (sess:find-session store :agent-session-test-session)))
        (flet ((append! (entry)
                 (sess:append-session-entry store session entry context)))
          (append! (sized-message-entry :user 400 :ci-u1))
          (append! (sized-message-entry :assistant 400 :ci-a1))
          (append! (sized-message-entry :user 40 :ci-u2)))
        (agent-session:recode-compaction-policy service :keep-recent-tokens 5)
        (agent-session:execute-session-compaction
         service
         (event:make-event :session-compaction-needed
                           :payload (list :mode :default-mode))
         context
         :custom-instructions "focus on the parser bug")
        (let ((prompt (getf (first (getf captured :messages)) :content)))
          (is (string= agent-session::+summarization-system-prompt+
                       (getf captured :instructions)))
          (is (search "<conversation>" prompt))
          (is (search "Additional focus: focus on the parser bug" prompt)))))))

(test (compaction-default-summarizer-merges-split-turn :fixture interactive-authority)
  "A split turn merges a separate prefix summary under a turn-context header."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :metadata (list :fake-deltas '("S")))
    (let* ((service (agent-session-service context))
           (store (session-log-store context))
           (session (sess:find-session store :agent-session-test-session)))
      (flet ((append! (entry)
               (sess:append-session-entry store session entry context)))
        (append! (sized-message-entry :user 40 :sp-s1))
        (append! (sized-message-entry :assistant 400 :sp-s2))
        (append! (sized-message-entry :assistant 40 :sp-s3)))
      (agent-session:recode-compaction-policy service :keep-recent-tokens 15)
      (agent-session:execute-session-compaction
       service
       (event:make-event :session-compaction-needed
                         :payload (list :mode :default-mode))
       context)
      (let ((leaf (sess:session-leaf-entry store session)))
        (is (typep leaf 'sess:compaction-entry))
        (is (string= (format nil "No prior history.~2%---~2%~
                                  **Turn Context (split turn):**~2%S")
                     (sess:entry-summary leaf)))))))
