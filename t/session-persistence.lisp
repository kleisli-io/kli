(in-package #:kli/tests)
(in-suite all)

(defclass unserializable-probe () ())

(defun print-then-read (form)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string (prin1-to-string form)))))

(defun roundtrip-record (object)
  "Serialized form of OBJECT, and the form from deserializing its printed
representation and re-serializing."
  (let* ((form (sess:serialize-record object))
         (restored (sess:deserialize-value (print-then-read form))))
    (values form (sess:serialize-record restored))))

(defun sample-context-patch-set ()
  (make-instance 'ctx:context-patch-set
                 :patches (list (ctx:make-append-message-patch
                                 (sess:make-user-message "patched"
                                                         :id :agent-message-901)
                                 :id :context-patch-902
                                 :actor :user))
                 :actor :user
                 :base-epoch 0
                 :result-epoch 1))

(test serialize-roundtrips-agent-messages
  "Serialization protocol -- an object serialized to a record, printed, read back with *read-eval* disabled, deserialized, and re-serialized must yield an identical record form."
  (dolist (message (list (sess:make-user-message "hello"
                                                 :id :agent-message-1
                                                 :metadata '(:k :v))
                         (sess:make-assistant-message "hi" :id :agent-message-2)
                         (sess:make-tool-result-message "result"
                                                        :id :agent-message-3
                                                        :tool-call-id "call-1"
                                                        :tool-name "bash"
                                                        :error-p t)
                         (sess:make-custom-agent-message :note "noted"
                                                         :id :agent-message-4)))
    (multiple-value-bind (before after) (roundtrip-record message)
      (is (equal before after)
          "round-trip preserves ~A" (sess:record-type before)))))

(test serialize-roundtrips-session-entries
  (dolist (entry (list (sess:make-message-entry
                        (sess:make-user-message "m" :id :agent-message-10)
                        :id :session-entry-1)
                       (sess:make-model-change-entry :anthropic :claude
                                                     :id :session-entry-2)
                       (sess:make-option-change-entry :reasoning-effort :high :id :session-entry-3)
                       (sess:make-compaction-entry "summary" :session-entry-1
                                                   :id :session-entry-4
                                                   :tokens-before 1200
                                                   :data '(:read-files ("a.lisp")))
                       (sess:make-branch-summary-entry :session-entry-1
                                                       "branch summary"
                                                       :id :session-entry-5
                                                       :data '(:modified-files
                                                               ("b.lisp")))
                       (sess:make-custom-entry :ui-state
                                               :id :session-entry-6
                                               :data '(:scroll 10))
                       (sess:make-custom-message-entry :note "include me"
                                                       :id :session-entry-7)))
    (multiple-value-bind (before after) (roundtrip-record entry)
      (is (equal before after)
          "round-trip preserves ~A" (sess:record-type before)))))

(test serialize-roundtrips-session-file-header
  (multiple-value-bind (before after)
      (roundtrip-record
       (sess:make-session-file-header
        :persisted-session
        :timestamp 123456
        :cwd "/tmp/project"
        :leaf-id :session-entry-9
        :metadata '(:branched-from :session-entry-2 :branched-at 100)))
    (is (equal before after))))

(test serialize-roundtrips-context-patches
  (context-lens-test-context)
  (multiple-value-bind (before after)
      (roundtrip-record (ctx:make-append-message-patch
                         (sess:make-assistant-message "appended"
                                                      :id :agent-message-20)
                         :id :context-patch-1
                         :actor :user
                         :metadata '(:reason :test)))
    (is (equal before after)))
  (multiple-value-bind (before after)
      (roundtrip-record (sample-context-patch-set))
    (is (equal before after))))

(test serialize-roundtrips-subjects
  (context-lens-test-context)
  (multiple-value-bind (before after)
      (roundtrip-record (ext:make-system-subject))
    (is (equal before after)))
  (multiple-value-bind (before after)
      (roundtrip-record (ext:make-subject :capabilities '(:context/commit-edit)))
    (is (equal before after))))

(test custom-entry-wrapping-context-patch-set-roundtrips
  (context-lens-test-context)
  (multiple-value-bind (before after)
      (roundtrip-record (sess:make-custom-entry
                         :context-patch-set
                         :id :session-entry-30
                         :data (sample-context-patch-set)))
    (is (equal before after))))

(test serialize-value-passes-data-atoms-and-signals-otherwise
  (is (null (sess:serialize-value nil)))
  (is (eq t (sess:serialize-value t)))
  (is (eq :keyword (sess:serialize-value :keyword)))
  (is (equal '(:a 1 "s") (sess:serialize-value '(:a 1 "s"))))
  (signals sess:unserializable-value (sess:serialize-value 3.14))
  (signals sess:unserializable-value (sess:serialize-value 'plain-symbol))
  (signals sess:unserializable-value (sess:serialize-value (cons 1 2)))
  (signals sess:unserializable-value
    (sess:serialize-value (make-instance 'unserializable-probe))))

(defun run-exploding-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool parameters context call-id on-update))
  (error "tool exploded"))

(ext:defextension test-exploding-tool
  (:provides
   (tool exploder
     :label "Exploder"
     :description "Signals a plain error so error results can be round-tripped."
     :parameters '(:object ())
     :runner #'run-exploding-tool)))

(test tool-error-result-entry-roundtrips-durably
  "An error tool-result built by INVOKE-TOOL must be representable by the
durable serializer: its :details may carry only keyword/string/integer
leaves, so the condition type arrives as a string."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-exploding-tool-extension-manifest*)
    (let ((result (ext:invoke-tool protocol :exploder '() context)))
      (is (ext:tool-result-error-p result))
      (is (stringp (getf (ext:tool-result-details result) :condition-type)))
      (is (string= "SIMPLE-ERROR"
                   (getf (ext:tool-result-details result) :condition-type)))
      (multiple-value-bind (before after)
          (roundtrip-record
           (sess:make-message-entry
            (sess:make-tool-result-message
             "tool exploded"
             :id :agent-message-880
             :tool-call-id "call-explode"
             :tool-name :exploder
             :error-p (ext:tool-result-error-p result)
             :metadata (list :details (ext:tool-result-details result)))
            :id :session-entry-880))
        (is (equal before after))))))

(test unregistered-record-tagged-list-is-plain-data
  (let ((datum '(:record :no-such-type :field 1)))
    (is (equal datum (sess:deserialize-value datum)))))

(test deserialize-advances-subsystem-counters
  (context-lens-test-context)
  (let ((sess::*entry-counter* (kli:make-id-counter))
        (sess::*message-counter* (kli:make-id-counter))
        (ctx::*context-patch-counter* (kli:make-id-counter)))
    (sess:deserialize-value
     (sess:serialize-record
      (sess:make-message-entry
       (sess:make-user-message "x" :id :agent-message-8002)
       :id :session-entry-9001)))
    (is (>= (kli:counter-value 'sess::*entry-counter*) 9001))
    (is (>= (kli:counter-value 'sess::*message-counter*) 8002))
    (sess:deserialize-value
     (sess:serialize-record
      (ctx:make-context-patch :append-message :id :context-patch-7003)))
    (is (>= (kli:counter-value 'ctx::*context-patch-counter*) 7003))))

(test migrate-session-record-is-identity-at-current-version
  (let ((record (sess:serialize-record
                 (sess:make-user-message "hi" :id :agent-message-40))))
    (is (eq record (sess:migrate-session-record record 1)))))
