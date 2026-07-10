(in-package #:kli/tests)

(defun project (event-type payload buffer &key (mode-id :default-mode))
  (tui-transcript:project-event-to-transcript
   event-type
   (event:make-event event-type :payload payload)
   mode-id
   buffer))

(test projection-default-method-returns-nil-for-unknown-event-type
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (is (null (project :nonsense '() buffer)))))

(test projection-tui-user-submitted-emits-user-event
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te (project :tui/user-submitted
                       '(:mode :default-mode :input "hello")
                       buffer)))
      (is (typep te 'tui-transcript:transcript-event))
      (is (eq :message (tui-transcript:event-kind te)))
      (is (eq :user (tui-transcript:event-role te)))
      (is (string= "hello" (tui-transcript:event-text te))))
    (is (null (project :tui/user-submitted
                       '(:mode :default-mode)
                       buffer))
        "no :input => no transcript event")
    (is (null (project :agent/user-message-appended
                       '(:kind :prompt :entry-id :e1 :text "hello")
                       buffer))
        "session-log event no longer projects to the TUI transcript")))

(test projection-streams-deltas-into-one-live-event
  "Successive deltas for a turn mutate the same live event. message-end emits no new event (already streamed) and drops the accumulator from the buffer."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te1 (project :agent/delta '(:turn-id :t1 :text "Hel") buffer)))
      (is (typep te1 'tui-transcript:transcript-event))
      (is (eq :message (tui-transcript:event-kind te1)))
      (is (eq :assistant (tui-transcript:event-role te1)))
      (is (string= "Hel" (tui-transcript:event-text te1)))
      (let ((te2 (project :agent/delta '(:turn-id :t1 :text "lo ") buffer)))
        (is (eq te1 te2) "later deltas mutate the same live event")
        (is (string= "Hello " (tui-transcript:event-text te2))))
      (let ((te3 (project :agent/delta '(:turn-id :t1 :text "world") buffer)))
        (is (eq te1 te3))
        (is (string= "Hello world" (tui-transcript:event-text te3))))
      (is (null (project :agent/message-end '(:turn-id :t1) buffer)))
      (is (string= "Hello world" (tui-transcript:event-text te1)))
      (is (zerop (hash-table-count buffer))))))

(test projection-empty-delta-before-text-creates-no-event
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (is (null (project :agent/delta '(:turn-id :t9 :text "") buffer)))
    (is (zerop (hash-table-count buffer)))
    (let ((te (project :agent/delta '(:turn-id :t9 :text "hi") buffer)))
      (is (string= "hi" (tui-transcript:event-text te))))))

(test projection-message-end-without-deltas-returns-nil
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (is (null (project :agent/message-end '(:turn-id :empty) buffer)))))

(test projection-streams-thinking-deltas-into-one-live-event
  "Successive thinking deltas for a turn mutate the same live :thinking event, the
   effort level rides as status, and message-end drops the accumulator."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te1 (project :agent/thinking-delta
                        '(:turn-id :t1 :text "Pon" :level :high) buffer)))
      (is (typep te1 'tui-transcript:transcript-event))
      (is (eq :thinking (tui-transcript:event-kind te1)))
      (is (null (tui-transcript:event-role te1)))
      (is (eq :high (tui-transcript:event-status te1))
          "the effort level rides as status so the gutter colour tracks it")
      (is (string= "Pon" (tui-transcript:event-text te1)))
      (let ((te2 (project :agent/thinking-delta '(:turn-id :t1 :text "dering.") buffer)))
        (is (eq te1 te2) "later thinking deltas mutate the same live event")
        (is (string= "Pondering." (tui-transcript:event-text te2))))
	      (is (null (project :agent/message-end '(:turn-id :t1) buffer)))
	      (is (zerop (hash-table-count buffer))))))

(test projection-replaces-live-thinking-text
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te1 (tui-transcript::project-agent-thinking-delta
                :agent/thinking-delta
                (event:make-event :agent/thinking-delta
                                  :payload '(:turn-id :t1
                                             :text "Header"
                                             :level :high))
                :default-mode buffer)))
      (let ((te2 (tui-transcript::project-agent-thinking-delta
                  :agent/thinking-delta
                  (event:make-event :agent/thinking-delta
                                    :payload '(:turn-id :t1
                                               :text "Full final thinking"
                                               :replacement-p t))
                  :default-mode buffer)))
        (is (eq te1 te2))
        (is (string= "Full final thinking"
                     (tui-transcript:event-text te2))))
      (let ((te3 (tui-transcript::project-agent-thinking-delta
                  :agent/thinking-delta
                  (event:make-event :agent/thinking-delta
                                    :payload '(:turn-id :t1 :text "."))
                  :default-mode buffer)))
        (is (string= "Full final thinking."
                     (tui-transcript:event-text te3)))))))

(test projection-scopes-thinking-replacements-to-content-block
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((first (project :agent/thinking-delta
                          '(:turn-id :t1 :content-index 0
                            :text "First provisional")
                          buffer))
          (second (project :agent/thinking-delta
                           '(:turn-id :t1 :content-index 1
                             :text "Second provisional")
                           buffer)))
      (let ((first-final (project :agent/thinking-delta
                                  '(:turn-id :t1 :content-index 0
                                    :text "First final" :replacement-p t)
                                  buffer))
            (second-final (project :agent/thinking-delta
                                   '(:turn-id :t1 :content-index 1
                                     :text "Second final" :replacement-p t)
                                   buffer)))
        (is (eq first first-final))
        (is (eq second second-final))
        (is (not (eq first second))
            "each content block owns a distinct live event")
        (is (string= "First final" (tui-transcript:event-text first)))
        (is (string= "Second final" (tui-transcript:event-text second))))
      (is (null (project :agent/message-end '(:turn-id :t1) buffer)))
      (is (zerop (hash-table-count buffer))
          "message-end clears every thinking block for the turn"))))

(test projection-prefers-raw-thinking-over-summary
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te1 (project :agent/thinking-delta
                        '(:turn-id :t1
                          :text "Inspecting merge state"
                          :source :summary
                          :level :high)
                        buffer)))
      (is (string= "Inspecting merge state"
                   (tui-transcript:event-text te1)))
      (let ((te2 (project :agent/thinking-delta
                          '(:turn-id :t1
                            :text "Need git status and conflict markers."
                            :source :raw)
                          buffer)))
        (is (eq te1 te2))
        (is (string= "Need git status and conflict markers."
                     (tui-transcript:event-text te2))))
      (is (null (project :agent/thinking-delta
                         '(:turn-id :t1
                           :text "Inspecting merge state"
                           :source :summary
                           :replacement-p t)
                         buffer)))
      (is (string= "Need git status and conflict markers."
                   (tui-transcript:event-text te1)))
      (let ((te3 (project :agent/thinking-delta
                          '(:turn-id :t1
                            :text "Need git status and conflict markers. Full."
                            :source :raw
                            :replacement-p t)
                          buffer)))
        (is (eq te1 te3))
        (is (string= "Need git status and conflict markers. Full."
                     (tui-transcript:event-text te3)))))))

(test projection-thinking-and-assistant-coexist-per-turn
  "A turn streams a :thinking event and a separate :assistant event, keyed apart so
   both live at once. The thinking delta arrives first, so it renders above the
   reply. message-end drops both accumulators."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((think (project :agent/thinking-delta
                          '(:turn-id :t2 :text "reasoning" :level :medium) buffer))
          (reply (project :agent/delta '(:turn-id :t2 :text "answer") buffer)))
      (is (eq :thinking (tui-transcript:event-kind think)))
      (is (eq :message (tui-transcript:event-kind reply)))
      (is (not (eq think reply)))
      (is (= 2 (hash-table-count buffer))
          "thinking and assistant accumulators are keyed apart")
      (is (null (project :agent/message-end '(:turn-id :t2) buffer)))
      (is (zerop (hash-table-count buffer))
          "message-end drops both the thinking and the assistant accumulator"))))

(test projection-empty-thinking-delta-before-text-creates-no-event
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (is (null (project :agent/thinking-delta '(:turn-id :t9 :text "" :level :low) buffer)))
    (is (zerop (hash-table-count buffer)))))

(defun string-keyed-hash-table (&rest key-value-pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on key-value-pairs by #'cddr
          do (setf (gethash key table) value))
    table))

(test projection-tool-execution-roundtrip
  (let* ((buffer (tui-transcript:make-projection-buffer))
         (start (project :agent/tool-execution-start
                         (list :execution-id :e1 :tool-name :bash
                               :arguments (string-keyed-hash-table
                                           "command" "ls -la"))
                         buffer))
         (end (project :agent/tool-execution-end
                       '(:execution-id :e1 :error-p nil
                         :result-text "one
two")
                       buffer)))
    (is (eq :tool-call (tui-transcript:event-kind start)))
    (is (eq :bash (tui-transcript:event-name start)))
    (is (string= "ls -la" (tui-transcript:event-text start))
        "the tool-call event carries the formatted invocation arguments")
    (is (eq :tool-result (tui-transcript:event-kind end)))
    (is (eq :bash (tui-transcript:event-name end)))
    (is (eq :ok (tui-transcript:event-status end)))
    (is (string= (format nil "one~%two") (tui-transcript:event-text end))
        "the tool-result event stores the full result text (collapse is render-time)")
    (is (zerop (hash-table-count buffer)))))

(test projection-tool-call-formats-arguments
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (flet ((call-text (execution-id arguments)
             (tui-transcript:event-text
              (project :agent/tool-execution-start
                       (list :execution-id execution-id :tool-name :tool
                             :arguments arguments)
                       buffer))))
      (is (string= "(+ 1 49)"
                   (call-text :e1 (string-keyed-hash-table "form" "(+ 1 49)")))
          "a single argument renders as its value alone")
      (is (string= "new=b old=a path=/tmp/x"
                   (call-text :e2 (string-keyed-hash-table
                                   "path" "/tmp/x" "old" "a" "new" "b")))
          "multiple arguments render as space-separated key=value pairs, key-sorted")
      (is (string= "" (call-text :e3 (string-keyed-hash-table)))
          "no arguments render as the empty string")
      (is (string= "" (call-text :e4 nil))
          "missing arguments render as the empty string"))))

(test projection-tool-call-drops-content-blobs-from-header
  "A default call header shows only scalar arguments: long or multi-line
content/patch blobs are omitted from the compact header, but retained as a
bounded expanded preview on the call presentation."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (flet ((call-event (execution-id arguments)
             (project :agent/tool-execution-start
                      (list :execution-id execution-id :tool-name :write
                            :arguments arguments)
                      buffer)))
      (let* ((event (call-event :e10 (string-keyed-hash-table
                                     "path" "/tmp/x"
                                     "content" (make-string 400 :initial-element #\a))))
             (term (tui-transcript:event-presentation event)))
        (is (string= "/tmp/x" (tui-transcript:event-text event))
            "a long content blob is dropped, leaving only the scalar path")
        (is (search "content:" (getf term :preview))
            "the omitted blob is still available in the expanded preview"))
      (let* ((event (call-event :e11 (string-keyed-hash-table
                                     "path" "/tmp/x"
                                     "content" (format nil "line1~%line2"))))
             (term (tui-transcript:event-presentation event)))
        (is (string= "/tmp/x" (tui-transcript:event-text event))
            "a multi-line content blob is dropped from the header too")
        (is (search (format nil "line1~%line2") (getf term :preview))
            "the expanded preview preserves newlines")))))

(test projection-tool-execution-error-status
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (project :agent/tool-execution-start
             '(:execution-id :e2 :tool-name :bash) buffer)
    (let ((end (project :agent/tool-execution-end
                        '(:execution-id :e2 :error-p t)
                        buffer)))
      (is (eq :error (tui-transcript:event-status end))))))

(test projection-tool-execution-end-carries-details-and-presentation
  "Public details and private presentation terms travel on separate slots. A
payload without details projects nil, so non-file tools are untouched."
  (let* ((buffer (tui-transcript:make-projection-buffer))
         (details '(:path "/tmp/x" :added 1 :removed 1
                    :changed-ranges ((:start 1 :end 1))))
         (term (ext:result-diff
                :updates (list (tools-filesystem:file-diff-presentation-update
                                "/tmp/x" "a" "b")))))
    (project :agent/tool-execution-start
             '(:execution-id :e7 :tool-name :edit) buffer)
    (let ((end (project :agent/tool-execution-end
                        (list :execution-id :e7 :error-p nil
                              :result-text "Edited /tmp/x (+1 -1)"
                              :details details
                              :result-term term)
                        buffer)))
      (is (eq details (tui-transcript:event-details end)))
      (is (eq term (tui-transcript:event-presentation end))))
    (project :agent/tool-execution-start
             '(:execution-id :e8 :tool-name :bash) buffer)
    (let ((end (project :agent/tool-execution-end
                        '(:execution-id :e8 :error-p nil :result-text "ok")
                        buffer)))
      (is (null (tui-transcript:event-details end))))))

(test projection-session-events-produce-system-messages
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (is (null (project :session-switch
                       '(:mode :default-mode :session-id :sess-1)
                       buffer))
        "session-switch is an internal lifecycle step that surfaces no line")
    (let ((te (project :session-branch
                       '(:mode :default-mode :branched-from-entry :entry-9
                         :new-session-id :sess-2)
                       buffer)))
      (is (eq :notice (tui-transcript:event-kind te)))
      (is (search "ENTRY-9" (tui-transcript:event-text te) :test #'char-equal)))
    (let ((te (project :session-reset '(:mode :default-mode) buffer)))
      (is (eq :notice (tui-transcript:event-kind te)))
      (is (search "reset" (tui-transcript:event-text te))))))

(test projection-tui-error-tags-status-error
  (let* ((buffer (tui-transcript:make-projection-buffer))
         (te (project :tui-error '(:message "kaboom") buffer)))
    (is (eq :notice (tui-transcript:event-kind te)))
    (is (eq :error (tui-transcript:event-status te))
        "errors carry :error status so the renderer picks the error bar")
    (is (search "Internal error: kaboom" (tui-transcript:event-text te)))))

(test projection-tui-error-renders-by-category
  "Classified failures shed the \"Internal error\" label: network errors get a
network prefix, provider errors render their self-describing report verbatim."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te (project :tui-error
                       '(:category :network :message "Connection reset by peer")
                       buffer)))
      (is (string= "Network error: Connection reset by peer"
                   (tui-transcript:event-text te)))
      (is (eq :error (tui-transcript:event-status te))))
    (let ((te (project :tui-error
                       '(:category :provider
                         :message "Anthropic API error (HTTP 529): overloaded")
                       buffer)))
      (is (string= "Anthropic API error (HTTP 529): overloaded"
                   (tui-transcript:event-text te))))
    (let ((te (project :tui-error
                       '(:category :config
                         :message "No credential for openai-codex -- run /auth login openai-codex")
                       buffer)))
      (is (string= "No credential for openai-codex -- run /auth login openai-codex"
                   (tui-transcript:event-text te))))
    (let ((te (project :tui-error
                       '(:category :internal :message "kaboom")
                       buffer)))
      (is (string= "Internal error: kaboom" (tui-transcript:event-text te))))))

(test projection-delta-accumulates-in-place-and-compacts-on-end
  "Per-delta appends extend one adjustable accumulator in place where
concatenate would copy the whole accumulated reply per delta; message-end
compacts the finalized text to a simple string, dropping growth headroom."
  (let* ((buffer (tui-transcript:make-projection-buffer))
         (te (project :agent/delta '(:turn-id :t-acc :text "Hello") buffer)))
    (project :agent/delta '(:turn-id :t-acc :text ", world") buffer)
    (let ((text-before (tui-transcript:event-text te)))
      (is (eq te (project :agent/delta '(:turn-id :t-acc :text "!") buffer)))
      (is (eq text-before (tui-transcript:event-text te))
          "appends mutate the accumulator instead of copying"))
    (is (string= "Hello, world!" (tui-transcript:event-text te)))
    (project :agent/message-end '(:turn-id :t-acc) buffer)
    (is (string= "Hello, world!" (tui-transcript:event-text te)))
    (is (typep (tui-transcript:event-text te) 'simple-string))))

(test projection-compaction-events-render-by-status
  "Compaction lifecycle events render as system lines; a failure carries
:error status so it never reads as a quiet no-op, and an auto-triggered
nothing-to-compact stays silent."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te (project :session-compaction-started
                       '(:mode :default-mode :trigger :manual) buffer)))
      (is (eq :notice (tui-transcript:event-kind te)))
      (is (string= "Compacting session..." (tui-transcript:event-text te))))
    (let ((te (project :session-compaction-finished
                       '(:mode :default-mode :trigger :auto :status :compacted)
                       buffer)))
      (is (string= "Session compacted." (tui-transcript:event-text te))))
    (let ((te (project :session-compaction-finished
                       '(:mode :default-mode :trigger :manual :status :failed
                         :error "HTTP 500 from summarizer")
                       buffer)))
      (is (string= "Compaction failed: HTTP 500 from summarizer"
                   (tui-transcript:event-text te)))
      (is (eq :error (tui-transcript:event-status te))))
    (is (null (project :session-compaction-finished
                       '(:mode :default-mode :trigger :auto
                         :status :nothing-to-compact)
                       buffer))
        "an auto-triggered no-op projects nothing")
    (let ((te (project :session-compaction-finished
                       '(:mode :default-mode :trigger :manual
                         :status :nothing-to-compact)
                       buffer)))
      (is (string= "Nothing to compact." (tui-transcript:event-text te))))))

(test projection-length-stop-reason-marks-reply-truncated
  "A message-end carrying a :length stop reason marks the live reply event
:truncated so the render annotates the cut. Only the reply is marked -- the
thinking event keeps its effort status -- and an ordinary :end leaves the
reply status untouched."
  (let* ((buffer (tui-transcript:make-projection-buffer))
         (think (project :agent/thinking-delta
                         '(:turn-id :t-cut :text "hmm" :level :low) buffer))
         (reply (project :agent/delta '(:turn-id :t-cut :text "Hello") buffer)))
    (is (null (project :agent/message-end
                       '(:turn-id :t-cut :stop-reason :length) buffer)))
    (is (eq :truncated (tui-transcript:event-status reply)))
    (is (eq :low (tui-transcript:event-status think)))
    (is (zerop (hash-table-count buffer))))
  (let* ((buffer (tui-transcript:make-projection-buffer))
         (reply (project :agent/delta '(:turn-id :t-ok :text "Hi") buffer)))
    (project :agent/message-end '(:turn-id :t-ok :stop-reason :end) buffer)
    (is (null (tui-transcript:event-status reply)))))

(test projection-agent-error-renders-by-category
  "Unsupervised :agent/error events (steer drains, worker catch-ups) render as
error system lines labeled like :tui-error -- the spinner stopping was
previously the only sign the turn died."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te (project :agent/error
                       '(:turn-id :t1 :category :network :supervised nil
                         :condition "Connection reset by peer")
                       buffer)))
      (is (eq :notice (tui-transcript:event-kind te)))
      (is (eq :error (tui-transcript:event-status te)))
      (is (string= "Network error: Connection reset by peer"
                   (tui-transcript:event-text te))))
    (let ((te (project :agent/error
                       '(:turn-id :t1 :category :provider :supervised nil
                         :condition "OpenAI API error (HTTP 500): boom")
                       buffer)))
      (is (string= "OpenAI API error (HTTP 500): boom"
                   (tui-transcript:event-text te))))
    (let ((te (project :agent/error
                       '(:turn-id :t1 :category :internal :supervised nil
                         :condition "kaboom")
                       buffer)))
      (is (string= "Internal error: kaboom"
                   (tui-transcript:event-text te))))))

(test projection-agent-error-skips-supervised
  "A supervised error is observed by the session retry loop -- retried or
re-signaled into the :tui-error path -- so projecting it too would render the
same condition twice."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (is (null (project :agent/error
                       '(:turn-id :t1 :category :provider :supervised t
                         :condition "boom")
                       buffer)))))

(test projection-agent-retry-emits-system-progress-line
  "Retry attempts render as system lines so a backoff window reads as
progress, with the zero-delay form dropping the wait clause."
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (let ((te (project :agent/retry
                       '(:attempt 2 :max-attempts 5 :delay-ms 2000
                         :category :network :message "reset")
                       buffer)))
      (is (eq :notice (tui-transcript:event-kind te)))
      (is (string= "Network error -- retrying in 2s (attempt 2 of 5)"
                   (tui-transcript:event-text te))))
    (let ((te (project :agent/retry
                       '(:attempt 1 :max-attempts 5 :delay-ms 0
                         :category :provider :message "x")
                       buffer)))
      (is (string= "Provider error -- retrying (attempt 1 of 5)"
                   (tui-transcript:event-text te))))))

(test projection-model-and-option-change-emit-formatted-selection
  (let ((buffer (tui-transcript:make-projection-buffer)))
    (multiple-value-bind (context protocol)
        (agent-session-test-context)
      (declare (ignore protocol))
      (let ((selection (agent-loop-register-model
                        context "p" "m" :metadata '())))
        (let ((te (project :model-change selection buffer)))
          (is (eq :notice (tui-transcript:event-kind te)))
          (is (search "Model:" (tui-transcript:event-text te))))
        (let ((te (project :option-change
                           '(:option-id :reasoning-effort :value :high)
                           buffer)))
          (is (eq :notice (tui-transcript:event-kind te)))
          (is (string= "Thinking: high" (tui-transcript:event-text te))))
        (is (null (project :option-change
                           '(:option-id :text-verbosity :value :low)
                           buffer)))))))

(defun count-projection-method-contributions (protocol)
  (count-if (lambda (c)
              (and (typep c 'ext:method-contribution)
                   (eq (ext:contribution-gf-name c)
                       'tui-transcript:project-event-to-transcript)))
            (ext:protocol-installed-contributions protocol)))

(test (projection-listener-filters-by-mode-id :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context
                                    :columns 32
                                    :mode-id :alpha))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (listener (find (tui-app:tui-app-listener-id app)
                         (agent-session:session-event-listeners service)
                         :key #'agent-session:listener-id
                         :test #'equal))
         (handler (agent-session:listener-handler listener))
         (probe (event:make-event :session-reset
                                  :payload '(:mode :alpha))))
    (funcall handler probe :alpha context)
    (is (= 1 (length (tui-app:tui-app-transcript-events app)))
        "matching mode-id projects into transcript")
    (funcall handler probe :beta context)
    (is (= 1 (length (tui-app:tui-app-transcript-events app)))
        "non-matching mode-id is dropped before projection")))

(test (projection-method-contributions-retract-on-deactivate :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tui-views:*tui-views-extension-manifest*
                        tui-input:*tui-input-extension-manifest*
                        tui-editor:*tui-editor-extension-manifest*
                        tui-terminal:*tui-terminal-extension-manifest*
                        tui-transcript:*tui-transcript-extension-manifest*)
    (is (plusp (count-projection-method-contributions protocol))
        "projection methods register on install")
    (let ((extension (kli:find-live-object (kli:context-registry context)
                                           :tui-transcript)))
      (ext:deactivate-extension protocol extension context))
    (is (zerop (count-projection-method-contributions protocol))
        "projection methods retract on deactivate")))

(test session-entry-replay-covers-message-entries
  "Stored message entries replay as the transcript events the live projectors
   would have shown: user/assistant messages, thinking blocks above the reply,
   tool calls, and tool results with status and details."
  (let ((events (tui-transcript:session-entry-transcript-events
                 (sess:make-message-entry (sess:make-user-message "hi")))))
    (is (= 1 (length events)))
    (is (eq :message (tui-transcript:event-kind (first events))))
    (is (eq :user (tui-transcript:event-role (first events))))
    (is (string= "hi" (tui-transcript:event-text (first events)))))
  (let* ((message (sess:make-assistant-message
                   "answer"
                   :metadata '(:thinking-blocks
                               ((:thinking "pondering"
                                 :signature nil :redacted nil))
                               :tool-calls
                               ((:id :call-1 :name "read"
                                 :arguments-json "{\"path\":\"a.md\"}")))))
         (events (tui-transcript:session-entry-transcript-events
                  (sess:make-message-entry message))))
    (is (equal '(:thinking :message :tool-call)
               (mapcar #'tui-transcript:event-kind events)))
    (is (string= "pondering" (tui-transcript:event-text (first events))))
    (is (eq :assistant (tui-transcript:event-role (second events))))
    (is (string= "answer" (tui-transcript:event-text (second events))))
    (is (string= "read" (tui-transcript:event-name (third events))))
    (is (string= "a.md" (tui-transcript:event-text (third events)))))
  (let* ((events (tui-transcript:session-entry-transcript-events
                  (sess:make-message-entry
                   (sess:make-tool-result-message
                    "contents"
                    :tool-call-id :call-1
                    :tool-name "read"
                    :metadata '(:details (:path "a.md"))))))
         (te (first events)))
    (is (= 1 (length events)))
    (is (eq :tool-result (tui-transcript:event-kind te)))
    (is (string= "read" (tui-transcript:event-name te)))
    (is (eq :ok (tui-transcript:event-status te)))
    (is (equal '(:path "a.md") (tui-transcript:event-details te)))
    (is (string= "contents" (tui-transcript:event-text te))))
  (let ((te (first (tui-transcript:session-entry-transcript-events
                    (sess:make-message-entry
                     (sess:make-tool-result-message
                      "boom" :tool-name "bash" :error-p t))))))
    (is (eq :error (tui-transcript:event-status te)))))

(test session-entry-replay-shows-raw-json-for-unparseable-arguments
  "Recorded tool-call arguments that fail to parse replay as their raw JSON
string instead of an empty invocation, so the call still shows what was sent."
  (let* ((message (sess:make-assistant-message
                   ""
                   :metadata '(:tool-calls
                               ((:id :call-1 :name "read"
                                 :arguments-json "{\"path\": tr")))))
         (events (tui-transcript:session-entry-transcript-events
                  (sess:make-message-entry message)))
         (te (first events)))
    (is (= 1 (length events)))
    (is (eq :tool-call (tui-transcript:event-kind te)))
    (is (string= "{\"path\": tr" (tui-transcript:event-text te)))))

(test session-entry-replay-covers-marker-entries
  "Model, option, compaction, and displayable custom entries replay as the
   same system lines the live projectors emit. Hidden custom entries and
   data-only entries surface nothing."
  (let ((te (first (tui-transcript:session-entry-transcript-events
                    (sess:make-model-change-entry "anthropic" "claude")))))
    (is (eq :notice (tui-transcript:event-kind te)))
    (is (string= "Model: anthropic/claude" (tui-transcript:event-text te))))
  (let ((te (first (tui-transcript:session-entry-transcript-events
                    (sess:make-option-change-entry :reasoning-effort :high)))))
    (is (string= "Thinking: high" (tui-transcript:event-text te))))
  (is (null (tui-transcript:session-entry-transcript-events
             (sess:make-option-change-entry :text-verbosity :low)))
      "non-reasoning option changes do not add transcript noise")
  (let ((te (first (tui-transcript:session-entry-transcript-events
                    (sess:make-compaction-entry "the summary" :kept-1)))))
    (is (string= "Session compacted." (tui-transcript:event-text te))))
  (let ((te (first (tui-transcript:session-entry-transcript-events
                    (sess:make-custom-message-entry :note "shown note")))))
    (is (eq :notice (tui-transcript:event-kind te)))
    (is (string= "shown note" (tui-transcript:event-text te))))
  (is (null (tui-transcript:session-entry-transcript-events
             (make-instance 'sess:custom-message-entry
                            :id :hidden-note
                            :custom-type :note
                            :display-p nil
                            :message (sess:make-custom-agent-message
                                      :note "hidden"))))
      "entries marked not for display surface nothing")
  (is (null (tui-transcript:session-entry-transcript-events
             (sess:make-custom-entry :checkpoint)))
      "data-only custom entries surface nothing")
  (is (null (tui-transcript:session-entry-transcript-events
             (sess:make-branch-summary-entry :sess-0 "branch summary")))
      "branch summaries are model-context machinery, not transcript rows"))
