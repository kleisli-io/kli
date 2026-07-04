(in-package #:kli/tui/transcript)

(defun make-projection-buffer ()
  (make-hash-table :test #'equal))

(defun delta-key (turn-id)
  (list :delta turn-id))

(defun thinking-key (turn-id)
  (list :thinking turn-id))

(defun tool-key (execution-id)
  (list :tool execution-id))

(defgeneric project-event-to-transcript (event-type event mode-id buffer)
  (:documentation
   "Return (or transcript-event nil). Buffer is the per-listener accumulator
    state. Methods may mutate buffer to aggregate across events (e.g. delta
    accumulation keyed by turn-id)."))

(defun append-streaming-text (buffer text)
  "Extend BUFFER (an adjustable fill-pointer string) with TEXT in place."
  (let* ((start (fill-pointer buffer))
         (end (+ start (length text))))
    (when (> end (array-dimension buffer 0))
      (adjust-array buffer (max end (* 2 (array-dimension buffer 0)))))
    (setf (fill-pointer buffer) end)
    (replace buffer text :start1 start)
    buffer))

(defun make-streaming-text (text)
  "Adjustable accumulator for a streaming event's text. Per-delta appends
   extend it in place where (concatenate 'string existing delta) would copy
   the whole accumulated reply on every delta -- O(reply^2) over a stream."
  (let ((buffer (make-array (max 64 (* 2 (length text)))
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (append-streaming-text buffer text)))

(defmethod project-event-to-transcript (event-type event mode-id buffer)
  (declare (ignore event-type event mode-id buffer))
  nil)

(defun project-user-submitted (event-type event mode-id buffer)
  (declare (ignore event-type mode-id buffer))
  (let ((text (getf (kli/event:event-payload event) :input)))
    (and (stringp text)
         (make-transcript-event :message :user text))))

(defun project-assistant-delta (event-type event mode-id buffer)
  "Stream incrementally by keeping one live assistant transcript-event per turn
   in the buffer, appending each delta's text. The first delta creates and
   returns the event, which the listener adds once. Later deltas mutate that same
   event in place and return it so the listener re-renders without duplicating it."
  (declare (ignore event-type mode-id))
  (let* ((payload (kli/event:event-payload event))
         (turn-id (getf payload :turn-id))
         (text (or (getf payload :text) "")))
    (when turn-id
      (let* ((key (delta-key turn-id))
             (existing (gethash key buffer)))
        (cond
          ((typep existing 'transcript-event)
           (append-streaming-text (event-text existing) text)
           existing)
          ((plusp (length text))
           (let ((te (make-transcript-event :message :assistant
                                            (make-streaming-text text))))
             (setf (gethash key buffer) te)
             te))
          (t nil))))))

(defun project-agent-thinking-delta (event-type event mode-id buffer)
  "Stream reasoning incrementally by keeping one live :thinking transcript-event
   per turn in the buffer, appending each delta's text. The first delta creates
   and returns the event, later deltas mutate it in place and return it. Status
   carries the effort level so the gutter colour tracks it. Reasoning arrives
   before the answer, so the thinking event lands above the reply."
  (declare (ignore event-type mode-id))
  (let* ((payload (kli/event:event-payload event))
         (turn-id (getf payload :turn-id))
         (text (or (getf payload :text) ""))
         (level (getf payload :level)))
    (when turn-id
      (let* ((key (thinking-key turn-id))
             (existing (gethash key buffer)))
        (cond
          ((typep existing 'transcript-event)
           (append-streaming-text (event-text existing) text)
           existing)
          ((plusp (length text))
           (let ((te (make-transcript-event :thinking nil
                                            (make-streaming-text text)
                                            :status level)))
             (setf (gethash key buffer) te)
             te))
          (t nil))))))

(defun project-assistant-message-end (event-type event mode-id buffer)
  "The live events were already streamed into the transcript by the deltas, so
   drop the per-turn accumulators and compact each finalized event's text from
   the growth-headroom accumulator to a simple string. A :length stop reason
   marks the reply event :truncated so the render annotates the cut. No new
   event to emit."
  (declare (ignore event-type mode-id))
  (let* ((payload (kli/event:event-payload event))
         (turn-id (getf payload :turn-id)))
    (when turn-id
      (when (eq (getf payload :stop-reason) :length)
        (let ((te (gethash (delta-key turn-id) buffer)))
          (when (typep te 'transcript-event)
            (setf (event-status te) :truncated))))
      (dolist (key (list (delta-key turn-id) (thinking-key turn-id)))
        (let ((te (gethash key buffer)))
          (when (typep te 'transcript-event)
            (setf (event-text te)
                  (coerce (event-text te) 'simple-string)))
          (remhash key buffer))))
    nil))

(defun project-tool-execution-start (event-type event mode-id buffer)
  "Emit the tool-call line from the boundary-computed call-term riding in the
   payload (an old event without one falls back to a scalar-args header). The
   name is always buffered for the matching end event; a :hidden call -- a read
   -- buffers its name but emits no row."
  (declare (ignore event-type mode-id))
  (let* ((payload (kli/event:event-payload event))
         (execution-id (getf payload :execution-id))
         (name (getf payload :tool-name))
         (call-term (or (getf payload :call-term)
                        (default-call-term (getf payload :arguments)))))
    (when execution-id
      (setf (gethash (tool-key execution-id) buffer) name))
    (unless (eq (presentation-kind call-term) :hidden)
      (make-transcript-event :tool-call nil
                             (or (getf call-term :text) "")
                             :name name
                             :presentation call-term))))

(defun project-tool-execution-end (event-type event mode-id buffer)
  "Store the full result text. Collapsing to a head-preview is a render-time
   concern toggled by Ctrl+O via TOOL-RESULT-DISPLAY-TEXT. The payload's
   details plist rides along untouched for renderers with structured views."
  (declare (ignore event-type mode-id))
  (let* ((payload (kli/event:event-payload event))
         (execution-id (getf payload :execution-id))
         (key (and execution-id (tool-key execution-id)))
         (name (and key (gethash key buffer)))
         (status (if (getf payload :error-p) :error :ok)))
    (when key (remhash key buffer))
    (make-transcript-event :tool-result nil
                           (or (getf payload :result-text) "")
                           :name name :status status
                           :details (getf payload :details)
                           :presentation (getf payload :result-term))))

(defun project-session-switch (event-type event mode-id buffer)
  "Session-switch surfaces no line of its own. The TUI app layer rebuilds the
   transcript from the switched-to session's stored entries instead (see
   SESSION-ENTRY-TRANSCRIPT-EVENTS), and the user-facing actions that switch
   (new, reset, branch) emit their own specific messages."
  (declare (ignore event-type event mode-id buffer))
  nil)

(defun project-session-branch (event-type event mode-id buffer)
  (declare (ignore event-type mode-id buffer))
  (let* ((payload (kli/event:event-payload event))
         (entry-id (getf payload :branched-from-entry)))
    (make-transcript-event
     :notice nil
     (format nil "Branched at ~A" entry-id))))

(defun project-session-reset (event-type event mode-id buffer)
  "A user-driven reset (:reason :reset, the default for an old event) shows the
   line; the cold initial bind (:reason :initial) shows nothing, so boot is silent."
  (declare (ignore event-type mode-id buffer))
  (when (eq (getf (kli/event:event-payload event) :reason :reset) :reset)
    (make-transcript-event :notice nil "Session reset.")))

(defun format-model-selection-line (label selection)
  (let* ((provider (kli/model/registry:model-selection-provider-id selection))
         (model (kli/model/registry:model-selection-model-id selection))
         (level (kli/model/registry:model-selection-option-value selection "reasoning-effort")))
    (if level
        (format nil "~A: ~A/~A thinking ~A"
                label provider model
                (string-downcase (symbol-name level)))
        (format nil "~A: ~A/~A" label provider model))))

(defun project-model-change (event-type event mode-id buffer)
  (declare (ignore event-type mode-id buffer))
  (let ((selection (kli/event:event-payload event)))
    (and selection
         (make-transcript-event
          :notice nil
          (format-model-selection-line "Model" selection)))))

(defun format-option-change-line (option-id value)
  (when (eq option-id :reasoning-effort)
    (format nil "Thinking: ~A"
            (string-downcase (symbol-name (or value :none))))))

(defun project-option-change (event-type event mode-id buffer)
  (declare (ignore event-type mode-id buffer))
  (let* ((payload (kli/event:event-payload event))
         (line (and (listp payload)
                    (format-option-change-line (getf payload :option-id)
                                               (getf payload :value)))))
    (and line
         (make-transcript-event :notice nil line))))

(defun project-agent-retry (event-type event mode-id buffer)
  "One system line per retry attempt so the backoff window reads as progress
instead of a frozen UI."
  (declare (ignore event-type mode-id buffer))
  (let* ((payload (kli/event:event-payload event))
         (label (case (getf payload :category)
                  (:network "Network error")
                  (:provider "Provider error")
                  (t "Error")))
         (delay-ms (getf payload :delay-ms)))
    (make-transcript-event
     :notice nil
     (format nil "~A -- retrying~@[ in ~As~] (attempt ~A of ~A)"
             label
             (and delay-ms (plusp delay-ms) (round delay-ms 1000))
             (getf payload :attempt)
             (getf payload :max-attempts)))))

(defun project-compaction-started (event-type event mode-id buffer)
  (declare (ignore event-type event mode-id buffer))
  (make-transcript-event :notice nil "Compacting session..."))

(defun project-compaction-finished (event-type event mode-id buffer)
  "A failed compaction renders as an error so it never reads as \"nothing to
compact\", and a user abort renders as a plain acknowledgment, never an
error. The nothing-to-compact outcome only renders for a manual /compact --
an auto-triggered no-op would be unprompted noise."
  (declare (ignore event-type mode-id buffer))
  (let* ((payload (kli/event:event-payload event))
         (status (getf payload :status)))
    (case status
      (:compacted
       (make-transcript-event :notice nil "Session compacted."))
      (:aborted
       (make-transcript-event :notice nil "Compaction aborted."))
      (:failed
       (make-transcript-event
        :notice nil
        (format nil "Compaction failed: ~A"
                (or (getf payload :error) "unknown error"))
        :status :error))
      (:nothing-to-compact
       (when (eq (getf payload :trigger) :manual)
         (make-transcript-event :notice nil "Nothing to compact."))))))

(defgeneric session-entry-transcript-events (entry)
  (:documentation
   "Transcript events replaying stored ENTRY, oldest first, mirroring what the
    live projectors would have shown when the entry was appended. Entry kinds
    with no user-facing line return NIL."))

(defmethod session-entry-transcript-events ((entry t))
  '())

(defgeneric message-transcript-events (message)
  (:documentation "Transcript events replaying a stored agent MESSAGE."))

(defmethod message-transcript-events ((message t))
  '())

(defmethod message-transcript-events ((message kli/session/log:user-message))
  (let ((content (kli/session/log:message-content message)))
    (when (and (stringp content) (plusp (length content)))
      (list (make-transcript-event :message :user content)))))

(defun replayed-call-term (call presentations)
  "Call-term for a replayed tool CALL: the term stored at execution time, keyed
   by call-id in PRESENTATIONS, or -- for a pre-presentation record -- a
   scalar-args header reconstructed from the call's arguments, falling back to
   the raw JSON when those arguments do not parse."
  (or (second (assoc (getf call :id) presentations :test #'equal))
      (multiple-value-bind (arguments parse-error)
          (kli/model/runtime:parse-tool-call-arguments
           (getf call :arguments-json))
        (if parse-error
            (kli/ext:call-header (getf call :arguments-json))
            (default-call-term arguments)))))

(defun replayed-tool-call-events (tool-calls presentations)
  "Tool-call events replaying an assistant message's recorded tool-calls. Each
   call's stored presentation term drives the view exactly as the live execution
   did: a :hidden call -- a read -- emits no row, a command renders as `$ cmd`."
  (loop for call in tool-calls
        for term = (replayed-call-term call presentations)
        unless (eq (presentation-kind term) :hidden)
          collect (make-transcript-event
                   :tool-call nil (or (getf term :text) "")
                   :name (getf call :name)
                   :presentation term)))

(defmethod message-transcript-events ((message kli/session/log:assistant-message))
  (let* ((metadata (kli/session/log:message-metadata message))
         (content (kli/session/log:message-content message))
         (events '()))
    (dolist (thinking-block (getf metadata :thinking-blocks))
      (let ((text (getf thinking-block :thinking)))
        (when (and (stringp text) (plusp (length text)))
          (push (make-transcript-event :thinking nil text) events))))
    (when (and (stringp content) (plusp (length content)))
      (push (make-transcript-event :message :assistant content) events))
    (nconc (nreverse events)
           (replayed-tool-call-events (getf metadata :tool-calls)
                                      (getf metadata :tool-call-presentations)))))

(defmethod message-transcript-events ((message kli/session/log:tool-result-message))
  (let ((metadata (kli/session/log:message-metadata message)))
    (list (make-transcript-event
           :tool-result nil
           (or (kli/session/log:message-content message) "")
           :name (kli/session/log:tool-name message)
           :status (if (kli/session/log:tool-error-p message) :error :ok)
           :details (getf metadata :details)
           :presentation (getf metadata :presentation)))))

(defmethod session-entry-transcript-events ((entry kli/session/log:message-entry))
  (message-transcript-events (kli/session/log:entry-message entry)))

(defmethod session-entry-transcript-events
    ((entry kli/session/log:custom-message-entry))
  "Display-only notes replay as system lines. Entries marked not for display
   stay hidden, like their model-context filtering."
  (when (kli/session/log:entry-display-p entry)
    (let ((content (kli/session/log:message-content
                    (kli/session/log:entry-message entry))))
      (when (and (stringp content) (plusp (length content)))
        (list (make-transcript-event :notice nil content))))))

(defmethod session-entry-transcript-events
    ((entry kli/session/log:model-change-entry))
  (list (make-transcript-event
         :notice nil
         (format nil "Model: ~A/~A"
                 (kli/session/log:entry-provider entry)
                 (kli/session/log:entry-model-id entry)))))

(defmethod session-entry-transcript-events
    ((entry kli/session/log:option-change-entry))
  (let ((line (format-option-change-line
               (kli/session/log:entry-option-id entry)
               (kli/session/log:entry-option-value entry))))
    (when line
      (list (make-transcript-event :notice nil line)))))

(defmethod session-entry-transcript-events
    ((entry kli/session/log:compaction-entry))
  (list (make-transcript-event :notice nil "Session compacted.")))

(defun project-agent-error (event-type event mode-id buffer)
  "An errored turn outside the session retry loop (steer drains, worker
catch-ups) surfaces here -- the spinner stopping would otherwise be the only
sign. Supervised errors are skipped: the retry loop either retries them
(:agent/retry lines) or re-signals them into the :tui-error path, and
rendering both would show the same condition twice."
  (declare (ignore event-type mode-id buffer))
  (let ((payload (kli/event:event-payload event)))
    (unless (getf payload :supervised)
      (let ((message (getf payload :condition)))
        (make-transcript-event
         :notice nil
         (case (getf payload :category)
           (:network (format nil "Network error: ~A" message))
           ((:provider :config) (format nil "~A" message))
           (t (format nil "Internal error: ~A" message)))
         :status :error)))))

(defun project-tui-error (event-type event mode-id buffer)
  "Provider and config errors carry a self-describing report, so they render
verbatim. Network failures get a labeling prefix, and everything else stays an
internal error -- the category is stamped by the reify site via
condition-category."
  (declare (ignore event-type mode-id buffer))
  (let* ((payload (kli/event:event-payload event))
         (message (getf payload :message)))
    (make-transcript-event
     :notice nil
     (case (getf payload :category)
       (:network (format nil "Network error: ~A" message))
       ((:provider :config) (format nil "~A" message))
       (t (format nil "Internal error: ~A" message)))
     :status :error)))
