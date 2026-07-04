(in-package #:kli/tui/transcript)

(defvar *transcript-counter* (make-id-counter))
(defvar *transcript-event-counter* (make-id-counter))
(defvar *transcript-view-counter* (make-id-counter))
(defvar *scrollback-renderer-counter* (make-id-counter))

(defun make-transcript-id ()
  (next-keyword-id "TUI-TRANSCRIPT" '*transcript-counter*))

(defun make-transcript-event-id ()
  (next-keyword-id "TUI-TRANSCRIPT-EVENT" '*transcript-event-counter*))

(defun make-transcript-view-id ()
  (next-keyword-id "TUI-TRANSCRIPT-VIEW" '*transcript-view-counter*))

(defun make-scrollback-renderer-id ()
  (next-keyword-id "TUI-SCROLLBACK-RENDERER" '*scrollback-renderer-counter*))

(defclass transcript-event (live-object)
  ((kind
    :initarg :kind
    :reader event-kind)
   (role
    :initarg :role
    :initform nil
    :reader event-role)
   (text
    :initarg :text
    :initform ""
    :accessor event-text)
   (name
    :initarg :name
    :initform nil
    :reader event-name)
   (status
    :initarg :status
    :initform nil
    :accessor event-status)
   (details
    :initarg :details
    :initform nil
    :reader event-details
    :documentation "Structured tool-result payload for the TUI view, separate from the model-visible text. NIL for tools without one.")
   (presentation
    :initarg :presentation
    :initform nil
    :reader event-presentation
    :documentation "Tool-owned presentation term for a :tool-call or :tool-result event: a tagged plist whose kind selects the renderer. NIL degrades to a generic box.")))

(defun make-transcript-event (kind role text &key id name status details
                                                  presentation)
  (make-instance 'transcript-event
                 :id (or id (make-transcript-event-id))
                 :kind kind
                 :role role
                 :text text
                 :name name
                 :status status
                 :details details
                 :presentation presentation))

(defclass transcript (live-object)
  ((events
    :initarg :events
    :initform '()
    :accessor transcript-events)
   (events-tail
    :initform nil
    :accessor transcript-events-tail
    :documentation "The last cons of EVENTS, kept so transcript-add-event
appends in constant time rather than copying the whole list each turn.")))

(defmethod initialize-instance :after ((transcript transcript) &key)
  (setf (transcript-events-tail transcript)
        (last (transcript-events transcript))))

(defun make-transcript (&key id protocol events)
  (make-instance 'transcript
                 :id (or id (make-transcript-id))
                 :protocol protocol
                 :events events))

(defun transcript-add-event (transcript event)
  (let ((cell (list event)))
    (if (transcript-events-tail transcript)
        (setf (cdr (transcript-events-tail transcript)) cell)
        (setf (transcript-events transcript) cell))
    (setf (transcript-events-tail transcript) cell))
  event)

(defun transcript-clear (transcript)
  (setf (transcript-events transcript) '()
        (transcript-events-tail transcript) nil)
  transcript)

(defun transcript-remove-last-user-event (transcript)
  "Drop the most recent :message/:user row and return its text, or NIL. The
   renderer reconciles the shrink on its next frame."
  (let* ((events (transcript-events transcript))
         (target (find-if (lambda (event)
                            (and (eq (event-kind event) :message)
                                 (eq (event-role event) :user)))
                          events :from-end t)))
    (when target
      (let ((remaining (remove target events)))
        (setf (transcript-events transcript) remaining
              (transcript-events-tail transcript) (last remaining)))
      (event-text target))))

(defclass transcript-view (live-object)
  ((transcript
    :initarg :transcript
    :accessor transcript-view-transcript)
   (editor
    :initarg :editor
    :accessor transcript-view-editor)
   (history
    :initarg :history
    :initform '()
    :accessor transcript-view-history)
   (history-tail
    :initform nil
    :accessor transcript-view-history-tail
    :documentation "The last cons of HISTORY, kept so record-history appends in
constant time rather than walking and copying the whole list each submit.")
   (history-position
    :initarg :history-position
    :initform nil
    :accessor transcript-view-history-position)
   (history-draft
    :initarg :history-draft
    :initform ""
    :accessor transcript-view-history-draft)
   (on-submit
    :initarg :on-submit
    :initform nil
    :accessor transcript-view-on-submit)))

(defmethod initialize-instance :after ((view transcript-view) &key)
  (setf (transcript-view-history-tail view)
        (last (transcript-view-history view))))

(declaim (ftype function submit-transcript-input))

(defun make-transcript-view (&key id protocol transcript editor (prompt "> ")
                                  on-submit)
  (let* ((transcript (or transcript (make-transcript :protocol protocol)))
         (view nil)
         (editor (or editor
                     (make-editor
                      :protocol protocol
                      :prompt prompt
                      :on-submit (lambda (value)
                                   (set-editor-value
                                    (transcript-view-editor view)
                                    "")
                                   (submit-transcript-input view value))))))
    (setf view
          (make-instance 'transcript-view
                         :id (or id (make-transcript-view-id))
                         :protocol protocol
                         :transcript transcript
                         :editor editor
                         :on-submit on-submit))
    view))

(defclass scrollback-renderer (live-object)
  ((transcript
    :initarg :transcript
    :accessor scrollback-renderer-transcript)
   (view
    :initarg :view
    :accessor scrollback-renderer-view)
   (terminal
    :initarg :terminal
    :accessor scrollback-renderer-terminal)
   (printed-events
    :initarg :printed-events
    :initform 0
    :accessor scrollback-renderer-printed-events)
   (frozen-stream-lines
    :initarg :frozen-stream-lines
    :initform 0
    :accessor scrollback-renderer-frozen-stream-lines
    :documentation "Open-event lines already scrolled into scrollback under overflow.")
   (streaming-event
    :initarg :streaming-event
    :initform nil
    :accessor scrollback-renderer-streaming-event
    :documentation "The commit boundary. While set, that event stays out of native scrollback and is drawn in the diffed bottom region. NIL means the region is the prompt alone.")
   (region-lines
    :initarg :region-lines
    :initform nil
    :accessor scrollback-renderer-region-lines
    :documentation "Caches the last drawn region.")
   (region-cursor-row
    :initarg :region-cursor-row
    :initform 0
    :accessor scrollback-renderer-region-cursor-row)
   (region-cursor-column
    :initarg :region-cursor-column
    :initform 0
    :accessor scrollback-renderer-region-cursor-column)
   (notice
    :initarg :notice
    :initform nil
    :accessor scrollback-renderer-notice
    :documentation "A transient hint line (e.g. the Esc-abort prompt) drawn just above the input prompt and below the streaming reply. Held here rather than as a :system transcript event so it clears on disarm or turn-end without leaving a permanent row in scrollback history.")
   (stream-render-cache
    :initform nil
    :accessor scrollback-renderer-stream-render-cache
    :documentation "(key . lines) for the open streaming event's last render, so spinner-tick frames between delta drains reuse it instead of re-parsing the whole accumulated reply. Cleared on finalize and reset.")
   (stream-md
    :initform nil
    :accessor scrollback-renderer-stream-md
    :documentation "An mdstream for the open assistant reply, so each delta renders incrementally against the committed prefix instead of re-parsing the whole accumulated markdown. Created lazily on first render, reset on a new streaming event, finalize, and reset.")
   (stream-hl
    :initform nil
    :accessor scrollback-renderer-stream-hl
    :documentation "An hl-stream for the open reply's open fenced code block, which mdstream cannot advance past. Each delta re-highlights only the open tail rather than re-scanning the whole growing block. Created lazily on first render, reset alongside stream-md.")
   (bottom-anchor-pending-p
    :initform nil
    :accessor scrollback-renderer-bottom-anchor-pending-p
    :documentation "Set by the screen-clear paths. The next frame jumps the cursor down so its content ends on the bottom row before drawing - the prompt reads as a chat input with history growing above - instead of drawing from the home position. A jump rather than newline scrolling, so nothing is pushed into native scrollback.")
   (behavior
    :initarg :behavior
    :accessor scrollback-renderer-behavior)))

(declaim (ftype function default-scrollback-render))

(defun make-scrollback-behavior (renderer-id)
  "Fault-policy is :escalate so a faulting render reaches the :render funnel, which owns recovery — skip frame and reset. Cell-level containment would corrupt the diff."
  (make-behavior-cell
   :id (list renderer-id :scrollback-renderer-behavior)
   :state '(:strategy :scrollback-render)
   :metadata '(:owner :tui-transcript)
   :capabilities '(:transcript/scrollback :behavior/hotpatch :behavior/state)
   :fault-policy :escalate
   :function #'default-scrollback-render))

(defun make-scrollback-renderer (transcript view terminal &key id protocol behavior)
  (let ((id (or id (make-scrollback-renderer-id))))
    (make-instance 'scrollback-renderer
                   :id id
                   :protocol protocol
                   :transcript transcript
                   :view view
                   :terminal terminal
                   :behavior (or behavior
                                 (make-scrollback-behavior id)))))
