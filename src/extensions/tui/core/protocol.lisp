(in-package #:kli/tui/core)

(defgeneric render-lines (view width)
  (:documentation "Render VIEW into a list of line specs at WIDTH columns."))

(defgeneric render-transcript-event (kind protocol event theme width)
  (:documentation
   "Render EVENT to display lines at WIDTH using THEME, eql-specialized on KIND."))

(defgeneric cursor-position (view width)
  (:documentation "Return (values ROW COL) for VIEW's cursor at WIDTH columns."))

(defgeneric handle-input (target input)
  (:documentation "Dispatch INPUT to TARGET (typically a view or screen frame)."))

(defgeneric handle-paste (view text)
  (:documentation "Deliver pasted TEXT to VIEW, returning the text actually accepted."))

(defgeneric dismiss-overlay (view)
  (:documentation "Close VIEW's transient overlay (e.g. a completion popup).
True when one was dismissed, NIL when there was nothing to close."))

(defgeneric invalidate (target)
  (:documentation "Mark TARGET as needing redraw."))

(defgeneric set-focused (view state)
  (:documentation "Update VIEW's focus to STATE (a generalized boolean)."))

(defgeneric children (view)
  (:documentation "Return VIEW's child views (a list)."))

(defgeneric add-child (view child)
  (:documentation "Append CHILD to VIEW's children."))

(defgeneric remove-child (view child)
  (:documentation "Remove CHILD from VIEW's children."))

(defgeneric clear-children (view)
  (:documentation "Remove all children from VIEW."))

(defgeneric submit-editor (editor)
  (:documentation "Submit EDITOR's current buffer as a user event."))

(defgeneric recode-tui-behavior (target &rest args)
  (:documentation
   "Hot-patch TARGET's TUI behavior. The default method delegates to
RECODE-BEHAVIOR when TARGET is itself a behavior-cell. Sibling
extensions specialize on their concrete TUI classes."))

(defgeneric write-terminal (terminal string)
  (:documentation "Write STRING to TERMINAL."))

(defgeneric terminal-size (terminal)
  (:documentation "Return (values ROWS COLS) for TERMINAL."))

(defgeneric render-frame (frame terminal &key force)
  (:documentation "Render FRAME into TERMINAL. FORCE bypasses dirty-line skipping."))

(defgeneric decode-input (decoder data)
  (:documentation
   "Decode the raw input chunk DATA against DECODER, returning a list of
input events. Sibling extensions specialize on their decoder classes."))

(defmethod recode-tui-behavior ((target behavior-cell) &rest args)
  (apply #'recode-behavior target args))

(defmethod dismiss-overlay (view)
  (declare (ignore view))
  nil)
