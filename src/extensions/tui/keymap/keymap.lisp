(in-package #:kli/tui/keymap)

(defparameter *default-keymap*
  (let ((h (make-hash-table :test #'equal)))
    (dolist (pair
             '(("ctrl+l" . :clear-screen)  ("ctrl+o" . :tool-output)
               ("enter" . :submit)
               ("ctrl+j" . :newline)  ("shift+enter" . :newline)
               ("backspace" . :backspace)
               ("left" . :move-char-left)   ("ctrl+b" . :move-char-left)
               ("right" . :move-char-right) ("ctrl+f" . :move-char-right)
               ("up" . :move-line-up)       ("ctrl+p" . :move-line-up)
               ("down" . :move-line-down)   ("ctrl+n" . :move-line-down)
               ("home" . :move-line-start)  ("ctrl+a" . :move-line-start)
               ("end" . :move-line-end)     ("ctrl+e" . :move-line-end)
               ("alt+b" . :move-word-left)  ("alt+left" . :move-word-left)  ("ctrl+left" . :move-word-left)
               ("alt+f" . :move-word-right) ("alt+right" . :move-word-right) ("ctrl+right" . :move-word-right)
               ("delete" . :delete-char-forward) ("ctrl+d" . :delete-char-forward)
               ("ctrl+u" . :delete-to-line-start) ("ctrl+k" . :delete-to-line-end)
               ("ctrl+w" . :delete-word-backward) ("alt+backspace" . :delete-word-backward)
               ("alt+d" . :delete-word-forward)   ("alt+delete" . :delete-word-forward)
               ("ctrl+z" . :undo)
               ("tab" . :insert-tab)  ("space" . :insert-space)
               ("insert" . :swallow) ("shift+tab" . :swallow)
               ("escape" . :abort)
               ("page-up" . :ignore) ("page-down" . :ignore)))
      (setf (gethash (car pair) h) (cdr pair)))
    h)
  "Base key-id string to action keyword table reproducing the built-in bindings.
One base table shared by all protocols, layered under per-protocol overrides
where an override wins over the base. App-level actions (:clear-screen,
:tool-output) are intercepted by the router via the route context. Submit,
newline, and backspace are normalized by the router to canonical form. :swallow
and :ignore are recognized but inert, never inserted as literal text.")

(defparameter +keymap-actions+
  '(:clear-screen :tool-output :submit :newline :backspace
    :move-char-left :move-char-right :move-line-up :move-line-down
    :move-line-start :move-line-end :move-word-left :move-word-right
    :delete-char-forward :delete-to-line-start :delete-to-line-end
    :delete-word-backward :delete-word-forward :insert-tab :insert-space
    :abort :swallow :ignore :undo :next-surface)
  "Actions a user config may target (load-time validation whitelist).
:next-surface ships with no default binding -- surface switching is the /tab
command's job; a chord is the user's own call.")

(defparameter +keymap-storage-key+ :kli/tui/keymap)

(defun protocol-keymap-overrides (protocol)
  "Per-protocol override layer (key-id string -> action keyword)."
  (ensure-protocol-storage protocol +keymap-storage-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun keymap-action (protocol key-id)
  "Resolve KEY-ID. The override layer wins over the base default, NIL if unbound."
  (when (stringp key-id)
    (multiple-value-bind (v present)
        (gethash key-id (protocol-keymap-overrides protocol))
      (if present v (gethash key-id *default-keymap*)))))

(defun keymap-recognized-p (protocol key-id)
  "T iff KEY-ID has any binding (incl. inert actions)."
  (and (keymap-action protocol key-id) t))

(defun register-keybinding (protocol key-id action)
  "Imperative override. ACTION is a keyword, NIL removes the override."
  (if action
      (setf (gethash key-id (protocol-keymap-overrides protocol)) action)
      (remhash key-id (protocol-keymap-overrides protocol)))
  key-id)

(defun unregister-keybinding (protocol key-id)
  (remhash key-id (protocol-keymap-overrides protocol))
  key-id)

(defun keymap-action-name (name)
  "User JSON action string -> known action keyword, or NIL (skip + warn)."
  (let ((kw (intern (string-upcase (substitute #\- #\_ name)) :keyword)))
    (and (member kw +keymap-actions+) kw)))

(defun load-keymap (protocol source)
  "SOURCE = pathname | json string | parsed jzon hash. Merges {\"ctrl+z\":\"undo\"}
   (optionally nested under \"keybindings\") into the override layer. Returns the
   count applied. Unknown action names are skipped (warned)."
  (let* ((json (etypecase source
                 (pathname (com.inuoe.jzon:parse (uiop:read-file-string source)))
                 (string   (if (uiop:file-exists-p source)
                               (com.inuoe.jzon:parse (uiop:read-file-string source))
                               (com.inuoe.jzon:parse source)))
                 (hash-table source)))
         (binds (or (gethash "keybindings" json) json))
         (n 0))
    (maphash (lambda (key-id action-name)
               (let ((action (keymap-action-name action-name)))
                 (cond (action (register-keybinding protocol key-id action) (incf n))
                       (t (warn "Unknown keybinding action ~S for ~S" action-name key-id)))))
             binds)
    n))
