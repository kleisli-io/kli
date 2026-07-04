(in-package #:kli/tui/style)

(defstruct (theme (:constructor %make-theme (name tokens)))
  "TOKENS is an equal hash from token-name string to a color or NIL."
  name
  tokens)

(defun load-theme (source)
  "SOURCE = pathname | json string | parsed jzon hash. Resolves colors+vars
into a flat name->(color|nil) table at load time. NIL token = terminal default."
  (let* ((json (etypecase source
                 (pathname (com.inuoe.jzon:parse (uiop:read-file-string source)))
                 (string   (if (uiop:file-exists-p source)
                               (com.inuoe.jzon:parse (uiop:read-file-string source))
                               (com.inuoe.jzon:parse source)))
                 (hash-table source)))
         (vars   (gethash "vars" json))
         (colors (gethash "colors" json))
         (tokens (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (setf (gethash k tokens)
                     (cond ((or (null v) (zerop (length v))) nil)
                           ((char= (char v 0) #\#) (hex->color v))
                           (t (hex->color (gethash v vars))))))
             colors)
    (%make-theme (gethash "name" json) tokens)))

(defun theme-token (theme name &optional default)
  (multiple-value-bind (v present) (gethash name (theme-tokens theme))
    (if present v default)))

(defun style (theme token text &key bg attrs (mode *color-mode*))
  "TOKEN is a JSON-verbatim string (e.g. \"error\", \"mdHeading\")."
  (style-span text :fg (theme-token theme token)
                   :bg (and bg (theme-token theme bg))
                   :attrs attrs :mode mode))

(defparameter +theme-token-names+
  '("accent" "border" "borderAccent" "borderMuted" "success" "error" "warning"
    "muted" "dim" "text" "thinkingText" "selectedBg" "userMessageBg"
    "userMessageText" "customMessageBg" "customMessageText" "customMessageLabel"
    "toolPendingBg" "toolSuccessBg" "toolErrorBg" "toolTitle" "toolOutput"
    "mdHeading" "mdLink" "mdLinkUrl" "mdCode" "mdCodeBlock" "mdCodeBlockBorder"
    "mdQuote" "mdQuoteBorder" "mdHr" "mdListBullet" "toolDiffAdded"
    "toolDiffRemoved" "toolDiffContext" "syntaxComment" "syntaxKeyword"
    "syntaxFunction" "syntaxVariable" "syntaxString" "syntaxNumber" "syntaxType"
    "syntaxOperator" "syntaxPunctuation" "thinkingOff" "thinkingMinimal"
    "thinkingLow" "thinkingMedium" "thinkingHigh" "thinkingXhigh" "bashMode")
  "Every token key the built-in palettes define.")

(defparameter +theme-registry-key+ :kli/tui/style/themes)
(defparameter +active-theme-key+   :kli/tui/style/active-theme)
(defparameter +theme-mode-key+     :kli/tui/style/theme-mode
  "Protocol-storage key holding the deferred theme intent: :auto resolves the
background at boot, :explicit pins the active theme, absent reads as :auto.")

(defun protocol-theme-registry (protocol)
  (ensure-protocol-storage protocol +theme-registry-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun register-theme (protocol theme)
  (setf (gethash (theme-name theme) (protocol-theme-registry protocol)) theme))

(defun unregister-theme (protocol name)
  (remhash name (protocol-theme-registry protocol)))

(defun find-theme (protocol name)
  (gethash name (protocol-theme-registry protocol)))

(defun list-themes (protocol)
  (loop for k being the hash-keys of (protocol-theme-registry protocol) collect k))

(defun set-active-theme (protocol name)
  (assert (find-theme protocol name) () "No theme named ~S registered." name)
  (setf (protocol-storage protocol +active-theme-key+) name))

(defun active-theme (protocol)
  (let ((name (protocol-storage protocol +active-theme-key+ "dark")))
    (or (find-theme protocol name) (find-theme protocol "dark")
        (error "No active theme registered."))))

(defclass theme-contribution (contribution)
  ((theme :initarg :theme :reader contribution-theme)))

(defun make-theme-contribution (&key name theme source)
  (make-instance 'theme-contribution :kind :theme :name name :theme theme :source source))

(defmethod install-contribution ((protocol extension-protocol) (c theme-contribution) context)
  (declare (ignore context))
  (register-theme protocol (contribution-theme c))
  (push c (protocol-installed-contributions protocol))
  c)

(defmethod retract-contribution ((protocol extension-protocol) (c theme-contribution) context)
  (declare (ignore context))
  (unregister-theme protocol (theme-name (contribution-theme c)))
  (setf (protocol-installed-contributions protocol)
        (remove c (protocol-installed-contributions protocol)))
  c)

(defcontribution-kind :theme (extension-id form)
  (destructuring-bind (_ name theme-form) form
    (declare (ignore _))
    `(make-theme-contribution
      :name ',(normalize-extension-id name)
      :theme ,theme-form
      :source ',extension-id)))
