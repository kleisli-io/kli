;;;; claude-hooks - Response Builders
;;;;
;;;; Typed constructors for all Claude Code hook event output schemas.

(in-package :claude-hooks)

;;; ---------------------------------------------------------------------------
;;; PreToolUse Responses
;;; ---------------------------------------------------------------------------

(defun pre-tool-allow (&key reason context updated-input)
  "Allow tool use. Optionally add REASON, CONTEXT string, or UPDATED-INPUT hash-table."
  (let ((ht (make-ht "decision" "approve")))
    (when reason
      (setf (gethash "reason" ht) reason))
    (when context
      (setf (gethash "additionalContext" ht) context))
    (when updated-input
      (setf (gethash "updatedInput" ht) updated-input))
    ht))

(defun pre-tool-deny (&key reason context)
  "Deny/block tool use. Optionally add REASON and CONTEXT string."
  (let ((ht (make-ht "decision" "block")))
    (when reason
      (setf (gethash "reason" ht) reason))
    (when context
      (setf (gethash "additionalContext" ht) context))
    ht))

(defun pre-tool-ask (&key reason)
  "Escalate tool use to user confirmation. Optionally add REASON."
  (let ((ht (make-ht "decision" "ask")))
    (when reason
      (setf (gethash "reason" ht) reason))
    ht))

;;; ---------------------------------------------------------------------------
;;; PermissionRequest Responses
;;; ---------------------------------------------------------------------------

(defun permission-allow (&key updated-input)
  "Allow permission request. Optionally provide UPDATED-INPUT hash-table."
  (let ((ht (make-ht "behavior" "allow")))
    (when updated-input
      (setf (gethash "updatedInput" ht) updated-input))
    ht))

(defun permission-deny (&key message (interrupt nil interrupt-p))
  "Deny permission request with optional MESSAGE and INTERRUPT boolean.
When INTERRUPT is supplied (even as NIL/false), it is included in output."
  (let ((ht (make-ht "behavior" "deny")))
    (when message
      (setf (gethash "message" ht) message))
    (when interrupt-p
      (setf (gethash "interrupt" ht)
            (if interrupt t yason:false)))
    ht))

;;; ---------------------------------------------------------------------------
;;; Stop/SubagentStop Responses
;;; ---------------------------------------------------------------------------

(defun stop-continue (&key reason)
  "Force agent continuation with REASON. For Stop/SubagentStop hooks.
Output: {\"continue\":true,\"reason\":\"...\"}"
  (let ((ht (make-ht "continue" t)))
    (when reason
      (setf (gethash "reason" ht) reason))
    ht))

(defun stop-allow ()
  "Allow agent to stop. For Stop/SubagentStop hooks.
Returns NIL so no JSON is emitted (empty stdout = allow stop)."
  nil)

;;; ---------------------------------------------------------------------------
;;; Generic Responses
;;; ---------------------------------------------------------------------------

(defun context-response (text)
  "Inject bare additional context TEXT. Only for PreCompact hooks.
For other hooks, use the convenience wrappers: PROMPT-CONTEXT,
SESSION-START-CONTEXT, or POST-TOOL-CONTEXT."
  (make-ht "additionalContext" text))

(defun hook-context (event-name text)
  "Inject context TEXT wrapped in hookSpecificOutput for EVENT-NAME.
Required by UserPromptSubmit, SessionStart, and PostToolUse hooks which need
the nested {\"hookSpecificOutput\": {\"hookEventName\": ..., \"additionalContext\": ...}} schema."
  (make-ht "hookSpecificOutput"
            (make-ht "hookEventName" event-name
                     "additionalContext" text)))

(defun prompt-context (text)
  "Inject context TEXT for a UserPromptSubmit hook.
Shortcut for (hook-context \"UserPromptSubmit\" text)."
  (hook-context "UserPromptSubmit" text))

(defun session-start-context (text)
  "Inject context TEXT for a SessionStart hook.
Shortcut for (hook-context \"SessionStart\" text)."
  (hook-context "SessionStart" text))

(defun post-tool-context (text)
  "Inject context TEXT for a PostToolUse hook.
Shortcut for (hook-context \"PostToolUse\" text)."
  (hook-context "PostToolUse" text))

(defun block-response (&key reason)
  "Block action with optional REASON. For Stop, SubagentStop, UserPromptSubmit."
  (let ((ht (make-ht "decision" "block")))
    (when reason
      (setf (gethash "reason" ht) reason))
    ht))

(defun post-tool-block (&key reason context)
  "Signal bad tool result. Combines decision=block with hookSpecificOutput."
  (let ((ht (make-ht "decision" "block")))
    (when reason
      (setf (gethash "reason" ht) reason))
    (when context
      (setf (gethash "hookSpecificOutput" ht)
            (make-ht "additionalContext" context)))
    ht))

(defun empty-response ()
  "Silent success — empty JSON object."
  (make-ht))

(defun suppress-output (response)
  "Add suppressOutput:true to RESPONSE hash-table. Returns modified RESPONSE."
  (setf (gethash "suppressOutput" response) t)
  response)
