;;;; tool-call.lisp - PostToolUse hook handler for tool tracking
;;;;
;;;; Records :tool.call events via task-mcp HTTP endpoint for behavioral
;;;; fingerprinting. Also detects domain from edited file paths for
;;;; playbook session state (merged from playbook-post-tool).

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Tool Argument Extraction
;;; ---------------------------------------------------------------------------

(defun extract-relevant-args (tool-name tool-input)
  "Extract relevant arguments from TOOL-INPUT for fingerprinting.
   Returns a hash-table or NIL."
  (when tool-input
    (let ((args (make-hash-table :test #'equal)))
      (cond
        ;; File-based tools: capture file_path (normalized)
        ((member tool-name '("Read" "Edit" "Write") :test #'string=)
         (let ((path (normalize-file-path (gethash "file_path" tool-input))))
           (when path (setf (gethash "file_path" args) path))))

        ;; Search tools: capture pattern and path (normalized, stored as file_path)
        ((member tool-name '("Grep" "Glob") :test #'string=)
         (let ((pattern (gethash "pattern" tool-input))
               (path (normalize-file-path (gethash "path" tool-input))))
           (when pattern (setf (gethash "pattern" args) pattern))
           (when path (setf (gethash "file_path" args) path))))

        ;; Bash: capture command (truncated)
        ((string= tool-name "Bash")
         (let ((cmd (gethash "command" tool-input)))
           (when cmd
             (setf (gethash "command" args)
                   (if (> (length cmd) 100)
                       (concatenate 'string (subseq cmd 0 100) "...")
                       cmd)))))

        ;; Task: capture agent type and description
        ((string= tool-name "Task")
         (let ((agent-type (gethash "subagent_type" tool-input))
               (desc (gethash "description" tool-input)))
           (when agent-type (setf (gethash "subagent_type" args) agent-type))
           (when desc (setf (gethash "description" args) desc))))

        ;; LSP: capture operation and file (normalized)
        ((string= tool-name "LSP")
         (let ((op (gethash "operation" tool-input))
               (path (normalize-file-path (gethash "filePath" tool-input))))
           (when op (setf (gethash "operation" args) op))
           (when path (setf (gethash "file_path" args) path)))))

      (if (> (hash-table-count args) 0) args nil))))

;;; ---------------------------------------------------------------------------
;;; HTTP Client for task-mcp
;;; ---------------------------------------------------------------------------

(defun call-tool-call-endpoint (session-id tool-name tool-input)
  "Call task-mcp's /tool/call HTTP endpoint. Returns T on success."
  (handler-case
      (let* ((url (format nil "http://localhost:~D/tool/call?session-id=~A&tool=~A"
                          *task-mcp-port*
                          (quri:url-encode session-id)
                          (quri:url-encode tool-name)))
             (args (extract-relevant-args tool-name tool-input))
             (body (when args
                     (let ((ht (make-hash-table :test #'equal)))
                       (setf (gethash "args" ht) args)
                       (with-output-to-string (s)
                         (yason:encode ht s))))))
        (multiple-value-bind (response-body status-code)
            (if body
                (dex:post url
                          :content body
                          :headers '(("Content-Type" . "application/json"))
                          :connect-timeout 2
                          :read-timeout 2)
                (dex:post url
                          :connect-timeout 2
                          :read-timeout 2))
          (declare (ignore response-body))
          (and status-code (< status-code 300))))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; Playbook Domain Detection (merged from playbook-post-tool)
;;; ---------------------------------------------------------------------------

(defun record-file-domain (file-path input)
  "Detect domain from FILE-PATH and record to session playbook state."
  (when file-path
    (let ((domain (detect-domain-from-path file-path)))
      (when domain
        (let ((domains-file (session-file input "playbook" "domains.txt")))
          (when domains-file
            (append-line-unique domains-file domain)))))))

;;; ---------------------------------------------------------------------------
;;; Handler Entry Point
;;; ---------------------------------------------------------------------------

(defun tool-call-handler (input)
  "PostToolUse handler: record tool.call event and detect domains."
  (let ((tool-name (jref input "tool_name"))
        (cwd (jref input "cwd")))
    (when (and tool-name cwd)
      ;; Tool call tracking via task-mcp
      (let* ((coord-root (depot:coordination-root-from cwd))
             (session-info (when coord-root (read-session-file coord-root)))
             (tool-input (gethash "tool_input" input)))
        (when session-info
          (let ((task-session-id (gethash "session_id" session-info)))
            (when task-session-id
              (call-tool-call-endpoint task-session-id tool-name tool-input))))
        ;; Domain detection from file edits
        (when (member tool-name '("Edit" "Write") :test #'string=)
          (let ((file-path (when tool-input (gethash "file_path" tool-input))))
            (record-file-domain file-path input))))))
  (empty-response))
