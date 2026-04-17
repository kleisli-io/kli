;;;; kli-hook Tests - session-task-write handler
;;;;
;;;; Tests for the tool-name gate added to SESSION-TASK-WRITE-HANDLER
;;;; so that PARSE-TASK-RESPONSE only runs for the two tools whose
;;;; response format is canonically "Current task set to X (session Y joined)".

(in-package :kli-hook.tests)
(in-suite :session-task-write)

;;; ---------------------------------------------------------------------------
;;; session-task-write-tool-p
;;; ---------------------------------------------------------------------------

(test tool-p-accepts-task-set-current
  "Accepts the qualified MCP name for task_set_current."
  (is-true (kli-hook::session-task-write-tool-p
            "mcp__task__task_set_current")))

(test tool-p-accepts-task-bootstrap
  "Accepts the qualified MCP name for task_bootstrap."
  (is-true (kli-hook::session-task-write-tool-p
            "mcp__task__task_bootstrap")))

(test tool-p-accepts-bare-names
  "Substring match tolerates unqualified names (defensive)."
  (is-true (kli-hook::session-task-write-tool-p "task_set_current"))
  (is-true (kli-hook::session-task-write-tool-p "task_bootstrap")))

(test tool-p-rejects-unrelated-tools
  "Rejects other task-mcp tools whose responses could accidentally
   contain the 'to X (session Y joined)' shape."
  (dolist (name '("mcp__task__task_get"
                  "mcp__task__task_list"
                  "mcp__task__observe"
                  "mcp__task__task_create"
                  "mcp__task__task_fork"
                  "mcp__task__task_claim"
                  "mcp__task__task_release"
                  "mcp__task__task_complete"
                  "mcp__task__timeline"
                  "mcp__task__task_query"
                  "Read" "Write" "Bash" "Edit"))
    (is-false (kli-hook::session-task-write-tool-p name)
              "~A must NOT match the write gate" name)))

(test tool-p-rejects-nil
  "Handles nil tool-name gracefully."
  (is-false (kli-hook::session-task-write-tool-p nil)))

(test tool-p-rejects-empty
  "Handles empty tool-name gracefully."
  (is-false (kli-hook::session-task-write-tool-p "")))

;;; ---------------------------------------------------------------------------
;;; Handler gate behavior (structural)
;;; ---------------------------------------------------------------------------

(defun make-parseable-response-text (task-id session-id)
  "Build a response string that matches PARSE-TASK-RESPONSE's substring
   patterns, so we can verify that the handler gate blocks writes even
   when the response would parse cleanly."
  (format nil "Current task set to ~A (session ~A joined)"
          task-id session-id))

(defun make-write-input (tool-name tool-response cwd)
  "Build a synthetic PostToolUse input hash-table."
  (let ((input (make-ht "tool_name" tool-name
                         "cwd" cwd)))
    (when tool-response
      (setf (gethash "tool_response" input) tool-response))
    (setf (gethash "tool_input" input) (make-ht))
    input))

(defmacro with-handler-stubs ((&key (coord-root "/fake/coord-root")
                                    parse-counter
                                    write-counter
                                    delete-counter)
                              &body body)
  "Stub DEPOT:COORDINATION-ROOT-FROM, KLI-HOOK::PARSE-TASK-RESPONSE,
   CLAUDE-SESSION:WRITE-SESSION-TASK-FILE, and
   CLAUDE-SESSION:DELETE-SESSION-TASK-FILE for the duration of BODY so
   handler tests can observe call counts without touching the real
   filesystem or the git-root discovery path (which fails in the Nix
   build sandbox)."
  `(let ((coord-root-orig (fdefinition 'depot:coordination-root-from))
         (parse-orig      (fdefinition 'kli-hook::parse-task-response))
         (write-orig      (fdefinition 'claude-session:write-session-task-file))
         (delete-orig     (fdefinition 'claude-session:delete-session-task-file)))
     (unwind-protect
          (progn
            (setf (fdefinition 'depot:coordination-root-from)
                  (lambda (path) (declare (ignore path)) ,coord-root))
            ,@(when parse-counter
                `((setf (fdefinition 'kli-hook::parse-task-response)
                        (lambda (text)
                          (declare (ignore text))
                          (incf ,parse-counter)
                          (values nil nil)))))
            ,@(when write-counter
                `((setf (fdefinition 'claude-session:write-session-task-file)
                        (lambda (&rest args)
                          (declare (ignore args))
                          (incf ,write-counter)
                          nil))))
            ,@(when delete-counter
                `((setf (fdefinition 'claude-session:delete-session-task-file)
                        (lambda (&rest args)
                          (declare (ignore args))
                          (incf ,delete-counter)
                          t))))
            ,@body)
       (setf (fdefinition 'depot:coordination-root-from) coord-root-orig)
       (setf (fdefinition 'kli-hook::parse-task-response) parse-orig)
       (setf (fdefinition 'claude-session:write-session-task-file) write-orig)
       (setf (fdefinition 'claude-session:delete-session-task-file) delete-orig))))

(test handler-skips-parse-for-unrelated-tool
  "When tool-name is NOT task_set_current/task_bootstrap, the handler
   must not invoke PARSE-TASK-RESPONSE or WRITE-SESSION-TASK-FILE even
   if the response text looks parseable.  Regression for the pre-fix
   false-positive that rewrote claude-<pid>.json on unrelated tools."
  (let ((parse-calls 0)
        (write-calls 0))
    (with-handler-stubs (:parse-counter parse-calls :write-counter write-calls)
      (kli-hook::session-task-write-handler
       (make-write-input "mcp__task__observe"
                         (make-parseable-response-text "task-X" "HEX1")
                         "/tmp/fake-cwd")))
    (is (zerop parse-calls)
        "parse-task-response must not be called for unrelated tools")
    (is (zerop write-calls)
        "write-session-task-file must not be called for unrelated tools")))

(test handler-invokes-parse-for-task-set-current
  "Positive control: task_set_current WITH parseable response must
   reach PARSE-TASK-RESPONSE.  Regression against an over-tight gate."
  (let ((parse-calls 0)
        (write-calls 0))
    (with-handler-stubs (:parse-counter parse-calls :write-counter write-calls)
      (kli-hook::session-task-write-handler
       (make-write-input "mcp__task__task_set_current"
                         (make-parseable-response-text "task-Y" "HEX2")
                         "/tmp/fake-cwd")))
    (is (= 1 parse-calls)
        "parse-task-response must be called for task_set_current")
    (is (zerop write-calls)
        "write skipped because stubbed parse returned NIL values")))

(test handler-invokes-parse-for-task-bootstrap
  "Positive control: task_bootstrap also reaches PARSE-TASK-RESPONSE."
  (let ((parse-calls 0)
        (write-calls 0))
    (with-handler-stubs (:parse-counter parse-calls :write-counter write-calls)
      (kli-hook::session-task-write-handler
       (make-write-input "mcp__task__task_bootstrap"
                         (make-parseable-response-text "task-Z" "HEX3")
                         "/tmp/fake-cwd")))
    (is (= 1 parse-calls)
        "parse-task-response must be called for task_bootstrap")
    (is (zerop write-calls)
        "write skipped because stubbed parse returned NIL values")))

(test handler-task-release-path-still-fires
  "Regression: the task_release delete branch is upstream of the write
   gate and must still match on substring, unchanged by this fix."
  (let ((delete-calls 0))
    (with-handler-stubs (:delete-counter delete-calls)
      (kli-hook::session-task-write-handler
       (make-write-input "mcp__task__task_release"
                         "Released task foo (session HEX)"
                         "/tmp/fake-cwd")))
    (is (= 1 delete-calls)
        "delete-session-task-file must fire on task_release")))
