;;; MCP Framework - Tool Evolution Handlers
;;; Opt-in MCP handlers for hot-swapping tool behavior at runtime

(in-package #:mcp-framework)

;;; Evolution hooks (for validation/auth)

(defvar *before-evolution-hooks* nil
  "List of functions called before tool evolution.
   Each function receives (tool-name new-behavior reason).
   Signal an error to abort the evolution.")

(defun add-before-evolution-hook (hook)
  "Add a hook to validate/authorize tool evolution.
   HOOK receives (tool-name new-behavior reason) and can signal to abort."
  (pushnew hook *before-evolution-hooks*))

(defun remove-before-evolution-hook (hook)
  "Remove an evolution hook."
  (setf *before-evolution-hooks* (remove hook *before-evolution-hooks*)))

(defun clear-evolution-hooks ()
  "Remove all evolution hooks."
  (setf *before-evolution-hooks* nil))

(defun run-evolution-hooks (tool-name new-behavior reason)
  "Run all before-evolution hooks. Any hook can signal to abort."
  (dolist (hook *before-evolution-hooks*)
    (funcall hook tool-name new-behavior reason)))

;;; Behavior parsing

(defun parse-behavior-spec (params)
  "Parse behavior from either code string or function reference.
   Supports:
   - 'behavior': Lambda expression as string, evaluated
   - 'behaviorFunction': Existing function name as string, looked up"
  (cond
    ((gethash "behavior" params)
     (let ((code (gethash "behavior" params)))
       (handler-case
           (eval (read-from-string code))
         (error (e)
           (error 'invalid-params
                  :data (format nil "Invalid behavior code: ~A" e))))))
    ((gethash "behaviorFunction" params)
     (let ((fn-name (gethash "behaviorFunction" params)))
       (handler-case
           (symbol-function (read-from-string fn-name))
         (error (e)
           (error 'invalid-params
                  :data (format nil "Function not found: ~A" e))))))
    (t
     (error 'invalid-params
            :data "Must provide 'behavior' (code string) or 'behaviorFunction' (function name)"))))

;;; MCP Handlers

(defun handle-tools-inspect (params)
  "Handle tools/inspect request - return tool metadata."
  (let ((name (gethash "name" params)))
    (unless name
      (error 'invalid-params :data "Missing tool name"))
    (let ((tool (get-tool name)))
      (unless tool
        (error 'tool-not-found :tool-name name))
      (let ((info (funcall tool :inspect)))
        `(("name" . ,(getf info :mcp-name))
          ("documentation" . ,(or (getf info :documentation) ""))
          ("versions" . ,(getf info :versions))
          ("callCount" . ,(getf info :call-count))
          ("lastCalled" . ,(getf info :last-called))
          ("createdAt" . ,(getf info :created-at))
          ("hasBehaviorSource" . ,(if (getf info :behavior-source) t yason:false)))))))

(defun handle-tools-evolve (params)
  "Handle tools/evolve request - hot-swap tool behavior."
  (let ((name (gethash "name" params))
        (reason (gethash "reason" params "no reason given")))
    (unless name
      (error 'invalid-params :data "Missing tool name"))
    (let ((tool (get-tool name)))
      (unless tool
        (error 'tool-not-found :tool-name name))
      (let ((new-behavior (parse-behavior-spec params)))
        ;; Run validation hooks (can abort by signaling)
        (run-evolution-hooks name new-behavior reason)
        ;; Perform evolution - result is (:EVOLVED :VERSIONS N)
        (let ((result (funcall tool :evolve new-behavior reason)))
          `(("evolved" . ,name)
            ("reason" . ,reason)
            ("versions" . ,(second (member :versions result)))))))))

(defun handle-tools-devolve (params)
  "Handle tools/devolve request - rollback to previous behavior."
  (let ((name (gethash "name" params)))
    (unless name
      (error 'invalid-params :data "Missing tool name"))
    (let ((tool (get-tool name)))
      (unless tool
        (error 'tool-not-found :tool-name name))
      ;; Result is (:DEVOLVED :VERSIONS N) or (:ERROR "msg")
      (let ((result (funcall tool :devolve)))
        (if (eq (first result) :error)
            (error 'invalid-params :data (second result))
            `(("devolved" . ,name)
              ("versions" . ,(second (member :versions result)))))))))

(defun handle-tools-history (params)
  "Handle tools/history request - return evolution log."
  (let ((name (gethash "name" params)))
    (unless name
      (error 'invalid-params :data "Missing tool name"))
    (let ((tool (get-tool name)))
      (unless tool
        (error 'tool-not-found :tool-name name))
      (let ((history (get-pandoric tool 'behavior-history)))
        `(("name" . ,name)
          ("history" . ,(coerce
                         (mapcar (lambda (entry)
                                   `(("timestamp" . ,(first entry))
                                     ("reason" . ,(or (second entry) ""))
                                     ("hasBehaviorSource" .
                                      ,(if (ignore-errors
                                             (function-lambda-expression (third entry)))
                                           t yason:false))))
                                 (reverse history))
                         'vector)))))))

;;; Opt-in registration

(defvar *evolution-handlers-enabled* nil
  "Whether evolution handlers are currently registered.")

(defun enable-tool-evolution-handlers ()
  "Register MCP handlers for tool evolution (inspect/evolve/devolve/history).

   WARNING: This enables code evaluation via tools/evolve. Only enable in
   trusted environments or add validation via *before-evolution-hooks*.

   Registers:
   - tools/inspect: Get tool metadata (versions, call count, etc.)
   - tools/evolve: Hot-swap tool behavior (accepts code string or function name)
   - tools/devolve: Rollback to previous behavior
   - tools/history: Get evolution log with timestamps and reasons"
  (unless *evolution-handlers-enabled*
    (register-mcp-handler "tools/inspect" #'handle-tools-inspect)
    (register-mcp-handler "tools/evolve" #'handle-tools-evolve)
    (register-mcp-handler "tools/devolve" #'handle-tools-devolve)
    (register-mcp-handler "tools/history" #'handle-tools-history)
    (setf *evolution-handlers-enabled* t))
  :enabled)

(defun disable-tool-evolution-handlers ()
  "Unregister tool evolution handlers."
  (when *evolution-handlers-enabled*
    (remhash "tools/inspect" *mcp-handlers*)
    (remhash "tools/evolve" *mcp-handlers*)
    (remhash "tools/devolve" *mcp-handlers*)
    (remhash "tools/history" *mcp-handlers*)
    (setf *evolution-handlers-enabled* nil))
  :disabled)
