(in-package #:task-mcp-tests)

;;; ==========================================================================
;;; Fixtures
;;; ==========================================================================

(defmacro with-ephemeral-session ((&key (task-id "ephemeral-probe-task")) &body body)
  "Execute BODY with session globals bound to harmless ephemeral values.
   TASK-ID does not need to exist on disk because every test in this file
   exercises a rejection path that errors before elog-append-event runs."
  `(let ((*http-mode* nil)
         (*current-task-id* ,task-id)
         (*session-vc* (crdt:make-vector-clock))
         (*session-id* "task-mcp-tests"))
     ,@body))

(defmacro with-temp-tasks-root ((var) &body body)
  "Bind task:*tasks-root* to a fresh scratch directory, execute BODY,
   and remove the directory on exit."
  `(let ((,var (uiop:ensure-directory-pathname
                (format nil "~Atask-mcp-tests-~A-~A/"
                        (uiop:temporary-directory)
                        (get-universal-time)
                        (random 1000000)))))
     (ensure-directories-exist ,var)
     (unwind-protect
          (let ((task:*tasks-root* ,var))
            ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun signals-error-containing-p (thunk substring)
  "Run THUNK and return the error condition string if it signalled an
   error whose printed form contains SUBSTRING; NIL otherwise."
  (handler-case
      (progn (funcall thunk) nil)
    (error (c)
      (let ((msg (princ-to-string c)))
        (when (search substring msg) msg)))))

;;; ==========================================================================
;;; resolve-task-id (Layer 2a)
;;; ==========================================================================

(test resolve-task-id-rejects-nil
  (is (signals-error-containing-p (lambda () (resolve-task-id nil)) "nil")))

(test resolve-task-id-rejects-empty
  (is (signals-error-containing-p (lambda () (resolve-task-id "")) "empty")))

(test resolve-task-id-accepts-bare-id
  (is (string= "2026-04-15-foo" (resolve-task-id "2026-04-15-foo"))))

(test resolve-task-id-strips-depot-prefix
  (is (string= "2026-04-15-foo" (resolve-task-id "kleisli:2026-04-15-foo"))))

;;; ==========================================================================
;;; check-required-fields (Layer 3 pure helper)
;;; ==========================================================================

(test check-required-fields-unlisted-event-passes
  (is (null (check-required-fields :session.join nil)))
  (is (null (check-required-fields :tool.call '(:tool "x")))))

(test check-required-fields-task-create
  (is (null (check-required-fields :task.create '(:name "x"))))
  (is (equal '(:name) (check-required-fields :task.create '(:name nil))))
  (is (equal '(:name) (check-required-fields :task.create nil))))

(test check-required-fields-task-link
  (is (null (check-required-fields :task.link
                                   '(:target-id "x" :edge-type "phase-of"))))
  (is (equal '(:target-id)
             (check-required-fields :task.link
                                    '(:target-id nil :edge-type "phase-of"))))
  (is (equal '(:edge-type)
             (check-required-fields :task.link
                                    '(:target-id "x" :edge-type nil))))
  (is (equal '(:target-id :edge-type)
             (check-required-fields :task.link '(:target-id nil :edge-type nil)))))

(test check-required-fields-task-sever
  (is (null (check-required-fields :task.sever
                                   '(:target-id "x" :edge-type "depends-on"))))
  (is (equal '(:target-id)
             (check-required-fields :task.sever
                                    '(:target-id nil :edge-type "depends-on")))))

(test check-required-fields-task-fork
  (is (null (check-required-fields :task.fork
                                   '(:child-id "x" :edge-type "phase-of"))))
  (is (equal '(:child-id)
             (check-required-fields :task.fork
                                    '(:child-id nil :edge-type "phase-of")))))

(test check-required-fields-observation
  (is (null (check-required-fields :observation '(:text "hi"))))
  (is (equal '(:text) (check-required-fields :observation '(:text nil)))))

(test check-required-fields-set-metadata-key-required-value-exempt
  (is (null (check-required-fields :task.set-metadata
                                   '(:key "k" :value "v"))))
  ;; nil :value is legitimate (clearing semantic) — not flagged
  (is (null (check-required-fields :task.set-metadata
                                   '(:key "k" :value nil))))
  ;; nil :key is rejected
  (is (equal '(:key)
             (check-required-fields :task.set-metadata '(:key nil :value "v")))))

(test check-required-fields-exhaustive-required-fields-covered
  "Every entry in *event-required-fields* must reject a data plist
   whose required fields are all nil."
  (dolist (entry *event-required-fields*)
    (let* ((type (car entry))
           (required (cdr entry))
           (nil-plist (loop for f in required collect f collect nil)))
      (is (equal required (check-required-fields type nil-plist))
          "event ~A should flag all required fields when all are nil" type))))

;;; ==========================================================================
;;; emit-event gate (Layer 3, end-to-end)
;;; ==========================================================================

(test emit-event-rejects-link-nil-target
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (emit-event :task.link '(:target-id nil :edge-type "phase-of")))
         "TARGET-ID"))))

(test emit-event-rejects-sever-nil-target
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (emit-event :task.sever '(:target-id nil :edge-type "depends-on")))
         "TARGET-ID"))))

(test emit-event-rejects-fork-nil-child
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (emit-event :task.fork '(:child-id nil :edge-type "phase-of")))
         "CHILD-ID"))))

(test emit-event-rejects-observation-nil-text
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (emit-event :observation '(:text nil)))
         "TEXT"))))

(test emit-event-rejects-set-metadata-nil-key
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (emit-event :task.set-metadata '(:key nil :value "v")))
         "KEY"))))

(test emit-event-rejects-missing-required-field
  "Missing a required field (not just nil-valued) is also rejected."
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (emit-event :task.link '(:target-id "x")))
         "EDGE-TYPE"))))

;;; ==========================================================================
;;; tq-require-non-nil and tq-mutation-handler (Layer 2b)
;;; ==========================================================================

(test tq-require-non-nil-signals-on-nil
  (is (signals-error-containing-p
       (lambda () (tq-require-non-nil :observe :text nil))
       "nil or empty")))

(test tq-require-non-nil-signals-on-empty
  (is (signals-error-containing-p
       (lambda () (tq-require-non-nil :observe :text ""))
       "nil or empty")))

(test tq-require-non-nil-passes-on-nonempty
  (is (null (tq-require-non-nil :observe :text "hello"))))

(test tq-mutation-observe-rejects-nil
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (tq-mutation-handler "probe-id" :observe nil))
         "TEXT"))))

(test tq-mutation-set-meta-rejects-nil-key
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (tq-mutation-handler "probe-id" :set-meta nil "v"))
         "KEY"))))

(test tq-mutation-link-rejects-nil-target
  "Rejection routes through resolve-task-id (Layer 2a)."
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (tq-mutation-handler "probe-id" :link nil :phase-of))
         "identifier is nil"))))

(test tq-mutation-sever-rejects-nil-target
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (tq-mutation-handler "probe-id" :sever nil :depends-on))
         "identifier is nil"))))

(test tq-mutation-fork-rejects-nil-local-name
  (with-ephemeral-session ()
    (is (signals-error-containing-p
         (lambda () (tq-mutation-handler "probe-id" :fork nil "desc"))
         "LOCAL-NAME"))))

;;; ==========================================================================
;;; validate-required-string (Layer 1 helper)
;;; ==========================================================================

(test validate-required-string-rejects-nil
  (let ((r (validate-required-string "text" nil)))
    (is (not (null r)))
    (is (search "text is required" (mcp-framework::text-content-text r)))))

(test validate-required-string-rejects-empty
  (let ((r (validate-required-string "target_id" "")))
    (is (not (null r)))
    (is (search "target_id is required" (mcp-framework::text-content-text r)))))

(test validate-required-string-accepts-valid
  (is (null (validate-required-string "text" "hello"))))

;;; ==========================================================================
;;; Direct MCP tools (Layer 1, via call-tool)
;;; ==========================================================================

(defun call-with-args (tool-name alist)
  "Invoke TOOL-NAME through the MCP framework with ALIST as argument map."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k . v) in alist do (setf (gethash k h) v))
    (call-tool tool-name h)))

(defun text-of (content)
  "Extract the text string from a text-content JSON form."
  (cdr (assoc "text" (content-to-json content) :test #'string=)))

(test direct-tool-observe-rejects-nil-text
  (with-ephemeral-session ()
    (let ((r (call-with-args "observe" '(("text" . nil)))))
      (is (search "text is required" (text-of r))))))

(test direct-tool-observe-rejects-empty-text
  (with-ephemeral-session ()
    (let ((r (call-with-args "observe" '(("text" . "")))))
      (is (search "text is required" (text-of r))))))

(test direct-tool-task-link-rejects-nil-target
  (with-ephemeral-session ()
    (let ((r (call-with-args "task_link"
                             '(("target_id" . nil)
                               ("edge_type" . "phase-of")))))
      (is (search "target_id is required" (text-of r))))))

(test direct-tool-task-link-rejects-nil-edge-type
  (with-ephemeral-session ()
    (let ((r (call-with-args "task_link"
                             '(("target_id" . "x")
                               ("edge_type" . nil)))))
      (is (search "edge_type is required" (text-of r))))))

(test direct-tool-task-sever-rejects-empty-target
  (with-ephemeral-session ()
    (let ((r (call-with-args "task_sever"
                             '(("target_id" . "")
                               ("edge_type" . "depends-on")))))
      (is (search "target_id is required" (text-of r))))))

;;; ==========================================================================
;;; task_create ergonomics — Creation ≠ Selection response shape
;;; ==========================================================================

(test task-create-bootstrap-adopts-and-says-so
  "When no current task is set, the new task is adopted and the response
   says 'Adopted as current (bootstrap)' without naming a prior task."
  (with-temp-tasks-root (root)
    (declare (ignore root))
    (let ((*http-mode* nil)
          (*current-task-id* nil)
          (*session-vc* (crdt:make-vector-clock))
          (*session-id* "task-mcp-tests"))
      (let* ((r (call-with-args "task_create"
                                '(("name" . "bootstrap-probe")
                                  ("description" . "test"))))
             (msg (text-of r)))
        (is (search "Adopted as current (bootstrap)" msg))
        (is (not (search "unchanged" msg)))))))

(test task-create-nonbootstrap-names-prev
  "When a current task is already set, the new task does NOT become
   current; response names the unchanged task and points at
   task_set_current."
  (with-temp-tasks-root (root)
    (declare (ignore root))
    (let ((*http-mode* nil)
          (*current-task-id* "2026-04-15-existing")
          (*session-vc* (crdt:make-vector-clock))
          (*session-id* "task-mcp-tests"))
      (let* ((r (call-with-args "task_create"
                                '(("name" . "nonbootstrap-probe")
                                  ("description" . "test"))))
             (msg (text-of r)))
        (is (search "Current task unchanged (2026-04-15-existing)" msg))
        (is (search "task_set_current" msg))
        (is (not (search "Adopted as current" msg)))))))
