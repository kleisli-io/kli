(in-package #:task-mcp)

;;; MCP Tool Definitions
;;;
;;; Tools are defined using three macro tiers (see macros.lisp):
;;;   define-tool         — Plain MCP tool (no session context needed)
;;;   define-session-tool — Requires session context (auto-calls ensure-session-context)
;;;   define-task-tool    — Requires active task (auto-calls require-current-task)
;;;
;;; Additionally, the dispatch-level safety net in server.lisp calls
;;; ensure-session-context on every HTTP request before tool dispatch.

(defun today-prefix ()
  "Return today's date as YYYY-MM-DD string."
  (multiple-value-bind (s min h d m y)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore s min h))
    (format nil "~4,'0D-~2,'0D-~2,'0D" y m d)))

(defun date-prefixed-p (name)
  "Check if NAME already starts with a YYYY-MM-DD- date prefix."
  (and (>= (length name) 11)
       (digit-char-p (char name 0))
       (digit-char-p (char name 1))
       (digit-char-p (char name 2))
       (digit-char-p (char name 3))
       (char= (char name 4) #\-)
       (digit-char-p (char name 5))
       (digit-char-p (char name 6))
       (char= (char name 7) #\-)
       (digit-char-p (char name 8))
       (digit-char-p (char name 9))
       (char= (char name 10) #\-)))

(defun ensure-date-prefix (name)
  "Prepend today's date to NAME if it doesn't already have a date prefix."
  (if (date-prefixed-p name)
      name
      (format nil "~A-~A" (today-prefix) name)))

(defun validate-name-or-error (name)
  "Validate task NAME, returning error content if invalid, NIL if valid."
  (let ((result (task-validation:validate-task-name name)))
    (unless (task-validation:validation-result-valid-p result)
      (make-text-content
       (format nil "Error: Invalid task name '~A': ~A~%~%Suggestion: ~A"
               name
               (task-validation:validation-result-reason result)
               (task-validation:validation-result-suggestion result))))))

;;; ==========================================================================
;;; Multi-Depot Helpers (Coalgebraic Semantics)
;;; ==========================================================================
;;; Depot is EXPLICIT in task creation signature per coalgebraic model:
;;; QualifiedTaskID = Depot × LocalID (depot is part of task identity)

(defun resolve-depot-for-creation (explicit-depot task-name)
  "Resolve which depot to create a task in.
   EXPLICIT-DEPOT: User-provided depot name (nil = use default)
   TASK-NAME: The bare task name (already date-prefixed)

   Returns: (values depot-name qualified-id)
   Warns if cross-depot creation requested (sandbox may block).

   Depot resolution order:
   1. Explicit depot parameter (user override)
   2. Per-session depot from session context (set by register-pid from client CWD)
   3. Global *current-depot* (set at daemon startup — fallback)
   4. \"default\" (standalone mode)"
  (let* ((current (session-depot))
         (target-depot (or explicit-depot current "default"))
         (qualified-id (if (string= target-depot "default")
                           task-name
                           (task:qualify-task-id target-depot task-name))))
    ;; Validate depot exists if explicitly specified
    (when (and explicit-depot
               (> (hash-table-count task:*depot-tasks-roots*) 0)
               (not (gethash explicit-depot task:*depot-tasks-roots*)))
      (error "Unknown depot '~A'. Available depots: ~{~A~^, ~}"
             explicit-depot
             (alexandria:hash-table-keys task:*depot-tasks-roots*)))
    ;; Warn about cross-depot creation (sandbox limitation)
    (when (and explicit-depot
               current
               (not (string= explicit-depot current)))
      (format *error-output* "~&Note: Creating task in depot '~A' (current: '~A').~%"
              explicit-depot current))
    (values target-depot qualified-id)))

(defun resolve-task-id (id)
  "Resolve a task ID to a qualified ID.

   If ID is already qualified (contains ':'), returns it as-is.
   If unqualified, tries to find the task in:
     1. Per-session depot (from session context)
     2. Global *current-depot* (daemon startup fallback)
     3. Any known depot in *depot-tasks-roots*

   Returns qualified ID if found, otherwise qualifies with current depot."
  (if (task:qualified-id-p id)
      id  ; Already qualified
      ;; Unqualified - try to find where task exists
      (let ((current-depot (or (session-depot) "default")))
        ;; First try current depot
        (let ((candidate (if (string= current-depot "default")
                             id
                             (task:qualify-task-id current-depot id))))
          (if (probe-file (task:task-directory candidate))
              candidate
              ;; Search other depots
              (block found
                (maphash (lambda (depot-name tasks-root)
                           (declare (ignore tasks-root))
                           (unless (string= depot-name current-depot)
                             (let ((qid (task:qualify-task-id depot-name id)))
                               (when (probe-file (task:task-directory qid))
                                 (return-from found qid)))))
                         task:*depot-tasks-roots*)
                ;; Not found anywhere - default to current depot
                candidate))))))

(defun validate-cross-depot-edge (parent-id child-id edge-type)
  "Validate that edge type is appropriate for cross-depot relationship.

   :phase-of implies ownership - must be same depot.
   :related-to, :depends-on, :topic are reference edges - cross-depot OK.

   Signals error if :phase-of is used across depots."
  (multiple-value-bind (parent-depot parent-name) (task:parse-qualified-id parent-id)
    (declare (ignore parent-name))
    (multiple-value-bind (child-depot child-name) (task:parse-qualified-id child-id)
      (declare (ignore child-name))
      (when (and (not (string= parent-depot child-depot))
                 (string-equal edge-type "phase-of"))
        (error "Cannot create :phase-of edge across depots (~A -> ~A).~%~
                :phase-of implies ownership; use :related-to for cross-depot references."
               parent-depot child-depot))))
  t)

(define-tool task_create
    ((name string "Task name/ID")
     (description string "Task description" "")
     (depot string "Target depot (default: current depot)" nil))
  "Create a new task with directory and initial event.

Tasks are identified as depot:name (e.g., core:my-task). Specify depot to create
in a different depot (default: current depot)."
  ;; Validate name before creating
  (let ((validation-error (validate-name-or-error name)))
    (or validation-error
        (let ((full-name (ensure-date-prefix name)))
          (multiple-value-bind (target-depot qualified-id)
              (resolve-depot-for-creation
               (when (and depot (> (length depot) 0)) depot)
               full-name)
            (let* ((dir (task:ensure-task-directory qualified-id))
                   (prev (progn (ensure-session-context) *current-task-id*)))
              ;; Creation ≠ Selection: creating a task is a graph operation
              ;; (adds node), not a state operation (moves pointer).
              ;; Only task_set_current/task_bootstrap should change context.
              ;; Bootstrap exception: if no current task, adopt the new one.
              (unwind-protect
                   (progn
                     ;; Temporarily switch context for event emission
                     (setf *current-task-id* qualified-id)
                     (finalize-session-context)
                     (emit-event :task.create
                                 (list :name qualified-id
                                       :bare-name full-name
                                       :depot target-depot
                                       :description description))
                     (emit-event :session.join nil))
                ;; Restore: keep new only if we had no prior task (bootstrap)
                (setf *current-task-id* (or prev qualified-id))
                (finalize-session-context)
                (write-session-task-file)
                (when *http-mode* (save-session-context *session-id*)))
              (cleanup-stale-sessions)
              (make-text-content
               (format nil "Created task ~A at ~A" qualified-id dir))))))))

(define-session-tool task_fork
    ((name string "Child task name")
     (from string "Parent task ID (default: nil = top-level)" nil)
     (edge_type string "Edge type (default: phase-of)" nil)
     (description string "Task description / birth certificate" "")
     (depot string "Target depot for child task (default: current depot)" nil))
  "Create a subtask linked to a parent task.
from=nil creates top-level task. from=id creates subtask with edge.
Does NOT change current task. Use task_set_current or task_bootstrap to switch.

Phases (:phase-of) must be in the same depot as their parent.
Use :related-to for cross-depot references."
  ;; Validate name before creating
  (let ((validation-error (validate-name-or-error name)))
    (or validation-error
        (let* ((full-name (ensure-date-prefix name))
               ;; Resolve parent and edge type FIRST (needed for depot inheritance)
               (parent-id (let ((raw (or (when (and from (> (length from) 0)) from)
                                          *current-task-id*)))
                            (when raw (resolve-task-id raw))))
               (etype (or (when (and edge_type (> (length edge_type) 0)) edge_type)
                          "phase-of"))
               ;; Defense-in-depth: for phase-of edges, inherit depot from parent
               ;; rather than relying on session-depot() which is fragile under
               ;; concurrent sessions.  Matches TQ :fork handler (query.lisp).
               (explicit-depot (when (and depot (> (length depot) 0)) depot))
               (inherited-depot
                 (when (and (null explicit-depot)
                            (string-equal etype "phase-of")
                            parent-id
                            (task:qualified-id-p parent-id))
                   (multiple-value-bind (d b) (task:parse-qualified-id parent-id)
                     (declare (ignore b))
                     (unless (string= d "default") d)))))
          (multiple-value-bind (target-depot qualified-id)
              (resolve-depot-for-creation (or inherited-depot explicit-depot) full-name)
            ;; Validate cross-depot edge BEFORE creating directory (avoid orphans)
            (when parent-id
              (validate-cross-depot-edge parent-id qualified-id etype))
            (let* ((dir (task:ensure-task-directory qualified-id))
                   (prev *current-task-id*))
              ;; Fork never switches current task — unwind-protect ensures
              ;; restoration even if emit-event signals (matches TQ :fork
              ;; handler pattern at query.lisp:13-27).
              (unwind-protect
                   (progn
                     ;; Create child task
                     (setf *current-task-id* qualified-id)
                     (finalize-session-context)
                     (emit-event :task.create
                                 (append (list :name qualified-id
                                               :bare-name full-name
                                               :depot target-depot
                                               :description description)
                                         (when parent-id (list :parent parent-id))))
                     ;; Birth certificate observation
                     (when (> (length description) 0)
                       (emit-event :observation
                                   (list :text (format nil "Birth certificate: ~A" description))))
                     ;; Create edge on parent
                     (when parent-id
                       (setf *current-task-id* parent-id)
                       (finalize-session-context)
                       (emit-event :task.fork
                                   (list :child-id qualified-id :edge-type etype))))
                ;; Restore context — fork never switches current task
                (setf *current-task-id* prev)
                (finalize-session-context))
              (make-text-content
               (if parent-id
                   (format nil "Forked ~A from ~A (edge: ~A) at ~A" qualified-id parent-id etype dir)
                   (format nil "Created top-level task ~A at ~A" qualified-id dir)))))))))

(defun format-active-sessions-for-task (task-id)
  "Format active sessions working on TASK-ID.
   Returns a string like 'Sessions: 2 (7AB096EE, F864CFDB)' or empty string if none."
  (let* ((all-sessions (read-active-sessions 1))
         (bare (task-id-bare task-id))
         (matching (remove-if-not
                    (lambda (sess)
                      (let ((sess-task (getf sess :task-id)))
                        (and sess-task
                             (or (string= sess-task task-id)
                                 (string= sess-task bare)))))
                    all-sessions)))
    (if matching
        (format nil "Sessions: ~D (~{~A~^, ~})"
                (length matching)
                (mapcar (lambda (sess)
                          (let* ((sid (getf sess :session-id))
                                 (age (getf sess :age-minutes))
                                 (prefix (subseq sid 0 (min 8 (length sid)))))
                            (if (and age (> age 0))
                                (format nil "~A (~Dm ago)" prefix age)
                                prefix)))
                        matching))
        "")))

(define-session-tool task_get
    ((task_id string "Task ID (default: current)" nil))
  "Get computed state for a task.

Read-only — does not change current task or emit events.
Returns: task status, description, edges, observations, active sessions.
Use to peek at any task without switching context. See task_bootstrap to join a task."
  (let ((id (or (when (and task_id (> (length task_id) 0))
                  (resolve-task-id task_id))
                *current-task-id*)))
    (if (not id)
        (make-text-content "No task specified and no current task set.")
        (let* ((path (task:task-events-path id))
               (log (task:elog-load path))
               (events (reverse (task:event-log-events log))))
          (if events
              (let* ((state-str (format-task-state (task:compute-state events) events))
                     (sessions-str (format-active-sessions-for-task id))
                     (info-flow-str (handler-case
                                        (format-info-flow-summary id)
                                      (error () ""))))
                (make-text-content
                 (with-output-to-string (s)
                   (write-string state-str s)
                   (when (plusp (length sessions-str))
                     (terpri s) (write-string sessions-str s))
                   (when (plusp (length info-flow-str))
                     (write-string info-flow-str s)))))
              (make-text-content "No events found for task ~A" id))))))

(define-tool task_list
    ((grouped boolean "Group tasks by topic" nil)
     (limit integer "Max tasks to show (default 50, 0 for all)" nil))
  "List all tasks with their status."
  (let ((infos (task:get-cached-task-infos)))
    (make-text-content
     (if infos
         (if grouped
             (task:format-task-list-grouped infos)
             (format-task-list infos :limit (or limit 50)))
         "No tasks found."))))

(defun session-team-data ()
  "Return team metadata plist for current session, or NIL if not a teammate.
   HTTP mode: reads from session context registry (set by register-pid hook).
   STDIO mode: reads CLAUDE_CODE_* env vars directly (inherited from Claude)."
  (or (when *http-mode*
        (let ((ctx (bt:with-lock-held (*session-contexts-lock*)
                     (gethash *session-id* *session-contexts*))))
          (when (and ctx (ctx-team-name ctx))
            (list :team-name (ctx-team-name ctx)
                  :agent-name (ctx-agent-name ctx)
                  :agent-type (ctx-agent-type ctx)))))
      (let ((team (uiop:getenv "CLAUDE_CODE_TEAM_NAME")))
        (when (and team (plusp (length team)))
          (list :team-name team
                :agent-name (uiop:getenv "CLAUDE_CODE_AGENT_NAME")
                :agent-type (uiop:getenv "CLAUDE_CODE_AGENT_TYPE"))))))

(defun join-task (task-id)
  "Shared join logic: resolve ID, set current, emit session.join,
   write session file, cleanup. Returns resolved task-id on success.
   Multiple sessions can join the same task concurrently."
  (let* ((resolved-id (resolve-task-id task-id))
         (events-path (task:task-events-path resolved-id)))
    (unless (probe-file events-path)
      (error "Task ~A not found (no events.jsonl)" resolved-id))
    ;; Set current task + emit session.join (or session.team-join for teammates)
    (setf *current-task-id* resolved-id)
    (finalize-session-context)
    (let ((team-data (session-team-data)))
      (if team-data
          (emit-event :session.team-join team-data)
          (emit-event :session.join nil)))
    ;; Write session file for PostToolUse hooks (event tracking)
    (write-session-task-file)
    ;; Persist refreshed PID (write-session-task-file may update *claude-pid*)
    (when *http-mode* (save-session-context *session-id*))
    ;; Opportunistic cleanup of stale session files
    (cleanup-stale-sessions)
    resolved-id))

(define-session-tool task_set_current
    ((task_id string "Task ID to set as current"))
  "Set current task and emit session.join event.

Lightweight context switch — sets current task with minimal output.
Use for briefly switching to a child/phase task (e.g., to record observations)
then switching back. For full context when starting work, use task_bootstrap."
  (handler-case
      (let ((resolved (join-task task_id)))
        (make-text-content
         (format nil "Current task set to ~A (session ~A joined)"
                 resolved *session-id*)))
    (error (e)
      (make-text-content (format nil "~A" e)))))

(defun bootstrap-task (task-id)
  "Bootstrap full context for a task in one call.
   Sets *current-task-id*, emits session.join, returns composed context:
   task state + neighbors + enriched playbook query + latest handoff + swarm awareness.
   On error (task not found), returns available task list without side effects."
  (ensure-session-context)
  (let ((task-id (handler-case (join-task task-id)
                   (error (e)
                     (return-from bootstrap-task (format nil "~A" e))))))
    ;; Compose context
    (let* ((warnings nil)
           (events-path (task:task-events-path task-id))
           (log (task:elog-load events-path))
           (events (reverse (task:event-log-events log)))
           (state (task:compute-state events))
           (graph (get-or-build-graph))
           (state-str (format-task-state state events))
           (neighbors (task:format-task-neighbors graph task-id))
           (enriched (task:compute-enriched-query task-id graph))
           ;; Swarm awareness: find related sessions
           (related-sessions (handler-case
                                 (find-related-sessions task-id)
                               (error ()
                                 (push "Swarm awareness: unavailable" warnings)
                                 nil)))
           (swarm-str (format-swarm-awareness task-id related-sessions))
           ;; Behavioral similarity: find sessions with similar fingerprints
           (similar-sessions nil)
           (similar-degraded nil)
           (_ (handler-case
                  (multiple-value-setq (similar-sessions similar-degraded)
                    (find-similar-sessions *session-id* :max-age-hours 1))
                (error ()
                  (push "Similar session detection: unavailable" warnings)
                  nil)))
           ;; Merge vector clocks from similar sessions for causal ordering
           (_ (handler-case
                  (when similar-sessions
                    (merge-vector-clocks-from-sessions
                     (mapcar (lambda (s) (getf s :session-id)) similar-sessions)))
                (error () nil)))
           (similar-str (format-similar-sessions similar-sessions
                                                 :degraded similar-degraded))
           (handoffs-dir (format nil "~Ahandoffs/"
                                 (task:task-directory task-id)))
           (handoff-files (handler-case
                              (when (probe-file handoffs-dir)
                                (directory (merge-pathnames "*.md"
                                                           handoffs-dir)))
                            (error () nil)))
           (latest-handoff (when handoff-files
                             (namestring
                              (first (sort handoff-files #'string>
                                           :key #'namestring))))))
      (with-output-to-string (s)
        ;; Parseable line for PostToolUse hook (session-task-write)
        (format s "Current task set to ~A (session ~A joined)~%" task-id *session-id*)
        (write-string state-str s)
        (terpri s)
        (write-string neighbors s)
        (format s "~%Playbook query: ~A~%" enriched)
        (when latest-handoff
          (format s "~%Latest handoff: ~A~%" latest-handoff))
        ;; Append swarm awareness at the end
        (write-string swarm-str s)
        ;; Append behavioral similarity
        (when similar-sessions
          (terpri s)
          (write-string similar-str s))
        ;; Append swarm-aware plan frontier (if task has phases)
        (let ((frontier-str (handler-case
                                (format-swarm-frontier task-id *session-id*)
                              (error ()
                                (push "Plan frontier: unavailable" warnings)
                                ""))))
          (when (plusp (length frontier-str))
            (write-string frontier-str s)))
        ;; Append orphan detection (phases claimed by departed sessions)
        (let ((orphan-str (handler-case
                              (format-orphan-warnings task-id)
                            (error ()
                              (push "Orphan detection: unavailable" warnings)
                              ""))))
          (when (plusp (length orphan-str))
            (write-string orphan-str s)))
        ;; Append warnings section if any components failed
        (when warnings
          (format s "~%## Warnings~%~%")
          (dolist (w (nreverse warnings))
            (format s "- ~A~%" w)))))))

(define-session-tool task_bootstrap
    ((task_id string "Task ID"))
  "Bootstrap full task context in one call. Sets current task,
emits session.join, returns state + neighbors + playbook query + handoff.

Use when starting work on a task. Multiple sessions can work on the same task
simultaneously. Shows parallel sessions for coordination.
Returns: task state, graph neighbors, enriched playbook query, latest handoff,
swarm awareness (parallel sessions + similar work), and ready phases."
  (make-text-content (bootstrap-task task_id)))

(define-session-tool task_complete
    ((task_id string "Task ID (default: current)" nil))
  "Mark a task as completed. Completed tasks reject further mutations."
  (let ((id (or (when (and task_id (> (length task_id) 0))
                  (resolve-task-id task_id))
                *current-task-id*)))
    (unless id
      (error "No task specified and no current task set."))
    ;; Guard: check all descendants are complete before allowing completion
    (let ((incomplete (task:incomplete-descendants id)))
      (when incomplete
        (error "Cannot complete ~A: ~D incomplete subtask~:P:~%~{  - ~A (~A)~%~}"
               id (length incomplete)
               (loop for (cid . status) in incomplete
                     collect cid collect status))))
    (let ((prev *current-task-id*))
      (setf *current-task-id* id)
      (finalize-session-context)
      (unwind-protect
           (progn
             (emit-event :task.update-status (list :status "completed"))
             (let ((frontier-update (handler-case
                                        (completion-frontier-update id)
                                      (error () nil))))
               (make-text-content
                (format nil "Task ~A marked as completed.~@[~%~%~A~]"
                        id frontier-update))))
        (setf *current-task-id* prev)
        (finalize-session-context)))))

(define-session-tool task_reopen
    ((task_id string "Task ID (default: current)" nil))
  "Reopen a completed task, allowing mutations again."
  (let ((id (or (when (and task_id (> (length task_id) 0))
                  (resolve-task-id task_id))
                *current-task-id*)))
    (unless id
      (error "No task specified and no current task set."))
    (let ((prev *current-task-id*))
      (setf *current-task-id* id)
      (finalize-session-context)
      (unwind-protect
           (progn
             (emit-event :task.update-status (list :status "active"))
             (make-text-content
              (format nil "Task ~A reopened." id)))
        (setf *current-task-id* prev)
        (finalize-session-context)))))

(define-task-tool task_set_metadata
    ((key string "Metadata key (e.g. display-name, tags, phase, goals, depends-on)")
     (value string "Metadata value"))
  "Set a metadata key-value pair on the current task.
Convention keys: display-name, goals (JSON array), scope, phase, tags (comma-separated),
depends-on (JSON array of task IDs), enables (JSON array), related-to (JSON array)."
  (emit-event :task.set-metadata (list :key key :value value))
  (make-text-content
   (format nil "Metadata set: ~A = ~A" key value)))

;; artifact tool removed - file tracking via PostToolUse hook instead

(define-task-tool observe
    ((text string "Observation text"))
  "Record an observation for the current task."
  (emit-event :observation (list :text text))
  (make-text-content
   (format nil "Observation recorded (~D chars)" (length text))))

(define-task-tool spawn
    ((name string "Child task name")
     (reason string "Why this subtask is needed" ""))
  "Spawn a child task from the current task.
Uses parent's depot for the child (phase-of semantics)."
  ;; Validate name before creating
  (let ((validation-error (validate-name-or-error name)))
    (or validation-error
        (let* ((full-name (ensure-date-prefix name))
               ;; Defense-in-depth: inherit depot from parent task ID directly
               ;; instead of relying on session-depot() fallback chain.
               ;; Matches TQ :fork handler pattern (query.lisp).
               (parent-depot
                 (when (task:qualified-id-p *current-task-id*)
                   (multiple-value-bind (d b) (task:parse-qualified-id *current-task-id*)
                     (declare (ignore b))
                     (unless (string= d "default") d)))))
          (multiple-value-bind (target-depot qualified-id)
              (resolve-depot-for-creation parent-depot full-name)
            (let ((child-dir (task:ensure-task-directory qualified-id))
                  (parent *current-task-id*))
              ;; Create child task
              (setf *current-task-id* qualified-id)
              (finalize-session-context)
              (emit-event :task.create
                          (list :name qualified-id
                                :bare-name full-name
                                :depot target-depot
                                :description reason
                                :parent parent))
              ;; Record fork edge in parent
              (setf *current-task-id* parent)
              (finalize-session-context)
              (emit-event :task.fork
                          (list :child-id qualified-id :edge-type "phase-of"))
              (make-text-content
               (format nil "Spawned child task ~A at ~A" qualified-id child-dir))))))))

(define-task-tool task_link
    ((target_id string "Target task ID")
     (edge_type string "Edge type (e.g. depends-on, related-to, blocks)"))
  "Create typed edge from current task to target."
  (let ((resolved-target (resolve-task-id target_id)))
    ;; Validate cross-depot edge semantics
    (validate-cross-depot-edge *current-task-id* resolved-target edge_type)
    (emit-event :task.link
                (list :target-id resolved-target :edge-type edge_type))
    (make-text-content
     (format nil "Linked ~A -> ~A (edge: ~A)" *current-task-id* resolved-target edge_type))))

(define-task-tool task_sever
    ((target_id string "Target task ID")
     (edge_type string "Edge type to sever"))
  "Sever edge from current task to target."
  (let ((resolved-target (resolve-task-id target_id)))
    (emit-event :task.sever
                (list :target-id resolved-target :edge-type edge_type))
    (make-text-content
     (format nil "Severed ~A -> ~A (edge: ~A)" *current-task-id* resolved-target edge_type))))

(define-task-tool task_reclassify
    ((target_id string "Target task ID")
     (old_type string "Current edge type")
     (new_type string "New edge type"))
  "Change edge type from current task to target."
  (let ((resolved-target (resolve-task-id target_id)))
    ;; Validate new edge type for cross-depot semantics
    (validate-cross-depot-edge *current-task-id* resolved-target new_type)
    (emit-event :task.reclassify
                (list :target-id resolved-target :old-type old_type :new-type new_type))
    (make-text-content
     (format nil "Reclassified ~A -> ~A: ~A -> ~A"
             *current-task-id* resolved-target old_type new_type))))

(define-session-tool task_claim
    ((task_id string "Task ID to claim (default: current)" nil))
  "Claim task for current session.

Emits session.claim to record exclusive ownership in the event log.
Use for phases that need single-owner semantics (e.g., implementation
that modifies shared files). Multiple sessions can join a task without
claiming — claim is optional exclusive coordination.

Also use to take over orphaned phases — when bootstrap shows
'Orphaned Phases', claim the phase to continue the departed session's work."
  (let ((id (or (when (and task_id (> (length task_id) 0))
                  (resolve-task-id task_id))
                *current-task-id*)))
    (unless id (error "No task specified and no current task set."))
    ;; Set current task, persist to session file, and emit claim event.
    ;; The claim is recorded in the event log as a LWW-Register update,
    ;; visible to other sessions via compute-state for coordination.
    (setf *current-task-id* id)
    (finalize-session-context)
    (emit-event :session.claim nil)
    (write-session-task-file)
    (when *http-mode* (save-session-context *session-id*))
    (make-text-content
     (format nil "Task ~A claimed by session ~A" id *session-id*))))

(define-session-tool task_release
    ((task_id string "Task ID to release (default: current)" nil))
  "Release claim on task.
   Clears task_id in session file so hooks know we're no longer in task context."
  (let ((id (or (when (and task_id (> (length task_id) 0))
                  (resolve-task-id task_id))
                *current-task-id*)))
    (unless id (error "No task specified and no current task set."))
    (let ((prev *current-task-id*))
      (setf *current-task-id* id)
      (finalize-session-context)
      (unwind-protect
           (progn
             (emit-event :session.release nil)
             ;; Clear task_id in session file for hook coordination
             (clear-session-task-file)
             (make-text-content
              (format nil "Task ~A released by session ~A" id *session-id*)))
        (setf *current-task-id* prev)
        (finalize-session-context)))))

(defun handoff-timestamp ()
  "Return (values iso-timestamp handoff-name-prefix).
   ISO: 2026-01-31T17:30:00Z, prefix: 2026-01-31_17-30-00."
  (multiple-value-bind (s min h d m y)
      (decode-universal-time (get-universal-time) 0)
    (let ((iso (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
                       y m d h min s)))
      (values iso
              (format nil "~A_~A"
                      (subseq iso 0 10)
                      (substitute #\- #\: (subseq iso 11 19)))))))

(defun handoff-path-for (task-id summary)
  "Compute handoff file path from task ID and summary."
  (multiple-value-bind (timestamp name-prefix) (handoff-timestamp)
    (values (format nil "~Ahandoffs/~A_~A.md"
                    (task:task-directory task-id)
                    name-prefix
                    (slugify summary))
            timestamp)))

(define-task-tool handoff
    ((summary string "Handoff summary"))
  "Generate a handoff document for the current task."
  (let* ((state (current-task-state))
         (path (task:task-events-path *current-task-id*))
         (log (task:elog-load path))
         (events (reverse (task:event-log-events log))))
    (multiple-value-bind (handoff-path timestamp)
        (handoff-path-for *current-task-id* summary)
      ;; Ensure directory
      (ensure-directories-exist handoff-path)
      ;; Emit handoff event (registers as artifact + observation in state machine)
      (emit-event :handoff.create (list :summary summary :path handoff-path))
      ;; Generate minimal markdown (caller may overwrite with richer content)
      (with-open-file (s handoff-path :direction :output :if-exists :supersede)
        (format s "---~%")
        (format s "date: ~S~%" timestamp)
        (format s "task: ~S~%" *current-task-id*)
        (format s "session: ~S~%" *session-id*)
        (format s "type: handoff~%")
        (format s "---~%~%")
        (format s "# Handoff: ~A~%~%" summary)
        (format s "## Task State~%~%")
        (format s "~A~%" (format-task-state state))
        (format s "## Recent Events~%~%")
        (let ((recent (subseq events (max 0 (- (length events) 10)))))
          (dolist (ev recent)
            (format s "- ~A~%" (format-event ev)))))
      ;; Return structured metadata so callers (e.g. /ace:handoff skill) can
      ;; use Write tool to overwrite with rich content at the returned path.
      (make-text-content
       (format nil "Handoff written to ~A~%~%~
                    path: ~A~%~
                    task: ~A~%~
                    task_dir: ~A~%~
                    timestamp: ~A~%~
                    session: ~A"
               handoff-path handoff-path *current-task-id*
               (task:task-directory *current-task-id*)
               timestamp *session-id*)))))

(define-session-tool timeline
    ((task_id string "Task ID (default: current)" nil)
     (limit integer "Number of recent events" 20))
  "Show recent events for the current task."
  (let ((id (or (when (and task_id (> (length task_id) 0))
                  (resolve-task-id task_id))
                *current-task-id*)))
    (if (not id)
        (make-text-content "No task specified and no current task set.")
        (let* ((path (task:task-events-path id))
               (log (task:elog-load path))
               (all-events (task:event-log-events log))
               ;; Events are newest-first; take the first N
               (recent (subseq all-events 0 (min limit (length all-events)))))
          (make-text-content "~A"
           (with-output-to-string (s)
             (format s "Timeline for ~A (~D events, showing ~D):~%~%"
                     id (length all-events) (length recent))
             (dolist (ev recent)
               (format s "  ~A~%" (format-event ev)))))))))

(define-session-tool obs_search
    ((query string "Natural language query for observation search")
     (k integer "Number of results to return" 10)
     (task_id string "Optional: index a specific task first" nil))
  "Search observations by meaning and keywords.
Returns ranked results from most to least relevant."
  ;; Optionally index a specific task before searching
  (when (and task_id (> (length task_id) 0))
    (index-task-observations task_id))
  ;; Also index current task if set
  (when *current-task-id*
    (index-task-observations *current-task-id*))
  (let ((results (obs-retrieve *obs-graph* query :k k)))
    (make-text-content (format-obs-results results))))

(define-session-tool enriched_retrieve
    ((task_id string "Task ID for graph-aware retrieval (default: current)" nil)
     (k integer "Number of results to return" 10))
  "Search for relevant observations using task context to improve results.
Finds observations from this task and related tasks."
  (let ((id (or (when (and task_id (> (length task_id) 0)) task_id)
                *current-task-id*)))
    (unless id
      (error "No task specified and no current task set."))
    (multiple-value-bind (results enriched-query alpha)
        (enriched-retrieve id :k k)
      (declare (ignore alpha))
      (make-text-content
       (format nil "~A~%---~%Query used: ~A"
               (format-obs-results results)
               enriched-query)))))

(define-tool obs_feedback
    ((text string "Observation text to update quality for")
     (outcome string "Feedback: 'success' or 'failure'"))
  "Record whether an observation was helpful or not. Improves future search ranking."
  (let* ((record (gethash text (obs-graph-records *obs-graph*))))
    (unless record
      (error "Observation not found in index. Index the task first with obs_search."))
    (let* ((outcome-kw (cond
                         ((string-equal outcome "success") :success)
                         ((string-equal outcome "failure") :failure)
                         (t (error "Outcome must be 'success' or 'failure', got: ~A" outcome))))
           (old-quality (obs-quality text))
           (new-quality (quality-update old-quality outcome-kw)))
      (setf (obs-quality text) new-quality)
      (let* ((text (obs-record-text record))
             (snippet (subseq text 0 (min 100 (length text))))
             (level (cond
                      ((< new-quality 0.3) "low reliability")
                      ((< new-quality 0.6) "moderate reliability")
                      (t "high reliability"))))
        (make-text-content
         (format nil "Feedback recorded (~A). This observation now has ~A and will rank ~A in future searches.~%Observation: ~A"
                 outcome level
                 (if (string-equal outcome "success") "higher" "lower")
                 snippet))))))

(define-task-tool task_patterns ()
  "Get pattern activation counts for the current task."
  (let* ((path (task:task-events-path *current-task-id*))
         (log (task:elog-load path))
         (events (reverse (task:event-log-events log)))
         (counts (make-hash-table :test 'equal)))
    ;; Count pattern.activate events
    (dolist (ev events)
      (when (eq (task:event-type ev) :pattern.activate)
        (let ((pattern-id (getf (task:event-data ev) :pattern-id)))
          (when pattern-id
            (incf (gethash pattern-id counts 0))))))
    (make-text-content
     (with-output-to-string (s)
       (format s "Pattern activations for ~A:~%~%" *current-task-id*)
       (if (> (hash-table-count counts) 0)
           (maphash (lambda (id count)
                      (format s "  ~A: ~D activations~%" id count))
                    counts)
           (format s "  No pattern activations recorded.~%"))))))

;;; ==========================================================================
;;; Emergent Coordination Signals: File Activity Queries
;;; ==========================================================================

(define-tool file_activity
    ((file_path string "File path to check for recent activity")
     (task_id string "Task ID to search (default: all active tasks)" nil)
     (hours number "Hours of history to search (default: 1)" nil))
  "Query who recently touched a file for conflict avoidance.

Use before editing shared files to check for concurrent work. Returns sessions
that touched the file so you can coordinate and avoid conflicts.

Use BEFORE editing when: (1) task_bootstrap showed parallel sessions or similar work,
(2) you're about to edit shared infrastructure files (default.nix, package.lisp, server.lisp),
(3) the file-conflict hook warned about a recent edit by another session."
  (let* ((max-hours (or hours 1))
         (tid (when (and task_id (> (length task_id) 0))
                (resolve-task-id task_id)))
         (activity (recent-activity-on-file file_path
                                            :task-id tid
                                            :max-age-hours max-hours)))
    (make-text-content
     (if activity
         (with-output-to-string (s)
           (format s "File: ~A~%Recent activity (~D sessions):~%"
                   file_path (length activity))
           (dolist (a activity)
             (format s "  - ~A on ~A (~Dm ago)~%"
                     (getf a :session)
                     (getf a :task)
                     (getf a :age-minutes))))
         (format nil "File: ~A~%No recent activity by other sessions."
                 file_path)))))

(define-tool check_conflicts
    ((file_paths string "Comma-separated file paths to check")
     (task_id string "Task ID to search (default: all active tasks)" nil))
  "Check multiple files for potential edit conflicts.

Use before a batch of edits to detect concurrent work. Returns files with
recent activity by other sessions. More efficient than calling file_activity
for each file individually.

Use when: (1) you're about to edit 3+ files and want a single conflict scan,
(2) starting a phase that touches files listed in another session's fingerprint,
(3) proactively before large refactors across shared modules."
  (let* ((paths (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                        (uiop:split-string file_paths :separator ",")))
         (tid (when (and task_id (> (length task_id) 0))
                (resolve-task-id task_id)))
         (conflicts (check-file-conflicts paths :task-id tid)))
    (make-text-content
     (if conflicts
         (with-output-to-string (s)
           (format s "Potential conflicts found:~%~%")
           (dolist (c conflicts)
             (format s "~A:~%" (car c))
             (dolist (a (cdr c))
               (format s "  - ~A (~Dm ago)~%"
                       (getf a :session)
                       (getf a :age-minutes)))
             (terpri s)))
         "No conflicts detected. Files are clear for editing."))))

(define-tool session_register_pid
    ((pid integer "Claude Code's process ID to register for this session"))
  "Register Claude Code's PID for session file correlation.
   Called by SessionStart hook so hooks can find session files by PPID.
   The session ID is obtained from the HTTP request's Mcp-Session-Id header."
  (let ((session-id (or *session-id* (current-http-session-id))))
    (if session-id
        (make-text-content (register-claude-pid session-id pid))
        (make-text-content "Error: No session ID available"))))

(define-tool status
    ((task_id string "Task ID (default: current)" nil))
  "One-shot task overview: current state, plan progress, recent activity, health.
The 'git status' equivalent for kli — shows where you are and what needs doing."
  (let ((id (or (when (and task_id (> (length task_id) 0))
                  (resolve-task-id task_id))
                *current-task-id*)))
    (cond
      ;; No task: show recent active tasks
      ((not id)
       (make-text-content
        (with-output-to-string (s)
          (format s "No current task. Use --task <id> or set a current task first.~%~%")
          (format s "Recent active tasks:~%")
          (let ((infos (task:get-cached-task-infos)))
            (when infos
              (let* ((event-sourced (remove-if-not
                                     (lambda (tk) (getf tk :has-events)) infos))
                     (sorted (sort (copy-list event-sourced) #'>
                                   :key (lambda (tk) (or (getf tk :latest-mod) 0))))
                     (top (subseq sorted 0 (min 5 (length sorted)))))
                (dolist (tk top)
                  (format s "  ~A (~(~A~))~%"
                          (or (getf tk :display-name) (getf tk :id))
                          (getf tk :status)))))))))
      ;; Task exists: show full status
      (t
       (let* ((path (task:task-events-path id))
              (log (task:elog-load path))
              (events (reverse (task:event-log-events log)))
              (state (when events (task:compute-state events))))
         (if (not state)
             (make-text-content (format nil "Task ~A has no events." id))
             (make-text-content
              (with-output-to-string (s)
                ;; Header
                (let* ((meta (task:task-state-metadata state))
                       (display-name (crdt:lwwm-get meta "display-name"))
                       (desc (crdt:lww-value (task:task-state-description state)))
                       (status (crdt:lww-value (task:task-state-status state)))
                       (obs-count (crdt:gs-count (task:task-state-observations state)))
                       (sess-count (length (crdt:gs-members
                                            (task:task-state-sessions state)))))
                  (format s "~A~%" (or display-name id))
                  (when display-name
                    (format s "  ~A~%" id))
                  (format s "  ~A | ~D observations | ~D sessions~%"
                          status obs-count sess-count)
                  (when (and desc (> (length desc) 0))
                    (let ((end (or (position #\Newline desc) (length desc))))
                      (format s "  ~A~%" (subseq desc 0 (min 120 end))))))
                ;; Plan progress
                (let ((children (handler-case (task:task-children id)
                                  (error () nil))))
                  (when children
                    (let* ((states-ht (task:load-child-states children))
                           (frontier (handler-case (task:plan-frontier id)
                                       (error () nil)))
                           (frontier-set (make-hash-table :test 'equal))
                           (done-count 0))
                      (when frontier
                        (dolist (f frontier) (setf (gethash f frontier-set) t)))
                      (dolist (c children)
                        (let ((cstate (gethash c states-ht)))
                          (when (and cstate
                                     (string= "completed"
                                              (crdt:lww-value
                                               (task:task-state-status cstate))))
                            (incf done-count))))
                      (format s "~%Plan: ~D/~D phases done~@[, ~D ready~]~%"
                              done-count (length children)
                              (when (and frontier (plusp (length frontier)))
                                (length frontier)))
                      (dolist (c children)
                        (let* ((cstate (gethash c states-ht))
                               (cstatus (if cstate
                                            (or (crdt:lww-value
                                                 (task:task-state-status cstate))
                                                "?")
                                            "?"))
                               (short-name
                                 (multiple-value-bind (depot bare)
                                     (task:parse-qualified-id c)
                                   (declare (ignore depot))
                                   bare)))
                          (format s "  ~A ~A~%"
                                  (cond ((string= cstatus "completed") "[done]")
                                        ((gethash c frontier-set) "[READY]")
                                        (t (format nil "[~A]" cstatus)))
                                  short-name))))))
                ;; Recent observations (last 5)
                (let ((obs-events (remove-if-not
                                   (lambda (ev)
                                     (eq (task:event-type ev) :observation))
                                   events)))
                  (when obs-events
                    (let ((recent (last obs-events 5)))
                      (format s "~%Recent observations:~%")
                      (dolist (ob (reverse recent))
                        (let ((text (getf (task:event-data ob) :text)))
                          (format s "  [~A] ~A~%"
                                  (format-relative-time
                                   (task:event-timestamp ob))
                                  (if (> (length text) 120)
                                      (format nil "~A..."
                                              (subseq text 0 117))
                                      text)))))))
                ;; Health summary
                (handler-case
                    (let ((health (task:task-health-report)))
                      (when (and health (search "WARNING" health))
                        (format s "~%Health: warnings detected~%")))
                  (error ()))))))))))
