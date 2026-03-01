(in-package #:task)

;;; ============================================================
;;; EDGE ENCODING
;;; Edges stored as "target-id::edge-type" strings in OR-Set.
;;; The "::" separator never appears in task names.
;;; ============================================================

(defun encode-edge (target-id edge-type)
  "Encode a typed edge as a string for OR-Set storage.
   EDGE-TYPE is a keyword (e.g. :phase-of, :depends-on)."
  (format nil "~A::~A" target-id (string-downcase (symbol-name edge-type))))

(defun decode-edge (edge-str)
  "Decode an OR-Set edge string into (target-id . :type-keyword)."
  (let ((pos (search "::" edge-str)))
    (when pos
      (cons (subseq edge-str 0 pos)
            (intern (string-upcase (subseq edge-str (+ pos 2))) :keyword)))))

(defun edge-targets (edges-or-set &optional type-filter)
  "Return list of target IDs from edges OR-Set.
   When TYPE-FILTER is a keyword, only return edges of that type."
  (let ((result nil))
    (dolist (member (ors-members edges-or-set))
      (let ((decoded (decode-edge member)))
        (when (and decoded
                   (or (null type-filter)
                       (eq (cdr decoded) type-filter)))
          (push (car decoded) result))))
    (nreverse result)))

(defparameter *child-bearing-edge-types* '(:phase-of :forked-from)
  "Edge types that define the structural fibration (parent->child decomposition).
   Plan(T) = coslice of T in the structural subgraph induced by these types.
   Lateral edges (depends-on, related-to, blocks, supersedes) cross fibers.")

;;; ============================================================
;;; TASK STATE
;;; Coalgebraic state: edges OR-Set replaces children G-Set.
;;; Claim LWW-Register for session locking.
;;; ============================================================

(defstruct task-state
  (id "" :type string)
  (description (make-lww-register) :type lww-register)
  (status (make-lww-register) :type lww-register)
  (edges (make-or-set) :type or-set)
  (claim (make-lww-register) :type lww-register)
  (sessions (make-g-set) :type g-set)
  (observations (make-g-set) :type g-set)
  (artifacts (make-or-set) :type or-set)
  (metadata (make-lww-map) :type lww-map)
  ;; Coalgebraic behavioral trace: files touched by Edit/Write operations
  (files-touched (make-or-set) :type or-set)
  ;; Coalgebraic behavioral trace: tools used (for swarm fingerprinting)
  (tool-calls (make-g-set) :type g-set))

(defun apply-task-event (state event)
  "Apply a single event to task state. Mutates STATE."
  (let ((data (event-data event))
        (ts (event-timestamp event))
        (session (event-session event))
        (base-tag (format nil "~A:~A" (event-session event) (event-id event))))
    (flet ((etag (element)
             "Per-element tag: base-tag + element for OR-Set uniqueness.
              Multiple ors-add calls in the same event get distinct tags,
              so ors-remove only tombstones the target element."
             (format nil "~A:~A" base-tag element)))
      (case (event-type event)
        (:task.create
         ;; Only the first task.create establishes identity.
         ;; Foreign task.create events (from HTTP session registry poisoning)
         ;; are silently ignored to prevent identity corruption.
         (cond
           ;; First task.create: establish identity
           ((string= (task-state-id state) "")
            (setf (task-state-id state) (or (getf data :name) (getf data :id) ""))
            (lww-set (task-state-description state)
                     (or (getf data :description) "") ts session)
            (lww-set (task-state-status state) "active" ts session)
            (gs-add (task-state-sessions state) session))
           ;; Same task name: allow (idempotent replay)
           ((string= (task-state-id state) (or (getf data :name) (getf data :id) ""))
            (lww-set (task-state-description state)
                     (or (getf data :description) "") ts session)
            (lww-set (task-state-status state) "active" ts session)
            (gs-add (task-state-sessions state) session))
           ;; Different task name: foreign event, skip
           (t nil)))
        (:task.update-status
         (lww-set (task-state-status state) (getf data :status) ts session))
        (:task.update-description
         (lww-set (task-state-description state)
                  (getf data :description) ts session))
        (:session.join
         (gs-add (task-state-sessions state) session))
        ;; Team-aware session join: records session + team metadata
        (:session.team-join
         (gs-add (task-state-sessions state) session)
         (let ((team-name (getf data :team-name))
               (agent-name (getf data :agent-name))
               (agent-type (getf data :agent-type)))
           (when team-name
             (lwwm-set (task-state-metadata state)
                       (format nil "team:~A" session)
                       (format nil "~A|~A|~A" team-name
                               (or agent-name "") (or agent-type ""))
                       ts session))))
        (:artifact.create
         (let ((path (getf data :path)))
           (ors-add (task-state-artifacts state) path (etag path))))
        (:artifact.delete
         (ors-remove (task-state-artifacts state) (getf data :path)))
        (:observation
         (let ((text (getf data :text)))
           (when text
             (gs-add (task-state-observations state) text))))
        ;; Legacy: map :task.spawn to phase-of edge
        (:task.spawn
         (let ((edge (encode-edge (getf data :child-id) :phase-of)))
           (ors-add (task-state-edges state) edge (etag edge))))
        ;; New: fork creates typed edge from parent to child
        (:task.fork
         (let ((edge (encode-edge (getf data :child-id)
                                  (or (when (getf data :edge-type)
                                        (intern (string-upcase (getf data :edge-type))
                                                :keyword))
                                      :phase-of))))
           (ors-add (task-state-edges state) edge (etag edge))))
        ;; New: link creates typed edge to existing task
        (:task.link
         (let ((edge (encode-edge (getf data :target-id)
                                  (intern (string-upcase (getf data :edge-type))
                                          :keyword))))
           (ors-add (task-state-edges state) edge (etag edge))))
        ;; New: sever removes edge
        ;; Try all forms for cross-compatibility:
        ;; - Fork events from tools store qualified IDs (core:task)
        ;; - Fork events from TQ mutations store bare IDs (task)
        ;; - Sever events may use either form
        (:task.sever
         (let* ((target (getf data :target-id))
                (edge-type (intern (string-upcase (getf data :edge-type)) :keyword)))
           (multiple-value-bind (depot bare-id) (parse-qualified-id target)
             ;; Always try the form given in event
             (ors-remove (task-state-edges state) (encode-edge target edge-type))
             ;; If target was qualified, also try bare form
             (unless (string= target bare-id)
               (ors-remove (task-state-edges state) (encode-edge bare-id edge-type)))
             ;; If target was bare, also try qualified form (using current depot)
             (when (and (string= target bare-id) *current-depot*)
               (let ((qualified (qualify-task-id *current-depot* bare-id)))
                 (ors-remove (task-state-edges state) (encode-edge qualified edge-type)))))))
        ;; New: reclassify = remove old type + add new type
        ;; Try all forms for cross-compatibility (same as sever)
        (:task.reclassify
         (let* ((target (getf data :target-id))
                (old-type (intern (string-upcase (getf data :old-type)) :keyword))
                (new-type (intern (string-upcase (getf data :new-type)) :keyword)))
           (multiple-value-bind (depot bare-id) (parse-qualified-id target)
             ;; Remove old edge - try the form given
             (ors-remove (task-state-edges state) (encode-edge target old-type))
             ;; If target was qualified, also try bare form
             (unless (string= target bare-id)
               (ors-remove (task-state-edges state) (encode-edge bare-id old-type)))
             ;; If target was bare, also try qualified form
             (when (and (string= target bare-id) *current-depot*)
               (let ((qualified (qualify-task-id *current-depot* bare-id)))
                 (ors-remove (task-state-edges state) (encode-edge qualified old-type))))
             ;; Add new edge with qualified form (forward-compatible)
             (let ((edge (encode-edge target new-type)))
               (ors-add (task-state-edges state) edge (etag edge))))))
        ;; New: session claim/release
        (:session.claim
         (lww-set (task-state-claim state) session ts session))
        (:session.release
         (lww-set (task-state-claim state) "" ts session))
        (:task.set-metadata
         (lwwm-set (task-state-metadata state)
                   (getf data :key) (getf data :value) ts session))
        (:handoff.create
         (let ((path (getf data :path)))
           (ors-add (task-state-artifacts state) path (etag path)))
         (gs-add (task-state-observations state)
                 (format nil "Handoff: ~A → ~A"
                         (getf data :summary) (getf data :path))))
        ;; Legacy: file.touch events from old hook (backward compat for replaying old events)
        (:file.touch
         (let ((path (getf data :path)))
           (when path
             (ors-add (task-state-files-touched state) path (etag path)))))
        ;; Coalgebraic tool tracking: record tool usage + file tracking for Edit/Write
        (:tool.call
         (let ((tool (getf data :tool)))
           (when tool
             (gs-add (task-state-tool-calls state) tool))
           ;; Extract file path from Edit/Write tool calls for files-touched tracking
           (when (member tool '("Edit" "Write") :test #'string=)
             (let ((path (getf (getf data :args) :file-path)))
               (when path
                 (ors-add (task-state-files-touched state) path (etag path)))))))))
  state))

(defun compute-state (events)
  "Compute task state from event list (oldest first)."
  (let ((state (make-task-state)))
    (dolist (ev events state)
      (apply-task-event state ev))))

;;; ============================================================
;;; SESSION DEPARTURE DETECTION
;;; Scan events.jsonl for session.leave events for swarm coordination.
;;; ============================================================

(defun session-has-left-p (task-id session-id)
  "Check if SESSION-ID has emitted a session.leave event for TASK-ID.
   Returns plist (:session :timestamp :reason) if found, NIL otherwise.

   Note: Scans full event log (not time-bounded) to catch any historical departure.
   Used by TQ orphaned-phases query and task-mcp swarm coordination."
  (let ((events-path (task-events-path task-id)))
    (when (probe-file events-path)
      (handler-case
          (with-open-file (s events-path)
            (loop for line = (read-line s nil)
                  while line
                  for data = (ignore-errors (yason:parse line))
                  when (and data
                            (string= (gethash "type" data) "session.leave")
                            (string= (gethash "session" data) session-id))
                  return (let* ((ts (gethash "timestamp" data))
                                (evt-data (gethash "data" data))
                                (reason (when evt-data (gethash "reason" evt-data))))
                           (list :session session-id
                                 :timestamp ts
                                 :reason (or reason "unknown")))))
        (error () nil)))))
