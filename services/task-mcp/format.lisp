(in-package #:task-mcp)

;;; Output formatting
;;; CQRS read projection: the LWW-Map stores uniform strings,
;;; the read projection interprets convention keys semantically.

(defun format-metadata-section (meta s)
  "Format metadata with semantic interpretation of convention keys.
   Convention keys (goals, depends-on, enables, tags, phase, scope)
   are rendered specially. Remaining keys shown in generic section."
  (let ((keys (crdt:lwwm-keys meta))
        (convention-keys '("display-name" "goals" "depends-on" "enables"
                           "related-to" "tags" "phase" "scope")))
    ;; Goals — checklist
    (let ((goals-str (crdt:lwwm-get meta "goals")))
      (when goals-str
        (let ((goals (try-parse-json-array goals-str)))
          (when goals
            (format s "Goals (~D):~%" (length goals))
            (dolist (g goals)
              (format s "  - ~A~%" g))))))
    ;; Graph relationships — depends-on, enables, related-to
    (dolist (dep-key '("depends-on" "enables" "related-to"))
      (let ((val (crdt:lwwm-get meta dep-key)))
        (when val
          (let ((ids (try-parse-json-array val)))
            (when ids
              (format s "~:(~A~): ~{~A~^, ~}~%" dep-key ids))))))
    ;; Tags — inline
    (let ((tags (crdt:lwwm-get meta "tags")))
      (when tags (format s "Tags: ~A~%" tags)))
    ;; Phase — inline
    (let ((phase (crdt:lwwm-get meta "phase")))
      (when phase (format s "Phase: ~A~%" phase)))
    ;; Scope — inline
    (let ((scope (crdt:lwwm-get meta "scope")))
      (when scope (format s "Scope: ~A~%" scope)))
    ;; Remaining non-convention keys
    (let ((extra (set-difference keys convention-keys :test #'string=)))
      (when extra
        (format s "Metadata:~%")
        (dolist (key extra)
          (format s "  ~A: ~A~%" key (crdt:lwwm-get meta key)))))))

(defun format-task-state (state &optional events)
  "Format task state as multi-line text (CQRS read projection).
   Interprets convention metadata keys semantically.
   When EVENTS provided, includes recent observation text,
   Markov efficiency (α), and organization indicator."
  (with-output-to-string (s)
    (let* ((meta (task:task-state-metadata state))
           (display-name (crdt:lwwm-get meta "display-name")))
      ;; Identity
      (format s "Task: ~A~%" (task:task-state-id state))
      (when display-name
        (format s "Display Name: ~A~%" display-name))
      (format s "Status: ~A~%" (crdt:lww-value (task:task-state-status state)))
      (format s "Description: ~A~%"
              (crdt:lww-value (task:task-state-description state)))
      ;; Sessions
      (let ((sessions (crdt:gs-members (task:task-state-sessions state))))
        (format s "Sessions: ~D (~{~A~^, ~})~%" (length sessions) sessions))
      ;; Enriched metadata (convention keys interpreted semantically)
      (format-metadata-section meta s)
      ;; Claim status
      (let ((claim (crdt:lww-value (task:task-state-claim state))))
        (when (and claim (> (length claim) 0))
          (format s "Claimed by: ~A~%" claim)))
      ;; Edges grouped by type
      (let ((edges (crdt:ors-members (task:task-state-edges state))))
        (when edges
          (let ((by-type (make-hash-table :test 'equal)))
            (dolist (edge-str edges)
              (let ((decoded (task:decode-edge edge-str)))
                (when decoded
                  (push (car decoded) (gethash (cdr decoded) by-type)))))
            (format s "Edges (~D):~%" (length edges))
            (maphash (lambda (etype targets)
                       (format s "  ~A: ~{~A~^, ~}~%"
                               (string-downcase (symbol-name etype))
                               (nreverse targets)))
                     by-type))))
      ;; Artifacts
      (let ((artifacts (crdt:ors-members (task:task-state-artifacts state))))
        (when artifacts
          (format s "Artifacts:~%")
          (dolist (a artifacts) (format s "  - ~A~%" a))))
      ;; Observations — last 10, newest first
      (let ((obs-count (crdt:gs-count (task:task-state-observations state))))
        (when (> obs-count 0)
          (format s "Observations: ~D~%" obs-count)
          (when events
            (let* ((all-obs (remove-if-not
                              (lambda (ev) (eq (task:event-type ev) :observation))
                              events))
                   (total (length all-obs))
                   (recent (reverse (last all-obs 10))))
              (dolist (ob recent)
                (let ((text (getf (task:event-data ob) :text)))
                  (format s "  [~A] ~A~%"
                          (format-relative-time (task:event-timestamp ob))
                          text)))
              (when (> total 10)
                (format s "  (~D earlier observations not shown — use obs_search to find them)~%"
                        (- total 10)))))))      ;; Event summary
      (when events
        (format s "Events: ~D total~%" (length events)))
      ;; Productivity (when events available)
      (when (and events (> (length events) 3))
        (handler-case
            (multiple-value-bind (alpha) (task:action-functor events state)
              (let* ((org (task:organization-indicator events state))
                     (bucket (cond
                               ((< alpha 0.25) "low — consider recording more observations")
                               ((< alpha 0.50) "moderate")
                               ((< alpha 0.70) "good")
                               (t "high"))))
                (format s "Productivity: ~A~%" bucket)
                (unless (getf org :organized)
                  (format s "Search readiness: ~D more observations needed for effective retrieval~%"
                          (max 0 (getf org :deficit))))))
          (error ()
            (format s "Productivity: unavailable (not enough data to compute)~%"))))
      ;; Graph context (enriched functor: own topic + 1-hop reference neighbors)
      (let ((ctx (handler-case
                     (task:format-graph-context
                      (get-or-build-graph)
                      (task:task-state-id state))
                   (error () nil))))
        (when ctx
          (format s "~A" ctx)))
      ;; Self-attention: if this is a phase (has parent), inject plan context
      (let ((plan-ctx (handler-case
                          (task:auto-plan-context (task:task-state-id state))
                        (error () nil))))
        (when plan-ctx
          (format s "~A" plan-ctx))))))

(defun format-relative-time (ut)
  "Human-readable relative time from universal-time integer."
  (let* ((now (get-universal-time))
         (diff (- now ut)))
    (cond
      ((< diff 60) "just now")
      ((< diff 3600) (format nil "~Dm ago" (floor diff 60)))
      ((< diff 86400) (format nil "~Dh ago" (floor diff 3600)))
      (t (format nil "~Dd ago" (floor diff 86400))))))

(defun slugify (str &optional (max-len 40))
  "Convert string to filename-safe slug: lowercase, non-alnum collapsed to single dash."
  (let* ((trimmed (subseq (string-downcase str) 0 (min max-len (length str))))
         (result (make-string-output-stream))
         (prev-dash nil))
    (loop for c across trimmed
          do (cond
               ((alphanumericp c)
                (write-char c result)
                (setf prev-dash nil))
               ((not prev-dash)
                (write-char #\- result)
                (setf prev-dash t))))
    (string-trim "-" (get-output-stream-string result))))

(defun format-tool-call-snippet (data)
  "Format tool.call event data into a human-readable snippet.
   DATA is a plist with :tool and optionally :args."
  (let ((tool (getf data :tool))
        (args (getf data :args)))
    (if (null args)
        tool  ; Just tool name if no args
        ;; Format based on what args are present
        (let ((parts (list tool))
              (file-path (getf args :file_path))
              (pattern (getf args :pattern))
              (search-path (getf args :path))
              (cmd (getf args :command))
              (agent (getf args :subagent_type)))
          ;; File path is most important
          (when file-path
            (push file-path parts))
          ;; Pattern for search tools
          (when pattern
            (push (format nil "pattern=~A"
                          (if (> (length pattern) 30)
                              (concatenate 'string (subseq pattern 0 30) "...")
                              pattern))
                  parts))
          ;; Path for search tools (if not file_path)
          (when (and search-path (not file-path))
            (push (format nil "in ~A" search-path) parts))
          ;; Command for Bash (truncated)
          (when cmd
            (push (format nil "$ ~A"
                          (if (> (length cmd) 40)
                              (concatenate 'string (subseq cmd 0 40) "...")
                              cmd))
                  parts))
          ;; Agent type for Task
          (when agent
            (push (format nil "agent=~A" agent) parts))
          ;; Join with " — "
          (format nil "~{~A~^ — ~}" (nreverse parts))))))

(defun format-event (ev)
  "Format a single event as one-line text with content snippet."
  (let* ((type (task:event-type ev))
         (data (task:event-data ev))
         (snippet (case type
                    (:observation
                     (let ((text (getf data :text)))
                       (when text
                         (if (> (length text) 200)
                             (format nil "~A..." (subseq text 0 200))
                             text))))
                    (:artifact.create (getf data :path))
                    (:task.create (getf data :name))
                    (:task.set-metadata
                     (format nil "~A = ~A"
                             (getf data :key) (getf data :value)))
                    (:task.update-status (getf data :status))
                    (:task.spawn (getf data :child-id))
                    (:task.fork
                     (format nil "~A (edge: ~A)"
                             (getf data :child-id) (getf data :edge-type)))
                    (:task.link
                     (format nil "-> ~A (edge: ~A)"
                             (getf data :target-id) (getf data :edge-type)))
                    (:task.sever
                     (format nil "x> ~A (edge: ~A)"
                             (getf data :target-id) (getf data :edge-type)))
                    (:task.reclassify
                     (format nil "~A: ~A -> ~A"
                             (getf data :target-id) (getf data :old-type) (getf data :new-type)))
                    (:handoff.create (getf data :path))
                    (:session.claim "claimed")
                    (:session.release "released")
                    (:tool.call (format-tool-call-snippet data))
                    (t nil))))
    (format nil "[~A] ~A ~A~@[ — ~A~]"
            (format-relative-time (task:event-timestamp ev))
            (task:event-type ev)
            (task:event-session ev)
            snippet)))

(defun format-task-list (tasks &key (limit 50))
  "Format cached task info plists as multi-line text.
   Works with plists from get-cached-task-infos (graph cache).
   Event-sourced tasks shown first, sorted by recency.
   LIMIT controls max tasks shown (0 = all)."
  (let* ((event-sourced (remove-if-not (lambda (tk) (getf tk :has-events)) tasks))
         (sorted (sort (copy-list event-sourced) #'>
                       :key (lambda (tk) (or (getf tk :latest-mod) 0))))
         (total (length sorted))
         (showing (if (and limit (plusp limit))
                      (subseq sorted 0 (min limit total))
                      sorted)))
    (with-output-to-string (s)
      (if (and limit (plusp limit) (> total limit))
          (format s "Tasks (~D of ~D, use --limit 0 for all):~%~%" limit total)
          (format s "Tasks (~D):~%~%" total))
      (dolist (tk showing)
        (let ((id (getf tk :id))
              (status (getf tk :status))
              (name (getf tk :display-name))
              (goals-count (getf tk :goal-count))
              (goals-done (getf tk :goals-done))
              (tags (getf tk :tags))
              (has-plan (getf tk :has-plan))
              (has-handoffs (getf tk :has-handoffs))
              (has-research (getf tk :has-research)))
          (format s "  ~A (~(~A~))~%" (or name id) status)
          (when name
            (format s "    ~A~%" id))
          ;; Artifacts line
          (let ((artifacts nil))
            (when has-research (push "research" artifacts))
            (when has-handoffs (push "handoffs" artifacts))
            (when has-plan (push "plan" artifacts))
            (when artifacts
              (format s "    ~{~A~^, ~}" artifacts)
              (when (and goals-count (plusp goals-count))
                (format s " | ~D/~D goals" goals-done goals-count))
              (format s "~%")))
          (when (and tags (listp tags))
            (format s "    [~{~A~^, ~}]~%" tags))
          (format s "~%")))
      (when *current-task-id*
        (format s "Current: ~A~%" *current-task-id*)))))
