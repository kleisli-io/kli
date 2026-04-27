;;;; playbook tests - Session contract: get-or-error split + write-merge

(in-package :playbook.tests)
(in-suite :session-contract)

;;; -------------------------------------------------------------------------
;;; get-session-or-error: strict accessor for activation/feedback paths.
;;; -------------------------------------------------------------------------

(test record-feedback-on-evicted-session-signals-error-not-creates-virgin
  "record-feedback against an unknown session signals UNKNOWN-SESSION-ERROR
   instead of silently creating a virgin session."
  (let ((playbook::*session-states* (make-hash-table :test 'equal)))
    (signals unknown-session-error
      (record-feedback "evicted-sid" "pat-x" :helpful))
    ;; Hash table is still empty — no virgin session was inserted.
    (is (zerop (hash-table-count playbook::*session-states*)))))

(test record-activation-on-evicted-session-signals-error-not-creates-virgin
  "record-activation against an unknown session signals UNKNOWN-SESSION-ERROR
   instead of silently creating a virgin session."
  (let ((playbook::*session-states* (make-hash-table :test 'equal)))
    (signals unknown-session-error
      (record-activation "evicted-sid" "pat-x"))
    (is (zerop (hash-table-count playbook::*session-states*)))))

;;; -------------------------------------------------------------------------
;;; write-feedback-state-file guard: HTTP mode requires resolved Claude-sid
;;; -------------------------------------------------------------------------

(test feedback-write-blocked-when-claude-sid-unresolved
  "In HTTP mode with no resolved Claude-sid, write-feedback-state-file
   refuses to write rather than emitting an MCP-sid-keyed file the Stop
   hook cannot find."
  (let* ((dir (format nil "/tmp/test-fb-blocked-~a" (random 1000000)))
         (sid "mcp-sid-no-claude")
         (playbook::*session-states* (make-hash-table :test 'equal))
         (playbook::*http-mode* t))
    (unwind-protect
         (let ((session (get-or-create-session sid)))
           ;; Explicitly leave claude-session-id NIL.
           (is (null (playbook::session-state-claude-session-id session)))
           (record-activation sid "pat-a")
           (let ((result (write-feedback-state-file dir sid)))
             (is (null result))
             ;; And no file should have been created at the MCP-sid path.
             (is (null (probe-file (feedback-state-path dir sid))))))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

(test feedback-state-file-always-keyed-by-claude-sid-when-resolved
  "When Claude-sid is resolved, the on-disk file path uses claude-sid,
   not the MCP session id."
  (let* ((dir (format nil "/tmp/test-fb-keyed-~a" (random 1000000)))
         (mcp-sid "mcp-shell-1")
         (claude-sid "claude-shell-A")
         (playbook::*session-states* (make-hash-table :test 'equal))
         (playbook::*http-mode* t))
    (unwind-protect
         (let ((session (get-or-create-session mcp-sid)))
           (setf (playbook::session-state-claude-session-id session) claude-sid)
           (record-activation mcp-sid "pat-a")
           (let ((path (write-feedback-state-file dir mcp-sid)))
             (is (not (null path)))
             ;; File path must contain the claude-sid, not the mcp-sid.
             (is (search claude-sid (namestring path)))
             (is (not (search mcp-sid (namestring path))))
             ;; Stop hook reads via feedback-state-path keyed by claude-sid.
             (is (probe-file (feedback-state-path dir claude-sid)))
             (is (null (probe-file (feedback-state-path dir mcp-sid))))))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

;;; -------------------------------------------------------------------------
;;; write-feedback-state-file merge: union with existing on-disk content.
;;; -------------------------------------------------------------------------

(test write-feedback-state-merges-with-existing-file
  "Two MCP sessions sharing one Claude-sid each writing in sequence
   produce a file whose activated and feedback_given are the UNION
   of both sessions, not just the last writer's."
  (let* ((dir (format nil "/tmp/test-fb-merge-~a" (random 1000000)))
         (sid-1 "mcp-shell-1")
         (sid-2 "mcp-shell-2")
         (claude-sid "claude-shared")
         (playbook::*session-states* (make-hash-table :test 'equal))
         (playbook::*http-mode* t))
    (unwind-protect
         (progn
           ;; Shell 1: activate pat-a, pat-b; feedback on pat-a.
           (let ((s1 (get-or-create-session sid-1)))
             (setf (playbook::session-state-claude-session-id s1) claude-sid))
           (record-activation sid-1 "pat-a")
           (record-activation sid-1 "pat-b")
           (record-feedback sid-1 "pat-a" :helpful)
           (write-feedback-state-file dir sid-1)
           ;; Shell 2: activate pat-b, pat-c; feedback on pat-c.
           (let ((s2 (get-or-create-session sid-2)))
             (setf (playbook::session-state-claude-session-id s2) claude-sid))
           (record-activation sid-2 "pat-b")
           (record-activation sid-2 "pat-c")
           (record-feedback sid-2 "pat-c" :helpful)
           (write-feedback-state-file dir sid-2)
           ;; The file must contain the union, not last-writer-wins.
           (let* ((path (feedback-state-path dir claude-sid))
                  (state (read-feedback-state-file path))
                  (activated (coerce (gethash "activated" state) 'list))
                  (feedback (coerce (gethash "feedback_given" state) 'list))
                  (pending (coerce (gethash "pending" state) 'list)))
             (is (= 3 (length activated)))
             (is (member "pat-a" activated :test #'string=))
             (is (member "pat-b" activated :test #'string=))
             (is (member "pat-c" activated :test #'string=))
             (is (= 2 (length feedback)))
             (is (member "pat-a" feedback :test #'string=))
             (is (member "pat-c" feedback :test #'string=))
             ;; pending = activated - feedback = {pat-b}.
             (is (= 1 (length pending)))
             (is (equal "pat-b" (first pending)))))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

(test multi-mcp-session-shared-claude-sid-pending-reflects-union-not-last
  "pending = (union activated) - (union feedback_given) across all MCP
   sessions sharing one Claude-sid. Verifies the union semantic at the
   level the Stop hook reads."
  (let* ((dir (format nil "/tmp/test-fb-pending-~a" (random 1000000)))
         (claude-sid "claude-pending-test")
         (mcp-sids '("shell-1" "shell-2" "shell-3"))
         (playbook::*session-states* (make-hash-table :test 'equal))
         (playbook::*http-mode* t))
    (unwind-protect
         (progn
           ;; Each shell activates {pat-1, pat-2, pat-3} but feedbacks
           ;; only its own pat (shell-1 → pat-1, etc.). So under last-
           ;; writer-wins, the file would show only the last shell's
           ;; feedback. Under union semantics, all 3 patterns get fb.
           (dolist (sid mcp-sids)
             (let ((s (get-or-create-session sid)))
               (setf (playbook::session-state-claude-session-id s) claude-sid))
             (dolist (p '("pat-1" "pat-2" "pat-3"))
               (record-activation sid p))
             (record-feedback sid (format nil "pat-~A"
                                          (1+ (position sid mcp-sids
                                                        :test #'string=)))
                              :helpful)
             (write-feedback-state-file dir sid))
           (let* ((path (feedback-state-path dir claude-sid))
                  (state (read-feedback-state-file path))
                  (activated (coerce (gethash "activated" state) 'list))
                  (feedback (coerce (gethash "feedback_given" state) 'list))
                  (pending (coerce (gethash "pending" state) 'list))
                  (count (gethash "count" state)))
             (is (= 3 (length activated)))
             ;; UNION of feedback across 3 shells = {pat-1,pat-2,pat-3}.
             (is (= 3 (length feedback)))
             ;; pending = activated - feedback = {} (everyone gave fb).
             (is (zerop (length pending)))
             (is (zerop count))))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

;;; -------------------------------------------------------------------------
;;; write-feedback-state-file mode-agnostic invariant: claude-sid is the key.
;;; -------------------------------------------------------------------------

(test feedback-write-without-resolved-claude-sid-is-an-error-not-fallback
  "A session with no claude-session-id refuses to write rather than
   falling back to an MCP-sid-keyed path that the Stop hook cannot find.
   Holds in HTTP mode for any unresolved session, regardless of which
   sid the caller passes in."
  (let* ((dir (format nil "/tmp/test-fb-no-fallback-~a" (random 1000000)))
         (mcp-sid "mcp-sid-unresolved")
         (playbook::*session-states* (make-hash-table :test 'equal))
         (playbook::*http-mode* t))
    (unwind-protect
         (let ((session (get-or-create-session mcp-sid)))
           (is (null (playbook::session-state-claude-session-id session))
               "HTTP-mode get-or-create leaves claude-session-id NIL")
           (record-activation mcp-sid "pat-a")
           (let ((result (write-feedback-state-file dir mcp-sid)))
             (is (null result) "write must refuse")
             (is (null (probe-file (feedback-state-path dir mcp-sid)))
                 "no MCP-sid-keyed file may have been written")))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

(test stdio-session-keys-feedback-state-by-session-id
  "In stdio mode the session-id IS the Claude session ID by
   construction.  GET-OR-CREATE-SESSION populates claude-session-id
   eagerly, so write-feedback-state-file produces a file at the
   session-id-keyed path without ever consulting a fallback."
  (let* ((dir (format nil "/tmp/test-fb-stdio-~a" (random 1000000)))
         (sid "stdio-claude-sid")
         (playbook::*session-states* (make-hash-table :test 'equal))
         (playbook::*http-mode* nil))
    (unwind-protect
         (let ((session (get-or-create-session sid)))
           (is (string= sid (playbook::session-state-claude-session-id session))
               "stdio get-or-create must stamp session-id as claude-sid")
           (record-activation sid "pat-stdio")
           (let ((path (write-feedback-state-file dir sid)))
             (is (not (null path)) "write must succeed in stdio mode")
             (is (probe-file (feedback-state-path dir sid))
                 "file must land at the session-id-keyed path")))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

;;; -------------------------------------------------------------------------
;;; PID registry: multiple sids per PID, newest-first.
;;; -------------------------------------------------------------------------

(test register-claude-session-for-pid-allows-multiple-sids-per-pid
  "Two registrations for the same PID with different sids must coexist;
   the registry no longer overwrites prior entries.  Defends against
   silent coalescing of parallel Claude Code shells when peer-PID
   lineage is shared."
  (let ((playbook::*pid-claude-session-registry* (make-hash-table :test 'eql)))
    (register-claude-session-for-pid 100 "claude-shell-A")
    (register-claude-session-for-pid 100 "claude-shell-B")
    (let ((entries (gethash 100 playbook::*pid-claude-session-registry*)))
      (is (= 2 (length entries))
          "both sids must be retained")
      (is (member "claude-shell-A" entries :key #'car :test #'equal)
          "earlier sid stays in the registry")
      (is (member "claude-shell-B" entries :key #'car :test #'equal)
          "later sid is recorded too"))))

(test lookup-claude-session-by-pid-returns-newest
  "lookup-claude-session-by-pid picks the most recently registered sid
   when a PID has multiple entries.  The Claude-Session-Id header is
   the authoritative per-request signal; this fallback is best-effort."
  (let ((playbook::*pid-claude-session-registry* (make-hash-table :test 'eql)))
    (register-claude-session-for-pid 200 "older-sid")
    (sleep 0.01)
    (register-claude-session-for-pid 200 "newer-sid")
    (is (string= "newer-sid" (lookup-claude-session-by-pid 200))
        "lookup must return the most recent sid")))

(test register-claude-session-for-pid-promotes-on-re-registration
  "Re-registering the same sid for a PID promotes it to newest-first
   without duplicating the entry."
  (let ((playbook::*pid-claude-session-registry* (make-hash-table :test 'eql)))
    (register-claude-session-for-pid 300 "sid-X")
    (register-claude-session-for-pid 300 "sid-Y")
    (register-claude-session-for-pid 300 "sid-X")
    (let ((entries (gethash 300 playbook::*pid-claude-session-registry*)))
      (is (= 2 (length entries)) "no duplicate entries for sid-X")
      (is (string= "sid-X" (car (first entries)))
          "re-registered sid is now at the head"))))

(test resolve-claude-session-id-resolves-when-only-one-distinct-sid-registered
  "resolve-claude-session-id walks the per-PID list-of-(sid . ts) shape and
   returns the registered claude-sid when the registry holds exactly one
   distinct sid (one Claude Code shell, possibly re-registered)."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*pid-claude-session-registry* (make-hash-table :test 'eql))
        (playbook::*http-mode* t))
    (register-claude-session-for-pid 400 "the-only-claude-sid")
    (sleep 0.01)
    (register-claude-session-for-pid 401 "the-only-claude-sid")
    (let* ((session (get-or-create-session "mcp-resolve"))
           (resolved (playbook::resolve-claude-session-id session)))
      (is (string= "the-only-claude-sid" resolved)
          "single distinct sid in registry must resolve"))))

(test resolve-claude-session-id-refuses-when-distinct-sids-across-pids
  "resolve-claude-session-id REFUSES (returns NIL) when the registry holds
   two distinct claude-sids registered under different PIDs.  Peer-PID
   resolution is fundamentally ambiguous in parallel-shell scenarios:
   guessing the most-recent globally causes feedback writes to land
   under the wrong claude-sid, where the activator's Stop hook cannot
   read them.  Callers must pass the Claude-Session-Id header for a
   deterministic answer."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*pid-claude-session-registry* (make-hash-table :test 'eql))
        (playbook::*http-mode* t))
    (register-claude-session-for-pid 400 "claude-shell-A")
    (sleep 0.01)
    (register-claude-session-for-pid 401 "claude-shell-B")
    (let* ((session (get-or-create-session "mcp-resolve"))
           (resolved (playbook::resolve-claude-session-id session)))
      (is (null resolved)
          "ambiguous registry must resolve to NIL, not a guess")
      (is (null (playbook::session-state-claude-session-id session))
          "no fabricated identity may be stamped on the session"))))

(test resolve-claude-session-id-refuses-when-distinct-sids-on-one-pid
  "When two parallel Claude shells share a UNIX PID lineage, the same
   PID accumulates entries for distinct claude-sids.  Even within a
   single PID's list-of-entries, distinct sids mean ambiguous identity:
   the resolver must refuse rather than pick the newest."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*pid-claude-session-registry* (make-hash-table :test 'eql))
        (playbook::*http-mode* t))
    (register-claude-session-for-pid 500 "claude-shell-A")
    (sleep 0.01)
    (register-claude-session-for-pid 500 "claude-shell-B")
    (let* ((session (get-or-create-session "mcp-resolve"))
           (resolved (playbook::resolve-claude-session-id session)))
      (is (null resolved)
          "two distinct sids on one PID must resolve to NIL"))))

(test feedback-bridge-survives-parallel-shell-without-header
  "Regression for the feedback-nudge false-positive bug: when a parallel
   Claude shell registers a different claude-sid AFTER the activator,
   a header-less :feedback! call from the activator's MCP session
   must NOT mis-route the bridge write to the parallel shell's
   feedback-state.json.  Pre-fix, resolve-claude-session-id picked the
   globally-most-recent sid and the bridge wrote under the wrong key,
   leaving the activator's Stop hook to re-fire on patterns the agent
   had already given feedback on.  Post-fix, the resolver refuses
   ambiguous resolution; the activator's session — already stamped
   from its own header — keeps its correct claude-sid."
  (let ((dir (format nil "/tmp/test-fb-bridge-survives-~a" (random 1000000)))
        (playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*pid-claude-session-registry* (make-hash-table :test 'eql))
        (playbook::*http-mode* t))
    (unwind-protect
         (progn
           ;; Activator session: its claude-sid was set from a header at
           ;; request entry, and the activation pre-populated the
           ;; feedback-state.json under that claude-sid.
           (let ((s (get-or-create-session "mcp-activator")))
             (setf (playbook::session-state-claude-session-id s)
                   "claude-activator"))
           (record-activation "mcp-activator" "pat-stuck-1")
           (record-activation "mcp-activator" "pat-stuck-2")
           ;; Parallel shell registers a different claude-sid LATER.
           (register-claude-session-for-pid 600 "claude-activator")
           (sleep 0.01)
           (register-claude-session-for-pid 700 "claude-parallel")
           ;; Subsequent :feedback! call on the activator session.
           ;; It already has its claude-sid stamped from header entry,
           ;; so apply-claude-session-id should NOT re-resolve.  Even if
           ;; the resolver were reached, post-fix it returns NIL.
           (record-feedback "mcp-activator" "pat-stuck-1" :not-relevant)
           (record-feedback "mcp-activator" "pat-stuck-2" :not-relevant)
           (let ((path (write-feedback-state-file dir "mcp-activator")))
             (is (not (null path))
                 "bridge write must succeed for the activator session")
             ;; The file path must be keyed by the activator's sid,
             ;; not the parallel shell's.
             (is (search "claude-activator" (namestring path)))
             (is (not (search "claude-parallel" (namestring path))))
             ;; And the file must reflect that feedback cleared pending.
             (let ((state (read-feedback-state-file path)))
               (is (zerop (gethash "count" state))
                   "pending count must be zero after :feedback!")
               (is (zerop (length (gethash "pending" state))))
               (is (= 2 (length (gethash "feedback_given" state))))))
           ;; And no parallel-shell-keyed file exists.
           (is (null (probe-file (feedback-state-path dir "claude-parallel")))
               "no feedback file may have been written under the parallel sid"))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

(test feedback-given-survives-cleanup-cycle
  "cleanup-session drops the in-memory state, but the on-disk
   feedback-state.json persists. A subsequent write from a fresh
   session-state with the same Claude-sid merges in the on-disk
   feedback_given rather than overwriting it with an empty list."
  (let* ((dir (format nil "/tmp/test-fb-cleanup-~a" (random 1000000)))
         (sid-1 "mcp-shell-cleanup-1")
         (sid-2 "mcp-shell-cleanup-2")
         (claude-sid "claude-cleanup")
         (playbook::*session-states* (make-hash-table :test 'equal))
         (playbook::*http-mode* t))
    (unwind-protect
         (progn
           ;; Shell 1: write feedback for pat-a, then get cleaned up.
           (let ((s1 (get-or-create-session sid-1)))
             (setf (playbook::session-state-claude-session-id s1) claude-sid))
           (record-activation sid-1 "pat-a")
           (record-feedback sid-1 "pat-a" :helpful)
           (write-feedback-state-file dir sid-1)
           (cleanup-session sid-1)
           ;; The on-disk file must still carry pat-a's feedback.
           (let* ((path (feedback-state-path dir claude-sid))
                  (state-after-cleanup (read-feedback-state-file path)))
             (is (not (null state-after-cleanup)))
             (is (member "pat-a"
                         (coerce (gethash "feedback_given" state-after-cleanup) 'list)
                         :test #'string=)))
           ;; Shell 2: writes its own feedback. Merge must preserve pat-a.
           (let ((s2 (get-or-create-session sid-2)))
             (setf (playbook::session-state-claude-session-id s2) claude-sid))
           (record-activation sid-2 "pat-b")
           (record-feedback sid-2 "pat-b" :helpful)
           (write-feedback-state-file dir sid-2)
           (let* ((path (feedback-state-path dir claude-sid))
                  (state (read-feedback-state-file path))
                  (feedback (coerce (gethash "feedback_given" state) 'list)))
             (is (= 2 (length feedback)))
             (is (member "pat-a" feedback :test #'string=))
             (is (member "pat-b" feedback :test #'string=))))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

;;; -------------------------------------------------------------------------
;;; reconcile-miskeyed-feedback-state-files: archive non-UUID-keyed dirs.
;;; -------------------------------------------------------------------------

(defun %make-fb-fixture (root dir-name &optional (payload "{\"activated\":[],\"feedback_given\":[],\"pending\":[],\"count\":0}"))
  "Create .claude/sessions/<DIR-NAME>/playbook/feedback-state.json under
   ROOT with PAYLOAD.  Returns the file path."
  (let ((path (merge-pathnames
               (format nil ".claude/sessions/~A/playbook/feedback-state.json"
                       dir-name)
               (uiop:ensure-directory-pathname root))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string payload s))
    path))

(test reconcile-miskeyed-archives-mcp-sid-shaped-dirs
  "A feedback-state.json under a 32-hex-uppercase MCP-sid-shaped
   directory is archived to .claude/sessions/.miskeyed/<dir>/."
  (let* ((root (format nil "/tmp/test-miskeyed-mcp-~a" (random 1000000)))
         (mcp-sid "588EE17EFB1F2C93FFD661F8A4C040FC"))
    (unwind-protect
         (let ((src (%make-fb-fixture root mcp-sid)))
           (is (probe-file src))
           (let ((summary (reconcile-miskeyed-feedback-state-files root)))
             (is (= 1 (cdr (assoc :scanned summary))))
             (is (= 1 (cdr (assoc :archived summary)))))
           (is (null (probe-file src)) "source file moved")
           (is (probe-file
                (merge-pathnames
                 (format nil ".claude/sessions/.miskeyed/~A/feedback-state.json"
                         mcp-sid)
                 (uiop:ensure-directory-pathname root)))
               "archived copy is at .miskeyed/<mcp-sid>/"))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname root) :validate t)))))

(test reconcile-miskeyed-leaves-uuid-shaped-dirs-alone
  "feedback-state.json under a UUID-shaped (Claude-sid) directory must
   not be touched — it is in the canonical location."
  (let* ((root (format nil "/tmp/test-miskeyed-uuid-~a" (random 1000000)))
         (claude-sid "a11797b3-80a8-4a57-8eb1-689ee86c03f4"))
    (unwind-protect
         (let ((src (%make-fb-fixture root claude-sid)))
           (let ((summary (reconcile-miskeyed-feedback-state-files root)))
             (is (zerop (cdr (assoc :scanned summary))))
             (is (zerop (cdr (assoc :archived summary)))))
           (is (probe-file src) "UUID-keyed file stays put"))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname root) :validate t)))))

(test reconcile-miskeyed-skips-its-own-archive-root
  "The .miskeyed/ archive root itself is not a session directory and
   must be skipped on subsequent scans, otherwise reconciliation would
   recursively archive its own output."
  (let* ((root (format nil "/tmp/test-miskeyed-self-~a" (random 1000000)))
         (mcp-sid "9DF4E8A685E2BCBE8005A43E9AC5A37F"))
    (unwind-protect
         (progn
           (%make-fb-fixture root mcp-sid)
           ;; First pass archives the misKeyed file.
           (reconcile-miskeyed-feedback-state-files root)
           ;; Second pass must be a no-op: nothing scanned, nothing archived,
           ;; and the .miskeyed root must still exist with its content.
           (let ((summary (reconcile-miskeyed-feedback-state-files root)))
             (is (zerop (cdr (assoc :scanned summary))))
             (is (zerop (cdr (assoc :archived summary)))))
           (is (probe-file
                (merge-pathnames
                 (format nil ".claude/sessions/.miskeyed/~A/feedback-state.json"
                         mcp-sid)
                 (uiop:ensure-directory-pathname root)))
               "archived file from first pass survives second pass"))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname root) :validate t)))))

(test reconcile-miskeyed-mixed-fixture-counts-correctly
  "A fixture with both UUID-shaped and MCP-sid-shaped dirs scans the
   misKeyed ones and leaves the others alone."
  (let* ((root (format nil "/tmp/test-miskeyed-mixed-~a" (random 1000000)))
         (uuid-1 "03473b1c-644c-4787-9703-8624785130c1")
         (uuid-2 "a11797b3-80a8-4a57-8eb1-689ee86c03f4")
         (mcp-1 "588EE17EFB1F2C93FFD661F8A4C040FC")
         (mcp-2 "B8D75162F1D7EC1922C4FD1A8A137F33"))
    (unwind-protect
         (let ((uuid-1-path (%make-fb-fixture root uuid-1))
               (uuid-2-path (%make-fb-fixture root uuid-2))
               (mcp-1-path (%make-fb-fixture root mcp-1))
               (mcp-2-path (%make-fb-fixture root mcp-2))
               (root-pathname (uiop:ensure-directory-pathname root)))
           (let ((summary (reconcile-miskeyed-feedback-state-files root)))
             (is (= 2 (cdr (assoc :scanned summary))))
             (is (= 2 (cdr (assoc :archived summary)))))
           ;; UUIDs untouched.
           (is (probe-file uuid-1-path))
           (is (probe-file uuid-2-path))
           ;; MCP-sids gone from original location.
           (is (null (probe-file mcp-1-path)))
           (is (null (probe-file mcp-2-path)))
           ;; And present under .miskeyed/.
           (is (probe-file
                (merge-pathnames
                 (format nil ".claude/sessions/.miskeyed/~A/feedback-state.json"
                         mcp-1)
                 root-pathname)))
           (is (probe-file
                (merge-pathnames
                 (format nil ".claude/sessions/.miskeyed/~A/feedback-state.json"
                         mcp-2)
                 root-pathname))))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname root) :validate t)))))
