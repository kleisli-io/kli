(in-package #:task-tests)

;;; --- Event serialization ---

(test event-json-round-trip
  "Event serializes to JSON and parses back correctly."
  (let* ((vc (make-vector-clock))
         (_ (vc-increment vc "s1"))
         (ev (make-event :id "ev-1" :timestamp 1000 :session "s1"
                         :clock vc :type :task.create
                         :data (list :name "test" :description "a task")))
         (json (event-to-json-string ev))
         (parsed (json-string-to-event json)))
    (declare (ignore _))
    (is (string= "ev-1" (event-id parsed)))
    (is (= 1000 (event-timestamp parsed)))
    (is (string= "s1" (event-session parsed)))
    (is (eq :task.create (event-type parsed)))
    (is (= 1 (vc-get (event-clock parsed) "s1")))
    (is (string= "test" (getf (event-data parsed) :name)))
    (is (string= "a task" (getf (event-data parsed) :description)))))

(test event-empty-data-round-trip
  "Event with no data serializes correctly."
  (let* ((ev (make-event :id "ev-2" :timestamp 500 :session "s1"
                         :clock (make-vector-clock) :type :session.join))
         (json (event-to-json-string ev))
         (parsed (json-string-to-event json)))
    (is (string= "ev-2" (event-id parsed)))
    (is (eq :session.join (event-type parsed)))
    (is (null (event-data parsed)))))

;;; --- Event Log ---

(test elog-append-and-count
  (let ((log (make-event-log)))
    (elog-append log (make-event :id "e1" :timestamp 100 :session "s1"
                                :clock (make-vector-clock) :type :task.create))
    (elog-append log (make-event :id "e2" :timestamp 200 :session "s1"
                                :clock (make-vector-clock) :type :observation))
    (is (= 2 (length (event-log-events log))))))

(test elog-save-load-round-trip
  "Save and reload preserves all events."
  (let* ((tmp (format nil "/tmp/task-test-~A.jsonl" (get-universal-time)))
         (vc (make-vector-clock))
         (log (make-event-log :path tmp)))
    (vc-increment vc "s1")
    (elog-append log (make-event :id "e1" :timestamp 100 :session "s1"
                                :clock vc :type :task.create
                                :data (list :name "test")))
    (vc-increment vc "s1")
    (elog-append log (make-event :id "e2" :timestamp 200 :session "s1"
                                :clock vc :type :observation
                                :data (list :text "found it")))
    (elog-save log)
    (let ((loaded (elog-load tmp)))
      (is (= 2 (length (event-log-events loaded))))
      ;; Events are in push order (newest first) after load
      (let ((first-ev (first (reverse (event-log-events loaded)))))
        (is (string= "e1" (event-id first-ev)))
        (is (eq :task.create (event-type first-ev)))))
    ;; Cleanup
    (delete-file tmp)))

(test elog-merge-deduplicates
  "Merge unions events by ID, deduplicates, sorts by timestamp."
  (let ((log1 (make-event-log))
        (log2 (make-event-log)))
    (elog-append log1 (make-event :id "e1" :timestamp 100 :session "s1"
                                  :clock (make-vector-clock) :type :task.create))
    (elog-append log1 (make-event :id "e2" :timestamp 300 :session "s1"
                                  :clock (make-vector-clock) :type :observation))
    ;; log2 has e1 (shared) and e3
    (elog-append log2 (make-event :id "e1" :timestamp 100 :session "s1"
                                  :clock (make-vector-clock) :type :task.create))
    (elog-append log2 (make-event :id "e3" :timestamp 200 :session "s2"
                                  :clock (make-vector-clock) :type :artifact.create))
    (let ((merged (elog-merge log1 log2)))
      (is (= 3 (length (event-log-events merged))))
      ;; Sorted by timestamp
      (is (equal '("e1" "e3" "e2")
                 (mapcar #'event-id (event-log-events merged)))))))

;;; --- Edge Encoding ---

(test edge-encoding-round-trip
  "encode-edge and decode-edge are inverses."
  (let ((encoded (encode-edge "phase-1" :phase-of)))
    (is (string= "phase-1::phase-of" encoded))
    (let ((decoded (decode-edge encoded)))
      (is (string= "phase-1" (car decoded)))
      (is (eq :phase-of (cdr decoded))))))

(test edge-targets-filtering
  "edge-targets filters OR-Set members by type."
  (let ((os (make-or-set)))
    (ors-add os (encode-edge "p1" :phase-of) "s:1")
    (ors-add os (encode-edge "p2" :phase-of) "s:2")
    (ors-add os (encode-edge "dep1" :depends-on) "s:3")
    ;; All targets
    (is (= 3 (length (edge-targets os))))
    ;; Filtered by type (set comparison — OR-Set order is unspecified)
    (let ((phases (edge-targets os :phase-of)))
      (is (= 2 (length phases)))
      (is (member "p1" phases :test #'string=))
      (is (member "p2" phases :test #'string=)))
    (is (equal '("dep1") (edge-targets os :depends-on)))))

;;; --- Task State ---

(test compute-state-basic
  "Full pipeline: create -> artifact -> session join -> observe."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "my-task" :description "a test"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :artifact.create
                              :data (list :path "plan.md"))
                  (make-event :id "e3" :timestamp 300 :session "s2"
                              :clock (make-vector-clock) :type :session.join)
                  (make-event :id "e4" :timestamp 400 :session "s2"
                              :clock (make-vector-clock) :type :observation
                              :data (list :text "found a bug"))))
         (state (compute-state events)))
    (is (string= "my-task" (task-state-id state)))
    (is (string= "a test" (lww-value (task-state-description state))))
    (is (string= "active" (lww-value (task-state-status state))))
    (is (= 2 (gs-count (task-state-sessions state))))
    (is (member "plan.md" (ors-members (task-state-artifacts state)) :test #'string=))
    (is (= 1 (gs-count (task-state-observations state))))))

(test compute-state-status-update
  "Status update via LWW-Register."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "task"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.update-status
                              :data (list :status "completed"))))
         (state (compute-state events)))
    (is (string= "completed" (lww-value (task-state-status state))))))

(test compute-state-artifact-delete
  "Artifact add then delete removes from members."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "task"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :artifact.create
                              :data (list :path "draft.md"))
                  (make-event :id "e3" :timestamp 300 :session "s1"
                              :clock (make-vector-clock) :type :artifact.delete
                              :data (list :path "draft.md"))))
         (state (compute-state events)))
    (is (null (ors-members (task-state-artifacts state))))))

(test compute-state-unknown-event-ignored
  "Unknown event types are silently ignored."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "task"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :future.event
                              :data (list :foo "bar"))))
         (state (compute-state events)))
    (is (string= "task" (task-state-id state)))))

;;; --- Coalgebraic Edge Operations ---

(test fork-event-adds-edge
  "task.fork adds typed edge to edges OR-Set."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "parent"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "child-1" :edge-type "phase-of"))
                  (make-event :id "e3" :timestamp 300 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "child-2" :edge-type "forked-from"))))
         (state (compute-state events)))
    (is (= 2 (length (ors-members (task-state-edges state)))))
    (is (equal '("child-1") (edge-targets (task-state-edges state) :phase-of)))
    (is (equal '("child-2") (edge-targets (task-state-edges state) :forked-from)))))

(test sever-event-removes-edge
  "task.sever removes edge from OR-Set."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "parent"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "child-1" :edge-type "phase-of"))
                  (make-event :id "e3" :timestamp 300 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "child-2" :edge-type "phase-of"))
                  (make-event :id "e4" :timestamp 400 :session "s1"
                              :clock (make-vector-clock) :type :task.sever
                              :data (list :target-id "child-2" :edge-type "phase-of"))))
         (state (compute-state events)))
    (is (= 1 (length (ors-members (task-state-edges state)))))
    (is (equal '("child-1") (edge-targets (task-state-edges state) :phase-of)))))

(test reclassify-event-changes-type
  "task.reclassify removes old edge type and adds new."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "parent"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.link
                              :data (list :target-id "other" :edge-type "depends-on"))
                  (make-event :id "e3" :timestamp 300 :session "s1"
                              :clock (make-vector-clock) :type :task.reclassify
                              :data (list :target-id "other"
                                          :old-type "depends-on"
                                          :new-type "related-to"))))
         (state (compute-state events)))
    (is (null (edge-targets (task-state-edges state) :depends-on)))
    (is (equal '("other") (edge-targets (task-state-edges state) :related-to)))))

(test session-claim-release
  "session.claim and session.release update claim LWW-Register."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "task"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :session.claim)
                  (make-event :id "e3" :timestamp 300 :session "s1"
                              :clock (make-vector-clock) :type :session.release)))
         (state (compute-state events)))
    ;; After release, claim is empty
    (is (string= "" (lww-value (task-state-claim state))))))

(test legacy-spawn-to-edge
  "Legacy :task.spawn maps to phase-of edge in OR-Set."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "parent"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.spawn
                              :data (list :child-id "old-child"))))
         (state (compute-state events)))
    (is (= 1 (length (ors-members (task-state-edges state)))))
    (is (equal '("old-child") (edge-targets (task-state-edges state) :phase-of)))))

;;; --- OR-Set Tag Uniqueness ---

(test sever-preserves-sibling-edges
  "Severing one edge does not remove sibling edges created in same event."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "parent"))
                  ;; Three forks with SAME event-id (simulates same-second creation)
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "child-a" :edge-type "phase-of"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "child-b" :edge-type "phase-of"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "child-c" :edge-type "phase-of"))
                  ;; Sever child-b
                  (make-event :id "e3" :timestamp 300 :session "s1"
                              :clock (make-vector-clock) :type :task.sever
                              :data (list :target-id "child-b" :edge-type "phase-of"))))
         (state (compute-state events))
         (remaining (edge-targets (task-state-edges state) :phase-of)))
    ;; child-b should be gone, child-a and child-c should survive
    (is (= 2 (length remaining)))
    (is (member "child-a" remaining :test #'string=))
    (is (member "child-c" remaining :test #'string=))
    (is (not (member "child-b" remaining :test #'string=)))))

(test per-element-tags-unique
  "Each ors-add in apply-task-event gets a unique tag."
  (let* ((events (list
                  (make-event :id "e1" :timestamp 100 :session "s1"
                              :clock (make-vector-clock) :type :task.create
                              :data (list :name "parent"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "a" :edge-type "phase-of"))
                  (make-event :id "e2" :timestamp 200 :session "s1"
                              :clock (make-vector-clock) :type :task.fork
                              :data (list :child-id "b" :edge-type "phase-of"))))
         (state (compute-state events))
         (edges-ht (crdt::ors-elements (task-state-edges state)))
         (all-tags nil))
    ;; Collect all tags from the OR-Set internals
    (maphash (lambda (elem tags) (declare (ignore elem)) (dolist (t1 tags) (push t1 all-tags)))
             edges-ht)
    ;; Each tag should be unique
    (is (= (length all-tags)
            (length (remove-duplicates all-tags :test #'equal))))))

;;; --- Topic Extraction ---

(test extract-topic-strips-phase-prefix
  "Phase-N prefix stripped: topic reflects actual content, not phase number."
  (is (string= "unified-graph"
               (extract-topic "2026-01-31-phase-1-unified-graph")))
  (is (string= "ace-core"
               (extract-topic "2026-01-31-phase-2-ace-core")))
  (is (string= "lib-extraction"
               (extract-topic "2026-01-31-phase-2a-lib-extraction")))
  (is (string= "conventions"
               (extract-topic "2026-01-31-phase-0-conventions")))
  (is (string= "playbook-enrichment"
               (extract-topic "2026-01-30-phase-6-playbook-enrichment"))))

(test extract-topic-phase-tasks-dont-false-cluster
  "Phase tasks with different content produce different topics."
  (let ((topics (mapcar #'extract-topic
                  '("2026-01-31-phase-1-unified-graph"
                    "2026-01-31-phase-1-lisp-pipeline"
                    "2026-01-31-phase-1-task-bootstrap-tool"))))
    ;; All three should have different topics
    (is (= 3 (length (remove-duplicates topics :test #'string=))))))

(test extract-topic-strips-double-date
  "Double-dated tasks extract real topic, not second date."
  (is (string= "swank-mcp"
               (extract-topic "2026-01-28-2026-01-28-swank-mcp-profiling")))
  (is (string= "lol-reactive"
               (extract-topic "2026-01-24-2026-01-24-lol-reactive-demo"))))

(test extract-topic-skips-leading-noise
  "Noise words at any position are skipped, not used as topic."
  (is (string= "task-fork"
               (extract-topic "2026-01-31-fix-task-fork-structural-edges")))
  (is (string= "missing-health"
               (extract-topic "2026-01-31-add-missing-health-queries")))
  ;; Noise mid-word shouldn't match
  (is (string= "fixture-setup"
               (extract-topic "2026-01-31-fixture-setup"))))

(test extract-topic-normal-tasks-unaffected
  "Non-phase, non-noise tasks produce same topics as before."
  (is (string= "skill-task"
               (extract-topic "2026-01-31-skill-task-tooling-migration")))
  (is (string= "coalgebraic-task"
               (extract-topic "2026-01-31-coalgebraic-task-infrastructure")))
  (is (string= "bubblewrap-swank"
               (extract-topic "2026-01-31-bubblewrap-swank-sandboxing")))
  (is (string= "handoff-convergence"
               (extract-topic "2026-01-31-handoff-convergence")))
  (is (string= "mcp-framework"
               (extract-topic "2026-01-31-mcp-framework-http-transport"))))

;;; --- Task Name Validation ---

(test validate-task-name-valid-verb-object
  "Valid: verb + object pattern."
  (is (validation-result-valid-p (validate-task-name "implement-user-auth")))
  (is (validation-result-valid-p (validate-task-name "fix-login-redirect")))
  (is (validation-result-valid-p (validate-task-name "add-retry-logic")))
  (is (validation-result-valid-p (validate-task-name "remove-deprecated-api"))))

(test validate-task-name-valid-with-prefix
  "Valid: structural prefix + number + semantic content."
  (is (validation-result-valid-p (validate-task-name "phase-1-fix-login")))
  (is (validation-result-valid-p (validate-task-name "research-2-caching-strategies")))
  (is (validation-result-valid-p (validate-task-name "implement-3-oauth-flow"))))

(test validate-task-name-valid-noun-phrase
  "Valid: multi-word noun phrase."
  (is (validation-result-valid-p (validate-task-name "user-authentication")))
  (is (validation-result-valid-p (validate-task-name "database-connection-pool")))
  (is (validation-result-valid-p (validate-task-name "api-rate-limiting"))))

(test validate-task-name-valid-single-word
  "Valid: single word with 6+ characters."
  (is (validation-result-valid-p (validate-task-name "authentication")))
  (is (validation-result-valid-p (validate-task-name "validation")))
  (is (validation-result-valid-p (validate-task-name "infrastructure"))))

(test validate-task-name-valid-uppercase
  "Valid: uppercase names work (case insensitive)."
  (is (validation-result-valid-p (validate-task-name "CREATE-VALIDATION-MODULE")))
  (is (validation-result-valid-p (validate-task-name "INTEGRATE-MCP-TOOLS")))
  (is (validation-result-valid-p (validate-task-name "ENHANCE-SCAFFOLD-OPERATIONS"))))

(test validate-task-name-invalid-letter-number
  "Invalid: single letter + number (P1, T2)."
  (let ((result (validate-task-name "P1")))
    (is (not (validation-result-valid-p result)))
    (is (stringp (validation-result-reason result)))
    (is (stringp (validation-result-suggestion result))))
  (is (not (validation-result-valid-p (validate-task-name "P5"))))
  (is (not (validation-result-valid-p (validate-task-name "T2"))))
  (is (not (validation-result-valid-p (validate-task-name "A1")))))

(test validate-task-name-invalid-no-semantic-content
  "Invalid: prefix + number with no semantic content."
  (is (not (validation-result-valid-p (validate-task-name "phase-1"))))
  (is (not (validation-result-valid-p (validate-task-name "research-2"))))
  (is (not (validation-result-valid-p (validate-task-name "implement-3")))))

(test validate-task-name-invalid-vague-words
  "Invalid: vague words as sole content."
  (is (not (validation-result-valid-p (validate-task-name "stuff"))))
  (is (not (validation-result-valid-p (validate-task-name "misc"))))
  (is (not (validation-result-valid-p (validate-task-name "wip"))))
  (is (not (validation-result-valid-p (validate-task-name "temp"))))
  (is (not (validation-result-valid-p (validate-task-name "core")))))

(test validate-task-name-invalid-too-short
  "Invalid: words too short to be meaningful."
  (is (not (validation-result-valid-p (validate-task-name "foo"))))
  (is (not (validation-result-valid-p (validate-task-name "bar"))))
  (is (not (validation-result-valid-p (validate-task-name "baz")))))

(test validate-task-name-invalid-pure-numeric
  "Invalid: pure numeric names."
  (is (not (validation-result-valid-p (validate-task-name "123"))))
  (is (not (validation-result-valid-p (validate-task-name "456-789")))))

;;; --- Name Suggestion from Description ---

(test suggest-name-from-description-basic
  "Generates kebab-case slug from description."
  (is (string= "research-current-architecture"
               (suggest-name-from-description "Research the current architecture")))
  (is (string= "implement-depot-parameter"
               (suggest-name-from-description "Implement the depot parameter")))
  (is (string= "fix-login-redirect-bug"
               (suggest-name-from-description "Fix the login redirect bug"))))

(test suggest-name-from-description-filters-stopwords
  "Filters common stopwords from output."
  ;; Stopwords "the" and "to" are filtered, but "system" passes (4 words max)
  (is (string= "add-user-authentication-system"
               (suggest-name-from-description "Add the user authentication to the system")))
  ;; "new" is not a stopword, so it appears in output (limited to 4 words)
  (is (string= "update-database-schema-new"
               (suggest-name-from-description "Update the database schema for the new API"))))

(test suggest-name-from-description-limits-words
  "Limits output to max-words (default 4)."
  (let ((result (suggest-name-from-description
                 "Implement a comprehensive user authentication system with OAuth2 support")))
    (is (<= (length (remove-if-not (lambda (c) (char= c #\-)) result)) 3))))

(test slugify-handles-special-chars
  "Slugify handles path separators and underscores."
  (is (string= "src-components-button"
               (slugify "src/components/Button")))
  (is (string= "user-auth-service"
               (slugify "user_auth_service"))))

;;; --- Graph / Scan / Health Regression Tests ---
;;;
;;; These tests use real ace/tasks/ data via detect-all-task-roots.
;;; They verify the multi-depot graph pipeline does not regress
;;; when single-depot legacy code is removed.

(defun task-roots-available-p ()
  "Try to set up *depot-tasks-roots* from real depot data. Returns T if available."
  (handler-case
      (progn
        (detect-all-task-roots)
        (plusp (hash-table-count *depot-tasks-roots*)))
    (error () nil)))

(defmacro with-task-roots (&body body)
  "Execute BODY only if real depot task roots are available, otherwise skip."
  `(if (not (task-roots-available-p))
       (skip "No depot task roots available (sandbox or missing depot)")
       (progn ,@body)))

(test detect-all-task-roots-preserves-data-on-failure
  "detect-all-task-roots must not wipe existing data if discovery fails.
   Regression test for the clear-then-error bug where clrhash ran before
   find-symbol, leaving *depot-tasks-roots* permanently empty."
  ;; Pre-populate the hash table with known data
  (let ((saved-roots (make-hash-table :test 'equal))
        (saved-tasks-root *tasks-root*)
        (saved-current-depot *current-depot*))
    ;; Save current state
    (maphash (lambda (k v) (setf (gethash k saved-roots) v))
             *depot-tasks-roots*)
    (unwind-protect
         (progn
           ;; Set up known state
           (clrhash *depot-tasks-roots*)
           (setf (gethash "test-depot" *depot-tasks-roots*) "/tmp/fake-tasks/")
           (is (= 1 (hash-table-count *depot-tasks-roots*))
               "Precondition: table has one entry")
           ;; Call detect-all-task-roots — even if it succeeds or uses
           ;; fallback, it should never leave the table empty
           (handler-case (detect-all-task-roots)
             (error () nil))
           (is (plusp (hash-table-count *depot-tasks-roots*))
               "detect-all-task-roots must not leave *depot-tasks-roots* empty"))
      ;; Restore original state
      (clrhash *depot-tasks-roots*)
      (maphash (lambda (k v) (setf (gethash k *depot-tasks-roots*) v))
               saved-roots)
      (setf *tasks-root* saved-tasks-root)
      (setf *current-depot* saved-current-depot))))

(test scan-depot-tasks-returns-plists
  "scan-depot-tasks returns a list of plists with expected keys."
  (with-task-roots
    (let ((infos (scan-depot-tasks "core")))
      (is (listp infos))
      (is (plusp (length infos)))
      ;; Each info is a plist with qualified :id, :depot, :bare-id, and :topic
      (let* ((first (first infos))
             (keys (loop for (k v) on first by #'cddr collect k)))
        (is (getf first :id))
        (is (stringp (getf first :id)))
        (is (search "core:" (getf first :id))
            "ID should be depot-qualified")
        (is (equal "core" (getf first :depot)))
        (is (stringp (getf first :bare-id)))
        (is (getf first :topic))
        ;; Should have standard metadata keys
        (is (member :has-events keys))
        (is (member :status keys))))))

(test scan-depot-tasks-qualifies-ids
  "scan-depot-tasks returns tasks with depot-qualified IDs."
  (with-task-roots
    (let ((infos (scan-depot-tasks "core")))
      (is (listp infos))
      (is (plusp (length infos)))
      ;; IDs should be qualified with depot prefix
      (let ((id (getf (first infos) :id)))
        (is (stringp id))
        (is (search "core:" id))))))

(test scan-all-depot-tasks-aggregates
  "scan-all-depot-tasks returns tasks from all detected depots."
  (with-task-roots
    (let* ((all (scan-all-depot-tasks))
           (ids (mapcar (lambda (info) (getf info :id)) all))
           (depots (remove-duplicates
                    (mapcar (lambda (id)
                              (subseq id 0 (position #\: id)))
                            ids)
                    :test #'string=)))
      (is (plusp (length all)))
      ;; Should have tasks from multiple depots
      (is (>= (length depots) 1))
      ;; Total should be >= any single depot
      (is (>= (length all) (length (scan-depot-tasks "core")))))))

(test build-multi-depot-task-graph-structure
  "build-multi-depot-task-graph returns a task-graph with nodes and edges."
  (with-task-roots
    (let ((g (build-multi-depot-task-graph)))
      (is (typep g 'task-graph))
      ;; Should have nodes
      (is (plusp (hash-table-count (task-graph-nodes g))))
      ;; Should have forward edges (tasks reference each other)
      (is (plusp (hash-table-count (task-graph-forward g))))
      ;; Node count should match scan-all-depot-tasks
      (is (= (hash-table-count (task-graph-nodes g))
              (length (scan-all-depot-tasks)))))))

(test graph-node-has-expected-properties
  "Graph nodes contain expected property keys."
  (with-task-roots
    (let* ((g (build-multi-depot-task-graph))
           (nodes (task-graph-nodes g))
           (sample-id nil)
           (sample-props nil))
      (maphash (lambda (id props)
                 (unless sample-id
                   (setf sample-id id sample-props props)))
               nodes)
      ;; Node ID is the hash key (a qualified string like "core:task-name")
      (is (not (null sample-id)))
      (is (stringp sample-id))
      ;; Properties plist has :topic and :display-name
      (is (not (null sample-props)))
      (is (not (null (getf sample-props :topic))))
      (is (not (null (getf sample-props :display-name)))))))

(test health-queries-return-lists
  "All health query functions return lists without error."
  (with-task-roots
    ;; Each should return a list (possibly empty) — now always multi-depot
    (is (listp (find-stale-tasks)))
    (is (listp (find-dead-ends)))
    (is (listp (find-declared-orphans)))
    (is (listp (find-stale-claims)))
    (is (listp (find-unexplored-frontier)))
    (is (listp (find-convergent-clusters)))
    (is (listp (find-premature-completions)))))

(test task-health-data-structure
  "task-health-data returns alist with expected categories."
  (with-task-roots
    (let ((health (task-health-data)))
      (is (listp health))
      (is (plusp (length health)))
      ;; Should contain standard health categories
      (let ((categories (mapcar #'first health)))
        (is (member :stale-forks categories))
        (is (member :dead-ends categories))
        (is (member :unlinked-roots categories))
        (is (member :stale-claims categories))
        (is (member :unexplored-frontier categories))))))

;;; --- Thread Isolation (Session Globals) ---
;;;
;;; Regression test for the thread-safety fix in task-mcp server.lisp.
;;; The :around method on acceptor-dispatch-request wraps each HTTP request
;;; with LET bindings of session globals, preventing concurrent threads from
;;; clobbering each other's *current-task-id* etc.
;;;
;;; These tests exercise the same defvar + LET pattern using task globals.

(test thread-isolation-let-binding
  "LET-bound specials are isolated across threads (regression: plan query context)."
  ;; This tests the pattern used by the :around method fix.
  ;; Without LET bindings, concurrent threads race on defvar specials.
  (let ((results (make-array 2 :initial-element nil))
        (barrier (bt:make-semaphore :name "test-barrier")))
    ;; Thread A: set *tasks-root* to "A", wait, then read it back
    (let ((ta (bt:make-thread
               (lambda ()
                 (let ((*tasks-root* nil))  ;; thread-local LET binding
                   (setf *tasks-root* "/depot-a/tasks/")
                   (bt:signal-semaphore barrier)  ;; signal ready
                   (sleep 0.05)  ;; give thread B time to run
                   (setf (aref results 0) *tasks-root*)))))
          ;; Thread B: wait for A, then set *tasks-root* to "B"
          (tb (bt:make-thread
               (lambda ()
                 (let ((*tasks-root* nil))  ;; thread-local LET binding
                   (bt:wait-on-semaphore barrier)  ;; wait for A
                   (setf *tasks-root* "/depot-b/tasks/")
                   (setf (aref results 1) *tasks-root*))))))
      (bt:join-thread ta)
      (bt:join-thread tb)
      ;; Each thread should see its own value, not the other's
      (is (string= "/depot-a/tasks/" (aref results 0)))
      (is (string= "/depot-b/tasks/" (aref results 1))))))

(test thread-isolation-without-let-races
  "Without LET bindings, concurrent threads clobber shared specials."
  ;; Documents the bug that the :around method fix prevents.
  ;; With bare SETF on a defvar, Thread B's write is visible to Thread A.
  (let ((results (make-array 2 :initial-element nil))
        (barrier (bt:make-semaphore :name "test-barrier"))
        (*tasks-root* nil))  ;; outer LET so we don't pollute real state
    ;; Thread A: set, wait, read — uses SETF without inner LET
    (let ((ta (bt:make-thread
               (lambda ()
                 (setf *tasks-root* "/depot-a/tasks/")
                 (bt:signal-semaphore barrier)
                 (sleep 0.05)
                 (setf (aref results 0) *tasks-root*))))
          ;; Thread B: wait, then SETF the same global
          (tb (bt:make-thread
               (lambda ()
                 (bt:wait-on-semaphore barrier)
                 (setf *tasks-root* "/depot-b/tasks/")
                 (setf (aref results 1) *tasks-root*)))))
      (bt:join-thread ta)
      (bt:join-thread tb)
      ;; Thread A sees Thread B's value (the race)
      (is (string= "/depot-b/tasks/" (aref results 0)))
      (is (string= "/depot-b/tasks/" (aref results 1))))))

;;; --- task-children Bare ID Qualification ---

(test task-children-qualifies-bare-ids
  "task-children qualifies bare child IDs using parent's depot (regression)."
  (with-task-roots
    ;; Find a parent task with children
    (let* ((all (scan-all-depot-tasks))
           (parent-with-children
             (loop for info in all
                   for id = (getf info :id)
                   when (and (qualified-id-p id)
                             (task-children id))
                   return id)))
      (when parent-with-children
        (let ((children (task-children parent-with-children)))
          ;; All children should be qualified
          (dolist (child children)
            (is (qualified-id-p child)
                "Child ~A of ~A should be qualified"
                child parent-with-children)))))))
