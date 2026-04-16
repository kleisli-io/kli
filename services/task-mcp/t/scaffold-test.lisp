(in-package #:task-mcp-tests)

;;; Regression tests for the scaffold-plan / scaffold-chain / :fork
;;; boundary introduced by the kli-scaffold-plan-bad-phase-names task:
;;;   * :fork now improves invalid names from description (Phase 2).
;;;   * scaffold-chain therefore produces descriptive IDs (Phase 3).
;;;   * :display-name is emitted by every scaffold path (Phase 4).

(in-suite :task-mcp-tests)

;;; --- Fixtures (scaffold-specific) -----------------------------------------

(defmacro with-scaffold-env ((&key (parent "parent-probe")) &body body)
  "Execute BODY with a temporary *tasks-root* and the parent task
   directory created so :fork has somewhere to write.  Binds both
   task-mcp's *current-task-id* (read by emit-event / session
   bookkeeping) and tq:*current-task-id* (read by execute-scaffold-*
   when they start the scaffold pipeline)."
  `(with-temp-tasks-root (root)
     (declare (ignorable root))
     (let ((*http-mode* nil)
           (*current-task-id* ,parent)
           (tq:*current-task-id* ,parent)
           (*session-vc* (crdt:make-vector-clock))
           (*session-id* "task-mcp-tests"))
       (task:ensure-task-directory ,parent)
       ,@body)))

(defun read-events (task-id)
  "Read the JSONL event stream for TASK-ID, returning a list of plists."
  (let ((path (merge-pathnames (format nil "~A/events.jsonl" task-id)
                               task:*tasks-root*)))
    (with-open-file (in path :direction :input :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil nil)
              while line
              collect (task:json-string-to-event line))))))

(defun set-meta-events-for-key (events key)
  "Return all :task.set-metadata events whose data names KEY."
  (loop for e in events
        when (and (eq (task:event-type e) :task.set-metadata)
                  (let ((data (task:event-data e)))
                    (string= key (getf data :key))))
          collect e))

(defun display-name-value (event)
  "Extract the :value field from a :task.set-metadata event."
  (getf (task:event-data event) :value))

;;; --- Phase 2: :fork name improvement at the mutation boundary --------------

(test fork-improves-invalid-name-from-description
  "When :fork receives an invalid LOCAL-NAME and a usable
   DESCRIPTION, the task is created under an improved slug derived
   from the description — not under the raw input."
  (with-scaffold-env (:parent "parent-probe")
    (let ((created-id (tq-mutation-handler
                       "parent-probe"
                       :fork "phase-1"
                       "HOAS prelude and integration tests"
                       "phase-of")))
      (is (stringp created-id))
      (is (search "hoas-prelude-integration-tests" created-id)
          "created-id should reflect the description, not phase-1")
      (is (not (search "phase-1" created-id))
          "raw 'phase-1' should not appear in the final id"))))

(test fork-accepts-already-valid-name-unchanged
  "A name that already validates must pass through :fork without
   being replaced by a description-derived slug."
  (with-scaffold-env (:parent "parent-probe")
    (let ((created-id (tq-mutation-handler
                       "parent-probe"
                       :fork "fix-validation"
                       "Any description"
                       "phase-of")))
      (is (search "fix-validation" created-id)))))

(test fork-rejects-invalid-name-with-empty-description
  "An invalid LOCAL-NAME combined with an empty DESCRIPTION leaves
   no material for name improvement and must signal
   task-validation:name-improvement-failed rather than creating a
   'phase-1'-shaped task."
  (with-scaffold-env (:parent "parent-probe")
    (is (signals-error-containing-p
         (lambda ()
           (tq-mutation-handler "parent-probe" :fork "phase-1" "" "phase-of"))
         "phase-1"))))

(test fork-rejects-invalid-name-with-nil-description
  "Nil description is structurally identical to empty from the
   improvement path's perspective."
  (with-scaffold-env (:parent "parent-probe")
    (is (signals-error-containing-p
         (lambda ()
           (tq-mutation-handler "parent-probe" :fork "phase-1" nil "phase-of"))
         "phase-1"))))

;;; --- Phase 3: scaffold-chain produces descriptive ids ---------------------

(test scaffold-chain-produces-descriptive-ids
  "scaffold-chain's 'phase-N' counters are improved at the :fork
   boundary (Phase 2) so the resulting task IDs reflect the
   user-supplied descriptions."
  (with-scaffold-env (:parent "parent-probe")
    (let* ((tq:*mutation-handler* #'tq-mutation-handler)
           (result (tq::execute-scaffold-chain
                    (list "Refactor name validation"
                          "Wire fork boundary checks"
                          "Regression tests for scaffold"))))
      (let ((ids (mapcar #'cdr (tq:scaffold-result-created result))))
        (is (= 3 (length ids)))
        (dolist (id ids)
          (is (not (search "phase-" id))
              "descriptive slug should not retain the raw phase-N form"))))))

;;; --- Phase 4: :display-name is always emitted -----------------------------

(test scaffold-chain-emits-display-name-metadata
  "Every phase produced by scaffold-chain must carry a
   :display-name metadata event whose value is the description
   passed in by the caller."
  (with-scaffold-env (:parent "parent-probe")
    (let* ((tq:*mutation-handler* #'tq-mutation-handler)
           (result (tq::execute-scaffold-chain
                    (list "First readable label"
                          "Second readable label"))))
      (dolist (pair (tq:scaffold-result-created result))
        (let* ((task-id (cdr pair))
               (events (read-events task-id))
               (display-metas (set-meta-events-for-key events "display-name")))
          (is (= 1 (length display-metas))
              "exactly one display-name set-meta event per phase")
          (is (member (display-name-value (first display-metas))
                      '("First readable label" "Second readable label")
                      :test #'string=)))))))

(test scaffold-plan-default-display-name-is-description
  "scaffold-plan specs without explicit :display-name must
   synthesise one from the phase description so format-plan
   renders a label instead of the raw id."
  (with-scaffold-env (:parent "parent-probe")
    (let* ((tq:*mutation-handler* #'tq-mutation-handler)
           (result (tq::execute-scaffold-plan
                    '((p1 "Readable implicit label")))))
      (let* ((task-id (cdar (tq:scaffold-result-created result)))
             (events (read-events task-id))
             (display-metas (set-meta-events-for-key events "display-name")))
        (is (= 1 (length display-metas)))
        (is (string= "Readable implicit label"
                     (display-name-value (first display-metas))))))))

(test scaffold-plan-explicit-display-name-wins
  "When a spec provides :display-name explicitly, the explicit value
   is stored and the description is NOT used as a fallback."
  (with-scaffold-env (:parent "parent-probe")
    (let* ((tq:*mutation-handler* #'tq-mutation-handler)
           (result (tq::execute-scaffold-plan
                    '((p1 "Description not used here"
                          :display-name "Explicit override")))))
      (let* ((task-id (cdar (tq:scaffold-result-created result)))
             (events (read-events task-id))
             (display-metas (set-meta-events-for-key events "display-name")))
        (is (= 1 (length display-metas)))
        (is (string= "Explicit override"
                     (display-name-value (first display-metas))))))))
