(in-package #:task-tests)

;;; Tests for task-validation name-improvement behaviour and the
;;; tq:maybe-improve-name boundary helper introduced by the
;;; kli-scaffold-plan-bad-phase-names task.

(def-suite :name-validation-tests
  :description "Tests for name-improvement boundary and failure condition."
  :in :task-tests)

(in-suite :name-validation-tests)

;;; --- maybe-improve-name happy paths ---

(test maybe-improve-name-returns-valid-name-unchanged
  "A name that already passes validation is returned as-is."
  (is (string= "fix-validation"
               (tq:maybe-improve-name "fix-validation" "anything"))))

(test maybe-improve-name-returns-valid-name-unchanged-ignoring-description
  "Even when DESCRIPTION could suggest a different slug, the valid
   original wins — improvement is only triggered on invalid input."
  (is (string= "hoas-prelude"
               (tq:maybe-improve-name "hoas-prelude"
                                      "Some completely different phrase"))))

(test maybe-improve-name-slugifies-invalid-name-from-description
  "phase-1 is invalid; a descriptive DESCRIPTION yields a slug that
   validates, and that slug is returned."
  (is (string= "hoas-prelude-integration-tests"
               (tq:maybe-improve-name "phase-1"
                                      "HOAS prelude and integration tests"))))

;;; --- maybe-improve-name failure paths ---

(defun maybe-improve-name-failure (local-name description)
  "Call maybe-improve-name and return the signalled condition, or NIL."
  (handler-case
      (progn (tq:maybe-improve-name local-name description) nil)
    (task-validation:name-improvement-failed (c) c)))

(test maybe-improve-name-signals-on-empty-description
  "An invalid name paired with an empty description cannot be
   improved and must signal name-improvement-failed with the original
   input preserved in the :input slot."
  (let ((c (maybe-improve-name-failure "phase-1" "")))
    (is (not (null c)))
    (is (string= "phase-1"
                 (task-validation:name-improvement-failed-input c)))
    (is (string= ""
                 (task-validation:name-improvement-failed-description c)))
    (is (stringp (task-validation:name-improvement-failed-reason c)))))

(test maybe-improve-name-signals-on-nil-description
  "An invalid name paired with a nil description must signal
   name-improvement-failed; the :description slot faithfully reports
   nil rather than coercing to an empty string."
  (let ((c (maybe-improve-name-failure "phase-1" nil)))
    (is (not (null c)))
    (is (string= "phase-1"
                 (task-validation:name-improvement-failed-input c)))
    (is (null (task-validation:name-improvement-failed-description c)))))

(test maybe-improve-name-signals-when-suggestion-itself-invalid
  "A description that slugifies to something too short to pass
   validation must still trigger the failure branch — not silently
   return the invalid input."
  (let ((c (maybe-improve-name-failure "phase-1" "xy")))
    (is (not (null c)))
    (is (string= "phase-1"
                 (task-validation:name-improvement-failed-input c)))
    (is (string= "xy"
                 (task-validation:name-improvement-failed-description c)))))

;;; --- Condition reporting ---

(test name-improvement-failed-report-mentions-input-and-reason
  "The condition's :report method must include both the invalid input
   name and the validation reason so callers formatting the error for
   users (e.g. task_query's top-level handler) get a self-describing
   message."
  (let* ((c (maybe-improve-name-failure "phase-1" ""))
         (msg (princ-to-string c)))
    (is (search "phase-1" msg))
    (is (search (task-validation:name-improvement-failed-reason c) msg))))
