(defpackage #:task-tests
  (:use #:cl #:task #:crdt #:fiveam)
  (:import-from #:tq
                #:*graph*
                #:*mutation-handler*
                #:*current-task-id*
                #:all-nodes
                #:node
                #:active
                #:dormant
                #:follow
                #:back
                #:where-step
                #:select-fields
                #:sort-by-field
                #:take-n
                #:ids-step
                #:count-step
                #:enrich-step
                #:group-by-step
                #:group-by-result-p
                #:group-by-result-groups
                #:node-union
                #:node-intersection
                #:node-difference
                #:safe-read-query
                #:interpret-query
                #:format-query-result
                #:mutation-log-p
                #:scaffold-result-p
                #:tq-error
                #:tq-error-message
                #:tq-parse-error
                #:mutation-without-handler
                #:query
                #:define-query
                #:exact-node)
  (:import-from #:task-validation
                #:validate-task-name
                #:validation-result-valid-p
                #:validation-result-reason
                #:validation-result-suggestion
                #:suggest-name-from-description
                #:slugify))

(in-package #:task-tests)

(def-suite :task-tests
  :description "Tests for task event-sourced library")

(in-suite :task-tests)

;;; --- Shared Test Helpers ---
;;; Defined here (loaded first) so test-markov.lisp and test-query.lisp
;;; can use WITH-TASK-ROOTS as a macro rather than an undefined function.
;;;
;;; WITH-TASK-ROOTS used to skip the test body when no real .kli/tasks/
;;; was reachable. Skips silently mask regressions in the integration
;;; surface, so the macro now BUILDS a small synthetic tasks tree under
;;; /tmp/ for the duration of the body. That makes the integration
;;; tests runnable inside the build sandbox without any environment
;;; preparation.

(defun %synth-event (id ts type data &optional (session "synth-session"))
  "Construct an EVENT for the synthetic fixture."
  (make-event :id id :timestamp ts :session session
              :clock (make-vector-clock) :type type :data data))

(defun %make-synthetic-tasks-root ()
  "Build a minimal three-task fixture under /tmp/ and return its path.

   Layout:
     synth-parent/events.jsonl   3 events: task.create + 2 task.fork
                                 (phase-of edges to childA + childB)
     synth-childA/events.jsonl   18 events: task.create + 17 observations
                                 (>15 events: cached-task-metrics calls
                                  cached-all-tasks with default :min-events 15)
     synth-childB/events.jsonl   3 events: task.create + observation +
                                 task.update-status (status=completed)"
  (let* ((root (format nil "/tmp/synth-tasks-~A/" (symbol-name (gensym "T"))))
         (write-task
          (lambda (id events)
            (let* ((dir (concatenate 'string root id "/"))
                   (path (concatenate 'string dir "events.jsonl"))
                   (log (make-event-log :path path)))
              (ensure-directories-exist dir)
              (dolist (ev events) (elog-append log ev))
              (elog-save log)))))
    (funcall write-task "synth-parent"
             (list (%synth-event "p1" 1000 :task.create
                                 '(:name "synth-parent"
                                   :description "synthetic parent"))
                   (%synth-event "p2" 1010 :task.fork
                                 '(:child-id "synth-childA"
                                   :edge-type "phase-of"))
                   (%synth-event "p3" 1020 :task.fork
                                 '(:child-id "synth-childB"
                                   :edge-type "phase-of"))))
    (funcall write-task "synth-childA"
             (cons (%synth-event "a1" 2000 :task.create
                                 '(:name "synth-childA"
                                   :description "synthetic child A"))
                   (loop for i from 1 to 17
                         for ts from 2010 by 5
                         collect (%synth-event
                                  (format nil "a~D" (1+ i))
                                  ts :observation
                                  (list :text (format nil "obs ~D" i))))))
    (funcall write-task "synth-childB"
             (list (%synth-event "b1" 3000 :task.create
                                 '(:name "synth-childB"
                                   :description "synthetic child B"))
                   (%synth-event "b2" 3010 :observation
                                 '(:text "single observation"))
                   (%synth-event "b3" 3020 :task.update-status
                                 '(:status "completed"))))
    root))

(defmacro with-task-roots (&body body)
  "Execute BODY against a freshly-built synthetic tasks root.

   Builds a unique fixture under /tmp/, dynamically binds *tasks-root*
   to it, clears the per-task and graph caches around BODY, then
   removes the fixture on exit. Always runs the body — sandbox builds
   no longer skip integration tests."
  (let ((root-var (gensym "ROOT")))
    `(let ((,root-var (%make-synthetic-tasks-root)))
       (unwind-protect
            (let ((*tasks-root* ,root-var))
              (clear-graph-cache)
              (clear-infos-cache)
              (clear-task-state-cache)
              (clear-markov-cache)
              (multiple-value-prog1 (progn ,@body)
                (clear-graph-cache)
                (clear-infos-cache)
                (clear-task-state-cache)
                (clear-markov-cache)))
         (uiop:delete-directory-tree (pathname ,root-var)
                                     :validate t
                                     :if-does-not-exist :ignore)))))
