;;;; session-leave.lisp - SessionEnd hook handler
;;;;
;;;; Best-effort: SessionEnd fires non-deterministically (cleanup-on-start
;;;; in session-start is the primary mechanism).
;;;;
;;;; 1. Emit :session.leave event to task's events.jsonl
;;;; 2. Update playbook co-application ledger
;;;; 3. Clean up session files
;;;;
;;;; Merged from: session-leave.lisp + playbook-session-end.nix

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Event Emission
;;; ---------------------------------------------------------------------------

(defun make-session-leave-event (session-id reason)
  "Create a session.leave event as hash-table."
  (let* ((ts (get-universal-time))
         (event-id (format nil "~A-~D" session-id ts))
         (data (make-hash-table :test #'equal))
         (event (make-hash-table :test #'equal)))
    (setf (gethash "reason" data) reason)
    (setf (gethash "id" event) event-id)
    (setf (gethash "timestamp" event) ts)
    (setf (gethash "session" event) session-id)
    (setf (gethash "clock" event) (make-hash-table))
    (setf (gethash "type" event) "session.leave")
    (setf (gethash "data" event) data)
    event))

(defun append-event-to-log (events-path event)
  "Append event to events.jsonl file."
  (let ((json-line (with-output-to-string (s)
                     (yason:encode event s))))
    (with-open-file (out events-path
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist nil)
      (when out
        (write-line json-line out)
        t))))

;;; ---------------------------------------------------------------------------
;;; Playbook Co-Application Mining (merged from playbook-session-end)
;;; ---------------------------------------------------------------------------

(defun update-playbook-state (cwd session-id)
  "Update co-application ledger and task-level session state. Best-effort."
  (handler-case
      (let ((ids (when (and cwd session-id)
                   (read-activated-ids cwd session-id))))
        (when (and ids (>= (length ids) 2) cwd)
          (let* ((ledger-path (co-app-ledger-path cwd))
                 (ledger (read-co-app-ledger ledger-path))
                 (pairs (generate-pairs ids)))
            (update-co-app-ledger ledger pairs)
            (save-co-app-ledger ledger ledger-path)))
        ;; Save task-level session state
        (when (and ids cwd)
          (let ((task-state-path (format nil "~a/.claude/playbook-state.json" cwd)))
            (let* ((state (or (read-json-file task-state-path)
                              (make-hash-table :test 'equal)))
                   (sessions (or (gethash "sessions" state) nil))
                   (entry (make-hash-table :test 'equal)))
              (setf (gethash "session_id" entry) session-id)
              (setf (gethash "activated" entry) ids)
              (setf (gethash "count" entry) (length ids))
              (setf (gethash "sessions" state)
                    (append sessions (list entry)))
              (atomic-write-json task-state-path state)))))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; File Cleanup
;;; ---------------------------------------------------------------------------

(defun safe-delete-file (path)
  "Delete file if it exists, ignoring errors."
  (when (and path (probe-file path))
    (handler-case (delete-file path)
      (error () nil))))

;;; ---------------------------------------------------------------------------
;;; Handler Entry Point
;;; ---------------------------------------------------------------------------

(defun session-leave-handler (input)
  "SessionEnd handler: emit leave event, update playbook, cleanup."
  (let ((cwd (jref input "cwd")))
    (when cwd
      (let* ((coord-root (depot:coordination-root-from cwd))
             (ppid (get-ppid))
             (session-file (when coord-root (session-file-path coord-root ppid)))
             (session-info (when coord-root (read-session-file coord-root ppid))))
        (when session-info
          (let ((events-path (gethash "events_path" session-info))
                (session-id (gethash "session_id" session-info)))
            ;; 1. Emit :session.leave event
            (when (and events-path session-id (probe-file events-path))
              (let ((event (make-session-leave-event session-id "graceful")))
                (append-event-to-log events-path event)))
            ;; 2. Update playbook co-application state
            (update-playbook-state cwd (jref input "session_id"))
            ;; 3. Delete session file
            (safe-delete-file session-file)
            ;; 4. Delete active-pids file
            (safe-delete-file (active-pid-path coord-root ppid)))))))
  (empty-response))
