;;;; playbook-hooks - Co-Application Utilities
;;;;
;;;; Canonical pair keys, ledger I/O, and pair generation for
;;;; tracking which patterns are applied together in sessions.

(in-package :playbook-hooks)

(defun co-app-key (id-a id-b)
  "Canonical key for a co-application pair: sorted \"min:max\"."
  (if (string< id-a id-b)
      (format nil "~a:~a" id-a id-b)
      (format nil "~a:~a" id-b id-a)))

(defun generate-pairs (ids)
  "Generate all unique pairs from a list of IDs as canonical keys."
  (let ((result nil))
    (loop for (a . rest) on ids
          do (dolist (b rest)
               (push (co-app-key a b) result)))
    (nreverse result)))

(defun read-co-app-ledger (path)
  "Read co-application ledger from JSON file at PATH.
Returns a hash-table of canonical-key to count. Empty table if file missing."
  (or (claude-hooks:read-json-file path) (make-hash-table :test 'equal)))

(defun update-co-app-ledger (ledger pairs)
  "Increment counts in LEDGER for each canonical pair key in PAIRS.
Returns LEDGER."
  (dolist (key pairs ledger)
    (incf (gethash key ledger 0))))

(defun save-co-app-ledger (ledger path)
  "Save co-application LEDGER to JSON file at PATH atomically."
  (claude-hooks:atomic-write-json path ledger))

(defun co-app-ledger-path (cwd)
  "Default path for co-application ledger, relative to CWD."
  (format nil "~a/ace/playbook-co-applications.json" cwd))
