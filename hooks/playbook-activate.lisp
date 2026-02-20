;;;; playbook-activate.lisp - UserPromptSubmit hook handler
;;;;
;;;; Lightweight domain detection + nudge. Detects programming domains
;;;; from prompt text and nudges Claude to use pq_query with (activate ...).

(in-package #:kli-hook)

(defun playbook-activate-handler (input)
  "UserPromptSubmit handler: detect domains and nudge playbook activation."
  (let ((prompt (jref input "prompt")))
    (when (skip-prompt-p prompt)
      (return-from playbook-activate-handler (empty-response)))
    (let ((domains (detect-domains-from-text prompt)))
      (if domains
          (let ((session-pb-dir (ensure-session-dir input "playbook"))
                (domain-str (format nil "~{~a~^ ~}" domains)))
            (declare (ignore session-pb-dir))
            (let ((domains-file (session-file input "playbook" "domains.txt")))
              (when domains-file
                (dolist (d domains)
                  (append-line-unique domains-file d))))
            (prompt-context
             (format nil "Domains detected: ~a — use `pq_query('(-> (activate \"...\" :boost (~a)) (:take 5))')`"
                     domain-str domain-str)))
          (empty-response)))))
