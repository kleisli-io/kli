;;;; playbook-hooks - Package Definition
;;;;
;;;; Shared library for playbook hook programs. Provides domain detection,
;;;; co-application ledger utilities, and session I/O for activated patterns.

(defpackage :playbook-hooks
  (:use :cl)
  (:export
   ;; Domain detection
   #:*domain-keywords*
   #:*extension-domains*
   #:detect-domains-from-text
   #:detect-domain-from-path

   ;; Co-application utilities
   #:co-app-key
   #:generate-pairs
   #:read-co-app-ledger
   #:update-co-app-ledger
   #:save-co-app-ledger
   #:co-app-ledger-path

   ;; Session I/O
   #:read-activated-ids))
