;;; test-patterns.lisp - Minimal pattern struct for PQ tests
;;;
;;; Provides the same pattern struct interface as playbook-mcp::pattern
;;; without depending on the full MCP server library.

(defpackage #:kli-pattern
  (:use #:cl)
  (:export #:pattern #:make-pattern
           #:pattern-id #:pattern-domain #:pattern-content
           #:pattern-helpful #:pattern-harmful
           #:pattern-evolution-history #:pattern-embedding))

(in-package #:kli-pattern)

(defstruct (pattern (:conc-name pattern-))
  "Minimal pattern struct for testing PQ queries."
  (id nil :type (or null string))
  (domain nil :type (or null string))
  (content "" :type string)
  (helpful 0 :type integer)
  (harmful 0 :type integer)
  (evolution-history nil :type list)
  (embedding nil :type (or null list)))
