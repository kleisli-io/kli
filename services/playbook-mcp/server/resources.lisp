;;; Playbook MCP Server - MCP Resource Definitions
;;; Resources for accessing pattern data

(in-package #:playbook-mcp)

;;; All patterns resource

(mcp-framework:define-resource "playbook://patterns"
    (:name "All Patterns"
     :description "Complete list of all loaded patterns as JSON"
     :mime-type "application/json")
  (with-output-to-string (s)
    (yason:encode
     (mapcar #'pattern-to-json (list-patterns))
     s)))

;;; Proven patterns resource

(mcp-framework:define-resource "playbook://proven"
    (:name "Proven Patterns"
     :description "Patterns with helpful >= 3"
     :mime-type "application/json")
  (with-output-to-string (s)
    (yason:encode
     (mapcar #'pattern-to-json (proven-patterns))
     s)))

;;; Stats resource

(mcp-framework:define-resource "playbook://stats"
    (:name "Playbook Statistics"
     :description "Statistics about loaded patterns"
     :mime-type "application/json")
  (let ((patterns (list-patterns))
        (stats (make-hash-table :test 'equal)))
    ;; Count by domain
    (let ((by-domain (make-hash-table :test 'equal)))
      (dolist (p patterns)
        (incf (gethash (pattern-domain p) by-domain 0)))
      (setf (gethash "byDomain" stats) by-domain))
    ;; Total counts
    (setf (gethash "totalPatterns" stats) (length patterns))
    (setf (gethash "totalHelpful" stats)
          (reduce #'+ patterns :key #'pattern-helpful))
    (setf (gethash "totalHarmful" stats)
          (reduce #'+ patterns :key #'pattern-harmful))
    (setf (gethash "embeddingsCached" stats) (embedding-cache-size))
    (with-output-to-string (s)
      (yason:encode stats s))))
