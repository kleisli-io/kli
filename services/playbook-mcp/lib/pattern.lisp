;;; Playbook MCP Server - Pattern Data Model
;;; Pattern struct with evolution support

(in-package #:playbook-mcp)

;;; Pattern structure

(defstruct (pattern (:conc-name pattern-))
  "A playbook pattern with feedback tracking and evolution history."
  ;; Core identity
  (id nil :type (or null string))           ; e.g., "lisp-000042"
  (domain nil :type (or null string))       ; e.g., "lisp"

  ;; Content
  (content "" :type string)                 ; Pattern text

  ;; Feedback counters
  (helpful 0 :type integer)
  (harmful 0 :type integer)

  ;; Evolution history - list of (timestamp reason old-content)
  (evolution-history nil :type list)

  ;; Source tracking
  (source-file nil :type (or null string))  ; Path to playbook.md
  (line-number nil :type (or null integer)) ; Line in source file

  ;; Embedding cache (transient, not persisted)
  (embedding nil :type (or null list)))     ; 768-dim vector from Ollama

;;; Pattern ID generation

(defun generate-pattern-id (domain)
  "Generate a new pattern ID for the given domain.
   Format: domain-NNNNNN where N is a random 6-digit number."
  (format nil "~A-~6,'0D" domain (random 1000000)))

;;; Pattern effectiveness

(defun pattern-effectiveness (pattern)
  "Calculate pattern effectiveness score.
   Returns helpful - harmful, clamped to 0."
  (max 0 (- (pattern-helpful pattern) (pattern-harmful pattern))))

;;; Pattern serialization

(defun pattern-to-line (pattern)
  "Serialize pattern to playbook.md line format.
   Format: [domain-NNNNNN] helpful=N harmful=M :: content"
  (format nil "[~A] helpful=~D harmful=~D :: ~A"
          (pattern-id pattern)
          (pattern-helpful pattern)
          (pattern-harmful pattern)
          (pattern-content pattern)))

(defun pattern-to-json (pattern)
  "Convert pattern to JSON-compatible alist for MCP responses."
  `(("id" . ,(pattern-id pattern))
    ("domain" . ,(pattern-domain pattern))
    ("content" . ,(pattern-content pattern))
    ("helpful" . ,(pattern-helpful pattern))
    ("harmful" . ,(pattern-harmful pattern))
    ("effectiveness" . ,(pattern-effectiveness pattern))
    ,@(when (pattern-evolution-history pattern)
        `(("evolutionCount" . ,(length (pattern-evolution-history pattern)))))))
