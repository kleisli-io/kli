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

(defun pattern-beta-score (pattern)
  "Bayesian Beta-Binomial posterior mean. Returns (h+1)/(h+m+2) in [0.0, 1.0].
   h=0,m=0 → 0.5 (uncertain). h=16,m=0 → 0.944 (confident good). h=1,m=2 → 0.40 (likely harmful)."
  (let ((h (pattern-helpful pattern)) (m (pattern-harmful pattern)))
    (/ (float (1+ h)) (float (+ h m 2)))))

(defun pattern-beta-variance (pattern)
  "Variance of Beta(h+1,m+1) — uncertainty measure for feedback prioritization.
   High variance = most informative to ask about next."
  (let* ((h (pattern-helpful pattern)) (m (pattern-harmful pattern))
         (a (1+ h)) (b (1+ m)) (n (+ a b)))
    (/ (* (float a) (float b)) (* (float (* n n)) (float (1+ n))))))

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
