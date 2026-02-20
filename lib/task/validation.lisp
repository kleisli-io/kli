;;; Task Name Validation
;;;
;;; Provides semantic validation for task names to prevent meaningless
;;; identifiers like "P1", "phase-1", "stuff" from polluting the task graph.

(defpackage #:task-validation
  (:use #:cl)
  (:export
   ;; Validation
   #:validate-task-name
   #:validation-result
   #:make-validation-result
   #:validation-result-valid-p
   #:validation-result-reason
   #:validation-result-suggestion
   ;; Name generation
   #:suggest-name-from-description
   #:slugify
   ;; Configuration (allow customization)
   #:*allowed-prefixes*
   #:*action-verbs*
   #:*vague-words*
   #:*stopwords*
   #:*min-single-word-length*))

(in-package #:task-validation)

;;; Configuration

(defparameter *allowed-prefixes*
  '("phase" "research" "implement" "design" "fix" "refactor" "test" "explore"
    "spike" "investigate" "analyze" "document" "review" "plan" "create" "add"
    "update" "remove" "delete" "enable" "disable" "configure" "migrate" "verify")
  "Structural prefixes that can precede numbers in task names.")

(defparameter *action-verbs*
  '("add" "remove" "fix" "implement" "create" "update" "delete" "refactor"
    "move" "rename" "extract" "inline" "optimize" "validate" "check" "verify"
    "handle" "process" "parse" "format" "convert" "transform" "migrate"
    "enable" "disable" "configure" "setup" "teardown" "init" "cleanup"
    "build" "deploy" "debug" "trace" "log" "audit" "fetch" "load" "save"
    "store" "cache" "sync" "merge" "split" "encode" "decode" "encrypt"
    "filter" "sort" "group" "aggregate" "reduce" "map" "collect" "ensure"
    "connect" "disconnect" "open" "close" "read" "write" "send" "receive"
    "integrate" "enhance" "improve" "simplify" "consolidate" "normalize")
  "Action verbs that indicate good naming grammar.")

(defparameter *vague-words*
  '("stuff" "thing" "things" "misc" "other" "temp" "tmp" "wip" "draft"
    "new" "old" "foo" "bar" "baz" "asdf" "qwer" "xxx" "yyy" "core" "main")
  "Words too vague to be sole semantic content.")

(defparameter *stopwords*
  '("a" "an" "the" "is" "are" "was" "were" "be" "been" "being"
    "to" "of" "in" "for" "on" "with" "at" "by" "from" "as"
    "into" "through" "during" "before" "after" "above" "below"
    "this" "that" "these" "those" "it" "its" "and" "or" "but"
    "if" "then" "else" "when" "where" "why" "how" "all" "each"
    "every" "both" "few" "more" "most" "other" "some" "such"
    "no" "nor" "not" "only" "own" "same" "so" "than" "too"
    "very" "just" "can" "will" "should" "would" "could" "may"
    "might" "must" "shall" "need" "has" "have" "had" "do" "does" "did")
  "Common stopwords to filter from slugs.")

(defparameter *min-single-word-length* 6
  "Minimum length for a single-word name to be considered descriptive.")

;;; Result Structure

(defstruct validation-result
  "Result of task name validation."
  (valid-p nil :type boolean)
  (reason nil :type (or null string))
  (suggestion nil :type (or null string)))

;;; Helper Functions

(defun split-kebab (name)
  "Split kebab-case NAME into list of parts."
  (let ((parts nil)
        (current (make-string-output-stream)))
    (loop for char across (string-downcase name)
          if (char= char #\-)
            do (let ((part (get-output-stream-string current)))
                 (when (> (length part) 0) (push part parts)))
               (setf current (make-string-output-stream))
          else do (write-char char current))
    (let ((final (get-output-stream-string current)))
      (when (> (length final) 0) (push final parts)))
    (nreverse parts)))

(defun numeric-p (s)
  "Check if S is purely numeric."
  (and (> (length s) 0) (every #'digit-char-p s)))

(defun letter-number-p (s)
  "Check if S is a single letter followed by numbers (P1, T2)."
  (and (>= (length s) 2)
       (alpha-char-p (char s 0))
       (every #'digit-char-p (subseq s 1))))

(defun prefix-part-p (p)
  "Check if P is a known structural prefix."
  (member p *allowed-prefixes* :test #'string=))

(defun action-verb-p (w)
  "Check if W is a known action verb."
  (member w *action-verbs* :test #'string=))

(defun vague-word-p (w)
  "Check if W is a known vague word."
  (member w *vague-words* :test #'string=))

(defun meaningful-part-p (p)
  "Check if P is meaningful (not just number or letter+number)."
  (and (> (length p) 1)
       (not (numeric-p p))
       (not (letter-number-p p))))

(defun filter-structural-parts (parts)
  "Remove leading prefix and numbers, keep semantic core."
  (let ((result parts))
    ;; Remove leading prefix if present
    (when (and result (prefix-part-p (first result)))
      (setf result (rest result)))
    ;; Remove leading number if present (after prefix)
    (when (and result (numeric-p (first result)))
      (setf result (rest result)))
    ;; Filter any remaining pure numbers
    (remove-if #'numeric-p result)))

(defun has-good-grammar-p (parts)
  "Check for valid naming grammar patterns."
  (let ((semantic (filter-structural-parts parts)))
    (cond
      ;; No semantic content
      ((null semantic) nil)
      ;; Starts with action verb - need object too
      ((action-verb-p (first semantic))
       (> (length semantic) 1))
      ;; Multiple meaningful words - noun phrase
      ((and (> (length semantic) 1)
            (every #'meaningful-part-p semantic))
       t)
      ;; Single substantial word
      ((and (= (length semantic) 1)
            (>= (length (first semantic)) *min-single-word-length*)
            (not (vague-word-p (first semantic))))
       t)
      (t nil))))

;;; Main API

(defun validate-task-name (name)
  "Validate NAME is semantically descriptive.
   Returns a VALIDATION-RESULT struct."
  (let* ((parts (split-kebab name))
         (semantic (filter-structural-parts parts)))
    (cond
      ;; Single letter+number like P1, T2
      ((and (= (length parts) 1) (letter-number-p (first parts)))
       (make-validation-result
        :valid-p nil
        :reason "Names like 'P1' are not descriptive"
        :suggestion "Use: <verb>-<object> (e.g., 'implement-auth')"))

      ;; Pure numeric
      ((and parts (every #'numeric-p parts))
       (make-validation-result
        :valid-p nil
        :reason "Pure numeric names not allowed"
        :suggestion "Use: phase-<N>-<description>"))

      ;; No semantic content after filtering
      ((null semantic)
       (make-validation-result
        :valid-p nil
        :reason "No semantic content after structural prefix"
        :suggestion "Add description: phase-1-<what-it-does>"))

      ;; Insufficient descriptive content
      ((not (has-good-grammar-p parts))
       (make-validation-result
        :valid-p nil
        :reason "Insufficient descriptive content"
        :suggestion "Use: <verb>-<object>, <noun>-<noun>, or 6+ char noun"))

      ;; Valid!
      (t (make-validation-result :valid-p t)))))

;;; Name Generation

(defun slugify (text &key (max-words 4))
  "Convert TEXT to a kebab-case slug suitable for task names.
   Filters stopwords and limits to MAX-WORDS meaningful words."
  (let* (;; Replace path separators and underscores with spaces
         (normalized (substitute #\Space #\/ (substitute #\Space #\_ text)))
         ;; Remove non-alphanumeric except spaces and hyphens
         (cleaned (remove-if-not
                   (lambda (c) (or (alphanumericp c) (char= c #\Space) (char= c #\-)))
                   normalized))
         ;; Split on whitespace, filter empty
         (words (remove-if (lambda (w) (= (length w) 0))
                           (loop for start = 0 then (1+ end)
                                 for end = (position #\Space cleaned :start start)
                                 collect (string-downcase (subseq cleaned start (or end (length cleaned))))
                                 while end)))
         ;; Filter stopwords and short words
         (meaningful (remove-if (lambda (w)
                                  (or (< (length w) 3)
                                      (member w *stopwords* :test #'string=)))
                                words))
         ;; Take first max-words
         (selected (subseq meaningful 0 (min max-words (length meaningful)))))
    (if selected
        (format nil "~{~A~^-~}" selected)
        "")))

(defun suggest-name-from-description (description &key (max-words 4))
  "Generate a valid task name slug from DESCRIPTION.
   Alias for SLUGIFY with default settings."
  (slugify description :max-words max-words))
