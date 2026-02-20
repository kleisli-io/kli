;;;; claude-hooks - Prompt Skip Helpers
;;;;
;;;; Classification predicates for UserPromptSubmit hooks that need to
;;;; filter trivial, slash-command, or confirmation prompts.

(in-package :claude-hooks)

(defparameter *confirmation-words*
  '("yes" "no" "y" "n" "ok" "approved" "continue" "looks good" "lgtm")
  "Words that indicate a simple confirmation prompt.
Exported so hooks can extend with (push \"new-word\" *confirmation-words*).")

(defun trivial-prompt-p (prompt)
  "True if PROMPT is NIL or shorter than 20 characters."
  (or (null prompt) (< (length prompt) 20)))

(defun slash-command-p (prompt)
  "True if PROMPT starts with a slash command (after trimming whitespace)."
  (and prompt
       (> (length prompt) 0)
       (let ((trimmed (string-left-trim '(#\Space #\Tab) prompt)))
         (and (> (length trimmed) 0)
              (char= (char trimmed 0) #\/)))))

(defun confirmation-p (prompt)
  "True if PROMPT is a simple confirmation word (case-insensitive).
Matches against *CONFIRMATION-WORDS*."
  (and prompt
       (member (string-downcase (string-trim '(#\Space #\Tab #\Newline) prompt))
               *confirmation-words* :test #'string=)
       t))

(defun skip-prompt-p (prompt)
  "True if PROMPT should be skipped by a UserPromptSubmit hook.
Combines trivial-prompt-p, slash-command-p, and confirmation-p."
  (or (trivial-prompt-p prompt)
      (slash-command-p prompt)
      (confirmation-p prompt)))
