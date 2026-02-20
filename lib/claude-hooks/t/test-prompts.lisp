;;;; claude-hooks Tests - Prompt Skip Helpers

(in-package :claude-hooks.tests)
(in-suite :prompts)

;;; trivial-prompt-p

(test trivial-nil
  "trivial-prompt-p returns true for nil."
  (is-true (trivial-prompt-p nil)))

(test trivial-short
  "trivial-prompt-p returns true for short prompts."
  (is-true (trivial-prompt-p "ok"))
  (is-true (trivial-prompt-p "fix the bug")))

(test trivial-long-enough
  "trivial-prompt-p returns false for prompts >= 20 chars."
  (is-false (trivial-prompt-p "deploy the NixOS module now")))

;;; slash-command-p

(test slash-basic
  "slash-command-p detects slash commands."
  (is-true (slash-command-p "/commit"))
  (is-true (slash-command-p "/plan some feature")))

(test slash-with-leading-space
  "slash-command-p handles leading whitespace."
  (is-true (slash-command-p "  /commit")))

(test slash-not-slash
  "slash-command-p returns false for non-slash prompts."
  (is-false (slash-command-p "deploy the module"))
  (is-false (slash-command-p ""))
  (is-false (slash-command-p nil)))

;;; confirmation-p

(test confirmation-basic
  "confirmation-p detects confirmation words."
  (is-true (confirmation-p "yes"))
  (is-true (confirmation-p "no"))
  (is-true (confirmation-p "y"))
  (is-true (confirmation-p "ok"))
  (is-true (confirmation-p "approved"))
  (is-true (confirmation-p "lgtm")))

(test confirmation-case-insensitive
  "confirmation-p is case-insensitive."
  (is-true (confirmation-p "YES"))
  (is-true (confirmation-p "Approved"))
  (is-true (confirmation-p "LGTM")))

(test confirmation-with-whitespace
  "confirmation-p trims whitespace."
  (is-true (confirmation-p "  yes  "))
  (is-true (confirmation-p (format nil "  ok~%"))))

(test confirmation-multi-word
  "confirmation-p handles multi-word confirmations."
  (is-true (confirmation-p "looks good")))

(test confirmation-not-confirmation
  "confirmation-p returns false for non-confirmation prompts."
  (is-false (confirmation-p "deploy the module"))
  (is-false (confirmation-p nil)))

;;; skip-prompt-p

(test skip-trivial
  "skip-prompt-p skips trivial prompts."
  (is-true (skip-prompt-p "ok"))
  (is-true (skip-prompt-p nil)))

(test skip-slash
  "skip-prompt-p skips slash commands."
  (is-true (skip-prompt-p "/commit message here")))

(test skip-confirmation
  "skip-prompt-p skips confirmations."
  (is-true (skip-prompt-p "looks good")))

(test skip-real-prompt
  "skip-prompt-p returns false for real prompts."
  (is-false (skip-prompt-p "deploy NixOS module with SBCL integration"))
  (is-false (skip-prompt-p "fix the authentication bug in login flow")))
