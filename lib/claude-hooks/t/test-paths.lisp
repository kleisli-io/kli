;;;; claude-hooks Tests - Session Path Helpers

(in-package :claude-hooks.tests)
(in-suite :paths)

(test session-dir-basic
  "session-dir extracts correct path from input."
  (let ((dir (session-dir (make-ht "cwd" "/home/user/project" "session_id" "abc123"))))
    (is (string= "/home/user/project/.claude/sessions/abc123" dir))))

(test session-dir-missing-session-id
  "session-dir returns nil when session_id missing."
  (is (null (session-dir (make-ht "cwd" "/home/user")))))

(test session-dir-missing-cwd
  "session-dir returns nil when cwd missing."
  (is (null (session-dir (make-ht "session_id" "abc")))))

(test session-dir-empty-input
  "session-dir returns nil for empty input."
  (is (null (session-dir (make-ht)))))

(test session-file-single-part
  "session-file with one path part."
  (let ((f (session-file (make-ht "cwd" "/proj" "session_id" "s1") "file.txt")))
    (is (string= "/proj/.claude/sessions/s1/file.txt" f))))

(test session-file-multiple-parts
  "session-file with multiple path parts."
  (let ((f (session-file (make-ht "cwd" "/proj" "session_id" "s1")
                         "playbook" "activated.jsonl")))
    (is (string= "/proj/.claude/sessions/s1/playbook/activated.jsonl" f))))

(test session-file-nil-propagation
  "session-file returns nil when session-dir returns nil."
  (is (null (session-file (make-ht "cwd" "/proj") "file.txt"))))

(test ensure-session-dir-creates
  "ensure-session-dir creates directory and returns path."
  (let* ((tmp (format nil "/tmp/claude-hooks-test-~a" (random 1000000)))
         (input (make-ht "cwd" tmp "session_id" "test-ensure"))
         (dir (ensure-session-dir input "playbook")))
    (unwind-protect
         (progn
           (is (stringp dir))
           (is (probe-file (format nil "~a/" dir))))
      ;; Cleanup
      (uiop:delete-directory-tree (parse-namestring (format nil "~a/" tmp))
                                  :validate t :if-does-not-exist :ignore))))

(test ensure-session-dir-nil-input
  "ensure-session-dir returns nil for incomplete input."
  (is (null (ensure-session-dir (make-ht "cwd" "/proj")))))

;;; --------------------------------------------------------------------------
;;; normalize-file-path tests
;;; --------------------------------------------------------------------------

(test normalize-file-path-nil
  "normalize-file-path returns nil for nil input."
  (is (null (normalize-file-path nil))))

(test normalize-file-path-empty-string
  "normalize-file-path returns nil for empty string."
  (is (null (normalize-file-path ""))))

(test normalize-file-path-non-string
  "normalize-file-path returns nil for non-string input."
  (is (null (normalize-file-path 42))))

(test normalize-file-path-existing-file
  "normalize-file-path resolves existing files via truename."
  (let ((path (normalize-file-path "/tmp")))
    (is (stringp path))
    ;; truename should resolve any symlinks
    (is (char= #\/ (char path 0)))))

(test normalize-file-path-double-dot
  "normalize-file-path resolves .. components for existing paths."
  ;; Use /tmp/../tmp which exists on both sides of the ..
  (let ((a (normalize-file-path "/tmp"))
        (b (normalize-file-path "/tmp/../tmp")))
    (is (string= a b))))

(test normalize-file-path-non-existent
  "normalize-file-path handles non-existent files without error."
  (let ((path (normalize-file-path "/tmp/does-not-exist-normalize-test-12345.txt")))
    (is (stringp path))
    (is (search "does-not-exist-normalize-test-12345" path))))

(test normalize-file-path-idempotent
  "normalize-file-path is idempotent."
  (let* ((path "/tmp")
         (once (normalize-file-path path))
         (twice (normalize-file-path once)))
    (is (string= once twice))))

(test normalize-file-path-double-slash
  "normalize-file-path collapses double slashes."
  (let ((a (normalize-file-path "/tmp"))
        (b (normalize-file-path "//tmp")))
    (is (string= a b))))
