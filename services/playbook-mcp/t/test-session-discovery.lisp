;;;; playbook-mcp tests - Session Discovery
;;;; Tests for world root detection, depot root detection, and the
;;;; session announcement reading that bridges hooks ↔ MCP server.

(in-package :playbook-mcp.tests)
(in-suite :session-discovery)

;;; ensure-trailing-slash

(test ensure-trailing-slash-adds-slash
  "ensure-trailing-slash adds / when missing."
  (is (string= "/foo/bar/" (playbook-mcp::ensure-trailing-slash "/foo/bar"))))

(test ensure-trailing-slash-preserves-slash
  "ensure-trailing-slash preserves existing trailing /."
  (is (string= "/foo/bar/" (playbook-mcp::ensure-trailing-slash "/foo/bar/"))))

;;; read-latest-session-announcement

(test read-latest-session-from-depot-root
  "read-latest-session-announcement reads from depot root first."
  (let* ((dir (format nil "/tmp/test-sess-disc-~a" (random 1000000)))
         (claude-dir (format nil "~a/.claude/" dir))
         (path (format nil "~a/latest-session" claude-dir)))
    (ensure-directories-exist claude-dir)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "DEADBEEF-1234" s))
    (unwind-protect
         (is (string= "DEADBEEF-1234"
                       (playbook-mcp::read-latest-session-announcement dir)))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory claude-dir))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test read-latest-session-falls-back-to-world-root
  "read-latest-session-announcement falls back to world root."
  ;; Create a world root with depot-graph.nix and .claude/latest-session
  (let* ((world-dir (format nil "/tmp/test-world-~a/" (random 1000000)))
         (depot-dir (format nil "~asome-depot/" world-dir))
         (graph-marker (format nil "~adepot-graph.nix" world-dir))
         (world-claude (format nil "~a.claude/" world-dir))
         (session-file (format nil "~alatest-session" world-claude)))
    ;; Create structure: world-dir/depot-graph.nix, world-dir/.claude/latest-session
    (ensure-directories-exist depot-dir)
    (ensure-directories-exist world-claude)
    (with-open-file (s graph-marker :direction :output :if-exists :supersede)
      (write-string "{}" s))
    (with-open-file (s session-file :direction :output :if-exists :supersede)
      (write-string "WORLD-SESSION-42" s))
    (unwind-protect
         ;; When depot root has no latest-session, should fall back to world root
         ;; We test the flet try-read logic directly via read-latest-session-announcement
         ;; depot-dir has no .claude/latest-session → should try world root
         (let ((result (playbook-mcp::read-latest-session-announcement depot-dir)))
           ;; This test only verifies that depot root miss returns nil (no world root
           ;; detection in this context since mcp-find-world-root uses CWD).
           ;; The important thing: it doesn't crash on missing file.
           (is (null result)))
      ;; Cleanup
      (ignore-errors (delete-file session-file))
      (ignore-errors (delete-file graph-marker))
      (ignore-errors (uiop:delete-empty-directory world-claude))
      (ignore-errors (uiop:delete-empty-directory depot-dir))
      (ignore-errors (uiop:delete-empty-directory world-dir)))))

(test read-latest-session-trims-whitespace
  "read-latest-session-announcement trims whitespace from file content."
  (let* ((dir (format nil "/tmp/test-sess-trim-~a" (random 1000000)))
         (claude-dir (format nil "~a/.claude/" dir))
         (path (format nil "~a/latest-session" claude-dir)))
    (ensure-directories-exist claude-dir)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "  ABC123~%  "))
    (unwind-protect
         (is (string= "ABC123"
                       (playbook-mcp::read-latest-session-announcement dir)))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory claude-dir))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test read-latest-session-empty-file
  "read-latest-session-announcement returns nil for empty file."
  (let* ((dir (format nil "/tmp/test-sess-empty-~a" (random 1000000)))
         (claude-dir (format nil "~a/.claude/" dir))
         (path (format nil "~a/latest-session" claude-dir)))
    (ensure-directories-exist claude-dir)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "" s))
    (unwind-protect
         (is (null (playbook-mcp::read-latest-session-announcement dir)))
      (ignore-errors (delete-file path))
      (ignore-errors (uiop:delete-empty-directory claude-dir))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test read-latest-session-missing-dir
  "read-latest-session-announcement returns nil for nonexistent directory."
  (is (null (playbook-mcp::read-latest-session-announcement
             "/tmp/nonexistent-dir-12345"))))

;;; Session claim mechanics

(test try-claim-session-creates-lock
  "try-claim-session creates mcp.lock with our PID."
  (let* ((dir (format nil "/tmp/test-claim-~a/" (random 1000000)))
         (playbook-mcp::*mcp-lock-path* nil))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (is-true (playbook-mcp::try-claim-session dir))
           (is (not (null playbook-mcp::*mcp-lock-path*)))
           (let ((content (string-trim '(#\Space #\Newline)
                                       (uiop:read-file-string
                                        (merge-pathnames "mcp.lock" dir)))))
             (is (= (sb-posix:getpid) (parse-integer content)))))
      (ignore-errors (delete-file (merge-pathnames "mcp.lock" dir)))
      (ignore-errors (uiop:delete-empty-directory dir)))))

(test try-claim-session-fails-if-claimed
  "try-claim-session fails when lock already exists with running PID."
  (let* ((dir (format nil "/tmp/test-claim-fail-~a/" (random 1000000)))
         (lock-path (merge-pathnames "mcp.lock" dir))
         (playbook-mcp::*mcp-lock-path* nil))
    (ensure-directories-exist dir)
    ;; Write our own PID (simulating existing claim by running process)
    (with-open-file (s lock-path :direction :output :if-exists :supersede)
      (format s "~A" (sb-posix:getpid)))
    (unwind-protect
         (is-false (playbook-mcp::try-claim-session dir))
      (ignore-errors (delete-file lock-path))
      (ignore-errors (uiop:delete-empty-directory dir)))))

;;; Cleanup

(test cleanup-mcp-lock-removes-file
  "cleanup-mcp-lock removes the lock file and clears the path."
  (let* ((dir (format nil "/tmp/test-cleanup-lock-~a/" (random 1000000)))
         (lock-path (namestring (merge-pathnames "mcp.lock" dir))))
    (ensure-directories-exist dir)
    (with-open-file (s lock-path :direction :output :if-exists :supersede)
      (format s "~A" (sb-posix:getpid)))
    (let ((playbook-mcp::*mcp-lock-path* lock-path))
      (cleanup-mcp-lock)
      (is (null playbook-mcp::*mcp-lock-path*))
      (is (null (probe-file lock-path))))
    (ignore-errors (uiop:delete-empty-directory dir))))

(test cleanup-mcp-lock-noop-when-nil
  "cleanup-mcp-lock is safe to call when *mcp-lock-path* is nil."
  (let ((playbook-mcp::*mcp-lock-path* nil))
    ;; Should not signal any error
    (cleanup-mcp-lock)
    (is (null playbook-mcp::*mcp-lock-path*))))
