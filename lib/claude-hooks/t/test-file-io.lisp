;;;; claude-hooks Tests - File I/O Utilities

(in-package :claude-hooks.tests)
(in-suite :file-io)

;;; Helper: create a temp dir for tests
(defun make-test-dir ()
  (let ((dir (format nil "/tmp/claude-hooks-fio-~a/" (random 1000000))))
    (ensure-directories-exist dir)
    dir))

(defun cleanup-test-dir (dir)
  (uiop:delete-directory-tree (parse-namestring dir)
                              :validate t :if-does-not-exist :ignore))

;;; append-line

(test append-line-creates-file
  "append-line creates file and writes line."
  (let* ((dir (make-test-dir))
         (f (format nil "~atest.txt" dir)))
    (unwind-protect
         (progn
           (append-line f "hello")
           (is (equal '("hello") (read-lines f))))
      (cleanup-test-dir dir))))

(test append-line-appends
  "append-line appends to existing file."
  (let* ((dir (make-test-dir))
         (f (format nil "~atest.txt" dir)))
    (unwind-protect
         (progn
           (append-line f "first")
           (append-line f "second")
           (is (equal '("first" "second") (read-lines f))))
      (cleanup-test-dir dir))))

;;; append-line-unique

(test append-line-unique-deduplicates
  "append-line-unique does not add duplicate lines."
  (let* ((dir (make-test-dir))
         (f (format nil "~atest.txt" dir)))
    (unwind-protect
         (progn
           (append-line-unique f "nix")
           (append-line-unique f "lisp")
           (append-line-unique f "nix")
           (is (equal '("nix" "lisp") (read-lines f))))
      (cleanup-test-dir dir))))

(test append-line-unique-creates
  "append-line-unique creates file on first call."
  (let* ((dir (make-test-dir))
         (f (format nil "~anew.txt" dir)))
    (unwind-protect
         (progn
           (append-line-unique f "first")
           (is (equal '("first") (read-lines f))))
      (cleanup-test-dir dir))))

;;; read-lines

(test read-lines-missing-file
  "read-lines returns nil for missing file."
  (is (null (read-lines "/tmp/nonexistent-file-abc123.txt"))))

;;; read-json-file

(test read-json-file-roundtrip
  "read-json-file parses JSON written by atomic-write-json."
  (let* ((dir (make-test-dir))
         (f (format nil "~adata.json" dir))
         (data (make-ht "key" "value" "count" 42)))
    (unwind-protect
         (progn
           (atomic-write-json f data)
           (let ((loaded (read-json-file f)))
             (is (string= "value" (gethash "key" loaded)))
             (is (= 42 (gethash "count" loaded)))))
      (cleanup-test-dir dir))))

(test read-json-file-missing
  "read-json-file returns default for missing file."
  (is (eq :fallback (read-json-file "/tmp/nonexistent-abc.json" :fallback))))

(test read-json-file-default-nil
  "read-json-file returns nil by default for missing file."
  (is (null (read-json-file "/tmp/nonexistent-abc.json"))))

;;; atomic-write-json

(test atomic-write-json-creates-dirs
  "atomic-write-json creates parent directories."
  (let* ((dir (make-test-dir))
         (f (format nil "~adeep/nested/data.json" dir)))
    (unwind-protect
         (progn
           (atomic-write-json f (make-ht "x" 1))
           (is (= 1 (gethash "x" (read-json-file f)))))
      (cleanup-test-dir dir))))
