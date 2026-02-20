;;; Playbook MCP Library - File Locking
;;; Cross-process file locking using POSIX lockf.
;;;
;;; Provides with-file-lock macro for atomic file operations
;;; across multiple MCP server instances.

(in-package #:playbook-mcp)

;;; File locking macro using POSIX lockf
;;;
;;; lockf commands:
;;; - F-LOCK (1): Exclusive lock, blocks until acquired
;;; - F-TLOCK (2): Try exclusive lock, returns immediately if unavailable
;;; - F-ULOCK (0): Unlock

(defmacro with-file-lock ((path &key (wait t)) &body body)
  "Execute BODY with exclusive file lock on PATH.
   Creates a .lock file adjacent to PATH for coordination.

   If WAIT is T (default), blocks until lock acquired.
   If WAIT is NIL, returns NIL immediately if lock unavailable.

   Uses POSIX lockf for cross-process synchronization.
   The lock is automatically released on normal exit or error."
  (let ((lock-path (gensym "LOCK-PATH"))
        (stream (gensym "STREAM"))
        (fd (gensym "FD"))
        (cmd (gensym "CMD")))
    `(let* ((,lock-path (format nil "~A.lock" ,path))
            (,cmd (if ,wait sb-posix:f-lock sb-posix:f-tlock)))
       (ensure-directories-exist ,lock-path)
       (with-open-file (,stream ,lock-path
                                :direction :io
                                :if-exists :overwrite
                                :if-does-not-exist :create)
         (let ((,fd (sb-sys:fd-stream-fd ,stream)))
           (handler-case
               (progn
                 (sb-posix:lockf ,fd ,cmd 0)
                 (unwind-protect
                     (multiple-value-prog1
                         (progn ,@body))
                   (sb-posix:lockf ,fd sb-posix:f-ulock 0)))
             (sb-posix:syscall-error (e)
               ;; EAGAIN/EWOULDBLOCK means lock unavailable (non-blocking mode)
               (if (member (sb-posix:syscall-errno e)
                           (list sb-posix:eagain sb-posix:ewouldblock))
                   nil
                   (error e)))))))))
