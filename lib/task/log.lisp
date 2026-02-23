(in-package #:task)

;;; Per-request elog cache
;;; Bound to a fresh hash-table in the HTTP request handler (server.lisp).
;;; When non-NIL, elog-load returns cached results instead of re-reading disk.
;;; NIL means no caching (stdio mode or outside request scope).

(defvar *elog-cache* nil
  "Per-request cache: hash-table mapping path-string -> event-log.
   Bind to (make-hash-table :test 'equal) around each HTTP request.")

(defstruct event-log
  (events nil :type list)
  (clock (make-vector-clock) :type vector-clock)
  (path nil :type (or null pathname string)))

(defun elog-append (log event)
  "Append event and merge its clock. Mutates LOG."
  (push event (event-log-events log))
  (setf (event-log-clock log)
        (vc-merge (event-log-clock log) (event-clock event)))
  log)

(declaim (fixnum *elog-save-counter*))
(sb-ext:defglobal *elog-save-counter* 0
  "Monotonic counter for unique temp filenames across threads.")

(defun elog-save (log)
  "Save event log to JSONL file (atomic: write tmp then rename).
   Uses a unique temp filename per write to prevent races when
   parallel sessions write to the same events.jsonl."
  (let* ((path (event-log-path log))
         (unique (sb-ext:atomic-incf *elog-save-counter*))
         (tmp (format nil "~A.~D-~D.tmp" path (sb-posix:getpid) unique)))
    (with-open-file (s tmp :direction :output :if-exists :supersede)
      (dolist (ev (reverse (event-log-events log)))
        (write-string (event-to-json-string ev) s)
        (terpri s)))
    (rename-file tmp path)))

(defun elog-load-from-disk (path)
  "Load event log from JSONL file (always reads disk).
   Skips corrupt lines with a warning instead of failing fatally."
  (let ((log (make-event-log :path path)))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (loop for line = (read-line s nil nil)
              while (and line (> (length line) 0))
              do (handler-case
                     (elog-append log (json-string-to-event line))
                   (error (e)
                     (warn "elog-load: skipping corrupt line in ~A: ~A" path e))))))
    log))

(defun elog-load (path)
  "Load event log from JSONL file. Uses per-request cache when *elog-cache* is bound.
   Falls through to disk read when cache is NIL (stdio mode)."
  (let ((key (namestring path)))
    (if *elog-cache*
        (or (gethash key *elog-cache*)
            (setf (gethash key *elog-cache*) (elog-load-from-disk path)))
        (elog-load-from-disk path))))

(defun elog-append-event (path event)
  "Append a single event to JSONL file using O_APPEND semantics.
   Safe for concurrent writers: POSIX guarantees atomic appends
   under PIPE_BUF (4096 bytes). Typical event lines are ~200-500 bytes."
  (with-open-file (s path :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (write-string (event-to-json-string event) s)
    (terpri s)))

(defun elog-merge (log1 log2)
  "Merge two event logs. Union by event ID, timestamp sort. Returns NEW log."
  (let ((seen (make-hash-table :test 'equal))
        (all-events nil)
        (result (make-event-log :path (or (event-log-path log1)
                                          (event-log-path log2)))))
    (dolist (ev (append (event-log-events log1) (event-log-events log2)))
      (unless (gethash (event-id ev) seen)
        (setf (gethash (event-id ev) seen) t)
        (push ev all-events)))
    (setf (event-log-events result)
          (sort all-events #'< :key #'event-timestamp))
    (setf (event-log-clock result)
          (vc-merge (event-log-clock log1) (event-log-clock log2)))
    result))
