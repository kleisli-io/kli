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

;;; --- Corrupt-line quarantine + warning dedupe ---
;;; Malformed events.jsonl lines:
;;;   - are appended raw to <path>.quarantine (one sidecar per task);
;;;   - emit a single structured log line on the first encounter per
;;;     (path, line-hash) pair, deduped via *elog-warn-seen*;
;;;   - subsequent encounters bump a counter without re-logging.
;;; The dedupe table is process-global; restart clears it.

(defvar *elog-warn-seen* (make-hash-table :test 'equal)
  "Dedupe table: key (path . sxhash(line)) → counter cons (first-ts . count).")

(defvar *elog-warn-lock* (bt:make-lock "elog-warn")
  "Protects *elog-warn-seen* from concurrent access.")

(defparameter *elog-quarantine-cap* (* 1024 1024)
  "Maximum bytes retained per quarantine sidecar. When exceeded, the head
   is dropped on next write (truncate-and-keep-tail policy).")

(defun quarantine-path (path)
  "Sidecar file path for quarantined lines."
  (concatenate 'string (namestring path) ".quarantine"))

(defun rotate-quarantine-if-needed (qpath)
  "Truncate the head of QPATH if its size exceeds *elog-quarantine-cap*.
   Keeps the most recent ~half of the cap."
  (when (and (probe-file qpath)
             (> (with-open-file (s qpath) (file-length s))
                *elog-quarantine-cap*))
    (let* ((bytes (with-open-file (s qpath :element-type '(unsigned-byte 8))
                    (let ((len (file-length s))
                          (keep-from (- (file-length s) (truncate *elog-quarantine-cap* 2))))
                      (file-position s keep-from)
                      (let ((buf (make-array (- len keep-from)
                                             :element-type '(unsigned-byte 8))))
                        (read-sequence buf s)
                        buf))))
           (tmp (concatenate 'string qpath ".rot")))
      (with-open-file (s tmp :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
        (write-sequence bytes s))
      (rename-file tmp qpath))))

(defun quarantine-line (path line line-no condition)
  "Append the unparseable LINE plus a header to <PATH>.quarantine."
  (let ((qpath (quarantine-path path)))
    (handler-case
        (progn
          (rotate-quarantine-if-needed qpath)
          (with-open-file (s qpath :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
            (format s ";; line=~D ts=~D reason=~A~%~A~%"
                    line-no (get-universal-time) condition line)))
      (error () nil))))

(defun elog-warn-seen-p (path line)
  "T if (PATH,LINE) has already been warned about this process. Updates
   the dedupe counter as a side effect."
  (let ((key (cons (namestring path) (sxhash line))))
    (bt:with-lock-held (*elog-warn-lock*)
      (let ((entry (gethash key *elog-warn-seen*)))
        (cond
          (entry (incf (cdr entry)) t)
          (t (setf (gethash key *elog-warn-seen*)
                   (cons (get-universal-time) 1))
             nil))))))

(defun emit-elog-corrupt (path line-no condition)
  "Single-line structured log replacing the old multi-line warn."
  (format *error-output*
          "~&;; elog-quarantine path=~A line=~D reason=~A~%"
          path line-no condition))

(defun elog-warn-summary ()
  "Return a list of (path key-hash count first-ts) for inspection or
   periodic flush. Read-only."
  (bt:with-lock-held (*elog-warn-lock*)
    (let ((rows nil))
      (maphash (lambda (k v)
                 (push (list (car k) (cdr k) (cdr v) (car v)) rows))
               *elog-warn-seen*)
      rows)))

(defun elog-warn-reset ()
  "Clear the dedupe table. Counts are lost — call elog-warn-summary first."
  (bt:with-lock-held (*elog-warn-lock*)
    (clrhash *elog-warn-seen*)))

(defun elog-load-from-disk (path)
  "Load event log from JSONL file (always reads disk).
   Recoverable old-format lines are accepted via json-string-to-event's
   id synthesis. Truly unparseable lines are appended to <path>.quarantine
   and reported once per (path,line-hash) via *elog-warn-seen*."
  (let ((log (make-event-log :path path))
        (line-no 0))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (loop for line = (read-line s nil nil)
              while (and line (> (length line) 0))
              do (incf line-no)
                 (handler-case
                     (elog-append log (json-string-to-event line))
                   (error (e)
                     (let ((cstr (princ-to-string e)))
                       (quarantine-line path line line-no cstr)
                       (unless (elog-warn-seen-p path line)
                         (emit-elog-corrupt path line-no cstr))))))))
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

;;; --- Event-file verification + repair ---
;;; verify-events runs the tolerant parser in dry-run mode (no quarantine
;;; writes) and reports per-file counts. repair-events atomically rewrites
;;; each file to bake in synthesized ids, leaving truly-unparseable lines
;;; in the .quarantine sidecar. Both walk *tasks-root*/*/events.jsonl.

(defun synthesized-id-p (id)
  "T if ID was produced by SYNTHESIZE-EVENT-ID (prefix \"synth-\")."
  (and (stringp id)
       (>= (length id) 6)
       (string= "synth-" id :end2 6)))

(defun verify-events-file (path)
  "Dry-run parse PATH. Returns plist
   (:total N :good N :synthesized N :quarantined N :error STRING-OR-NIL).
   :synthesized counts lines that lack a top-level id (i.e. lines that
   would be rewritten if repair ran now). Lines whose stored id starts
   with the synth- prefix from a prior repair are NOT counted —
   verify reports the would-change delta so a clean result means
   repair would be a no-op.

   Does NOT write to <path>.quarantine and does NOT mutate the dedupe
   table. Process-global *synthesized-event-ids* still increments — that
   counter is intentionally cumulative across the daemon's lifetime."
  (let ((total 0) (good 0) (synth 0) (bad 0) (err nil))
    (handler-case
        (when (probe-file path)
          (with-open-file (s path :direction :input)
            (loop for line = (read-line s nil nil)
                  while (and line (> (length line) 0))
                  do (incf total)
                     (handler-case
                         (multiple-value-bind (ev synth-p)
                             (json-string-to-event line)
                           (declare (ignore ev))
                           (incf good)
                           (when synth-p (incf synth)))
                       (event-parse-error () (incf bad))
                       (error () (incf bad))))))
      (error (e) (setf err (princ-to-string e))))
    (list :total total :good good :synthesized synth
          :quarantined bad :error err)))

(defun %task-events-paths (tasks-root)
  "Return a list of (task-id . events.jsonl-path) for every immediate
   subdirectory of TASKS-ROOT that contains an events.jsonl file."
  (let ((result nil))
    (dolist (dir (directory (merge-pathnames "*/" (pathname tasks-root))))
      (let* ((id (car (last (pathname-directory dir))))
             (events (merge-pathnames "events.jsonl" dir)))
        (when (and id (probe-file events))
          (push (cons id (namestring events)) result))))
    (nreverse result)))

(defun verify-events-tree (&optional (tasks-root *tasks-root*))
  "Walk TASKS-ROOT/*/events.jsonl, dry-run-parse each, return plist:
     (:tasks N
      :totals (:lines L :good G :synthesized S :quarantined Q :errors E)
      :per-task ((:id ID :total N :good G :synthesized S :quarantined Q
                  :error STRING-OR-NIL) ...))
   Per-task entries are sorted: bad>0 first, then synth>0, then clean."
  (unless tasks-root
    (detect-tasks-root)
    (setf tasks-root *tasks-root*))
  (let ((per-task nil)
        (lines 0) (good 0) (synth 0) (bad 0) (errs 0))
    (dolist (entry (%task-events-paths tasks-root))
      (destructuring-bind (id . path) entry
        (let* ((res (verify-events-file path))
               (t-total (getf res :total))
               (t-good (getf res :good))
               (t-synth (getf res :synthesized))
               (t-bad (getf res :quarantined))
               (t-err (getf res :error)))
          (incf lines t-total) (incf good t-good)
          (incf synth t-synth) (incf bad t-bad)
          (when t-err (incf errs))
          (push (list :id id :total t-total :good t-good
                      :synthesized t-synth :quarantined t-bad
                      :error t-err)
                per-task))))
    (let* ((ordered (nreverse per-task))
           (sorted (stable-sort ordered
                                (lambda (a b)
                                  (let ((ab (getf a :quarantined))
                                        (bb (getf b :quarantined))
                                        (as (getf a :synthesized))
                                        (bs (getf b :synthesized)))
                                    (cond ((/= ab bb) (> ab bb))
                                          ((/= as bs) (> as bs))
                                          (t nil)))))))
      (list :tasks (length sorted)
            :totals (list :lines lines :good good
                          :synthesized synth :quarantined bad
                          :errors errs)
            :per-task sorted))))

(defun verify-events-tree-to-json (stream &optional (tasks-root *tasks-root*))
  "JSON-encode (verify-events-tree TASKS-ROOT) onto STREAM."
  (let* ((data (verify-events-tree tasks-root))
         (totals (getf data :totals))
         (per-task (getf data :per-task))
         (root-ht (make-hash-table :test 'equal))
         (totals-ht (make-hash-table :test 'equal))
         (per-task-list (mapcar
                         (lambda (e)
                           (let ((ht (make-hash-table :test 'equal)))
                             (setf (gethash "id" ht) (getf e :id)
                                   (gethash "total" ht) (getf e :total)
                                   (gethash "good" ht) (getf e :good)
                                   (gethash "synthesized" ht) (getf e :synthesized)
                                   (gethash "quarantined" ht) (getf e :quarantined))
                             (when (getf e :error)
                               (setf (gethash "error" ht) (getf e :error)))
                             ht))
                         per-task)))
    (setf (gethash "lines" totals-ht) (getf totals :lines)
          (gethash "good" totals-ht) (getf totals :good)
          (gethash "synthesized" totals-ht) (getf totals :synthesized)
          (gethash "quarantined" totals-ht) (getf totals :quarantined)
          (gethash "errors" totals-ht) (getf totals :errors))
    (setf (gethash "tasks_root" root-ht)
          (and tasks-root (namestring tasks-root))
          (gethash "tasks" root-ht) (getf data :tasks)
          (gethash "totals" root-ht) totals-ht
          (gethash "per_task" root-ht) per-task-list)
    (yason:encode root-ht stream)))

(defun repair-events-file (path)
  "Re-parse PATH with the tolerant parser, atomically rewrite to bake
   in synthesized ids. Truly-unparseable lines are appended to
   <path>.quarantine and excluded from the rewrite. Preserves event
   order. Returns plist
     (:read N :written N :synthesized S :quarantined Q :rewrote-p BOOL).
   Idempotent on a clean file (rewrote-p NIL when no changes needed).
   NOT safe to run while a writer holds PATH for append — stop the
   daemon or otherwise quiesce the task before invoking."
  (unless (probe-file path)
    (return-from repair-events-file
      (list :read 0 :written 0 :synthesized 0 :quarantined 0 :rewrote-p nil)))
  (let ((events nil) (read-n 0) (synth 0) (bad 0) (line-no 0))
    (with-open-file (s path :direction :input)
      (loop for line = (read-line s nil nil)
            while (and line (> (length line) 0))
            do (incf line-no) (incf read-n)
               (handler-case
                   (multiple-value-bind (ev synth-p)
                       (json-string-to-event line)
                     (when synth-p (incf synth))
                     (push ev events))
                 (error (e)
                   (incf bad)
                   (quarantine-line path line line-no
                                    (princ-to-string e))))))
    (let* ((ordered (nreverse events))
           (rewrite-needed (or (plusp synth) (plusp bad))))
      (when rewrite-needed
        (let* ((unique (sb-ext:atomic-incf *elog-save-counter*))
               (tmp (format nil "~A.~D-~D.repair.tmp"
                            path (sb-posix:getpid) unique)))
          (with-open-file (out tmp :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
            (dolist (ev ordered)
              (write-string (event-to-json-string ev) out)
              (terpri out)))
          (rename-file tmp path)))
      (list :read read-n
            :written (length ordered)
            :synthesized synth
            :quarantined bad
            :rewrote-p rewrite-needed))))

(defun repair-events-tree (&optional (tasks-root *tasks-root*))
  "Walk TASKS-ROOT/*/events.jsonl, repair-events-file each. Returns:
     (:tasks N
      :rewrote R
      :totals (:read L :written W :synthesized S :quarantined Q)
      :per-task ((:id ID :read R :written W :synthesized S
                  :quarantined Q :rewrote-p BOOL :error STRING-OR-NIL) ...))
   Per-task list contains only files that were rewritten or errored."
  (unless tasks-root
    (detect-tasks-root)
    (setf tasks-root *tasks-root*))
  (let* ((entries (%task-events-paths tasks-root))
         (per-task nil) (rewrote 0)
         (read-n 0) (written 0) (synth 0) (bad 0))
    (dolist (entry entries)
      (destructuring-bind (id . path) entry
        (multiple-value-bind (res err)
            (handler-case (values (repair-events-file path) nil)
              (error (e) (values nil (princ-to-string e))))
          (cond
            (err
             (push (list :id id :read 0 :written 0 :synthesized 0
                         :quarantined 0 :rewrote-p nil :error err)
                   per-task))
            (t
             (let ((t-read (getf res :read))
                   (t-written (getf res :written))
                   (t-synth (getf res :synthesized))
                   (t-bad (getf res :quarantined))
                   (t-rew (getf res :rewrote-p)))
               (incf read-n t-read) (incf written t-written)
               (incf synth t-synth) (incf bad t-bad)
               (when t-rew (incf rewrote))
               (when (or t-rew (plusp t-bad))
                 (push (list :id id :read t-read :written t-written
                             :synthesized t-synth :quarantined t-bad
                             :rewrote-p t-rew :error nil)
                       per-task))))))))
    (list :tasks (length entries)
          :rewrote rewrote
          :totals (list :read read-n :written written
                        :synthesized synth :quarantined bad)
          :per-task (nreverse per-task))))
