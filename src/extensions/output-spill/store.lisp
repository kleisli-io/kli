(in-package #:kli/output-spill)

;;; A shared materialization backend for tool output that would otherwise be
;;; computed and destroyed. A capping site keeps a bounded in-image window and
;;; hands the full output here; the store keeps it whole -- a :file backing under
;;; /tmp/kli-outputs-<uid>/<run-identity>/, or a :sequence backing held in image
;;; (already-resident data such as object id lists) -- and mints an opaque handle.
;;; The model retrieves the rest losslessly through the read-result / search-result
;;; tools, which page and scan the backing with bounded heap. The inline window is
;;; necessarily lossy (context is hard-bounded); the backing never is, so a handle's
;;; promise that the full result is retrievable holds. Four write modes (adopt an
;;; on-disk file, write an in-memory string, tee a stream, register an in-image
;;; sequence) share one per-protocol registry, mutex-guarded because skar runs N
;;; worker threads over one protocol-storage. Loads after the extension protocol
;;; (for protocol-storage / the run-identity root) and before every consumer.

(defparameter *output-spill-enabled* t
  "Master switch. NIL makes every write entry point a no-op returning NIL, so a
caller falls back to its plain truncate-and-discard window with no handle.")

(defparameter *output-spill-directory* nil
  "Override for the uid-scoped store base. NIL computes /tmp/kli-outputs-<uid>/.
The per-run-identity subdirectory is appended either way. Tests bind this to a
per-test temp directory so they never touch the real store.")

(defparameter *output-spill-session-budget* (* 64 1024 1024)
  "Per-registry total-bytes ceiling for :file backings. A new spill evicts whole
oldest entries until it fits; a lone backing larger than the budget is still kept
whole (the backing must stay lossless -- eviction drops whole results, never the
middle of a retained one).")

(defparameter *output-spill-sweep-ttl-seconds* (* 24 60 60)
  "Crash-backstop age for the startup sweep: run-dirs older than this are GC'd.")

(defparameter *output-spill-page-lines* 200
  "Default line/element window returned by read-result when no limit is given.")

(defparameter *output-spill-search-matches* 100
  "Default cap on matches returned by one search-result page.")

(defparameter +output-spill-registry-key+ :kli/output-spill.registry
  "Per-protocol storage key for the spill registry.")

(defparameter +spill-read-chunk-bytes+ 65536
  "Octet chunk size for the streaming line pager and newline-count seek.")

(defparameter +spill-search-batch-lines+ 512
  "Line batch size the streaming search pages while scanning.")

;;; Filesystem helpers.

(defun file-byte-length (path)
  "PATH's length in bytes, or 0 when absent."
  (with-open-file (stream path :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
    (if stream (file-length stream) 0)))

(defun write-octets (octets path)
  (with-open-file (stream path :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (write-sequence octets stream)))

(defun harden-file (path)
  (sb-posix:chmod (uiop:native-namestring path) #o600))

(defun native-no-slash (path)
  (string-right-trim "/" (uiop:native-namestring path)))

(defun lstat-no-follow (path)
  "lstat PATH with any trailing slash stripped, so a symlinked directory is
detected rather than followed. NIL when PATH does not exist."
  (handler-case (sb-posix:lstat (native-no-slash path))
    (sb-posix:syscall-error () nil)))

(defun unix->universal (unix-seconds)
  (+ unix-seconds 2208988800))

;;; Store layout and hardening. The base is uid-scoped so users coexist on a
;;; shared box and the fixed node can't be squatted; each run-identity gets a
;;; subdir so protocol teardown GCs a whole run in one tree delete.

(defun output-spill-base-directory ()
  (uiop:ensure-directory-pathname
   (or *output-spill-directory*
       (format nil "/tmp/kli-outputs-~D" (sb-posix:getuid)))))

(defun output-spill-run-directory (protocol)
  (uiop:ensure-directory-pathname
   (merge-pathnames (uiop:ensure-directory-pathname
                     (protocol-run-identity protocol))
                    (output-spill-base-directory))))

(defun run-dir-file (protocol name)
  (merge-pathnames name (output-spill-run-directory protocol)))

(defun directory-hardening-status (path)
  "One of :ok :missing :symlink :not-dir :not-ours for the store directory PATH."
  (let ((stat (lstat-no-follow path)))
    (cond
      ((null stat) :missing)
      ((= sb-posix:s-iflnk (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt))
       :symlink)
      ((/= sb-posix:s-ifdir (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt))
       :not-dir)
      ((/= (sb-posix:stat-uid stat) (sb-posix:getuid)) :not-ours)
      (t :ok))))

(defun mkdir-0700 (path)
  (let ((dir (uiop:ensure-directory-pathname path)))
    (ensure-directories-exist dir)
    (sb-posix:chmod (native-no-slash dir) #o700)
    dir))

(defun ensure-hardened-directory (path)
  "Make PATH a directory we own at mode 0700, recreating it when it is missing,
a symlink, a non-directory, or owned by someone else."
  (ecase (directory-hardening-status path)
    (:ok (uiop:ensure-directory-pathname path))
    (:missing (mkdir-0700 path))
    ((:symlink :not-dir)
     (sb-posix:unlink (native-no-slash path))
     (mkdir-0700 path))
    (:not-ours
     (uiop:delete-directory-tree (uiop:ensure-directory-pathname path)
                                 :validate t :if-does-not-exist :ignore)
     (mkdir-0700 path))))

(defun ensure-run-directory (protocol)
  "Harden the uid-scoped base then PROTOCOL's run-dir; return the run-dir."
  (ensure-hardened-directory (output-spill-base-directory))
  (ensure-hardened-directory (output-spill-run-directory protocol)))

;;; Registry. One per protocol, in protocol-storage. The lock serializes the
;;; whole spill transaction (counter, token, file write, eviction, insert) so
;;; concurrent worker threads cannot lose entries, collide tokens, or orphan
;;; files. Registry creation itself takes a process-global lock because
;;; protocol-storage has no atomic get-or-create. The registry is a struct, so it
;;; is snapshot-unserializable -- skipped on capture, reconstructed fresh on
;;; restart (with a fresh run-identity, the old run-dir abandoned and age-swept),
;;; and untouched by in-place restore. A serializable registry would rehydrate
;;; stale paths embedding the old run-identity, so it must stay a struct.

(defstruct (spill-registry (:constructor %make-spill-registry))
  (counter 0)
  (entries '())
  (total-bytes 0)
  (lock (sb-thread:make-mutex :name "output-spill-registry")))

;;; An entry's KIND selects the backing: :file (PATH on disk, BYTES counted
;;; against the budget) or :sequence (ELEMENTS held in image, already resident, so
;;; BYTES is 0 and ELEMENT-COUNT is its length). OWNED is true for a backing the
;;; store created and may unlink on eviction; NIL for a registered external file
;;; (a live background-job log) whose lifetime belongs to its producer -- never a
;;; victim, never unlinked.
(defstruct (spill-entry (:constructor %make-spill-entry))
  token kind path elements bytes element-count producer-uuid (owned t))

(defvar *registry-creation-lock*
  (sb-thread:make-mutex :name "output-spill-registry-creation"))

(defun ensure-spill-registry (protocol)
  (or (protocol-storage protocol +output-spill-registry-key+)
      (sb-thread:with-mutex (*registry-creation-lock*)
        (or (protocol-storage protocol +output-spill-registry-key+)
            (setf (protocol-storage protocol +output-spill-registry-key+)
                  (%make-spill-registry))))))

(defun mint-spill-token (run-identity producer-uuid counter)
  "The opaque handle: digest(run-identity, producer-uuid, counter)[:12]. The
run-identity makes it process-unique, the counter per-spill-unique under the
registry lock; producer-uuid is attribution only."
  (subseq (ironclad:byte-array-to-hex-string
           (ironclad:digest-sequence
            :sha256
            (ironclad:ascii-string-to-byte-array
             (format nil "~A:~A:~D" run-identity (or producer-uuid "") counter))))
          0 12))

(defun find-spill-entry (protocol token)
  "The entry registered under TOKEN for PROTOCOL, or NIL when unknown or evicted."
  (let ((registry (protocol-storage protocol +output-spill-registry-key+)))
    (when registry
      (sb-thread:with-mutex ((spill-registry-lock registry))
        (find token (spill-registry-entries registry)
              :key #'spill-entry-token :test #'string=)))))

;;; The registry transaction. The backing is whole -- no head+tail cap -- so a
;;; retained result is always lossless; pressure drops whole oldest entries.

(defun evict-to-fit (registry incoming)
  "Drop oldest OWNED entries (unlinking any backing file) until INCOMING more bytes
fit the session budget. An externally-owned entry (a registered live file) is never
a victim and its file is never unlinked -- its lifetime belongs to its producer. A
:sequence entry frees no disk but still ages out."
  (loop for oldest = (find-if #'spill-entry-owned
                              (spill-registry-entries registry) :from-end t)
        while (and oldest
                   (> (+ (spill-registry-total-bytes registry) incoming)
                      *output-spill-session-budget*))
        do (when (spill-entry-path oldest)
             (ignore-errors (uiop:delete-file-if-exists (spill-entry-path oldest))))
           (setf (spill-registry-entries registry)
                 (remove oldest (spill-registry-entries registry) :test #'eq))
           (decf (spill-registry-total-bytes registry) (spill-entry-bytes oldest))))

(defun record-spill (registry entry)
  "Evict to make room for ENTRY's bytes, then prepend it. Caller holds the lock."
  (evict-to-fit registry (spill-entry-bytes entry))
  (push entry (spill-registry-entries registry))
  (incf (spill-registry-total-bytes registry) (spill-entry-bytes entry))
  entry)

(defun register-spill (protocol producer-uuid writer)
  "Run WRITER, a function of the destination path returning the bytes written, as
one critical section: mint a token, harden the run-dir, let WRITER produce the
file whole, evict to fit, and record a :file entry. Returns the spill-entry, or
NIL when spilling is disabled or any step fails (the caller then emits a
handle-less, degraded marker)."
  (unless *output-spill-enabled*
    (return-from register-spill nil))
  (handler-case
      (let ((registry (ensure-spill-registry protocol)))
        (sb-thread:with-mutex ((spill-registry-lock registry))
          (ensure-run-directory protocol)
          (let* ((counter (incf (spill-registry-counter registry)))
                 (token (mint-spill-token (protocol-run-identity protocol)
                                          producer-uuid counter))
                 (dest (run-dir-file protocol token))
                 (bytes (funcall writer dest)))
            (record-spill registry
                          (%make-spill-entry :token token :kind :file :path dest
                                             :bytes bytes
                                             :producer-uuid producer-uuid)))))
    (error () nil)))

(defun adopt-file-whole (src dest)
  "Move SRC to DEST whole, hardened. A same-filesystem rename is zero-copy and
preserves mode; a cross-filesystem source is copied then unlinked. Returns the
byte size of DEST."
  (handler-case
      (sb-posix:rename (uiop:native-namestring src) (uiop:native-namestring dest))
    (sb-posix:syscall-error ()
      (uiop:copy-file src dest)
      (ignore-errors (uiop:delete-file-if-exists src))))
  (harden-file dest)
  (file-byte-length dest))

(defun adopt-file-spill (protocol src-path &key producer-uuid)
  "Adopt the on-disk SRC-PATH into the store whole. Zero-copy rename when it
shares the store's filesystem (captures are minted in-store, so it does). Returns
the spill-entry or NIL."
  (register-spill protocol producer-uuid
                  (lambda (dest) (adopt-file-whole src-path dest))))

(defun write-string-spill (protocol text &key producer-uuid)
  "Spill the in-memory TEXT whole. Returns the spill-entry or NIL."
  (register-spill protocol producer-uuid
                  (lambda (dest)
                    (let ((octets (sb-ext:string-to-octets
                                   text :external-format :utf-8)))
                      (write-octets octets dest)
                      (harden-file dest)
                      (length octets)))))

(defun register-sequence-spill (protocol elements &key producer-uuid)
  "Register an in-image ELEMENTS sequence as a :sequence backing (no file; the
data is already resident). read-result pages it by subseq, search-result scans it.
Returns the spill-entry or NIL when spilling is disabled."
  (unless *output-spill-enabled*
    (return-from register-sequence-spill nil))
  (handler-case
      (let ((registry (ensure-spill-registry protocol))
            (vector (coerce elements 'simple-vector)))
        (sb-thread:with-mutex ((spill-registry-lock registry))
          (let* ((counter (incf (spill-registry-counter registry)))
                 (token (mint-spill-token (protocol-run-identity protocol)
                                          producer-uuid counter)))
            (record-spill registry
                          (%make-spill-entry :token token :kind :sequence
                                             :elements vector :bytes 0
                                             :element-count (length vector)
                                             :producer-uuid producer-uuid)))))
    (error () nil)))

(defun register-file-handle (protocol path &key producer-uuid)
  "Register an existing, externally-owned PATH as a :file backing without copying or
taking ownership. The handle pages and searches the live file through read-result /
search-result; eviction never unlinks it and its bytes do not count against the
budget, because its lifetime belongs to its producer (a background job's output
file, reaped by the job machinery). Returns the spill-entry, or NIL when spilling is
disabled or registration fails."
  (unless *output-spill-enabled*
    (return-from register-file-handle nil))
  (handler-case
      (let ((registry (ensure-spill-registry protocol)))
        (sb-thread:with-mutex ((spill-registry-lock registry))
          (let* ((counter (incf (spill-registry-counter registry)))
                 (token (mint-spill-token (protocol-run-identity protocol)
                                          producer-uuid counter)))
            (record-spill registry
                          (%make-spill-entry :token token :kind :file :path path
                                             :bytes 0 :owned nil
                                             :producer-uuid producer-uuid)))))
    (error () nil)))

;;; Tee mode. A bounded-tee character stream forwards the first LIMIT characters
;;; to an in-image window and streams every character to a part file. The token
;;; is reserved under the lock at open; eviction and the registry insert happen
;;; under the lock at finalize. finalize runs from the caller's unwind-protect, so
;;; a producer that throws mid-stream still lands a partial (lossless of what was
;;; produced) backing, and the worker that owns the stream is the one that closes
;;; it.

(defclass spill-tee-stream (sb-gray:fundamental-character-output-stream)
  ((target :initarg :target :reader tee-target)
   (limit :initarg :limit :reader tee-limit)
   (file-stream :initarg :file-stream :reader tee-file-stream)
   (written :initform 0 :accessor tee-written)
   (protocol :initarg :protocol :reader tee-protocol)
   (token :initarg :token :reader tee-token)
   (part-path :initarg :part-path :reader tee-part-path)
   (producer-uuid :initarg :producer-uuid :reader tee-producer-uuid)))

(defmethod sb-gray:stream-write-char ((stream spill-tee-stream) char)
  (let ((written (tee-written stream)))
    (when (< written (tee-limit stream))
      (write-char char (tee-target stream)))
    (write-char char (tee-file-stream stream))
    (setf (tee-written stream) (1+ written)))
  char)

(defmethod sb-gray:stream-write-string ((stream spill-tee-stream)
                                        string &optional (start 0) end)
  (let* ((end (or end (length string)))
         (written (tee-written stream))
         (remaining (- (tee-limit stream) written))
         (take (min (max remaining 0) (- end start))))
    (when (plusp take)
      (write-string string (tee-target stream)
                    :start start :end (+ start take)))
    (write-string string (tee-file-stream stream) :start start :end end)
    (setf (tee-written stream) (+ written (- end start))))
  string)

(defun open-spill-tee (protocol &key producer-uuid
                                     (window-limit *output-spill-page-lines*))
  "A bounded-tee stream over a fresh part file, or NIL when spilling is disabled
or the file cannot be opened (the caller then uses its own plain bounded stream).
Reserves the token under the registry lock; the part is finalized by
finalize-spill-tee."
  (unless *output-spill-enabled*
    (return-from open-spill-tee nil))
  (handler-case
      (let ((registry (ensure-spill-registry protocol)))
        (sb-thread:with-mutex ((spill-registry-lock registry))
          (ensure-run-directory protocol)
          (let* ((counter (incf (spill-registry-counter registry)))
                 (token (mint-spill-token (protocol-run-identity protocol)
                                          producer-uuid counter))
                 (part (run-dir-file protocol (format nil "~A.part" token)))
                 (file-stream (open part :direction :output
                                         :external-format :utf-8
                                         :if-exists :supersede
                                         :if-does-not-exist :create)))
            (make-instance 'spill-tee-stream
                           :target (make-string-output-stream)
                           :limit window-limit
                           :file-stream file-stream
                           :protocol protocol
                           :token token
                           :part-path part
                           :producer-uuid producer-uuid))))
    (error () nil)))

(defun finalize-spill-tee (stream)
  "Close STREAM's part file, fold it into the registry as a whole :file backing,
and return the entry. Safe after a producer error -- the partial part finalizes
as the backing. NIL when STREAM is NIL (spilling disabled) or finalization fails."
  (unless stream
    (return-from finalize-spill-tee nil))
  (ignore-errors (close (tee-file-stream stream)))
  (handler-case
      (let ((registry (ensure-spill-registry (tee-protocol stream))))
        (sb-thread:with-mutex ((spill-registry-lock registry))
          (let* ((dest (run-dir-file (tee-protocol stream) (tee-token stream)))
                 (bytes (adopt-file-whole (tee-part-path stream) dest)))
            (record-spill registry
                          (%make-spill-entry :token (tee-token stream) :kind :file
                                             :path dest :bytes bytes
                                             :producer-uuid
                                             (tee-producer-uuid stream))))))
    (error () nil)))

(defun discard-spill-tee (stream)
  "Close STREAM's part file and delete it without registering a backing -- the
windowed output fit whole, so no handle is needed. Returns NIL. Safe when STREAM
is NIL (spilling disabled)."
  (when stream
    (ignore-errors (close (tee-file-stream stream)))
    (ignore-errors (uiop:delete-file-if-exists (tee-part-path stream))))
  nil)

(defun tee-window (stream)
  "The in-image windowed prefix STREAM forwarded (at most its limit characters).
Drains the window target, so call once."
  (get-output-stream-string (tee-target stream)))

(defun tee-truncated-p (stream)
  "True when STREAM saw more characters than its window limit."
  (> (tee-written stream) (tee-limit stream)))

;;; Streaming primitives over a :file backing. All are bounded-heap (a fixed
;;; octet chunk plus the current page) and UTF-8-safe (lines are split on 0x0A,
;;; which is never a UTF-8 lead or continuation byte, so a line is always a
;;; complete sequence). They never slurp the file, so an arbitrarily large
;;; backing is reachable -- the property head+tail and the filesystem search lose.

(defun decode-line (octets)
  "Decode the fill-pointered OCTETS buffer (one line, sans newline) as UTF-8."
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun read-lines-from-offset (path start-byte max-lines)
  "Bounded-heap line pager. From byte START-BYTE, read up to MAX-LINES lines split
on 0x0A; a trailing line with no final newline counts at EOF. Returns (values
lines next-byte-offset eof-p): NEXT-BYTE-OFFSET resumes the next page; EOF-P is
true when the file end was reached within this page."
  (with-open-file (stream path :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
    (unless stream
      (return-from read-lines-from-offset (values '() start-byte t)))
    (let ((size (file-length stream)))
      (when (>= start-byte size)
        (return-from read-lines-from-offset (values '() size t)))
      (file-position stream start-byte)
      (let ((lines '())
            (line (make-array 256 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0))
            (chunk (make-array +spill-read-chunk-bytes+
                               :element-type '(unsigned-byte 8)))
            (pos start-byte)
            (line-start start-byte))
        (loop
          (when (>= (length lines) max-lines)
            (return (values (nreverse lines) line-start (>= line-start size))))
          (let ((n (read-sequence chunk stream)))
            (when (zerop n)
              (when (plusp (fill-pointer line))
                (push (decode-line line) lines))
              (return (values (nreverse lines) size t)))
            (block scan
              (dotimes (i n)
                (let ((byte (aref chunk i)))
                  (incf pos)
                  (cond
                    ((= byte 10)
                     (push (decode-line line) lines)
                     (setf (fill-pointer line) 0
                           line-start pos)
                     (when (>= (length lines) max-lines)
                       (return-from scan)))
                    (t (vector-push-extend byte line))))))))))))

(defun byte-offset-of-line (path line)
  "Byte offset where 0-indexed LINE begins, or NIL when LINE is past the last
line. Bounded-heap newline count; allocates no line."
  (when (<= line 0)
    (return-from byte-offset-of-line 0))
  (with-open-file (stream path :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
    (unless stream (return-from byte-offset-of-line nil))
    (let ((size (file-length stream))
          (chunk (make-array +spill-read-chunk-bytes+
                             :element-type '(unsigned-byte 8)))
          (pos 0)
          (seen 0))
      (loop
        (let ((n (read-sequence chunk stream)))
          (when (zerop n) (return nil))
          (dotimes (i n)
            (incf pos)
            (when (= (aref chunk i) 10)
              (incf seen)
              (when (= seen line)
                (return-from byte-offset-of-line
                  (if (>= pos size) nil pos))))))))))

(defun page-lines (path start-line n)
  "Random-access page: N lines from 0-indexed START-LINE. Returns (values lines
next-byte-offset eof-p), or (values nil nil t) when START-LINE is past EOF."
  (let ((offset (byte-offset-of-line path start-line)))
    (if offset
        (read-lines-from-offset path offset n)
        (values '() nil t))))

(defun search-within (path pattern &key (start-line 0)
                                        (max-matches *output-spill-search-matches*))
  "Streaming scan from 0-indexed START-LINE: collect up to MAX-MATCHES (lineno .
text) whose TEXT matches the cl-ppcre PATTERN. Returns (values matches next-lineno
eof-p) for continuation -- re-enter with :start-line next-lineno. Reaches the
middle of an arbitrarily large backing."
  (let ((start-byte (byte-offset-of-line path start-line)))
    (if (null start-byte)
        (values '() start-line t)
        (let ((scanner (cl-ppcre:create-scanner pattern))
              (matches '())
              (byte start-byte)
              (lineno start-line))
          (loop
            (multiple-value-bind (lines next eof)
                (read-lines-from-offset path byte +spill-search-batch-lines+)
              (dolist (line lines)
                (when (cl-ppcre:scan scanner line)
                  (push (cons lineno line) matches)
                  (when (>= (length matches) max-matches)
                    (return-from search-within
                      (values (nreverse matches) (1+ lineno) nil))))
                (incf lineno))
              (setf byte next)
              (when eof
                (return (values (nreverse matches) lineno t)))))))))

;;; Streaming primitives over a :sequence backing -- the in-image analogue.

(defun read-sequence-window (elements start limit)
  "A window of LIMIT elements of the simple-vector ELEMENTS from index START.
Returns (values items next-index eof-p)."
  (let* ((len (length elements))
         (start (max 0 start))
         (end (min len (+ start limit))))
    (if (>= start len)
        (values '() len t)
        (values (coerce (subseq elements start end) 'list)
                end
                (>= end len)))))

(defun search-sequence (elements pattern after max-matches)
  "Up to MAX-MATCHES (index . printed) elements at or after index AFTER whose
princ matches the cl-ppcre PATTERN. Returns (values matches next-index eof-p)."
  (let ((scanner (cl-ppcre:create-scanner pattern))
        (len (length elements))
        (matches '()))
    (loop for i from (max 0 after) below len
          for text = (princ-to-string (aref elements i))
          do (when (cl-ppcre:scan scanner text)
               (push (cons i text) matches)
               (when (>= (length matches) max-matches)
                 (return-from search-sequence
                   (values (nreverse matches) (1+ i) (>= (1+ i) len)))))
          finally (return (values (nreverse matches) len t)))))

;;; Marker. The handle must ride the tool-result CONTENT text (details reach the
;;; wire only as an appended XML-JSON blob), so a caller appends this after its
;;; windowed output. A degraded result carries no handle -- never point the model
;;; at a handle that is not there.

(defun format-spill-marker (subject &key shown total handle (unit "byte")
                                    (shown-unit "chars") degraded)
  "Content-text marker for a windowed result. SUBJECT names the stream (\"stdout\",
\"find results\", ...); SHOWN is the amount kept in-image; TOTAL is the full size
in UNITs; HANDLE is the spill token. DEGRADED omits the handle."
  (cond
    (degraded
     (format nil "[~A truncated to ~:D ~A; full result could not be retained]"
             subject shown shown-unit))
    (handle
     (format nil "[~A truncated to ~:D ~A; full ~:D-~A result at handle ~A — ~
read-result / search-result to retrieve]"
             subject shown shown-unit total unit handle))
    (t
     (format nil "[~A truncated to ~:D ~A]" subject shown shown-unit))))

;;; Lifecycle. A retract effect reaps a run's backings on teardown/rollback; the
;;; whole run-dir is the unit, GC'd in one tree delete. The install runs the age
;;; sweep as the crash backstop: opaque names need no file->session map, so age
;;; alone keys it.

(defun reap-spills (protocol)
  "Delete PROTOCOL's run-dir and clear its registry slot."
  (ignore-errors
   (uiop:delete-directory-tree (output-spill-run-directory protocol)
                               :validate t :if-does-not-exist :ignore))
  (setf (protocol-storage protocol +output-spill-registry-key+) nil))

(defun excluded-spill-directory-p (path excluded)
  (let ((native (native-no-slash path)))
    (some (lambda (excluded-path)
            (string= native (native-no-slash excluded-path)))
          excluded)))

(defun sweep-stale-spills (&key (cutoff (- (get-universal-time)
                                           *output-spill-sweep-ttl-seconds*))
                                (base (output-spill-base-directory))
                                (exclude '()))
  "Delete run-subdirs of BASE whose mtime is older than CUTOFF (universal-time),
except directories listed in EXCLUDE. Returns the count swept."
  (let ((swept 0))
    (when (eq :ok (directory-hardening-status base))
      (dolist (sub (ignore-errors (uiop:subdirectories base)))
        (let ((stat (lstat-no-follow sub)))
          (when (and stat
                     (not (excluded-spill-directory-p sub exclude))
                     (< (unix->universal (sb-posix:stat-mtime stat)) cutoff))
            (ignore-errors
             (uiop:delete-directory-tree sub :validate t
                                             :if-does-not-exist :ignore))
            (incf swept)))))
    swept))

(defun install-output-spill-reap (protocol contribution context)
  "Installer: run the crash-backstop age sweep once. Spills accrue lazily on first
write."
  (declare (ignore protocol contribution context))
  (ignore-errors (sweep-stale-spills))
  nil)

(defun refresh-output-spill-reap (protocol contribution context)
  "Refresh output-spill startup cleanup after boot snapshot reuse.

A reused boot snapshot keeps the current protocol and may already have live spill
handles. Refresh therefore runs only the crash-backstop stale-run sweep, excluding
this protocol's current run directory, and does not clear the registry or reap
live handles."
  (declare (ignore contribution context))
  (ignore-errors
    (sweep-stale-spills :exclude (list (output-spill-run-directory protocol))))
  nil)

(defun retract-output-spill-reap (protocol contribution context)
  "Retractor: reap this run's spills so a teardown or rollback leaves no files."
  (declare (ignore contribution context))
  (reap-spills protocol))
