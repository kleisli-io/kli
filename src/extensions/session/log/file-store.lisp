(in-package #:kli/session/log)

(defvar *writing-through* t
  "When NIL, file-store write-through is suppressed (bound during loading so
that replay does not rewrite the file).")

(defclass file-session-store (session-store)
  ((root :initarg :root :reader file-store-root))
  (:documentation "Persists each session as a file of readable records, a
versioned header followed by one entry record per session entry. Writes go
through the same generic seams as the in-memory store, NOTE-STORED-SESSION for
the header and APPEND-SESSION-ENTRY for each entry, and LOAD-SESSION-FILE
replays the records into a fresh session. Records are read with READ rather than
line-split, so message content containing newlines round-trips, and with
*READ-EVAL* disabled."))

(defun stored-session-paths (store)
  (directory (merge-pathnames (make-pathname :name :wild :type "session")
                              (file-store-root store))))

(defun make-file-session-store (root &key (id :session-store))
  "A file store over ROOT. Construction advances the session counter past
the ids already stored there (ids are the file names, so nothing is read),
because a fresh boot that minted from zero would reuse an early id and
NOTE-STORED-SESSION would silently overwrite that session's file."
  (let ((store (make-instance 'file-session-store :id id :root root)))
    (dolist (path (stored-session-paths store))
      (advance-keyword-counter '*session-counter*
                               (intern (string-upcase (pathname-name path))
                                       :keyword)))
    store))

(defun session-file-path (store session-id)
  (merge-pathnames (make-pathname :name (string-downcase (string session-id))
                                  :type "session")
                   (file-store-root store)))

(defun write-session-datum (stream record)
  (with-standard-io-syntax
    (prin1 record stream)
    (terpri stream)))

(defun write-session-file (store session)
  "Rewrite SESSION's durable copy via a same-directory temp file and rename,
so a crash mid-rewrite cannot truncate the stored session. Only the live
chain from the current leaf persists -- entries a retraction or repoint
orphaned leave the durable copy -- and the header pins no leaf, so the
file's last record IS the leaf and plain appends keep that true. A pinned
header leaf goes stale on the very next append, which is how branch and
retract continuations silently truncated on reload. The temp's \"tmp\" type
keeps it invisible to LIST-STORED-SESSIONS' \"session\" wildcard."
  (let* ((path (session-file-path store (object-id session)))
         (temp (make-pathname :type "tmp" :defaults path)))
    (ensure-directories-exist path)
    (with-open-file (stream temp :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-session-datum stream (serialize-record
                                   (make-session-file-header
                                    (object-id session)
                                    :session-uuid (session-uuid session)
                                    :leaf-id nil
                                    :metadata (session-metadata session))))
      (dolist (entry (session-branch store session nil))
        (write-session-datum stream (serialize-record entry))))
    (rename-file temp path)))

(defmethod note-stored-session ((store file-session-store) session context)
  "Persist the session, EXCEPT a session with no entries, no metadata, and
no file yet: every boot mints one of those, and writing it at creation
strands an empty file per abandoned boot. The first entry append (or a
metadata update) materializes the file instead. A session with an existing
file is always rewritten -- this is the update seam for renames, branch
stamps, and leaf repoints."
  (declare (ignore context))
  (when (and *writing-through*
             (or (plusp (session-entry-count session))
                 (session-metadata session)
                 (probe-file (session-file-path store (object-id session)))))
    (write-session-file store session))
  session)

(defmethod append-session-entry ((store file-session-store) (session session)
                                 (entry session-entry) context)
  "Validate, mutate memory, then persist, materializing the file with its header
first when creation deferred it. SERIALIZE-RECORD runs first as a discarded
probe: an unserializable payload signals here, before CALL-NEXT-METHOD mutates
anything, so the in-memory session never diverges from the file. The real write
runs after the mutation, so the entry carries its assigned parent-id, and inside
a fault barrier, so a transient I/O fault is contained -- the live session stays
authoritative rather than aborting the turn. A contained fault marks the session
desynced; the next append rewrites the whole file from memory to heal the gap
rather than appending past it."
  (when *writing-through*
    (serialize-record entry))
  (call-next-method)
  (when *writing-through*
    (kli/ext:with-extension-fault-barrier
        (:seam :session-log-write :id (object-id session) :policy :continue
         :on-fault (setf (session-desynced-p session) t))
      (let ((path (session-file-path store (object-id session))))
        (if (and (probe-file path) (not (session-desynced-p session)))
            (with-open-file (stream path :direction :output
                                         :if-exists :append)
              (write-session-datum stream (serialize-record entry)))
            (write-session-file store session))
        (setf (session-desynced-p session) nil))))
  entry)

(define-condition session-load-error (error)
  ((reason :initarg :reason :reader session-load-error-reason)
   (datum :initarg :datum :initform nil :reader session-load-error-datum)
   (source :initarg :source :initform nil :reader session-load-error-source)
   (cause :initarg :cause :initform nil :reader session-load-error-cause))
  (:report
   (lambda (condition stream)
     (format stream "Session load error (~S)~@[ from ~S~]~@[: ~S~]~@[ (~A)~]"
             (session-load-error-reason condition)
             (session-load-error-source condition)
             (session-load-error-datum condition)
             (session-load-error-cause condition)))))

(defmacro with-session-load-recovery ((&key source) &body body)
  "Evaluate BODY offering per-record recovery. A handler may invoke SKIP-ENTRY
to discard the current record (yielding NIL) or USE-VALUE to substitute an
entry. SOURCE is accepted for symmetry with the surrounding load."
  (declare (ignore source))
  `(restart-case (progn ,@body)
     (skip-entry () :report "Skip this record and continue loading." nil)
     (use-value (entry)
       :report "Use a supplied entry in place of this record."
       entry)))

(defun read-session-datum (stream source eof)
  "Read one record datum from STREAM with *READ-EVAL* disabled, returning EOF
at end of file. Unreadable input signals SESSION-LOAD-ERROR."
  (handler-case
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (read stream nil eof)))
    (error (condition)
      (error 'session-load-error :reason :unreadable-record :source source
                                 :cause condition))))

(defun read-entry-datum-tolerating-torn-tail (stream source eof)
  "Read one entry record like READ-SESSION-DATUM, but treat an unreadable
record that consumed the stream to its end as a torn trailing record (a crash
mid-append), returning (values EOF t). Unreadable input with bytes remaining
is mid-file garbage and keeps signaling. Second value is true only for a torn
tail."
  (handler-case (values (read-session-datum stream source eof) nil)
    (session-load-error (condition)
      (if (eq (read-char stream nil :eof) :eof)
          (values eof t)
          (error condition)))))

(defun read-session-header (stream source eof)
  (let ((datum (read-session-datum stream source eof)))
    (when (eq datum eof)
      (error 'session-load-error :reason :empty-file :source source))
    (unless (and (record-p datum)
                 (eq (record-type datum) :session-file-header))
      (error 'session-load-error :reason :bad-header :datum datum :source source))
    (let ((version (record-field datum :format-version)))
      (unless (eql version *session-format-version*)
        (error 'session-load-error :reason :unknown-version
                                   :datum version :source source))
      (let ((header (deserialize-value (migrate-session-record datum version))))
        (advance-keyword-counter '*session-counter* (header-session-id header))
        header))))

(defun deserialize-entry-record (datum source)
  (unless (record-p datum)
    (error 'session-load-error :reason :not-a-record :datum datum :source source))
  (let ((object (deserialize-value datum)))
    (unless (typep object 'session-entry)
      (error 'session-load-error :reason :not-an-entry :datum datum :source source))
    object))

(defun instantiate-loaded-session (store header context)
  (let ((session (make-session :id (header-session-id header)
                               :metadata (header-metadata header)
                               :uuid (header-session-uuid header))))
    (store-session store session context)
    session))

(defun load-session-file (store path &optional context)
  "Read the session file at PATH into a fresh session in STORE and return
(values session skipped-tail-p), the second value true when a torn trailing
record was dropped. Bad records signal SESSION-LOAD-ERROR offering SKIP-ENTRY
/ USE-VALUE, while a fault that prevents loading offers ABORT-LOAD (yielding
NIL)."
  (let ((*writing-through* nil)
        (eof '#:eof))
    (with-open-file (stream path :direction :input)
      (restart-case
          (let* ((header (read-session-header stream path eof))
                 (session (instantiate-loaded-session store header context))
                 (skipped-tail-p nil))
            (loop
              (multiple-value-bind (datum torn-p)
                  (read-entry-datum-tolerating-torn-tail stream path eof)
                (when torn-p
                  (setf skipped-tail-p t))
                (when (eq datum eof)
                  (return))
                (let ((entry (with-session-load-recovery (:source path)
                               (deserialize-entry-record datum path))))
                  (when entry
                    (let ((parent-id (entry-parent-id entry)))
                      ;; A torn write can drop an entry mid-file; its
                      ;; descendants then reference a parent that never loaded.
                      ;; Append only roots and entries whose parent survived, so
                      ;; an orphaned subtree drops while the readable prefix
                      ;; loads -- a gap no longer bricks the whole session.
                      (when (or (null parent-id)
                                (session-entry-by-id store session parent-id))
                        (append-session-entry store session entry context)))))))
            ;; Honor a pinned header leaf only when it survived the load.
            ;; A leaf inside a dropped subtree would make SESSION-BRANCH signal;
            ;; falling through leaves the last appended survivor as the leaf.
            (let ((leaf-id (header-leaf-id header)))
              (when (and leaf-id (session-entry-by-id store session leaf-id))
                (setf (session-leaf-id session) leaf-id)))
            (values session skipped-tail-p))
        (abort-load ()
          :report "Abort loading this session file."
          nil)))))

(defun clip-preview (text limit)
  (if (and (stringp text) (> (length text) limit))
      (subseq text 0 limit)
      text))

(defun user-message-record-content (datum)
  "Content string when DATUM is a user message-entry record, else NIL."
  (when (eq (record-type datum) :message-entry)
    (let ((message (record-field datum :message)))
      (when (and (record-p message)
                 (eq (record-field message :role) :user))
        (let ((content (record-field message :content)))
          (when (stringp content)
            content))))))

(defun scan-session-file (path)
  "Describe the stored session at PATH by inspecting raw record forms, so listing
mints no ids and bumps no id counter. Returns a plist, or NIL when the file is
missing. A file without a valid header signals SESSION-LOAD-ERROR, while a
torn trailing record just ends the entry count. The first user message content
seeds the preview. A branched session's header metadata yields :branched-from /
:branched-at, and the first user message after the branch-point entry seeds
:branch-preview -- the prompt that distinguishes the branch, where :preview is
the shared prefix's opening prompt across its whole tree."
  (let ((eof '#:eof))
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (when stream
        (let ((header (read-session-datum stream path eof)))
          (when (eq header eof)
            (error 'session-load-error :reason :empty-file :source path))
          (unless (and (record-p header)
                       (eq (record-type header) :session-file-header))
            (error 'session-load-error :reason :bad-header
                                       :datum header :source path))
          (let* ((metadata (record-field header :metadata))
                 (branched-at (getf metadata :branched-at))
                 (entry-count 0)
                 (entry-ids '())
                 (preview nil)
                 (branch-preview nil)
                 (branch-point-preview nil)
                 (past-branch-point-p nil))
            (loop
              (let ((datum (read-entry-datum-tolerating-torn-tail
                            stream path eof)))
                (when (eq datum eof)
                  (return))
                (when (record-p datum)
                  (incf entry-count)
                  (let ((entry-id (record-field datum :id)))
                    (when entry-id
                      (push entry-id entry-ids)))
                  (let ((content (user-message-record-content datum)))
                    (when (and content (null preview))
                      (setf preview (clip-preview content 60)))
                    (when (and content (not past-branch-point-p))
                      (setf branch-point-preview (clip-preview content 60)))
                    (when (and content past-branch-point-p (null branch-preview))
                      (setf branch-preview (clip-preview content 60))))
                  (when (and branched-at
                             (eq (record-field datum :id) branched-at))
                    (setf past-branch-point-p t)))))
            (list :id (record-field header :session-id)
                  :name (getf metadata :name)
                  :branched-from (getf metadata :branched-from)
                  :branched-at branched-at
                  :branch-preview branch-preview
                  :branch-point-preview (and branched-at branch-point-preview)
                  :leaf-id (record-field header :leaf-id)
                  :entry-count entry-count
                  :entry-ids (nreverse entry-ids)
                  :preview preview
                  :mtime (file-write-date path))))))))

(defun corrupt-session-row (path)
  "Listing row for a session file whose scan signaled, so corrupt files stay
visible in /resume instead of silently vanishing. The id is minted from the
file name, matching the downcased-id naming SESSION-FILE-PATH writes."
  (list :id (intern (string-upcase (pathname-name path)) :keyword)
        :name "(corrupt)"
        :corrupt t
        :entry-count 0
        :preview nil
        :mtime (file-write-date path)))

(defun list-stored-sessions (store)
  "Stored sessions in STORE, newest-first by mtime then id-string descending so
the order is deterministic despite one-second mtime granularity. Unscannable
files appear as :corrupt rows. NIL for a store with no backing directory."
  (when (typep store 'file-session-store)
    (let ((rows
            (loop for path in (stored-session-paths store)
                  for row = (handler-case (scan-session-file path)
                              (session-load-error () (corrupt-session-row path)))
                  when row collect row)))
      (sort rows
            (lambda (a b)
              (let ((ma (getf a :mtime))
                    (mb (getf b :mtime)))
                (if (eql ma mb)
                    (string> (string (getf a :id)) (string (getf b :id)))
                    (> ma mb))))))))

(defun blank-session-row-p (row)
  "True for a listing ROW with nothing to resume: no entries, no name, and
not corrupt -- the header-only files old boots stranded. Corrupt rows are
never blank, their files must stay visible rather than silently vanish."
  (and (zerop (getf row :entry-count 0))
       (null (getf row :name))
       (not (getf row :corrupt))))

(defun session-forest (rows)
  "Arrange session listing ROWS into a depth-first forest, returning the rows
in tree order with :depth and :tree-prefix keys prepended. A row whose
:branched-from names another row's id renders as that row's child, ordered
among its siblings by where each diverged in the parent (its :branched-at
position in the parent's :entry-ids) with unknown branch points keeping
ROWS order at the end. Rows without a stored parent are roots in ROWS
order, so files predating branch metadata degrade to a flat list, and rows
a metadata cycle leaves unreachable from any root are appended as roots
rather than dropped. :tree-prefix carries the trunk glyphs: a child renders
under \"├─ \" or \"└─ \" with a \"│  \" continuation for every ancestor
that has later siblings."
  (let ((visited (make-hash-table :test #'eq))
        (result '()))
    (labels ((children-of (row)
               (let ((ids (getf row :entry-ids)))
                 (stable-sort
                  (remove-if-not
                   (lambda (child)
                     (and (eq (getf child :branched-from) (getf row :id))
                          (not (gethash (getf child :id) visited))))
                   rows)
                  #'<
                  :key (lambda (child)
                         (or (position (getf child :branched-at) ids)
                             most-positive-fixnum)))))
             (emit (row depth prefix stem)
               (setf (gethash (getf row :id) visited) t)
               (push (list* :depth depth :tree-prefix prefix row) result)
               (loop for (child . rest) on (children-of row)
                     do (emit child (1+ depth)
                              (concatenate 'string stem (if rest "├─ " "└─ "))
                              (concatenate 'string stem (if rest "│  " "   "))))))
      (dolist (row rows)
        (let ((parent (getf row :branched-from)))
          (when (and (not (gethash (getf row :id) visited))
                     (or (null parent)
                         (not (find parent rows
                                    :key (lambda (other) (getf other :id))))))
            (emit row 0 "" ""))))
      (dolist (row rows)
        (unless (gethash (getf row :id) visited)
          (emit row 0 "" ""))))
    (nreverse result)))

(defun session-row-branch-label (row)
  "One-line label locating ROW within its session tree: the prompt it
rewound past and the prompt that diverged after it. NIL for rows without
branch context, where callers fall back to the plain preview."
  (flet ((flat (text) (substitute #\Space #\Newline text)))
    (let ((fork (getf row :branch-point-preview))
          (divergent (getf row :branch-preview)))
      (cond
        ((and fork divergent)
         (format nil "@\"~A\" -> ~A" (flat fork) (flat divergent)))
        (fork (format nil "@\"~A\"" (flat fork)))
        (divergent (flat divergent))))))

(defun delete-stored-session (store id)
  "Remove the stored file for ID and drop any loaded copy. T when a file was
removed, NIL otherwise or for a store with no backing directory."
  (when (typep store 'file-session-store)
    (let ((path (session-file-path store id)))
      (when (probe-file path)
        (delete-file path)
        (remhash id (store-sessions store))
        t))))
