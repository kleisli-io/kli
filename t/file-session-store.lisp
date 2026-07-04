(in-package #:kli/tests)
(in-suite all)

(defvar *file-store-test-counter* 0)

(defun temp-session-root ()
  (merge-pathnames (format nil "kli-session-test-~D-~D/"
                           (get-universal-time)
                           (incf *file-store-test-counter*))
                   #p"/tmp/"))

(defun write-record-line (stream record)
  (with-standard-io-syntax
    (prin1 record stream)
    (terpri stream)))

(test file-session-store-round-trips-a-session
  (let* ((root (temp-session-root))
         (ctx-a (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (session (sess:create-session store ctx-a
                                       :id :persisted
                                       :metadata '(:title "demo"))))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "hello" :id :m1) :id :e1)
     ctx-a)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-assistant-message "hi" :id :m2) :id :e2)
     ctx-a)
    (sess:append-session-entry
     store session
     (sess:make-message-entry
      (sess:make-tool-result-message "ok" :id :m3 :tool-name "bash") :id :e3)
     ctx-a)
    (sess:append-session-entry
     store session
     (sess:make-custom-entry :ui-state :id :e4 :data '(:scroll 1))
     ctx-a)
    (let* ((path (sess:session-file-path store :persisted))
           (ctx-b (kli:make-kernel-host))
           (fresh (sess:make-file-session-store root))
           (loaded (sess:load-session-file fresh path ctx-b)))
      (is (eq :persisted (kli:object-id loaded)))
      (is (eq :e4 (sess:session-leaf-id loaded)))
      (is (equal '(:title "demo") (sess:session-metadata loaded)))
      (is (equal '(:e1 :e2 :e3 :e4)
                 (mapcar #'kli:object-id
                         (sess:session-branch fresh loaded nil))))
      (is (equal '("hello" "hi" "ok")
                 (mapcar #'sess:message-content
                         (sess:session-context-messages
                          (sess:build-session-context fresh loaded)))))
      (ignore-errors (delete-file path)))))

(test file-session-load-skips-bad-record-and-continues
  (let* ((root (temp-session-root))
         (store (sess:make-file-session-store root))
         (path (sess:session-file-path store :skips)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-session-file-header :skips :leaf-id :e2)))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "good1" :id :m1) :id :e1)))
      (write-record-line s '(:not-a-valid-record 1 2))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-assistant-message "good2" :id :m2)
                             :id :e2))))
    (let* ((ctx (kli:make-kernel-host))
           (loaded (handler-bind
                       ((sess:session-load-error
                          (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'sess:skip-entry))))
                     (sess:load-session-file store path ctx))))
      (is (equal '(:e1 :e2)
                 (mapcar #'kli:object-id
                         (sess:session-branch store loaded nil))))
      (is (equal '("good1" "good2")
                 (mapcar #'sess:message-content
                         (sess:session-context-messages
                          (sess:build-session-context store loaded)))))
      (ignore-errors (delete-file path)))))

(test file-session-load-replaces-bad-record-via-use-value
  (let* ((root (temp-session-root))
         (store (sess:make-file-session-store root))
         (path (sess:session-file-path store :replaces)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-session-file-header :replaces :leaf-id :e1)))
      (write-record-line s '(:not-a-valid-record 1 2)))
    (let* ((ctx (kli:make-kernel-host))
           (replacement (sess:make-message-entry
                         (sess:make-user-message "substituted" :id :m1) :id :e1))
           (loaded (handler-bind
                       ((sess:session-load-error
                          (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'use-value replacement))))
                     (sess:load-session-file store path ctx))))
      (is (equal '(:e1)
                 (mapcar #'kli:object-id
                         (sess:session-branch store loaded nil))))
      (ignore-errors (delete-file path)))))

(test file-session-load-bad-header-is-restartable
  (let* ((root (temp-session-root))
         (store (sess:make-file-session-store root))
         (path (sess:session-file-path store :bad-header)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "no header" :id :m1)
                             :id :e1))))
    (let ((ctx (kli:make-kernel-host)))
      (signals sess:session-load-error
        (sess:load-session-file store path ctx))
      (is (null (handler-bind
                    ((sess:session-load-error
                       (lambda (c)
                         (declare (ignore c))
                         (invoke-restart 'sess:abort-load))))
                  (sess:load-session-file store path ctx)))))
    (ignore-errors (delete-file path))))

(test file-session-unreadable-record-captures-the-reader-condition
  "An unreadable record re-signals :unreadable-record carrying the underlying
reader condition as its cause, so the failure stays diagnosable."
  (with-input-from-string (in "(:torn 1 2")    ; unbalanced -> reader signals
    (let ((c (handler-case
                 (progn (sess::read-session-datum in :unreadable-src :eof) nil)
               (sess:session-load-error (e) e))))
      (is (typep c 'sess:session-load-error))
      (is (eq :unreadable-record (sess:session-load-error-reason c)))
      (is (eq :unreadable-src (sess:session-load-error-source c)))
      (is (typep (sess:session-load-error-cause c) 'error)
          "the underlying reader condition is captured as the cause"))))

(test file-session-store-write-failure-preserves-previous-file
  "A failure mid-rewrite must leave the previously stored file intact: the
rewrite goes to a temp file that only renames over the original on success.
The temp residue is invisible to the session listing."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (session (sess:create-session store ctx :id :atomic-rewrite)))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "kept" :id :m1) :id :e1)
     ctx)
    (let ((original (fdefinition 'sess::write-session-datum))
          (calls 0))
      (unwind-protect
           (progn
             (setf (fdefinition 'sess::write-session-datum)
                   (lambda (stream record)
                     (when (> (incf calls) 1)
                       (error "simulated write failure"))
                     (funcall original stream record)))
             (signals error (sess:note-stored-session store session ctx)))
        (setf (fdefinition 'sess::write-session-datum) original)))
    (let* ((ctx-b (kli:make-kernel-host))
           (fresh (sess:make-file-session-store root))
           (loaded (sess:load-session-file
                    fresh (sess:session-file-path store :atomic-rewrite) ctx-b)))
      (is (equal '(:e1)
                 (mapcar #'kli:object-id
                         (sess:session-branch fresh loaded nil)))))
    (is (= 1 (length (sess:list-stored-sessions store))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test file-session-load-tolerates-torn-trailing-record
  "A crash mid-append leaves a torn final record. The load drops it, returns
the readable prefix, and reports the drop via the second value. A clean file
reports no dropped tail."
  (let* ((root (temp-session-root))
         (store (sess:make-file-session-store root))
         (path (sess:session-file-path store :torn-tail))
         (ctx (kli:make-kernel-host)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-session-file-header :torn-tail
                                                           :leaf-id :e1)))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "kept" :id :m1) :id :e1)))
      (write-string "(:record :type :message-entry :fields (:content \"torn" s))
    (multiple-value-bind (loaded skipped-tail-p)
        (sess:load-session-file store path ctx)
      (is (eq t skipped-tail-p))
      (is (equal '(:e1)
                 (mapcar #'kli:object-id
                         (sess:session-branch store loaded nil)))))
    (let ((clean (sess:session-file-path store :clean-tail)))
      (with-open-file (s clean :direction :output
                               :if-exists :supersede :if-does-not-exist :create)
        (write-record-line s (sess:serialize-record
                              (sess:make-session-file-header :clean-tail))))
      (is (null (nth-value 1 (sess:load-session-file store clean ctx)))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test file-session-load-still-signals-mid-file-garbage
  "Unreadable bytes with records after them are corruption, not a torn tail --
the load signals instead of silently dropping the rest of the session."
  (let* ((root (temp-session-root))
         (store (sess:make-file-session-store root))
         (path (sess:session-file-path store :mid-garbage)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-session-file-header :mid-garbage)))
      (write-line "#<garbage>" s)
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "after" :id :m1) :id :e1))))
    (signals sess:session-load-error
      (sess:load-session-file store path (kli:make-kernel-host)))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test list-stored-sessions-scans-through-torn-tail
  "A torn trailing record must not hide the session from the listing -- the
scan counts the readable prefix."
  (let* ((root (temp-session-root))
         (store (sess:make-file-session-store root))
         (path (sess:session-file-path store :torn-listed)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-session-file-header :torn-listed)))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "kept" :id :m1) :id :e1)))
      (write-string "(:record :type :message-entry :fields (:content \"torn" s))
    (let ((rows (sess:list-stored-sessions store)))
      (is (= 1 (length rows)))
      (let ((row (first rows)))
        (is (not (getf row :corrupt)))
        (is (= 1 (getf row :entry-count)))
        (is (string= "kept" (getf row :preview)))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test list-stored-sessions-lists-corrupt-files
  "Files with no readable header show as (corrupt) rows instead of vanishing
from the listing."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root)))
    (let ((session (sess:create-session store ctx :id :intact)))
      (sess:append-session-entry
       store session
       (sess:make-message-entry (sess:make-user-message "hello")) ctx))
    (with-open-file (s (sess:session-file-path store :mangled)
                       :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
      (write-string "total garbage" s))
    (with-open-file (s (sess:session-file-path store :empty)
                       :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
      (declare (ignorable s)))
    (let* ((rows (sess:list-stored-sessions store))
           (corrupt (remove-if-not (lambda (row) (getf row :corrupt)) rows))
           (mangled (find :mangled corrupt :key (lambda (row) (getf row :id)))))
      (is (= 3 (length rows)))
      (is (= 2 (length corrupt)))
      (is (not (null mangled)))
      (is (string= "(corrupt)" (getf mangled :name)))
      (is (= 0 (getf mangled :entry-count))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test file-session-store-construction-mints-past-stored-files
  "Constructing a file store advances the session counter past the ids its
directory already holds, so a fresh boot can never mint an id that collides
with -- and silently overwrites -- a session stored by an earlier run."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root)))
    (sess:create-session store ctx :id :session-31337
                               :metadata '(:name "stored"))
    (let* ((rebooted (sess:make-file-session-store root))
           (minted-id (kli:object-id (sess:create-session rebooted ctx)))
           (name (symbol-name minted-id)))
      (is (not (eq :session-31337 minted-id)))
      (is (> (parse-integer name
                            :start (1+ (position #\- name :from-end t)))
             31337)))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test file-session-reload-then-mint-does-not-collide
  (let* ((root (temp-session-root))
         (store (sess:make-file-session-store root))
         (path (sess:session-file-path store :collide)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-session-file-header :collide
                                                           :leaf-id :session-entry-9000)))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "loaded" :id :agent-message-9000)
                             :id :session-entry-9000))))
    (let* ((ctx (kli:make-kernel-host))
           (loaded (sess:load-session-file store path ctx))
           (minted (sess:make-message-entry (sess:make-assistant-message "fresh"))))
      (is (not (eq :session-entry-9000 (kli:object-id minted))))
      (is (eq minted (sess:append-session-entry store loaded minted ctx)))
      (is (eq (kli:object-id minted) (sess:session-leaf-id loaded)))
      (ignore-errors (delete-file path)))))

(test file-session-store-defers-file-until-first-write
  "Creating a session writes no file -- every boot mints one, and abandoned
empties must not accumulate on disk. The first entry append materializes
the file with its header, while metadata at creation persists immediately."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (session (sess:create-session store ctx :id :lazy-boot)))
    (is (null (probe-file (sess:session-file-path store :lazy-boot)))
        "an entry-less session leaves no file")
    (is (null (sess:list-stored-sessions store)))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "first prompt")
                              :id :lz-e1)
     ctx)
    (is (not (null (probe-file (sess:session-file-path store :lazy-boot))))
        "the first append materializes the file")
    (let ((row (first (sess:list-stored-sessions store))))
      (is (eq :lazy-boot (getf row :id)))
      (is (= 1 (getf row :entry-count)))
      (is (search "first prompt" (getf row :preview))))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-assistant-message "a reply"))
     ctx)
    (is (= 2 (getf (first (sess:list-stored-sessions store)) :entry-count))
        "later appends extend the materialized file")
    (sess:create-session store ctx :id :named-at-birth
                              :metadata '(:name "kept"))
    (is (not (null (probe-file (sess:session-file-path store :named-at-birth))))
        "metadata at creation persists immediately")
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test file-session-branch-continuation-survives-reload
  "Work appended to a branched session survives a reload: the stored header
pins no leaf, so the file's last record is the leaf. Pinned branch-point
headers were how post-rewind conversations silently truncated on restart."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (parent (sess:create-session store ctx :id :continue-parent)))
    (sess:append-session-entry
     store parent
     (sess:make-message-entry (sess:make-user-message "one" :id :cm1) :id :c1)
     ctx)
    (sess:append-session-entry
     store parent
     (sess:make-message-entry (sess:make-assistant-message "two" :id :cm2)
                              :id :c2)
     ctx)
    (let ((branch (sess:branch-session-at-entry store parent :c2 ctx)))
      (sess:append-session-entry
       store branch
       (sess:make-message-entry (sess:make-user-message "three" :id :cm3)
                                :id :c3)
       ctx)
      (let* ((path (sess:session-file-path store (kli:object-id branch)))
             (fresh (sess:make-file-session-store (temp-session-root)))
             (loaded (sess:load-session-file fresh path
                                             (kli:make-kernel-host))))
        (is (eq :c3 (sess:session-leaf-id loaded))
            "the continuation is the leaf after reload")
        (is (equal '(:c1 :c2 :c3)
                   (mapcar #'kli:object-id
                           (sess:session-branch fresh loaded nil))))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test file-session-repoint-rewrite-drops-orphaned-tail
  "A leaf repoint rewrites the durable copy down to the live chain: the
orphaned tail leaves the file rather than riding a header pin, and appends
after the repoint extend the chain across reload."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (session (sess:create-session store ctx :id :repointed)))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "kept" :id :rm1) :id :r1)
     ctx)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "retracted" :id :rm2)
                              :id :r2)
     ctx)
    (sess:repoint-session-leaf store session :r1 ctx)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "continued" :id :rm3)
                              :id :r3)
     ctx)
    (let* ((path (sess:session-file-path store :repointed))
           (fresh (sess:make-file-session-store (temp-session-root)))
           (loaded (sess:load-session-file fresh path (kli:make-kernel-host))))
      (is (eq :r3 (sess:session-leaf-id loaded)))
      (is (equal '(:r1 :r3)
                 (mapcar #'kli:object-id
                         (sess:session-branch fresh loaded nil)))
          "the retracted entry left the durable copy"))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test file-session-load-drops-orphaned-subtree-after-gap
  "A torn write can leave a gap mid-file: entries after the missing one
reference a parent that never loads. The load drops that orphaned subtree
instead of bricking the whole session, keeps the readable prefix, and repoints
the leaf off the dropped entry the header pinned."
  (let* ((root (temp-session-root))
         (store (sess:make-file-session-store root))
         (path (sess:session-file-path store :gap))
         (ctx (kli:make-kernel-host)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-session-file-header :gap :leaf-id :g5)))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "kept1" :id :gm1) :id :g1)))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-assistant-message "kept2" :id :gm2)
                             :id :g2 :parent-id :g1)))
      ;; :g3 is the torn gap -- never written; :g4/:g5 descend from it.
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "orphan1" :id :gm4)
                             :id :g4 :parent-id :g3)))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-assistant-message "orphan2" :id :gm5)
                             :id :g5 :parent-id :g4))))
    (let ((loaded (sess:load-session-file store path ctx)))
      (is (equal '(:g1 :g2)
                 (mapcar #'kli:object-id
                         (sess:session-branch store loaded nil)))
          "the orphaned subtree dropped, the readable prefix survived")
      (is (eq :g2 (sess:session-leaf-id loaded))
          "the leaf repointed off the dropped entry the header pinned")
      (is (null (sess:session-entry-by-id store loaded :g5))
          "the orphaned entry never entered the session"))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test file-session-post-fault-append-rewrites-and-resyncs-disk
  "A contained write fault leaves the file behind memory. The session is marked
desynced, so the next append rewrites the whole file from memory rather than
appending past the gap -- the durable copy re-syncs and reload sees every entry."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (session (sess:create-session store ctx :id :resync))
         (path (sess:session-file-path store :resync)))
    (flet ((append! (content id)
             (sess:append-session-entry
              store session
              (sess:make-message-entry (sess:make-user-message content) :id id)
              ctx))
           (reloaded-ids ()
             (let ((fresh (sess:make-file-session-store (temp-session-root))))
               (mapcar #'kli:object-id
                       (sess:session-branch
                        fresh
                        (sess:load-session-file fresh path
                                                (kli:make-kernel-host))
                        nil)))))
      (append! "one" :rs1)
      (let ((fail nil)
            (original (fdefinition 'sess::write-session-datum)))
        (unwind-protect
             (progn
               (setf (fdefinition 'sess::write-session-datum)
                     (lambda (stream record)
                       (when fail (error "simulated write failure"))
                       (funcall original stream record)))
               (setf fail t)
               ;; Bind NIL to exercise the production :continue barrier rather
               ;; than the suite-wide :escalate, so the fault is contained.
               (let ((ext:*extension-fault-policy* nil))
                 (append! "two" :rs2)))
          (setf (fdefinition 'sess::write-session-datum) original)))
      (is (equal '(:rs1) (reloaded-ids))
          "the contained fault left :rs2 in memory but not on disk")
      (is (equal '(:rs1 :rs2)
                 (mapcar #'kli:object-id
                         (sess:session-branch store session nil)))
          "memory keeps the entry the durable write dropped")
      (append! "three" :rs3)
      (is (equal '(:rs1 :rs2 :rs3) (reloaded-ids))
          "the heal rewrote the whole chain, re-syncing disk with memory"))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test blank-session-rows-detected-and-skipped-by-listing-consumers
  "BLANK-SESSION-ROW-P marks listing rows with nothing to resume: no
entries, no name, not corrupt. Corrupt rows are never blank -- their files
must stay visible."
  (is (sess:blank-session-row-p '(:id :x :entry-count 0)))
  (is (not (sess:blank-session-row-p '(:id :x :entry-count 0 :name "kept"))))
  (is (not (sess:blank-session-row-p '(:id :x :entry-count 3))))
  (is (not (sess:blank-session-row-p '(:id :x :entry-count 0 :corrupt t)))))

(test append-unserializable-entry-leaves-memory-and-disk-consistent
  "The file store serializes the entry as a discarded probe before mutating
memory, so an unserializable payload is rejected with the in-memory entries
unchanged and the live chain still equal to the on-disk chain -- no half-write
that would diverge RAM from the file."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (session (sess:create-session store ctx :id :probe-reject)))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "good" :id :gm1) :id :ge1)
     ctx)
    (let ((count-before (sess:session-entry-count session)))
      (signals sess:unserializable-value
        (sess:append-session-entry
         store session
         (sess:make-custom-entry :ui-state :id :bad-entry :data (list :bad 3.14))
         ctx))
      (is (= count-before (sess:session-entry-count session))
          "the rejected entry never entered the in-memory table")
      (let* ((path (sess:session-file-path store :probe-reject))
             (fresh (sess:make-file-session-store (temp-session-root)))
             (loaded (sess:load-session-file fresh path (kli:make-kernel-host))))
        (is (equal (mapcar #'kli:object-id (sess:session-branch store session nil))
                   (mapcar #'kli:object-id (sess:session-branch fresh loaded nil)))
            "the on-disk chain matches the live chain")))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(defvar *id-mint-race-counter* (kli:make-id-counter))

(defun mint-ids-concurrently (mint n-threads per-thread)
  "Every id minted by N-THREADS each calling MINT PER-THREAD times."
  (let ((ids nil) (lock (sb-thread:make-mutex)))
    (mapc #'sb-thread:join-thread
          (loop repeat n-threads
                collect (sb-thread:make-thread
                         (lambda ()
                           (let ((local (loop repeat per-thread collect (funcall mint))))
                             (sb-thread:with-mutex (lock)
                               (setf ids (nconc local ids))))))))
    ids))

(test atomic-id-mint-yields-distinct-under-contention
  "The id counter mints lock-free via atomic-incf, so concurrent workers minting
at once never collide. A plain incf of a special var would lose ids to the
read-modify-write race."
  (let* ((threads 8) (per 500) (total (* threads per))
         (ids (mint-ids-concurrently
               (lambda () (kli:next-counter-value '*id-mint-race-counter*))
               threads per)))
    (is (= total (length ids)))
    (is (= total (length (remove-duplicates ids)))
        "every minted id is distinct")))

(test concurrent-appends-under-lock-lose-no-entry
  "Every concurrent append on one session lands exactly once under the
per-session mutex, with reads running alongside: the entry table and order stay
consistent and no entry is lost. An unsynchronized table write would corrupt
SBCL's hash chains outright."
  (let* ((ctx (kli:make-kernel-host))
         (store (sess:make-session-store))
         (session (sess:create-session store ctx :id :concurrent))
         (threads 8) (per 200) (total (* threads per))
         (readers (loop repeat 2
                        collect (sb-thread:make-thread
                                 (lambda ()
                                   (loop repeat 3000
                                         do (sess:session-entry-count session)
                                            (sess:session-entries session))))))
         (writers (loop for tid below threads
                        collect (let ((tid tid))
                                  (sb-thread:make-thread
                                   (lambda ()
                                     (loop for i below per
                                           do (sess:append-session-entry
                                               store session
                                               (sess:make-message-entry
                                                (sess:make-user-message "m")
                                                :id (intern (format nil "C-~D-~D" tid i)
                                                            :keyword))
                                               ctx))))))))
    (mapc #'sb-thread:join-thread (append writers readers))
    (is (= total (sess:session-entry-count session)))
    (is (= total (length (sess:session-entries session))))
    (is (= total (length (remove-duplicates
                          (mapcar #'kli:object-id (sess:session-entries session)))))
        "no entry was dropped or duplicated")))
