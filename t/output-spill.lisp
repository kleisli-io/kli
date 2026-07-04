(in-package #:kli/tests)
(in-suite all)

;;; The shared materialization backend and the read-result / search-result
;;; retrieval tools. Each test binds *output-spill-directory* to a fresh per-test
;;; temp dir and small budget/page values, so nothing touches the real /tmp store.
;;; Reap and the age-sweep delete directories: the dev REPL sandbox blocks rmdir,
;;; but the `nix build ... tests` gate does not, so those assertions run for real
;;; there.

(defvar *output-spill-test-counter* 0)

(defun output-spill-temp-root ()
  (namestring
   (merge-pathnames (format nil "kli-output-spill-test-~D-~D/"
                            (get-universal-time)
                            (incf *output-spill-test-counter*))
                    #p"/tmp/")))

(defmacro with-spill-store ((proto) &body body)
  `(let* ((spill:*output-spill-directory* (output-spill-temp-root))
          (,proto (ext:make-extension-protocol)))
     (unwind-protect (progn ,@body)
       (ignore-errors
        (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                    :validate t :if-does-not-exist :ignore)))))

(defmacro with-spill-tools ((proto ctx) &body body)
  "Like WITH-SPILL-STORE, plus a kernel-host CTX whose active protocol is PROTO,
so the read-result / search-result runners resolve PROTO via active-protocol."
  `(let* ((spill:*output-spill-directory* (output-spill-temp-root))
          (,ctx (kli:make-kernel-host))
          (,proto (ext:make-extension-protocol)))
     (setf (kli:active-protocol ,ctx) ,proto)
     (unwind-protect (progn ,@body)
       (ignore-errors
        (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                    :validate t :if-does-not-exist :ignore)))))

(defun spill-file-mode (path)
  (logand (sb-posix:stat-mode (sb-posix:stat (uiop:native-namestring path)))
          #o777))

(defun spill-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s)))

(defun spill-result-text (result)
  (getf (first (ext:tool-result-content result)) :text))

(defun spill-run-files (proto)
  "Names on disk under PROTO's run-dir via POSIX readdir -- robust where CL
directory truename resolution (and its dotless-name globbing) is not."
  (let ((handle (sb-posix:opendir
                 (string-right-trim
                  "/" (uiop:native-namestring (spill:output-spill-run-directory proto)))))
        (names '()))
    (unwind-protect
         (loop for entry = (sb-posix:readdir handle)
               until (sb-alien:null-alien entry)
               for name = (sb-posix:dirent-name entry)
               unless (member name '("." "..") :test #'string=)
                 do (push name names))
      (sb-posix:closedir handle))
    names))

(defun write-byte-file (path string)
  (with-open-file (stream path :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede :if-does-not-exist :create)
    (write-sequence (sb-ext:string-to-octets string :external-format :utf-8)
                    stream))
  path)

(defun write-multibyte-lines (path n &key middle)
  "Write N lines to PATH; every 7th carries multibyte content; line MIDDLE (when
given) is a unique ASCII marker. Returns PATH."
  (with-open-file (s path :direction :output :external-format :utf-8
                          :if-exists :supersede :if-does-not-exist :create)
    (dotimes (i n)
      (cond ((eql i middle) (write-string "THE-MIDDLE-MARKER" s))
            ((zerop (mod i 7)) (format s "line ~D 中文 😀 Ωμέγα" i))
            (t (format s "line ~D plain" i)))
      (write-char #\Newline s)))
  path)

;;; Write modes ------------------------------------------------------------

(test output-spill-write-string-mode
  "write-string-spill records a :file entry, writes the content verbatim, and
hardens the file to 0600."
  (with-spill-store (p)
    (let ((entry (spill:write-string-spill p "hello world" :producer-uuid "uuidA")))
      (is (spill:spill-entry-token entry) "an entry is returned")
      (is (eq :file (spill:spill-entry-kind entry)) "the backing is a file")
      (is (string= "hello world" (uiop:read-file-string (spill:spill-entry-path entry)))
          "the content round-trips verbatim")
      (is (= #o600 (spill-file-mode (spill:spill-entry-path entry)))
          "the spill file is mode 0600")
      (is (string= "uuidA" (spill:spill-entry-producer-uuid entry))
          "the producer uuid is recorded for attribution"))))

(test output-spill-adopt-file-keeps-whole
  "adopt-file-spill moves an on-disk capture in whole: a small file renames
zero-copy, a large file is kept whole and lossless (never capped); the source is
consumed either way."
  (with-spill-store (p)
    (let ((spill:*output-spill-session-budget* 100000))
      (spill:ensure-run-directory p)
      (let* ((small (write-byte-file (spill:run-dir-file p "small.tmp") "compact"))
             (small-entry (spill:adopt-file-spill p small)))
        (is (eq :file (spill:spill-entry-kind small-entry)) "a file backing is registered")
        (is (not (probe-file small)) "the source is consumed by the rename")
        (is (string= "compact" (uiop:read-file-string (spill:spill-entry-path small-entry)))
            "the adopted content is intact"))
      (let* ((payload (make-string 5000 :initial-element #\z))
             (big (write-byte-file (spill:run-dir-file p "big.tmp") payload))
             (big-entry (spill:adopt-file-spill p big)))
        (is (= 5000 (spill:spill-entry-bytes big-entry)) "the large capture is kept whole")
        (is (string= payload (uiop:read-file-string (spill:spill-entry-path big-entry)))
            "the whole large capture round-trips byte-for-byte")
        (is (not (probe-file big)) "the oversized source is consumed")))))

(test output-spill-over-budget-result-is-kept-whole
  "A lone result larger than the session budget is kept whole and lossless: the
backing never head+tail-caps; eviction drops whole oldest results, not the middle
of a retained one."
  (with-spill-store (p)
    (let ((spill:*output-spill-session-budget* 1000))
      (let* ((payload (make-string 5000 :initial-element #\x))
             (entry (spill:write-string-spill p payload)))
        (is (= 5000 (spill:spill-entry-bytes entry)) "the whole 5000 bytes are retained")
        (is (string= payload (uiop:read-file-string (spill:spill-entry-path entry)))
            "the over-budget result round-trips byte-for-byte")))))

(test output-spill-tee-is-byte-exact-across-sizes-and-chunkings
  "The bounded-tee backing equals the full produced output byte-for-byte for every
size and chunking, while the in-image window stays within its limit and equals the
head. The load-bearing correctness guarantee."
  (with-spill-store (p)
    (let ((spill:*output-spill-session-budget* (* 16 1024 1024))
          (limit 100))
      (dolist (size '(0 10 500 30000 30001))
        (dolist (chunk '(1 7 64 8192))
          (let* ((full (with-output-to-string (s)
                         (dotimes (i size)
                           (write-char (code-char (+ 32 (mod (* i 7) 90))) s))))
                 (tee (spill:open-spill-tee p :window-limit limit))
                 (window nil) (truncated nil))
            (unwind-protect
                 (loop for i from 0 below (length full) by chunk
                       do (write-string full tee :start i
                                                  :end (min (length full) (+ i chunk))))
              (setf window (spill:tee-window tee)
                    truncated (spill:tee-truncated-p tee))
              (spill:finalize-spill-tee tee))
            (let* ((entry (first (spill:spill-registry-entries
                                  (spill:ensure-spill-registry p))))
                   (spilled (uiop:read-file-string (spill:spill-entry-path entry))))
              (is (string= spilled full)
                  "size ~D chunk ~D: backing is byte-exact" size chunk)
              (is (<= (length window) limit)
                  "size ~D chunk ~D: window within limit" size chunk)
              (is (string= window (subseq full 0 (min size limit)))
                  "size ~D chunk ~D: window is the head" size chunk)
              (is (eq truncated (> size limit))
                  "size ~D chunk ~D: truncated-p is exact" size chunk))))))))

(test output-spill-tee-finalizes-a-partial-on-producer-error
  "A producer that throws mid-stream still lands the produced prefix when finalize
runs from the caller's unwind-protect -- the backing is lossless of what existed."
  (with-spill-store (p)
    (let ((entry nil))
      (ignore-errors
       (let ((tee (spill:open-spill-tee p :window-limit 10)))
         (unwind-protect
              (progn (write-string "abcdefghij-partial" tee)
                     (error "producer blew up"))
           (setf entry (spill:finalize-spill-tee tee)))))
      (is (and entry t) "the partial backing is finalized despite the error")
      (is (string= "abcdefghij-partial"
                   (uiop:read-file-string (spill:spill-entry-path entry)))
          "the produced prefix is preserved exactly"))))

(test output-spill-register-sequence-mode
  "register-sequence-spill records an in-image :sequence entry with no file and no
budget bytes (the data is already resident)."
  (with-spill-store (p)
    (let ((entry (spill:register-sequence-spill p '("alpha" "beta" "gamma"))))
      (is (eq :sequence (spill:spill-entry-kind entry)) "the backing is a sequence")
      (is (null (spill:spill-entry-path entry)) "a sequence backing has no file")
      (is (= 0 (spill:spill-entry-bytes entry)) "a sequence backing counts no disk bytes")
      (is (= 3 (spill:spill-entry-element-count entry)) "the element count is recorded"))))

;;; Naming, hardening, durability -----------------------------------------

(test output-spill-opaque-naming
  "Tokens are 12-char lowercase-hex handles, distinct per spill, leaking nothing of
the content."
  (with-spill-store (p)
    (let ((tokens (loop repeat 50
                        collect (spill:spill-entry-token
                                 (spill:write-string-spill p "x")))))
      (is (= 50 (length (remove-duplicates tokens :test #'string=)))
          "50 spills mint 50 distinct handles")
      (is (every (lambda (tk)
                   (and (= 12 (length tk))
                        (every (lambda (c) (digit-char-p c 16)) tk)))
                 tokens)
          "each handle is 12 hex characters"))))

(test output-spill-run-directory-is-0700
  "The run directory the store creates is owned by us at mode 0700."
  (with-spill-store (p)
    (spill:write-string-spill p "x")
    (is (eq :ok (spill:directory-hardening-status (spill:output-spill-run-directory p)))
        "the run-dir passes the hardening check")
    (is (= #o700 (logand (sb-posix:stat-mode
                          (sb-posix:stat (string-right-trim
                                          "/" (uiop:native-namestring
                                               (spill:output-spill-run-directory p)))))
                         #o777))
        "the run-dir is mode 0700")))

(test output-spill-budget-evicts-oldest-first
  "Under budget pressure the oldest spills are evicted and their files unlinked,
keeping total-bytes within the session budget."
  (with-spill-store (p)
    (let ((spill:*output-spill-session-budget* 1000))
      (let* ((e1 (spill:write-string-spill p (make-string 300 :initial-element #\a)))
             (e2 (spill:write-string-spill p (make-string 300 :initial-element #\b)))
             (e3 (spill:write-string-spill p (make-string 300 :initial-element #\c)))
             (e4 (spill:write-string-spill p (make-string 300 :initial-element #\d)))
             (reg (spill:ensure-spill-registry p)))
        (declare (ignore e2 e3))
        (is (<= (spill:spill-registry-total-bytes reg) 1000)
            "total bytes stay within the budget")
        (is (= 3 (length (spill:spill-registry-entries reg)))
            "the oldest entry was evicted")
        (is (not (probe-file (spill:spill-entry-path e1)))
            "the evicted spill's file is unlinked")
        (is (and (probe-file (spill:spill-entry-path e4)) t)
            "the newest spill survives")))))

(test output-spill-kill-switch-disables-all-modes
  "With *output-spill-enabled* nil every write mode returns nil and no store
directory is created -- the caller falls back to its plain window."
  (with-spill-store (p)
    (let ((spill:*output-spill-enabled* nil))
      (is (null (spill:write-string-spill p "x")) "write-string is a no-op")
      (is (null (spill:register-sequence-spill p '("x"))) "register-sequence is a no-op")
      (is (null (spill:open-spill-tee p)) "open-spill-tee yields no stream")
      (is (not (probe-file (spill:output-spill-run-directory p)))
          "no run directory is created"))))

(test output-spill-failure-degrades-without-a-path
  "When the store root cannot be written the write mode returns nil, so the caller
emits a degraded, handle-less marker instead of pointing at a missing file."
  (let ((spill:*output-spill-directory* "/proc/kli-spill-cannot-exist/")
        (p (ext:make-extension-protocol)))
    (is (null (spill:write-string-spill p "data"))
        "an unwritable root yields no entry")))

(test output-spill-hardens-a-symlinked-root
  "A symlink planted where the base belongs is detected and recreated as a
directory we own, never followed."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (base (spill:output-spill-base-directory))
         (target (format nil "/tmp/kli-spill-symlink-target-~D/"
                         (incf *output-spill-test-counter*))))
    (ensure-directories-exist (pathname target))
    (unwind-protect
         (progn
           (sb-posix:symlink target (string-right-trim "/" (uiop:native-namestring base)))
           (is (eq :symlink (spill:directory-hardening-status base))
               "the planted symlink is detected")
           (spill:ensure-hardened-directory base)
           (is (eq :ok (spill:directory-hardening-status base))
               "hardening replaces it with an owned directory"))
      (ignore-errors (uiop:delete-directory-tree (pathname base)
                                                 :validate t :if-does-not-exist :ignore))
      (ignore-errors (uiop:delete-directory-tree (pathname target)
                                                 :validate t :if-does-not-exist :ignore)))))

(test output-spill-concurrent-spills-are-unique-and-lossless
  "16 threads x 64 spills over one registry mint 1024 distinct handles with no lost
entries and no orphaned files -- the mutex closes the registry data race."
  (with-spill-store (p)
    ;; A spawned thread does not inherit the parent's dynamic bindings, so each
    ;; worker rebinds the store specials to the test's values; otherwise the
    ;; threads would spill into the real global store.
    (let ((root spill:*output-spill-directory*)
          (budget (* 64 1024 1024)))
      (let* ((threads
               (loop for tid below 16
                     collect (let ((tid tid))
                               (sb-thread:make-thread
                                (lambda ()
                                  (let ((spill:*output-spill-directory* root)
                                        (spill:*output-spill-session-budget* budget))
                                    (loop for i below 64
                                          collect (spill:spill-entry-token
                                                   (spill:write-string-spill
                                                    p (format nil "t~D-i~D" tid i))))))))))
             (tokens (loop for th in threads append (sb-thread:join-thread th)))
             (reg (spill:ensure-spill-registry p)))
        (is (= 1024 (length tokens)) "every spill returned a handle")
        (is (= 1024 (length (remove-duplicates tokens :test #'string=)))
            "all 1024 handles are distinct")
        (is (= 1024 (length (spill:spill-registry-entries reg)))
            "no registry entry was lost")
        (is (= 1024 (spill:spill-registry-counter reg)) "the counter is exact")
        (let ((on-disk (length (spill-run-files p))))
          (is (= 1024 on-disk) "no file is orphaned (~D on disk)" on-disk))))))

(test output-spill-reap-removes-the-run-and-clears-the-registry
  "Reaping deletes the whole run directory and clears the registry slot; the
retract effect fn does the same."
  (with-spill-store (p)
    (spill:write-string-spill p "to-reap")
    (let ((run (spill:output-spill-run-directory p)))
      (spill:reap-spills p)
      (is (not (probe-file run)) "the run directory is gone")
      (let ((fresh (spill:ensure-spill-registry p)))
        (is (and (zerop (spill:spill-registry-counter fresh))
                 (null (spill:spill-registry-entries fresh)))
            "the registry slot was cleared (re-creation yields a fresh, empty one)"))))
  (with-spill-store (p)
    (spill:write-string-spill p "to-retract")
    (let ((run (spill:output-spill-run-directory p)))
      (spill:retract-output-spill-reap p nil nil)
      (is (not (probe-file run)) "the retract effect reaps the run directory"))))

(test output-spill-sweep-selects-by-age
  "The startup sweep deletes run-subdirs older than an explicit cutoff: a future
cutoff sweeps every run, a past cutoff sweeps none."
  (with-spill-store (p)
    (let ((p2 (ext:make-extension-protocol)))
      (spill:write-string-spill p "a")
      (spill:write-string-spill p2 "b")
      (let ((base (spill:output-spill-base-directory)))
        (is (= 0 (spill:sweep-stale-spills :cutoff 0 :base base))
            "a past cutoff sweeps nothing")
        (is (= 2 (spill:sweep-stale-spills
                  :cutoff (* 2 (get-universal-time)) :base base))
            "a future cutoff sweeps every run-dir")))))

(test output-spill-extension-deactivate-reaps-run
  "The output-spill defextension wires the reap effect through ordinary extension
deactivation: retracting the extension deletes this protocol's run directory and
clears the registry slot."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (let ((extension (install-extension context spill:*output-spill-extension-manifest*)))
           (let* ((entry (spill:write-string-spill protocol "to-deactivate"))
                  (token (spill:spill-entry-token entry))
                  (run (spill:output-spill-run-directory protocol)))
             (is (probe-file run) "the spill run directory exists before deactivation")
             (with-extension-load-authority
               (ext:deactivate-extension protocol extension context))
             (is (not (probe-file run)) "deactivation reaps the run directory")
             (is (null (spill:find-spill-entry protocol token))
                 "the old handle no longer resolves after registry clear")
             (let ((fresh (spill:ensure-spill-registry protocol)))
               (is (and (zerop (spill:spill-registry-counter fresh))
                        (null (spill:spill-registry-entries fresh)))
                   "the next registry access creates a fresh empty registry"))))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test output-spill-extension-install-runs-startup-sweep
  "Installing the output-spill defextension runs the crash-backstop sweep over the
uid-scoped base before any new spill is created."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (spill:*output-spill-sweep-ttl-seconds* -1)
         (context (kli:make-kernel-host)))
    (unwind-protect
         (let* ((base (spill:output-spill-base-directory))
                (stale (merge-pathnames "stale-run/" base)))
           (spill:ensure-hardened-directory base)
           (spill:ensure-hardened-directory stale)
           (write-byte-file (merge-pathnames "payload" stale) "old")
           (is (probe-file stale) "the stale run directory exists before install")
           (switch-to-extension-protocol context)
           (install-extension context spill:*output-spill-extension-manifest*)
           (is (not (probe-file stale))
               "extension install swept an older run directory from the base"))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

;;; Streaming primitives ---------------------------------------------------

(test output-spill-pager-reconstructs-byte-exact
  "read-lines-from-offset pages a multibyte file losslessly: every line equals the
read-line reference, the in-memory window stays bounded, and the final offset is
the file length. The lossless-backing invariant the handle's promise rests on."
  (with-spill-store (p)
    (spill:ensure-run-directory p)
    (let* ((path (write-multibyte-lines (spill:run-dir-file p "pager.txt") 3000
                                        :middle 1500))
           (reference (with-open-file (s path :external-format :utf-8)
                        (loop for l = (read-line s nil :eof) until (eq l :eof) collect l)))
           (paged '()) (offset 0) (max-window 0) (pages 0))
      (loop
        (multiple-value-bind (lines next eof)
            (spill:read-lines-from-offset path offset 137)
          (setf max-window (max max-window (length lines)))
          (incf pages)
          (setf paged (nconc paged lines) offset next)
          (when eof (return))))
      (is (equal reference paged) "every line reconstructs byte-exact, multibyte intact")
      (is (= 3000 (length paged)) "all 3000 lines are returned")
      (is (<= max-window 137) "the in-memory window never exceeds the page size")
      (is (= offset (spill-file-bytes path)) "the final offset equals the file length"))))

(test output-spill-search-within-reaches-the-middle
  "search-within finds the exact middle line (head+tail loses it, the filesystem
search skips a >2 MB file), counts every multibyte match across pages, and
paginates by a line cursor."
  (with-spill-store (p)
    (spill:ensure-run-directory p)
    (let ((path (write-multibyte-lines (spill:run-dir-file p "search.txt") 3000
                                       :middle 1500)))
      (multiple-value-bind (matches next eof)
          (spill:search-within path "THE-MIDDLE-MARKER" :max-matches 5)
        (declare (ignore next eof))
        (is (= 1 (length matches)) "the middle marker is found")
        (is (= 1500 (car (first matches))) "at its true line number"))
      (let ((total 0) (after 0) (firsts nil))
        (loop
          (multiple-value-bind (matches next eof)
              (spill:search-within path "中文" :start-line after :max-matches 1000)
            (when (and (null firsts) matches)
              (setf firsts (mapcar #'car (subseq matches 0 (min 3 (length matches))))))
            (incf total (length matches))
            (setf after next)
            (when eof (return))))
        (is (= (ceiling 3000 7) total) "every multibyte line is found across pages")
        (is (equal '(0 7 14) firsts) "the first matches are the expected lines")))))

(test output-spill-random-access-and-past-eof
  "byte-offset-of-line + page-lines give random access to a deep line and degrade
gracefully past EOF."
  (with-spill-store (p)
    (spill:ensure-run-directory p)
    (let ((path (write-multibyte-lines (spill:run-dir-file p "ra.txt") 3000)))
      (is (= 0 (spill:byte-offset-of-line path 0)) "line 0 starts at byte 0")
      (is (null (spill:byte-offset-of-line path 99999)) "a line past EOF has no offset")
      (multiple-value-bind (lines next eof) (spill:page-lines path 2000 1)
        (declare (ignore next eof))
        (is (string= "line 2000 plain" (first lines)) "page-lines reaches a deep line"))
      (is (equal '(nil nil t) (multiple-value-list (spill:page-lines path 99999 5)))
          "a page past EOF returns empty + eof"))))

(test output-spill-sequence-pager-and-scanner
  "read-sequence-window slices an in-image backing and search-sequence scans it,
both returning a continuation cursor; a window past the end degrades gracefully."
  (let ((els (coerce (loop for i below 500
                           collect (if (zerop (mod i 50))
                                       (format nil "id-~D-tag" i)
                                       (format nil "id-~D" i)))
                     'simple-vector)))
    (multiple-value-bind (win next eof) (spill:read-sequence-window els 100 25)
      (is (string= "id-100-tag" (first win)) "the window starts at the requested index")
      (is (= 25 (length win)) "the window honors the limit")
      (is (= 125 next) "the next-index cursor advances")
      (is (not eof) "more remains"))
    (multiple-value-bind (matches next eof) (spill:search-sequence els "tag" 0 1000)
      (declare (ignore next))
      (is (= 10 (length matches)) "every tagged element is matched")
      (is (equal '(0 . "id-0-tag") (first matches)) "with its index and printed form")
      (is-true eof "the scan reaches the end"))
    (is (equal '(nil 500 t) (multiple-value-list (spill:read-sequence-window els 5000 10)))
        "a window past the end returns empty + eof")))

;;; Marker -----------------------------------------------------------------

(test output-spill-marker-formats-content-text
  "The handle marker renders a content-text line for each case: a full result with
a handle (byte or element unit), a degraded handle-less result, and a plain
truncation."
  (let ((full (spill:format-spill-marker "stdout" :shown 30000 :total 812043
                                         :handle "9f2a1c4b"))
        (seq (spill:format-spill-marker "object ids" :shown 200 :total 5000
                                        :unit "element" :handle "abc123abc123"))
        (degraded (spill:format-spill-marker "stderr" :shown 30000 :degraded t))
        (plain (spill:format-spill-marker "eval value" :shown 100)))
    (is (search "handle 9f2a1c4b" full) "the full marker carries the handle")
    (is (search "812,043-byte" full) "the full marker reports the byte total")
    (is (search "read-result" full) "the full marker names the retrieval tool")
    (is (search "5,000-element" seq) "a sequence marker uses the element unit")
    (is (and (search "could not be retained" degraded) (not (search "handle" degraded)))
        "the degraded marker omits any handle")
    (is (and (search "100" plain) (not (search "handle" plain)))
        "the plain marker is just a truncation notice")))

;;; read-result / search-result tools --------------------------------------

(test output-spill-read-result-tool-pages-both-backings
  "read-result pages a :file backing by line and a :sequence backing by element,
each from an arbitrary start, advertising the next-cursor and the end of result."
  (with-spill-tools (p ctx)
    (let* ((big (with-output-to-string (s) (dotimes (i 1000) (format s "row ~D~%" i))))
           (fh (spill:spill-entry-token (spill:write-string-spill p big)))
           (sh (spill:spill-entry-token
                (spill:register-sequence-spill
                 p (loop for i below 30 collect (format nil "el-~D" i))))))
      (let ((text (spill-result-text
                   (spill::run-read-result-tool nil (list :handle fh :start 500 :limit 3) ctx))))
        (is (search "row 500" text) "the file window begins at the requested line")
        (is (search "row 502" text) "and spans the limit")
        (is (not (search "row 503" text)) "without overrunning it")
        (is (search "start 503" text) "the footer advertises the next-line cursor"))
      (let ((text (spill-result-text
                   (spill::run-read-result-tool nil (list :handle fh :start 998 :limit 50) ctx))))
        (is (search "row 999" text) "a window reaches the last line")
        (is (search "end of result" text) "and marks the end"))
      (let ((text (spill-result-text
                   (spill::run-read-result-tool nil (list :handle sh :start 0 :limit 2) ctx))))
        (is (search "el-0" text) "the sequence window begins at the requested element")
        (is (search "el-1" text) "and spans the limit")
        (is (search "start 2" text) "advertising the next-element cursor")))))

(test output-spill-read-result-tool-rejects-unknown-handle
  "read-result on an unknown or evicted handle returns a clean error result, never
pointing at a backing that is not there."
  (with-spill-tools (p ctx)
    (spill:write-string-spill p "present")
    (let ((r (spill::run-read-result-tool nil (list :handle "deadbeefdead") ctx)))
      (is (ext:tool-result-error-p r) "the result is flagged an error")
      (is (search "unknown or already evicted" (spill-result-text r))
          "with an explanatory message"))))

(test output-spill-search-result-tool-finds-and-paginates
  "search-result returns matches as `N: text` with a continuation cursor, resumes
from an after-cursor, and rejects a malformed pattern cleanly."
  (with-spill-tools (p ctx)
    (let* ((body (with-output-to-string (s)
                   (dotimes (i 1000)
                     (format s "row ~D ~:[plain~;HIT~]~%" i (zerop (mod i 100))))))
           (fh (spill:spill-entry-token (spill:write-string-spill p body))))
      (let ((text (spill-result-text
                   (spill::run-search-result-tool nil (list :handle fh :pattern "HIT" :max 5) ctx))))
        (is (search "0: row 0 HIT" text) "the first match carries its line number")
        (is (search "5 matches" text) "the page count is reported")
        (is (search "after " text) "and the continuation cursor is advertised"))
      (let ((text (spill-result-text
                   (spill::run-search-result-tool nil (list :handle fh :pattern "HIT" :after 500) ctx))))
        (is (search "500: row 500 HIT" text) "an after-cursor resumes mid-result")
        (is (search "end of result" text) "and the tail reaches the end"))
      (let ((r (spill::run-search-result-tool nil (list :handle fh :pattern "(oops") ctx)))
        (is (ext:tool-result-error-p r) "a malformed pattern errors cleanly")))))

(test (output-spill-tool-registers-and-pages-through-invoke :fixture tool-authority)
  "read-result registers as a real tool and pages through invoke-tool: a
:tools/standard grant confers :result/read, so the gated tool is admitted and runs
end-to-end over the wire."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-extensions context spill:*output-spill-extension-manifest*)
           (let* ((big (with-output-to-string (s) (dotimes (i 500) (format s "line ~D~%" i))))
                  (handle (spill:spill-entry-token (spill:write-string-spill protocol big)))
                  (result (ext:invoke-tool protocol :read-result
                                           (list :handle handle :start 100 :limit 3)
                                           context)))
             (is (not (ext:tool-result-error-p result)) "the gated tool is admitted and runs")
             (is (search "line 100" (spill-result-text result)) "it pages from the requested line")
             (is (search "start 103" (spill-result-text result)) "and advertises the next cursor")))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))
