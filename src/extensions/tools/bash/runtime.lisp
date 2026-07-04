(in-package #:kli/tools/bash)

(defparameter *default-bash-timeout-seconds* 30)
(defparameter *maximum-bash-timeout-seconds* 300)

(defparameter *bash-output-character-limit* 30000
  "Largest stdout or stderr capture the bash tool returns to the model, in
characters. A single tool result feeds the model context whole, so a chatty
command -- a recursive find, a verbose build log -- can overflow the context
window in one turn while still fitting in memory: the cap is a context budget,
not a heap guard, and sits far below any model's window so the conversation and
concurrent results keep their room. The windowed reader keeps the head and a
tail joined by an elision marker, so the start and any terminal error both
survive the cut; because it reads at most ~LIMIT head plus ~4*TAIL tail bytes
and never the whole span, this same cap also bounds the heap.")

(defun output-file-byte-length (path)
  "PATH's length in bytes, or 0 when it does not exist."
  (with-open-file (stream path :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
    (if stream (file-length stream) 0)))

(defun read-output-file-tail (path byte-length tail-chars)
  "Last TAIL-CHARS characters of PATH. Seeks to a byte offset near the end, then
resyncs onto a UTF-8 lead byte so a character split by the seek degrades to a
replacement character rather than corrupting the read."
  (let* ((budget (* 4 tail-chars))
         (start (max 0 (- byte-length budget))))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (file-position stream start)
      (let* ((buffer (make-array (- byte-length start)
                                 :element-type '(unsigned-byte 8)))
             (count (read-sequence buffer stream))
             (index 0))
        (when (plusp start)
          (loop while (and (< index count)
                           (= #b10 (ash (aref buffer index) -6)))
                do (incf index)))
        (let ((text (sb-ext:octets-to-string buffer :start index :end count
                                             :external-format
                                             '(:utf-8 :replacement #\?))))
          (if (> (length text) tail-chars)
              (subseq text (- (length text) tail-chars))
              text))))))

(defun read-output-file-head (path offset head-chars)
  "First HEAD-CHARS characters of PATH at or after byte OFFSET. Resyncs onto a
UTF-8 lead byte so a character split by OFFSET degrades to a replacement
character rather than corrupting the read."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-position stream offset)
    (let* ((buffer (make-array (* 4 head-chars) :element-type '(unsigned-byte 8)))
           (count (read-sequence buffer stream))
           (index 0))
      (when (plusp offset)
        (loop while (and (< index count)
                         (= #b10 (ash (aref buffer index) -6)))
              do (incf index)))
      (let ((text (sb-ext:octets-to-string buffer :start index :end count
                                           :external-format
                                           '(:utf-8 :replacement #\?))))
        (if (> (length text) head-chars)
            (subseq text 0 head-chars)
            text)))))

(defun read-job-output-window (path offset limit)
  "Decode PATH's bytes from byte OFFSET to EOF, bounded to ~LIMIT characters. A
backlog within LIMIT decodes whole. A larger backlog returns a tail-weighted
window -- a small head, an elision marker, then the most recent tail -- and still
advances to EOF, so the next poll resumes at live: a liveness poll favors
freshness over completeness, and the full log is served by redirecting the job to
a file. Reads at most ~LIMIT head characters plus ~4*TAIL tail bytes, never the
whole span. Returns (values text new-offset truncated-p)."
  (let ((byte-length (output-file-byte-length path)))
    (cond
      ((<= byte-length offset) (values "" offset nil))
      ((<= (- byte-length offset) limit)
       (values (read-output-file-head path offset limit) byte-length nil))
      (t
       (let* ((head-chars (floor limit 3))
              (tail-chars (- limit head-chars))
              (head (read-output-file-head path offset head-chars))
              (tail (read-output-file-tail path byte-length tail-chars)))
         (values
          (format nil "~A~%[... +~:D characters dropped, advanced to live ...]~%~A"
                  head (- (- byte-length offset) limit) tail)
          byte-length t))))))

(defun read-windowed-output-file (path limit &key (head-fraction 2/3))
  "The whole content of PATH when it holds at most LIMIT characters, else its
first HEAD and last TAIL characters (HEAD+TAIL = LIMIT) joined by an elision
marker, so a terminal error survives an otherwise oversized capture. Each line is
front-truncated past the per-line render limit, so one structurally-long line
cannot flood the context. Returns (values text truncated-p), where truncated-p
flags the aggregate window. Reads at most ~LIMIT head characters plus ~4*TAIL tail
bytes, never the whole file. Bytes that fail to decode as UTF-8 read as
replacement characters."
  (let ((byte-length (with-open-file (stream path :element-type '(unsigned-byte 8)
                                                  :if-does-not-exist nil)
                       (if stream (file-length stream) 0))))
    (if (zerop byte-length)
        (values "" nil)
        (with-open-file (stream path :direction :input
                                     :external-format '(:utf-8 :replacement #\?))
          (let* ((probe (make-string (min (1+ limit) byte-length)))
                 (count (read-sequence probe stream)))
            (if (<= count limit)
                (values (render-bounded-lines (subseq probe 0 count)) nil)
                (let* ((head-chars (floor (* limit head-fraction)))
                       (tail-chars (- limit head-chars))
                       (head (subseq probe 0 head-chars))
                       (tail (read-output-file-tail path byte-length tail-chars)))
                  (values
                   (render-bounded-lines
                    (format nil "~A~%[... output truncated to first ~:D + last ~:D characters ...]~%~A"
                            head head-chars tail-chars tail))
                   t))))))))

(defparameter *interactive-bash-command-names*
  '("vi" "vim" "nvim" "nano" "emacs"
    "less" "more" "man"
    "top" "htop" "watch"
    "ssh" "mosh"
    "tmux" "screen"))

(defun read-shell-word (command start)
  (let ((index start)
        (length (length command))
        (quote nil))
    (values
     (with-output-to-string (word)
       (loop while (< index length)
             for char = (char command index)
             do (cond
                  ((and (not quote) (whitespace-char-p char))
                   (return))
                  ((and (not quote) (member char '(#\' #\") :test #'char=))
                   (setf quote char)
                   (incf index))
                  ((and quote (char= char quote))
                   (setf quote nil)
                   (incf index))
                  ((and (char= char #\\) (< (1+ index) length))
                   (incf index)
                   (write-char (char command index) word)
                   (incf index))
                  (t
                   (write-char char word)
                   (incf index)))))
     index)))

(defun shell-command-words (command &key (limit 12))
  (let ((index 0)
        (length (length command))
        (words '()))
    (loop while (and (< index length) (< (length words) limit))
          do (loop while (and (< index length)
                              (whitespace-char-p (char command index)))
                   do (incf index))
             (when (< index length)
               (multiple-value-bind (word next-index)
                   (read-shell-word command index)
                 (push word words)
                 (setf index next-index))))
    (nreverse words)))

(defun shell-basename (word)
  (let ((position (position #\/ word :from-end t)))
    (if position
        (subseq word (1+ position))
        word)))

(defun shell-assignment-word-p (word)
  (let ((position (position #\= word)))
    (and position
         (plusp position)
         (every (lambda (char)
                  (or (alphanumericp char) (char= char #\_)))
                (subseq word 0 position))
         (or (alpha-char-p (char word 0))
             (char= (char word 0) #\_)))))

(defun shell-option-word-p (word)
  (and (plusp (length word))
       (char= (char word 0) #\-)))

(defun effective-shell-command-word (command)
  (let ((skip-wrapper-options nil))
    (dolist (word (shell-command-words command))
      (let ((name (shell-basename word)))
        (cond
          ((shell-assignment-word-p word))
          ((and skip-wrapper-options (shell-option-word-p word)))
          ((member name '("env" "command" "exec" "time")
                   :test #'string=)
           (setf skip-wrapper-options t))
          ((member name '("sudo" "doas") :test #'string=)
           (setf skip-wrapper-options t))
          (t
           (return name)))))))

;;; Per-protocol bash policy: a plist of (:cwd :env-allowlist :default-timeout),
;;; serializable scalars/lists, consulted at every bash spawn. Each knob is
;;; opt-in -- a NIL policy changes nothing.

(defparameter +bash-policy-key+ :kli/tools/bash.policy
  "Per-protocol storage key holding the session's bash policy.")

(defun bash-policy (protocol)
  "The session's bash policy on PROTOCOL, or NIL when none is set."
  (protocol-storage protocol +bash-policy-key+))

(defun (setf bash-policy) (plist protocol)
  (setf (protocol-storage protocol +bash-policy-key+) plist))

(defun policy-cwd (policy) (getf policy :cwd))
(defun policy-env-allowlist (policy) (getf policy :env-allowlist))
(defun policy-default-timeout (policy) (getf policy :default-timeout))

(defun effective-directory (policy call-directory)
  "Per-call directory wins; else the policy's locked cwd; else NIL (inherit)."
  (or call-directory (policy-cwd policy)))

(defun effective-timeout (policy call-timeout builtin-default)
  "Per-call timeout wins; else the policy default; else the builtin default."
  (or call-timeout (policy-default-timeout policy) builtin-default))

(defparameter +bash-environment-floor-names+ '("PATH")
  "The execution substrate carried into every restricted child -- what a shell
needs to resolve commands at all. The allowlist layers the parent's other
variables on top, so it governs inheritance alone and never names this floor.")

(defun restricted-environment (allowlist
                               &optional (getenv #'sb-ext:posix-getenv))
  "The child environment for a shell-out as VAR=value strings, or NIL to inherit
the parent's whole environment unchanged.

With an explicit ALLOWLIST the child gets a strict whitelist: the execution floor
(PATH) plus each listed parent variable that is set. An allowed-but-unset
variable is simply absent.

With no ALLOWLIST the child inherits the parent environment unchanged (NIL).
Keeping a named variable out of a shell-out's environment is the process
sandbox's job, not the tool's."
  (when allowlist
    (loop for name in (remove-duplicates
                       (append +bash-environment-floor-names+ allowlist)
                       :from-end t :test #'string=)
          for value = (funcall getenv name)
          when value collect (concatenate 'string name "=" value))))

(defun interactive-shell-command-name (command)
  (let ((name (effective-shell-command-word command)))
    (and name
         (member name *interactive-bash-command-names* :test #'string=)
         name)))

(defun bash-timeout-seconds (parameters &optional policy)
  (let* ((value (effective-timeout policy
                                   (tool-parameter parameters :timeout)
                                   *default-bash-timeout-seconds*))
         (seconds (etypecase value
                    (integer value)
                    (string (parse-integer value)))))
    (unless (plusp seconds)
      (error "Bash timeout must be positive."))
    (min seconds *maximum-bash-timeout-seconds*)))

(defun bash-output-text (stdout stderr exit-code)
  (cond
    ((and (zerop exit-code) (plusp (length stdout)))
     stdout)
    ((and (plusp (length stdout)) (plusp (length stderr)))
     (format nil "~A~%~A" stdout stderr))
    ((plusp (length stdout))
     stdout)
    ((plusp (length stderr))
     stderr)
    ((zerop exit-code)
     "")
    (t
     (format nil "Command exited with status ~D." exit-code))))

(defun utf8-complete-prefix-length (bytes count)
  "Length of the largest prefix of BYTES[0,COUNT) that ends on a complete UTF-8
sequence, so a chunk split mid-character can emit only its decodable head and
carry the trailing bytes forward."
  (let ((index (1- count)))
    (loop while (and (>= index 0)
                     (= #b10 (ash (aref bytes index) -6)))
          do (decf index))
    (if (< index 0)
        count
        (let* ((lead (aref bytes index))
               (length (cond ((= 0 (ash lead -7)) 1)
                             ((= #b110 (ash lead -5)) 2)
                             ((= #b1110 (ash lead -4)) 3)
                             ((= #b11110 (ash lead -3)) 4)
                             (t 1))))
          (if (<= (+ index length) count) count index)))))

(defun make-incremental-decoder ()
  "A closure of one byte chunk to its decoded string, decoding only the complete
UTF-8 prefix and carrying any trailing partial sequence into the next call, so
concatenating every result reproduces the full decode."
  (let ((carry (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (lambda (chunk)
      (let ((buffer (make-array (+ (length carry) (length chunk))
                                :element-type '(unsigned-byte 8))))
        (replace buffer carry)
        (replace buffer chunk :start1 (length carry))
        (let* ((count (length buffer))
               (complete (utf8-complete-prefix-length buffer count))
               (text (sb-ext:octets-to-string buffer :start 0 :end complete
                                              :external-format
                                              '(:utf-8 :replacement #\?))))
          (setf carry (make-array (- count complete)
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer (- count complete)))
          (replace carry buffer :start2 complete)
          text)))))

(defun stream-new-output (path offset decoder on-update)
  "Read PATH from byte OFFSET to end, decode the newly complete UTF-8 prefix
through DECODER, and funcall ON-UPDATE with it when non-empty. Returns the byte
offset to resume from."
  (let ((length (with-open-file (stream path :element-type '(unsigned-byte 8)
                                             :if-does-not-exist nil)
                  (if stream (file-length stream) 0))))
    (if (<= length offset)
        offset
        (with-open-file (stream path :element-type '(unsigned-byte 8))
          (file-position stream offset)
          (let* ((buffer (make-array (- length offset)
                                     :element-type '(unsigned-byte 8)))
                 (count (read-sequence buffer stream))
                 (complete (utf8-complete-prefix-length buffer count))
                 (text (funcall decoder (subseq buffer 0 complete))))
            (when (plusp (length text))
              (funcall on-update text))
            (+ offset complete))))))

(defun wait-for-bash-process (process timeout-seconds &key on-update stream-path)
  "Wait for PROCESS, polling so the deadline and an abort request both cut the
wait short. When ON-UPDATE and STREAM-PATH are both supplied, funcall ON-UPDATE
with each newly captured tail as STREAM-PATH grows. Returns (values exit-code
timed-out-p aborted-p)."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second)))
        (offset 0)
        (decoder (and on-update stream-path (make-incremental-decoder))))
    (flet ((pump ()
             (when decoder
               (setf offset
                     (stream-new-output stream-path offset decoder on-update)))))
      (loop while (sb-ext:process-alive-p process)
            do (when (tool-abort-requested-p)
                 (return-from wait-for-bash-process (values nil nil t)))
               (when (>= (get-internal-real-time) deadline)
                 (return-from wait-for-bash-process (values nil t nil)))
               (pump)
               (sleep 0.05))
      (pump)
      (sb-ext:process-wait process)
      (values (or (sb-ext:process-exit-code process) -1) nil nil))))

(defvar *setsid-program* :unprobed
  "Absolute path of setsid(1), NIL when absent from PATH, :unprobed before
the first bash spawn. Probed once -- the answer cannot change mid-image.")

(defun setsid-program ()
  (when (eq *setsid-program* :unprobed)
    (setf *setsid-program*
          (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                              :separator ":")
                for path = (and (plusp (length dir))
                                (probe-file
                                 (merge-pathnames
                                  "setsid"
                                  (uiop:ensure-directory-pathname dir))))
                when path return (namestring path))))
  *setsid-program*)

(defun spawn-bash-process (shell command input directory
                           stdout-path stderr-path leader-path
                           &key environment)
  "Spawn SHELL -c COMMAND. When setsid(1) is available, run under `setsid -w` so
the child leads its own process group -- a timeout kill can then reap the whole
tree -- while run-program still tracks the child's true liveness and exit
status. Plain `setsid` forks and its parent exits, which run-program would read
as immediate completion, so the wait flag is essential. The child records its
own pid to LEADER-PATH so the group can be signalled by its leader, since
run-program holds the wait parent (in our own group, not the child's) and killpg
on a non-leader is ESRCH, a silent no-op. Returns (values process
leader-path-or-nil); the second value is the file holding the group leader pid,
or NIL when no group was created and the direct child is signalled instead.
ENVIRONMENT, when non-NIL, is the child's exact environment (a list of VAR=value
strings); NIL inherits the full environment."
  (let ((setsid (setsid-program)))
    (values (apply #'sb-ext:run-program
                   (or setsid shell)
                   (if setsid
                       (list "-w" shell "-c"
                             (format nil "printf '%d' $$ > \"~A\"~%~A"
                                     leader-path command))
                       (list "-c" command))
                   :search (not setsid)
                   :wait nil
                   :input (make-string-input-stream input)
                   :output stdout-path
                   :if-output-exists :supersede
                   :error stderr-path
                   :if-error-exists :supersede
                   :directory directory
                   (when environment (list :environment environment)))
            (and setsid leader-path))))

(defun read-leader-pid (path)
  "The integer group leader pid the spawned child recorded at PATH, or NIL when
PATH is absent or unwritten (the child died before recording it)."
  (ignore-errors
   (with-open-file (stream path :if-does-not-exist nil)
     (when stream
       (let ((line (read-line stream nil nil)))
         (when (and line (plusp (length line)))
           (parse-integer line :junk-allowed t)))))))

(defun terminate-bash-process (process leader-path)
  "SIGTERM then SIGKILL the command. When LEADER-PATH names a captured group
leader pid, signal the whole process group (negative pid) so grandchildren die
with it; otherwise signal the direct child alone (grandchildren orphan rather
than wedge). The final reap is bounded -- SIGKILL has been delivered, so a child
alive past the bound is pathological, and blocking on it would wedge the worker
for the command's natural lifetime."
  (let ((leader (and leader-path (read-leader-pid leader-path))))
    (flet ((signal-tree (number)
             (if leader
                 (sb-ext:run-program "kill"
                                     (list (format nil "-~D" number)
                                           (format nil "-~D" leader))
                                     :search t :wait t :error nil)
                 (ignore-errors (sb-ext:process-kill process number :pid)))))
      (when (sb-ext:process-alive-p process)
        (signal-tree 15)
        (sleep 0.1)
        (when (sb-ext:process-alive-p process)
          (signal-tree 9)
          (loop repeat 100
                while (sb-ext:process-alive-p process)
                do (sleep 0.05)))))))

(defun spill-bash-capture (context path truncated-p)
  "When TRUNCATED-P, adopt PATH's whole capture into the result store so the model
can page or search the full output through read-result / search-result, and return
(values handle-token total-bytes). A same-filesystem move in the common case (both
under the temp root), a copy otherwise. (values NIL NIL) when the stream was not
truncated or the spill is unavailable (disabled or failed)."
  (when truncated-p
    (let* ((protocol (active-protocol context))
           (entry (and protocol (adopt-file-spill protocol path))))
      (when entry
        (values (spill-entry-token entry) (spill-entry-bytes entry))))))

(defun bash-stream-spill-notice (subject truncated-p handle bytes)
  "Footer line for SUBJECT's stream: a spill handle marker when HANDLE names a
retained backing, a bare truncation notice when capped without one, or NIL when the
stream fit whole."
  (when truncated-p
    (if handle
        (format-spill-marker subject :shown *bash-output-character-limit*
                                     :total bytes :handle handle :unit "byte")
        (format-spill-marker subject :shown *bash-output-character-limit*))))

(defun bash-tool-result (command stdout stderr exit-code timeout-seconds
                         timed-out-p aborted-p
                         stdout-truncated-p stderr-truncated-p
                         &key cwd stdout-handle stdout-bytes
                              stderr-handle stderr-bytes)
  "Build the model-facing tool result. CWD, when supplied, is the post-command
working directory the next command will run in; it is appended to the result
text as a footer so a backend whose cwd persists across commands stays legible
in the transcript. The per-command local backend passes no CWD and emits none.
A truncated stream with a spill handle ends in a marker pointing the model at the
full, lossless output."
  (cond
    (aborted-p
     (tool-text-result
      "Command aborted before completion."
      :details (list :command command
                     :timeout-seconds timeout-seconds
                     :aborted-p t
                     :stdout stdout
                     :stderr stderr
                     :stdout-truncated-p stdout-truncated-p
                     :stderr-truncated-p stderr-truncated-p)
      :error-p t))
    (timed-out-p
     (tool-text-result
      (format nil "Command timed out after ~D second~:P." timeout-seconds)
      :details (list :command command
                     :timeout-seconds timeout-seconds
                     :timed-out-p t
                     :stdout stdout
                     :stderr stderr
                     :stdout-truncated-p stdout-truncated-p
                     :stderr-truncated-p stderr-truncated-p)
      :error-p t))
    (t
     (tool-text-result
      (format nil "~A~@[~%~A~]~@[~%~A~]~@[~%[cwd: ~A]~]"
              (bash-output-text stdout stderr exit-code)
              (bash-stream-spill-notice "stdout" stdout-truncated-p
                                        stdout-handle stdout-bytes)
              (bash-stream-spill-notice "stderr" stderr-truncated-p
                                        stderr-handle stderr-bytes)
              cwd)
      :details (list :command command
                     :exit-code exit-code
                     :timeout-seconds timeout-seconds
                     :cwd cwd
                     :stdout stdout
                     :stderr stderr
                     :stdout-truncated-p stdout-truncated-p
                     :stderr-truncated-p stderr-truncated-p
                     :stdout-handle stdout-handle
                     :stderr-handle stderr-handle)
      :error-p (not (zerop exit-code))))))

(defun local-bash-run (spec context)
  "Run SPEC's command in a local child process, capturing stdout/stderr through
temp files capped at *bash-output-character-limit* per stream. SPEC is a plist
(:command :shell :directory :input :timeout-seconds); returns a result plist
(:exit-code :stdout :stderr :timed-out-p :aborted-p :stdout-truncated-p
:stderr-truncated-p :stdout-handle :stdout-bytes :stderr-handle :stderr-bytes). The
default provider of the :bash-exec capability."
  (let ((command (getf spec :command))
        (shell (or (getf spec :shell) "sh"))
        (directory (getf spec :directory))
        (input (or (getf spec :input) ""))
        (timeout-seconds (getf spec :timeout-seconds))
        (environment (getf spec :environment))
        (on-update (getf spec :on-update)))
    (uiop:with-temporary-file (:pathname stdout-path :prefix "kli-bash-stdout")
      (uiop:with-temporary-file (:pathname stderr-path :prefix "kli-bash-stderr")
        (uiop:with-temporary-file (:pathname leader-path :prefix "kli-bash-leader")
          (let ((process nil)
                (leader-file nil))
            (unwind-protect
                 (progn
                   (multiple-value-setq (process leader-file)
                     (spawn-bash-process shell command input directory
                                         stdout-path stderr-path leader-path
                                         :environment environment))
                   (multiple-value-bind (exit-code timed-out-p aborted-p)
                       (wait-for-bash-process process timeout-seconds
                                              :on-update on-update
                                              :stream-path stdout-path)
                     (when (or timed-out-p aborted-p)
                       (terminate-bash-process process leader-file))
                     (multiple-value-bind (stdout stdout-truncated-p)
                         (read-windowed-output-file stdout-path
                                                    *bash-output-character-limit*)
                       (multiple-value-bind (stderr stderr-truncated-p)
                           (read-windowed-output-file stderr-path
                                                      *bash-output-character-limit*)
                         (multiple-value-bind (stdout-handle stdout-bytes)
                             (spill-bash-capture context stdout-path
                                                 stdout-truncated-p)
                           (multiple-value-bind (stderr-handle stderr-bytes)
                               (spill-bash-capture context stderr-path
                                                   stderr-truncated-p)
                             (list :exit-code exit-code
                                   :stdout stdout
                                   :stderr stderr
                                   :timed-out-p timed-out-p
                                   :aborted-p aborted-p
                                   :stdout-truncated-p stdout-truncated-p
                                   :stderr-truncated-p stderr-truncated-p
                                   :stdout-handle stdout-handle
                                   :stdout-bytes stdout-bytes
                                   :stderr-handle stderr-handle
                                   :stderr-bytes stderr-bytes)))))))
              (when process
                (terminate-bash-process process leader-file)))))))))

(defparameter +local-bash-exec-provider-id+ :local-bash-exec
  "Id of the built-in local bash-exec provider, selected by default.")

(defparameter +active-bash-exec-provider-id-key+
  :kli/tools/bash.active-bash-exec-provider-id
  "Per-protocol storage key holding the selected bash-exec provider id.")

(defun active-bash-exec-provider-id (protocol)
  "Id of the bash-exec provider the bash tool resolves on PROTOCOL, defaulting
to the local provider. Setf it to shadow local exec with another backend."
  (protocol-storage protocol +active-bash-exec-provider-id-key+
                    +local-bash-exec-provider-id+))

(defun (setf active-bash-exec-provider-id) (id protocol)
  (setf (protocol-storage protocol +active-bash-exec-provider-id-key+) id))

(defun seed-bash-exec-provider-id (protocol id)
  "Select backend ID on PROTOCOL only when no backend has been explicitly chosen
yet -- the storage key is unset. A settings seed thus never clobbers a live
/bash-shell pick, the only other setter. Returns T when it seeded, NIL when a
selection already stood."
  (when (eq :unset (protocol-storage protocol +active-bash-exec-provider-id-key+
                                     :unset))
    (setf (active-bash-exec-provider-id protocol) id)
    t))

(defun bash-exec-provider (protocol)
  (require-capability-provider protocol :bash-exec
                               :contract :bash-exec/v1
                               :provider-id (active-bash-exec-provider-id
                                             protocol)))

;;; Persistent-shell bash-exec provider: one long-lived shell per session, so
;;; cd, export, and other shell state persist across commands. Each command's
;;; stdout and stderr go to per-command temp files (the same windowed capture
;;; the local provider uses); the control channel -- the shell's own stdout --
;;; carries only a per-command completion marker, so a large blob never wedges
;;; it and neither pump starves the other.

(defparameter +persistent-shell-bash-exec-provider-id+ :persistent-shell-bash-exec
  "Id of the persistent-shell bash-exec provider.")

(defparameter +persistent-shell-storage-key+
  :kli/tools/bash.persistent-shell
  "Per-protocol storage key holding the session's live persistent shell.")

(defstruct (persistent-shell (:constructor %make-persistent-shell))
  process control input leader stderr-log (sequence 0))

(defun program-on-path (name)
  "Absolute path of NAME on PATH, or NIL when absent."
  (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                      :separator ":")
        for path = (and (plusp (length dir))
                        (probe-file
                         (merge-pathnames name
                                          (uiop:ensure-directory-pathname dir))))
        when path return (namestring path)))

(defvar *bash-program* :unprobed
  "Absolute path of bash(1), NIL when absent, :unprobed before the first spawn.
Probed once -- it cannot change mid-image.")

(defun bash-program ()
  (when (eq *bash-program* :unprobed)
    (setf *bash-program* (program-on-path "bash")))
  *bash-program*)

(defun persistent-shell-side-log ()
  "A temp file holding the shell's own stderr for its lifetime, kept off the
control channel so a shell-level diagnostic cannot masquerade as a marker.
Removed at teardown."
  (let (log)
    (uiop:with-temporary-file (:pathname path :keep t
                               :prefix "kli-persh-log" :type "log")
      (setf log path))
    log))

(defun spawn-persistent-shell (directory &key environment)
  "Spawn the long-lived shell -- bash without profile/rc when available, else sh
-- reading commands from its stdin. Under setsid(1) it leads its own process
group so teardown can reap the whole tree, and run-program still tracks its true
liveness. DIRECTORY seeds the initial working directory. ENVIRONMENT, when
non-NIL, restricts the shell -- and so every command it runs -- to that exact
environment, set once here; NIL inherits. Returns (values process control-stream
input-stream leader-pid stderr-log)."
  (let* ((bash (bash-program))
         (shell (or bash "sh"))
         (setsid (setsid-program))
         (shell-arguments (if bash
                              (list "--noprofile" "--norc" "-s")
                              (list "-s")))
         (stderr-log (persistent-shell-side-log))
         (process (apply #'sb-ext:run-program
                   (or setsid shell)
                   (if setsid
                       (list* "-w" shell shell-arguments)
                       shell-arguments)
                   :search (not setsid)
                   :wait nil
                   :input :stream
                   :output :stream
                   :error stderr-log
                   :if-error-exists :supersede
                   :directory directory
                   (when environment (list :environment environment))))
         (input (sb-ext:process-input process))
         (control (sb-ext:process-output process)))
    ;; The shell's own pid is the group leader under setsid; read it off the
    ;; control channel before any command frames its output to a file.
    (format input "printf '%d\\n' \"$$\"~%")
    (finish-output input)
    (let ((leader (let ((line (read-line control nil nil)))
                    (and line (parse-integer line :junk-allowed t)))))
      (values process control input leader stderr-log))))

(defun make-session-persistent-shell (directory &key environment)
  (multiple-value-bind (process control input leader stderr-log)
      (spawn-persistent-shell directory :environment environment)
    (%make-persistent-shell :process process :control control :input input
                            :leader leader :stderr-log stderr-log)))

(defun persistent-shell-teardown (shell)
  "Kill the shell's process group (its grandchildren with it when setsid formed
one), then the shell directly as a fallback, close the channels, reap, and
delete the side log. Idempotent."
  (let ((leader (persistent-shell-leader shell))
        (process (persistent-shell-process shell)))
    (when leader
      (ignore-errors
       (sb-ext:run-program "kill" (list "-9" (format nil "-~D" leader))
                           :search t :wait t :error nil)))
    (ignore-errors (sb-ext:process-kill process 9))
    (ignore-errors (close (persistent-shell-input shell)))
    (ignore-errors (close (persistent-shell-control shell)))
    (ignore-errors (sb-ext:process-wait process))
    (let ((log (persistent-shell-stderr-log shell)))
      (when log (ignore-errors (uiop:delete-file-if-exists log))))))

(defun await-persistent-shell-command (shell command mark
                                       out-path err-path in-path
                                       timeout-seconds on-update context)
  "Frame COMMAND to redirect its output to OUT-PATH/ERR-PATH (stdin from IN-PATH
when given) and emit MARK with the exit status and the post-command working
directory on completion, send it, then wait: streaming OUT-PATH to ON-UPDATE as
it grows, the marker as completion, a vanished process as death (SBCL listen
reports no EOF here, so death is detected directly), the deadline as timeout, and
an abort request as abort. Returns a result plist with an extra :dead-p and the
authoritative post-command :cwd (the directory the next command will run in)."
  (let ((input (persistent-shell-input shell))
        (control (persistent-shell-control shell))
        (process (persistent-shell-process shell))
        (offset 0)
        (decoder (and on-update (make-incremental-decoder)))
        (deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second)))
        (exit-code nil) (cwd nil)
        (timed-out-p nil) (aborted-p nil) (dead-p nil))
    (flet ((pump ()
             (when decoder
               (setf offset (stream-new-output out-path offset decoder on-update)))))
      (handler-case
          (progn
            ;; Emit the working directory beside the marker, read off the live
            ;; shell ($PWD) so it is authoritative across cd/pushd/popd and a
            ;; script that cd's internally -- state the model cannot recompute.
            ;; It rides the control channel only; the output files never see it.
            (format input
                    "{ ~A ; } ~@[< '~A' ~]> '~A' 2> '~A'~%printf '~A %d %s\\n' \"$?\" \"$PWD\"~%"
                    command (and in-path (namestring in-path))
                    (namestring out-path) (namestring err-path) mark)
            (finish-output input))
        (stream-error () (setf dead-p t)))
      (unless dead-p
        (loop
          (pump)
          (cond
            ((listen control)
             (let ((line (read-line control nil :eof)))
               (if (eq line :eof)
                   (progn (setf dead-p t) (return))
                   ;; token0 = mark, token1 = exit, rest-of-line = cwd. The cwd
                   ;; is the remainder so an absolute path with spaces survives;
                   ;; a literal newline in a path is pathological -- it would
                   ;; truncate here, which we accept rather than fight.
                   (let* ((mark-end (position #\Space line))
                          (exit-end (and mark-end
                                         (position #\Space line
                                                   :start (1+ mark-end)))))
                     (when (and mark-end (string= (subseq line 0 mark-end) mark))
                       (setf exit-code (parse-integer line :start (1+ mark-end)
                                                           :end exit-end
                                                           :junk-allowed t))
                       (when exit-end (setf cwd (subseq line (1+ exit-end))))
                       (return))))))
            ((not (sb-ext:process-alive-p process)) (setf dead-p t) (return))
            ((tool-abort-requested-p) (setf aborted-p t) (return))
            ((>= (get-internal-real-time) deadline) (setf timed-out-p t) (return))
            (t (sleep 0.02)))))
      (pump)
      (multiple-value-bind (stdout stdout-truncated-p)
          (read-windowed-output-file out-path *bash-output-character-limit*)
        (multiple-value-bind (stderr stderr-truncated-p)
            (read-windowed-output-file err-path *bash-output-character-limit*)
          (multiple-value-bind (stdout-handle stdout-bytes)
              (spill-bash-capture context out-path stdout-truncated-p)
            (multiple-value-bind (stderr-handle stderr-bytes)
                (spill-bash-capture context err-path stderr-truncated-p)
              (list :exit-code (if dead-p -1 (or exit-code -1))
                    :stdout stdout
                    :stderr stderr
                    :timed-out-p timed-out-p
                    :aborted-p aborted-p
                    :dead-p dead-p
                    :cwd cwd
                    :stdout-truncated-p stdout-truncated-p
                    :stderr-truncated-p stderr-truncated-p
                    :stdout-handle stdout-handle
                    :stdout-bytes stdout-bytes
                    :stderr-handle stderr-handle
                    :stderr-bytes stderr-bytes))))))))

(defun persistent-shell-run-command (shell command timeout-seconds
                                     &key on-update input context)
  "Run COMMAND in SHELL through per-command temp files, redirecting stdin from
INPUT when supplied. Returns the result plist of await-persistent-shell-command."
  (let ((mark (format nil "kli-persh-mark-~D"
                      (incf (persistent-shell-sequence shell)))))
    (uiop:with-temporary-file (:pathname out-path :prefix "kli-persh-stdout")
      (uiop:with-temporary-file (:pathname err-path :prefix "kli-persh-stderr")
        (flet ((await (in-path)
                 (await-persistent-shell-command shell command mark
                                                 out-path err-path in-path
                                                 timeout-seconds on-update
                                                 context)))
          (if (and input (plusp (length input)))
              (uiop:with-temporary-file (:pathname in-path :prefix "kli-persh-stdin")
                (with-open-file (stream in-path :direction :output
                                                :if-exists :supersede
                                                :external-format :utf-8)
                  (write-string input stream))
                (await in-path))
              (await nil)))))))

(defun session-persistent-shell (protocol)
  (protocol-storage protocol +persistent-shell-storage-key+))

(defun ensure-session-persistent-shell (protocol directory &key environment)
  "The session's persistent shell, spawned on first use and respawned when a
prior command left it dead. DIRECTORY seeds the initial working directory; the
command stream governs cwd thereafter. ENVIRONMENT restricts the shell at spawn,
so a respawn picks up the current policy."
  (let ((existing (session-persistent-shell protocol)))
    (if (and existing
             (sb-ext:process-alive-p (persistent-shell-process existing)))
        existing
        (progn
          (when existing (persistent-shell-teardown existing))
          (setf (protocol-storage protocol +persistent-shell-storage-key+)
                (make-session-persistent-shell directory
                                               :environment environment))))))

(defun teardown-session-persistent-shell (protocol)
  "Reap the session's persistent shell if live and clear the slot. Idempotent."
  (let ((shell (session-persistent-shell protocol)))
    (when shell (persistent-shell-teardown shell))
    (setf (protocol-storage protocol +persistent-shell-storage-key+) nil)))

(defun persistent-shell-death-note (stderr)
  "STDERR with a note that the shell exited before signalling completion, so a
command that ran exit or exec and took the shell down reports a failure rather
than a silent empty success."
  (let ((note "Persistent shell exited before the command completed (a bare exit or exec ends it); it was respawned for the next command."))
    (if (plusp (length stderr))
        (format nil "~A~%~A" stderr note)
        note)))

(defun persistent-shell-bash-run (spec context)
  "The :run entry of the persistent-shell bash-exec provider. Runs SPEC's
command in the session's long-lived shell -- spawned lazily, respawned when a
command leaves it dead -- and returns the same result plist as the local
provider. A command that times out, is aborted, or ends the shell tears it down
so the next command starts clean rather than inherit half-run state."
  (let* ((protocol (active-protocol context))
         (shell (ensure-session-persistent-shell
                 protocol (getf spec :directory)
                 :environment (getf spec :environment)))
         (result (persistent-shell-run-command
                  shell (getf spec :command) (getf spec :timeout-seconds)
                  :on-update (getf spec :on-update)
                  :input (getf spec :input)
                  :context context))
         (dead-p (getf result :dead-p)))
    (when (or dead-p (getf result :timed-out-p) (getf result :aborted-p))
      (teardown-session-persistent-shell protocol))
    (list :exit-code (getf result :exit-code)
          :stdout (getf result :stdout)
          :stderr (if dead-p
                      (persistent-shell-death-note (getf result :stderr))
                      (getf result :stderr))
          :timed-out-p (getf result :timed-out-p)
          :aborted-p (getf result :aborted-p)
          :cwd (getf result :cwd)
          :stdout-truncated-p (getf result :stdout-truncated-p)
          :stderr-truncated-p (getf result :stderr-truncated-p)
          :stdout-handle (getf result :stdout-handle)
          :stdout-bytes (getf result :stdout-bytes)
          :stderr-handle (getf result :stderr-handle)
          :stderr-bytes (getf result :stderr-bytes))))

(defun install-persistent-shell-teardown (protocol contribution context)
  "Installer of the teardown effect: nothing eager. The shell is spawned lazily
by the first command, so a session that selects but never uses the provider
spawns no process."
  (declare (ignore protocol contribution context))
  nil)

(defun retract-persistent-shell-teardown (protocol contribution context)
  "Retractor: reap the session's persistent shell so a protocol teardown or
rollback leaves no live shell -- and under setsid no orphaned grandchild --
behind."
  (declare (ignore contribution context))
  (teardown-session-persistent-shell protocol))

;;; Background jobs. A run_in_background command launches as its own setsid group
;;; (not `cmd &` in the persistent shell, whose group and sequential marker
;;; protocol can't isolate a job) and returns a job id at once. Output accrues to
;;; per-job temp files that outlive the launching call; sibling tools read, list,
;;; and stop jobs; a drain effect SIGKILLs every live group on rollback.

(defparameter +bash-jobs-storage-key+ :kli/tools/bash.bash-jobs
  "Per-protocol storage key for the session's background-job table.")

(defstruct (bash-jobs-table (:constructor %make-bash-jobs-table))
  (counter 0)
  (jobs '()))

(defstruct (bash-job (:constructor %make-bash-job))
  id process leader-path leaderf outf errf (offset 0) decoder exit done command
  spill-handle)

(defun session-bash-jobs (protocol)
  (protocol-storage protocol +bash-jobs-storage-key+))

(defun ensure-session-bash-jobs (protocol)
  (or (session-bash-jobs protocol)
      (setf (protocol-storage protocol +bash-jobs-storage-key+)
            (%make-bash-jobs-table))))

(defun background-requested-p (value)
  "Whether VALUE asks for backgrounding, tolerating a string the wire may send."
  (and value (not (member value '(nil :false :no "false" "no" "0") :test #'equalp))))

(defun mint-bash-job-file (kind)
  "A temp file the jobs table owns: a job's output must survive across polls, so
it is deleted only on kill, drain, or teardown -- never by with-temporary-file."
  (let (path)
    (uiop:with-temporary-file (:pathname p :keep t
                               :prefix (format nil "kli-bgjob-~A" kind))
      (setf path p))
    path))

(defun await-leader-pid (process path &key (tries 400) (delay 0.005))
  "Wait briefly for the job's prologue to record its group leader pid at PATH, so
a kill or drain right after launch signals the group rather than orphaning it.
NIL when none is recorded (no setsid, or the child died first)."
  (loop repeat tries
        for pid = (read-leader-pid path)
        when pid return pid
        when (not (sb-ext:process-alive-p process))
          return (read-leader-pid path)
        do (sleep delay)))

(defun spawn-background-job (table command shell directory input &key environment)
  "Spawn COMMAND detached and register it in TABLE under a fresh id. Reuses
spawn-bash-process but never waits -- the handle returns at once. Reads the leader
pid before returning so teardown can killpg even if rollback fires immediately.
ENVIRONMENT, when non-NIL, restricts the job's environment like the foreground."
  (let* ((id (format nil "bash-~D" (incf (bash-jobs-table-counter table))))
         (outf (mint-bash-job-file "out"))
         (errf (mint-bash-job-file "err"))
         (leaderf (mint-bash-job-file "leader")))
    (multiple-value-bind (process leader-path)
        (spawn-bash-process shell command input directory outf errf leaderf
                            :environment environment)
      (when leader-path
        (await-leader-pid process leader-path))
      (%make-bash-job :id id :process process
                      :leader-path leader-path :leaderf leaderf
                      :outf outf :errf errf
                      :decoder (make-incremental-decoder)
                      :command command))))

(defun find-bash-job (protocol id)
  (let ((table (session-bash-jobs protocol)))
    (and table (find id (bash-jobs-table-jobs table)
                     :key #'bash-job-id :test #'string=))))

(defun bash-job-output-bytes (job)
  (with-open-file (stream (bash-job-outf job) :element-type '(unsigned-byte 8)
                                              :if-does-not-exist nil)
    (if stream (file-length stream) 0)))

(defun refresh-bash-job-status (job)
  "Reap and record JOB's exit when its process has exited, without touching the
output file -- so a status read (bash-jobs) never consumes bytes a later
bash-output poll must see. Reaping a never-blocked child is safe: once not alive,
process-wait is non-blocking and the exit code is valid. Idempotent."
  (when (and (not (bash-job-done job))
             (not (sb-ext:process-alive-p (bash-job-process job))))
    (sb-ext:process-wait (bash-job-process job))
    (setf (bash-job-exit job) (or (sb-ext:process-exit-code (bash-job-process job)) -1)
          (bash-job-done job) t)))

(defun poll-bash-job (job)
  "Decode output JOB appended since the last poll; on completion, reap and take a
final tail so bytes between the last tail and exit are not lost. Returns (values
new-text done-p exit-code truncated-p). Incremental: concatenated polls reproduce
the capture up to truncation notices -- a backlog over *bash-output-character-limit*
is windowed tail-weighted and advanced to live, bounding the heap and favoring
freshness over completeness."
  (let ((text (make-string-output-stream))
        (truncated nil))
    (flet ((drain ()
             (let* ((path (bash-job-outf job))
                    (offset (bash-job-offset job)))
               (if (<= (- (output-file-byte-length path) offset)
                       *bash-output-character-limit*)
                   (setf (bash-job-offset job)
                         (stream-new-output path offset (bash-job-decoder job)
                                            (lambda (s) (write-string s text))))
                   (multiple-value-bind (window new-offset cut)
                       (read-job-output-window path offset
                                               *bash-output-character-limit*)
                     (write-string window text)
                     (setf (bash-job-offset job) new-offset
                           (bash-job-decoder job) (make-incremental-decoder)
                           truncated (or truncated cut)))))))
      (drain)
      (let ((was-done (bash-job-done job)))
        (refresh-bash-job-status job)
        (when (and (bash-job-done job) (not was-done))
          (drain))))
    (values (get-output-stream-string text)
            (bash-job-done job)
            (bash-job-exit job)
            truncated)))

(defun kill-bash-job (job)
  "SIGTERM then SIGKILL JOB's process group, reap, and record the exit. Leaves the
output files for a final bash-output read; drain or teardown deletes them.
Idempotent."
  (terminate-bash-process (bash-job-process job) (bash-job-leader-path job))
  (ignore-errors (sb-ext:process-wait (bash-job-process job)))
  (setf (bash-job-exit job) (or (sb-ext:process-exit-code (bash-job-process job)) -1)
        (bash-job-done job) t))

(defun delete-bash-job-files (job)
  (ignore-errors (uiop:delete-file-if-exists (bash-job-outf job)))
  (ignore-errors (uiop:delete-file-if-exists (bash-job-errf job)))
  (when (bash-job-leaderf job)
    (ignore-errors (uiop:delete-file-if-exists (bash-job-leaderf job)))))

(defun teardown-session-bash-jobs (protocol)
  "Kill every job's group, delete its files, clear the table, so a teardown or
rollback leaves no detached process behind. Idempotent."
  (let ((table (session-bash-jobs protocol)))
    (when table
      (dolist (job (bash-jobs-table-jobs table))
        (kill-bash-job job)
        (delete-bash-job-files job))
      (setf (protocol-storage protocol +bash-jobs-storage-key+) nil))))

(defun bash-job-launch-result (job)
  (tool-text-result
   (format nil "Started background job ~A. Read its output with bash-output and ~
stop it with bash-kill. It ignores the command timeout and runs until it ~
finishes or is stopped."
           (bash-job-id job))
   :details (list :job-id (bash-job-id job)
                  :command (bash-job-command job)
                  :background-p t)))

(defun launch-bash-job (protocol parameters command)
  "Launch COMMAND as a background job and return its job-id result at once,
reading :shell/:directory/:input as the foreground path does and honoring the
session policy's cwd and env-allowlist. :default-timeout does not apply -- a
detached job has no wait loop; bash-kill and the drain are its controls."
  (let* ((policy (bash-policy protocol))
         (table (ensure-session-bash-jobs protocol))
         (shell (or (tool-parameter parameters :shell) "sh"))
         (directory (effective-directory policy
                                         (tool-parameter parameters :directory)))
         (input (or (tool-parameter parameters :input) ""))
         (environment (restricted-environment (policy-env-allowlist policy)))
         (job (spawn-background-job table command shell directory input
                                    :environment environment)))
    (push job (bash-jobs-table-jobs table))
    (bash-job-launch-result job)))

(defun ensure-bash-job-spill-handle (protocol job)
  "Register a handle over JOB's live output file on first need, caching it on the
job so repeated polls surface one stable handle. The file is the job's, reaped by
the job machinery; the handle just pages and searches it. Returns the token or NIL."
  (or (bash-job-spill-handle job)
      (let ((entry (register-file-handle protocol (bash-job-outf job)
                                         :producer-uuid (bash-job-id job))))
        (when entry
          (setf (bash-job-spill-handle job) (spill-entry-token entry))))))

(defun run-bash-output-tool (tool parameters context &key call-id on-update)
  "Read a background job's output since the last read, plus its run/exit status.
When a poll windows the backlog, surface a handle over the live output file so the
model can page or search the part the window dropped."
  (declare (ignore tool call-id on-update))
  (let* ((protocol (active-protocol context))
         (job-id (required-tool-parameter parameters :job_id))
         (job (find-bash-job protocol job-id)))
    (if (null job)
        (tool-text-result (format nil "No background job ~A." job-id)
                          :details (list :job-id job-id)
                          :error-p t)
        (multiple-value-bind (text done-p exit-code truncated) (poll-bash-job job)
          (let* ((status (if done-p
                             (format nil "[job ~A exited with status ~D]"
                                     job-id exit-code)
                             (format nil "[job ~A still running]" job-id)))
                 (handle (and truncated
                              (ensure-bash-job-spill-handle protocol job)))
                 (marker (and handle
                              (format-spill-marker
                               "job output"
                               :shown *bash-output-character-limit*
                               :total (bash-job-output-bytes job)
                               :handle handle :unit "byte"))))
            (tool-text-result
             (format nil "~@[~A~%~]~A~@[~%~A~]"
                     (and (plusp (length text)) text) status marker)
             :details (list :job-id job-id
                            :done-p done-p
                            :exit-code (and done-p exit-code)
                            :output text
                            :truncated truncated
                            :result-handle handle)
             :error-p (and done-p (not (eql exit-code 0)))))))))

(defun run-bash-kill-tool (tool parameters context &key call-id on-update)
  "Stop a running background job by its id, terminating its process group."
  (declare (ignore tool call-id on-update))
  (let* ((protocol (active-protocol context))
         (job-id (required-tool-parameter parameters :job_id))
         (job (find-bash-job protocol job-id)))
    (if (null job)
        (tool-text-result (format nil "No background job ~A." job-id)
                          :details (list :job-id job-id)
                          :error-p t)
        (progn
          (kill-bash-job job)
          (tool-text-result
           (format nil "Stopped background job ~A." job-id)
           :details (list :job-id job-id :killed-p t))))))

(defun bash-job-listing-line (job)
  (if (bash-job-done job)
      (format nil "~A  done (exit ~D, ~:D byte~:P captured)"
              (bash-job-id job) (or (bash-job-exit job) -1)
              (bash-job-output-bytes job))
      (format nil "~A  running (~:D byte~:P captured)"
              (bash-job-id job) (bash-job-output-bytes job))))

(defun bash-job-plist (job)
  (list :id (bash-job-id job)
        :done-p (bash-job-done job)
        :exit-code (and (bash-job-done job) (bash-job-exit job))
        :bytes (bash-job-output-bytes job)
        :command (bash-job-command job)))

(defun run-bash-jobs-tool (tool parameters context &key call-id on-update)
  "List the session's background jobs: id, run/exit status, bytes captured. Reaps
finished jobs without consuming output, so a later bash-output read still returns it."
  (declare (ignore tool parameters call-id on-update))
  (let* ((protocol (active-protocol context))
         (table (session-bash-jobs protocol))
         (jobs (and table (reverse (bash-jobs-table-jobs table)))))
    (dolist (job jobs) (refresh-bash-job-status job))
    (tool-text-result
     (if (null jobs)
         "No background jobs."
         (format nil "~{~A~^~%~}" (mapcar #'bash-job-listing-line jobs)))
     :details (list :jobs (mapcar #'bash-job-plist jobs)))))

(defun install-bash-jobs-drain (protocol contribution context)
  "Installer: nothing eager. Jobs spawn lazily on the first background launch."
  (declare (ignore protocol contribution context))
  nil)

(defun retract-bash-jobs-drain (protocol contribution context)
  "Retractor: reap every background job so a rollback leaves no process behind."
  (declare (ignore contribution context))
  (teardown-session-bash-jobs protocol))

(defun run-bash-tool (tool parameters context &key call-id on-update)
  "Guard interactive commands, then run COMMAND through the selected bash-exec
provider under the session's bash policy, and format the result. Exec and output
capping live in the provider. A run_in_background request launches a detached job
and returns its id at once."
  (declare (ignore tool call-id))
  (let* ((protocol (active-protocol context))
         (policy (bash-policy protocol))
         (command (required-tool-parameter parameters :command))
         (interactive-name (interactive-shell-command-name command)))
    (when interactive-name
      (return-from run-bash-tool
        (tool-text-result
         (format nil "Command ~S looks interactive and is not supported by the Bash tool."
                 interactive-name)
         :details (list :command command
                        :interactive-command interactive-name)
         :error-p t)))
    (when (background-requested-p (tool-parameter parameters :run_in_background))
      (return-from run-bash-tool
        (launch-bash-job protocol parameters command)))
    (let* ((timeout-seconds (bash-timeout-seconds parameters policy))
           (spec (list :command command
                       :shell (or (tool-parameter parameters :shell) "sh")
                       :directory (effective-directory
                                   policy (tool-parameter parameters :directory))
                       :input (or (tool-parameter parameters :input) "")
                       :timeout-seconds timeout-seconds
                       :environment (restricted-environment
                                     (policy-env-allowlist policy))
                       :on-update on-update))
           (result (provider-call (bash-exec-provider protocol)
                                  :run spec context)))
      (bash-tool-result command
                        (getf result :stdout)
                        (getf result :stderr)
                        (getf result :exit-code)
                        timeout-seconds
                        (getf result :timed-out-p)
                        (getf result :aborted-p)
                        (getf result :stdout-truncated-p)
                        (getf result :stderr-truncated-p)
                        :cwd (getf result :cwd)
                        :stdout-handle (getf result :stdout-handle)
                        :stdout-bytes (getf result :stdout-bytes)
                        :stderr-handle (getf result :stderr-handle)
                        :stderr-bytes (getf result :stderr-bytes)))))
