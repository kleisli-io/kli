(in-package #:kli/tools/eval)

(defmacro! with-source-forms ((form source &key current) &body body)
  "Read SOURCE's top-level forms one at a time under the live *package* and
*readtable*, binding FORM to each and running BODY before the next read, so a
form that changes how later forms read -- in-package, a reader macro, #. --
takes effect. CURRENT, when given, is a place blanked before each read (a read
error reports no form) and set to each form before BODY (an eval error reports
that form)."
  `(let ((,g!eof (list :eof)))
     (with-input-from-string (,g!stream ,source)
       (loop
         ,@(when current `((setf ,current nil)))
         (let ((,form (read ,g!stream nil ,g!eof)))
           (when (eq ,form ,g!eof) (return))
           ,@(when current `((setf ,current ,form)))
           ,@body)))))

(defparameter +eval-session-id+ :eval-session
  "Registry id of the per-session current-package cell.")

(defclass eval-session (live-object)
  ((current-package
    :initform "CL-USER"
    :accessor eval-session-current-package))
  (:documentation "Per-session current package for the eval tool. Sticky across
calls so an in-package in one call governs the next, like a REPL; default
CL-USER."))

(defun eval-session-cell (context)
  "The session's current-package cell, or nil when the eval session effect is
not installed."
  (find-live-object (context-registry context) +eval-session-id+))

(defun resolve-eval-package (parameters context)
  "Resolve a call's starting package -- explicit :package, else the sticky
session package, else CL-USER -- as (values package name). Signals on an
unknown package."
  (let* ((name (or (tool-parameter parameters :package)
                   (let ((cell (eval-session-cell context)))
                     (and cell (eval-session-current-package cell)))
                   "CL-USER"))
         (package (or (find-package name)
                      (error "Unknown package: ~A" name))))
    (values package name)))

(defun persist-eval-package (context package-name)
  "Record PACKAGE-NAME as the session's sticky package, when a session cell is
installed and PACKAGE-NAME is non-nil (nil means the worker was killed before
it could report)."
  (let ((cell (eval-session-cell context)))
    (when (and cell package-name)
      (setf (eval-session-current-package cell) package-name))))

(defparameter +eval-package-block-tag+ "eval_current_package"
  "Delimiter tag wrapping the current-package block in the system prompt.")

(defun eval-package-block-open ()
  (format nil "~%~%<~A>" +eval-package-block-tag+))

(defun eval-package-block-close ()
  (format nil "</~A>" +eval-package-block-tag+))

(defun eval-package-block (package-name)
  "The delimited current-package block -- the package name only, so it busts the
cached system prefix only when the package actually changes."
  (format nil "~A~%Current eval package: ~A~%~A"
          (eval-package-block-open) package-name (eval-package-block-close)))

(defun install-eval-session (protocol contribution context)
  "Register the session's current-package cell and, when an agent session is
present, install a system-prompt layer that surfaces the current package on
every submission. Returns the layer handle as contribution state."
  (declare (ignore protocol))
  (let ((cell (register-live-object
               (context-registry context)
               (make-instance 'eval-session :id +eval-session-id+)))
        (service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (when service
      (let ((label (contribution-name contribution)))
        (add-system-prompt-layer
         service label
         (lambda () (eval-package-block (eval-session-current-package cell)))
         :kind :append)
        (list :service service :label label)))))

(defun uninstall-eval-session (protocol contribution context)
  "Remove the current-package layer (if one was installed) and drop the session
cell."
  (declare (ignore protocol))
  (let ((handle (contribution-state contribution)))
    (when handle
      (remove-system-prompt-layer (getf handle :service) (getf handle :label))))
  (remove-live-object (context-registry context) +eval-session-id+))

(defparameter *eval-print-length* 100
  "Bound on *print-length* when printing eval tool values.")

(defparameter *eval-print-level* 20
  "Bound on *print-level* when printing eval tool values.")

(defparameter *eval-output-character-limit* 30000
  "Characters of eval output kept inline in the result. Output past this streams
to a lossless backing the model retrieves through read-result / search-result, so
the inline window is a context budget, not a heap guard -- the tee forwards a
bounded prefix to the image and the rest straight to disk, so a printing loop
never lands the whole span in the heap.")

(defparameter *eval-value-character-limit* 30000
  "Characters of a single printed value kept inline. A value whose printed form
overflows is retained whole behind a handle; the inline form ends in a [+N chars]
marker. The tee bounds the heap, so a giant atom *print-length*/*print-level*
cannot bound -- a long string, a huge bignum -- is reachable without slurping.")

(defparameter *default-eval-timeout-seconds* 30)
(defparameter *maximum-eval-timeout-seconds* 300)

(defparameter *eval-backtrace-frame-limit* 32
  "Bound on the number of backtrace frames captured into an eval error's
:details, so a deep stack cannot bloat the result.")

(defmacro with-bounded-printer (&body body)
  "Run BODY under the eval tool's length/level/circle printer bounds, so a
huge structure cannot exhaust the heap and a circular one cannot print
forever."
  `(let ((*print-circle* t)
         (*print-length* *eval-print-length*)
         (*print-level* *eval-print-level*))
     ,@body))

(defun eval-timeout-seconds (parameters)
  (let* ((value (or (tool-parameter parameters :timeout)
                    *default-eval-timeout-seconds*))
         (seconds (etypecase value
                    (integer value)
                    (string (parse-integer value)))))
    (unless (plusp seconds)
      (error "Eval timeout must be positive."))
    (min seconds *maximum-eval-timeout-seconds*)))

(defclass bounded-output-stream (sb-gray:fundamental-character-output-stream)
  ((target
    :initarg :target
    :reader bounded-output-stream-target)
   (limit
    :initarg :limit
    :reader bounded-output-stream-limit)
   (written
    :initform 0
    :accessor bounded-output-stream-written))
  (:documentation
   "Character output stream that forwards to TARGET until LIMIT characters
have been written and counts but drops the rest. The cap applies inside
the stream because a single oversized write-string would land in the heap
before any after-the-fact check could see it."))

(defun bounded-output-stream-truncated-p (stream)
  (> (bounded-output-stream-written stream)
     (bounded-output-stream-limit stream)))

(defmethod sb-gray:stream-write-char ((stream bounded-output-stream) char)
  (let ((written (bounded-output-stream-written stream)))
    (when (< written (bounded-output-stream-limit stream))
      (write-char char (bounded-output-stream-target stream)))
    (setf (bounded-output-stream-written stream) (1+ written)))
  char)

(defmethod sb-gray:stream-write-string ((stream bounded-output-stream)
                                        string &optional (start 0) end)
  (let* ((end (or end (length string)))
         (written (bounded-output-stream-written stream))
         (remaining (- (bounded-output-stream-limit stream) written))
         (take (min (max remaining 0) (- end start))))
    (when (plusp take)
      (write-string string (bounded-output-stream-target stream)
                    :start start :end (+ start take)))
    (setf (bounded-output-stream-written stream)
          (+ written (- end start))))
  string)

(defun printable-value-handle (protocol value)
  "Retain VALUE's whole printed form behind a handle by re-printing it through a
spill tee -- finalized under unwind-protect so a print-object that throws mid-stream
still lands the partial backing. Called only for a value already known to overflow,
so the backing is always registered. Returns (values token bytes), or NIL when no
protocol, spilling is off, or finalize failed."
  (when protocol
    (let ((tee (open-spill-tee protocol
                               :window-limit *eval-value-character-limit*)))
      (when tee
        (let ((entry nil))
          (unwind-protect (with-bounded-printer (prin1 value tee))
            (setf entry (finalize-spill-tee tee)))
          (when entry
            (values (spill-entry-token entry) (spill-entry-bytes entry))))))))

(defun printable-values (values &optional protocol)
  "VALUES printed readably under bounded printer control, each capped inline at
*eval-value-character-limit* with a [+N chars] marker. A value whose printed form
overflows is also retained whole behind a handle when PROTOCOL allows. Returns
(values strings truncated-p handles): STRINGS the inline forms; TRUNCATED-P true
when any overflowed; HANDLES a list of (index token . bytes) for retained values."
  (let ((truncated nil)
        (handles '())
        (limit *eval-value-character-limit*))
    (values
     (with-bounded-printer
       (loop for value in values
             for index from 0
             collect
             (let* ((target (make-string-output-stream))
                    (bounded (make-instance 'bounded-output-stream
                                            :target target :limit limit)))
               (prin1 value bounded)
               (let ((written (bounded-output-stream-written bounded))
                     (text (get-output-stream-string target)))
                 (if (> written limit)
                     (progn
                       (setf truncated t)
                       (multiple-value-bind (token bytes)
                           (printable-value-handle protocol value)
                         (when token (push (list* index token bytes) handles)))
                       (format nil "~A[+~D chars]" text (- written limit)))
                     text)))))
     truncated
     (nreverse handles))))

(defun printed-value-spill-notices (handles)
  "Footer marker lines for values retained behind handles -- one per entry of
HANDLES ((index token . bytes))."
  (mapcar (lambda (handle)
            (destructuring-bind (index token . bytes) handle
              (format-spill-marker (format nil "value ~D" index)
                                   :shown *eval-value-character-limit*
                                   :total bytes :handle token :unit "byte")))
          handles))

(defun eval-tool-output-text (stdout value-lines)
  (cond
    ((and (plusp (length stdout)) value-lines)
     (format nil "~A~%~{~A~^~%~}" stdout value-lines))
    ((plusp (length stdout))
     stdout)
    (value-lines
     (format nil "~{~A~^~%~}" value-lines))
    (t "")))

(defun eval-condition-restarts (condition)
  "Active restarts for CONDITION as report strings, named restart first."
  (mapcar (lambda (restart)
            (let ((name (restart-name restart)))
              (if name
                  (format nil "~A: ~A" name restart)
                  (princ-to-string restart))))
          (compute-restarts condition)))

(defun eval-condition-restart-names (condition)
  "Names of CONDITION's active restarts as strings, anonymous restarts omitted --
the exact tokens a caller passes back to resume a parked eval."
  (loop for restart in (compute-restarts condition)
        for name = (restart-name restart)
        when name collect (string name)))

(defun capture-eval-condition (condition form)
  "Snapshot CONDITION into a durable plist while its signaling stack is still
live -- keyword/string/integer leaves only, the condition type downgraded to
a string. Called from a handler-bind handler, so the backtrace and active
restarts reflect the error site rather than the unwound runner; FORM is the
top-level form that was evaluating, surfaced as :source."
  (with-bounded-printer
    (list* :condition-type (princ-to-string (type-of condition))
           :message (princ-to-string condition)
           :category (kli/ext:condition-category condition)
           :backtrace (mapcar #'princ-to-string
                              (sb-debug:list-backtrace
                               :count *eval-backtrace-frame-limit*))
           :restarts (eval-condition-restarts condition)
           (when form
             (list :source (prin1-to-string form))))))

(defun capture-eval-control-stack-exhaustion (condition form)
  "Describe a control-stack exhaustion after its signaling stack has unwound.
Backtrace and restart inspection are deliberately omitted because either can
consume stack while the guard page is still unprotected."
  (with-bounded-printer
    (list* :condition-type (princ-to-string (type-of condition))
           :message (princ-to-string condition)
           :category :internal
           :backtrace '()
           :restarts '()
           :resource-exhausted-p t
           (when form
             (list :source (prin1-to-string form))))))

(defmacro! with-eval-control-stack-capture ((source-form) &body body)
  "Run BODY; after a control-stack exhaustion unwinds, return (:error plist)."
  `(handler-case
       (progn ,@body)
     (sb-kernel::control-stack-exhausted (,g!condition)
       (list :error
             (capture-eval-control-stack-exhaustion ,g!condition
                                                    ,source-form)))))

(defmacro! with-structured-capture ((source-form) &body body)
  "Run BODY; on an error, unwind and return (:error plist) snapshotted at signal
time, the plist's :source taken from SOURCE-FORM's current value. BODY returns
the success value when nothing signals."
  `(with-eval-control-stack-capture (,source-form)
     (block ,g!capture
       (handler-bind
           ((error (lambda (condition)
                     (return-from ,g!capture
                       (list :error
                             (capture-eval-condition condition ,source-form))))))
         ,@body))))

(defparameter +nested-directive-names+
  '("IN-PACKAGE" "DEFPACKAGE" "IN-READTABLE"
    "SET-MACRO-CHARACTER" "SET-DISPATCH-MACRO-CHARACTER"
    "MAKE-DISPATCH-MACRO-CHARACTER")
  "Symbol-names (package-insensitive) of package/readtable directives that
silently misbehave when nested inside a larger top-level form, since the whole
form is read before any of it runs.")

(defparameter +binding-operators+
  (list 'let 'let* 'flet 'labels 'macrolet 'symbol-macrolet)
  "The un-shadowable CL binding special operators. EQ-matched so only a genuine
binder gets head-skipping -- a same-named macro in another package does not.")

(defun nested-directives (form)
  "Distinct names of package/readtable directives nested at least one level deep
inside FORM. Quote/function/quasiquote subtrees are pruned, and a binding-list
head of a CL binding operator is not treated as a call (by grammar it is a bound
name, never evaluated). FORM itself (depth 0) is never reported."
  (let ((found '()))
    (labels ((walk-bindings (bindings depth)
               (when (listp bindings)
                 (dolist (entry bindings)
                   (when (consp entry)
                     (dolist (sub (cdr entry)) (walk sub depth))))))
             (walk (node depth)
               (when (consp node)
                 (let* ((head (car node))
                        (name (and (symbolp head) (symbol-name head))))
                   (cond
                     ((and name (member name '("QUOTE" "FUNCTION" "QUASIQUOTE")
                                        :test #'string=)))
                     ((and (symbolp head)
                           (member head +binding-operators+ :test #'eq))
                      (walk-bindings (second node) (1+ depth))
                      (dolist (b (cddr node)) (walk b (1+ depth))))
                     (t
                      (when (and (>= depth 1) name
                                 (member name +nested-directive-names+
                                         :test #'string=))
                        (pushnew name found :test #'string=))
                      (dolist (child node) (walk child (1+ depth)))))))))
      (walk form 0))
    (nreverse found)))

(defun directive-warning (form)
  "A one-line caution when FORM nests a package/readtable directive, else nil."
  (let ((names (nested-directives form)))
    (when names
      (format nil "Caution: ~{~A~^, ~} appears nested inside a larger form. If ~
these are package/readtable directives (not a local binding of the same name), ~
the whole form is read before any of it runs, so names after the directive were ~
read in the previous package/readtable, not the new one. Put a real directive at ~
top level -- its own form -- to govern the forms that follow."
              names))))

(defun evaluate-forms-capturing (source)
  "Read and evaluate SOURCE's forms one at a time in order -- load semantics, so
a form that changes how later forms read takes effect before the next read.
Returns (values outcome warnings completed): OUTCOME is (:values list count) or
(:error plist) captured at signal time so its backtrace and restarts reflect the
error site, WARNINGS the per-form nested-directive cautions, COMPLETED the number
of forms fully evaluated (meaningful on the error path, where the offending form
did not complete)."
  (let ((current-form nil)
        (warnings '())
        (completed 0))
    (values
     (with-structured-capture (current-form)
       (let ((values nil)
             (count 0))
         (with-source-forms (form source :current current-form)
           (incf count)
           (let ((warning (directive-warning form)))
             (when warning (push warning warnings)))
           (setf values (multiple-value-list (eval form)))
           (incf completed))
         (when (zerop count)
           (error "Eval tool received no forms."))
         (list :values values completed)))
     (nreverse warnings)
     completed)))

(defun spawn-eval-worker (package output-stream thunk)
  "Run THUNK on a dedicated thread with the invoking subject, *package*, and the
output streams rebound, returning the thread; THUNK's value is read back with
join-thread. The subject is rebound because a fresh thread would otherwise run
under the global system subject. Evaluating in the live image confers full
first-party image authority -- a form runs with the invoking subject and can
write/edit files and run processes -- so this is no confinement boundary, only a
faithful relay of the caller's authority. Reading runs on this thread under
*package* so an unqualified symbol interns in the requested package and read-time
evaluation (#.) stays inside the deadline and abort guards rather than on the
runner."
  (let ((subject *call-subject*))
    (sb-thread:make-thread
     (lambda ()
       (let ((*call-subject* subject)
             (*package* package)
             (*standard-output* output-stream)
             (*error-output* output-stream))
         (multiple-value-bind (value warnings completed) (funcall thunk)
           (values value (list :package (package-name *package*)
                               :warnings warnings
                               :forms-evaluated completed)))))
     :name "kli-eval-tool")))

(defun spawn-eval-thread (source package output-stream)
  "Read and evaluate SOURCE's forms on a dedicated thread, returning the thread;
its value is (:values list count) or (:error plist)."
  (spawn-eval-worker package output-stream
                     (lambda () (evaluate-forms-capturing source))))

(defun definition-target (form)
  "The symbol FORM redefines -- the cadr of a (definer NAME ...) form when NAME
is a non-nil symbol, else nil. Read under the active *package* so the name
interns where the definition will."
  (when (and (consp form) (symbolp (second form)) (second form))
    (second form)))

(defun recompile-and-rerun (definition-source rerun-source)
  "On the calling thread under the active *package*: snapshot the prior
fdefinition of the symbol DEFINITION-SOURCE redefines, evaluate the definition
capturing compiler notes and warnings, then evaluate RERUN-SOURCE under the
structured condition capture. Returns (values info warnings): INFO a plist with
:redefined-symbol, :prior-fdefinition (a live function object or nil, image-only),
the :prior-fdefinition-restorable flag, :compiler-notes, and the :definition and
:rerun outcomes ((:values list count) | (:error plist)); WARNINGS the per-form
nested-directive cautions from the definition and re-run, like the eval tool's."
  (let* ((notes '())
         (target nil)
         (prior nil)
         (restorable nil)
         (def-warnings '())
         (definition-outcome
           (let ((current-form nil))
             (flet ((note (condition)
                      (push (princ-to-string condition) notes)
                      (let ((restart (find-restart 'muffle-warning condition)))
                        (when restart (invoke-restart restart)))))
               (handler-bind ((sb-ext:compiler-note #'note)
                              (warning #'note))
                 (with-structured-capture (current-form)
                   (let ((values nil)
                         (count 0))
                     (with-source-forms (form definition-source :current current-form)
                       (incf count)
                       (let ((warning (directive-warning form)))
                         (when warning (push warning def-warnings)))
                       (when (= count 1)
                         (setf target (definition-target form))
                         (when (and target (fboundp target))
                           (setf restorable t
                                 prior (fdefinition target))))
                       (setf values (multiple-value-list (eval form))))
                     (when (zerop count)
                       (error "Recompile-rerun received no definition."))
                     (list :values values count))))))))
    (multiple-value-bind (rerun-outcome rerun-warnings)
        (if (eq (first definition-outcome) :error)
            (values '(:skipped) '())
            (evaluate-forms-capturing rerun-source))
      (values
       (list :redefined-symbol target
             :prior-fdefinition prior
             :prior-fdefinition-restorable restorable
             :compiler-notes (nreverse notes)
             :definition definition-outcome
             :rerun rerun-outcome)
       (append (nreverse def-warnings) rerun-warnings)))))

(defun spawn-recompile-rerun-thread (definition-source rerun-source package
                                     output-stream)
  (spawn-eval-worker package output-stream
                     (lambda ()
                       (recompile-and-rerun definition-source rerun-source))))

(defun wait-for-eval-thread (thread timeout-seconds)
  "Wait for THREAD, polling so the deadline and an abort request both cut
the wait short. Returns (values outcome timed-out-p aborted-p report); REPORT is
the worker's (:package name :warnings list :forms-evaluated k) plist, nil when
the thread was interrupted before it could report."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop while (sb-thread:thread-alive-p thread)
          do (when (tool-abort-requested-p)
               (return-from wait-for-eval-thread (values nil nil t)))
             (when (>= (get-internal-real-time) deadline)
               (return-from wait-for-eval-thread (values nil t nil)))
             (sleep 0.05))
    (multiple-value-bind (outcome report) (sb-thread:join-thread thread)
      (values outcome nil nil report))))

(defun terminate-eval-thread (thread)
  "Interrupt THREAD and wait briefly for it to unwind, returning true when
it stopped. Interrupting arbitrary mid-eval code can leave image state
half-mutated -- locks held, structures partially written -- but the
alternative is a runaway form wedging the agent worker beyond even quit's
reach. The reap is bounded like the bash kill: a thread alive past it is
stuck in foreign code, and joining it would wedge the worker for the
form's natural lifetime."
  (when (sb-thread:thread-alive-p thread)
    (ignore-errors (sb-thread:terminate-thread thread))
    (sb-thread:join-thread thread :timeout 5 :default nil))
  (not (sb-thread:thread-alive-p thread)))

(defun make-eval-output (context)
  "An output stream to bind for an eval, plus the spill tee behind it (NIL when no
protocol is active or spilling is off). When teed, the stream forwards the first
*eval-output-character-limit* characters to an inline window and streams the whole
output to a lossless backing; otherwise it is a plain bounded stream that drops the
overflow. Returns (values stream tee)."
  (let* ((protocol (active-protocol context))
         (tee (and protocol
                   (open-spill-tee protocol
                                   :window-limit *eval-output-character-limit*))))
    (if tee
        (values tee tee)
        (values (make-instance 'bounded-output-stream
                               :target (make-string-output-stream)
                               :limit *eval-output-character-limit*)
                nil))))

(defun captured-eval-output (output readable-p)
  "Captured output text and truncated-p flag for the bound OUTPUT stream, empty
when READABLE-P is NIL because the eval thread may still be writing. Reads the
inline window only -- never finalizes the backing, since a parked thread may resume
and write more; finalize is the terminal caller's job once the thread has stopped."
  (if readable-p
      (etypecase output
        (spill-tee-stream
         (values (tee-window output) (tee-truncated-p output)))
        (bounded-output-stream
         (values (get-output-stream-string (bounded-output-stream-target output))
                 (bounded-output-stream-truncated-p output))))
      (values "" nil)))

(defun finalize-eval-tee (tee truncated-p)
  "Fold TEE's backing into the registry and return (values handle total-bytes) when
the output was TRUNCATED, or discard it (no handle) when it fit whole. (values NIL
NIL) when there is no tee or finalize failed. Call only once the producing thread
has stopped -- it closes the part file."
  (when tee
    (if truncated-p
        (let ((entry (finalize-spill-tee tee)))
          (when entry
            (values (spill-entry-token entry) (spill-entry-bytes entry))))
        (discard-spill-tee tee))))

(defun eval-output-spill-notice (truncated-p handle bytes)
  "Footer line for truncated eval output: a handle marker pointing at the retained
backing, a bare truncation notice when nothing was retained, or NIL when the output
fit whole."
  (when truncated-p
    (if handle
        (format-spill-marker "eval output" :shown *eval-output-character-limit*
                                           :total bytes :handle handle :unit "byte")
        (format-spill-marker "eval output" :shown *eval-output-character-limit*))))

(defun eval-annotation-prefix (package current-package warnings)
  "Leading annotation text for an eval result: a package-change line when the
call moved the current package, then any nested-directive cautions. Each line
ends in a newline so the result body follows below; empty string when neither
applies."
  (with-output-to-string (stream)
    (when (and current-package
               (not (string= current-package (package-name package))))
      (format stream "Package: ~A -> ~A~%" (package-name package) current-package))
    (dolist (warning warnings)
      (format stream "~A~%" warning))))

(defun interrupted-eval-result (reason package timeout-seconds
                                stdout truncated-p stopped-p &key handle bytes)
  (make-tool-result
   :content (list (make-tool-text-content
                   (format nil "~A -- the interrupted form may have left ~
image state partially modified.~:[~%(output unavailable -- the eval ~
thread did not stop)~;~]~@[~%~A~]"
                           (ecase reason
                             (:timeout (format nil "Eval timed out after ~D second~:P"
                                               timeout-seconds))
                             (:abort "Eval aborted before completion")
                             (:killed "The parked eval thread died before completing"))
                           stopped-p
                           (eval-output-spill-notice truncated-p handle bytes))))
   :details (append (list :package (package-name package)
                          :current-package (package-name package)
                          :timeout-seconds timeout-seconds)
                    (ecase reason
                      (:timeout (list :timed-out-p t))
                      (:abort (list :aborted-p t))
                      (:killed (list :killed-p t)))
                    (list :stdout stdout
                          :stdout-truncated-p truncated-p)
                    (when handle (list :stdout-handle handle)))
   :error-p t))

(defun eval-error-result (info package timeout-seconds stdout truncated-p
                          &key current-package warnings forms-evaluated
                               handle bytes extra-details)
  "Tool-result for a form that signaled. INFO is the captured-condition plist;
its structured fields ride :details to the model, the condition message heads
the human text, and any captured output precedes it. CURRENT-PACKAGE,
WARNINGS, and FORMS-EVALUATED ride alongside; HANDLE/BYTES point at the retained
output backing when the captured output overflowed. EXTRA-DETAILS prefixes the
result details."
  (let ((message (getf info :message)))
    (make-tool-result
     :content (list (make-tool-text-content
                     (format nil "~A~A~@[~%~D earlier form~:P ran before this error.~]~
~@[~%~A~]"
                             (eval-annotation-prefix package current-package warnings)
                             (if (plusp (length stdout))
                                 (format nil "~A~%~A" stdout message)
                                 message)
                             (and forms-evaluated (plusp forms-evaluated) forms-evaluated)
                             (eval-output-spill-notice truncated-p handle bytes))))
     :details (append extra-details
                      (list :package (package-name package)
                            :current-package (or current-package
                                                 (package-name package))
                            :timeout-seconds timeout-seconds)
                      info
                      (list :stdout stdout
                            :stdout-truncated-p truncated-p)
                      (when handle (list :stdout-handle handle))
                      (when forms-evaluated (list :forms forms-evaluated))
                      (when warnings (list :warnings warnings)))
     :error-p t)))

(defun eval-values-result (values form-count package timeout-seconds
                           stdout truncated-p
                           &key extra-details current-package warnings
                                handle bytes protocol)
  "Tool-result for forms that evaluated to VALUES. EXTRA-DETAILS prefixes :details
-- the interactive path tags the resumed park there. CURRENT-PACKAGE drives the
delta line and rides :details; WARNINGS surface the nested-directive cautions.
HANDLE/BYTES point at the retained output backing; PROTOCOL lets an overflowing
value be retained whole behind its own handle."
  (multiple-value-bind (value-lines values-truncated-p value-handles)
      (printable-values values protocol)
    (make-tool-result
     :content (list (make-tool-text-content
                     (format nil "~A~A~@[~%~A~]~{~%~A~}"
                             (eval-annotation-prefix package current-package warnings)
                             (eval-tool-output-text stdout value-lines)
                             (eval-output-spill-notice truncated-p handle bytes)
                             (printed-value-spill-notices value-handles))))
     :details (append extra-details
                      (list :package (package-name package)
                            :current-package (or current-package
                                                 (package-name package))
                            :forms form-count
                            :timeout-seconds timeout-seconds
                            :values value-lines
                            :stdout stdout
                            :stdout-truncated-p truncated-p
                            :values-truncated values-truncated-p)
                      (when handle (list :stdout-handle handle))
                      (when value-handles
                        (list :value-handles
                              (mapcar (lambda (h) (list :index (first h)
                                                        :handle (second h)
                                                        :bytes (cddr h)))
                                      value-handles)))
                      (when warnings (list :warnings warnings))))))

(defun eval-on-error (parameters)
  "The :on-error mode: :return (one-shot, default) or :interactive (park on error)."
  (let ((value (tool-parameter parameters :on-error)))
    (cond
      ((null value) :return)
      ((string-equal value "return") :return)
      ((string-equal value "interactive") :interactive)
      (t (error "Unknown :on-error mode ~S; expected \"return\" or \"interactive\"."
                value)))))

(defparameter +eval-repair-note+
  "Note: the input had unbalanced parentheses and was auto-repaired before it was
read. Repair is indentation-driven, so a sub-form can close at a different place
than simply adding a trailing paren would; the exact text read is in
:repaired-source. Verify its structure -- especially how many top-level forms it
became -- matches what you intended."
  "Leading note prepended to a result whose source paren-repair rewrote.")

(defun prepend-result-note (content note)
  "CONTENT with NOTE prepended to its leading text block, or a fresh text block
when there is none, so an annotation precedes the result body."
  (let ((first (first content)))
    (if (and first (eq (getf first :type) :text))
        (cons (make-tool-text-content (format nil "~A~%~A" note (getf first :text)))
              (rest content))
        (cons (make-tool-text-content note) content))))

(defun annotate-repair (result repaired-details)
  "When REPAIRED-DETAILS is non-nil -- paren-repair rewrote at least one input --
prepend the repair note to RESULT's text and merge REPAIRED-DETAILS into its
:details; otherwise RESULT unchanged."
  (if repaired-details
      (make-tool-result
       :content (prepend-result-note (tool-result-content result) +eval-repair-note+)
       :details (append repaired-details (tool-result-details result))
       :error-p (tool-result-error-p result))
      result))

(defun run-eval-tool (tool parameters context &key call-id on-update)
  "Evaluate :form in :package. The forms are read and run on a dedicated
thread so symbols intern in the requested package at read time and the
deadline and an abort request stay observable from a polling wait, exactly
like the bash tool's process wait -- a runaway form is interrupted instead
of wedging the agent worker. Output is capped inside the capture stream at
*eval-output-character-limit* characters. With :on-error \"interactive\" an
unhandled error parks the thread on the live restarts instead of unwinding;
eval-continue and eval-abort then resume or unwind it."
  (declare (ignore call-id on-update))
  (multiple-value-bind (source repaired-p)
      (repair-if-needed (or (tool-parameter parameters :form)
                            (error "Eval tool requires :form.")))
    (let* ((package (resolve-eval-package parameters context))
           (timeout-seconds (eval-timeout-seconds parameters))
           (result
             (if (eq (eval-on-error parameters) :interactive)
                 (run-interactive-eval tool source package context timeout-seconds)
                 (multiple-value-bind (stream tee) (make-eval-output context)
                   (let ((thread (spawn-eval-thread source package stream)))
                     (multiple-value-bind (outcome timed-out-p aborted-p report)
                         (wait-for-eval-thread thread timeout-seconds)
                       (if (or timed-out-p aborted-p)
                           (let ((stopped-p (terminate-eval-thread thread)))
                             (multiple-value-bind (stdout truncated-p)
                                 (captured-eval-output stream stopped-p)
                               (multiple-value-bind (handle bytes)
                                   (and stopped-p (finalize-eval-tee tee truncated-p))
                                 (interrupted-eval-result (if timed-out-p :timeout :abort)
                                                          package timeout-seconds
                                                          stdout truncated-p stopped-p
                                                          :handle handle :bytes bytes))))
                           (progn
                             (persist-eval-package context (getf report :package))
                             (destructuring-bind (kind payload &optional form-count) outcome
                               (multiple-value-bind (stdout truncated-p)
                                   (captured-eval-output stream t)
                                 (multiple-value-bind (handle bytes)
                                     (finalize-eval-tee tee truncated-p)
                                   (ecase kind
                                     (:error
                                      (eval-error-result payload package timeout-seconds
                                                         stdout truncated-p
                                                         :current-package (getf report :package)
                                                         :warnings (getf report :warnings)
                                                         :forms-evaluated (getf report
                                                                                :forms-evaluated)
                                                         :handle handle :bytes bytes))
                                     (:values
                                      (eval-values-result payload form-count package
                                                          timeout-seconds stdout truncated-p
                                                          :current-package (getf report :package)
                                                          :warnings (getf report :warnings)
                                                          :handle handle :bytes bytes
                                                          :protocol (active-protocol context)))))))))))))))
      (annotate-repair result
                       (when repaired-p
                         (list :repaired-p t :repaired-source source))))))

(defun outcome->wire (outcome)
  "Normalize an evaluation OUTCOME -- (:values list count) | (:error plist) -- to
a wire keyword plist: a success carries :status :ok with printable :values and
:forms; an error carries :status :error spread with the captured condition
fields, so jsonify renders a nested object rather than a ragged array."
  (ecase (first outcome)
    (:values (destructuring-bind (values count) (rest outcome)
               (multiple-value-bind (value-lines values-truncated-p)
                   (printable-values values)
                 (list :status :ok
                       :values value-lines
                       :forms count
                       :values-truncated values-truncated-p))))
    (:skipped (list :status :skipped))
    (:error (list* :status :error (second outcome)))))

(defun recompile-rerun-summary (symbol definition rerun stdout truncated-p
                                &key handle bytes protocol)
  (with-output-to-string (text)
    (when (plusp (length stdout))
      (format text "~A~%" stdout))
    (if (eq (first definition) :error)
        (format text "Redefinition failed: ~A. The re-run was skipped."
                (getf (second definition) :message))
        (progn
          (format text "Redefined ~A. " (or symbol "form"))
          (if (eq (first rerun) :error)
              (format text "Re-run still errors: ~A" (getf (second rerun) :message))
              (multiple-value-bind (value-lines values-truncated-p value-handles)
                  (printable-values (second rerun) protocol)
                (declare (ignore values-truncated-p))
                (format text "Re-run succeeded: ~{~A~^ ~}" value-lines)
                (dolist (notice (printed-value-spill-notices value-handles))
                  (format text "~%~A" notice))))))
    (let ((notice (eval-output-spill-notice truncated-p handle bytes)))
      (when notice (format text "~%~A" notice)))))

(defun recompile-rerun-result (info package timeout-seconds stdout truncated-p
                               &key current-package warnings handle bytes protocol)
  "Tool-result for a recompile-rerun. INFO is the recompile-and-rerun plist; its
compiler notes, normalized redefinition and re-run outcomes, and restorable flag
ride :details to the model, the prior fdefinition itself staying in the image.
CURRENT-PACKAGE drives the delta line and rides :details; WARNINGS surface the
nested-directive cautions, uniform with the eval tool. HANDLE/BYTES point at the
retained output backing; PROTOCOL retains an overflowing re-run value."
  (let ((definition (getf info :definition))
        (rerun (getf info :rerun))
        (symbol (getf info :redefined-symbol)))
    (make-tool-result
     :content (list (make-tool-text-content
                     (format nil "~A~A"
                             (eval-annotation-prefix package current-package warnings)
                             (recompile-rerun-summary symbol definition rerun
                                                      stdout truncated-p
                                                      :handle handle :bytes bytes
                                                      :protocol protocol))))
     :details (append
               (list :package (package-name package)
                     :current-package (or current-package (package-name package))
                     :timeout-seconds timeout-seconds
                     :redefined-symbol (and symbol (princ-to-string symbol))
                     :prior-fdefinition-restorable
                     (getf info :prior-fdefinition-restorable)
                     :compiler-notes (getf info :compiler-notes)
                     :definition (outcome->wire definition)
                     :rerun (outcome->wire rerun)
                     :stdout stdout
                     :stdout-truncated-p truncated-p)
               (when handle (list :stdout-handle handle))
               (when warnings (list :warnings warnings)))
     :error-p (or (eq (first definition) :error)
                  (eq (first rerun) :error)))))

(defun run-recompile-rerun-tool (tool parameters context &key call-id on-update)
  "Redefine :definition then re-run :form in the live image in one step: the prior
fdefinition is snapshotted (returned for manual restore) before the redefinition
is applied, the definition is evaluated capturing compiler notes, and :form runs
under the structured condition capture. The redefinition takes effect immediately
and is not rolled back if :form errors. Both steps share a dedicated thread under
:package and reuse the eval tool's deadline, abort, and bounded-output machinery."
  (declare (ignore tool call-id on-update))
  (multiple-value-bind (definition def-repaired-p)
      (repair-if-needed (or (tool-parameter parameters :definition)
                            (error "Recompile-rerun requires :definition.")))
    (multiple-value-bind (form form-repaired-p)
        (repair-if-needed (or (tool-parameter parameters :form)
                              (error "Recompile-rerun requires :form.")))
      (let* ((package (resolve-eval-package parameters context))
             (timeout-seconds (eval-timeout-seconds parameters))
             (result
               (multiple-value-bind (stream tee) (make-eval-output context)
                 (let ((thread (spawn-recompile-rerun-thread definition form package
                                                             stream)))
                   (multiple-value-bind (outcome timed-out-p aborted-p report)
                       (wait-for-eval-thread thread timeout-seconds)
                     (if (or timed-out-p aborted-p)
                         (let ((stopped-p (terminate-eval-thread thread)))
                           (multiple-value-bind (stdout truncated-p)
                               (captured-eval-output stream stopped-p)
                             (multiple-value-bind (handle bytes)
                                 (and stopped-p (finalize-eval-tee tee truncated-p))
                               (interrupted-eval-result (if timed-out-p :timeout :abort)
                                                        package timeout-seconds
                                                        stdout truncated-p stopped-p
                                                        :handle handle :bytes bytes))))
                         (progn
                           (persist-eval-package context (getf report :package))
                           (multiple-value-bind (stdout truncated-p)
                               (captured-eval-output stream t)
                             (multiple-value-bind (handle bytes)
                                 (finalize-eval-tee tee truncated-p)
                               (recompile-rerun-result outcome package timeout-seconds
                                                       stdout truncated-p
                                                       :current-package (getf report :package)
                                                       :warnings (getf report :warnings)
                                                       :handle handle :bytes bytes
                                                       :protocol (active-protocol context)))))))))))
        (annotate-repair
         result
         (when (or def-repaired-p form-repaired-p)
           (append (list :repaired-p t)
                   (when def-repaired-p (list :repaired-definition definition))
                   (when form-repaired-p (list :repaired-form form)))))))))
