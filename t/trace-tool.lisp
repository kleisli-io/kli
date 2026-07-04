(in-package #:kli/tests)

(in-suite all)

;;; Trace tooling: the trace tool installs a reversible trace stamped with its
;;; extension, trace-read relays the per-protocol ring, and retracting the
;;; owning extension untraces. A fresh fixture function per concern keeps the
;;; image-global trace state from leaking between tests.

(defun trace-fixture-add (a b)
  (+ a b))

(defun trace-fixture-mul (a b)
  (* a b))

(defmacro trace-fixture-macro (x)
  `(identity ,x))

(defun ensure-untraced (symbol)
  "Untrace SYMBOL only if it is currently traced, so test cleanup neither errors
nor warns when a retract already untraced it."
  (when (member symbol (eval '(trace)))
    (eval (list 'untrace symbol))))

(ext:defextension trace-fixture-declared
  (:provides
   (trace trace-fixture-add
     :function "trace-fixture-add"
     :package "KLI/TESTS")))

(defun install-trace-tools (context)
  "The trace family; returns the trace tool's manifest handle so a test can
retract it and observe the trace reverse."
  (install-extension context tools-trace:*trace-read-tool-extension-manifest*)
  (install-extension context tools-trace:*untrace-tool-extension-manifest*)
  (install-extension context tools-trace:*trace-tool-extension-manifest*))

(test trace-tool-traces-reads-and-reverses
  "Tracing routes a function's calls into the ring trace-read relays, and
retracting the trace tool's extension untraces it (the reversibility a
subprocess cannot offer)."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (let ((handle (install-trace-tools context)))
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (is (not (ext:tool-result-error-p
                       (ext:invoke-tool protocol :trace
                                        '(:function "trace-fixture-add"
                                          :package "KLI/TESTS")
                                        context)))))
           (is (member 'trace-fixture-add (eval '(trace)))
               "the image reports the function traced")
           (trace-fixture-add 2 3)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/inspect))))
             (let* ((result (ext:invoke-tool protocol :trace-read '() context))
                    (details (ext:tool-result-details result)))
               (is (not (ext:tool-result-error-p result)))
               (is (plusp (getf details :count)))
               (is (every #'stringp (getf details :lines)))
               (is (search "TRACE-FIXTURE-ADD" (tool-result-text result)))))
           (with-extension-load-authority
             (ext:retract-manifest handle protocol context))
           (is (not (member 'trace-fixture-add (eval '(trace))))
               "retracting the extension untraced the function")
           (let ((before (length (ext:protocol-trace-buffer protocol))))
             (trace-fixture-add 4 5)
             (is (= before (length (ext:protocol-trace-buffer protocol)))
                 "a call after teardown no longer appends")))
      (ensure-untraced 'trace-fixture-add)
      (setf *trace-output* saved-trace-output))))

(test trace-tools-gate-on-image-capabilities
  "trace mutates the image and gates on :image/eval; trace-read is read-only and
gates on the weaker :image/inspect. An eval holder reaches both through the
implication closure; an inspect-only holder reads but cannot trace."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
             (signals ext:capability-denied
               (ext:invoke-tool protocol :trace
                                '(:function "trace-fixture-mul") context))
             (signals ext:capability-denied
               (ext:invoke-tool protocol :trace-read '() context)))
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/inspect))))
             (is (not (ext:tool-result-error-p
                       (ext:invoke-tool protocol :trace-read '() context))))
             (signals ext:capability-denied
               (ext:invoke-tool protocol :trace
                                '(:function "trace-fixture-mul") context)))
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (is (not (ext:tool-result-error-p
                       (ext:invoke-tool protocol :trace
                                        '(:function "trace-fixture-mul"
                                          :package "KLI/TESTS")
                                        context))))
             (is (not (ext:tool-result-error-p
                       (ext:invoke-tool protocol :trace-read '() context))))))
      (ensure-untraced 'trace-fixture-mul)
      (setf *trace-output* saved-trace-output))))

(test trace-contribution-kind-reverses-declaratively
  "A manifest may declare a trace as a first-class :trace contribution; it
installs under the declaring extension and reverses on retract."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (with-granted-authority (:manifest/install :manifest/retract :image/eval)
           (let ((handle (ext:install-manifest
                          *trace-fixture-declared-extension-manifest*
                          protocol context)))
             (is (member 'trace-fixture-add (eval '(trace)))
                 "the declared trace traced the function on activation")
             (ext:retract-manifest handle protocol context)
             (is (not (member 'trace-fixture-add (eval '(trace))))
                 "retracting the declaring extension untraced it")))
      (ensure-untraced 'trace-fixture-add)
      (setf *trace-output* saved-trace-output))))

;;; Per-line bound: a structurally-long trace line is front-truncated as its
;;; newline arrives, so one line cannot bloat the ring.

(test trace-ring-front-truncates-a-long-line
  "A line past the per-line limit is stored as head + [+N chars] on its newline;
the line-count cap is unaffected."
  (let* ((text:*render-line-limit* 10)
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (stream (make-instance 'ext::trace-ring-stream
                                :protocol protocol :cap 10)))
    (write-string "short" stream)
    (write-char #\Newline stream)
    (write-string (make-string 100 :initial-element #\x) stream)
    (write-char #\Newline stream)
    (let ((lines (reverse (ext:protocol-trace-buffer protocol))))
      (is (= 2 (length lines)))
      (is (string= "short" (first lines)))
      (is (string= "xxxxxxxxxx[+90 chars]" (second lines))))))

(test trace-unknown-package-errors
  "Tracing with a :package that names no package errors rather than silently
resolving to CL-USER."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (let ((result (ext:invoke-tool
                            protocol :trace
                            '(:function "trace-fixture-add"
                              :package "NO-SUCH-PACKAGE-XYZ")
                            context)))
               (is (ext:tool-result-error-p result))
               (is (search "Unknown package" (tool-result-text result))))))
      (ensure-untraced 'trace-fixture-add)
      (setf *trace-output* saved-trace-output))))

(test trace-undefined-function-errors
  "Tracing a symbol with no function definition errors rather than reporting a
success that never accrues calls."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (let ((result (ext:invoke-tool
                            protocol :trace
                            '(:function "trace-fixture-undefined"
                              :package "KLI/TESTS")
                            context)))
               (is (ext:tool-result-error-p result))
               (is (search "is not a function" (tool-result-text result))))))
      (setf *trace-output* saved-trace-output))))

(test trace-result-echoes-the-resolved-symbol
  "A successful trace names the resolved PKG::SYMBOL in its text and :details, so
the model sees exactly what was traced, not just the spec it passed."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (let* ((result (ext:invoke-tool
                             protocol :trace
                             '(:function "trace-fixture-add" :package "KLI/TESTS")
                             context))
                    (details (ext:tool-result-details result)))
               (is (not (ext:tool-result-error-p result)))
               (is (string= "KLI/TESTS::TRACE-FIXTURE-ADD" (getf details :symbol)))
               (is (string= "KLI/TESTS" (getf details :package)))
               (is (search "KLI/TESTS::TRACE-FIXTURE-ADD"
                           (tool-result-text result))))))
      (ensure-untraced 'trace-fixture-add)
      (setf *trace-output* saved-trace-output))))

(test untrace-removes-one-trace-and-errors-when-absent
  "untrace stops one traced function and leaves the others active; untracing a
function that is not traced errors rather than silently succeeding."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (ext:invoke-tool protocol :trace
                              '(:function "trace-fixture-add" :package "KLI/TESTS")
                              context)
             (ext:invoke-tool protocol :trace
                              '(:function "trace-fixture-mul" :package "KLI/TESTS")
                              context)
             (is (member 'trace-fixture-add (eval '(trace))))
             (is (member 'trace-fixture-mul (eval '(trace))))
             (let ((removed (ext:invoke-tool
                             protocol :untrace
                             '(:function "trace-fixture-add" :package "KLI/TESTS")
                             context)))
               (is (not (ext:tool-result-error-p removed)))
               (is (search "Untraced KLI/TESTS::TRACE-FIXTURE-ADD"
                           (tool-result-text removed))))
             (is (not (member 'trace-fixture-add (eval '(trace))))
                 "the named trace was removed")
             (is (member 'trace-fixture-mul (eval '(trace)))
                 "the other trace survives")
             (let ((absent (ext:invoke-tool
                            protocol :untrace
                            '(:function "trace-fixture-add" :package "KLI/TESTS")
                            context)))
               (is (ext:tool-result-error-p absent))
               (is (search "No active trace" (tool-result-text absent))))))
      (ensure-untraced 'trace-fixture-add)
      (ensure-untraced 'trace-fixture-mul)
      (setf *trace-output* saved-trace-output))))

(test trace-read-marks-evicted-lines
  "When the ring drops older lines past its cap, trace-read leads with a marker and
reports how many under :evicted, so a truncated trace does not read as complete."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((stream (make-instance 'ext::trace-ring-stream
                                        :protocol protocol :cap 3)))
             (dotimes (i 5)
               (write-string (format nil "line-~D" i) stream)
               (write-char #\Newline stream)))
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/inspect))))
             (let* ((result (ext:invoke-tool protocol :trace-read '() context))
                    (details (ext:tool-result-details result)))
               (is (= 2 (getf details :evicted))
                   "two lines were dropped past the cap of three")
               (is (= 3 (getf details :count)) "the ring kept the most recent three")
               (is (search "[+2 earlier trace lines dropped]"
                           (tool-result-text result))))))
      (setf *trace-output* saved-trace-output))))

(test trace-rejects-a-macro
  "Tracing a macro errors rather than installing a trace on an expander that does
not fire at compiled call sites."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (let ((result (ext:invoke-tool
                            protocol :trace
                            '(:function "trace-fixture-macro" :package "KLI/TESTS")
                            context)))
               (is (ext:tool-result-error-p result))
               (is (search "macro" (tool-result-text result)))))
           (is (not (member 'trace-fixture-macro (eval '(trace))))
               "the rejected macro was never traced"))
      (ensure-untraced 'trace-fixture-macro)
      (setf *trace-output* saved-trace-output))))

(test trace-double-trace-is-idempotent
  "Tracing a function already traced reports the existing trace and installs no
second contribution, rather than erroring or stacking duplicates."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (is (not (ext:tool-result-error-p
                       (ext:invoke-tool
                        protocol :trace
                        '(:function "trace-fixture-add" :package "KLI/TESTS")
                        context))))
             (let* ((again (ext:invoke-tool
                            protocol :trace
                            '(:function "trace-fixture-add" :package "KLI/TESTS")
                            context))
                    (details (ext:tool-result-details again)))
               (is (not (ext:tool-result-error-p again)))
               (is (getf details :already-tracing-p))
               (is (search "Already tracing" (tool-result-text again)))))
           (is (= 1 (count-if
                     (lambda (c)
                       (and (eq (ext:contribution-kind c) :trace)
                            (eq (getf (ext:contribution-state c) :symbol)
                                'trace-fixture-add)))
                     (ext:protocol-installed-contributions protocol)))
               "the second trace installed no duplicate contribution"))
      (ensure-untraced 'trace-fixture-add)
      (setf *trace-output* saved-trace-output))))

(test trace-flags-a-package-qualifier-override
  "A package-qualified :function whose home package differs from an explicit
:package traces the qualified symbol and flags the override, rather than silently
ignoring the :package the caller passed."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (progn
           (install-trace-tools context)
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (let* ((result (ext:invoke-tool
                             protocol :trace
                             '(:function "kli/tests::trace-fixture-add"
                               :package "CL-USER")
                             context))
                    (details (ext:tool-result-details result)))
               (is (not (ext:tool-result-error-p result)))
               (is (getf details :package-mismatch))
               (is (string= "KLI/TESTS" (getf details :package)))
               (is (search "overriding the requested package"
                           (tool-result-text result))))))
      (ensure-untraced 'trace-fixture-add)
      (setf *trace-output* saved-trace-output))))

(test deactivate-extension-best-effort-survives-a-failing-retractor
  "When one contribution's retractor errors, teardown still retracts the rest and
removes the extension, rather than stranding a half-torn-down extension."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (with-granted-authority (:manifest/install :manifest/retract :image/eval)
           (let* ((handle (ext:install-manifest
                           *trace-fixture-declared-extension-manifest*
                           protocol context))
                  (trace-contribution
                    (find-if (lambda (c) (eq (ext:contribution-kind c) :trace))
                             (ext:protocol-installed-contributions protocol)))
                  (ext-id (ext:contribution-extension trace-contribution))
                  (poison (ext:make-effect-contribution
                           :name "poison-retractor"
                           :installer (lambda (protocol contribution context)
                                        (declare (ignore protocol contribution context))
                                        nil)
                           :retractor (lambda (protocol contribution context)
                                        (declare (ignore protocol contribution context))
                                        (error "retractor refuses to run")))))
             (is (member 'trace-fixture-add (eval '(trace)))
                 "the declared trace traced the function on activation")
             (setf (ext:contribution-extension poison) ext-id)
             (ext:install-contribution protocol poison context)
             (ext:retract-manifest handle protocol context)
             (is (not (member 'trace-fixture-add (eval '(trace))))
                 "the real trace reversed despite the poison retractor erroring")
             (is (null (gethash ext-id (ext:protocol-extensions protocol)))
                 "the extension was removed despite a retractor erroring")))
      (ensure-untraced 'trace-fixture-add)
      (setf *trace-output* saved-trace-output))))

(test trace-teardown-flushes-a-partial-final-line
  "Teardown finalizes a trace line still mid-emit with no trailing newline, so the
last line is not silently dropped when the trace reverses."
  (let* ((text:*render-line-limit* 100)
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (saved-trace-output *trace-output*))
    (unwind-protect
         (let ((handle (install-trace-tools context)))
           (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
             (ext:invoke-tool protocol :trace
                              '(:function "trace-fixture-add" :package "KLI/TESTS")
                              context))
           (let* ((contribution
                    (find-if (lambda (c) (eq (ext:contribution-kind c) :trace))
                             (ext:protocol-installed-contributions protocol)))
                  (stream (getf (ext:contribution-state contribution) :ring-stream)))
             (write-string "mid-line-with-no-newline" stream)
             (is (null (ext:protocol-trace-buffer protocol))
                 "an unfinished line has not landed before teardown"))
           (with-extension-load-authority
             (ext:retract-manifest handle protocol context))
           (let ((lines (reverse (ext:protocol-trace-buffer protocol))))
             (is (= 1 (length lines)))
             (is (string= "mid-line-with-no-newline" (first lines))
                 "teardown flushed the partial trailing line")))
      (ensure-untraced 'trace-fixture-add)
      (setf *trace-output* saved-trace-output))))
