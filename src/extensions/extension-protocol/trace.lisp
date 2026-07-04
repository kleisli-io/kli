(in-package #:kli/ext)

;;; Trace as a reversible contribution. Installing a trace-contribution traces a
;;; function and routes its trace output into a bounded per-protocol ring of
;;; lines; retracting untraces and restores the prior trace sink. The trace
;;; inherits the effect install/retract morphism, so a trace stamped with its
;;; extension drains through deactivate-extension exactly like any other
;;; manifest entry -- the reversibility a subprocess cannot guarantee.
;;;
;;; SBCL keeps one global *trace-output*, so a protocol's ring owns the sink for
;;; the span it is installed; sequential per-protocol use is isolated, while two
;;; protocols tracing at once share the last-installed sink. The ring holds
;;; strings (not the stream), so it survives the snapshot serializer.

(defparameter +trace-buffer-storage-key+ :kli/ext.trace-buffer
  "Storage key for a protocol's bounded ring of trace-output lines.")

(defparameter *trace-buffer-cap* 200
  "Most recent trace-output lines a protocol's ring retains.")

(defparameter +trace-evicted-storage-key+ :kli/ext.trace-evicted
  "Storage key for the count of trace-output lines dropped past the ring cap.")

(defun protocol-trace-buffer (protocol)
  "PROTOCOL's trace-output ring: a list of lines, newest first."
  (ensure-protocol-storage protocol +trace-buffer-storage-key+ (lambda () '())))

(defun protocol-trace-evicted (protocol)
  "How many trace-output lines PROTOCOL's ring has dropped past its cap."
  (ensure-protocol-storage protocol +trace-evicted-storage-key+ (lambda () 0)))

(defun trace-buffer-push-line (protocol cap line)
  "Prepend LINE to PROTOCOL's ring, evicting the oldest past CAP and counting the
drop so a reader can tell lines were lost."
  (let* ((kept (cons line (protocol-trace-buffer protocol)))
         (len (length kept))
         (keep (min cap len)))
    (when (> len keep)
      (setf (protocol-storage protocol +trace-evicted-storage-key+)
            (+ (protocol-trace-evicted protocol) (- len keep))))
    (setf (protocol-storage protocol +trace-buffer-storage-key+)
          (subseq kept 0 keep))))

(defclass trace-ring-stream (sb-gray:fundamental-character-output-stream)
  ((protocol
    :initarg :protocol
    :reader trace-ring-stream-protocol)
   (cap
    :initarg :cap
    :reader trace-ring-stream-cap)
   (line
    :initform (make-string-output-stream)
    :accessor trace-ring-stream-line)
   (count
    :initform 0
    :accessor trace-ring-stream-count))
  (:documentation
   "Gray output stream that finalizes each completed line into PROTOCOL's ring,
capped at CAP lines and at *render-line-limit* characters per line. Both caps
apply as output arrives, so neither a long trace nor one structurally-long line
can grow the ring; an unfinished trailing line is dropped."))

(defun finalize-trace-line (stream)
  "Push STREAM's current line into the ring, front-truncated past the per-line
limit, then reset for the next line."
  (let* ((count (trace-ring-stream-count stream))
         (head (get-output-stream-string (trace-ring-stream-line stream)))
         (line (if (> count *render-line-limit*)
                   (format nil "~A[+~D chars]" head (- count *render-line-limit*))
                   head)))
    (trace-buffer-push-line (trace-ring-stream-protocol stream)
                            (trace-ring-stream-cap stream)
                            line)
    (setf (trace-ring-stream-count stream) 0)))

(defmethod sb-gray:stream-write-char ((stream trace-ring-stream) char)
  (if (char= char #\Newline)
      (finalize-trace-line stream)
      (progn
        (when (< (trace-ring-stream-count stream) *render-line-limit*)
          (write-char char (trace-ring-stream-line stream)))
        (incf (trace-ring-stream-count stream))))
  char)

(defmethod sb-gray:stream-write-string ((stream trace-ring-stream) string
                                        &optional (start 0) end)
  (loop for i from start below (or end (length string))
        do (sb-gray:stream-write-char stream (char string i)))
  string)

(defclass trace-contribution (effect-contribution) ()
  (:documentation
   "A reversible trace. It inherits the effect install/retract morphism: its
installer traces a function and routes the output into a ring, its retractor
untraces and restores the prior sink."))

(defun resolve-trace-symbol (function-spec package)
  "Read FUNCTION-SPEC to a symbol under PACKAGE. A package-qualified name resolves
regardless of PACKAGE; an unqualified name interns there."
  (let* ((*package* package)
         (spec (read-from-string function-spec)))
    (unless (symbolp spec)
      (error "Trace function spec ~S did not read as a symbol." function-spec))
    spec))

(defun make-trace-contribution (&key function-spec package source extension
                                     (cap *trace-buffer-cap*))
  "A trace-contribution tracing the function FUNCTION-SPEC names, routing its
output into PROTOCOL's ring on install and untracing on retract. PACKAGE (a
package designator, default CL-USER) is where an unqualified spec interns.
EXTENSION, when supplied, stamps the drain key so deactivate-extension reverses
the trace."
  (let* ((pkg (or (find-package (or package :cl-user))
                  (error "Unknown package: ~A" package)))
         (contribution
           (make-instance
            'trace-contribution
            :kind :trace
            :name (normalize-extension-id function-spec)
            :source source
            :installer
            (lambda (protocol contribution context)
              (declare (ignore contribution context))
              (require-capability :image/eval)
              (let ((symbol (resolve-trace-symbol function-spec pkg))
                    (prior *trace-output*))
                (unless (fboundp symbol)
                  (error "~S is not a function." symbol))
                (when (macro-function symbol)
                  (error "~S is a macro, not a function; tracing it would trace its ~
expander, which does not fire at compiled call sites." symbol))
                (when (special-operator-p symbol)
                  (error "~S is a special operator, not a function, and cannot be ~
traced." symbol))
                (let ((stream (make-instance 'trace-ring-stream
                                             :protocol protocol :cap cap)))
                  (eval (list 'trace symbol))
                  (setf *trace-output* stream)
                  (list :symbol symbol :prior-trace-output prior
                        :ring-stream stream))))
            :retractor
            (lambda (protocol contribution context)
              (declare (ignore protocol context))
              (let* ((state (contribution-state contribution))
                     (stream (getf state :ring-stream)))
                (eval (list 'untrace (getf state :symbol)))
                (when (and stream (plusp (trace-ring-stream-count stream)))
                  (finalize-trace-line stream))
                (setf *trace-output* (getf state :prior-trace-output)))))))
    (when extension
      (setf (contribution-extension contribution) extension))
    contribution))

(defcontribution-kind :trace (extension-id form)
  (destructuring-bind (_ name &key function package) form
    (declare (ignore _))
    `(make-trace-contribution
      :function-spec ,(or function `(string ',name))
      :package ,package
      :source ',extension-id)))
