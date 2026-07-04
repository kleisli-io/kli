(in-package #:kli/runtime/isolated)

;;; Newline-delimited UTF-8 JSON-RPC 2.0 framing over injectable streams. No
;;; subprocess here -- this layer is exercised in-process over string streams.

(defparameter *max-message-length* (* 4 1024 1024)
  "Cap on a single newline-delimited message in characters, so a runaway line
cannot exhaust the heap.")

(define-condition isolated-error (error) ()
  (:documentation "Root of the errors this transport raises."))

(define-condition isolated-protocol-error (isolated-error)
  ((detail :initarg :detail :initform nil :reader isolated-error-detail))
  (:report (lambda (c s)
             (format s "isolated transport protocol error: ~A"
                     (isolated-error-detail c)))))

(define-condition isolated-timeout (isolated-error)
  ((method :initarg :method :initform nil :reader isolated-error-method)
   (timeout :initarg :timeout :initform nil :reader isolated-error-timeout))
  (:report (lambda (c s)
             (format s "isolated call ~A timed out after ~A s"
                     (isolated-error-method c) (isolated-error-timeout c)))))

(define-condition isolated-aborted (isolated-error)
  ((method :initarg :method :initform nil :reader isolated-error-method))
  (:report (lambda (c s)
             (format s "isolated call ~A aborted" (isolated-error-method c)))))

(define-condition isolated-jsonrpc-error (isolated-error)
  ((payload :initarg :payload :initform nil :reader isolated-jsonrpc-error-payload))
  (:report (lambda (c s)
             (format s "isolated call returned a JSON-RPC error: ~A"
                     (isolated-jsonrpc-error-payload c)))))

(define-condition isolated-transport-closed (isolated-error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "isolated transport closed"))))

(defun jsonify (value)
  "Coerce VALUE to a jzon-safe tree, passing hash-tables through unchanged so
case-sensitive JSON-RPC argument keys survive instead of being downcased."
  (kli/text:jsonify value :hash-tables :passthrough))

(defun write-jsonrpc-line (stream object)
  (com.inuoe.jzon:stringify object :stream stream)
  (write-char #\Newline stream)
  (force-output stream)
  object)

(defun write-jsonrpc-request (stream id method params)
  "Frame one JSON-RPC request line for METHOD with PARAMS correlated by ID."
  (let ((object (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" object) "2.0"
          (gethash "id" object) id
          (gethash "method" object) method)
    (when params
      (setf (gethash "params" object) (jsonify params)))
    (write-jsonrpc-line stream object)))

(defun write-jsonrpc-notification (stream method params)
  "Frame one id-less JSON-RPC notification line for METHOD with PARAMS."
  (let ((object (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" object) "2.0"
          (gethash "method" object) method)
    (when params
      (setf (gethash "params" object) (jsonify params)))
    (write-jsonrpc-line stream object)))

(defun write-jsonrpc-response (stream id result)
  "Frame one JSON-RPC success response correlated by ID. The server-direction
counterpart to write-jsonrpc-request, over the same line+jsonify primitives."
  (let ((object (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" object) "2.0"
          (gethash "id" object) id
          (gethash "result" object) (jsonify result))
    (write-jsonrpc-line stream object)))

(defun write-jsonrpc-error (stream id code message &optional data)
  "Frame one JSON-RPC error response correlated by ID. ID may be null for a
parse error with no recoverable id."
  (let ((object (make-hash-table :test #'equal))
        (err (make-hash-table :test #'equal)))
    (setf (gethash "code" err) code
          (gethash "message" err) message)
    (when data (setf (gethash "data" err) (jsonify data)))
    (setf (gethash "jsonrpc" object) "2.0"
          (gethash "id" object) id
          (gethash "error" object) err)
    (write-jsonrpc-line stream object)))

(defun read-message-line (stream)
  "Read one newline-delimited line, returning the string or :eof. Reads char by
char so the length cap applies before a long line is fully allocated; a final
line with no trailing newline is returned, the next read yields :eof."
  (let ((buffer (make-array 256 :element-type 'character
                                :adjustable t :fill-pointer 0)))
    (loop for char = (read-char stream nil :eof)
          do (cond
               ((eq char :eof)
                (return (if (zerop (fill-pointer buffer))
                            :eof
                            (coerce buffer 'simple-string))))
               ((char= char #\Newline)
                (return (coerce buffer 'simple-string)))
               (t
                (when (>= (fill-pointer buffer) *max-message-length*)
                  (error 'isolated-protocol-error
                         :detail "message exceeds the length cap"))
                (vector-push-extend char buffer))))))

(defun read-jsonrpc-message (stream)
  "Read and parse one newline-delimited JSON message, returning the equal
hash-table or :eof at end of stream."
  (let ((line (read-message-line stream)))
    (if (eq line :eof)
        :eof
        (com.inuoe.jzon:parse line))))

(defun classify-message (msg)
  "Return :response when MSG carries an id key, else :notification."
  (if (nth-value 1 (gethash "id" msg)) :response :notification))

(defun message-id (msg) (gethash "id" msg))
(defun message-method (msg) (gethash "method" msg))
(defun message-params (msg) (gethash "params" msg))
(defun message-result (msg) (gethash "result" msg))
(defun message-error (msg) (gethash "error" msg))
