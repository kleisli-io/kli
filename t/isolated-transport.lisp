(in-package #:kli/tests)

(in-suite all)

;;; Stream-level framing, in-process over string streams.

(test jsonrpc-request-framing-round-trips
  (let ((out (make-string-output-stream)))
    (isolated:write-jsonrpc-request out 7 "tools/list" '(:cursor "x"))
    (let ((text (get-output-stream-string out)))
      (is (= 1 (count #\Newline text)) "a request is exactly one line")
      (let ((msg (isolated:read-jsonrpc-message (make-string-input-stream text))))
        (is (string= "2.0" (gethash "jsonrpc" msg)))
        (is (= 7 (isolated:message-id msg)))
        (is (string= "tools/list" (isolated:message-method msg)))
        (is (string= "x" (gethash "cursor" (isolated:message-params msg))))
        (is (eq :response (isolated:classify-message msg)))))))

(test jsonrpc-request-passes-hash-table-arguments-through-unchanged
  "Tool-call arguments ride as a hash-table so their case-sensitive keys survive
the wire, unlike keyword plists whose keys downcase."
  (let ((arguments (make-hash-table :test #'equal))
        (params (make-hash-table :test #'equal))
        (out (make-string-output-stream)))
    (setf (gethash "filePath" arguments) "/tmp/x"
          (gethash "name" params) "read"
          (gethash "arguments" params) arguments)
    (isolated:write-jsonrpc-request out 11 "tools/call" params)
    (let* ((text (get-output-stream-string out))
           (msg (isolated:read-jsonrpc-message (make-string-input-stream text)))
           (wire-args (gethash "arguments" (isolated:message-params msg))))
      (is (string= "/tmp/x" (gethash "filePath" wire-args))
          "the mixed-case argument key survives unchanged")
      (is (null (nth-value 1 (gethash "filepath" wire-args)))
          "no downcased duplicate key is introduced"))))

(test jsonrpc-notification-has-no-id
  (let ((out (make-string-output-stream)))
    (isolated:write-jsonrpc-notification out "initialized" nil)
    (let* ((text (get-output-stream-string out))
           (msg (isolated:read-jsonrpc-message (make-string-input-stream text))))
      (is (null (isolated:message-id msg)))
      (is (string= "initialized" (isolated:message-method msg)))
      (is (eq :notification (isolated:classify-message msg))))))

(test jsonrpc-read-frames-each-line-then-eof
  (let ((stream (make-string-input-stream
                 (format nil "{\"id\":1,\"result\":1}~%{\"method\":\"x\"}~%"))))
    (is (= 1 (isolated:message-id (isolated:read-jsonrpc-message stream))))
    (is (string= "x" (isolated:message-method (isolated:read-jsonrpc-message stream))))
    (is (eq :eof (isolated:read-jsonrpc-message stream)))))

(test jsonrpc-read-parses-partial-last-line
  (let ((stream (make-string-input-stream "{\"id\":9,\"result\":true}")))
    (let ((msg (isolated:read-jsonrpc-message stream)))
      (is (= 9 (isolated:message-id msg)))
      (is (eq t (isolated:message-result msg))))
    (is (eq :eof (isolated:read-jsonrpc-message stream)))))

;;; Correlation and lifecycle, white-box over the registry without a subprocess.

(test isolated-correlation-routes-by-id
  (let* ((notes '())
         (proc (isolated::make-isolated-process
                :notification-handler (lambda (m) (push m notes))))
         (box7 (isolated::make-pending-call))
         (box8 (isolated::make-pending-call)))
    (isolated::register-pending proc 7 box7)
    (isolated::register-pending proc 8 box8)
    (isolated::route-message
     proc (isolated:read-jsonrpc-message
           (make-string-input-stream "{\"id\":7,\"result\":42}")))
    (is (sb-thread:wait-on-semaphore (isolated::pending-call-semaphore box7) :timeout 1)
        "the addressed box is signaled")
    (is (not (sb-thread:try-semaphore (isolated::pending-call-semaphore box8)))
        "no other box is signaled")
    (is (= 42 (gethash "result" (isolated::pending-call-message box7))))
    (isolated::route-message
     proc (isolated:read-jsonrpc-message
           (make-string-input-stream "{\"method\":\"tick\"}")))
    (is (= 1 (length notes)) "an id-less message reaches the handler")
    (is (string= "tick" (isolated:message-method (first notes))))))

(test isolated-call-times-out-and-reclaims-pending
  (let ((proc (isolated::make-isolated-process
               :input-stream (make-string-output-stream))))
    (signals isolated:isolated-timeout
      (isolated:call-isolated proc "slow" nil :timeout 0.1))
    (is (zerop (hash-table-count (isolated::isolated-process-pending proc)))
        "a timed-out request leaves no pending entry")))

(test isolated-call-aborts-promptly
  (let ((proc (isolated::make-isolated-process
               :input-stream (make-string-output-stream)))
        (start (get-internal-real-time)))
    (let ((ext:*tool-abort-predicate* (lambda () t)))
      (signals isolated:isolated-aborted
        (isolated:call-isolated proc "x" nil :timeout 30)))
    (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 5)
        "abort cuts the wait short")
    (is (zerop (hash-table-count (isolated::isolated-process-pending proc))))))

;;; Integration against a real child. The fixture is a POSIX shell responder
;;; that echoes the request method back as a result and emits one unsolicited
;;; notification at startup; it uses only shell builtins, no external tools.

(defun echo-fixture-script ()
  (format nil "~{~A~%~}"
          (list
           "printf '{\"jsonrpc\":\"2.0\",\"method\":\"ready\"}\\n'"
           "while IFS= read -r line; do"
           "case \"$line\" in"
           "*'\"id\":'*)"
           "rest=${line#*'\"id\":'}"
           "id=${rest%%[!0-9]*}"
           "rest=${line#*'\"method\":\"'}"
           "method=${rest%%'\"'*}"
           "printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"echo\":\"%s\"}}\\n' \"$id\" \"$method\""
           ";;"
           "esac"
           "done")))

(test isolated-round-trip-and-notification-against-fixture
  (let* ((notes (make-array 0 :adjustable t :fill-pointer 0))
         (note-lock (sb-thread:make-mutex))
         (proc (isolated:spawn-isolated-process
                "sh"
                :arguments (list "-c" (echo-fixture-script))
                :notification-handler
                (lambda (msg)
                  (sb-thread:with-mutex (note-lock)
                    (vector-push-extend msg notes))))))
    (is (not (null proc)) "spawn returns a transport")
    (unwind-protect
         (progn
           (let ((result (isolated:call-isolated proc "ping" '(:value 1) :timeout 5)))
             (is (hash-table-p result))
             (is (string= "ping" (gethash "echo" result))
                 "the response is correlated and mapped back"))
           (let ((deadline (+ (get-internal-real-time)
                              (* 5 internal-time-units-per-second))))
             (loop until (or (plusp (length notes))
                             (>= (get-internal-real-time) deadline))
                   do (sleep 0.02)))
           (is (plusp (length notes)) "the startup notification arrives")
           (is (string= "ready" (isolated:message-method (aref notes 0)))))
      (isolated:teardown-isolated proc))))

(test isolated-teardown-leaves-no-orphan
  (let ((proc (isolated:spawn-isolated-process
               "sh" :arguments (list "-c" (echo-fixture-script)))))
    (is (isolated:isolated-process-alive-p proc) "the child is running")
    (isolated:teardown-isolated proc)
    (is (not (isolated:isolated-process-alive-p proc)) "teardown reaps the child")))
