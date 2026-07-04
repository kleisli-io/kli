(in-package #:kli/model/transports)

(define-condition model-network-error (error)
  ((cause :initarg :cause :reader model-network-error-cause))
  (:documentation "A network-layer failure (socket, DNS, TLS, or stream) on a
model request, re-signaled under one type so consumers classify transport
trouble without knowing the HTTP stack's condition zoo.")
  (:report (lambda (c s) (format s "~A" (model-network-error-cause c)))))

(defmethod kli/ext:condition-category ((condition model-network-error))
  :network)

(define-condition model-stream-format-error (error)
  ((payload :initarg :payload :reader model-stream-format-error-payload)
   (cause :initarg :cause :reader model-stream-format-error-cause))
  (:documentation "An SSE data payload the provider sent that does not parse
as JSON. The provider broke the stream protocol, so this classifies as a
provider failure, not an internal one.")
  (:report (lambda (c s)
             (format s "Malformed payload from provider: ~A"
                     (model-stream-format-error-cause c)))))

(defmethod kli/ext:condition-category ((condition model-stream-format-error))
  :provider)

(defparameter *sse-max-line-length* (* 1024 1024)
  "Hard cap on one SSE line. Only a broken or hostile provider produces a
line this long, and accumulating it would grow without bound on a stream
that never ends its line, so the stream fails loudly instead.")

(defparameter *sse-max-event-size* (* 4 1024 1024)
  "Hard cap on the accumulated data payload of one SSE event. Bounds a
stream of data lines that never sends the closing blank line.")

(define-condition model-stream-overflow-error (error)
  ((limit :initarg :limit :reader model-stream-overflow-error-limit)
   (scope :initarg :scope :reader model-stream-overflow-error-scope))
  (:documentation "An SSE line or event payload exceeded its size cap. The
provider broke the stream protocol, so this classifies as a provider
failure and ends the stream visibly (no-silent-caps).")
  (:report (lambda (c s)
             (format s "Provider stream exceeded the ~:D-character ~(~A~) limit"
                     (model-stream-overflow-error-limit c)
                     (model-stream-overflow-error-scope c)))))

(defmethod kli/ext:condition-category ((condition model-stream-overflow-error))
  :provider)

(defparameter *error-body-cap* 65536
  "Character cap on a drained non-2xx response body. Error bodies are small
JSON or HTML pages, and an unbounded drain of a huge or never-ending body
exhausts the heap before the error even renders.")

(defun drain-capped-body (stream &optional (cap *error-body-cap*))
  "Read STREAM into a string for an error report, stopping at CAP characters.
A capped read appends a marker so a partial body is never mistaken for the
whole response. Read failures mid-drain keep what arrived."
  (let ((buf (make-array (min cap 1024) :element-type 'character
                                        :adjustable t :fill-pointer 0))
        (truncated nil))
    (ignore-errors
     (loop for ch = (read-char stream nil)
           while ch
           do (if (< (fill-pointer buf) cap)
                  (vector-push-extend ch buf)
                  (progn (setf truncated t) (return)))))
    (if truncated
        (format nil "~A [body truncated at ~:D characters]"
                (coerce buf 'simple-string) cap)
        (coerce buf 'simple-string))))

(defparameter *provider-error-display-cap* 400
  "Character cap when an error body must be rendered raw. Bodies that parse
as JSON reduce to the provider's error message instead.")

(defun %json-error-message (obj)
  "The human message inside a parsed provider error object, or NIL. Providers
wrap it differently: {error: {message}}, {response: {error: {message}}} on
Responses failure events, a string-valued error key, or a bare message key."
  (labels ((str (v) (and (stringp v)
                         (plusp (length (string-trim " " v)))
                         v))
           (msg (v)
             (cond
               ((stringp v) (str v))
               ((hash-table-p v)
                (or (str (gethash "message" v))
                    (msg (gethash "error" v)))))))
    (and (hash-table-p obj)
         (or (msg (gethash "error" obj))
             (msg (gethash "response" obj))
             (str (gethash "message" obj))))))

(defun provider-error-display (body)
  "Render BODY, a provider error response, for humans. JSON bodies reduce to
the provider's error message. Anything else (proxy HTML, truncated JSON) is
shown raw, capped with a marker. NIL when BODY is empty or not a string."
  (when (and (stringp body) (plusp (length body)))
    (let ((message (%json-error-message
                    (handler-case (com.inuoe.jzon:parse body)
                      (error () nil)))))
      (cond
        (message message)
        ((<= (length body) *provider-error-display-cap*) body)
        (t (format nil "~A... [~:D more characters]"
                   (subseq body 0 *provider-error-display-cap*)
                   (- (length body) *provider-error-display-cap*)))))))

(defun parse-sse-payload (data-string)
  "Parse an SSE data payload as JSON. Unparseable input means the provider is
breaking the stream protocol, so it re-signals as a provider-category
MODEL-STREAM-FORMAT-ERROR instead of dying as an internal parse fault."
  (handler-case (com.inuoe.jzon:parse data-string)
    (error (condition)
      (error 'model-stream-format-error :payload data-string :cause condition))))

(defun call-classifying-network-errors (thunk)
  "Run THUNK, re-signaling network-layer conditions as MODEL-NETWORK-ERROR.
Anything else -- provider API errors, parse bugs -- passes through untouched."
  (handler-bind (((or usocket:socket-error usocket:ns-error
                      cl+ssl::cl+ssl-error stream-error)
                  (lambda (condition)
                    (error 'model-network-error :cause condition))))
    (funcall thunk)))

(sb-alien:define-alien-routine ("shutdown" %socket-shutdown) sb-alien:int
  (fd sb-alien:int) (how sb-alien:int))

(defconstant +shut-rdwr+ 2 "Linux SHUT_RDWR -- shut down both directions of a socket.")

(defun stream-socket-fd (stream)
  "Descend flexi/chunga/cl+ssl wrappers to the underlying socket fd, or NIL. The live
model stream is flexi -> chunked -> cl+ssl::ssl-stream -> fd-stream, so the TLS layer
must be descended or an HTTPS stream resolves to no fd at all."
  (loop
    (typecase stream
      (integer (return stream))
      (sb-sys:fd-stream (return (sb-sys:fd-stream-fd stream)))
      (flexi-streams:flexi-stream
       (setf stream (flexi-streams:flexi-stream-stream stream)))
      (chunga:chunked-stream
       (setf stream (chunga:chunked-stream-stream stream)))
      (cl+ssl::ssl-stream
       (setf stream (cl+ssl::ssl-stream-socket stream)))
      (t (return nil)))))

(defun shutdown-request-stream (stream)
  "Cross-thread cancel of a blocking read. shutdown(fd, SHUT_RDWR) unwinds the parked
read (clean EOF on plain TCP, an SSL error through TLS). The reader thread closes the
stream in its own unwind-protect -- closing here would race that read and lose the
shutdown wakeup, wedging the reader. On a non-socket stream (no fd), degrade to a
best-effort close. Idempotent and safe from a non-reader thread."
  (let ((fd (ignore-errors (stream-socket-fd stream))))
    (if fd
        (ignore-errors (%socket-shutdown fd +shut-rdwr+))
        (ignore-errors (close stream)))))

(defvar *transport-connect* nil
  "Test seam: (host port https-p) -> (values raw-octet-stream effective-https-p),
replacing the real usocket connect. NIL routes through usocket:socket-connect. Lets tests
drive the :stream exchange (and its cancellation) over a socketpair with no network.")

(defun %connect-raw-stream (host port https)
  (if *transport-connect*
      (funcall *transport-connect* host port https)
      (values (usocket:socket-stream
               (usocket:socket-connect host port
                                       :element-type '(unsigned-byte 8)
                                       :nodelay :if-supported))
              https)))

(defun open-cancellable-stream (request url body headers &key (verify :required))
  "POST BODY to URL, owning the socket so the request is cancellable across its WHOLE
lifecycle. The TCP connection is opened here and REQUEST's stream-closer is armed on it
BEFORE any TLS or HTTP I/O, so a cross-thread shutdown-request-stream unwinds a read
parked in the TLS handshake or the response-header wait (the reasoning-model \"thinking\"
hold), not only the SSE body. drakma never attaches SSL to a provided :stream and its
:stream path assumes the flexi-over-chunked shape its own wrap-stream produces, so both
steps are replicated here using drakma's own SSL builder. Returns (values stream status)."
  (let* ((uri   (puri:parse-uri url))
         (host  (puri:uri-host uri))
         (port  (or (puri:uri-port uri) (if (eq (puri:uri-scheme uri) :https) 443 80))))
    (call-classifying-network-errors
     (lambda ()
       (multiple-value-bind (raw https)
           (%connect-raw-stream host port (eq (puri:uri-scheme uri) :https))
         (setf (model-request-stream-closer request)
               (lambda () (shutdown-request-stream raw)))
         (let ((handed-off nil))
           (unwind-protect
                (let ((wrapped (flexi-streams:make-flexi-stream
                                (chunga:make-chunked-stream
                                 (if https
                                     (drakma::make-ssl-stream raw :hostname host
                                                                  :verify verify)
                                     raw))
                                :external-format :latin-1)))
                  ;; :close t sends Connection: close. The socket is never reused, and
                  ;; without it a keep-alive error response has no EOF until the server
                  ;; idle-closes -- a non-2xx body drain would park for minutes.
                  (multiple-value-prog1
                      (drakma:http-request url
                                           :method :post
                                           :content (flexi-streams:string-to-octets
                                                     body :external-format :utf-8)
                                           :content-type "application/json"
                                           :additional-headers headers
                                           :stream wrapped :close t :want-stream t)
                    (setf handed-off t)))
             (unless handed-off
               ;; A failure before the streaming handoff (TLS handshake, request
               ;; write, header read) is the only exit where nobody else owns RAW --
               ;; the adapter's unwind-protect takes over once this returns. The
               ;; closer is disarmed first so a racing abort cannot shutdown(2) a
               ;; recycled fd. :abort skips the flush of buffered request bytes,
               ;; which would itself signal on the dead connection and leave the
               ;; fd open.
               (setf (model-request-stream-closer request) nil)
               (ignore-errors (close raw :abort t))))))))))

(defun %dispatch-stream-event (on-event event payload)
  "Invoke ON-EVENT inside the :model-stream fault barrier, declining to contain
conditions that classify as provider or network failures. Those mean the stream
itself is broken and must reach the caller's retry and rendering path -- a
contained one ends the turn as a silently truncated reply. A consumer bug stays
contained so one bad listener cannot kill the stream."
  (let ((broken nil))
    (block dispatch
      (kli/ext:with-extension-fault-barrier (:seam :model-stream :id :on-event)
        (handler-bind ((error (lambda (condition)
                                (unless (eq (kli/ext:condition-category condition)
                                            :internal)
                                  (setf broken condition)
                                  (return-from dispatch)))))
          (funcall on-event event payload))))
    (when broken (error broken))))

(defun %read-limited-line (stream limit)
  "READ-LINE with a length cap, returning the line string or :EOF. READ-LINE
would allocate an arbitrarily long line before any cap could apply, so this
reads char by char and signals MODEL-STREAM-OVERFLOW-ERROR at LIMIT."
  (let ((buf (make-array 80 :element-type 'character
                            :adjustable t :fill-pointer 0)))
    (loop for ch = (read-char stream nil :eof)
          do (cond
               ((eq ch :eof)
                (return (if (zerop (fill-pointer buf))
                            :eof
                            (coerce buf 'simple-string))))
               ((char= ch #\Newline)
                (return (coerce buf 'simple-string)))
               (t
                (when (>= (fill-pointer buf) limit)
                  (error 'model-stream-overflow-error :limit limit :scope :line))
                (vector-push-extend ch buf))))))

(defun stream-sse-events (char-stream on-event)
  "Frame a text/event-stream from CHAR-STREAM, invoking (ON-EVENT name data) per
event. A blank line closes the current event. Comment lines and a `data: [DONE]`
payload are skipped. EOF, including an unclean TLS close, ends the stream. Lines
and per-event payloads beyond their caps signal MODEL-STREAM-OVERFLOW-ERROR."
  (let ((event nil) (data '()) (event-size 0))
    (flet ((dispatch ()
             (when data
               (let ((payload (format nil "~{~A~^~%~}" (nreverse data))))
                 (setf data nil event-size 0)
                 (unless (string= payload "[DONE]")
                   (%dispatch-stream-event on-event event payload)))
               (setf event nil))))
      (call-classifying-network-errors
       (lambda ()
         (handler-case
             (loop for line = (%read-limited-line char-stream *sse-max-line-length*)
                   until (eq line :eof)
                   do (cond
                        ((zerop (length line)) (dispatch))
                        ((char= (char line 0) #\:) nil)
                        ((and (>= (length line) 6) (string= (subseq line 0 6) "event:"))
                         (setf event (string-trim " " (subseq line 6))))
                        ((and (>= (length line) 5) (string= (subseq line 0 5) "data:"))
                         (incf event-size (- (length line) 5))
                         (when (> event-size *sse-max-event-size*)
                           (error 'model-stream-overflow-error
                                  :limit *sse-max-event-size* :scope :event))
                         (push (string-left-trim " " (subseq line 5)) data)))
                   finally (dispatch))
           (end-of-file () (dispatch))))))))
