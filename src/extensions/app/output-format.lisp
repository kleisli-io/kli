(in-package #:kli/app)

;;;; Machine-consumable output for the headless surfaces. Each formatter is a
;;;; session-event listener closure that renders the agent event stream onto a
;;;; result stream in one of three shapes: the final text, one terminal result
;;;; object, or one object per event. A serialization fault degrades to the
;;;; plain final text plus a stderr note, and the structured record is built in
;;;; full before any write so a fault never leaves a half-written object behind.

(defun json-key (key)
  "A lowercase string object key from a plist KEY."
  (string-downcase (if (symbolp key) (symbol-name key) (princ-to-string key))))

(defun fmt-obj (&rest kvs)
  "A jzon-ready string-keyed object from alternating key/value arguments."
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defun payload->json-obj (payload)
  "A string-keyed jzon object mirroring an event PAYLOAD plist, lowercasing the
keyword keys. Values pass through jzon's own coercion."
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on payload by #'cddr
        do (setf (gethash (json-key key) table) value)
        finally (return table)))

(defun end-event-final-text (event)
  "The final assistant text carried by an :agent/end EVENT, or NIL."
  (getf (event-payload event) :text))

(defun write-json-line (object stream)
  "Serialize OBJECT to JSON in full, then write it to STREAM as one line. The
serialization completes before any write, so a fault leaves STREAM untouched
rather than carrying a half-written record."
  (let ((json (com.inuoe.jzon:stringify object)))
    (write-line json stream)))

(defun make-text-formatter (stream)
  "A formatter that writes only the final assistant text, on :agent/end."
  (lambda (event mode-id context)
    (declare (ignore mode-id context))
    (when (eq (event-type event) :agent/end)
      (write-line (or (end-event-final-text event) "") stream)
      (finish-output stream))))

(defun make-json-formatter (stream)
  "A formatter that writes one terminal result object, on :agent/end. A
serialization fault degrades to the plain final text plus a stderr note."
  (lambda (event mode-id context)
    (declare (ignore mode-id context))
    (when (eq (event-type event) :agent/end)
      (let ((payload (event-payload event)))
        (handler-case
            (let ((object (fmt-obj "type" "result"
                                   "text" (or (getf payload :text) "")
                                   "state" (string-downcase
                                            (princ-to-string
                                             (getf payload :state)))))
                  (usage (getf payload :usage)))
              (when usage (setf (gethash "usage" object) usage))
              (write-json-line object stream))
          (error (condition)
            (write-line (or (getf payload :text) "") stream)
            (format *error-output*
                    "kli: json output formatter fault (~A)~%" condition)))
        (finish-output stream)))))

(defparameter +stream-json-event-types+
  '(:agent/delta :agent/message-end :agent/tool-execution-end :agent/end)
  "Event types the stream-json formatter renders, one object per event.")

(defun make-stream-json-formatter (stream)
  "A formatter that writes one JSON object per relevant event. A serialization
fault on the terminal event degrades to the plain final text plus a stderr note."
  (lambda (event mode-id context)
    (declare (ignore mode-id context))
    (let ((type (event-type event))
          (payload (event-payload event)))
      (when (member type +stream-json-event-types+)
        (handler-case
            (write-json-line
             (fmt-obj "type" (string-downcase (symbol-name type))
                      "payload" (payload->json-obj payload))
             stream)
          (error (condition)
            (when (eq type :agent/end)
              (write-line (or (getf payload :text) "") stream))
            (format *error-output*
                    "kli: stream-json output formatter fault (~A)~%" condition)))
        (finish-output stream)))))

(defun output-formatter (format stream)
  "A session-event listener handler that renders the agent event stream onto
STREAM in FORMAT, one of :text, :json, or :stream-json."
  (ecase format
    (:text (make-text-formatter stream))
    (:json (make-json-formatter stream))
    (:stream-json (make-stream-json-formatter stream))))

(defun parse-output-format (token)
  "Map a --output-format TOKEN to a format keyword. An absent or unrecognized
token selects :text."
  (cond ((null token) :text)
        ((string-equal token "json") :json)
        ((string-equal token "stream-json") :stream-json)
        (t :text)))

(defun register-output-formatter (context format stream)
  "Register the FORMAT output formatter as a session-event listener on CONTEXT's
agent-session service, writing to STREAM. A profile without that service is a
no-op so a non-agent surface simply emits nothing."
  (let ((service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (when service
      (register-session-event-listener
       service
       (make-session-event-listener :output-formatter
                                    (output-formatter format stream))
       context))))
