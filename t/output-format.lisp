(in-package #:kli/tests)
(in-suite all)

;;;; Headless output formatters: format-token parsing, the text/json/stream-json
;;;; renderings over canned agent events, and the degrade-to-plain-text fault path.

(defun output-format-run (formatter &rest events)
  "Run FORMATTER (a listener closure) over EVENTS, returning the result string."
  (with-output-to-string (stream)
    (let ((handler (funcall formatter stream)))
      (dolist (event events)
        (funcall handler event nil nil)))))

(test output-format-parse-token
  "parse-output-format maps the known tokens and defaults the rest to :text."
  (is (eq :text (app::parse-output-format nil)))
  (is (eq :text (app::parse-output-format "text")))
  (is (eq :json (app::parse-output-format "json")))
  (is (eq :stream-json (app::parse-output-format "stream-json")))
  (is (eq :json (app::parse-output-format "JSON")))
  (is (eq :text (app::parse-output-format "bogus"))))

(test output-format-text-emits-final-text-on-end
  "The text formatter writes the final assistant text plus a newline on
:agent/end and nothing on any other event."
  (is (equal "the answer
"
             (output-format-run #'app::make-text-formatter
                                (event:make-event
                                 :agent/end
                                 :payload (list :text "the answer"
                                                :state :completed)))))
  (is (equal ""
             (output-format-run #'app::make-text-formatter
                                (event:make-event
                                 :agent/delta
                                 :payload (list :text "partial"))))))

(test output-format-json-emits-one-result-object
  "The json formatter writes one parseable result object on :agent/end with the
final text and run state."
  (let* ((line (output-format-run #'app::make-json-formatter
                                  (event:make-event
                                   :agent/end
                                   :payload (list :text "the answer"
                                                  :state :completed))))
         (object (com.inuoe.jzon:parse line)))
    (is (typep object 'hash-table))
    (is (equal "result" (gethash "type" object)))
    (is (equal "the answer" (gethash "text" object)))
    (is (equal "completed" (gethash "state" object)))))

(test output-format-stream-json-emits-one-object-per-event
  "The stream-json formatter writes one parseable object per relevant event."
  (let* ((out (output-format-run
               #'app::make-stream-json-formatter
               (event:make-event :agent/delta :payload (list :text "a"))
               (event:make-event :agent/delta :payload (list :text "b"))
               (event:make-event :agent/end
                                 :payload (list :text "ab" :state :completed))))
         (lines (remove "" (uiop:split-string out :separator '(#\Newline))
                        :test #'string=)))
    (is (= 3 (length lines)))
    (dolist (line lines)
      (let ((object (com.inuoe.jzon:parse line)))
        (is (typep object 'hash-table))
        (is (gethash "type" object))))))

(test output-format-json-degrades-to-plain-text-on-fault
  "A serialization fault degrades to the plain final text on the result stream
and a diagnostic on stderr, never a half-written object."
  (let ((poison (make-hash-table :test #'equal)))
    ;; A self-referential object makes jzon raise json-recursive-write-error.
    (setf (gethash "self" poison) poison)
    (let* ((event (event:make-event
                   :agent/end
                   :payload (list :text "fallback text"
                                  :state :completed
                                  :usage poison)))
           (err (make-string-output-stream))
           (out (with-output-to-string (stream)
                  (let ((*error-output* err))
                    (funcall (app::make-json-formatter stream) event nil nil)))))
      (is (equal "fallback text" (string-right-trim '(#\Newline) out)))
      (is (null (search "{" out)) "no half-written object reaches the stream")
      (is (plusp (length (get-output-stream-string err)))))))

(test output-format-terminal-input-tty-p-is-boolean
  "terminal-input-tty-p is callable and answers a boolean."
  (is (typep (app::terminal-input-tty-p) 'boolean)))

(test headless-io-splits-result-and-diagnostics
  "with-headless-io binds the result channel and the diagnostic channel to
distinct streams so machine output and diagnostics never interleave."
  (let ((result (make-string-output-stream))
        (diagnostics (make-string-output-stream)))
    (app::with-headless-io (:result result :diagnostics diagnostics)
      (write-string "payload" *standard-output*)
      (write-string "note" *error-output*))
    (is (equal "payload" (get-output-stream-string result)))
    (is (equal "note" (get-output-stream-string diagnostics)))))
