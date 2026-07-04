(in-package #:kli/tools/introspect)

;;; Read-only views of the running image: each tool relays one introspection
;;; provider entry and holds no live-object knowledge of its own.

(defparameter *introspect-print-length* 100
  "Bound on *print-length* when printing an introspection view.")

(defparameter *introspect-print-level* 20
  "Bound on *print-level* when printing an introspection view.")

(defparameter *introspect-object-limit* 200
  "Most live-object ids the list-objects tool renders before eliding the rest, so
a pathologically large registry cannot flood the context.")

(defmacro with-introspect-printer (&body body)
  "Run BODY under the introspect tool's length/level/circle printer bounds, so a
huge or circular structure cannot exhaust the heap or print forever."
  `(let ((*print-circle* t)
         (*print-length* *introspect-print-length*)
         (*print-level* *introspect-print-level*))
     ,@body))

(defun bounded-prin1 (object)
  "OBJECT printed readably under the introspect printer bounds, then each line
front-truncated past the per-line render limit."
  (render-bounded-lines (with-introspect-printer (prin1-to-string object))))

(defun introspection-provider (context)
  (require-capability-provider (active-protocol context)
                               :runtime/introspection
                               :contract :runtime/introspection/v1))

(defun run-list-objects-tool (tool parameters context &key call-id on-update)
  "Live-object ids, one per line; each round-trips to `inspect`."
  (declare (ignore tool parameters call-id on-update))
  (let* ((provider (introspection-provider context))
         (ids (mapcar (lambda (id) (provider-call provider :object-id-string id))
                      (provider-call provider :registry-object-ids
                                     (context-registry context))))
         (shown (if (> (length ids) *introspect-object-limit*)
                    (subseq ids 0 *introspect-object-limit*)
                    ids))
         (hidden (- (length ids) (length shown)))
         (truncated (plusp hidden))
         (protocol (and truncated (active-protocol context)))
         (entry (and protocol
                     (register-sequence-spill protocol ids
                                              :producer-uuid "list-objects")))
         (handle (and entry (spill-entry-token entry)))
         (element-count (and entry (spill-entry-element-count entry))))
    (make-tool-result
     :content (list (make-tool-text-content
                     (if ids
                         (render-bounded-lines
                          (format nil "~{~A~^~%~}~@[~%[+~D more object~:P]~]~@[~%~A~]"
                                  shown
                                  (when truncated hidden)
                                  (when truncated
                                    (if handle
                                        (format-spill-marker
                                         "live objects"
                                         :shown (length shown)
                                         :shown-unit "object ids"
                                         :total element-count
                                         :unit "object-id"
                                         :handle handle)
                                        (format-spill-marker
                                         "live objects"
                                         :shown (length shown)
                                         :shown-unit "object ids"
                                         :degraded t)))))
                         "No live objects.")))
     :details (append (list :objects shown :total (length ids)
                            :truncated truncated)
                      (when handle
                        (list :result-handle handle
                              :result-elements element-count))))))

(defun run-context-summary-tool (tool parameters context &key call-id on-update)
  "Summarize the active protocol, control plane, and live-object ids."
  (declare (ignore tool parameters call-id on-update))
  (let ((summary (provider-call (introspection-provider context)
                                :context-summary context)))
    (make-tool-result
     :content (list (make-tool-text-content (bounded-prin1 summary)))
     :details summary)))

(defun run-inspect-tool (tool parameters context &key call-id on-update)
  "Describe the live object named by :id, or report none."
  (declare (ignore tool call-id on-update))
  (let* ((id-string (or (tool-parameter parameters :id)
                        (error "Inspect tool requires :id.")))
         (description (provider-call (introspection-provider context)
                                     :describe-by-id
                                     (context-registry context)
                                     id-string)))
    (if description
        (make-tool-result
         :content (list (make-tool-text-content (bounded-prin1 description)))
         :details description)
        (make-tool-result
         :content (list (make-tool-text-content
                         (format nil "No live object with id ~A." id-string)))
         :details (list :id id-string :found nil)
         :error-p t))))
