(in-package #:kli/observability)

;;; Serialization

(defun jsonify-value (value)
  "Event-log coercion to a jzon-safe tree: hash-tables normalize, so parsed
arguments round-trip their entries with downcased keys."
  (kli/text:jsonify value :hash-tables :normalize))

(defun event->record (event)
  "Hash-table envelope for EVENT carrying timestamp, type, and the optional
source and payload, ready for jzon."
  (let ((record (make-hash-table :test #'equal))
        (source (event-source event))
        (payload (event-payload event)))
    (setf (gethash "timestamp" record) (event-timestamp event)
          (gethash "type" record) (string-downcase (symbol-name (event-type event))))
    (when source
      (setf (gethash "source" record) (jsonify-value source)))
    (when payload
      (setf (gethash "payload" record) (jsonify-value payload)))
    record))

(defun event->line (event)
  (com.inuoe.jzon:stringify (event->record event)))

;;; Sink

(defclass observability-sink (live-object)
  ((path :initform nil :accessor sink-path)
   (filter :initform nil :accessor sink-filter)
   (write-count :initform 0 :accessor sink-write-count)
   (last-error :initform nil :accessor sink-last-error)
   (mutex :initform (sb-thread:make-mutex :name "observability-sink")
          :reader sink-mutex
          :documentation "Serializes the file append and the write counter.
Events emit on whichever thread runs the action, so two threads can reach
write-event at once.")))

(defun make-observability-sink ()
  (make-instance 'observability-sink :id :observability-sink))

(defun sink-enabled-p (sink)
  (and (sink-path sink) t))

(defun prefix-match-p (prefix name)
  (and (<= (length prefix) (length name))
       (string= prefix name :end2 (length prefix))))

(defun event-passes-p (sink event)
  "True when the sink has no filter or some prefix matches the downcased
event-type name."
  (let ((filter (sink-filter sink)))
    (or (null filter)
        (let ((name (string-downcase (symbol-name (event-type event)))))
          (some (lambda (prefix) (prefix-match-p prefix name)) filter)))))

(defun write-event (sink event)
  "Append EVENT as one JSONL line. The line is serialized outside the sink
mutex, then the append and the counter update run under it so concurrent emits
never interleave bytes or lose counts. Guarded so a write failure records
itself on the sink and is dropped, never breaking the emitting action."
  (handler-case
      (let ((path (sink-path sink)))
        (when path
          (let ((line (format nil "~A~%" (event->line event))))
            (sb-thread:with-mutex ((sink-mutex sink))
              (ensure-directories-exist path)
              (with-open-file (out path :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :create
                                        :external-format :utf-8)
                (write-string line out)
                (force-output out))
              (incf (sink-write-count sink))))))
    (error (condition)
      (setf (sink-last-error sink) (princ-to-string condition))
      nil)))

;;; Settings

(defparameter +observability-settings-section+ "observability"
  "Settings section owning the sink configuration.")

(defparameter +observability-path-setting+ "path"
  "Settings key naming the JSONL sink file. A leading ~ expands against the
home directory and a relative path against the working directory. Absent or
empty leaves the sink disabled.")

(defparameter +observability-events-setting+ "events"
  "Settings key holding an array of event-type name prefixes to record.
Absent records every event.")

(defun normalize-filter (events)
  "List of downcased prefix strings from the settings array, or NIL for all
events. Non-string members are dropped."
  (when (and events (or (vectorp events) (listp events)))
    (let ((prefixes (loop for item across (coerce events 'vector)
                          when (stringp item)
                            collect (string-downcase item))))
      (and prefixes prefixes))))

(defun apply-observability (protocol contribution context)
  (declare (ignore protocol contribution))
  (let ((sink (find-live-object (context-registry context) :observability-sink))
        (service (find-config-service context)))
    (when sink
      (let* ((settings (and service (config-service-settings service)))
             (path (and settings (settings-value settings
                                                 +observability-settings-section+
                                                 +observability-path-setting+)))
             (events (and settings (settings-value settings
                                                   +observability-settings-section+
                                                   +observability-events-setting+)))
             (previous (list :path (sink-path sink) :filter (sink-filter sink))))
        (setf (sink-path sink) (and (stringp path) (plusp (length path))
                                    (expand-config-path path))
              (sink-filter sink) (normalize-filter events))
        previous))))

(defun revert-observability (protocol contribution context)
  (declare (ignore protocol))
  (let ((sink (find-live-object (context-registry context) :observability-sink))
        (state (contribution-state contribution)))
    (when sink
      (setf (sink-path sink) (getf state :path)
            (sink-filter sink) (getf state :filter)))))

;;; Command

(defun command-result (text)
  (make-command-result
   :content (list (make-command-text-content text))))

(defun run-observability-command (command arguments context &key call-id on-update)
  (declare (ignore command arguments call-id on-update))
  (let ((sink (find-live-object (context-registry context) :observability-sink)))
    (command-result
     (if (and sink (sink-enabled-p sink))
         (format nil "observability: enabled~%path: ~A~%filter: ~A~%events-written: ~D~@[~%last-error: ~A~]"
                 (namestring (sink-path sink))
                 (let ((filter (sink-filter sink)))
                   (if filter (format nil "~{~A~^, ~}" filter) "(all)"))
                 (sink-write-count sink)
                 (sink-last-error sink))
         (format nil "observability: disabled~@[~%last-error: ~A~]"
                 (and sink (sink-last-error sink)))))))

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun register-observability (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context))
        (source   (contribution-extension contribution)))
    (list
     (provider-call commands
                    :register-command
                    context
                    :observability
                    (make-command :name :observability
                                  :label "Observability"
                                  :description "Report the observability sink state."
                                  :runner #'run-observability-command)
                    :source source
                    :tier :core))))

(defun unregister-observability (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context)))
    (dolist (registration (contribution-state contribution))
      (provider-call commands
                     :unregister-command
                     context
                     registration))))

;;; Extension

(defextension observability
  (:requires
   (capability events   :contract events/v1)
   (capability config   :contract config/v1)
   (capability commands :contract commands/v1))
  (:provides
   (live-object observability-sink (make-observability-sink))
   (effect observability-settings
     #'apply-observability
     #'revert-observability)
   (effect observability-commands
     #'register-observability
     #'unregister-observability)
   (method kli/event:dispatch-event
       (:after)
       (kli/ext:extension-protocol kli/event:event t)
       (protocol event context)
     "Pure stream sink. Serializes every dispatched event to the JSONL file when the sink is enabled and the event passes the filter. Specializes on the event class so its key stays distinct from any other after-method on this generic function."
     (declare (ignore protocol))
     (let ((sink (find-live-object (context-registry context)
                                   :observability-sink)))
       (when (and sink (sink-enabled-p sink) (event-passes-p sink event))
         (write-event sink event))))))
