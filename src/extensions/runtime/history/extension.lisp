(in-package #:kli/runtime/history)

(defclass history-service (live-object)
  ((entries
    :initform '()
    :accessor history-entry-list)))

(define-capability-binding runtime-control
  :capability :runtime/control
  :contract :runtime/control/v1)

(defun make-history-service (&key (id :history-service))
  (make-instance 'history-service
                 :id id))

(defun protocol-id-or-nil (protocol)
  (and protocol
       (object-id protocol)))

(defun make-history-entry (type &key from to)
  (list :type type
        :from from
        :to to))

(defun record-history-entry (service type &key from to)
  (let ((entry (make-history-entry type
                                   :from from
                                   :to to)))
    (push entry (history-entry-list service))
    entry))

(defun history-entries (service)
  (reverse (copy-list (history-entry-list service))))

(defun history-switch-protocol (service control protocol-id context)
  (let ((from (protocol-id-or-nil (active-protocol context))))
    (let ((result (runtime-control-call (object-protocol service)
                                        :control-switch-protocol
                                        control
                                        protocol-id
                                        context)))
      (record-history-entry service
                            :switch-protocol
                            :from from
                            :to (protocol-id-or-nil result))
      result)))

(defun history-recover-protocol (service control context)
  (let ((from (protocol-id-or-nil (active-protocol context))))
    (let ((result (runtime-control-call (object-protocol service)
                                        :control-recover-protocol
                                        control
                                        context)))
      (record-history-entry service
                            :recover-protocol
                            :from from
                            :to (protocol-id-or-nil result))
      result)))

(defun make-history-contract ()
  (make-provider-contract
   :id :runtime/history/v1
   :capability :runtime/history
   :required-entries
   '(:history-switch-protocol
     :history-recover-protocol
     :record-history-entry
     :history-entries)))

(defun make-history-provider ()
  (make-provider
   :id :runtime-history-provider
   :capability :runtime/history
   :contracts '(:runtime/history/v1)
   :entries
   (list :history-switch-protocol #'history-switch-protocol
         :history-recover-protocol #'history-recover-protocol
         :record-history-entry #'record-history-entry
         :history-entries #'history-entries)))

(defextension history
  (:requires
   (capability runtime/control :contract runtime/control/v1))
  (:provides
   (contract runtime/history/v1
     (make-history-contract))
   (capability runtime/history (make-history-provider))
   (live-object history-service
     (make-history-service))))
