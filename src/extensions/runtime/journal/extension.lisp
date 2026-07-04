(in-package #:kli/runtime/journal)

(defclass journal-service (live-object)
  ((entries
    :initform '()
    :accessor journal-entry-list)))

(defun make-journal-service (&key (id :journal-service))
  (make-instance 'journal-service
                 :id id))

(defun make-journal-entry (&key type payload source)
  (list :type type
        :payload payload
        :source source))

(defun record-journal-entry (service type &key payload source)
  (let ((entry (make-journal-entry :type type
                                   :payload payload
                                   :source source)))
    (push entry (journal-entry-list service))
    entry))

(defun journal-entries (service)
  (reverse (copy-list (journal-entry-list service))))

(defun make-journal-contract ()
  (make-provider-contract
   :id :runtime/journal/v1
   :capability :runtime/journal
   :required-entries
   '(:make-journal-entry
     :record-journal-entry
     :journal-entries)))

(defun make-journal-provider ()
  (make-provider
   :id :runtime-journal-provider
   :capability :runtime/journal
   :contracts '(:runtime/journal/v1)
   :entries
   (list :make-journal-entry #'make-journal-entry
         :record-journal-entry #'record-journal-entry
         :journal-entries #'journal-entries)))

(defextension journal
  (:provides
   (contract runtime/journal/v1
     (make-journal-contract))
   (capability runtime/journal (make-journal-provider))
   (live-object journal-service
     (make-journal-service))))
