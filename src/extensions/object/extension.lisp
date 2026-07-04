(in-package #:kli/object)

(defun make-standard-object-contract ()
  (make-provider-contract
   :id :standard-object/v1
   :capability :standard-object
   :required-entries
   '(:make-standard-live-object)))

(defun make-standard-object-provider ()
  (make-provider
   :id :standard-object-provider
   :capability :standard-object
   :contracts '(:standard-object/v1)
   :entries
   (list :make-standard-live-object #'make-standard-live-object)))

(defextension standard-object
  (:provides
   (contract standard-object/v1
     (make-standard-object-contract))
   (capability standard-object (make-standard-object-provider))))
