(in-package #:kli/context/lens)

(define-record-deserializer :context-patch (record)
  (advance-keyword-counter '*context-patch-counter* (record-field record :id))
  (make-instance 'context-patch
                 :id (record-field record :id)
                 :kind (record-field record :kind)
                 :payload (deserialize-value (record-field record :payload))
                 :actor (deserialize-value (record-field record :actor))
                 :timestamp (record-field record :timestamp)
                 :metadata (deserialize-value (record-field record :metadata))))

(define-record-deserializer :context-patch-set (record)
  (make-instance 'context-patch-set
                 :patches (deserialize-value (record-field record :patches))
                 :actor (deserialize-value (record-field record :actor))
                 :timestamp (record-field record :timestamp)
                 :base-epoch (record-field record :base-epoch)
                 :result-epoch (record-field record :result-epoch)))

(define-record-deserializer :subject (record)
  (kli/ext:make-subject
   :grant (kli/ext:datum->grant (deserialize-value (record-field record :grant)))))
