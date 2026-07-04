(in-package #:kli/context/lens)

(defvar *agent-context-counter* (make-id-counter))
(defvar *context-patch-counter* (make-id-counter))
(defvar *sealed-context-counter* (make-id-counter))

(defun next-agent-context-id ()
  (next-keyword-id "AGENT-CONTEXT" '*agent-context-counter*))

(defun next-context-patch-id ()
  (next-keyword-id "CONTEXT-PATCH" '*context-patch-counter*))

(defun next-sealed-context-id ()
  (next-keyword-id "SEALED-CONTEXT" '*sealed-context-counter*))

(defclass agent-context (live-object)
  ((session
    :initarg :session
    :reader agent-context-session)
   (store
    :initarg :store
    :reader agent-context-store)
   (log-provider
    :initarg :log-provider
    :initform nil
    :accessor agent-context-log-provider)
   (entries-provider
    :initarg :entries-provider
    :initform nil
    :accessor agent-context-entries-provider)
   (leaf-id
    :initarg :leaf-id
    :initform nil
    :accessor agent-context-leaf-id)
   (projection
    :initarg :projection
    :accessor agent-context-projection)
   (capsule
    :initarg :capsule
    :accessor agent-context-capsule)
   (committed-patches
    :initform '()
    :accessor context-committed-patches)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor agent-context-metadata)))

(defclass context-projection ()
  ((messages
    :initarg :messages
    :initform '()
    :accessor projection-messages)
   (entries
    :initarg :entries
    :initform '()
    :accessor projection-entries)
   (leaf-id
    :initarg :leaf-id
    :initform nil
    :accessor projection-leaf-id)
   (epoch
    :initarg :epoch
    :initform 0
    :accessor projection-epoch)))

(defclass context-patch (live-object)
  ((kind
    :initarg :kind
    :reader context-patch-kind)
   (payload
    :initarg :payload
    :initform nil
    :reader context-patch-payload)
   (actor
    :initarg :actor
    :initform nil
    :reader context-patch-actor)
   (timestamp
    :initarg :timestamp
    :initform nil
    :reader context-patch-timestamp)
   (metadata
    :initarg :metadata
    :initform '()
    :reader context-patch-metadata)))

(defclass context-patch-set ()
  ((patches
    :initarg :patches
    :initform '()
    :reader context-patch-set-patches)
   (actor
    :initarg :actor
    :initform nil
    :reader context-patch-actor)
   (timestamp
    :initarg :timestamp
    :initform nil
    :reader context-patch-timestamp)
   (base-epoch
    :initarg :base-epoch
    :initform nil
    :reader context-patch-set-base-epoch)
   (result-epoch
    :initarg :result-epoch
    :initform nil
    :reader context-patch-set-result-epoch)))

(defclass sealed-context (live-object)
  ((messages
    :initarg :messages
    :initform '()
    :reader sealed-context-messages)
   (epoch
    :initarg :epoch
    :reader sealed-context-epoch)
   (source-context-id
    :initarg :source-context-id
    :reader sealed-context-source-context-id)
   (leaf-id
    :initarg :leaf-id
    :initform nil
    :reader sealed-context-leaf-id)
   (timestamp
    :initarg :timestamp
    :initform nil
    :reader sealed-context-timestamp)))

(defclass context-capability ()
  ((name
    :initarg :name
    :reader context-capability-name)))

(defun context-timestamp (&optional timestamp)
  (or timestamp (get-universal-time)))

(defun make-context-patch (kind &key id payload actor timestamp metadata)
  (make-instance 'context-patch
                 :id (or id (next-context-patch-id))
                 :kind kind
                 :payload payload
                 :actor actor
                 :timestamp (context-timestamp timestamp)
                 :metadata metadata))

(defun make-append-message-patch (message &key id actor timestamp metadata)
  (make-context-patch :append-message
                      :id id
                      :payload (list :message message)
                      :actor actor
                      :timestamp timestamp
                      :metadata metadata))

(defun plist-with-non-nil (&rest pairs)
  "Build a plist from KEY VALUE pairs, dropping any pair whose value is NIL."
  (loop for (key value) on pairs by #'cddr
        when value
          collect key and collect value))

(defun make-remove-message-patch (&key id actor timestamp metadata message-id index)
  (make-context-patch :remove-message
                      :id id
                      :payload (plist-with-non-nil :message-id message-id
                                                   :index index)
                      :actor actor
                      :timestamp timestamp
                      :metadata metadata))

(defun make-replace-message-patch (message &key id actor timestamp metadata
                                             message-id index)
  (make-context-patch :replace-message
                      :id id
                      :payload (plist-with-non-nil :message message
                                                   :message-id message-id
                                                   :index index)
                      :actor actor
                      :timestamp timestamp
                      :metadata metadata))
