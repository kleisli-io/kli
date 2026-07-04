(in-package #:kli/tests)

(defvar *test-fake-model-transient-errors* 0
  "Decremented by the fake adapter on each call until zero. The failure is a
model-network-error so the default retry classifier sees it as transient.")

(defun test-fake-provider-metadata-value (provider key &optional default)
  (getf (models:model-provider-metadata provider) key default))

(defun test-fake-provider-text-deltas (provider)
  (or (copy-list (test-fake-provider-metadata-value provider :fake-deltas))
      (let ((text (test-fake-provider-metadata-value provider
                                                     :fake-response
                                                     "fake response")))
        (list text))))

(defun test-fake-provider-tool-call (provider)
  (test-fake-provider-metadata-value provider :fake-tool-call))

(defun test-fake-provider-tool-calls (provider)
  (or (copy-list (test-fake-provider-metadata-value provider :fake-tool-calls))
      (let ((tool-call (test-fake-provider-tool-call provider)))
        (when tool-call
          (list tool-call)))))

(defun test-fake-provider-tool-call-delta (tool-call &key content-index)
  (rt:make-tool-call-delta
   (getf tool-call :name)
   (copy-list (getf tool-call :arguments))
   :call-id (getf tool-call :id)
   :content-index content-index))

(defun test-fake-provider-blocks (provider)
  (copy-list (test-fake-provider-metadata-value provider :fake-blocks)))

(defun emit-fake-block-deltas (emit index block)
  (let ((kind (getf block :kind)))
    (funcall emit (rt:make-block-start-delta kind :content-index index))
    (ecase kind
      (:text
       (dolist (text (getf block :text))
         (funcall emit (rt:make-assistant-delta text :content-index index))))
      (:thinking
       (dolist (text (getf block :text))
         (funcall emit (rt:make-thinking-delta text :content-index index))))
      (:toolcall
       (funcall emit (test-fake-provider-tool-call-delta
                      (getf block :tool-call)
                      :content-index index))))
    (funcall emit (rt:make-block-end-delta kind :content-index index))))

(defun emit-fake-flat-deltas (emit provider)
  (dolist (text (test-fake-provider-text-deltas provider))
    (funcall emit (rt:make-assistant-delta text)))
  (loop for tool-call in (test-fake-provider-tool-calls provider)
        for index from 0
        do (funcall emit (test-fake-provider-tool-call-delta tool-call
                                                             :content-index index))))

(defun test-fake-model-stream-adapter (provider request context &key emit)
  (declare (ignore request context))
  (when (plusp *test-fake-model-transient-errors*)
    (decf *test-fake-model-transient-errors*)
    (error 'transports:model-network-error
           :cause (make-condition 'simple-error
                                  :format-control
                                  "test-fake-model transient failure")))
  (let ((blocks (test-fake-provider-blocks provider))
        (usage (test-fake-provider-metadata-value provider :fake-usage)))
    (if blocks
        (loop for block in blocks
              for index from 0
              do (emit-fake-block-deltas emit index block))
        (emit-fake-flat-deltas emit provider))
    (when usage
      (funcall emit (rt:make-usage-delta usage)))))

(defun install-test-fake-model-provider (protocol contribution context)
  (declare (ignore protocol contribution))
  (rt:register-model-stream-adapter (kli:find-live-object
                                     (kli:context-registry context)
                                     :model-runtime-service)
                                    :fake
                                    #'test-fake-model-stream-adapter
                                    context))

(defun retract-test-fake-model-provider (protocol contribution context)
  (declare (ignore protocol contribution))
  (rt:unregister-model-stream-adapter (kli:find-live-object
                                       (kli:context-registry context)
                                       :model-runtime-service)
                                      :fake
                                      context))

(ext:defextension test-fake-model-provider
  (:requires
   (:capability :model/runtime :contract :model/runtime/v1))
  (:provides
   (effect test-fake-model-provider
     #'install-test-fake-model-provider
     #'retract-test-fake-model-provider)))
