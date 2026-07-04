(defpackage #:kli/model/providers/anthropic
  (:use #:cl)
  (:import-from #:kli/ext
                #:defextension
                #:contribution-state)
  (:import-from #:kli/model/registry
                #:make-provider-config
                #:make-model-option-schema)
  (:import-from #:kli/model/providers/common
                #:install-provider-catalogue
                #:retract-provider-catalogue)
  (:export
   #:*anthropic-provider-extension-manifest*))

(in-package #:kli/model/providers/anthropic)
