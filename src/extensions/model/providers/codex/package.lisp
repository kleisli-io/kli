(defpackage #:kli/model/providers/codex
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
   #:*codex-provider-extension-manifest*))

(in-package #:kli/model/providers/codex)
