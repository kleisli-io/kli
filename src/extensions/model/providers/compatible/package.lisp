(defpackage #:kli/model/providers/compatible
  (:use #:cl)
  (:import-from #:kli/ext
                #:contribution-kind
                #:contribution-name
                #:contribution-state
                #:defextension
                #:protocol-installed-contributions)
  (:import-from #:kli/model/registry
                #:make-provider-config
                #:make-model-option-schema
                #:model-option-schema-option-id
                #:normalize-option-id)
  (:import-from #:kli/model/providers/common
                #:install-provider-catalogue
                #:retract-provider-catalogue)
  (:export
   #:*compatible-provider-extension-manifest*
   #:*providers-path*
   #:providers-path
   #:parse-provider-configs
   #:load-provider-configs
   #:refresh-compatible-providers))

(in-package #:kli/model/providers/compatible)
