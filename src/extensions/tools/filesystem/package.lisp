(defpackage #:kli/tools/filesystem
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:defextension
                #:ensure-protocol-storage
                #:path-prefix-constraint
                #:register-coordinate-deriver
                #:tool-abort-requested-p
                #:tool-parameter
                #:required-tool-parameter
                #:result-diff
                #:tool-text-result)
  (:import-from #:kli/text
                #:*render-line-limit*
                #:render-truncate-front)
  (:import-from #:kli/output-spill
                #:open-spill-tee
                #:finalize-spill-tee
                #:discard-spill-tee
                #:spill-entry-token
                #:spill-entry-bytes
                #:page-lines
                #:search-within
                #:read-lines-from-offset
                #:format-spill-marker)
  (:import-from #:cl-ppcre
                #:create-scanner
                #:scan)
  (:import-from #:difflib
                #:unified-diff
                #:sequence-matcher
                #:set-sequences
                #:get-opcodes
                #:opcode-tag
                #:opcode-i1
                #:opcode-i2
                #:opcode-j1
                #:opcode-j2)
  (:import-from #:paren-repair
                #:repair
                #:delimiter-imbalanced-p
                #:source-syntax-valid-p
                #:source-syntax-diagnostics
                #:source-form-count
                #:source-position
                #:cst-diagnostic-start
                #:cst-diagnostic-message)
  (:export
   #:run-read-tool
   #:run-write-tool
   #:run-edit-tool
   #:run-find-tool
   #:run-search-tool
   #:*file-byte-limit*
   #:*read-line-limit*
   #:*search-output-limit*
   #:*search-scan-timeout-seconds*
   #:line-hash
   #:render-anchored-row
   #:split-file-lines
   #:compact-file-change-detail
   #:file-diff-presentation-update
   #:clamp-range
   #:remember-read
   #:anchor-known-p
   #:parse-hashline-patch
   #:patch-op-kind
   #:patch-op-start-line
   #:patch-op-start-hash
   #:patch-op-end-line
   #:patch-op-end-hash
   #:patch-op-payload
   #:*read-tool-extension-manifest*
   #:*write-tool-extension-manifest*
   #:*edit-tool-extension-manifest*
   #:*find-tool-extension-manifest*
   #:*search-tool-extension-manifest*
   #:*filesystem-anchor-lifecycle-extension-manifest*))
