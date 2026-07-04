(defpackage #:kli/output-spill
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:protocol-storage
                #:protocol-run-identity
                #:defextension
                #:tool-parameter
                #:required-tool-parameter
                #:tool-text-result)
  (:export
   #:*output-spill-enabled*
   #:*output-spill-directory*
   #:*output-spill-session-budget*
   #:*output-spill-sweep-ttl-seconds*
   #:*output-spill-page-lines*
   #:*output-spill-search-matches*

   #:output-spill-base-directory
   #:output-spill-run-directory
   #:run-dir-file
   #:ensure-run-directory
   #:directory-hardening-status
   #:ensure-hardened-directory

   #:spill-registry
   #:ensure-spill-registry
   #:spill-registry-counter
   #:spill-registry-entries
   #:spill-registry-total-bytes
   #:spill-entry
   #:spill-entry-token
   #:spill-entry-kind
   #:spill-entry-path
   #:spill-entry-elements
   #:spill-entry-bytes
   #:spill-entry-element-count
   #:spill-entry-producer-uuid
   #:spill-entry-owned
   #:find-spill-entry

   #:mint-spill-token

   #:adopt-file-spill
   #:write-string-spill
   #:register-sequence-spill
   #:register-file-handle
   #:spill-tee-stream
   #:open-spill-tee
   #:finalize-spill-tee
   #:discard-spill-tee
   #:tee-window
   #:tee-truncated-p

   #:read-lines-from-offset
   #:byte-offset-of-line
   #:page-lines
   #:search-within
   #:read-sequence-window
   #:search-sequence

   #:format-spill-marker

   #:reap-spills
   #:install-output-spill-reap
   #:retract-output-spill-reap
   #:sweep-stale-spills

   #:*output-spill-extension-manifest*))

(in-package #:kli/output-spill)
