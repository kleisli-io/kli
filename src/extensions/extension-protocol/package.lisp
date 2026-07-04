(defpackage #:kli/ext
  (:use #:cl)
  (:import-from #:kli
                #:live-object
                #:object-id
                #:object-protocol
                #:protocol
                #:active-protocol
                #:register-live-object
                #:find-live-object
                #:remove-live-object
                #:context-registry
                #:smoke-test-protocol)
  (:import-from #:let-over-lambda
                #:defmacro!
                #:symb)
  (:import-from #:kli/text
                #:*render-line-limit*)
  (:export
   #:normalize-extension-id

   #:extension-protocol
   #:make-extension-protocol
   #:protocol-extensions
   #:protocol-installed-contributions
   #:protocol-capabilities
   #:protocol-provider-contracts
   #:protocol-tools
   #:extension-factories
   #:protocol-storage
   #:protocol-storage-table
   #:ensure-protocol-storage
   #:protocol-run-identity
   #:snapshot-exempt-storage-key-p

   #:make-layer-stack
   #:layer-stack-labels
   #:layer-stack-member-p
   #:install-layer
   #:remove-layer-by-label
   #:compose-layers
   #:protocol-root-activations
   #:protocol-load-extension
   #:protocol-load-extension-source
   #:load-extension-source

   #:define-contribution-form-compiler
   #:defcontribution-kind
   #:unknown-contribution-kind
   #:unknown-contribution-kind-kind
   #:unknown-contribution-kind-form

   #:extension
   #:make-extension
   #:extension-source
   #:extension-version
   #:extension-metadata
   #:extension-requirement-list
   #:extension-contribution-list
   #:extension-contributions
   #:define-extension
   #:define-extension-in
   #:defextension
   #:*defining-protocol*
   #:*image-dump-in-progress*
   #:find-extension-definition
   #:make-defined-extension
   #:activate-extension
   #:deactivate-extension
   #:recode-extension
   #:extension-loaded-p
   #:call-with-manifest-capture
   #:load-extension-manifest
   #:install-manifest
   #:retract-manifest
   #:install-manifest-list
   #:retract-installed-extensions

   #:*author-clause-requirements*
   #:register-author-clause-requirements
   #:derive-requirement-specs
   #:derive-requirements-from-contributions
   #:spec->requirement
   #:extension-builder
   #:make-extension-builder
   #:extension-builder-id
   #:extension-builder-contributions
   #:extension-builder-requirements
   #:extension-builder-metadata
   #:builder-add-contribution
   #:build-extension
   #:kli-extension

   #:requirement
   #:make-requirement
   #:requirement-kind
   #:requirement-name
   #:requirement-contract
   #:requirement-provider-id
   #:requirement-source
   #:requirement-satisfied-p

   #:contribution
   #:contribution-kind
   #:contribution-name
   #:contribution-source
   #:contribution-extension
   #:install-contribution
   #:retract-contribution
   #:check-contribution-precondition

   #:live-object-contribution
   #:make-live-object-contribution
   #:contribution-object

   #:capability-contribution
   #:make-capability-contribution
   #:contribution-provider

   #:provider-contract-contribution
   #:make-provider-contract-contribution
   #:contribution-contract

   #:effect-contribution
   #:make-effect-contribution
   #:contribution-installer
   #:contribution-retractor
   #:contribution-state

   #:tool-contribution
   #:make-tool-contribution
   #:contribution-tool

   #:resource
   #:make-resource
   #:resource-uri
   #:resource-name
   #:resource-description
   #:resource-mime-type
   #:resource-reader
   #:resource-contribution
   #:make-resource-contribution
   #:contribution-resource
   #:protocol-resources
   #:find-resource
   #:list-resources
   #:read-resource

   #:trace-contribution
   #:make-trace-contribution
   #:resolve-trace-symbol
   #:protocol-trace-buffer
   #:protocol-trace-evicted
   #:*trace-buffer-cap*

   #:method-contribution
   #:make-method-contribution
   #:contribution-gf-name
   #:contribution-qualifiers
   #:contribution-specializer-names
   #:contribution-lambda-list
   #:contribution-body

   #:provider
   #:make-provider
   #:provider-capability
   #:provider-contract-list
   #:provider-entries

   #:provider-contract
   #:make-provider-contract
   #:contract-capability
   #:contract-required-entries
   #:contract-validator
   #:find-provider-contract
   #:provider-satisfies-contract-p

   #:tool
   #:make-tool
   #:tool-name
   #:tool-label
   #:tool-description
   #:tool-parameters
   #:tool-runner
   #:tool-renderer
   #:tool-metadata
   #:tool-result
   #:make-tool-result
   #:make-tool-text-content
   #:tool-error-result
   #:tool-result-content
   #:tool-result-details
   #:tool-result-error-p
   #:tool-parameter
   #:required-tool-parameter
   #:tool-text-result
   #:find-tool
   #:list-tools
   #:list-tool-contributions
   #:list-tools-from
   #:invoke-tool
   #:*tool-abort-predicate*
   #:tool-abort-requested-p
   #:recode-tool

   #:*call-header-scalar-limit*
   #:call-hidden
   #:call-command
   #:call-header
   #:result-summary
   #:result-diff
   #:result-listing
   #:result-filesystem-summary
   #:result-box
   #:presentation-kind
   #:tool-argument-ref
   #:tool-argument-value-string
   #:tool-argument-scalar-p
   #:tool-argument-pairs
   #:present-default-call-text
   #:make-tool-presenter
   #:default-call-term
   #:present-call
   #:present-result
   #:hidden-call-presenter
   #:command-call-presenter
   #:summary-result-presenter
   #:diff-result-presenter
   #:listing-result-presenter
   #:find-summary-result-presenter
   #:search-summary-result-presenter

   #:find-capabilities
   #:find-capability-providers
   #:find-capability-provider
   #:require-capability-provider
   #:capability-provided-p
   #:provider-ref
   #:provider-call

   #:define-capability-binding

   #:subject
   #:system-subject
   #:subject-capabilities
   #:subject-grant
   #:make-subject
   #:make-system-subject
   #:make-default-subject
   #:make-unrestricted-subject
   #:subject-meet
   #:with-system-authority
   #:capabilities-subject
   #:*call-subject*
   #:*ui-subject*
   #:*install-subject*
   #:*capability-implications*
   #:expand-implications
   #:check-capability
   #:require-capability
   #:capability-denied
   #:capability-denied-subject
   #:capability-denied-capability

   #:grant
   #:grant-p
   #:make-grant
   #:grant-top
   #:grant-bottom
   #:grant-covers-p
   #:grant-meet
   #:grant-without-atoms
   #:grant-equiv-p
   #:grant-capabilities
   #:grant-excluded
   #:grant-constraint
   #:constraint-any
   #:constraint-none
   #:path-prefix-constraint
   #:numeric-bound-constraint
   #:enumerated-constraint
   #:constraint-covers-p
   #:constraint-meet
   #:grant->datum
   #:datum->grant
   #:constraint->string
   #:grant-report
   #:confer
   #:grant-escalation
   #:grant-escalation-holder
   #:grant-escalation-requested

   #:grant-contribution
   #:make-grant-contribution
   #:contribution-principal
   #:contribution-grant
   #:check-authority
   #:principal-subject
   #:protocol-grant-set
   #:grant-set-lookup
   #:grant-set-has-p
   #:delegate-grant
   #:delegation-owner-id
   #:lifted-tool-atom
   #:lifted-server-grant
   #:coordinate-request
   #:register-coordinate-deriver

   #:condition-category
   #:condition-http-status

   #:with-extension-fault-barrier
   #:safely-invoke
   #:with-boot-stage
   #:boot-timing-enabled-p
   #:boot-marker
   #:note-fault
   #:fault-log-path
   #:skip-unit
   #:*extension-fault-policy*
   #:*fault-reify-hook*
   #:*fault-note-hook*
   #:register-fault-note-emitter
   #:deregister-fault-note-emitter
   #:*in-fault-barrier*
   #:with-supervised-dispatch
   #:*dispatch-chain*
   #:dispatch-chain-label))

(in-package #:kli/ext)
