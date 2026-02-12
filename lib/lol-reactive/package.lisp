;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; LOL-REACTIVE - A production-ready reactive web framework
;;;; Using Let Over Lambda patterns for fine-grained reactivity

(defpackage :lol-reactive
  (:use :cl :iterate)
  (:import-from :let-over-lambda
                :pandoriclet
                :with-pandoric
                :dlambda
                :defmacro!
                :aif
                :alambda
                :symb
                :mkstr)
  (:export
   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; CSS Infrastructure
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Registry (Let Over Lambda pattern - modules are closures)
   #:*component-css-registry*
   #:*css-load-order*
   #:make-css-module
   #:get-css-module
   #:get-component-css
   #:generate-all-component-css
   #:defcss
   #:clear-css-registry
   #:list-registered-css-components
   #:inspect-css-registry

   ;; Generation
   #:css-rule
   #:css-rules
   #:css-section
   #:css-keyframes
   #:css-media
   #:css-var
   #:css-var-definition

   ;; Tokens (Let Over Lambda pattern - token sets are closures)
   #:make-token-set
   #:*colors*
   #:*typography*
   #:*spacing*
   #:*effects*
   #:*default-colors*
   #:*default-typography*
   #:*default-spacing*
   #:*default-effects*
   #:get-color
   #:get-font
   #:get-spacing
   #:get-effect
   #:validate-token
   #:levenshtein-distance
   #:find-closest-match

   ;; Tailwind helpers
   #:tw-color
   #:tw-spacing
   #:tw-bg
   #:tw-text
   #:tw-border
   #:tw-arbitrary
   #:tw-bg-value
   #:tw-text-value
   #:tw-border-value
   #:classes
   #:null-or-empty-p

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Core Reactive Primitives
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Signals - Fine-grained reactivity
   #:*current-effect*
   #:make-signal
   #:make-effect
   #:make-computed
   #:batch

   ;; Pandoric signals - Introspectable reactive values
   #:make-pandoric-signal

   ;; Stores - Redux-like state management
   #:make-store

   ;; Evolving components - Hot-swap behavior
   #:make-evolving-component

   ;; Component factory - Dynamic creation
   #:make-component-factory
   #:*factory-registry*

   ;; Reactive collections
   #:make-reactive-list

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Component System
   ;;; ═══════════════════════════════════════════════════════════════════════

   #:defcomponent
   #:make-component
   #:component-render
   #:component-state
   #:component-dispatch
   #:with-component-state

   ;; Component registry
   #:register-component
   #:find-component
   #:*components*

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; HTML Generation
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Elements (cl-who shorthand)
   #:htm
   #:htm-str
   #:render-component
   #:component->html
   #:highlight-sexp

   ;; Page template
   #:html-page
   #:render-html
   #:generate-css-variables
   #:tailwind-config
   #:reactive-runtime-js

   ;; Escape (XSS prevention)
   #:escape-html
   #:safe-str
   #:safe-fmt

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Server Infrastructure
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Clack request/response abstraction
   #:*env*
   #:*response-headers*
   #:*path-params*
   #:path-param
   #:request-path
   #:request-method
   #:request-query-string
   #:request-header
   #:request-content-type
   #:request-content-length
   #:request-body
   #:request-body-json
   #:query-param
   #:query-params
   #:post-param
   #:post-params
   #:param
   #:response
   #:html-response
   #:json-response
   #:text-response
   #:redirect-response
   #:error-response
   #:with-response-headers
   #:add-response-header
   #:get-response-headers
   #:http-status-text
   ;; Session (Lack middleware)
   #:session-get
   #:session-set
   #:session-delete
   #:session-clear
   #:session-keys
   ;; CSRF (Lack middleware)
   #:csrf-token
   #:csrf-input

   ;; Routes and Application
   #:*routes*
   #:clear-routes
   #:list-routes
   #:route-handler
   #:make-app
   #:defroute
   #:defapi
   #:*server*
   #:start-server
   #:stop-server
   #:json-body
   ;; HTMX helpers
   #:htmx-request-p
   #:htmx-boosted-p
   #:htmx-history-restore-request-p
   #:htmx-target
   #:htmx-trigger
   #:htmx-trigger-name
   #:htmx-current-url
   #:htmx-prompt
   #:with-htmx-response

   ;; Security
   #:add-security-headers
   #:add-csp-header
   #:with-security
   #:sanitize-html
   #:sanitize-attribute
   #:sanitize-url
   #:generate-csrf-token
   #:get-csrf-token
   #:validate-csrf-token
   #:csrf-token-input
   #:with-csrf-validation
   #:check-rate-limit
   #:get-client-ip
   #:with-rate-limit
   #:*rate-limit-store*

   ;; Error handling
   #:*debug-mode*
   #:*error-log-path*
   #:with-error-handling
   #:log-error
   #:render-error-page
   #:render-404-page
   #:render-500-page
   #:enable-debug-mode
   #:disable-debug-mode

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Development Tools (Surgery)
   ;;; ═══════════════════════════════════════════════════════════════════════

   #:capture-snapshot
   #:restore-snapshot
   #:list-snapshots
   #:clear-snapshots
   #:component-state-tree
   #:surgery-get-state
   #:surgery-set-state
   #:surgery-eval-in-context
   #:surgery-dispatch
   #:xray-wrapper-html
   #:surgery-panel-html
   #:enable-surgery-mode
   #:disable-surgery-mode
   #:surgery-mode-p
   #:surgery-runtime-js
   #:surgery-css
   #:push-undo
   #:surgery-undo
   #:surgery-redo
   #:register-component-metadata

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Parenscript Utilities
   ;;; ═══════════════════════════════════════════════════════════════════════

   #:reactive-script
   #:on-click
   #:on-change
   #:bind-state

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; HTMX-Style Runtime
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Runtime generation
   #:htmx-runtime-js
   #:htmx-indicator-css

   ;; Autocomplete support
   #:render-autocomplete
   #:render-autocomplete-results
   #:autocomplete-css

   ;; Attribute helpers
   #:hx-get
   #:hx-post
   #:hx-put
   #:hx-delete

   ;; OOB response helpers
   #:oob-swap
   #:oob-content              ; innerHTML-only swap, preserves element attributes
   #:with-oob-swaps

   ;; Server-side helpers
   #:htmx-request-p
   #:htmx-target
   #:htmx-trigger
   #:htmx-current-url
   #:with-htmx-response
   #:set-htmx-trigger
   #:set-htmx-redirect
   #:set-htmx-location
   #:render-with-oob
   #:render-oob-only
   #:htmx-or-redirect
   #:htmx-or-full-page

   ;; Idiomorph/morph integration
   #:*idiomorph-version*
   #:idiomorph-js-path
   #:include-idiomorph
   #:htmx-morph-extension-js
   #:htmx-runtime-with-morph-js
   #:include-htmx-with-morph
   #:hx-morph

   ;; Client runtimes
   #:ws-client-js
   #:sse-client-js
   #:optimistic-js
   #:lol-reactive-runtime-js

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; WebSocket Server Support
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Connection management
   #:*ws-connections*
   #:ws-connection-count
   #:ws-channels

   ;; Handler creation
   #:make-ws-handler
   #:defws

   ;; Message sending
   #:ws-send
   #:ws-send-text
   #:ws-send-binary
   #:ws-send-json
   #:ws-close

   ;; Broadcasting
   #:ws-broadcast
   #:ws-broadcast-json
   #:ws-broadcast-all

   ;; HTMX integration
   #:ws-broadcast-html
   #:ws-broadcast-oob
   #:ws-broadcast-trigger

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; SSE (Server-Sent Events) Support
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Connection management
   #:*sse-connections*
   #:sse-connection-count
   #:sse-channels

   ;; Handler creation
   #:make-sse-handler
   #:defsse

   ;; Message formatting
   #:format-sse-event

   ;; Message sending
   #:sse-send
   #:sse-send-comment
   #:sse-ping-all

   ;; Broadcasting
   #:sse-broadcast
   #:sse-broadcast-all

   ;; HTMX integration
   #:sse-broadcast-html
   #:sse-broadcast-oob
   #:sse-broadcast-trigger

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Continuation-Based Wizards
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Wizard definition and registry
   #:defwizard
   #:register-wizard
   #:get-wizard-spec
   #:list-wizards
   #:inspect-wizard

   ;; Wizard session management
   #:start-wizard
   #:get-wizard-session
   #:remove-wizard-session
   #:cleanup-stale-sessions
   #:list-active-wizard-sessions

   ;; Wizard processing
   #:process-wizard-submission
   #:render-wizard-step
   #:render-wizard-complete

   ;; Wizard form helpers
   #:wizard-text-field
   #:wizard-select-field
   #:wizard-radio-group

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Component Composition (Props + Children)
   ;;; ═══════════════════════════════════════════════════════════════════════

   #:defcomponent-with-props
   #:with-props
   #:validate-props

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Context API
   ;;; ═══════════════════════════════════════════════════════════════════════

   #:defcontext
   #:defcontext-signal
   #:list-contexts
   #:get-context-info
   #:inspect-context
   #:inspect-all-contexts

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Fullstack (Isomorphic Components)
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Isomorphic components (server-render + client-hydrate)
   #:defisomorphic-component
   #:render-isomorphic
   #:isomorphic-page
   #:hydration-runtime-js
   #:include-hydration-runtime
   #:client-action-attr
   #:serialize-state
   #:deserialize-state

   ;; Component API (auto-generated REST endpoints)
   #:defcomponent-with-api
   #:register-api-component
   #:find-api-component
   #:list-api-components
   #:list-api-routes
   #:inspect-api-component
   #:generate-api-client-js
   #:api-client-script-tag

   ;;; ═══════════════════════════════════════════════════════════════════════
   ;;; Optimization (Compile-Time Analysis)
   ;;; ═══════════════════════════════════════════════════════════════════════

   ;; Reactive analysis
   #:analyze-reactive-dependencies
   #:optimize-reactive-code
   #:defoptimized-component

   ;; Template validation
   #:defvalidated-template
   #:validate-css-class
   #:validate-token-reference
   #:*css-class-registry*
   #:register-css-classes))

(in-package :lol-reactive)
