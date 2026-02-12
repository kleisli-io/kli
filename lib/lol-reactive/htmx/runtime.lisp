;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; HTMX-style client runtime for declarative DOM updates
;;;;
;;;; Provides AJAX request handling, swap strategies, and OOB updates
;;;; via Parenscript-generated JavaScript.

(in-package :lol-reactive)

;;; ============================================================================
;;; HTMX RUNTIME (Parenscript)
;;;
;;; Client-side runtime that processes hx-* attributes and performs
;;; partial page updates. All JavaScript generated via Parenscript.
;;; ============================================================================

(defun htmx-runtime-js ()
  "Generate the HTMX-style client runtime via Parenscript.
   Processes hx-get, hx-post, hx-swap, hx-target, hx-trigger, hx-sync attributes.
   Supports 9 swap strategies, out-of-band updates, request cancellation, and autocomplete."
  (parenscript:ps
    ;; HTMX Runtime Object
    (defvar *htmx*
      (ps:create
       "version" "0.3.1"
       "config" (ps:create
                "defaultSwapStyle" "innerHTML"
                "defaultSettleDelay" 20
                "withCredentials" nil
                "timeout" 0)

       ;; AbortController storage for hx-sync support
       "abortControllers" (ps:create)

       ;; IntersectionObserver storage for revealed/intersect triggers
       "observers" (ps:create)

       ;; Swap Strategies
       "swap" (lambda (target html swap-style)
               (let ((style (or swap-style (ps:@ *htmx* config default-swap-style))))
                 (cond
                   ((= style "innerHTML")
                    (setf (ps:@ target inner-h-t-m-l) html))
                   ((= style "outerHTML")
                    (setf (ps:@ target outer-h-t-m-l) html))
                   ((= style "beforebegin")
                    (ps:chain target (insert-adjacent-h-t-m-l "beforebegin" html)))
                   ((= style "afterbegin")
                    (ps:chain target (insert-adjacent-h-t-m-l "afterbegin" html)))
                   ((= style "beforeend")
                    (ps:chain target (insert-adjacent-h-t-m-l "beforeend" html)))
                   ((= style "afterend")
                    (ps:chain target (insert-adjacent-h-t-m-l "afterend" html)))
                   ((= style "textContent")
                    (setf (ps:@ target text-content) html))
                   ((= style "delete")
                    (ps:chain target (remove)))
                   ((= style "none")
                    nil)
                   (t
                    (setf (ps:@ target inner-h-t-m-l) html)))))

       ;; Out-of-Band Swap Processing
       "processOobSwaps" (lambda (response-html)
                            (let ((temp (ps:chain document (create-element "div"))))
                              (setf (ps:@ temp inner-h-t-m-l) response-html)
                              (let ((oob-elements (ps:chain temp (query-selector-all "[hx-swap-oob]"))))
                                (ps:chain oob-elements (for-each
                                                        (lambda (el)
                                                          (let* ((oob-value (ps:chain el (get-attribute "hx-swap-oob")))
                                                                 (target-id (ps:@ el id))
                                                                 (target (ps:chain document (get-element-by-id target-id))))
                                                            (when target
                                                              (ps:chain el (remove-attribute "hx-swap-oob"))
                                                              (let ((strategy (if (or (= oob-value "true")
                                                                                      (= oob-value ""))
                                                                                  "outerHTML"
                                                                                  oob-value)))
                                                                ;; For outerHTML, preserve dynamic classes from target
                                                                (when (= strategy "outerHTML")
                                                                  ;; Copy dynamic state classes (e.g., "open") to new element
                                                                  (when (ps:chain target class-list (contains "open"))
                                                                    (ps:chain el class-list (add "open"))))
                                                                ;; Perform the swap
                                                                (if (= strategy "outerHTML")
                                                                    ((ps:@ *htmx* swap) target (ps:@ el outer-h-t-m-l) strategy)
                                                                    ((ps:@ *htmx* swap) target (ps:@ el inner-h-t-m-l) strategy))
                                                                ;; Re-initialize HTMX on the new element
                                                                (let ((new-el (ps:chain document (get-element-by-id target-id))))
                                                                  (when new-el
                                                                    ;; Process the element itself
                                                                    ((ps:@ *htmx* process-element) new-el)
                                                                    ;; Process any children with hx-* attributes
                                                                    (let ((htmx-children (ps:chain new-el
                                                                                           (query-selector-all
                                                                                            "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                                                                      (ps:chain htmx-children
                                                                                (for-each (ps:@ *htmx* process-element))))
                                                                    ;; Dispatch htmx:load on new OOB content
                                                                    ((ps:@ *htmx* dispatch-event) "htmx:load" new-el
                                                                     (ps:create :elt new-el))))))
                                                            (ps:chain el (remove)))))))
                              (ps:@ temp inner-h-t-m-l)))

       ;; Request Handling with hx-sync support and form serialization
       "issueRequest" (lambda (element method url)
                        (let* ((target-selector (ps:chain element (get-attribute "hx-target")))
                               (target (if target-selector
                                           (if (= (ps:chain target-selector (char-at 0)) "#")
                                               (ps:chain document (get-element-by-id
                                                                   (ps:chain target-selector (substring 1))))
                                               (ps:chain document (query-selector target-selector)))
                                           element))
                               (swap-style (or (ps:chain element (get-attribute "hx-swap"))
                                               (ps:@ *htmx* config default-swap-style)))
                               (headers (ps:create
                                         "HX-Request" "true"
                                         "HX-Trigger" (or (ps:@ element id) "")
                                         "HX-Target" (or (and target (ps:@ target id)) "")
                                         "HX-Current-URL" (ps:@ window location href)))
                               ;; Form serialization: find form element
                               (form (if (= (ps:chain (ps:@ element tag-name) (to-lower-case)) "form")
                                         element
                                         (ps:chain element (closest "form"))))
                               ;; For GET: append element values as query parameters
                               (get-url (when (= method "GET")
                                          (if (= (ps:chain (ps:@ element tag-name) (to-lower-case)) "form")
                                              ;; Form GET: serialize all form inputs
                                              (let ((params (ps:new (-U-R-L-Search-Params
                                                                     (ps:new (-Form-Data element))))))
                                                (let ((qs (ps:chain params (to-string))))
                                                  (if (> (ps:@ qs length) 0)
                                                      (+ url "?" qs)
                                                      url)))
                                              ;; Input GET: include element's own name=value
                                              (let ((input-name (ps:chain element (get-attribute "name")))
                                                    (input-value (ps:@ element value)))
                                                (if (and input-name input-value
                                                         (> (ps:@ input-value length) 0))
                                                    (+ url "?"
                                                       (encode-u-r-i-component input-name) "="
                                                       (encode-u-r-i-component input-value))
                                                    url)))))
                               ;; Create FormData for POST/PUT/DELETE when we have a form
                               (body (when (and form (not (= method "GET")))
                                       (ps:new (-Form-Data form))))
                               ;; hx-sync support: parse "this:replace" or "this:drop" etc.
                               (sync-attr (ps:chain element (get-attribute "hx-sync")))
                               (sync-strategy (when sync-attr
                                                (let ((parts (ps:chain sync-attr (split ":"))))
                                                  (if (> (ps:@ parts length) 1)
                                                      (aref parts 1)
                                                      "replace"))))
                               (element-id (or (ps:@ element id) "htmx-default"))
                               (existing-controller (ps:getprop (ps:@ *htmx* abort-controllers) element-id))
                               ;; Determine if we should proceed with request
                               (should-proceed t)
                               ;; hx-keepalive: survive page navigation (e.g. blur-save)
                               (keepalive (ps:chain element (get-attribute "hx-keepalive")))
                               ;; Track request outcome for htmx:afterRequest
                               (request-succeeded false))
                          ;; Handle sync strategies
                          (when (and existing-controller sync-strategy)
                            (cond
                              ;; replace/abort: cancel existing, start new
                              ((or (= sync-strategy "replace")
                                   (= sync-strategy "abort"))
                               (ps:chain existing-controller (abort)))
                              ;; drop: don't start new request if one in flight
                              ((= sync-strategy "drop")
                               (setf should-proceed nil))))
                          ;; Only proceed if not dropped
                          (when should-proceed
                            ;; Create new AbortController for this request
                            (let ((controller (ps:new (-Abort-Controller)))
                                  (timeout-id nil))
                              (setf (ps:getprop (ps:@ *htmx* abort-controllers) element-id) controller)
                              ;; Set up request timeout if configured
                              (when (> (ps:@ *htmx* config timeout) 0)
                                (setf timeout-id
                                      (set-timeout
                                       (lambda ()
                                         (ps:chain controller (abort))
                                         ((ps:@ *htmx* dispatch-event) "htmx:timeout" element
                                          (ps:create :elt element :target target)))
                                       (ps:@ *htmx* config timeout))))
                              (ps:chain element class-list (add "htmx-request"))
                              ;; Dispatch htmx:configRequest - listeners can modify headers/params
                              ((ps:@ *htmx* dispatch-event) "htmx:configRequest" element
                               (ps:create :headers headers :elt element :target target :verb method))
                              ;; Dispatch htmx:beforeRequest (cancelable) - last chance to cancel
                              (let ((before-event (ps:new (-Custom-Event "htmx:beforeRequest"
                                                           (ps:create :bubbles t :cancelable t
                                                                      :detail (ps:create :elt element :target target))))))
                                (if (ps:chain element (dispatch-event before-event))
                                    ;; Not cancelled - proceed with request
                                    (progn
                              ;; Build fetch options - include body for non-GET with form data
                              ;; Note: Don't set Content-Type for FormData; browser sets it with boundary
                              (ps:chain
                               (fetch (or get-url url) (ps:create
                                           :method method
                                           :headers headers
                                           :body body
                                           :credentials (if (ps:@ *htmx* config with-credentials)
                                                            "include"
                                                            "same-origin")
                                           :signal (ps:@ controller signal)
                                           :keepalive (if keepalive true false)))
                               (then (lambda (response)
                                       (if (ps:@ response ok)
                                           (progn
                                             (setf request-succeeded t)
                                             (ps:chain response (text)))
                                           (progn
                                             ;; Dispatch htmx:responseError for HTTP errors
                                             ((ps:@ *htmx* dispatch-event) "htmx:responseError" element
                                              (ps:create :elt element :target target
                                                         :status (ps:@ response status)
                                                         :status-text (ps:@ response status-text)))
                                             (ps:chain -promise (reject (ps:@ response status-text)))))))
                               (then (lambda (html)
                                       (let ((remaining-html ((ps:@ *htmx* process-oob-swaps) html)))
                                         ;; Dispatch htmx:beforeSwap (cancelable, with shouldSwap flag)
                                         (let ((before-swap-event
                                                 (ps:new (-Custom-Event "htmx:beforeSwap"
                                                           (ps:create :bubbles t :cancelable t
                                                                      :detail (ps:create
                                                                               :elt element :target target
                                                                               :server-response remaining-html
                                                                               :should-swap t))))))
                                           (let ((not-cancelled (ps:chain element (dispatch-event before-swap-event))))
                                             (when (and not-cancelled
                                                        (aref (ps:@ before-swap-event detail) "should-swap")
                                                        target
                                                        (not (= swap-style "none")))
                                               (set-timeout
                                                (lambda ()
                                                  ;; For outerHTML, target is replaced - capture parent + id first
                                                  (let ((parent (ps:@ target parent-node))
                                                        (target-id (ps:@ target id)))
                                                    ((ps:@ *htmx* swap) target remaining-html swap-style)
                                                    ;; Re-initialize HTMX on new elements
                                                    (let ((scope (if (and (= swap-style "outerHTML") target-id)
                                                                     ;; Try to find new element with same ID
                                                                     (or (ps:chain document (get-element-by-id target-id))
                                                                         parent)
                                                                     target)))
                                                      (when scope
                                                        ;; Process the scope element itself if it has hx-* attrs
                                                        (when (ps:chain scope (get-attribute "hx-get"))
                                                          ((ps:@ *htmx* process-element) scope))
                                                        ;; Process any children with hx-* attributes
                                                        (let ((htmx-children (ps:chain scope
                                                                               (query-selector-all
                                                                                "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                                                          (ps:chain htmx-children
                                                                    (for-each (ps:@ *htmx* process-element))))))
                                                    ;; Dispatch htmx:afterSwap on the requesting element
                                                    ((ps:@ *htmx* dispatch-event) "htmx:afterSwap" element
                                                     (ps:create :elt element :target target))
                                                    ;; Dispatch htmx:load on new content (for library initialization)
                                                    ((ps:@ *htmx* dispatch-event) "htmx:load" scope
                                                     (ps:create :elt scope))
                                                    ;; Dispatch htmx:afterSettle after DOM has settled
                                                    ((ps:@ *htmx* dispatch-event) "htmx:afterSettle" element
                                                     (ps:create :elt element :target target))))
                                                (ps:@ *htmx* config default-settle-delay))))))))
                               (catch (lambda (err)
                                        ;; Don't log abort errors (they're intentional)
                                        (unless (= (ps:@ err name) "AbortError")
                                          ;; Dispatch htmx:sendError for network failures
                                          ((ps:@ *htmx* dispatch-event) "htmx:sendError" element
                                           (ps:create :elt element :target target :error err))
                                          (ps:chain console (error "HTMX error:" err)))))
                               (finally (lambda ()
                                          ;; Clear request timeout
                                          (when timeout-id
                                            (clear-timeout timeout-id))
                                          ;; Dispatch htmx:afterRequest (always fires, success or failure)
                                          ((ps:@ *htmx* dispatch-event) "htmx:afterRequest" element
                                           (ps:create :elt element :target target
                                                      :successful request-succeeded
                                                      :failed (not request-succeeded)))
                                          (ps:chain element class-list (remove "htmx-request"))
                                          ;; Clean up controller if this is the current one
                                          (when (= (ps:getprop (ps:@ *htmx* abort-controllers) element-id)
                                                   controller)
                                            (delete (ps:getprop (ps:@ *htmx* abort-controllers) element-id))))))) ;; close: delete+when+finally-lambda+finally+catch-lambda+catch+chain
                                    ) ;; close progn (if true branch)
                                    ;; Cancelled by htmx:beforeRequest - clean up
                                    (progn
                                      (ps:chain element class-list (remove "htmx-request"))
                                      (delete (ps:getprop (ps:@ *htmx* abort-controllers) element-id))))) ;; close: progn+if+let(before-event)
                              )))  ;; close: let(controller)+when(should-proceed)+let*(issueRequest lambda)

       ;; Utility: convert dash-case to camelCase
       "dashToCamel" (lambda (str)
                       (ps:chain str
                         (replace (ps:regex "/-([a-z])/g")
                                  (lambda (match letter)
                                    (ps:chain letter (to-upper-case))))))

       ;; hx-on-* attribute processing
       ;; Supports: hx-on-click, hx-on-htmx-after-swap, hx-on--after-swap
       "processHxOn" (lambda (element)
                       (let ((attrs (ps:chain -array prototype slice
                                              (call (ps:chain element attributes)))))
                         (ps:chain attrs
                           (for-each
                             (lambda (attr)
                               (let ((name (ps:@ attr name)))
                                 (when (ps:chain name (starts-with "hx-on"))
                                   (let* ((suffix (ps:chain name (substring 5)))
                                          (event-name
                                            (cond
                                              ;; hx-on--after-swap → htmx:afterSwap
                                              ((ps:chain suffix (starts-with "--"))
                                               (+ "htmx:" ((ps:@ *htmx* dash-to-camel)
                                                            (ps:chain suffix (substring 2)))))
                                              ;; hx-on-htmx-after-swap → htmx:afterSwap
                                              ((ps:chain suffix (starts-with "-htmx-"))
                                               (+ "htmx:" ((ps:@ *htmx* dash-to-camel)
                                                            (ps:chain suffix (substring 6)))))
                                              ;; hx-on-click → click
                                              (t (ps:chain suffix (substring 1)))))
                                          (code (ps:@ attr value)))
                                     (ps:chain element
                                       (add-event-listener event-name
                                         (ps:new (-Function "event" code))))))))))))

       ;; Dispatch a CustomEvent on an element (bubbles up)
       "dispatchEvent" (lambda (name element detail)
                         (let ((event (ps:new (-Custom-Event name
                                               (ps:create :bubbles t
                                                          :cancelable t
                                                          :detail detail)))))
                           (ps:chain element (dispatch-event event))))

       ;; Element Processing
       "processElement" (lambda (element)
                          ;; Process hx-on-* event handler attributes
                          ((ps:@ *htmx* process-hx-on) element)
                          (let ((get-url (ps:chain element (get-attribute "hx-get"))))
                            (when get-url
                              (let ((trigger ((ps:@ *htmx* parse-trigger)
                                              (or (ps:chain element (get-attribute "hx-trigger")) "click"))))
                                ((ps:@ *htmx* add-trigger-handler) element trigger
                                 (lambda () ((ps:@ *htmx* issue-request) element "GET" get-url))))))
                          (let ((post-url (ps:chain element (get-attribute "hx-post"))))
                            (when post-url
                              (let ((trigger ((ps:@ *htmx* parse-trigger)
                                              (or (ps:chain element (get-attribute "hx-trigger")) "click"))))
                                ((ps:@ *htmx* add-trigger-handler) element trigger
                                 (lambda () ((ps:@ *htmx* issue-request) element "POST" post-url))))))
                          (let ((put-url (ps:chain element (get-attribute "hx-put"))))
                            (when put-url
                              (let ((trigger ((ps:@ *htmx* parse-trigger)
                                              (or (ps:chain element (get-attribute "hx-trigger")) "click"))))
                                ((ps:@ *htmx* add-trigger-handler) element trigger
                                 (lambda () ((ps:@ *htmx* issue-request) element "PUT" put-url))))))
                          (let ((delete-url (ps:chain element (get-attribute "hx-delete"))))
                            (when delete-url
                              (let ((trigger ((ps:@ *htmx* parse-trigger)
                                              (or (ps:chain element (get-attribute "hx-trigger")) "click"))))
                                ((ps:@ *htmx* add-trigger-handler) element trigger
                                 (lambda () ((ps:@ *htmx* issue-request) element "DELETE" delete-url)))))))

       ;; Trigger Parsing
       "parseInterval" (lambda (str)
                         (cond
                           ((ps:chain str (ends-with "ms"))
                            (parse-int (ps:chain str (slice 0 -2))))
                           ((ps:chain str (ends-with "s"))
                            (* 1000 (parse-int (ps:chain str (slice 0 -1)))))
                           (t
                            (parse-int str))))

       "parseTrigger" (lambda (trigger-string)
                        (let ((parts (ps:chain trigger-string (split " ")))
                              (spec (ps:create
                                     :event "click"
                                     :delay nil
                                     :throttle nil
                                     :changed nil
                                     :once nil)))
                          (when (> (ps:@ parts length) 0)
                            (let ((first-part (aref parts 0)))
                              (if (or (ps:chain first-part (starts-with "delay:"))
                                      (ps:chain first-part (starts-with "throttle:"))
                                      (= first-part "changed")
                                      (= first-part "once"))
                                  (setf (ps:@ spec event) "click")
                                  (setf (ps:@ spec event) first-part))))
                          (ps:chain parts (for-each
                                           (lambda (part)
                                             (cond
                                               ((ps:chain part (starts-with "delay:"))
                                                (setf (ps:@ spec delay)
                                                      ((ps:@ *htmx* parse-interval)
                                                       (ps:chain part (substring 6)))))
                                               ((ps:chain part (starts-with "throttle:"))
                                                (setf (ps:@ spec throttle)
                                                      ((ps:@ *htmx* parse-interval)
                                                       (ps:chain part (substring 9)))))
                                               ((= part "changed")
                                                (setf (ps:@ spec changed) t))
                                               ((= part "once")
                                                (setf (ps:@ spec once) t))))))
                          spec))

       "addTriggerHandler" (lambda (element trigger-spec handler)
                              (let ((event-name (ps:@ trigger-spec event)))
                                (cond
                                  ;; IntersectionObserver triggers: revealed, intersect
                                  ((or (= event-name "revealed")
                                       (= event-name "intersect"))
                                   ((ps:@ *htmx* setup-intersection-observer)
                                    element
                                    (lambda (entry)
                                      (declare (ignore entry))
                                      ;; Respect delay modifier if specified
                                      (if (ps:@ trigger-spec delay)
                                          (set-timeout handler (ps:@ trigger-spec delay))
                                          (funcall handler)))
                                    (ps:create "threshold" 0.1
                                               "once" (ps:@ trigger-spec once))))

                                  ;; Load trigger: fire immediately (or after delay)
                                  ((= event-name "load")
                                   (if (ps:@ trigger-spec delay)
                                       (set-timeout handler (ps:@ trigger-spec delay))
                                       (funcall handler)))

                                  ;; Standard DOM events: use addEventListener
                                  (t
                                   (let ((timer nil)
                                         (last-value nil)
                                         (throttled nil)
                                         (fired nil))
                                     (ps:chain element
                                               (add-event-listener
                                                event-name
                                                (lambda (event)
                                                  (when (or (= (ps:chain (ps:@ element tag-name) (to-lower-case)) "a")
                                                            (= (ps:chain (ps:@ element tag-name) (to-lower-case)) "form"))
                                                    (ps:chain event (prevent-default)))
                                                  (let ((should-fire t))
                                                    (when (and (ps:@ trigger-spec once) fired)
                                                      (setf should-fire nil))
                                                    (when (and should-fire (ps:@ trigger-spec changed))
                                                      (let ((current-value (or (ps:@ element value)
                                                                               (ps:@ element inner-h-t-m-l))))
                                                        (if (= current-value last-value)
                                                            (setf should-fire nil)
                                                            (setf last-value current-value))))
                                                    (when (and should-fire (ps:@ trigger-spec throttle) throttled)
                                                      (setf should-fire nil))
                                                    (when should-fire
                                                      (if (ps:@ trigger-spec delay)
                                                          (progn
                                                            (clear-timeout timer)
                                                            (setf timer (set-timeout
                                                                         (lambda ()
                                                                           (setf fired t)
                                                                           (funcall handler))
                                                                         (ps:@ trigger-spec delay))))
                                                          (progn
                                                            (when (ps:@ trigger-spec throttle)
                                                              (setf throttled t)
                                                              (set-timeout
                                                               (lambda () (setf throttled nil))
                                                               (ps:@ trigger-spec throttle)))
                                                            (setf fired t)
                                                            (funcall handler)))))))))))))

       ;; ============================================================
       ;; Autocomplete / Keyboard Navigation Support
       ;; ============================================================

       ;; Setup autocomplete keyboard navigation for an input
       "setupAutocomplete" (lambda (input-id results-selector &optional options)
                             (let ((input (ps:chain document (get-element-by-id input-id)))
                                   (selected-index -1)
                                   (on-select (and options (ps:@ options on-select))))
                               (when input
                                 ;; Keyboard navigation handler
                                 (ps:chain input (add-event-listener "keydown"
                                   (lambda (event)
                                     (let* ((results-container (ps:chain document (query-selector results-selector)))
                                            (opts (if results-container
                                                      (ps:chain results-container (query-selector-all "[role=option]"))
                                                      (array)))
                                            (len (ps:@ opts length)))
                                       (when (> len 0)
                                         (cond
                                           ;; Arrow Down - navigate to next option
                                           ((= (ps:@ event key) "ArrowDown")
                                            (ps:chain event (prevent-default))
                                            (setf selected-index (if (>= selected-index (1- len))
                                                                     0
                                                                     (1+ selected-index)))
                                            ((ps:@ *htmx* highlight-option) opts selected-index input))
                                           ;; Arrow Up - navigate to previous option
                                           ((= (ps:@ event key) "ArrowUp")
                                            (ps:chain event (prevent-default))
                                            (setf selected-index (if (<= selected-index 0)
                                                                     (1- len)
                                                                     (1- selected-index)))
                                            ((ps:@ *htmx* highlight-option) opts selected-index input))
                                           ;; Enter - select current option
                                           ((= (ps:@ event key) "Enter")
                                            (when (>= selected-index 0)
                                              (ps:chain event (prevent-default))
                                              (let ((opt (aref opts selected-index)))
                                                (if on-select
                                                    (funcall on-select opt)
                                                    (ps:chain opt (click))))))
                                           ;; Escape - clear selection and close
                                           ((= (ps:@ event key) "Escape")
                                            (setf selected-index -1)
                                            ((ps:@ *htmx* clear-highlights) opts)
                                            (setf (ps:@ input aria-expanded) "false"))))))))
                                 ;; Reset selection when results update via MutationObserver
                                 (let ((results-el (ps:chain document (query-selector results-selector))))
                                   (when results-el
                                     (let ((observer (ps:new (-Mutation-Observer
                                                             (lambda ()
                                                               (setf selected-index -1)
                                                               ;; Update aria-expanded based on results presence
                                                               (let ((has-results (> (ps:@ (ps:chain results-el
                                                                                             (query-selector-all "[role=option]")) length) 0)))
                                                                 (setf (ps:@ input aria-expanded)
                                                                       (if has-results "true" "false"))))))))
                                       (ps:chain observer (observe results-el
                                                                   (ps:create "childList" t "subtree" t)))))))))

       ;; Highlight a specific option in the autocomplete list
       "highlightOption" (lambda (options index input)
                           (ps:chain options (for-each
                             (lambda (opt i)
                               (if (= i index)
                                   (progn
                                     (ps:chain opt class-list (add "selected"))
                                     (ps:chain opt (set-attribute "aria-selected" "true"))
                                     (ps:chain opt (scroll-into-view (ps:create :block "nearest")))
                                     ;; Update aria-activedescendant on input
                                     (when (and input (ps:@ opt id))
                                       (ps:chain input (set-attribute "aria-activedescendant" (ps:@ opt id)))))
                                   (progn
                                     (ps:chain opt class-list (remove "selected"))
                                     (ps:chain opt (set-attribute "aria-selected" "false"))))))))

       ;; Clear all highlights from autocomplete options
       "clearHighlights" (lambda (options)
                           (ps:chain options (for-each
                             (lambda (opt)
                               (ps:chain opt class-list (remove "selected"))
                               (ps:chain opt (set-attribute "aria-selected" "false"))))))

       ;; ============================================================
       ;; IntersectionObserver Support (revealed/intersect triggers)
       ;; ============================================================

       ;; Setup IntersectionObserver for an element
       ;; Options: threshold (0.1 default), once (disconnect after fire), root, rootMargin
       "setupIntersectionObserver" (lambda (element handler &optional options)
                                     (let* ((threshold (or (and options (ps:getprop options "threshold")) 0.1))
                                            (once (and options (ps:getprop options "once")))
                                            (root (and options (ps:getprop options "root")))
                                            (root-margin (or (and options (ps:getprop options "rootMargin")) "0px"))
                                            ;; Ensure element has ID for storage
                                            (element-id (or (ps:@ element id)
                                                            (progn
                                                              (setf (ps:@ element id)
                                                                    (+ "htmx-obs-" (ps:chain -math (random) (to-string 36) (substr 2 9))))
                                                              (ps:@ element id))))
                                            ;; Pre-declare observer so callback can reference it
                                            (observer nil))
                                       ;; Create and assign observer
                                       (setf observer
                                             (ps:new (-Intersection-Observer
                                                     (lambda (entries)
                                                       (ps:chain entries (for-each
                                                         (lambda (entry)
                                                           (when (ps:@ entry is-intersecting)
                                                             (funcall handler entry)
                                                             (when once
                                                               ;; Disconnect and clean up
                                                               (ps:chain observer (unobserve (ps:@ entry target)))
                                                               (delete (ps:getprop (ps:@ *htmx* observers) element-id))))))))
                                                     (ps:create "threshold" threshold
                                                                "root" root
                                                                "rootMargin" root-margin))))
                                       ;; Store observer for cleanup
                                       (setf (ps:getprop (ps:@ *htmx* observers) element-id) observer)
                                       ;; Start observing
                                       (ps:chain observer (observe element))
                                       ;; Return observer for manual control
                                       observer))

       ;; Disconnect observer for an element
       "disconnectObserver" (lambda (element)
                              (let* ((element-id (ps:@ element id))
                                     (observer (and element-id
                                                    (ps:getprop (ps:@ *htmx* observers) element-id))))
                                (when observer
                                  (ps:chain observer (disconnect))
                                  (delete (ps:getprop (ps:@ *htmx* observers) element-id)))))

       ;; ============================================================
       ;; Public API (htmx.org compatible)
       ;; ============================================================

       ;; htmx.process(elt) - initialize htmx behavior on dynamically added content
       "process" (lambda (elt)
                   ;; Process the element itself if it has verb attributes
                   (when (or (ps:chain elt (get-attribute "hx-get"))
                             (ps:chain elt (get-attribute "hx-post"))
                             (ps:chain elt (get-attribute "hx-put"))
                             (ps:chain elt (get-attribute "hx-delete")))
                     ((ps:@ *htmx* process-element) elt))
                   ;; Process children with verb attributes
                   (let ((children (ps:chain elt (query-selector-all
                                                  "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                     (ps:chain children (for-each (ps:@ *htmx* process-element))))
                   ;; Process hx-on-* on this element and all children
                   ((ps:@ *htmx* process-hx-on) elt)
                   (let ((all-children (ps:chain -array (from (ps:chain elt (get-elements-by-tag-name "*"))))))
                     (ps:chain all-children (for-each
                       (lambda (child)
                         (let ((attrs (ps:chain -array prototype slice (call (ps:chain child attributes)))))
                           (when (ps:chain attrs (some (lambda (attr)
                                                         (ps:chain (ps:@ attr name) (starts-with "hx-on")))))
                             ((ps:@ *htmx* process-hx-on) child))))))))

       ;; htmx.ajax(verb, path, target) - issue programmatic AJAX request
       "ajax" (lambda (verb path target)
                (let ((target-el (if (stringp target)
                                     (ps:chain document (query-selector target))
                                     target)))
                  (when target-el
                    ((ps:@ *htmx* issue-request) target-el verb path))))

       ;; htmx.trigger(elt, name, detail) - dispatch custom event on element
       "trigger" (lambda (elt name detail)
                   ((ps:@ *htmx* dispatch-event) name elt (or detail (ps:create))))

       ;; htmx.on(elt, event, fn) - add event listener, returns listener
       "on" (lambda (elt event-name listener)
              (ps:chain elt (add-event-listener event-name listener))
              listener)

       ;; htmx.off(elt, event, fn) - remove event listener
       "off" (lambda (elt event-name listener)
               (ps:chain elt (remove-event-listener event-name listener)))

       ;; htmx.onLoad(fn) - register callback for htmx:load events
       "onLoad" (lambda (callback)
                  (ps:chain document (add-event-listener "htmx:load"
                    (lambda (evt)
                      (callback (ps:@ evt detail elt))))))

       ;; ============================================================
       ;; Initialization
       ;; ============================================================

       "init" (lambda ()
               ;; Process hx-* elements (request verbs + hx-on-* handlers)
               (let ((htmx-elements (ps:chain document (query-selector-all
                                                        "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                 (ps:chain htmx-elements (for-each (lambda (el)
                                                     (ps:chain *htmx* (process-element el))))))
               ;; Process hx-on-* on elements without request verbs
               ;; Scan all elements for any hx-on-* attribute (not just a hardcoded list)
               (let ((all-elements (ps:chain -array (from (ps:chain document (get-elements-by-tag-name "*"))))))
                 (ps:chain all-elements (for-each
                   (lambda (el)
                     ;; Skip if already processed via processElement (has verb attributes)
                     (unless (or (ps:chain el (get-attribute "hx-get"))
                                 (ps:chain el (get-attribute "hx-post"))
                                 (ps:chain el (get-attribute "hx-put"))
                                 (ps:chain el (get-attribute "hx-delete")))
                       ;; Check if any attribute starts with hx-on
                       (let ((attrs (ps:chain -array prototype slice (call (ps:chain el attributes)))))
                         (when (ps:chain attrs (some (lambda (attr)
                                                       (ps:chain (ps:@ attr name) (starts-with "hx-on")))))
                           ((ps:@ *htmx* process-hx-on) el))))))))
               ;; Initialize autocomplete keyboard navigation
               (let ((ac-elements (ps:chain document (query-selector-all "[aria-autocomplete]"))))
                 (ps:chain ac-elements (for-each
                   (lambda (el)
                     (let ((el-id (ps:@ el id)))
                       (when el-id
                         ((ps:@ *htmx* setup-autocomplete)
                          el-id (+ "#" el-id "-results"))))))))
               (ps:chain console (log "(HTMX :status :loaded :version" (ps:@ *htmx* version) ")")))))

    ;; Auto-initialize on DOMContentLoaded
    (if (= (ps:@ document ready-state) "loading")
        (ps:chain document (add-event-listener "DOMContentLoaded"
                                               (ps:@ *htmx* init)))
        ((ps:@ *htmx* init)))

    ;; Lowercase alias for compatibility with standard htmx naming
    (setf (ps:@ window htmx) *htmx*)))

;;; ============================================================================
;;; HTMX ATTRIBUTE HELPERS
;;; ============================================================================

(defun hx-get (url &key target swap trigger)
  "Generate hx-get attribute string for cl-who."
  (format nil "~@[hx-get=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
          url target swap trigger))

(defun hx-post (url &key target swap trigger)
  "Generate hx-post attribute string for cl-who."
  (format nil "~@[hx-post=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
          url target swap trigger))

(defun hx-put (url &key target swap trigger)
  "Generate hx-put attribute string for cl-who."
  (format nil "~@[hx-put=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
          url target swap trigger))

(defun hx-delete (url &key target swap trigger)
  "Generate hx-delete attribute string for cl-who."
  (format nil "~@[hx-delete=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
          url target swap trigger))

;;; ============================================================================
;;; OOB RESPONSE HELPERS
;;; ============================================================================

(defun find-tag-end (html)
  "Find the position of > that ends the opening tag, skipping > inside quotes.
   Returns the position of the closing > or NIL if not found."
  (let ((in-quote nil)
        (len (length html)))
    (loop for i from 0 below len
          for char = (char html i)
          do (cond
               ((char= char #\")
                (setf in-quote (not in-quote)))
               ((and (char= char #\>) (not in-quote))
                (return i)))
          finally (return nil))))

(defun content-starts-with-id-p (html target-id)
  "Check if HTML starts with an element that has the specified ID.
   Returns T if the first element's opening tag contains id=\"TARGET-ID\".
   Correctly handles > characters inside quoted attribute values."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) html)))
    (when (and (> (length trimmed) 0)
               (char= (char trimmed 0) #\<))
      ;; Find end of opening tag (skip > inside quotes)
      (let ((tag-end (find-tag-end trimmed)))
        (when tag-end
          ;; Look for id="target-id" within the opening tag
          (let ((tag-content (subseq trimmed 0 tag-end))
                (id-pattern (format nil "id=\"~a\"" target-id)))
            (search id-pattern tag-content :test #'char-equal)))))))

(defun inject-oob-attribute (html swap-value)
  "Inject hx-swap-oob attribute into the first element's opening tag.
   Handles both regular tags and self-closing tags (e.g., <input />).
   Correctly handles > characters inside quoted attribute values.
   Returns the modified HTML string."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) html)))
    (let ((first-gt (find-tag-end trimmed)))
      (if first-gt
          ;; Check for self-closing tag: look for / before >
          (let* ((before-gt (subseq trimmed 0 first-gt))
                 (slash-pos (position #\/ before-gt :from-end t))
                 ;; Is it a self-closing tag? (/ appears near end, only whitespace between / and >)
                 (self-closing-p (and slash-pos
                                      (every (lambda (c) (member c '(#\Space #\Tab)))
                                             (subseq before-gt (1+ slash-pos)))))
                 ;; Insert position: before the / for self-closing, before > otherwise
                 (insert-pos (if self-closing-p slash-pos first-gt)))
            (concatenate 'string
                         (subseq trimmed 0 insert-pos)
                         (format nil " hx-swap-oob=\"~a\"" swap-value)
                         (subseq trimmed insert-pos)))
          ;; Fallback if no > found (shouldn't happen with valid HTML)
          html))))

(defun oob-swap (id content &key (swap "true"))
  "Generate an OOB swap element.
   SWAP can be: true (outerHTML), innerHTML, beforebegin, afterbegin, etc.

   Smart behavior for outerHTML swaps: if content already contains an element
   with the target ID, injects hx-swap-oob attribute directly instead of
   wrapping (which would create duplicate IDs)."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) content)))
    (if (and (string= swap "outerHTML")
             (content-starts-with-id-p trimmed id))
        ;; Content already has the ID - inject attribute directly
        (inject-oob-attribute trimmed swap)
        ;; Standard wrapping for innerHTML, other strategies, or content without ID
        (cl-who:with-html-output-to-string (s)
          (:div :id id :hx-swap-oob swap
                (cl-who:str content))))))

(defmacro with-oob-swaps ((&rest swaps) &body body)
  "Execute BODY and append OOB swap elements.
   SWAPS is a list of (id content &key swap) specifications."
  `(concatenate 'string
                (progn ,@body)
                ,@(mapcar (lambda (swap-spec)
                            `(oob-swap ,@swap-spec))
                          swaps)))

(defun oob-content (id content)
  "Generate an OOB innerHTML swap that preserves target element attributes.

   Unlike oob-swap which replaces the entire element (including class, hx-*, etc),
   this only replaces the innerHTML of the target element, preserving all attributes.

   Use this when the target element has attributes you want to keep, such as:
   - CSS classes for styling
   - hx-trigger for polling
   - data-* attributes

   Example:
     ;; Target: <div id=\"counter\" class=\"big\" hx-trigger=\"every 1s\">0</div>
     (oob-content \"counter\" \"42\")
     ;; Result: <span style=\"display:none\" hx-swap-oob=\"innerHTML:#counter\">42</span>
     ;; Target becomes: <div id=\"counter\" class=\"big\" hx-trigger=\"every 1s\">42</div>"
  (htm-str
    (:span :style "display:none" :hx-swap-oob (format nil "innerHTML:#~A" id)
      (cl-who:str content))))

;;; ============================================================================
;;; HTMX CSS
;;; ============================================================================

(defun htmx-indicator-css ()
  "Generate CSS for HTMX request indicators using lol-reactive CSS utilities."
  (concatenate 'string
               "/* HTMX Request Indicator Styles */"
               (css-rules ".htmx-request"
                          :opacity "0.7"
                          :cursor "wait")
               (css-rules ".htmx-request.htmx-indicator"
                          :opacity "1")
               (css-rules ".htmx-indicator"
                          :display "none")
               (css-rules ".htmx-request .htmx-indicator, .htmx-request.htmx-indicator"
                          :display "inline-block")))

;;; ============================================================================
;;; AUTOCOMPLETE SUPPORT
;;; ============================================================================

(defun render-autocomplete (&key id endpoint
                                 (placeholder "Search...")
                                 (debounce 300)
                                 (min-chars 1)
                                 class)
  "Render a search input with autocomplete behavior.

   ID: Unique identifier for this autocomplete (required)
   ENDPOINT: Server endpoint to fetch results from (required)
   PLACEHOLDER: Input placeholder text
   DEBOUNCE: Milliseconds to wait before firing request (default 300)
   MIN-CHARS: Minimum characters before searching (default 1, reserved for future use)
   CLASS: Additional CSS classes for the container"
  (declare (ignore min-chars)) ; Reserved for future use
  (htm-str
    (:div :class (format nil "autocomplete-container~@[ ~a~]" class)
      (:input :type "search"
              :id id
              :name "q"
              :placeholder placeholder
              :autocomplete "off"
              :hx-get endpoint
              :hx-trigger (format nil "input changed delay:~ams" debounce)
              :hx-target (format nil "#~a-results" id)
              :hx-sync "this:replace"
              :hx-indicator (format nil "#~a-loading" id)
              :role "combobox"
              :aria-controls (format nil "~a-results" id)
              :aria-expanded "false"
              :aria-autocomplete "list")
      (:span :id (format nil "~a-loading" id)
             :class "htmx-indicator autocomplete-loading"
             "Searching...")
      (:div :id (format nil "~a-results" id)
            :role "listbox"
            :class "autocomplete-results"
            :aria-label "Search results"))))

(defun render-autocomplete-results (items &key id render-item (empty-message "No results found"))
  "Render search results for autocomplete.

   ITEMS: List of items to render
   ID: Autocomplete ID (must match render-autocomplete)
   RENDER-ITEM: Function to render each item (default: identity)
   EMPTY-MESSAGE: Message to show when no results"
  (let ((render-fn (or render-item #'identity)))
    (htm-str
      (if items
          (cl-who:htm
            (:ul :id (format nil "~a-results" id)
                 :role "listbox"
                 :class "autocomplete-results"
              (loop for item in items
                    for i from 0
                    do (cl-who:htm
                        (:li :role "option"
                             :id (format nil "~a-option-~a" id i)
                             :class "autocomplete-result"
                             :tabindex "-1"
                             :aria-selected "false"
                             (cl-who:str (funcall render-fn item)))))))
          (cl-who:htm
            (:div :id (format nil "~a-results" id)
                  :role "listbox"
                  :class "autocomplete-results autocomplete-empty"
              (:span :class "autocomplete-no-results"
                     (cl-who:str empty-message))))))))

(defun autocomplete-css ()
  "CSS for autocomplete component using design system tokens.
   Uses CSS variables for theming support."
  (concatenate 'string
    "/* Autocomplete Component Styles */"
    (css-rules ".autocomplete-container"
               "position" "relative")
    (css-rules ".autocomplete-results"
               "position" "absolute"
               "width" "100%"
               "max-height" "300px"
               "overflow-y" "auto"
               "background" (css-var "color-surface")
               "border" (format nil "~a solid ~a"
                                (css-var "effect-border-thin")
                                (css-var "color-muted"))
               "border-radius" (css-var "space-1")
               "box-shadow" (css-var "effect-shadow-md")
               "z-index" (css-var "effect-z-modal")
               "margin" "0"
               "padding" "0")
    (css-rules ".autocomplete-results:empty"
               "display" "none")
    (css-rules ".autocomplete-result"
               "padding" (format nil "~a ~a"
                                 (css-var "space-2")
                                 (css-var "space-3"))
               "cursor" "pointer"
               "list-style" "none"
               "color" (css-var "color-text")
               "transition" (css-var "effect-transition-fast"))
    (css-rules ".autocomplete-result:hover, .autocomplete-result.selected"
               "background" (css-var "color-surface-alt"))
    (css-rules ".autocomplete-result.selected"
               "outline" (format nil "2px solid ~a" (css-var "color-primary"))
               "outline-offset" "-2px")
    (css-rules ".autocomplete-loading"
               "display" "none"
               "position" "absolute"
               "right" (css-var "space-2")
               "top" "50%"
               "transform" "translateY(-50%)"
               "color" (css-var "color-muted")
               "font-size" (css-var "font-small"))
    (css-rules ".htmx-request .autocomplete-loading"
               "display" "inline")
    (css-rules ".autocomplete-no-results"
               "display" "block"
               "padding" (css-var "space-3")
               "color" (css-var "color-muted")
               "font-style" "italic")
    (css-rules ".autocomplete-empty"
               "border" "none"
               "box-shadow" "none")))

;;; ============================================================================
;;; WEBSOCKET CLIENT RUNTIME (Parenscript)
;;; ============================================================================

(defun ws-client-js ()
  "Generate WebSocket client runtime via Parenscript.
   Handles connection management, reconnection, and message processing."
  (parenscript:ps
    ;; WebSocket Manager Object
    (defvar *ws-manager*
      (ps:create
       "connections" (ps:create)  ; channel -> WebSocket
       "reconnectDelay" 1000
       "maxReconnectDelay" 30000

       ;; Connect to a WebSocket channel
       "connect" (lambda (channel &optional options)
                   (let* ((protocol (if (= (ps:@ window location protocol) "https:") "wss:" "ws:"))
                          (url (+ protocol "//" (ps:@ window location host) "/ws/" channel))
                          (ws (ps:new (-Web-Socket url)))
                          (reconnect-delay (ps:@ *ws-manager* reconnect-delay))
                          ;; Support both kebab-case ('on-open') and camelCase (onOpen) keys
                          (on-message (and options (or (ps:getprop options "on-message")
                                                       (ps:@ options on-message))))
                          (on-open (and options (or (ps:getprop options "on-open")
                                                    (ps:@ options on-open))))
                          (on-close (and options (or (ps:getprop options "on-close")
                                                     (ps:@ options on-close)))))

                     ;; Store connection
                     (setf (ps:getprop (ps:@ *ws-manager* connections) channel) ws)

                     ;; Handle connection open
                     (setf (ps:@ ws onopen)
                           (lambda ()
                             (ps:chain console (log "WebSocket connected:" channel))
                             (setf reconnect-delay (ps:@ *ws-manager* reconnect-delay))
                             (when on-open
                               (funcall on-open ws))))

                     ;; Handle incoming messages
                     (setf (ps:@ ws onmessage)
                           (lambda (event)
                             (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                               ;; Process based on message type
                               (let ((msg-type (ps:@ data type)))
                                 (cond
                                   ;; HTML swap
                                   ((= msg-type "html")
                                    ((ps:@ *ws-manager* handle-html-update) data))
                                   ;; OOB updates
                                   ((= msg-type "oob")
                                    ((ps:@ *ws-manager* handle-oob-updates) data))
                                   ;; Event trigger
                                   ((= msg-type "trigger")
                                    ((ps:@ *ws-manager* handle-trigger) data))
                                   ;; Custom handler
                                   (t
                                    (when on-message
                                      (funcall on-message data ws))))))))

                     ;; Handle connection close with reconnection
                     (setf (ps:@ ws onclose)
                           (lambda (event)
                             (ps:chain console (log "WebSocket closed:" channel "- reconnecting in" reconnect-delay "ms"))
                             (when on-close
                               (funcall on-close event ws))
                             ;; Attempt reconnection with exponential backoff
                             (set-timeout
                              (lambda ()
                                ((ps:@ *ws-manager* connect) channel options))
                              reconnect-delay)
                             ;; Increase delay for next attempt (with max)
                             (setf reconnect-delay
                                   (ps:chain -math (min (* reconnect-delay 2)
                                                        (ps:@ *ws-manager* max-reconnect-delay))))))

                     ;; Handle errors
                     (setf (ps:@ ws onerror)
                           (lambda (error)
                             (ps:chain console (error "WebSocket error:" channel error))))

                     ;; Handle race condition: if connection opened before handlers were set
                     (when (and on-open (= (ps:@ ws ready-state) 1))
                       (ps:chain console (log "WebSocket already connected:" channel))
                       (funcall on-open ws))

                     ws))

       ;; Disconnect from a channel
       "disconnect" (lambda (channel)
                      (let ((ws (ps:getprop (ps:@ *ws-manager* connections) channel)))
                        (when ws
                          (ps:chain ws (close))
                          (delete (ps:getprop (ps:@ *ws-manager* connections) channel)))))

       ;; Send message to a channel
       "send" (lambda (channel data)
                (let ((ws (ps:getprop (ps:@ *ws-manager* connections) channel)))
                  (when (and ws (= (ps:@ ws ready-state) 1))
                    (ps:chain ws (send (if (stringp data)
                                           data
                                           (ps:chain -j-s-o-n (stringify data))))))))

       ;; Handle HTML update message
       "handleHtmlUpdate" (lambda (data)
                            (let ((target (ps:chain document (get-element-by-id (ps:@ data target)))))
                              (when target
                                (let ((swap (or (ps:@ data swap) "innerHTML")))
                                  ((ps:@ *htmx* swap) target (ps:@ data html) swap)
                                  ;; Re-initialize HTMX on updated content
                                  ((ps:@ *htmx* process-element) target)))))

       ;; Handle OOB updates message
       "handleOobUpdates" (lambda (data)
                            (ps:chain (ps:@ data updates) (for-each
                              (lambda (update)
                                (let ((target (ps:chain document (get-element-by-id (ps:@ update target)))))
                                  (when target
                                    (let ((swap (or (ps:@ update swap) "outerHTML")))
                                      ((ps:@ *htmx* swap) target (ps:@ update html) swap)
                                      ;; Re-process the element
                                      (let ((new-el (ps:chain document (get-element-by-id (ps:@ update target)))))
                                        (when new-el
                                          ((ps:@ *htmx* process-element) new-el))))))))))

       ;; Handle event trigger message
       "handleTrigger" (lambda (data)
                         (let ((event (ps:new (-Custom-Event (ps:@ data event)
                                                          (ps:create :detail (ps:@ data detail)
                                                                     :bubbles t)))))
                           (ps:chain document (dispatch-event event))))))))

;;; ============================================================================
;;; SSE CLIENT RUNTIME (Parenscript)
;;; ============================================================================

(defun sse-client-js ()
  "Generate Server-Sent Events client runtime via Parenscript.
   Handles EventSource connections and message processing."
  (parenscript:ps
    ;; SSE Manager Object
    (defvar *sse-manager*
      (ps:create
       "connections" (ps:create)  ; channel -> EventSource
       "reconnectDelay" 3000      ; Default retry from SSE spec

       ;; Connect to an SSE channel
       "connect" (lambda (channel &optional options)
                   (let* ((url (+ "/sse/" channel))
                          (source (ps:new (-Event-Source url)))
                          ;; Support both kebab-case ('on-open') and camelCase (onOpen) keys
                          (on-message (and options (or (ps:getprop options "on-message")
                                                       (ps:@ options on-message))))
                          (on-open (and options (or (ps:getprop options "on-open")
                                                    (ps:@ options on-open))))
                          (on-error (and options (or (ps:getprop options "on-error")
                                                     (ps:@ options on-error)))))

                     ;; Store connection
                     (setf (ps:getprop (ps:@ *sse-manager* connections) channel) source)

                     ;; Handle connection open
                     (setf (ps:@ source onopen)
                           (lambda ()
                             (ps:chain console (log "SSE connected:" channel))
                             (when on-open
                               (funcall on-open source))))

                     ;; Handle 'connected' event (server confirmation)
                     (ps:chain source (add-event-listener "connected"
                       (lambda (event)
                         (ps:chain console (log "SSE confirmed:" channel
                                                (ps:chain -j-s-o-n (parse (ps:@ event data))))))))

                     ;; Handle 'update' event (HTML updates)
                     (ps:chain source (add-event-listener "update"
                       (lambda (event)
                         (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                           ((ps:@ *sse-manager* handle-html-update) data)))))

                     ;; Handle 'oob' event (out-of-band updates)
                     (ps:chain source (add-event-listener "oob"
                       (lambda (event)
                         (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                           ((ps:@ *sse-manager* handle-oob-updates) data)))))

                     ;; Handle 'trigger' event (custom events)
                     (ps:chain source (add-event-listener "trigger"
                       (lambda (event)
                         (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                           ((ps:@ *sse-manager* handle-trigger) data)))))

                     ;; Handle generic 'message' event (fallback)
                     (setf (ps:@ source onmessage)
                           (lambda (event)
                             (when on-message
                               (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                                 (funcall on-message data source)))))

                     ;; Handle errors (EventSource auto-reconnects)
                     (setf (ps:@ source onerror)
                           (lambda (event)
                             (ps:chain console (warn "SSE error:" channel event))
                             (when on-error
                               (funcall on-error event source))))

                     ;; Handle race condition: if connection opened before handlers were set
                     (when (and on-open (= (ps:@ source ready-state) 1))
                       (ps:chain console (log "SSE already connected:" channel))
                       (funcall on-open source))

                     source))

       ;; Disconnect from a channel
       "disconnect" (lambda (channel)
                      (let ((source (ps:getprop (ps:@ *sse-manager* connections) channel)))
                        (when source
                          (ps:chain source (close))
                          (delete (ps:getprop (ps:@ *sse-manager* connections) channel)))))

       ;; Handle HTML update event
       "handleHtmlUpdate" (lambda (data)
                            (let ((target (ps:chain document (get-element-by-id (ps:@ data target)))))
                              (when target
                                (let ((swap (or (ps:@ data swap) "innerHTML")))
                                  ((ps:@ *htmx* swap) target (ps:@ data html) swap)
                                  ;; Re-initialize HTMX on updated content
                                  ((ps:@ *htmx* process-element) target)))))

       ;; Handle OOB updates event
       "handleOobUpdates" (lambda (data)
                            (ps:chain (ps:@ data updates) (for-each
                              (lambda (update)
                                (let ((target (ps:chain document (get-element-by-id (ps:@ update target)))))
                                  (when target
                                    (let ((swap (or (ps:@ update swap) "outerHTML")))
                                      ((ps:@ *htmx* swap) target (ps:@ update html) swap)
                                      ;; Re-process the element
                                      (let ((new-el (ps:chain document (get-element-by-id (ps:@ update target)))))
                                        (when new-el
                                          ((ps:@ *htmx* process-element) new-el))))))))))

       ;; Handle event trigger
       "handleTrigger" (lambda (data)
                         (let ((event (ps:new (-Custom-Event (ps:@ data event)
                                                          (ps:create :detail (ps:@ data detail)
                                                                     :bubbles t)))))
                           (ps:chain document (dispatch-event event))))))))

;;; ============================================================================
;;; OPTIMISTIC UPDATE RUNTIME (Parenscript)
;;; ============================================================================

(defun optimistic-js ()
  "Generate optimistic update client code via Parenscript.
   Provides instant UI feedback before server response with automatic rollback."
  (parenscript:ps
    (defvar *optimistic*
      (ps:create
       ;; Store original states for rollback
       "originals" (ps:create)

       ;; Apply optimistic state to element
       "apply" (lambda (element config)
                 (let ((id (or (ps:@ element id)
                               (ps:chain -math (random) (to-string 36) (substr 2 9)))))
                   ;; Ensure element has ID for tracking
                   (unless (ps:@ element id)
                     (setf (ps:@ element id) id))
                   ;; Save original state
                   (setf (ps:getprop (ps:@ *optimistic* originals) id)
                         (ps:create
                          :text-content (ps:@ element text-content)
                          :inner-h-t-m-l (ps:@ element inner-h-t-m-l)
                          :class-name (ps:@ element class-name)
                          :disabled (ps:@ element disabled)
                          :value (ps:@ element value)))
                   ;; Apply optimistic changes
                   (when (ps:@ config text)
                     (setf (ps:@ element text-content) (ps:@ config text)))
                   (when (ps:@ config html)
                     (setf (ps:@ element inner-h-t-m-l) (ps:@ config html)))
                   (when (ps:@ config class)
                     (setf (ps:@ element class-name) (ps:@ config class)))
                   (when (ps:@ config add-class)
                     (ps:chain element class-list (add (ps:@ config add-class))))
                   (when (ps:@ config remove-class)
                     (ps:chain element class-list (remove (ps:@ config remove-class))))
                   (when (not (ps:=== undefined (ps:@ config disabled)))
                     (setf (ps:@ element disabled) (ps:@ config disabled)))
                   id))

       ;; Rollback to original state
       "rollback" (lambda (element-or-id)
                    (let* ((id (if (stringp element-or-id)
                                   element-or-id
                                   (ps:@ element-or-id id)))
                           (element (if (stringp element-or-id)
                                        (ps:chain document (get-element-by-id element-or-id))
                                        element-or-id))
                           (original (ps:getprop (ps:@ *optimistic* originals) id)))
                      (when (and element original)
                        (setf (ps:@ element text-content) (ps:@ original text-content))
                        (setf (ps:@ element inner-h-t-m-l) (ps:@ original inner-h-t-m-l))
                        (setf (ps:@ element class-name) (ps:@ original class-name))
                        (setf (ps:@ element disabled) (ps:@ original disabled))
                        (when (ps:@ original value)
                          (setf (ps:@ element value) (ps:@ original value)))
                        ;; Clean up stored state
                        (delete (ps:getprop (ps:@ *optimistic* originals) id)))))

       ;; Confirm optimistic change (clear stored original)
       "confirm" (lambda (element-or-id)
                   (let ((id (if (stringp element-or-id)
                                 element-or-id
                                 (ps:@ element-or-id id))))
                     (delete (ps:getprop (ps:@ *optimistic* originals) id))))

       ;; Wrap HTMX request with optimistic update
       "wrap" (lambda (element config)
                (let ((id ((ps:@ *optimistic* apply) element config)))
                  ;; Listen for HTMX events to confirm or rollback
                  (ps:chain element (add-event-listener "htmx:afterRequest"
                    (lambda (event)
                      (if (ps:@ event detail successful)
                          ((ps:@ *optimistic* confirm) id)
                          ((ps:@ *optimistic* rollback) id)))
                    (ps:create :once t)))))))))

;;; ============================================================================
;;; COMBINED RUNTIME
;;; ============================================================================

(defun lol-reactive-runtime-js ()
  "Generate the complete lol-reactive client runtime.
   Includes HTMX runtime, WebSocket client, SSE client, and optimistic updates."
  (concatenate 'string
               (htmx-runtime-js)
               ";"
               (ws-client-js)
               ";"
               (sse-client-js)
               ";"
               (optimistic-js)))
