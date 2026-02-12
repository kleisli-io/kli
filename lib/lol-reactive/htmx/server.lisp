;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; HTMX server-side helpers for request detection and response generation

(in-package :lol-reactive)

;;; ============================================================================
;;; REQUEST DETECTION
;;;
;;; Detect HTMX requests via HX-* headers sent by the client runtime.
;;; ============================================================================

(defun htmx-request-p ()
  "Check if current request is from HTMX client.
   Returns T if HX-Request header is 'true'."
  (string= "true" (request-header "HX-Request")))

(defun htmx-target ()
  "Get target element ID from HTMX request.
   Returns the value of HX-Target header, or NIL if not present."
  (request-header "HX-Target"))

(defun htmx-trigger ()
  "Get triggering element ID from HTMX request.
   Returns the value of HX-Trigger header, or NIL if not present."
  (request-header "HX-Trigger"))

(defun htmx-current-url ()
  "Get current browser URL from HTMX request.
   Returns the value of HX-Current-URL header, or NIL if not present."
  (request-header "HX-Current-URL"))

;;; ============================================================================
;;; RESPONSE HELPERS
;;;
;;; Generate HTMX-compatible responses with proper headers.
;;; ============================================================================

(defmacro with-htmx-response ((&key trigger retarget reswap push-url refresh) &body body)
  "Execute BODY with HTMX response headers set.

   Options:
   - TRIGGER: Event name(s) to trigger on client (string or plist)
   - RETARGET: CSS selector to retarget the response to
   - RESWAP: Change the swap strategy (e.g., 'outerHTML')
   - PUSH-URL: URL to push to browser history
   - REFRESH: If T, trigger a full page refresh

   Returns an html-response with the body content and accumulated headers.

   Example:
   (with-htmx-response (:trigger \"cartUpdated\" :reswap \"innerHTML\")
     (render-cart-items))"
  `(progn
     ,@(when trigger
         `((add-response-header "HX-Trigger"
                                ,(if (stringp trigger)
                                     trigger
                                     `(cl-json:encode-json-to-string ,trigger)))))
     ,@(when retarget
         `((add-response-header "HX-Retarget" ,retarget)))
     ,@(when reswap
         `((add-response-header "HX-Reswap" ,reswap)))
     ,@(when push-url
         `((add-response-header "HX-Push-Url" ,push-url)))
     ,@(when refresh
         `((add-response-header "HX-Refresh" "true")))
     (html-response (progn ,@body))))

(defun set-htmx-trigger (event-name &optional event-detail)
  "Set HX-Trigger response header to fire client-side event.
   EVENT-NAME: String name of event to trigger
   EVENT-DETAIL: Optional plist of event detail data

   Must be called within with-response-headers context."
  (add-response-header "HX-Trigger"
                       (if event-detail
                           (cl-json:encode-json-to-string
                            (list (cons event-name event-detail)))
                           event-name)))

(defun set-htmx-redirect (url)
  "Set HX-Redirect header to redirect the browser.
   Unlike HX-Location, this performs a full page redirect.

   Must be called within with-response-headers context."
  (add-response-header "HX-Redirect" url))

(defun set-htmx-location (url &key target swap)
  "Set HX-Location header for client-side navigation.
   URL: Target URL
   TARGET: Optional CSS selector for swap target
   SWAP: Optional swap strategy

   Must be called within with-response-headers context."
  (add-response-header "HX-Location"
                       (if (or target swap)
                           (cl-json:encode-json-to-string
                            (alexandria:alist-hash-table
                             (remove nil
                                     (list (cons "path" url)
                                           (when target (cons "target" target))
                                           (when swap (cons "swap" swap))))))
                           url)))

;;; ============================================================================
;;; OOB RESPONSE RENDERING
;;;
;;; Combine primary content with out-of-band updates.
;;; ============================================================================

(defun render-with-oob (main-content &rest oob-updates)
  "Render main content with out-of-band updates.

   MAIN-CONTENT: Primary HTML content (string)
   OOB-UPDATES: List of OOB update specifications:
     - (id content) - default outerHTML swap
     - (id content :swap strategy) - specific swap strategy

   Example:
   (render-with-oob
     (render-cart-item product-id)
     (list \"cart-count\" (format nil \"~a\" (cart-count)))
     (list \"cart-total\" (format nil \"$~,2F\" (cart-total)) :swap \"innerHTML\")
     (list \"cart-dropdown\" (render-cart-dropdown) :swap \"outerHTML\"))"
  (with-output-to-string (s)
    ;; Primary content
    (write-string (or main-content "") s)
    ;; OOB updates
    (dolist (update oob-updates)
      (destructuring-bind (id content &key (swap "true")) update
        (write-string (oob-swap id content :swap swap) s)))))

(defmacro render-oob-only (&rest oob-updates)
  "Render only out-of-band updates (no primary target content).
   Use with hx-swap='none' on the triggering element.

   Each OOB-UPDATE is: (id content &key swap)

   Example:
   (render-oob-only
     (\"cart-count\" (format nil \"~a\" count))
     (\"cart-total\" (format nil \"$~,2F\" total) :swap \"innerHTML\"))"
  `(render-with-oob nil ,@(mapcar (lambda (u) `(list ,@u)) oob-updates)))

;;; ============================================================================
;;; CONDITIONAL RENDERING
;;;
;;; Helpers for responding differently to HTMX vs regular requests.
;;; ============================================================================

(defmacro htmx-or-redirect (htmx-body redirect-url)
  "If HTMX request, evaluate HTMX-BODY. Otherwise, redirect.

   Example:
   (defroute \"/api/cart/add\" :post (product-id)
     (add-to-cart product-id)
     (htmx-or-redirect
       (render-oob-only
         (\"cart-count\" (format nil \"~a\" (cart-count))))
       \"/cart\"))"
  `(if (htmx-request-p)
       ,htmx-body
       (redirect-response ,redirect-url)))

(defmacro htmx-or-full-page (htmx-body full-page-body)
  "If HTMX request, return partial. Otherwise, render full page.

   Example:
   (defroute \"/products/:id\" :get (id)
     (let ((product (get-product id)))
       (htmx-or-full-page
         (render-product-card product)  ; Partial for HTMX
         (html-page :body (render-product-page product)))))"
  `(if (htmx-request-p)
       ,htmx-body
       ,full-page-body))
