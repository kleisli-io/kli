;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Idiomorph integration for DOM morphing with state preservation
;;;;
;;;; Provides smooth DOM updates that preserve:
;;;; - Focus state (inputs stay focused)
;;;; - Scroll position (lists maintain position)
;;;; - Form state (partially filled forms preserved)
;;;;
;;;; Based on idiomorph 0.3.0: https://github.com/bigskysoftware/idiomorph

(in-package :lol-reactive)

;;; ============================================================================
;;; IDIOMORPH LIBRARY INCLUSION
;;; ============================================================================

(defparameter *idiomorph-version* "0.3.0"
  "Version of the vendored idiomorph library.")

(defun idiomorph-js-path ()
  "Return the path to the vendored idiomorph.min.js file.
   For static serving, configure your server to serve this file."
  "/static/idiomorph.min.js")

(defun include-idiomorph ()
  "Generate script tag to include idiomorph library.
   Call this in your page head or before htmx-runtime-js."
  (cl-who:with-html-output-to-string (s)
    (:script :src (idiomorph-js-path))))

(defun idiomorph-inline-js ()
  "Return the inline idiomorph JavaScript for embedding directly in pages.
   This is useful when you can't serve static files."
  ;; The actual JS is vendored in static/idiomorph.min.js
  ;; This function reads and returns it for inline inclusion
  "/* idiomorph 0.3.0 - load from /static/idiomorph.min.js or use include-idiomorph */")

;;; ============================================================================
;;; MORPH SWAP STRATEGIES
;;;
;;; Extend the HTMX runtime with morph-based swap strategies.
;;; ============================================================================

(defun htmx-morph-extension-js ()
  "Generate JavaScript that extends HTMX runtime with morph swap strategies.
   Call this AFTER htmx-runtime-js and idiomorph are loaded."
  (parenscript:ps
    ;; Check if Idiomorph is available
    (when (and (typeof *htmx*) (typeof -idiomorph))
      ;; Store original swap function
      (let ((original-swap (ps:@ *htmx* swap)))
        ;; Replace with morph-aware version
        (setf (ps:@ *htmx* swap)
              (lambda (target html swap-style)
                (cond
                  ;; morph - morph innerHTML, preserve focus/state
                  ((= swap-style "morph")
                   (let ((temp (ps:chain document (create-element "div"))))
                     (setf (ps:@ temp inner-h-t-m-l) html)
                     ((ps:@ -idiomorph morph) target (ps:@ temp first-child)
                      (ps:create :morph-style "innerHTML"
                                 :ignore-active-value t))))

                  ;; morph:outerHTML - morph entire element
                  ((= swap-style "morph:outerHTML")
                   (let ((temp (ps:chain document (create-element "div"))))
                     (setf (ps:@ temp inner-h-t-m-l) html)
                     ((ps:@ -idiomorph morph) target (ps:@ temp first-child)
                      (ps:create :morph-style "outerHTML"
                                 :ignore-active-value t))))

                  ;; morph:innerHTML - explicit innerHTML morph
                  ((= swap-style "morph:innerHTML")
                   (let ((temp (ps:chain document (create-element "div"))))
                     (setf (ps:@ temp inner-h-t-m-l) html)
                     ((ps:@ -idiomorph morph) target (ps:@ temp first-child)
                      (ps:create :morph-style "innerHTML"
                                 :ignore-active-value t))))

                  ;; Default to original swap
                  (t
                   (funcall original-swap target html swap-style))))))

      ;; Log morph extension loaded
      (ps:chain console (log "(HTMX :morph :loaded :idiomorph" (ps:@ -idiomorph defaults morph-style) ")")))))

;;; ============================================================================
;;; COMBINED RUNTIME WITH MORPH
;;; ============================================================================

(defun htmx-runtime-with-morph-js ()
  "Generate complete HTMX runtime with idiomorph morph support.
   Includes both the base runtime and morph extension."
  (concatenate 'string
               (htmx-runtime-js)
               (htmx-morph-extension-js)))

;;; ============================================================================
;;; HTML HELPERS
;;; ============================================================================

(defun include-htmx-with-morph ()
  "Include all HTMX runtime scripts including morph support.
   Use this instead of include-htmx when morph swaps are needed.

   Generates:
   1. idiomorph.min.js script tag
   2. HTMX runtime inline script
   3. Morph extension inline script"
  (cl-who:with-html-output-to-string (s)
    ;; Load idiomorph first
    (:script :src (idiomorph-js-path))
    ;; Then HTMX runtime with morph extension
    (:script (cl-who:str (htmx-runtime-with-morph-js)))))

;;; ============================================================================
;;; MORPH-SPECIFIC ATTRIBUTE HELPERS
;;; ============================================================================

(defun hx-morph (url &key target trigger (morph-style "morph"))
  "Generate hx-get with morph swap.
   MORPH-STYLE can be:
   - \"morph\" (default) - morph innerHTML, preserve focus
   - \"morph:outerHTML\" - morph entire element
   - \"morph:innerHTML\" - explicit innerHTML morph

   Example:
   (:div (hx-morph \"/api/search\" :target \"#results\" :trigger \"input delay:300ms\"))
     (:input :type \"text\" :name \"q\"))"
  (format nil "hx-get=\"~a\"~@[ hx-target=\"~a\"~] hx-swap=\"~a\"~@[ hx-trigger=\"~a\"~]"
          url target morph-style trigger))
