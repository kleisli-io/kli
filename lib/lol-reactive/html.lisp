;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; HTML generation layer using cl-who
;;;;
;;;; GENERIC INFRASTRUCTURE - NO hardcoded colors, fonts, or theme styles.
;;;; Apps provide their own visual identity by setting *colors*, *typography*, etc.

(in-package :lol-reactive)

;;; ============================================================================
;;; CL-WHO CONFIGURATION
;;; ============================================================================

;; Use HTML5 mode
(setf cl-who:*attribute-quote-char* #\"
      cl-who:*html-empty-tag-aware-p* t)

;;; ============================================================================
;;; PAGE TEMPLATE (GENERIC)
;;;
;;; Provides infrastructure only. Apps define aesthetics via:
;;; - Setting *colors*, *typography*, etc. before rendering
;;; - Registering CSS modules via defcss
;;; - Passing custom head-extra content
;;; ============================================================================

(defun html-page (&key (title "LOL-REACTIVE")
                       (lang "en")
                       (body-class "")
                       head-extra
                       body
                       css-href
                       (include-tailwind t)
                       (include-htmx t)
                       (include-surgery nil))
  "Generate a complete HTML page with token-driven CSS variables.

   NO hardcoded colors, fonts, or styles - apps provide their own theme.

   Options:
   - TITLE: Page title
   - LANG: HTML lang attribute (default \"en\")
   - BODY-CLASS: Additional body CSS classes
   - HEAD-EXTRA: Custom head content (string)
   - BODY: Page body content (string)
   - CSS-HREF: Compiled CSS stylesheet path (when provided, suppresses CDN)
   - INCLUDE-TAILWIND: Include Tailwind CDN (default t, ignored when CSS-HREF set)
   - INCLUDE-HTMX: Include HTMX-style runtime (default t)
   - INCLUDE-SURGERY: Include surgery panel runtime (default nil)"
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:html :lang lang
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title (cl-who:str title))

       ;; Compiled CSS (when provided, replaces CDN)
       (when css-href
         (cl-who:htm
          (:link :rel "stylesheet" :href css-href)))

       ;; Tailwind CDN (only when no compiled CSS)
       (when (and include-tailwind (not css-href))
         (cl-who:htm
          (:script :src "https://cdn.tailwindcss.com")
          (:script (cl-who:str (tailwind-config)))))

       ;; CSS variables from tokens
       (:style (cl-who:str (generate-css-variables)))

       ;; Registered component CSS
       (:style (cl-who:str (generate-all-component-css)))

       ;; HTMX indicator styles
       (when include-htmx
         (cl-who:htm
          (:style (cl-who:str (htmx-indicator-css)))))

       ;; App-provided head content
       (cl-who:str (or head-extra "")))

      (:body :class body-class
        ;; Main content
        (cl-who:str (or body ""))

        ;; Reactive runtime script (Parenscript)
        (:script (cl-who:str (reactive-runtime-js)))

        ;; HTMX runtime (optional, default on)
        (when include-htmx
          (cl-who:htm
           (:script (cl-who:str (htmx-runtime-js)))))

        ;; Surgery panel (optional)
        (when include-surgery
          (cl-who:htm
           (:style (cl-who:str (surgery-css)))
           (:script (cl-who:str (surgery-runtime-js)))))))))

;;; ============================================================================
;;; CL-WHO SHORTHAND MACROS
;;; ============================================================================

(defmacro htm (&body body)
  "Shorthand for cl-who output to *standard-output*."
  `(cl-who:with-html-output (*standard-output*) ,@body))

(defmacro htm-str (&body body)
  "Generate HTML and return as string."
  `(cl-who:with-html-output-to-string (s) ,@body))

;;; ============================================================================
;;; COMPONENT RENDERING
;;; ============================================================================

(defun render-component (component)
  "Render a component to HTML string."
  (funcall component :render))

(defun component->html (component &key (wrapper t))
  "Convert a component to HTML, optionally wrapping in a container."
  (let ((html (render-component component))
        (id (funcall component :id)))
    (if wrapper
        (cl-who:with-html-output-to-string (s)
          (:div :id id
                :class "component-wrapper"
                :data-component-id id
            (cl-who:str html)))
        html)))

;;; ============================================================================
;;; REACTIVE RUNTIME (Parenscript)
;;;
;;; Client-side reactivity. ALL JavaScript generated via Parenscript.
;;; ============================================================================

(defun reactive-runtime-js ()
  "Generate the client-side reactive runtime via Parenscript.
   NO raw JavaScript strings."
  (parenscript:ps
    ;; LOL-REACTIVE Runtime
    (defvar *lol-reactive*
      (ps:create
       :components (ps:new (-Map))

       :register (lambda (id handlers)
                   (ps:chain (ps:@ this components) (set id handlers)))

       :dispatch (lambda (component-id action &rest args)
                   (ps:chain
                    (fetch "/api/dispatch"
                           (ps:create :method "POST"
                                      :headers (ps:create "Content-Type" "application/json")
                                      :body (ps:chain -j-s-o-n (stringify
                                             (ps:create :component-id component-id
                                                        :action action
                                                        :args args)))))
                    (then (lambda (r) (ps:chain r (json))))
                    (then (lambda (data)
                            (when (ps:@ data html)
                              (let ((el (ps:chain document (query-selector
                                         (+ "[data-component-id=\"" component-id "\"]")))))
                                (when el
                                  (setf (ps:@ el inner-h-t-m-l) (ps:@ data html)))))))))

       :set-state (lambda (component-id key value)
                    (ps:chain
                     (fetch "/api/set-state"
                            (ps:create :method "POST"
                                       :headers (ps:create "Content-Type" "application/json")
                                       :body (ps:chain -j-s-o-n (stringify
                                              (ps:create :component-id component-id
                                                         :key key
                                                         :value value)))))
                     (then (lambda (r) (ps:chain r (json))))
                     (then (lambda (data)
                             (when (ps:@ data html)
                               (let ((el (ps:chain document (query-selector
                                          (+ "[data-component-id=\"" component-id "\"]")))))
                                 (when el
                                   (setf (ps:@ el inner-h-t-m-l) (ps:@ data html)))))))))))

    ;; Shorthand - only set if LOL-REACTIVE exists
    (defvar dispatch
      (when *lol-reactive*
        (ps:chain (ps:@ *lol-reactive* dispatch) (bind *lol-reactive*))))
    (defvar set-state
      (when *lol-reactive*
        (ps:chain (ps:getprop *lol-reactive* "set-state") (bind *lol-reactive*))))

    (ps:chain console (log "(LOL-REACTIVE :status :loaded)"))))

;;; ============================================================================
;;; S-EXPRESSION HIGHLIGHTING (generic utility)
;;; ============================================================================

(defun highlight-sexp (form)
  "Convert a Lisp form to syntax-highlighted HTML.
   Uses CSS classes that apps can style as needed."
  (let ((str (prin1-to-string form)))
    ;; Simple highlighting - keywords, strings, numbers
    (setf str (cl-ppcre:regex-replace-all
               ":(\\w+)"
               str
               "<span class=\"sexp-keyword\">:\\1</span>"))
    (setf str (cl-ppcre:regex-replace-all
               "\"([^\"]*)\""
               str
               "<span class=\"sexp-string\">\"\\1\"</span>"))
    (setf str (cl-ppcre:regex-replace-all
               "\\b(\\d+)\\b"
               str
               "<span class=\"sexp-number\">\\1</span>"))
    str))
