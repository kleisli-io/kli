;;;; KLI Dashboard — Package definition
;;;; Redesigned task dashboard with KLI landing page aesthetic.
;;;; Direct calls to task: functions — no Swank RPC.

(defpackage :kli-dashboard
  (:use :cl)
  (:import-from :lol-reactive
    #:htm-str #:html-page #:defroute #:start-server #:stop-server
    #:defcss #:generate-all-component-css #:generate-css-variables #:classes
    #:*colors* #:*typography* #:*spacing* #:*effects*
    #:get-color #:get-font #:get-spacing #:get-effect
    #:reactive-script #:make-token-set)
  (:export #:start #:stop #:reload))
