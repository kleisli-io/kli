(defpackage #:kli/tui/style
  (:use #:cl)
  (:import-from #:kli/ext
                #:defextension #:defcontribution-kind
                #:contribution
                #:install-contribution #:retract-contribution
                #:extension-protocol #:protocol-installed-contributions
                #:protocol-storage #:ensure-protocol-storage
                #:normalize-extension-id)
  (:import-from #:buildlisp/resources
                #:resource-path)
  (:export
   #:color #:make-color #:color-r #:color-g #:color-b #:hex->color
   #:color->256 #:detect-color-mode #:*color-mode*
   #:fg-truecolor #:bg-truecolor #:fg-256 #:bg-256 #:sgr #:style-span
   #:+fg-reset+ #:+bg-reset+
   #:theme #:theme-name #:theme-tokens #:load-theme #:theme-token #:style
   #:+theme-token-names+
   #:register-theme #:unregister-theme #:find-theme #:list-themes
   #:active-theme #:set-active-theme #:protocol-theme-registry #:+theme-mode-key+
   #:theme-contribution #:make-theme-contribution #:contribution-theme
   #:*tui-style-extension-manifest*))
