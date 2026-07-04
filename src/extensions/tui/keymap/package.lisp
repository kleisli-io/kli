(defpackage #:kli/tui/keymap
  (:use #:cl)
  (:import-from #:kli/ext
                #:defextension #:defcontribution-kind
                #:contribution
                #:install-contribution #:retract-contribution
                #:extension-protocol #:protocol-installed-contributions
                #:ensure-protocol-storage
                #:normalize-extension-id)
  (:export
   #:*default-keymap* #:+keymap-actions+
   #:keymap-action #:keymap-recognized-p
   #:register-keybinding #:unregister-keybinding
   #:load-keymap #:protocol-keymap-overrides
   #:keybinding-contribution #:make-keybinding-contribution
   #:contribution-key-id #:contribution-action
   #:keybinding
   #:*tui-keymap-extension-manifest*))
