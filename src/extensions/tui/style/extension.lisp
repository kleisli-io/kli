(in-package #:kli/tui/style)

(defextension tui-style
  (:provides
   (theme :dark  (load-theme (resource-path "kli/tui/style" "dark.json")))
   (theme :light (load-theme (resource-path "kli/tui/style" "light.json")))))
