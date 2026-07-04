(in-package #:kli/tests)

(defun builtin-theme (file)
  (tui-style:load-theme (buildlisp/resources:resource-path "kli/tui/style" file)))

(defun make-tui-style-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context tui-style:*tui-style-extension-manifest*)
    (values protocol context)))

(test style-hex->color
  "Six-digit hex parses to RGB, three-digit shorthand expands (#abc to #aabbcc), and empty, named, or nil inputs yield nil."
  (let ((c (tui-style:hex->color "#8abeb7")))
    (is (= 138 (tui-style:color-r c)))
    (is (= 190 (tui-style:color-g c)))
    (is (= 183 (tui-style:color-b c))))
  (let ((c (tui-style:hex->color "#abc")))
    (is (= 170 (tui-style:color-r c)))
    (is (= 187 (tui-style:color-g c)))
    (is (= 204 (tui-style:color-b c))))
  (is (null (tui-style:hex->color "")))
  (is (null (tui-style:hex->color "blue")))
  (is (null (tui-style:hex->color nil))))

(test style-color->256-anchors
  (flet ((idx (hex) (tui-style:color->256 (tui-style:hex->color hex))))
    (is (= 16  (idx "#000000")))
    (is (= 231 (idx "#ffffff")))
    (is (= 196 (idx "#ff0000")))
    (is (= 244 (idx "#808080")))
    (is (= 109 (idx "#8abeb7")))
    (is (= 143 (idx "#b5bd68")))))

(test style-truecolor-sequences
  (let ((c (tui-style:hex->color "#8abeb7")))
    (is (string= (format nil "~C[38;2;138;190;183m" #\Esc)
                 (tui-style:fg-truecolor c)))
    (is (string= (format nil "~C[48;2;138;190;183m" #\Esc)
                 (tui-style:bg-truecolor c)))))

(test style-selective-resets
  (let ((tui-style:*color-mode* :truecolor)
        (accent (tui-style:hex->color "#8abeb7")))
    (is (string= (format nil "~C[38;2;138;190;183m~C[1mhi~C[22m~C[39m"
                         #\Esc #\Esc #\Esc #\Esc)
                 (tui-style:style-span "hi" :fg accent :attrs '(:bold))))
    (let ((out (tui-style:style-span "x" :fg accent :attrs '(:italic :underline))))
      (is (string= (format nil "~C[38;2;138;190;183m~C[3m~C[4mx~C[24m~C[23m~C[39m"
                           #\Esc #\Esc #\Esc #\Esc #\Esc #\Esc)
                   out))
      (is (null (search (format nil "~C[0m" #\Esc) out))
          "never emits the global ESC[0m reset"))
    (is (string= (format nil "~C[48;2;52;53;65m prompt ~C[49m" #\Esc #\Esc)
                 (tui-style:style-span " prompt " :bg (tui-style:hex->color "#343541"))))))

(test style-none-mode-is-plain
  (let ((tui-style:*color-mode* :none))
    (is (string= "hi"
                 (tui-style:style-span "hi"
                                       :fg (tui-style:hex->color "#8abeb7")
                                       :attrs '(:bold))))))

(test style-load-theme-dark
  "Loading the dark theme resolves var-referenced tokens (accent, border), maps a blank token to the default (nil), and reads literal hex tokens directly."
  (let ((theme (builtin-theme "dark.json")))
    (is (string= "dark" (tui-style:theme-name theme)))
    (let ((accent (tui-style:theme-token theme "accent")))
      (is (= 138 (tui-style:color-r accent)))
      (is (= 190 (tui-style:color-g accent)))
      (is (= 183 (tui-style:color-b accent))))
    (let ((border (tui-style:theme-token theme "border")))
      (is (= 95 (tui-style:color-r border)))
      (is (= 135 (tui-style:color-g border)))
      (is (= 255 (tui-style:color-b border))))
    (is (null (tui-style:theme-token theme "text")))
    (let ((label (tui-style:theme-token theme "customMessageLabel")))
      (is (= 149 (tui-style:color-r label)))
      (is (= 117 (tui-style:color-g label)))
      (is (= 205 (tui-style:color-b label))))))

(test style-load-theme-string
  "Loading a theme from a JSON string resolves a var-referenced token (accent), a literal hex token (error), and maps a blank token to the default (nil)."
  (let ((theme (tui-style:load-theme
                "{\"name\":\"t\",\"vars\":{\"a\":\"#8abeb7\"},\"colors\":{\"accent\":\"a\",\"error\":\"#cc6666\",\"text\":\"\"}}")))
    (is (string= "t" (tui-style:theme-name theme)))
    (is (= 138 (tui-style:color-r (tui-style:theme-token theme "accent"))))
    (is (= 204 (tui-style:color-r (tui-style:theme-token theme "error"))))
    (is (null (tui-style:theme-token theme "text")))))

(test style-load-theme-light
  (let ((theme (builtin-theme "light.json")))
    (is (string= "light" (tui-style:theme-name theme)))
    (let ((accent (tui-style:theme-token theme "accent")))
      (is (= 90 (tui-style:color-r accent)))
      (is (= 128 (tui-style:color-g accent)))
      (is (= 128 (tui-style:color-b accent))))
    (let ((label (tui-style:theme-token theme "customMessageLabel")))
      (is (= 126 (tui-style:color-r label)))
      (is (= 87 (tui-style:color-g label)))
      (is (= 194 (tui-style:color-b label))))))

(test style-token-completeness
  (let ((theme (builtin-theme "dark.json")))
    (dolist (name tui-style:+theme-token-names+)
      (is-true (nth-value 1 (gethash name (tui-style:theme-tokens theme)))
               "token ~S present after load" name))))

(test style-style-helper
  (let ((tui-style:*color-mode* :truecolor)
        (theme (builtin-theme "dark.json")))
    (is (string= (format nil "~C[38;2;204;102;102mx~C[39m" #\Esc #\Esc)
                 (tui-style:style theme "error" "x")))
    (is (string= (format nil "~C[38;2;204;102;102m~C[48;2;52;53;65mx~C[49m~C[39m"
                         #\Esc #\Esc #\Esc #\Esc)
                 (tui-style:style theme "error" "x" :bg "userMessageBg")))))

(test style-extension-registers-builtin-themes
  (let ((protocol (make-tui-style-fixture)))
    (is (member "dark" (tui-style:list-themes protocol) :test #'string=))
    (is (member "light" (tui-style:list-themes protocol) :test #'string=))
    (is (string= "dark" (tui-style:theme-name (tui-style:active-theme protocol))))))

(test style-theme-contribution-roundtrip
  (multiple-value-bind (protocol context) (make-tui-style-fixture)
    (let* ((custom (tui-style:load-theme
                    "{\"name\":\"custom\",\"vars\":{},\"colors\":{\"accent\":\"#abcdef\"}}"))
           (contrib (tui-style:make-theme-contribution
                     :name :custom :theme custom :source :test)))
      (is (null (tui-style:find-theme protocol "custom")))
      (ext:install-contribution protocol contrib context)
      (is (eq custom (tui-style:find-theme protocol "custom")))
      (ext:retract-contribution protocol contrib context)
      (is (null (tui-style:find-theme protocol "custom"))))))

(test style-set-active-theme
  (let ((protocol (make-tui-style-fixture)))
    (tui-style:set-active-theme protocol "light")
    (is (string= "light" (tui-style:theme-name (tui-style:active-theme protocol))))
    (signals error (tui-style:set-active-theme protocol "nonexistent"))))
