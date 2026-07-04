(defpackage #:kli/tui/ansi
  (:use #:cl)
  (:import-from #:kli/text
                #:normalize-text)
  (:import-from #:kli/tui/style
                #:make-color #:color-r #:color-g #:color-b)
  (:import-from #:kli/tui/markdown
                #:make-seg)
  (:export #:ansi->segs))

(in-package #:kli/tui/ansi)
