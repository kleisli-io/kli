(defpackage #:kli/tui/markdown
  (:use #:cl)
  (:import-from #:kli/ext
                #:defextension)
  (:import-from #:kli/tui/style
                #:style-span #:theme-token)
  (:import-from #:kli/text
                #:character-width #:visible-width #:split-lines
                #:normalize-text #:string-prefix-p)
  (:import-from #:3bmd-grammar
                #:parse-doc)
  (:import-from #:3bmd-code-blocks
                #:*code-blocks* #:code-block)
  (:import-from #:3bmd-tables
                #:*tables*)
  (:import-from #:3bmd
                #:table)
  (:import-from #:colorize
                #:scan-string #:find-coloring-type)
  (:import-from #:difflib
                #:sequence-matcher #:set-sequences #:get-opcodes
                #:opcode-tag #:opcode-i1 #:opcode-i2 #:opcode-j1 #:opcode-j2)
  (:export
   #:seg #:make-seg #:seg-text #:seg-fg #:seg-attrs
   #:render-seg-line #:seg-line-width #:char-wrap-segs
   #:markdown->lines
   #:*md-stream*
   #:make-mdstream
   #:*hl-stream*
   #:make-hl-stream
   #:*highlight-memoize-p*
   #:render-diff #:render-diff-hunk-blocks #:render-unified-diff #:unified-diff-p
   #:*tui-markdown-extension-manifest*
   #:highlight-code-lines #:prefix-wrap-segs))

(in-package #:kli/tui/markdown)
