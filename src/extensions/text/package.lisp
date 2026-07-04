(defpackage #:kli/text
  (:use #:cl)
  (:export
   #:character-width
   #:visible-width
   #:truncate-to-width
   #:wrap-text
   #:pad-right
   #:wrap-logical-line
   #:hard-wrap
   #:split-lines
   #:*render-line-limit*
   #:render-truncate-front
   #:render-bounded-lines
   #:split-words
   #:split-on-whitespace
   #:whitespace-char-p
   #:trim-whitespace
   #:blank-string-p
   #:normalize-text
   #:printable-string-p
   #:parse-positive-integer
   #:string-prefix-p
   #:string-suffix-p
   #:jsonify))
