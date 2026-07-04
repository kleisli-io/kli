(defpackage #:kli/runtime/relocation
  (:use #:cl)
  (:export
   #:*blessed-sonames*
   #:directoryless-namestring-p
   #:locate-relocated-lib
   #:reopen-blessed-libs))
