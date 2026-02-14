(defpackage #:depot
  (:use #:cl)
  (:export
   ;; Git utilities
   #:find-git-root
   ;; Depot detection (from CWD)
   #:find-depot-root
   #:find-world-root
   #:coordination-root
   ;; Depot detection (from path parameter - for hooks)
   #:find-depot-root-from
   #:find-world-root-from
   #:coordination-root-from
   ;; Depot validation
   #:valid-depot-name-p
   #:resolve-depot-root
   ;; Sibling depot discovery
   #:list-sibling-depots
   #:depot-has-tasks-p
   ;; Task-specific depot utilities
   #:depot-tasks-root
   #:depot-meta-root
   #:list-depots-with-tasks
   ;; Utilities
   #:strip-trailing-slash))
