(defpackage #:crdt
  (:use #:cl)
  (:export
   ;; Vector Clock
   #:vector-clock #:make-vector-clock #:vc-entries
   #:vc-increment #:vc-get #:vc-merge
   #:vc-equal-p
   ;; G-Set
   #:g-set #:make-g-set
   #:gs-add #:gs-contains-p #:gs-members #:gs-count #:gs-merge
   ;; LWW-Register
   #:lww-register #:make-lww-register
   #:lww-value #:lww-timestamp #:lww-session
   #:lww-set #:lww-merge
   ;; OR-Set
   #:or-set #:make-or-set
   #:ors-add #:ors-remove #:ors-contains-p #:ors-members #:ors-merge
   ;; LWW-Map
   #:lww-map #:make-lww-map
   #:lwwm-set #:lwwm-get #:lwwm-keys #:lwwm-merge))
