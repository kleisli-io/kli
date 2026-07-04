(in-package #:kli/tests)

(in-suite all)

;;; Named removable layers. An extension that recodes a shared slot installs a
;;; layer keyed by its contribution id and retracts by remove-by-label, so
;;; retracting contributions out of install order never strands a sibling. These
;;; assert composition order, order-independent removal, idempotent re-install,
;;; and that a contribution-id label drains by exactly that id.

(defun tagging-layer (tag)
  "A layer transform that appends TAG to the running string."
  (lambda (value) (concatenate 'string value tag)))

(defun abc-stack ()
  "A stack of three layers installed in order a, b, c."
  (let* ((a (ext:install-layer (ext:make-layer-stack) :a (tagging-layer "A")))
         (ab (ext:install-layer a :b (tagging-layer "B"))))
    (ext:install-layer ab :c (tagging-layer "C"))))

(test layers-compose-in-install-order
  (is (string= "base:ABC" (ext:compose-layers (abc-stack) "base:")))
  (is (equal '(:a :b :c) (ext:layer-stack-labels (abc-stack))))
  (is (string= "base:" (ext:compose-layers (ext:make-layer-stack) "base:"))
      "an empty stack returns the base unchanged"))

(test removing-middle-layer-keeps-survivors-in-order
  (let ((without-b (ext:remove-layer-by-label (abc-stack) :b)))
    (is (equal '(:a :c) (ext:layer-stack-labels without-b)))
    (is (string= "base:AC" (ext:compose-layers without-b "base:"))
        "the surviving layers keep their original relative order")))

(test removal-is-order-independent
  (let* ((stack (abc-stack))
         (a-then-c (ext:remove-layer-by-label
                    (ext:remove-layer-by-label stack :a) :c))
         (c-then-a (ext:remove-layer-by-label
                    (ext:remove-layer-by-label stack :c) :a)))
    (is (equal (ext:layer-stack-labels a-then-c)
               (ext:layer-stack-labels c-then-a))
        "dropping the same set in either order yields the same labels")
    (is (string= (ext:compose-layers a-then-c "base:")
                 (ext:compose-layers c-then-a "base:"))
        "and the same composition")))

(test reinstalling-a-label-replaces-in-place
  (let ((reinstalled (ext:install-layer (abc-stack) :b (tagging-layer "B2"))))
    (is (equal '(:a :b :c) (ext:layer-stack-labels reinstalled))
        "re-install keeps the label in its original position")
    (is (string= "base:AB2C" (ext:compose-layers reinstalled "base:"))
        "re-install replaces the transform with no double-append")))

(test position-first-installs-earliest
  (let ((stack (ext:install-layer (abc-stack) :z (tagging-layer "Z")
                                  :position :first)))
    (is (string= "base:ZABC" (ext:compose-layers stack "base:")))
    (is (equal '(:z :a :b :c) (ext:layer-stack-labels stack)))))

(test append-kind-layer-concatenates-a-thunk-block
  "An :append-kind layer's transform is a thunk of no arguments; compose
concatenates its fresh block onto the running value. The default kind stays
:transform, so the (value)->value layers above are unaffected."
  (let ((stack (ext:install-layer (ext:make-layer-stack) :tag
                                  (lambda () "BLOCK") :kind :append)))
    (is (string= "base:BLOCK" (ext:compose-layers stack "base:")))
    (is (string= "base:BLOCK" (ext:compose-layers stack "base:"))
        "recomposing from the same base is a fixpoint")))

(test contribution-id-label-drains-by-its-id
  (let* ((id '(:context-files . :system-prompt))
         (stack (ext:install-layer (ext:make-layer-stack) id (tagging-layer "X"))))
    (is (ext:layer-stack-member-p stack id)
        "a layer keyed by a contribution id is present")
    (is (not (ext:layer-stack-member-p (ext:remove-layer-by-label stack id) id))
        "retracting that contribution id drains exactly its layer")))
