(in-package #:depot-tests)

(def-suite :depot-tests
  :description "Tests for depot discovery library")

(in-suite :depot-tests)

;;; ==========================================================================
;;; Utility Tests
;;; ==========================================================================

(test strip-trailing-slash-basic
  "Test strip-trailing-slash with various inputs"
  (is (string= "foo" (strip-trailing-slash "foo/")))
  (is (string= "foo" (strip-trailing-slash "foo")))
  (is (string= "/home/user" (strip-trailing-slash "/home/user/")))
  (is (string= "/home/user" (strip-trailing-slash "/home/user")))
  ;; Root should stay as-is
  (is (string= "/" (strip-trailing-slash "/"))))

;;; ==========================================================================
;;; Validation Tests
;;; ==========================================================================

(test valid-depot-name-p-basic
  "Test depot name validation"
  ;; Valid names
  (is-true (valid-depot-name-p "core"))
  (is-true (valid-depot-name-p "amti"))
  (is-true (valid-depot-name-p "my-depot"))
  (is-true (valid-depot-name-p "depot_2"))
  (is-true (valid-depot-name-p "Depot123"))
  ;; Invalid names
  (is-false (valid-depot-name-p ""))
  (is-false (valid-depot-name-p "123depot"))    ; starts with number
  (is-false (valid-depot-name-p "-depot"))      ; starts with hyphen
  (is-false (valid-depot-name-p "_depot"))      ; starts with underscore
  (is-false (valid-depot-name-p "depot/path"))  ; contains slash
  (is-false (valid-depot-name-p "depot.."))     ; contains dots
  (is-false (valid-depot-name-p nil)))          ; not a string

;;; ==========================================================================
;;; Discovery Tests (Integration)
;;; ==========================================================================
;;; These tests may depend on the current working environment

(test find-depot-root-returns-string-or-nil
  "Test that find-depot-root returns string or NIL"
  (let ((result (find-depot-root)))
    (is (or (null result)
            (stringp result)))))

(test find-world-root-returns-string-or-nil
  "Test that find-world-root returns string or NIL"
  (let ((result (find-world-root)))
    (is (or (null result)
            (stringp result)))))

(test list-sibling-depots-returns-list
  "Test that list-sibling-depots returns a list"
  (let ((result (list-sibling-depots)))
    (is (or (null result)
            (listp result)))
    ;; If in depot-of-depots, should include at least current depot
    (when result
      (is (every #'stringp result)))))

;;; ==========================================================================
;;; Resolution Tests
;;; ==========================================================================

(test resolve-depot-root-nil-returns-current
  "Test that resolve-depot-root with NIL returns current depot"
  (let ((current (find-depot-root))
        (resolved (resolve-depot-root nil)))
    (is (equal current resolved))))

(test resolve-depot-root-invalid-signals-error
  "Test that invalid depot names signal error"
  (let ((world-root (find-world-root)))
    (when world-root  ; Only test if in depot-of-depots
      (signals error (resolve-depot-root "123invalid"))
      (signals error (resolve-depot-root "../escape")))))
