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
;;; Git Root Discovery Tests
;;; ==========================================================================

(test find-git-root-returns-string-or-nil
  "Test that find-git-root returns string or NIL"
  (let ((result (find-git-root)))
    (is (or (null result)
            (stringp result)))))

(test find-git-root-from-returns-string-or-nil
  "Test that find-git-root-from returns string or NIL"
  (let ((result (find-git-root-from (namestring (uiop:getcwd)))))
    (is (or (null result)
            (stringp result)))))

(test find-git-root-from-nil-returns-nil
  "Test that find-git-root-from with NIL returns NIL"
  (is (null (find-git-root-from nil))))

;;; ==========================================================================
;;; Coordination Root Tests
;;; ==========================================================================

(test coordination-root-returns-string
  "Test that coordination-root returns a string"
  (let ((result (coordination-root)))
    (is (stringp result))))

(test coordination-root-from-returns-string-or-nil
  "Test that coordination-root-from returns string or NIL"
  (let ((result (coordination-root-from (namestring (uiop:getcwd)))))
    (is (or (null result)
            (stringp result)))))
