;;;; claude-hooks Tests - JSON Utilities

(in-package :claude-hooks.tests)
(in-suite :json)

(test make-ht-basic
  "make-ht creates hash-table with correct entries."
  (let ((ht (make-ht "a" 1 "b" 2)))
    (is (= 1 (gethash "a" ht)))
    (is (= 2 (gethash "b" ht)))
    (is (= 2 (hash-table-count ht)))))

(test make-ht-empty
  "make-ht with no args creates empty hash-table."
  (let ((ht (make-ht)))
    (is (= 0 (hash-table-count ht)))))

(test make-ht-nested
  "make-ht with nested hash-tables."
  (let ((ht (make-ht "outer" (make-ht "inner" 42))))
    (is (= 42 (gethash "inner" (gethash "outer" ht))))))

(test make-ht-boolean-nil
  "make-ht preserves t, nil, and yason:false."
  (let ((ht (make-ht "yes" t "no" yason:false "null" nil)))
    (is (eq t (gethash "yes" ht)))
    (is (eq yason:false (gethash "no" ht)))
    (is (null (gethash "null" ht)))))

(test jref-basic
  "jref accesses top-level keys."
  (let ((ht (make-ht "x" 42)))
    (is (= 42 (jref ht "x")))))

(test jref-nested
  "jref traverses nested hash-tables."
  (let ((ht (make-ht "a" (make-ht "b" (make-ht "c" "deep")))))
    (is (string= "deep" (jref ht "a" "b" "c")))))

(test jref-missing-key
  "jref returns nil for missing keys."
  (let ((ht (make-ht "a" 1)))
    (is (null (jref ht "missing")))
    (is (null (jref ht "missing" "deep")))))

(test jref-nil-safe
  "jref on nil returns nil."
  (is (null (jref nil "a")))
  (is (null (jref nil "a" "b"))))

(test encode-parse-round-trip
  "encode-json and parse-json round-trip correctly."
  (let* ((original (make-ht "x" 42 "nested" (make-ht "y" "hello")))
         (json (encode-json original))
         (parsed (parse-json json)))
    (is (= 42 (jref parsed "x")))
    (is (string= "hello" (jref parsed "nested" "y")))))

(test encode-json-booleans
  "encode-json handles t/nil/yason:false correctly."
  (is (string= "{\"v\":true}" (encode-json (make-ht "v" t))))
  (is (string= "{\"v\":null}" (encode-json (make-ht "v" nil))))
  (is (string= "{\"v\":false}" (encode-json (make-ht "v" yason:false)))))

(test encode-json-empty
  "encode-json on empty hash-table produces {}."
  (is (string= "{}" (encode-json (make-ht)))))
