;;;; playbook-hooks tests - Co-Application Utilities

(in-package :playbook-hooks.tests)
(in-suite :co-app)

(test co-app-key-canonical
  "co-app-key produces same key regardless of argument order."
  (is (string= (co-app-key "nix-001" "nix-002")
               (co-app-key "nix-002" "nix-001"))))

(test co-app-key-format
  "co-app-key produces min:max format."
  (is (string= "a:b" (co-app-key "a" "b")))
  (is (string= "a:b" (co-app-key "b" "a"))))

(test generate-pairs-three
  "generate-pairs produces 3 pairs from 3 IDs."
  (let ((pairs (generate-pairs '("a" "b" "c"))))
    (is (= 3 (length pairs)))
    (is (member "a:b" pairs :test #'string=))
    (is (member "a:c" pairs :test #'string=))
    (is (member "b:c" pairs :test #'string=))))

(test generate-pairs-empty
  "generate-pairs returns NIL for empty or single-element list."
  (is (null (generate-pairs nil)))
  (is (null (generate-pairs '("a")))))

(test update-co-app-ledger-increments
  "update-co-app-ledger increments counts for each pair."
  (let ((ledger (make-hash-table :test 'equal)))
    (update-co-app-ledger ledger '("a:b" "a:c"))
    (update-co-app-ledger ledger '("a:b"))
    (is (= 2 (gethash "a:b" ledger)))
    (is (= 1 (gethash "a:c" ledger)))))

(test co-app-ledger-roundtrip
  "save and read co-app ledger roundtrips correctly."
  (let ((tmp (format nil "/tmp/test-co-app-~a.json" (random 1000000)))
        (ledger (make-hash-table :test 'equal)))
    (setf (gethash "a:b" ledger) 5)
    (setf (gethash "c:d" ledger) 3)
    (save-co-app-ledger ledger tmp)
    (let ((loaded (read-co-app-ledger tmp)))
      (is (= 5 (gethash "a:b" loaded)))
      (is (= 3 (gethash "c:d" loaded))))
    (delete-file tmp)))

(test read-co-app-ledger-missing
  "read-co-app-ledger returns empty table for missing file."
  (let ((ledger (read-co-app-ledger "/tmp/nonexistent-ledger.json")))
    (is (hash-table-p ledger))
    (is (= 0 (hash-table-count ledger)))))

(test co-app-ledger-path-format
  "co-app-ledger-path produces correct path."
  (is (string= "/home/user/ace/playbook-co-applications.json"
               (co-app-ledger-path "/home/user"))))
