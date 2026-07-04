(in-package #:kli/tests)
(in-suite all)

(defun parse-profile-specs-quietly (settings &rest args)
  (handler-bind ((warning #'muffle-warning))
    (apply #'profiles:parse-profile-specs settings args)))

(defun resolve-profile-spec-quietly (name specs &rest args)
  (handler-bind ((warning #'muffle-warning))
    (apply #'profiles:resolve-profile-spec name specs args)))

(test profile-spec-parse-reads-entries
  (let* ((settings (com.inuoe.jzon:parse "{
      \"profiles\": {
        \"writing\": {\"extends\": \"Focus\",
                      \"enable\": [\"zotero\", \"prose-lint\"],
                      \"disable\": [\"bash-tool\"],
                      \"settings\": {\"theme\": \"light\"}},
        \"focus\": {}}}"))
         (specs (profiles:parse-profile-specs settings :builtins '(:headless))))
    (is (hash-table-p specs))
    (is (= 2 (hash-table-count specs)))
    (let ((writing (gethash :writing specs)))
      (is (profiles:profile-spec-p writing))
      (is (eq :writing (profiles:profile-spec-name writing)))
      (is (eq :focus (profiles:profile-spec-extends writing))
          "extends values normalize like extension ids")
      (is (equal '(:zotero :prose-lint)
                 (profiles:profile-spec-enable writing)))
      (is (equal '(:bash-tool) (profiles:profile-spec-disable writing)))
      (is (equal "light" (gethash "theme"
                                  (profiles:profile-spec-settings writing)))))
    (let ((focus (gethash :focus specs)))
      (is (profiles:profile-spec-p focus))
      (is (null (profiles:profile-spec-extends focus)))
      (is (null (profiles:profile-spec-enable focus)))
      (is (null (profiles:profile-spec-disable focus)))
      (is (null (profiles:profile-spec-settings focus))))))

(test profile-spec-parse-tolerates-missing-or-malformed-profiles-key
  (let ((absent (profiles:parse-profile-specs
                 (com.inuoe.jzon:parse "{}") :builtins '())))
    (is (hash-table-p absent))
    (is (zerop (hash-table-count absent))))
  (let ((wrong-type (parse-profile-specs-quietly
                     (com.inuoe.jzon:parse "{\"profiles\": \"nope\"}")
                     :builtins '())))
    (is (hash-table-p wrong-type))
    (is (zerop (hash-table-count wrong-type))))
  (signals warning
    (profiles:parse-profile-specs
     (com.inuoe.jzon:parse "{\"profiles\": [1]}") :builtins '())))

(test profile-spec-parse-skips-malformed-entries
  (let* ((settings (com.inuoe.jzon:parse "{
      \"profiles\": {
        \"good\": {\"enable\": [\"a\"]},
        \"not-object\": 42,
        \"bad-extends\": {\"extends\": 7},
        \"bad-enable\": {\"enable\": \"oops\"},
        \"bad-members\": {\"enable\": [\"a\", 3]},
        \"bad-settings\": {\"settings\": [\"not\", \"object\"]}}}"))
         (specs (parse-profile-specs-quietly settings :builtins '())))
    (is (= 1 (hash-table-count specs))
        "every malformed entry is skipped, the good one survives")
    (is (profiles:profile-spec-p (gethash :good specs))))
  (signals warning
    (profiles:parse-profile-specs
     (com.inuoe.jzon:parse "{\"profiles\": {\"bad\": 1}}") :builtins '())))

(test profile-spec-parse-rejects-builtin-shadowing
  (let* ((settings (com.inuoe.jzon:parse "{
      \"profiles\": {
        \"interactive-terminal\": {\"disable\": [\"bash-tool\"]},
        \"mine\": {}}}"))
         (specs (parse-profile-specs-quietly
                 settings :builtins '(:headless :interactive-terminal))))
    (is (= 1 (hash-table-count specs)))
    (is (null (gethash :interactive-terminal specs)))
    (is (profiles:profile-spec-p (gethash :mine specs))))
  (signals warning
    (profiles:parse-profile-specs
     (com.inuoe.jzon:parse "{\"profiles\": {\"headless\": {}}}")
     :builtins '(:headless))))

(test profile-spec-resolve-bottoms-at-default-base
  (let* ((specs (profiles:parse-profile-specs
                 (com.inuoe.jzon:parse
                  "{\"profiles\": {\"mine\": {\"enable\": [\"x\"]}}}")
                 :builtins '(:interactive-terminal)))
         (resolved (profiles:resolve-profile-spec
                    :mine specs :builtins '(:interactive-terminal))))
    (is (profiles:resolved-profile-p resolved))
    (is (eq :mine (profiles:resolved-profile-name resolved)))
    (is (eq :interactive-terminal (profiles:resolved-profile-base resolved))
        "an extends-less spec bottoms at *default-profile-base*")
    (is (equal '(:x) (profiles:resolved-profile-enable resolved)))
    (is (null (profiles:resolved-profile-disable resolved))))
  (let* ((specs (profiles:parse-profile-specs
                 (com.inuoe.jzon:parse "{\"profiles\": {\"mine\": {}}}")
                 :builtins '(:headless)))
         (resolved (profiles:resolve-profile-spec
                    :mine specs :builtins '(:headless)
                    :default-base :headless)))
    (is (eq :headless (profiles:resolved-profile-base resolved))
        "the default base is parameterizable")))

(test profile-spec-resolve-composes-extends-chain
  (let* ((specs (profiles:parse-profile-specs
                 (com.inuoe.jzon:parse "{
      \"profiles\": {
        \"base\": {\"extends\": \"headless\",
                   \"enable\": [\"a\", \"b\"],
                   \"disable\": [\"c\"],
                   \"settings\": {\"theme\": \"dark\",
                                  \"compaction\": {\"enabled\": true}}},
        \"leaf\": {\"extends\": \"base\",
                   \"enable\": [\"c\"],
                   \"disable\": [\"b\", \"d\"],
                   \"settings\": {\"theme\": \"light\",
                                  \"compaction\": {\"reserveTokens\": 8192}}}}}")
                 :builtins '(:headless)))
         (resolved (profiles:resolve-profile-spec
                    :leaf specs :builtins '(:headless))))
    (is (eq :leaf (profiles:resolved-profile-name resolved)))
    (is (eq :headless (profiles:resolved-profile-base resolved))
        "the chain bottoms at the builtin its root extends")
    (let ((enable (profiles:resolved-profile-enable resolved))
          (disable (profiles:resolved-profile-disable resolved)))
      (is (null (set-exclusive-or '(:a :c) enable))
          "the leaf re-enables :c over the base's disable")
      (is (null (set-exclusive-or '(:b :d) disable))
          "the leaf's disable wins over the base's enable"))
    (let ((settings (profiles:resolved-profile-settings resolved)))
      (is (equal "light" (gethash "theme" settings))
          "leaf settings override base settings")
      (is (eq t (gethash "enabled" (gethash "compaction" settings)))
          "nested base settings survive the overlay merge")
      (is (= 8192 (gethash "reserveTokens" (gethash "compaction" settings)))))))

(test profile-spec-resolve-builtin-name-is-degenerate
  (let ((resolved (profiles:resolve-profile-spec
                   :headless (make-hash-table) :builtins '(:headless))))
    (is (profiles:resolved-profile-p resolved))
    (is (eq :headless (profiles:resolved-profile-name resolved)))
    (is (eq :headless (profiles:resolved-profile-base resolved)))
    (is (null (profiles:resolved-profile-enable resolved)))
    (is (null (profiles:resolved-profile-disable resolved)))
    (is (zerop (hash-table-count
                (profiles:resolved-profile-settings resolved))))))

(test profile-spec-resolve-fails-soft-on-cycles
  (let ((specs (profiles:parse-profile-specs
                (com.inuoe.jzon:parse "{
      \"profiles\": {
        \"a\": {\"extends\": \"b\"},
        \"b\": {\"extends\": \"a\"},
        \"self\": {\"extends\": \"self\"}}}")
                :builtins '(:headless))))
    (is (null (resolve-profile-spec-quietly :a specs :builtins '(:headless))))
    (is (null (resolve-profile-spec-quietly :self specs
                                            :builtins '(:headless))))
    (signals warning
      (profiles:resolve-profile-spec :a specs :builtins '(:headless)))))

(test profile-spec-resolve-fails-soft-on-unknown-and-dangling
  (let ((specs (profiles:parse-profile-specs
                (com.inuoe.jzon:parse
                 "{\"profiles\": {\"a\": {\"extends\": \"ghost\"}}}")
                :builtins '(:headless))))
    (is (null (resolve-profile-spec-quietly :nope specs
                                            :builtins '(:headless))))
    (is (null (resolve-profile-spec-quietly :a specs :builtins '(:headless))))
    (signals warning
      (profiles:resolve-profile-spec :nope specs :builtins '(:headless)))
    (signals warning
      (profiles:resolve-profile-spec :a specs :builtins '(:headless)))))
