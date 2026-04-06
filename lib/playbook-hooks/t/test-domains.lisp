;;;; playbook-hooks tests - Domain Detection

(in-package :playbook-hooks.tests)
(in-suite :domains)

(test detect-domains-nix-lisp
  "Detects nix and lisp domains from text."
  (is (equal '("lisp" "nix") (detect-domains-from-text "deploy NixOS with SBCL"))))

(test detect-domains-single
  "Detects a single domain."
  (is (equal '("python") (detect-domains-from-text "run pytest on the Django models"))))

(test detect-domains-none
  "Returns NIL for unrecognized text."
  (is (null (detect-domains-from-text "hello world"))))

(test detect-domains-nil
  "Returns NIL for NIL input."
  (is (null (detect-domains-from-text nil))))

(test detect-domains-case-insensitive
  "Domain detection is case-insensitive."
  (is (equal '("nix") (detect-domains-from-text "NIXOS configuration"))))

(test detect-domain-from-path-nix
  "Detects nix domain from .nix extension."
  (is (string= "nix" (detect-domain-from-path "/home/user/module.nix"))))

(test detect-domain-from-path-lisp
  "Detects lisp domain from .lisp extension."
  (is (string= "lisp" (detect-domain-from-path "/src/foo.lisp"))))

(test detect-domain-from-path-unknown
  "Returns NIL for unknown extension."
  (is (null (detect-domain-from-path "/tmp/readme.md"))))

(test detect-domain-from-path-nil
  "Returns NIL for NIL path."
  (is (null (detect-domain-from-path nil))))
