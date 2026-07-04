(in-package #:kli/tests)
(in-suite all)

;;;; CA-store discovery and forced TLS verification for kli self-update.

(defun %ca-env (alist)
  "Environment reader returning the value bound for a name in ALIST, else nil."
  (lambda (name) (cdr (assoc name alist :test #'string=))))

(defun %ca-fs (paths)
  "Filesystem probe returning PATH when it is a member of PATHS, else nil."
  (lambda (path) (and (member path paths :test #'string=) path)))

(defun %ca-discover (&key env files dirs)
  "discover-ca-bundle driven through injected environment and filesystem seams."
  (app::discover-ca-bundle :getenv (%ca-env env)
                           :probe-file (%ca-fs files)
                           :probe-dir (%ca-fs dirs)))

(test ca-discover-env-file-wins-over-candidates
  "SSL_CERT_FILE is honored ahead of the filesystem candidates."
  (multiple-value-bind (path kind)
      (%ca-discover :env '(("SSL_CERT_FILE" . "/custom/ca.pem"))
                    :files '("/custom/ca.pem" "/etc/ssl/certs/ca-certificates.crt")
                    :dirs '("/etc/ssl/certs"))
    (is (equal "/custom/ca.pem" path))
    (is (eq :file kind))))

(test ca-discover-env-precedence-file-dir-curl
  "Env order is SSL_CERT_FILE, then SSL_CERT_DIR, then CURL_CA_BUNDLE."
  (multiple-value-bind (path kind)
      (%ca-discover :env '(("SSL_CERT_DIR" . "/custom/certs")
                           ("CURL_CA_BUNDLE" . "/curl/ca.pem"))
                    :files '("/curl/ca.pem") :dirs '("/custom/certs"))
    (is (equal "/custom/certs" path))
    (is (eq :directory kind)))
  (multiple-value-bind (path kind)
      (%ca-discover :env '(("CURL_CA_BUNDLE" . "/curl/ca.pem")) :files '("/curl/ca.pem"))
    (is (equal "/curl/ca.pem" path))
    (is (eq :file kind))))

(test ca-discover-stale-env-falls-through
  "An override whose path is absent falls through to the candidates."
  (multiple-value-bind (path kind)
      (%ca-discover :env '(("SSL_CERT_FILE" . "/gone/ca.pem"))
                    :files '("/etc/ssl/cert.pem"))
    (is (equal "/etc/ssl/cert.pem" path))
    (is (eq :file kind))))

(test ca-discover-file-candidate-order
  "The first existing file candidate wins over later ones."
  (is (equal "/etc/ssl/certs/ca-certificates.crt"
             (%ca-discover :files '("/etc/ssl/certs/ca-certificates.crt"
                                    "/etc/ssl/cert.pem"))))
  (is (equal "/etc/pki/tls/certs/ca-bundle.crt"
             (%ca-discover :files '("/etc/pki/tls/certs/ca-bundle.crt")))))

(test ca-discover-directory-candidate-when-no-file
  "A directory candidate is used only after the file candidates miss."
  (multiple-value-bind (path kind)
      (%ca-discover :dirs '("/etc/ssl/certs"))
    (is (equal "/etc/ssl/certs" path))
    (is (eq :directory kind))))

(test ca-discover-nil-when-absent
  "Discovery returns nil when neither env nor filesystem yields a store."
  (is (null (%ca-discover))))

(test ca-verify-plist-file-and-directory
  "ca-verify-plist forces :verify :required and dispatches file vs directory."
  (is (equal '(:verify :required :ca-file "/x/ca.pem")
             (app::ca-verify-plist "/x/ca.pem" :file)))
  (is (equal '(:verify :required :ca-directory "/x/certs")
             (app::ca-verify-plist "/x/certs" :directory))))

(test ca-update-verify-args-fails-closed-without-store
  "update-verify-args signals rather than fetch unverified when no store is found."
  (let ((app::*ca-getenv* (constantly nil))
        (app::*ca-bundle-file-candidates* '())
        (app::*ca-bundle-dir-candidates* '()))
    (signals error (app::update-verify-args))))

(test ca-update-fetchers-force-verification-against-discovered-anchor
  "Both self-update fetchers reach the transport with :verify :required and the
discovered ca-file."
  (uiop:with-temporary-file (:pathname ca)
    (let* ((ca-path (namestring ca))
           (captured '())
           (app::*ca-getenv* (constantly nil))
           (app::*ca-bundle-file-candidates* (list ca-path))
           (app::*ca-bundle-dir-candidates* '())
           (app::*self-update-http* nil)
           (app::*self-update-fetcher* nil)
           (app::*self-update-transport*
             (lambda (url &rest args)
               (declare (ignore url))
               (push args captured)
               (values "body" 200))))
      (app::update-http-get "https://api.github.com/x")
      (app::update-fetch-octets "https://github.com/y.tar.gz")
      (is (= 2 (length captured)))
      (dolist (args captured)
        (is (eq :required (getf args :verify)))
        (is (equal ca-path (getf args :ca-file)))))))
