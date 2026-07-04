(in-package #:kli/tests)
(in-suite all)

;;;; `kli docs`: kli-scoped URL construction and the fetch-and-print surface.

(test docs-urls-are-kli-scoped
  "The index and page URLs always address the kli project, and a page path's
leading slash is dropped."
  (let ((app::*docs-base-url* "https://example.test"))
    (is (equal "https://example.test/kli/llms.txt" (app::docs-index-url)))
    (is (equal "https://example.test/kli/extend/anatomy.md"
               (app::docs-page-url "extend/anatomy")))
    (is (equal "https://example.test/kli/extend/anatomy.md"
               (app::docs-page-url "/extend/anatomy")))
    (is (search "/kli/" (app::docs-index-url)))
    (is (search "/kli/" (app::docs-page-url "x/y")))))

(test docs-base-url-trims-trailing-slash
  "A trailing slash on the configured origin does not double up in the URL."
  (let ((app::*docs-base-url* "https://example.test/"))
    (is (equal "https://example.test/kli/llms.txt" (app::docs-index-url)))))

(test run-docs-default-fetches-index
  "No argument (and the explicit `index` token) fetches the kli docs index."
  (flet ((index-url-for (args)
           (let* ((seen nil)
                  (app::*docs-base-url* "https://example.test")
                  (app::*docs-http* (lambda (url) (values (progn (setf seen url) "# kli") 200))))
             (let ((out (make-string-output-stream)) (code nil))
               (let ((*standard-output* out)) (setf code (app::run-docs args)))
               (is (eql 0 code)))
             seen)))
    (is (equal "https://example.test/kli/llms.txt" (index-url-for '())))
    (is (equal "https://example.test/kli/llms.txt" (index-url-for '("index"))))))

(test run-docs-fetches-page-as-md
  "A path argument fetches that kli page's .md variant and prints the body."
  (let* ((seen nil)
         (app::*docs-base-url* "https://example.test")
         (app::*docs-http* (lambda (url) (values (progn (setf seen url) "# Anatomy") 200)))
         (out (make-string-output-stream))
         (code nil))
    (let ((*standard-output* out)) (setf code (app::run-docs '("extend/lisp-extensions/anatomy"))))
    (is (eql 0 code))
    (is (equal "https://example.test/kli/extend/lisp-extensions/anatomy.md" seen))
    (is (search "Anatomy" (get-output-stream-string out)))))

(test run-docs-reports-http-error
  "A non-200 status returns a non-zero code and names the status on stderr."
  (let ((app::*docs-base-url* "https://example.test")
        (app::*docs-http* (lambda (url) (declare (ignore url)) (values "nope" 404)))
        (err (make-string-output-stream))
        (code nil))
    (let ((*error-output* err)) (setf code (app::run-docs '("missing/page"))))
    (is (eql 1 code))
    (is (search "404" (get-output-stream-string err)))))

(test run-docs-survives-transport-error
  "A transport failure is caught: non-zero code, no escape."
  (let ((app::*docs-base-url* "https://example.test")
        (app::*docs-http* (lambda (url) (declare (ignore url)) (error "boom")))
        (err (make-string-output-stream))
        (code nil))
    (let ((*error-output* err)) (setf code (app::run-docs '("some/page"))))
    (is (eql 1 code))
    (is (search "failed" (get-output-stream-string err)))))

(test docs-search-url-is-kli-scoped-and-encoded
  "The search URL hits /search.md, pins project=kli, and percent-encodes the query."
  (let* ((app::*docs-base-url* "https://example.test")
         (url (app::docs-search-url "live extensions")))
    (is (search "https://example.test/search.md?q=" url))
    (is (search "project=kli" url))
    (is (not (find #\Space url)))))

(test run-docs-search-proxies-query
  "`search` joins the query tokens and proxies to the kli-scoped /search.md."
  (let* ((seen nil)
         (app::*docs-base-url* "https://example.test")
         (app::*docs-http* (lambda (url) (values (progn (setf seen url) "# Search") 200)))
         (out (make-string-output-stream))
         (code nil))
    (let ((*standard-output* out)) (setf code (app::run-docs '("search" "live" "extensions"))))
    (is (eql 0 code))
    (is (search "/search.md?q=" seen))
    (is (search "project=kli" seen))
    (is (search "live" seen))
    (is (search "extensions" seen))
    (is (not (find #\Space seen)))))

(test run-docs-search-needs-a-query
  "`search` with no terms returns non-zero and never touches the network."
  (let ((app::*docs-http* (lambda (url) (declare (ignore url)) (error "network must not be reached")))
        (err (make-string-output-stream))
        (code nil))
    (let ((*error-output* err)) (setf code (app::run-docs '("search"))))
    (is (eql 1 code))
    (is (search "query" (get-output-stream-string err)))))

(test run-docs-help-prints-usage
  "help/--help/-h print usage and exit 0 without touching the network."
  (dolist (flag '("help" "--help" "-h"))
    (let ((app::*docs-http* (lambda (url) (declare (ignore url)) (error "network must not be reached")))
          (out (make-string-output-stream))
          (code nil))
      (let ((*standard-output* out)) (setf code (app::run-docs (list flag))))
      (is (eql 0 code))
      (is (search "kli docs" (get-output-stream-string out))))))
