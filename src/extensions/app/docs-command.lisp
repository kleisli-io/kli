(in-package #:kli/app)

(defparameter +docs-project+ "kli"
  "kli docs always addresses the kli project's docs, never another project.")

(defparameter +default-docs-base-url+ "https://docs.kleisli.io"
  "Docs origin used when no environment override is set.")

(defvar *docs-base-url* nil
  "Override for the docs origin. Nil resolves the env/default at call time.")

(defvar *docs-http* nil
  "Test seam: a function of (url) returning (values body-string status) that
replaces the live docs GET. Nil performs the real request.")

(defun docs-base-url ()
  "Docs origin: *docs-base-url*, then KLI_DOCS_BASE, then KLEISLI_BASE_URL, then
the default, with any trailing slash dropped."
  (let ((override (or *docs-base-url*
                      (uiop:getenv "KLI_DOCS_BASE")
                      (uiop:getenv "KLEISLI_BASE_URL"))))
    (string-right-trim "/"
                       (if (and override (plusp (length override)))
                           override
                           +default-docs-base-url+))))

(defun docs-index-url ()
  "URL of the kli docs index (per-project llms.txt)."
  (format nil "~A/~A/llms.txt" (docs-base-url) +docs-project+))

(defun docs-page-url (page-path)
  "URL of the kli doc page at PAGE-PATH (a `section/page' path) as Markdown."
  (format nil "~A/~A/~A.md"
          (docs-base-url) +docs-project+ (string-left-trim "/" page-path)))

(defun docs-search-url (query)
  "URL of the kli-scoped Markdown search results for QUERY."
  (format nil "~A/search.md?q=~A&project=~A"
          (docs-base-url) (drakma:url-encode query :utf-8) +docs-project+))

(defun docs-http-get (url)
  "GET URL as Markdown, returning (values body-string status)."
  (if *docs-http*
      (funcall *docs-http* url)
      (multiple-value-bind (body status)
          (drakma:http-request url
                               :user-agent "kli-docs"
                               :additional-headers '(("Accept" . "text/markdown")))
        (values (if (stringp body)
                    body
                    (flexi-streams:octets-to-string body :external-format :utf-8))
                status))))

(defun emit-docs-fetch (url)
  "GET URL and print the body, or report the failure on *error-output*. Returns a
process exit code: 0 on a 200, non-zero otherwise."
  (handler-case
      (multiple-value-bind (body status) (docs-http-get url)
        (cond
          ((eql status 200)
           (write-string body)
           (fresh-line)
           0)
          (t
           (format *error-output* "~&kli docs: HTTP ~A for ~A~%" status url)
           1)))
    (error (condition)
      (format *error-output* "~&kli docs failed: ~A~%" condition)
      1)))

(defun print-docs-usage (&optional (stream *standard-output*))
  "Write the `kli docs` usage summary."
  (format stream "~&~{~A~%~}"
          '("Usage: kli docs [<section>/<page> | search <query>]"
            ""
            "  kli docs                     print the kli docs index"
            "  kli docs <section>/<page>    print a kli doc page as Markdown"
            "  kli docs search <query>      search the kli docs"
            ""
            "Docs are fetched live from https://docs.kleisli.io; override the"
            "origin with KLI_DOCS_BASE.")))

(defun run-docs (args)
  "Fetch kli documentation from the live docs site and print it. The project is
always kli. Returns a process exit code."
  (let ((head (first args)))
    (cond
      ((member head '("help" "--help" "-h") :test #'string=)
       (print-docs-usage)
       0)
      ((or (null head) (string= head "index"))
       (emit-docs-fetch (docs-index-url)))
      ((string= head "search")
       (let ((query (string-trim " " (format nil "~{~A~^ ~}" (rest args)))))
         (if (plusp (length query))
             (emit-docs-fetch (docs-search-url query))
             (progn
               (format *error-output* "~&kli docs search: needs a query~%")
               1))))
      (t (emit-docs-fetch (docs-page-url head))))))
