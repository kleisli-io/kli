(in-package #:task-mcp-tests)

;;; Regression tests for the route-aware request deadline.
;;;
;;; Background: GET /mcp is the MCP HTTP transport's SSE long-poll
;;; stream — it deliberately holds the connection open across many
;;; tool calls.  The request-deadline machinery used to apply a fixed
;;; *REQUEST-DEADLINE-SECONDS* (default 30) to every request, which
;;; 503'd the SSE stream every 30 seconds (one daemon log slice
;;; recorded 1352 deadline warnings on a single session).
;;;
;;; The fix splits the deadline into two:
;;;
;;;   *REQUEST-DEADLINE-SECONDS* — default for tool POSTs / hook calls.
;;;   *SSE-DEADLINE-SECONDS*     — for GET /mcp (SSE long-poll).
;;;
;;; EFFECTIVE-DEADLINE-SECONDS picks per (METHOD, PATH); the
;;; WITH-REQUEST-DEADLINE macro now takes :METHOD as a key and uses it
;;; to select the deadline.  REQUEST-DEADLINE-EXCEEDED carries the
;;; chosen deadline so the 503 body reports it accurately rather than
;;; the bare *REQUEST-DEADLINE-SECONDS*.

(in-suite :task-mcp-tests)

;;; --- EFFECTIVE-DEADLINE-SECONDS dispatch ----------------------------------

(test effective-deadline-routes-get-mcp-to-sse-deadline
  "GET /mcp (the SSE long-poll) selects *SSE-DEADLINE-SECONDS*; both
   the bare path and the trailing-slash form are recognised."
  (let ((task-mcp::*request-deadline-seconds* 30)
        (task-mcp::*sse-deadline-seconds* 86400))
    (is (= 86400 (task-mcp::effective-deadline-seconds :get "/mcp")))
    (is (= 86400 (task-mcp::effective-deadline-seconds :get "/mcp/")))))

(test effective-deadline-routes-tool-post-to-default
  "POST /mcp (tool dispatch) and other routes use the default tool
   deadline.  /admin/diag, POST /tool/call, and POST /register-pid are
   explicitly checked because they are the hot paths for hook
   integration and have no per-route override."
  (let ((task-mcp::*request-deadline-seconds* 30)
        (task-mcp::*sse-deadline-seconds* 86400))
    (is (= 30 (task-mcp::effective-deadline-seconds :post "/mcp")))
    (is (= 30 (task-mcp::effective-deadline-seconds :get "/admin/diag")))
    (is (= 30 (task-mcp::effective-deadline-seconds :post "/tool/call")))
    (is (= 30 (task-mcp::effective-deadline-seconds :post "/register-pid")))))

(test effective-deadline-falls-back-to-default-for-unknown-method
  "An unknown or NIL method on /mcp must not be misclassified as a
   GET — it falls back to the default tool deadline."
  (let ((task-mcp::*request-deadline-seconds* 30)
        (task-mcp::*sse-deadline-seconds* 86400))
    (is (= 30 (task-mcp::effective-deadline-seconds nil "/mcp")))
    (is (= 30 (task-mcp::effective-deadline-seconds :put "/mcp")))
    (is (= 30 (task-mcp::effective-deadline-seconds :delete "/mcp")))))

(test effective-deadline-honours-nil-bindings
  "Binding either deadline var to NIL passes NIL through, signalling
   to WITH-REQUEST-DEADLINE that BODY runs with no deadline at all."
  (let ((task-mcp::*request-deadline-seconds* nil)
        (task-mcp::*sse-deadline-seconds* nil))
    (is (null (task-mcp::effective-deadline-seconds :get "/mcp")))
    (is (null (task-mcp::effective-deadline-seconds :post "/mcp")))))

;;; --- WITH-REQUEST-DEADLINE behaviour --------------------------------------

(test get-mcp-sse-long-poll-not-503ed-at-tool-deadline
  "Regression for the 1352-warnings-per-session symptom: a GET /mcp
   handler that runs longer than *REQUEST-DEADLINE-SECONDS* must NOT
   signal — it is the SSE long-poll and uses *SSE-DEADLINE-SECONDS*.
   We bind a 1s tool deadline and 60s SSE deadline, then sleep 1.5s."
  (let ((task-mcp::*request-deadline-seconds* 1)
        (task-mcp::*sse-deadline-seconds* 60))
    (let ((result
            (handler-case
                (task-mcp::with-request-deadline
                    (:method :get :path "/mcp" :session-id "probe")
                  (sleep 1.5)
                  :ran-to-completion)
              (task-mcp::request-deadline-exceeded ()
                :wrongly-signalled))))
      (is (eq :ran-to-completion result)))))

(test tool-post-still-honours-default-deadline
  "POST /mcp tool dispatch must still honour
   *REQUEST-DEADLINE-SECONDS*.  Bind a 1s deadline and sleep 1.5s —
   REQUEST-DEADLINE-EXCEEDED must be signalled."
  (let ((task-mcp::*request-deadline-seconds* 1)
        (task-mcp::*sse-deadline-seconds* 60))
    (signals task-mcp::request-deadline-exceeded
      (task-mcp::with-request-deadline
          (:method :post :path "/mcp" :session-id "probe")
        (sleep 1.5)))))

(test request-deadline-condition-carries-effective-seconds
  "REQUEST-DEADLINE-EXCEEDED must carry the deadline that actually
   fired (1s here), not the bare *REQUEST-DEADLINE-SECONDS* default.
   The HTTP 503 response body reads this slot to report
   deadline_seconds accurately for the offending route."
  (let ((task-mcp::*request-deadline-seconds* 1)
        (task-mcp::*sse-deadline-seconds* 60)
        (signalled nil))
    (handler-case
        (task-mcp::with-request-deadline
            (:method :post :path "/tool/call" :session-id "probe")
          (sleep 1.5))
      (task-mcp::request-deadline-exceeded (c)
        (setf signalled t)
        (is (eql 1 (task-mcp::request-deadline-deadline-seconds c)))
        (is (equal "/tool/call" (task-mcp::request-deadline-path c)))
        (is (equal "probe" (task-mcp::request-deadline-session-id c)))))
    (is (eq t signalled))))

(test request-deadline-report-prefers-slot-over-global
  "The condition's PRINC representation reports the slot value when
   present, so the 503 body and warning logs attribute the deadline
   to the route that actually fired (e.g. 86400 for SSE) rather than
   the global default (30)."
  (let ((task-mcp::*request-deadline-seconds* 30))
    (let ((c (make-condition 'task-mcp::request-deadline-exceeded
                             :path "/mcp" :session-id "probe"
                             :deadline-seconds 86400)))
      (is (search "86400s deadline" (princ-to-string c))))))

(test with-request-deadline-skips-deadline-when-effective-is-nil
  "When EFFECTIVE-DEADLINE-SECONDS returns NIL (both deadline vars
   bound to NIL), the macro must run BODY with no SB-SYS:WITH-DEADLINE
   wrapping at all.  A 0.1s sleep should complete cleanly."
  (let ((task-mcp::*request-deadline-seconds* nil)
        (task-mcp::*sse-deadline-seconds* nil))
    (let ((result
            (task-mcp::with-request-deadline
                (:method :get :path "/mcp" :session-id "probe")
              (sleep 0.1)
              :ok)))
      (is (eq :ok result)))))

;;; --- Per-route override table --------------------------------------------

(test route-deadline-overrides-apply-to-health-and-admin-unwedge
  "The default *ROUTE-DEADLINE-OVERRIDES* table includes literal
   entries for GET /health (5s) and POST /admin/unwedge (60s).
   Lookup must return the literal value, ignoring the deadline
   defvars — those defvars apply only to routes whose entry resolves
   them by symbol, or to routes with no entry at all."
  (let ((task-mcp::*request-deadline-seconds* 30)
        (task-mcp::*sse-deadline-seconds* 86400))
    (is (= 5  (task-mcp::effective-deadline-seconds :get "/health")))
    (is (= 60 (task-mcp::effective-deadline-seconds :post "/admin/unwedge")))))

(test route-deadline-override-respected-on-tool-post
  "An entry added to *ROUTE-DEADLINE-OVERRIDES* must take precedence
   over the default fallback for matching POSTs.  We bind a custom
   table with a 1s override on POST /slow and verify a 1.5s sleep
   signals REQUEST-DEADLINE-EXCEEDED — the override fired, not the
   30s default."
  (let ((task-mcp::*request-deadline-seconds* 30)
        (task-mcp::*sse-deadline-seconds* 86400)
        (task-mcp::*route-deadline-overrides*
          '(((:post . "/slow") . 1))))
    (is (= 1 (task-mcp::effective-deadline-seconds :post "/slow")))
    (signals task-mcp::request-deadline-exceeded
      (task-mcp::with-request-deadline
          (:method :post :path "/slow" :session-id "probe")
        (sleep 1.5)))))

(test route-deadline-override-bypassed-on-sse-get
  "Symbol-valued entries in the table resolve at lookup time, so a
   custom *SSE-DEADLINE-SECONDS* binding is honoured by GET /mcp's
   entry without rewriting the table.  Bind the SSE deadline to 60s
   and the tool deadline to 1s; a 1.5s sleep on GET /mcp must NOT
   signal — the override resolved to 60s, not 1s."
  (let ((task-mcp::*request-deadline-seconds* 1)
        (task-mcp::*sse-deadline-seconds* 60))
    (is (= 60 (task-mcp::effective-deadline-seconds :get "/mcp")))
    (let ((result
            (handler-case
                (task-mcp::with-request-deadline
                    (:method :get :path "/mcp" :session-id "probe")
                  (sleep 1.5)
                  :ran-to-completion)
              (task-mcp::request-deadline-exceeded ()
                :wrongly-signalled))))
      (is (eq :ran-to-completion result)))))

(test unknown-route-falls-back-to-default-deadline
  "A route with no matching entry in *ROUTE-DEADLINE-OVERRIDES* must
   fall back to *REQUEST-DEADLINE-SECONDS*.  Tested against an empty
   table (every route is unknown) and the default table (a route the
   built-ins do not enumerate)."
  (let ((task-mcp::*request-deadline-seconds* 42)
        (task-mcp::*sse-deadline-seconds* 86400))
    (let ((task-mcp::*route-deadline-overrides* nil))
      (is (= 42 (task-mcp::effective-deadline-seconds :post "/mcp")))
      (is (= 42 (task-mcp::effective-deadline-seconds :get "/mcp")))
      (is (= 42 (task-mcp::effective-deadline-seconds :post "/anything"))))
    (is (= 42 (task-mcp::effective-deadline-seconds :post "/tool/call")))
    (is (= 42 (task-mcp::effective-deadline-seconds :get "/admin/diag")))))

(test admin-diag-reports-active-deadline-config
  "DIAG-SNAPSHOT-PLIST must surface the resolved
   *ROUTE-DEADLINE-OVERRIDES* under :ROUTE-DEADLINES so an operator
   reading /admin/diag can confirm the active config without source
   access.  Each entry is rendered as a (:METHOD :PATH :SECONDS) plist;
   symbol-valued entries are resolved to the seconds currently in
   effect under the caller's bindings."
  (let ((task-mcp::*request-deadline-seconds* 30)
        (task-mcp::*sse-deadline-seconds* 86400))
    (let ((snapshot (task-mcp::route-deadlines-snapshot)))
      (is (= 4 (length snapshot)))
      (let ((sse-entry
              (find-if (lambda (e)
                         (and (eq :get (getf e :method))
                              (equal "/mcp" (getf e :path))))
                       snapshot)))
        (is (not (null sse-entry)))
        (is (= 86400 (getf sse-entry :seconds))))
      (let ((tool-entry
              (find-if (lambda (e)
                         (and (eq :post (getf e :method))
                              (equal "/mcp" (getf e :path))))
                       snapshot)))
        (is (not (null tool-entry)))
        (is (= 30 (getf tool-entry :seconds))))
      (let ((unwedge-entry
              (find-if (lambda (e)
                         (and (eq :post (getf e :method))
                              (equal "/admin/unwedge" (getf e :path))))
                       snapshot)))
        (is (not (null unwedge-entry)))
        (is (= 60 (getf unwedge-entry :seconds))))
      (let ((health-entry
              (find-if (lambda (e)
                         (and (eq :get (getf e :method))
                              (equal "/health" (getf e :path))))
                       snapshot)))
        (is (not (null health-entry)))
        (is (= 5 (getf health-entry :seconds)))))))

(test route-prefix-matches-p-discriminates-cleanly
  "ROUTE-PREFIX-MATCHES-P matches exact PATH = PREFIX and PATH
   continuations under PREFIX/, but not unrelated paths that merely
   share a prefix substring (e.g. /mcp vs /mcp-other)."
  (is (task-mcp::route-prefix-matches-p "/mcp" "/mcp"))
  (is (task-mcp::route-prefix-matches-p "/mcp/" "/mcp"))
  (is (task-mcp::route-prefix-matches-p "/mcp/sub" "/mcp"))
  (is (not (task-mcp::route-prefix-matches-p "/mcp-other" "/mcp")))
  (is (not (task-mcp::route-prefix-matches-p "/" "/mcp")))
  (is (not (task-mcp::route-prefix-matches-p "" "/mcp"))))
