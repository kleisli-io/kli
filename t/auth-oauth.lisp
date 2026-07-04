(in-package #:kli/tests)

(in-suite all)

(defun make-fake-jwt (payload-json)
  "Three-segment token whose payload is base64url(UTF-8 PAYLOAD-JSON)."
  (flet ((seg (s)
           (kli/auth/oauth::base64url
            (flexi-streams:string-to-octets s :external-format :utf-8))))
    (format nil "~A.~A.~A"
            (seg "{\"alg\":\"none\"}") (seg payload-json) "sig")))

(defun ok-token-stub (&optional (access "AT") (refresh "RT") (expires-in 3600))
  (lambda (grant-type params)
    (declare (ignore grant-type params))
    (values (format nil
                    "{\"access_token\":\"~A\",\"refresh_token\":\"~A\",\"expires_in\":~D}"
                    access refresh expires-in)
            200)))

(test pkce-known-answer-and-verifier-shape
  (is (string= "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
               (oauth:pkce-challenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")))
  (is (= 43 (length (oauth:pkce-verifier))))
  (is (not (string= (oauth:pkce-verifier) (oauth:pkce-verifier)))))

(test authorize-url-contains-locked-params
  (let ((url (oauth:build-authorize-url "CHALLENGE" "STATE")))
    (dolist (frag '("response_type=code"
                    "client_id=app_EMoamEEZ73f0CkXaXp7hrann"
                    "code_challenge=CHALLENGE"
                    "code_challenge_method=S256"
                    "state=STATE"
                    "id_token_add_organizations=true"
                    "codex_cli_simplified_flow=true"
                    "originator=kli"
                    "scope="
                    "redirect_uri="))
      (is (search frag url) "authorize URL missing ~A" frag))))

(test parse-authorization-input-handles-all-shapes
  (flet ((check (input code state)
           (multiple-value-bind (c s) (oauth:parse-authorization-input input)
             (is (equal code c))
             (is (equal state s)))))
    (check "https://localhost:1455/auth/callback?code=AC123&state=ST456"
           "AC123" "ST456")
    (check "AC123#ST456" "AC123" "ST456")
    (check "code=AC123&state=ST456" "AC123" "ST456")
    (check "AC123" "AC123" nil)
    (check "" nil nil)))

(test jwt-account-id-decodes-claim-and-tolerates-garbage
  (let ((token (make-fake-jwt
                "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct-9XzQ_be-77\"}}")))
    (is (string= "acct-9XzQ_be-77" (oauth:jwt-account-id token))))
  (is (null (oauth:jwt-account-id "only.two")))
  (is (null (oauth:jwt-account-id "not-a-jwt"))))

(test parse-token-response-applies-margin-and-validates
  (let ((before (get-universal-time)))
    (multiple-value-bind (access refresh expires)
        (kli/auth/oauth::parse-token-response
         "{\"access_token\":\"AT\",\"refresh_token\":\"RT\",\"expires_in\":3600}"
         "authorization_code")
      (is (string= "AT" access))
      (is (string= "RT" refresh))
      (is (<= (+ before 3600 -60) expires (+ (get-universal-time) 3600 -60)))))
  (signals oauth:token-endpoint-error
    (kli/auth/oauth::parse-token-response "{\"access_token\":\"AT\"}"
                                          "authorization_code")))

(test token-exchange-and-refresh-send-expected-grant
  (let* ((seen nil)
         (oauth:*token-http*
           (lambda (grant-type params)
             (push (cons grant-type params) seen)
             (funcall (ok-token-stub) grant-type params))))
    (multiple-value-bind (access refresh expires)
        (oauth:token-exchange "the-code" "the-verifier")
      (is (string= "AT" access))
      (is (string= "RT" refresh))
      (is (integerp expires)))
    (multiple-value-bind (access refresh expires) (oauth:token-refresh "rt0")
      (declare (ignore refresh expires))
      (is (string= "AT" access)))
    (let ((refresh-call (first seen))
          (exchange-call (second seen)))
      (is (string= "refresh_token" (car refresh-call)))
      (is (string= "refresh_token"
                   (cdr (assoc "grant_type" (cdr refresh-call) :test #'string=))))
      (is (string= "rt0"
                   (cdr (assoc "refresh_token" (cdr refresh-call) :test #'string=))))
      (is (string= "authorization_code" (car exchange-call)))
      (is (string= "the-code"
                   (cdr (assoc "code" (cdr exchange-call) :test #'string=))))
      (is (string= "the-verifier"
                   (cdr (assoc "code_verifier" (cdr exchange-call) :test #'string=)))))))

(test token-exchange-signals-on-non-2xx
  (let ((oauth:*token-http*
          (lambda (grant-type params)
            (declare (ignore grant-type params))
            (values "{\"error\":\"bad_request\"}" 401))))
    (signals oauth:token-endpoint-error
      (oauth:token-exchange "c" "v"))))

(test token-endpoint-error-classifies-as-provider-with-status
  "An expired refresh token is a provider failure carrying its HTTP status,
not an internal error. An invalid_grant body points at /auth login; other
bodies get no hint."
  (let ((oauth:*token-http*
          (lambda (grant-type params)
            (declare (ignore grant-type params))
            (values "{\"error\":\"invalid_grant\"}" 401))))
    (handler-case (oauth:token-refresh "rt-expired")
      (oauth:token-endpoint-error (c)
        (is (eq :provider (ext:condition-category c)))
        (is (eql 401 (ext:condition-http-status c)))
        (is (search "run /auth login" (princ-to-string c))))
      (:no-error (&rest values)
        (declare (ignore values))
        (fail "expected a token-endpoint-error"))))
  (is (not (search "run /auth login"
                   (princ-to-string
                    (make-condition 'oauth:token-endpoint-error
                                    :grant-type "refresh_token"
                                    :status 500
                                    :body "{\"error\":\"server_error\"}"))))))

(test token-network-failures-classify-as-network
  "A network-layer failure during a token request signals token-network-error
with :network category instead of escaping as a raw internal socket
condition. end-of-file is a stream-error, one of the classified types."
  (let ((oauth:*token-http*
          (lambda (grant-type params)
            (declare (ignore grant-type params))
            (error 'end-of-file :stream (make-string-input-stream "")))))
    (handler-case (oauth:token-refresh "rt0")
      (oauth:token-network-error (c)
        (is (eq :network (ext:condition-category c)))
        (is (typep (oauth:token-network-error-cause c) 'end-of-file))
        (is (string= "refresh_token"
                     (oauth:token-network-error-grant-type c))))
      (:no-error (&rest values)
        (declare (ignore values))
        (fail "expected a token-network-error")))))

(test (refresh-updates-reference-persists-and-does-not-refetch :fixture interactive-authority)
  "A second resolve must return the cached token without another endpoint call."
  (auth-only-context)
  (with-temp-credentials (path)
    (let ((calls 0)
          (ref (auth:make-oauth-credential-reference
                "codex" :access "stale" :refresh "rt0"
                :expires (- (get-universal-time) 1) :store-path path)))
      (is (auth:oauth-credential-expired-p ref))
      (let ((oauth:*token-http*
              (lambda (grant-type params)
                (declare (ignore grant-type params))
                (incf calls)
                (values "{\"access_token\":\"AT-FRESH\",\"refresh_token\":\"RT-FRESH\",\"expires_in\":3600}"
                        200))))
        (is (string= "AT-FRESH" (auth:credential-reference-value ref)))
        (is (= 1 calls))
        (is (not (auth:oauth-credential-expired-p ref))))
      (let ((oauth:*token-http*
              (lambda (grant-type params)
                (declare (ignore grant-type params))
                (error "token endpoint must not be called again"))))
        (is (string= "AT-FRESH" (auth:credential-reference-value ref))))
      (let ((record (auth:read-credential-record "codex" path)))
        (is (eq :oauth (auth:credential-record-kind record)))
        (is (string= "RT-FRESH" (gethash "refresh" record))))
      (is (= #o600
             (logand (sb-posix:stat-mode (sb-posix:stat (namestring path)))
                     #o777))))))

(test (store-oauth-credential-registers-and-persists-0600 :fixture interactive-authority)
  (multiple-value-bind (context protocol) (auth-only-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (let ((store (credential-store context)))
        (auth:store-oauth-credential store "codex" context
                                     :access "AT" :refresh "RT"
                                     :expires (+ (get-universal-time) 3600)
                                     :account-id "acct" :path path)
        (is (auth:credential-available-p store "codex"))
        (is (string= "AT"
                     (auth:resolved-credential-value
                      (auth:resolve-credential store "codex" context))))
        (let ((record (auth:read-credential-record "codex" path)))
          (is (eq :oauth (auth:credential-record-kind record)))
          (is (string= "RT" (gethash "refresh" record))))
        (is (= #o600
               (logand (sb-posix:stat-mode (sb-posix:stat (namestring path)))
                       #o777)))))))

(test (refresh-updates-memory-before-persisting :fixture interactive-authority)
  "The live reference must already hold the rotated token at the moment the
durable record is written, so a reader landing between the two steps never sees
stale memory against a fresh file."
  (auth-only-context)
  (with-temp-credentials (path)
    (let* ((ref (auth:make-oauth-credential-reference
                 "codex" :access "stale" :refresh "rt0"
                 :expires (- (get-universal-time) 1) :store-path path))
           (memory-at-write nil)
           (original (fdefinition 'kli/auth/core::write-credential-record)))
      (unwind-protect
           (progn
             (setf (fdefinition 'kli/auth/core::write-credential-record)
                   (lambda (&rest args)
                     (setf memory-at-write
                           (kli/auth/core::oauth-credential-reference-access ref))
                     (apply original args)))
             (let ((oauth:*token-http* (ok-token-stub "AT-FRESH" "RT-FRESH")))
               (is (string= "AT-FRESH" (auth:credential-reference-value ref)))))
        (setf (fdefinition 'kli/auth/core::write-credential-record) original))
      (is (string= "AT-FRESH" memory-at-write))
      (let ((record (auth:read-credential-record "codex" path)))
        (is (string= "AT-FRESH" (gethash "access" record)))
        (is (string= "RT-FRESH" (gethash "refresh" record)))))))

(test (auth-code-requires-present-and-matching-state :fixture interactive-authority)
  "A bare code with no state, or a state that does not match the armed login, is
rejected as a possible CSRF; only the code carrying the armed state proceeds to
the token exchange."
  (multiple-value-bind (context protocol) (model-command-test-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (invoke-test-command context :auth '(:words ("login" "codex")))
      (let* ((pending (oauth:pending-login (kli:active-protocol context) "codex"))
             (state (getf pending :state)))
        (is (stringp state))
        (is (commands:command-result-error-p
             (invoke-test-command context :auth '(:words ("code" "AC123")))))
        (is (commands:command-result-error-p
             (invoke-test-command context :auth
                                  '(:words ("code" "AC123#WRONGSTATE")))))
        (let ((oauth:*token-http* (ok-token-stub)))
          (let ((ok (invoke-test-command
                     context :auth
                     (list :words (list "code" (format nil "AC123#~A" state))))))
            (is (not (commands:command-result-error-p ok)))))
        (is (auth:read-credential-record "codex" path))))))
