(in-package #:kli/auth/oauth)

(defparameter +client-id+    "app_EMoamEEZ73f0CkXaXp7hrann")
(defparameter +authorize-url+ "https://auth.openai.com/oauth/authorize")
(defparameter +token-url+     "https://auth.openai.com/oauth/token")
(defparameter +redirect-uri+ "http://localhost:1455/auth/callback")
(defparameter +scope+        "openid profile email offline_access")
(defparameter +originator+   "kli")
(defparameter +refresh-margin+ 60)

(defparameter +pending-login-key+ :kli/auth/oauth/pending-login
  "protocol-storage key for a per-provider-id table of in-flight `/auth login`
   handshakes (provider-id -> plist (:verifier v :state st)). Per-protocol and
   per-provider, so coexisting protocols and concurrent logins never clobber.")

(defun pending-logins (protocol)
  "Per-protocol table of in-flight logins, keyed by provider-id string."
  (ensure-protocol-storage protocol +pending-login-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun pending-login (protocol provider-id)
  (gethash provider-id (pending-logins protocol)))

(defun (setf pending-login) (plist protocol provider-id)
  (setf (gethash provider-id (pending-logins protocol)) plist))

(defun clear-pending-login (protocol provider-id)
  (remhash provider-id (pending-logins protocol)))

(defun encode-query (alist)
  (format nil "~{~A~^&~}"
          (loop for (k . v) in alist
                collect (format nil "~A=~A"
                                (drakma:url-encode k :utf-8)
                                (drakma:url-encode v :utf-8)))))

(defun build-authorize-url (challenge state)
  (format nil "~A?~A" +authorize-url+
          (encode-query
           `(("response_type" . "code")
             ("client_id" . ,+client-id+)
             ("redirect_uri" . ,+redirect-uri+)
             ("scope" . ,+scope+)
             ("code_challenge" . ,challenge)
             ("code_challenge_method" . "S256")
             ("state" . ,state)
             ("id_token_add_organizations" . "true")
             ("codex_cli_simplified_flow" . "true")
             ("originator" . ,+originator+)))))

(defun percent-decode (s)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (i 0) (n (length s)))
    (loop while (< i n) do
      (let ((ch (char s i)))
        (cond
          ((char= ch #\+) (vector-push-extend (char-code #\Space) out) (incf i))
          ((and (char= ch #\%) (<= (+ i 2) (1- n)))
           (vector-push-extend (parse-integer s :start (1+ i) :end (+ i 3) :radix 16) out)
           (incf i 3))
          (t (vector-push-extend (char-code ch) out) (incf i)))))
    (flexi-streams:octets-to-string out :external-format :utf-8)))

(defun parse-query (query)
  (when query
    (loop for pair in (uiop:split-string query :separator "&")
          for eq = (position #\= pair)
          when (plusp (length pair))
            collect (if eq
                        (cons (percent-decode (subseq pair 0 eq))
                              (percent-decode (subseq pair (1+ eq))))
                        (cons (percent-decode pair) "")))))

(defun looks-like-url-p (s)
  (let ((uri (ignore-errors (puri:parse-uri s))))
    (and uri (puri:uri-scheme uri) t)))

(defun parse-authorization-input (input)
  "Returns (values code state). URL -> code#state -> code=... -> bare."
  (let ((value (string-trim '(#\Space #\Tab #\Newline #\Return) input)))
    (cond
      ((zerop (length value)) (values nil nil))
      ((looks-like-url-p value)
       (let ((params (parse-query (puri:uri-query (puri:parse-uri value)))))
         (values (cdr (assoc "code" params :test #'string=))
                 (cdr (assoc "state" params :test #'string=)))))
      ((find #\# value)
       (let ((h (position #\# value)))
         (values (subseq value 0 h) (subseq value (1+ h)))))
      ((search "code=" value)
       (let ((params (parse-query value)))
         (values (cdr (assoc "code" params :test #'string=))
                 (cdr (assoc "state" params :test #'string=)))))
      (t (values value nil)))))

(defun base64url-decode-bytes (s)
  (let* ((s (substitute #\+ #\- (substitute #\/ #\_ s)))
         (pad (mod (- 4 (mod (length s) 4)) 4))
         (padded (concatenate 'string s (make-string pad :initial-element #\=))))
    (cl-base64:base64-string-to-usb8-array padded)))

(defun jwt-account-id (token)
  (let ((parts (uiop:split-string token :separator ".")))
    (when (= (length parts) 3)
      (handler-case
          (let* ((payload (com.inuoe.jzon:parse
                           (flexi-streams:octets-to-string
                            (base64url-decode-bytes (second parts))
                            :external-format :utf-8)))
                 (auth (and (hash-table-p payload)
                            (gethash "https://api.openai.com/auth" payload)))
                 (acct (and (hash-table-p auth)
                            (gethash "chatgpt_account_id" auth))))
            (and (stringp acct) (plusp (length acct)) acct))
        (error () nil)))))

(defvar *token-http* nil
  "Test seam: (grant-type params) -> (values body-string status). NIL => real POST.")

(define-condition token-endpoint-error (error)
  ((grant-type :initarg :grant-type :reader token-endpoint-error-grant-type)
   (status :initarg :status :initform nil :reader token-endpoint-error-status)
   (body :initarg :body :initform nil :reader token-endpoint-error-body))
  (:report
   (lambda (c s)
     (let ((body (token-endpoint-error-body c)))
       (format s "OpenAI token ~A failed~@[ (HTTP ~A)~]: ~A~:[~; -- run /auth login to re-authenticate~]"
               (token-endpoint-error-grant-type c)
               (token-endpoint-error-status c)
               body
               (and (stringp body) (search "invalid_grant" body)))))))

(defmethod kli/ext:condition-category ((condition token-endpoint-error))
  :provider)

(defmethod kli/ext:condition-http-status ((condition token-endpoint-error))
  (token-endpoint-error-status condition))

(define-condition token-network-error (error)
  ((grant-type :initarg :grant-type :initform nil
               :reader token-network-error-grant-type)
   (cause :initarg :cause :reader token-network-error-cause))
  (:report
   (lambda (c s)
     (format s "OpenAI token ~@[~A ~]request failed at the network layer: ~A"
             (token-network-error-grant-type c)
             (token-network-error-cause c)))))

(defmethod kli/ext:condition-category ((condition token-network-error))
  :network)

(defun token-post (params)
  (multiple-value-bind (body status)
      (drakma:http-request +token-url+ :method :post :parameters params
                                       :external-format-out :utf-8)
    (values (if (stringp body)
                body
                (flexi-streams:octets-to-string body :external-format :utf-8))
            status)))

(defun call-token-endpoint (grant-type params)
  "POST to the token endpoint, classifying network-layer failures as
:network-category TOKEN-NETWORK-ERROR so a blip during refresh is labeled
and retryable instead of dying as an internal raw socket condition."
  (handler-bind (((or usocket:socket-error usocket:ns-error
                      cl+ssl::cl+ssl-error stream-error)
                  (lambda (condition)
                    (error 'token-network-error :grant-type grant-type
                                                :cause condition))))
    (if *token-http*
        (funcall *token-http* grant-type params)
        (token-post params))))

(defun parse-token-response (json-string grant-type)
  "Returns (values access refresh expires-universal-time)."
  (let* ((obj (com.inuoe.jzon:parse json-string))
         (access (and (hash-table-p obj) (gethash "access_token" obj)))
         (refresh (and (hash-table-p obj) (gethash "refresh_token" obj)))
         (expires-in (and (hash-table-p obj) (gethash "expires_in" obj))))
    (unless (and (stringp access) (stringp refresh) (realp expires-in))
      (error 'token-endpoint-error :grant-type grant-type
                                   :body (format nil "missing fields: ~A" json-string)))
    (values access refresh
            (+ (get-universal-time) (truncate expires-in) (- +refresh-margin+)))))

(defun token-exchange (code verifier)
  (multiple-value-bind (body status)
      (call-token-endpoint
       "authorization_code"
       `(("grant_type" . "authorization_code")
         ("client_id" . ,+client-id+)
         ("code" . ,code)
         ("code_verifier" . ,verifier)
         ("redirect_uri" . ,+redirect-uri+)))
    (unless (and (integerp status) (<= 200 status 299))
      (error 'token-endpoint-error :grant-type "authorization_code"
                                   :status status :body body))
    (parse-token-response body "authorization_code")))

(defun token-refresh (refresh-token)
  (multiple-value-bind (body status)
      (call-token-endpoint
       "refresh_token"
       `(("grant_type" . "refresh_token")
         ("refresh_token" . ,refresh-token)
         ("client_id" . ,+client-id+)))
    (unless (and (integerp status) (<= 200 status 299))
      (error 'token-endpoint-error :grant-type "refresh_token"
                                   :status status :body body))
    (parse-token-response body "refresh_token")))

(defun fetch-refreshed-oauth-tokens (reference refresh-token)
  "Provider token-endpoint refresh for the codex flow: exchange REFRESH-TOKEN and
return (values access refresh expires account-id). The OpenAI-specific HTTP and
JWT account extraction live here; updating the reference and persisting the
record is the core's responsibility."
  (unless (and (stringp refresh-token) (plusp (length refresh-token)))
    (error 'token-endpoint-error :grant-type "refresh_token"
                                 :body "no refresh token on reference"))
  (multiple-value-bind (access refresh expires) (token-refresh refresh-token)
    (values access refresh expires
            (or (jwt-account-id access)
                (oauth-credential-reference-account-id reference)))))
