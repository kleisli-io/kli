(in-package #:kli/tests)

(in-suite all)

;;; Inbound lift: a connected MCP server materialized as a first-class kli
;;; manifest. Reuses the sh fixture and the event recorder from t/mcp-client.
;;; The lift manifest connects on install, so the recorder context supplies the
;;; events extension the server notification needs.

(defun lift-fixture-manifest ()
  (isolated:lift-mcp-server "sh"
                            :arguments (list "-c" (mcp-fixture-script))
                            :id :fixture
                            :timeout 5))

(defun lift-effect-client (protocol handle)
  "The mcp-client held as the lift effect's state, found by the extension id."
  (let ((effect (find-if (lambda (contribution)
                           (and (typep contribution 'ext:effect-contribution)
                                (equal (ext:contribution-extension contribution)
                                       (kli:object-id handle))))
                         (ext:protocol-installed-contributions protocol))))
    (and effect (ext:contribution-state effect))))

(test (inbound-lift-materializes-and-reverses :fixture extension-load-authority)
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let* ((protocol (kli:active-protocol context))
            (handle (with-extension-load-authority
                      (ext:install-manifest (lift-fixture-manifest)
                                            protocol context)))
            (client (lift-effect-client protocol handle)))
       (is (not (null (ext:find-tool protocol "echo")))
           "the server tool installs into the protocol")
       (is (member "echo" (ext:list-tools protocol)
                   :key #'ext:tool-name :test #'string=))
       (is (member "file:///a" (ext:list-resources protocol)
                   :key #'ext:resource-uri :test #'string=)
           "the server resource installs as a first-class resource")
       (is (isolated:isolated-process-alive-p
            (isolated:mcp-client-process client))
           "the server child is alive while installed")
       (ext:retract-manifest handle protocol context)
       (is (null (ext:find-tool protocol "echo")))
       (is (null (ext:list-tools protocol)) "retract removes the tool")
       (is (null (ext:list-resources protocol)) "retract removes the resource")
       (is (not (isolated:isolated-process-alive-p
                 (isolated:mcp-client-process client)))
           "retract reaps the server child")))))

(test (inbound-lift-invokes-and-reads-through-protocol :fixture lifted-tool-authority)
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let* ((protocol (kli:active-protocol context))
            (handle (with-extension-load-authority
                      (ext:install-manifest (lift-fixture-manifest)
                                            protocol context))))
       (unwind-protect
            (let ((result (ext:invoke-tool protocol "echo" nil context)))
              (is (not (ext:tool-result-error-p result)))
              (is (string= "called"
                           (getf (first (ext:tool-result-content result))
                                 :text))
                  "invoking the lifted tool routes through tools/call")
              (let ((contents (ext:read-resource protocol "file:///a")))
                (is (string= "body" (getf (first contents) :text))
                    "reading the lifted resource routes through resources/read")))
         (ext:retract-manifest handle protocol context))))))

(test (inbound-lift-mediates-invocation-by-capability :fixture extension-load-authority)
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let* ((protocol (kli:active-protocol context))
            (handle (with-extension-load-authority
                      (ext:install-manifest (lift-fixture-manifest)
                                            protocol context))))
       (unwind-protect
            (progn
              (let ((ext:*call-subject*
                      (ext:make-subject :capabilities '(:other))))
                (signals ext:capability-denied
                  (ext:invoke-tool protocol "echo" nil context)))
              (let* ((ext:*call-subject*
                       (ext:make-subject
                        :grant (ext:lifted-server-grant :fixture '("echo"))))
                     (result (ext:invoke-tool protocol "echo" nil context)))
                (is (not (ext:tool-result-error-p result))
                    "a covering grant admits the call")))
         (ext:retract-manifest handle protocol context))))))

(test (inbound-lift-bridges-server-notifications :fixture extension-load-authority)
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (let* ((protocol (kli:active-protocol context))
            (handle (with-extension-load-authority
                      (ext:install-manifest (lift-fixture-manifest)
                                            protocol context))))
       (unwind-protect
            (let ((deadline (+ (get-internal-real-time)
                               (* 5 internal-time-units-per-second))))
              (loop until (or (plusp (length recorded))
                              (>= (get-internal-real-time) deadline))
                    do (sleep 0.02))
              (is (plusp (length recorded))
                  "the server notification surfaced as a kli event")
              (let ((event (aref recorded 0)))
                (is (eq :mcp/notification (event:event-type event)))
                (is (string= "notifications/message"
                             (getf (event:event-payload event) :method)))))
         (ext:retract-manifest handle protocol context))))))
