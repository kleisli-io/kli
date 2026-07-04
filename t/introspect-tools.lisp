(in-package #:kli/tests)

(defun install-introspect-tools (context)
  "The introspection provider plus its three read-only tools, with
standard-object and journal so the registry holds inspectable live objects."
  (install-extensions context
                      obj:*standard-object-extension-manifest*
                      intro:*introspection-extension-manifest*
                      journal:*journal-extension-manifest*
                      tools-introspect:*list-objects-tool-extension-manifest*
                      tools-introspect:*context-summary-tool-extension-manifest*
                      tools-introspect:*inspect-tool-extension-manifest*))

(test image-eval-implies-image-inspect
  "Eval confers the weaker read-only inspect atom; inspect confers nothing
back. The union fold leaves eval's existing implications intact."
  (is (member :image/inspect (ext:expand-implications '(:image/eval))))
  (is (member :file/write (ext:expand-implications '(:image/eval))))
  (is (not (member :image/eval (ext:expand-implications '(:image/inspect)))))
  (is (ext:check-capability (ext:make-subject :capabilities '(:image/eval))
                            :image/inspect))
  (is (not (ext:check-capability
            (ext:make-subject :capabilities '(:image/inspect))
            :image/eval))))

(test introspect-tools-surface-live-image-data
  "Each tool relays its provider entry; an id from list-objects round-trips
through inspect, and an unknown id reports not-found."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-introspect-tools context)
    (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/inspect))))
      (let* ((result (ext:invoke-tool protocol :list-objects '() context))
             (objects (getf (ext:tool-result-details result) :objects)))
        (is (not (ext:tool-result-error-p result)))
        (is (every #'stringp objects))
        (is (member "JOURNAL-SERVICE" objects :test #'string=))
        (is (search "JOURNAL-SERVICE" (tool-result-text result))))
      (let* ((result (ext:invoke-tool protocol :context-summary '() context))
             (details (ext:tool-result-details result)))
        (is (not (ext:tool-result-error-p result)))
        (is (eq :extension-protocol (getf details :active-protocol)))
        (is (member :journal-service (getf details :objects))))
      (let* ((result (ext:invoke-tool protocol :inspect
                                      '(:id "JOURNAL-SERVICE") context))
             (details (ext:tool-result-details result)))
        (is (not (ext:tool-result-error-p result)))
        (is (eq :journal-service (getf details :id))))
      (let ((result (ext:invoke-tool protocol :inspect
                                     '(:id "NO-SUCH-OBJECT") context)))
        (is (ext:tool-result-error-p result))
        (is (search "No live object" (tool-result-text result)))))))

(test introspect-tools-gate-on-image-inspect
  "The tools gate on the weakest :image/* atom: an empty subject is denied at
mediation, an inspect-only subject invokes all three yet still cannot eval, and
an eval holder reaches them through the implication closure."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-introspect-tools context)
    (install-extension context tools-eval:*eval-tool-extension-manifest*)
    (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (ext:invoke-tool protocol :list-objects '() context))
      (signals ext:capability-denied
        (ext:invoke-tool protocol :context-summary '() context))
      (signals ext:capability-denied
        (ext:invoke-tool protocol :inspect '(:id "JOURNAL-SERVICE") context)))
    (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/inspect))))
      (is (not (ext:tool-result-error-p
                (ext:invoke-tool protocol :list-objects '() context))))
      (is (not (ext:tool-result-error-p
                (ext:invoke-tool protocol :context-summary '() context))))
      (is (not (ext:tool-result-error-p
                (ext:invoke-tool protocol :inspect
                                 '(:id "JOURNAL-SERVICE") context))))
      (signals ext:capability-denied
        (ext:invoke-tool protocol :eval '(:form "1") context)))
    (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
      (is (not (ext:tool-result-error-p
                (ext:invoke-tool protocol :list-objects '() context))))
      (is (not (ext:tool-result-error-p
                (ext:invoke-tool protocol :context-summary '() context))))
      (is (not (ext:tool-result-error-p
                (ext:invoke-tool protocol :inspect
                                 '(:id "JOURNAL-SERVICE") context)))))))

;;; Context-pollution bounds: the printer caps depth and breadth, each rendered
;;; line is front-truncated, and list-objects caps the id count.

(test introspect-bounded-prin1-bounds-depth-breadth-and-line-width
  "The printer bounds depth and length, and each rendered line is front-truncated."
  (let ((tools-introspect:*introspect-print-level* 3)
        (tools-introspect:*introspect-print-length* 5)
        (text:*render-line-limit* 40))
    (is (search "#" (tools-introspect::bounded-prin1 '((((((:deep)))))))))
    (is (search "..." (tools-introspect::bounded-prin1
                       (loop for i below 100 collect i))))
    (is (search "[+" (tools-introspect::bounded-prin1
                      (make-string 100 :initial-element #\x))))))

(test list-objects-count-cap-elides-a-large-registry
  "Below the object limit, list-objects renders at most the limit, notes the
hidden count, reports :total/:truncated, and retains the full id list behind a
sequence handle."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (tools-introspect:*introspect-object-limit* 1)
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-introspect-tools context)
           (install-extension context spill:*output-spill-extension-manifest*)
           (let* ((provider (tools-introspect::introspection-provider context))
                  (ids (mapcar (lambda (id) (ext:provider-call provider :object-id-string id))
                               (ext:provider-call provider :registry-object-ids
                                                  (kli:context-registry context))))
                  (hidden-id (second ids))
                  (ext:*call-subject* (ext:make-subject :capabilities '(:image/inspect
                                                                        :result/read)))
                  (result (ext:invoke-tool protocol :list-objects '() context))
                  (details (ext:tool-result-details result))
                  (total (getf details :total))
                  (shown (getf details :objects))
                  (handle (getf details :result-handle)))
             (is (<= (length shown) 1))
             (is (eql (and (> total 1) t) (and (getf details :truncated) t)))
             (when (> total 1)
               (is (search "more object" (tool-result-text result)))
               (is (search "read-result" (tool-result-text result)))
               (is (stringp handle) "the full object id sequence is retained")
               (is (not (search hidden-id (tool-result-text result)))
                   "the hidden id is not in the inline window")
               (let ((page (ext:invoke-tool protocol :read-result
                                            (list :handle handle :start 1 :limit 5)
                                            context)))
                 (is (search hidden-id (tool-result-text page))
                     "read-result reaches the id dropped from the inline window")))))
      (ignore-errors
        (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                    :validate t :if-does-not-exist :ignore)))))
