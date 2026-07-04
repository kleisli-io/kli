(in-package #:kli/tests)

(in-suite all)

;;; Attenuated delegation of lifted tools. Each lifted tool carries a per-tool
;;; lattice coordinate; invoke-tool builds the request from the live arguments
;;; and gates it; delegation confers an attenuated grant to a child principal as
;;; a reversible, snapshot-able grant-contribution. All pure -- no subprocess --
;;; except the closing end-to-end lift, which spawns the fixture server and runs
;;; in the sandbox. Reuses the sh fixture and isolated-fixture-spec from
;;; t/isolated-pin / t/mcp-client.

(defun delegation-tool (name coordinate)
  (ext:make-tool :name name
                 :runner (lambda (&rest ignored) (declare (ignore ignored)) nil)
                 :metadata (list :coordinate coordinate)))

(defun delegation-admits-p (subject tool args)
  "Non-nil when TOOL's gate admits a call with ARGS under SUBJECT."
  (let ((ext:*call-subject* subject))
    (handler-case (progn (ext::mediate-tool-call tool args) t)
      (ext:capability-denied () nil))))

(defun string-key-hash-table (&rest kvs)
  "A string-keyed hash-table from alternating key/value arguments -- the shape a
parsed-JSON tool call hands the gate."
  (loop with h = (make-hash-table :test #'equal)
        for (k v) on kvs by #'cddr do (setf (gethash k h) v)
        finally (return h)))

(defun capability-tool (name capabilities)
  "A tool gated only by its supply-side CAPABILITIES list -- the shape the
introspect / eval / trace surfaces carry, so a delegated principal is decided at
the same gate the real tools use."
  (ext:make-tool :name name
                 :runner (lambda (&rest ignored) (declare (ignore ignored)) nil)
                 :metadata (list :capabilities capabilities)))

(defun image-family-grant (&rest capabilities)
  "The grant a holder of CAPABILITIES carries, closed under the :image/* family
order (debug confers eval confers inspect). Built through the subject so the
implication closure is materialized -- the cover check is a flat lookup and does
not expand at check time."
  (ext:subject-grant (ext:make-subject :capabilities capabilities)))

(test coordinate-gate-enforces-argument-constraint
  (let* ((atom (ext:lifted-tool-atom :fs "read"))
         (tool (delegation-tool :read
                                (list :atom atom :constraint :path-prefix :arg :path)))
         (under-tmp (ext:make-subject
                     :grant (ext:make-grant
                             :atoms (list (cons atom
                                                (ext:path-prefix-constraint "/tmp/")))))))
    (is (delegation-admits-p under-tmp tool '(:path "/tmp/x"))
        "a path under the confined prefix is admitted")
    (is (null (delegation-admits-p under-tmp tool '(:path "/etc/x")))
        "a path outside the prefix is denied")))

(test coordinate-gate-reads-hash-table-args
  "Agent tool arguments arrive as a string-keyed hash-table; a constrained
coordinate must read them through tool-parameter, not getf, which errors on a
hash-table. Asserts no crash plus the correct admit/deny under the live arg."
  (let* ((atom (ext:lifted-tool-atom :fs "read"))
         (tool (delegation-tool :read
                                (list :atom atom :constraint :path-prefix :arg :path)))
         (under-tmp (ext:make-subject
                     :grant (ext:make-grant
                             :atoms (list (cons atom
                                                (ext:path-prefix-constraint "/tmp/")))))))
    (is (delegation-admits-p under-tmp tool (string-key-hash-table "path" "/tmp/x"))
        "a hash-table path under the prefix is admitted with no getf crash")
    (is (null (delegation-admits-p under-tmp tool (string-key-hash-table "path" "/etc/x")))
        "a hash-table path outside the prefix is denied")))

(test delegation-confers-attenuated-and-rejects-escalation
  (let* ((protocol (ext:make-extension-protocol))
         (tools '("read" "write" "delete"))
         (a-read (ext:lifted-tool-atom :fs "read"))
         (a-delete (ext:lifted-tool-atom :fs "delete")))
    (ext:install-contribution
     protocol
     (ext:make-grant-contribution :principal :planner
                                  :grant (ext:lifted-server-grant :fs tools))
     nil)
    ;; planner confines the child's read to /tmp -- an attenuation it covers
    (ext:delegate-grant protocol :planner :triager
                        (ext:make-grant
                         :atoms (list (cons a-read
                                            (ext:path-prefix-constraint "/tmp/")))))
    (is (ext:grant-covers-p (ext:grant-set-lookup protocol :planner)
                            (ext:grant-set-lookup protocol :triager))
        "the delegated grant stays under the parent's authority")
    ;; the triager cannot widen its confined read when re-delegating
    (signals ext:grant-escalation
      (ext:delegate-grant protocol :triager :grandchild
                          (ext:make-grant
                           :atoms (list (cons a-read
                                              (ext:path-prefix-constraint "/"))))))
    ;; nor confer an atom it was never delegated
    (signals ext:grant-escalation
      (ext:delegate-grant protocol :triager :grandchild
                          (ext:make-grant :capabilities (list a-delete))))))

(test revocation-is-sibling-independent
  (let* ((protocol (ext:make-extension-protocol))
         (a-read (ext:lifted-tool-atom :fs "read"))
         (a-write (ext:lifted-tool-atom :fs "write"))
         (ca (ext:make-grant-contribution
              :principal :a :grant (ext:make-grant :capabilities (list a-read))))
         (cb (ext:make-grant-contribution
              :principal :b :grant (ext:make-grant :capabilities (list a-write)))))
    (ext:install-contribution protocol ca nil)
    (ext:install-contribution protocol cb nil)
    (ext:retract-contribution protocol ca nil)
    (is (null (ext:grant-set-has-p protocol :a)) "the revoked delegatee is gone")
    (is (ext:grant-set-has-p protocol :b) "the sibling survives")
    (is (ext:check-authority protocol :b a-write)
        "the sibling keeps its authority")))

(test delegation-tree-snapshots-and-restores
  (let* ((source (ext:make-extension-protocol))
         (a-read (ext:lifted-tool-atom :fs "read"))
         (read-coord (list :atom a-read :constraint :path-prefix :arg :path)))
    (ext:install-contribution
     source
     (ext:make-grant-contribution :principal :planner
                                  :grant (ext:lifted-server-grant :fs '("read" "write")))
     nil)
    (ext:delegate-grant source :planner :triager
                        (ext:make-grant
                         :atoms (list (cons a-read
                                            (ext:path-prefix-constraint "/tmp/")))))
    (multiple-value-bind (captured skipped)
        (snapshot::snapshot-protocol-storage source)
      (is (null skipped) "the delegation policy serializes whole")
      (let ((restored (ext:make-extension-protocol)))
        (snapshot::rehydrate-protocol-storage restored (list :storage captured))
        (is (ext:grant-equiv-p (ext:grant-set-lookup source :triager)
                               (ext:grant-set-lookup restored :triager))
            "the delegatee restores with identical authority")
        (is (ext:check-authority restored :triager
                                 (ext:coordinate-request read-coord '(:path "/tmp/x")))
            "the restored child still reaches its confined slice")
        (is (null (ext:check-authority restored :triager
                                       (ext:coordinate-request read-coord '(:path "/etc/x"))))
            "the restored child is still confined")))))

;;; Attenuated debug delegation over the :image/* family. A lead holding the
;;; strongest image atom confers only the weakest, so a triage subagent reaches
;;; read-only introspection but neither eval nor the interactive debugger; the
;;; conferral rides snapshot/restore as one grant-set datum and revokes alone.

(test debug-lead-delegates-inspect-as-read-only-confinement
  (let ((protocol (ext:make-extension-protocol)))
    (ext:install-contribution
     protocol
     (ext:make-grant-contribution :principal :lead
                                  :grant (image-family-grant :image/debug))
     nil)
    (ext:delegate-grant protocol :lead :triager
                        (ext:make-grant :capabilities '(:image/inspect)))
    (let ((triager (ext:principal-subject protocol :triager)))
      (is (delegation-admits-p triager (capability-tool :inspect '(:image/inspect)) '())
          "the triager reaches read-only introspection")
      (is (null (delegation-admits-p triager (capability-tool :eval '(:image/eval)) '()))
          "the triager cannot evaluate in the live image")
      (is (null (delegation-admits-p triager (capability-tool :debug '(:image/debug)) '()))
          "the triager cannot drive the interactive debugger"))))

(test inspect-holder-cannot-confer-eval-or-debug
  (let ((protocol (ext:make-extension-protocol)))
    (ext:install-contribution
     protocol
     (ext:make-grant-contribution :principal :weak
                                  :grant (image-family-grant :image/inspect))
     nil)
    (signals ext:grant-escalation
      (ext:delegate-grant protocol :weak :child
                          (ext:make-grant :capabilities '(:image/eval))))
    (signals ext:grant-escalation
      (ext:delegate-grant protocol :weak :child
                          (ext:make-grant :capabilities '(:image/debug))))))

(test image-delegation-snapshots-and-restores-as-one-datum
  (let ((source (ext:make-extension-protocol)))
    (ext:install-contribution
     source
     (ext:make-grant-contribution :principal :lead
                                  :grant (image-family-grant :image/debug))
     nil)
    (ext:delegate-grant source :lead :triager
                        (ext:make-grant :capabilities '(:image/inspect)))
    (multiple-value-bind (captured skipped)
        (snapshot::snapshot-protocol-storage source)
      (is (null skipped) "the delegation policy serializes whole")
      (let ((restored (ext:make-extension-protocol)))
        (snapshot::rehydrate-protocol-storage restored (list :storage captured))
        (is (ext:grant-equiv-p (ext:grant-set-lookup source :triager)
                               (ext:grant-set-lookup restored :triager))
            "the delegatee restores with identical authority")
        (is (delegation-admits-p (ext:principal-subject restored :triager)
                                 (capability-tool :inspect '(:image/inspect)) '())
            "the restored triager still reaches read-only introspection")
        (is (null (delegation-admits-p (ext:principal-subject restored :triager)
                                       (capability-tool :eval '(:image/eval)) '()))
            "the restored triager is still confined out of eval")))))

(test image-delegation-revoke-is-sibling-independent
  (let ((protocol (ext:make-extension-protocol))
        (context (kli:make-kernel-host)))
    (ext:install-contribution
     protocol
     (ext:make-grant-contribution :principal :lead
                                  :grant (image-family-grant :image/debug))
     nil)
    (ext:delegate-grant protocol :lead :triager-a
                        (ext:make-grant :capabilities '(:image/inspect)))
    (ext:delegate-grant protocol :lead :triager-b
                        (ext:make-grant :capabilities '(:image/inspect)))
    (ext:with-system-authority
      (ext:deactivate-extension
       protocol
       (ext:make-extension :id (ext:delegation-owner-id :lead :triager-a))
       context))
    (is (null (ext:grant-set-has-p protocol :triager-a))
        "the revoked triager loses its inspect access")
    (is (ext:grant-set-has-p protocol :triager-b)
        "the sibling delegation survives")
    (is (delegation-admits-p (ext:principal-subject protocol :triager-b)
                             (capability-tool :inspect '(:image/inspect)) '())
        "the sibling triager keeps read-only introspection")))

(test empty-principal-denies-everything
  (let* ((protocol (ext:make-extension-protocol))
         (tool (delegation-tool :read
                                (list :atom (ext:lifted-tool-atom :fs "read")))))
    (is (null (delegation-admits-p (ext:principal-subject protocol :ghost) tool '()))
        "an unrecorded principal carries the bottom grant -- denies all")))

(test agent-principal-projects-grant-set-with-slot-fallback
  (let* ((protocol (ext:make-extension-protocol))
         (a-read (ext:lifted-tool-atom :fs "read"))
         (a-write (ext:lifted-tool-atom :fs "write")))
    (ext:install-contribution
     protocol
     (ext:make-grant-contribution :principal :child
                                  :grant (ext:make-grant :capabilities (list a-read)))
     nil)
    (let ((agent (make-instance 'agents::agent
                                :principal :child
                                :subject (ext:make-system-subject))))
      (is (ext:check-capability (agents::agent-call-subject agent protocol) a-read)
          "a recorded principal projects its grant-set authority")
      (is (null (ext:check-capability (agents::agent-call-subject agent protocol)
                                      a-write))
          "the projection is confined to the delegated slice"))
    (let ((agent (make-instance 'agents::agent
                                :principal :unseeded
                                :subject (ext:make-system-subject))))
      (is (ext:check-capability (agents::agent-call-subject agent protocol) a-write)
          "a principal with no grant-set entry falls back to the slot subject"))
    (let ((agent (make-instance 'agents::agent
                                :subject (ext:make-subject :capabilities '(:x)))))
      (is (ext:check-capability (agents::agent-call-subject agent protocol) :x)
          "a principal-less agent is gated by its slot subject unchanged"))))

(test (delegation-confines-lifted-tool-end-to-end :fixture lifted-tool-authority)
  "Lift the fixture server, delegate only its echo atom to a child principal, and
confirm invoke-tool admits the child's slice and denies an unrecorded principal."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (multiple-value-bind (state handle)
        (app:install-isolated-extension (isolated-fixture-spec) protocol context
                                        :confirm-fn (lambda (stage card)
                                                      (declare (ignore stage card)) t))
      (is (eq :installed state))
      (unwind-protect
           (let ((echo (ext:lifted-tool-atom :fixture "echo")))
             (ext:install-contribution
              protocol
              (ext:make-grant-contribution
               :principal :planner :grant (ext:lifted-server-grant :fixture '("echo")))
              context)
             (ext:delegate-grant protocol :planner :child
                                 (ext:make-grant :capabilities (list echo)))
             (let ((ext:*call-subject* (ext:principal-subject protocol :child)))
               (is (not (ext:tool-result-error-p
                         (ext:invoke-tool protocol "echo" nil context)))
                   "the delegated child invokes its slice end-to-end"))
             (let ((ext:*call-subject* (ext:principal-subject protocol :stranger)))
               (signals ext:capability-denied
                 (ext:invoke-tool protocol "echo" nil context))))
        (ext:retract-manifest handle protocol context)))))

;;; Coordinate derivers. When a tool's gate constraint is not a live named
;;; argument -- it lives in a patch body, say -- the tool registers a deriver and
;;; the kernel computes the request, so authority is still decided at the one
;;; gate seam.

(test coordinate-requests-fans-out-one-request-per-patched-path
  "The edit patch-path deriver yields one path-prefix request per file the patch
targets, so a multi-file patch fans out to several requests."
  (let* ((requests (ext::coordinate-requests
                    '(:atom :file/edit :deriver :edit-patch-paths)
                    (list :input (format nil "@@ /tmp/a~%+~%~~hi~%@@ /etc/b~%+~%~~yo"))))
         (constraints (mapcar (lambda (r) (ext:grant-constraint r :file/edit)) requests)))
    (is (= 2 (length requests)))
    (is (member '(:path-prefix "/tmp/a") constraints :test #'equal))
    (is (member '(:path-prefix "/etc/b") constraints :test #'equal))))

(test coordinate-requests-garbage-patch-demands-nothing
  "A patch that does not parse targets no file, so it derives no request; the
runner rejects it downstream before any write."
  (is (null (ext::coordinate-requests
             '(:atom :file/edit :deriver :edit-patch-paths)
             (list :input "not a patch")))))

(test coordinate-requests-singular-arg-path-is-unchanged
  "A literal-arg coordinate still yields exactly one request from the named
parameter, so the lifted-tool and delegation path is untouched."
  (let ((requests (ext::coordinate-requests
                   '(:atom :file/write :constraint :path-prefix :arg :path)
                   '(:path "/tmp/x"))))
    (is (= 1 (length requests)))
    (is (equal '(:path-prefix "/tmp/x")
               (ext:grant-constraint (first requests) :file/write)))))

(test coordinate-deriver-unregistered-name-errors
  (signals error (ext::coordinate-deriver :no-such-deriver)))

(test mediate-skips-coordinate-atom-but-honors-other-capabilities
  "The coordinate atom is demanded only through its argument-aware request, never
re-demanded coarse; a sibling capability the coordinate does not name is still
demanded unconstrained, so the fail-safe holds for it."
  (let ((tool (ext:make-tool
               :name :probe
               :runner (lambda (&rest ignored) (declare (ignore ignored)) nil)
               :metadata '(:coordinate (:atom :file/write :constraint :path-prefix :arg :path)
                           :capabilities (:file/write :other)))))
    (is (delegation-admits-p
         (ext:make-subject
          :grant (ext:make-grant
                  :atoms (list (cons :file/write (ext:path-prefix-constraint "/tmp/"))
                               (cons :other (ext:constraint-any)))))
         tool '(:path "/tmp/x"))
        "a constrained writer that also holds the sibling cap is admitted in-bounds")
    (is (null (delegation-admits-p
               (ext:make-subject
                :grant (ext:make-grant
                        :atoms (list (cons :file/write
                                           (ext:path-prefix-constraint "/tmp/")))))
               tool '(:path "/tmp/x")))
        "missing the sibling cap is denied even within the coordinate bounds")))
