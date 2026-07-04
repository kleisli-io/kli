(in-package #:kli/tests)
(in-suite all)

(defun call-with-observability-context (thunk)
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj")))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          observability:*observability-extension-manifest*))
    (funcall thunk context root)))

(defmacro with-observability-context ((context-var &optional (root-var (gensym "ROOT")))
                                      &body body)
  `(call-with-observability-context
    (lambda (,context-var ,root-var)
      (declare (ignorable ,root-var))
      ,@body)))

(defun observability-sink-object (context)
  (kli:find-live-object (kli:context-registry context) :observability-sink))

(defun jsonl-lines (path)
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil nil)
            while line collect line))))

(test observability-make-event-stamps-wall-clock-timestamp
  (let* ((before (get-universal-time))
         (event (event:make-event :probe/timestamp :source :test))
         (after (get-universal-time)))
    (is (integerp (event:event-timestamp event)))
    (is (<= before (event:event-timestamp event) after))))

(test (observability-session-emit-sites-carry-source :fixture interactive-authority)
  (let* ((context (agent-session-test-context))
         (service (agent-session-service context))
         (seen '()))
    (agent-session:register-session-event-listener
     service
     (agent-session:make-session-event-listener
      :source-probe
      (lambda (event mode-id ctx)
        (declare (ignore mode-id ctx))
        (push (cons (event:event-type event) (event:event-source event)) seen)))
     context)
    (bind-agent-session-mode context)
    (agent-session:reset-agent-session service :default-mode context)
    (agent-session:clear-agent-session service :default-mode context)
    (dolist (type '(:session-switch :session-reset :session-cleared))
      (let ((entry (assoc type seen)))
        (is (not (null entry)) "expected a ~A event" type)
        (is (not (null (cdr entry)))
            "~A must carry a non-nil source" type)))))

(test observability-serialization-round-trips-through-jzon
  (let* ((event (event:make-event
                 :probe/serialize
                 :source :probe-seam
                 :payload (list :tool-name "echo"
                                :state :running
                                :missing nil
                                :items (list 1 2 3)
                                :nested (list :depth 2 :flag t))))
         (record (com.inuoe.jzon:parse (observability::event->line event))))
    (is (integerp (gethash "timestamp" record)))
    (is (equal "probe/serialize" (gethash "type" record)))
    (is (equal "probe-seam" (gethash "source" record)))
    (let ((payload (gethash "payload" record)))
      (is (equal "echo" (gethash "tool-name" payload)))
      (is (equal "running" (gethash "state" payload)))
      (is (null (nth-value 1 (gethash "missing" payload)))
          "nil payload values are omitted, never serialized as false")
      (is (equalp #(1 2 3) (gethash "items" payload)))
      (let ((nested (gethash "nested" payload)))
        (is (= 2 (gethash "depth" nested)))
        (is (eq t (gethash "flag" nested)))))))

(test observability-sink-appends-one-jsonl-line-per-event
  (with-observability-context (context root)
    (let* ((path (merge-pathnames "events.jsonl" root))
           (sink (observability-sink-object context)))
      (setf (observability:sink-path sink) path)
      (dotimes (i 3)
        (event:emit-event context
                          (event:make-event :probe/line :payload (list :i i)
                                            :source :test)))
      (let ((lines (jsonl-lines path)))
        (is (= 3 (length lines)))
        (is (= 3 (observability:sink-write-count sink)))
        (dolist (line lines)
          (is (hash-table-p (com.inuoe.jzon:parse line))))))))

(test observability-filter-keeps-only-matching-prefixes
  (with-observability-context (context root)
    (let* ((path (merge-pathnames "filtered.jsonl" root))
           (sink (observability-sink-object context)))
      (setf (observability:sink-path sink) path
            (observability::sink-filter sink) (list "agent/"))
      (event:emit-event context (event:make-event :agent/turn-start :source :test))
      (event:emit-event context (event:make-event :session-reset :source :test))
      (event:emit-event context (event:make-event :agent/turn-end :source :test))
      (let ((types (mapcar (lambda (line)
                             (gethash "type" (com.inuoe.jzon:parse line)))
                           (jsonl-lines path))))
        (is (= 2 (length types)))
        (is (every (lambda (type) (eql 0 (search "agent/" type))) types))))))

(test observability-disabled-sink-writes-nothing-and-never-faults
  (with-observability-context (context root)
    (let ((sink (observability-sink-object context))
          (path (merge-pathnames "absent.jsonl" root)))
      (is (null (observability:sink-path sink)))
      (is (not (observability::sink-enabled-p sink)))
      (event:emit-event context (event:make-event :probe/disabled :source :test))
      (is (null (probe-file path)))
      (is (= 0 (observability:sink-write-count sink))))))

(test observability-captures-contained-fault-as-event
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (with-observability-context (context root)
      (let* ((path (merge-pathnames "faults.jsonl" root))
             (sink (observability-sink-object context))
             (ext:*extension-fault-policy* nil)
             (ext:*fault-note-hook* (event:make-fault-emitter context)))
        (setf (observability:sink-path sink) path)
        (is (null (ext:safely-invoke (lambda () (error "boom-fault"))
                                     :test-seam :unit-7))
            "containment holds -- safely-invoke returns the fallback")
        (let ((records (mapcar #'com.inuoe.jzon:parse (jsonl-lines path))))
          (is (= 1 (length records)))
          (let ((record (first records)))
            (is (equal "fault" (gethash "type" record)))
            (is (equal "test-seam" (gethash "source" record)))
            (let ((payload (gethash "payload" record)))
              (is (equal "test-seam" (gethash "seam" payload)))
              (is (equal "unit-7" (gethash "unit" payload)))
              (is (search "boom-fault" (gethash "condition" payload))))))))))

(test observability-write-failure-records-last-error-without-signalling
  (with-observability-context (context root)
    (let* ((blocker (merge-pathnames "blocker" root))
           (sink (observability-sink-object context)))
      (with-open-file (out blocker :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
        (write-string "x" out))
      (setf (observability:sink-path sink)
            (pathname (concatenate 'string (namestring blocker) "/sink.jsonl")))
      (finishes
       (event:emit-event context
                         (event:make-event :probe/unwritable :source :test)))
      (is (= 0 (observability:sink-write-count sink)))
      (is (not (null (observability::sink-last-error sink)))))))

(test observability-command-reports-sink-state
  (with-observability-context (context root)
    (let* ((path (merge-pathnames "command.jsonl" root))
           (sink (observability-sink-object context)))
      (setf (observability:sink-path sink) path)
      (event:emit-event context (event:make-event :probe/command :source :test))
      (let ((text (command-result-text
                   context
                   (invoke-test-command context :observability))))
        (is (search "enabled" text))
        (is (search (namestring path) text))
        (is (search "events-written:" text))
        (is (null (search "events-written: 0" text))
            "the sink counted at least the emitted event")))))

(test observability-tool-call-arguments-hash-table-round-trips
  "A jzon-parsed arguments table -- the shape every tool call carries -- must
round-trip its keys and values instead of degrading to an address string."
  (let ((arguments (make-hash-table :test #'equal)))
    (setf (gethash "file_path" arguments) "/tmp/x"
          (gethash "limit" arguments) 40
          (gethash "missing" arguments) nil
          (gethash "nested" arguments) (list :depth 2))
    (let* ((event (event:make-event :probe/tool-call
                                    :source :agent-loop
                                    :payload (list :tool-name "read"
                                                   :arguments arguments)))
           (line (observability::event->line event))
           (record (com.inuoe.jzon:parse line))
           (args (gethash "arguments" (gethash "payload" record))))
      (is (null (search "#<" line))
          "arguments must never render as an opaque printed object")
      (is (hash-table-p args))
      (is (equal "/tmp/x" (gethash "file_path" args)))
      (is (= 40 (gethash "limit" args)))
      (is (null (nth-value 1 (gethash "missing" args)))
          "nil values are omitted, never serialized as false")
      (is (= 2 (gethash "depth" (gethash "nested" args)))))))

(test observability-concurrent-emits-write-distinct-lines
  "N threads emitting M events each yield exactly N*M well-formed JSONL lines
and an exact write count, so concurrent appends neither interleave nor lose
counter updates."
  (with-observability-context (context root)
    (let* ((path (merge-pathnames "concurrent.jsonl" root))
           (sink (observability-sink-object context))
           (threads 6)
           (per-thread 20))
      (setf (observability:sink-path sink) path)
      (mapc #'sb-thread:join-thread
            (loop for tid below threads
                  collect (let ((tid tid))
                            (sb-thread:make-thread
                             (lambda ()
                               (dotimes (i per-thread)
                                 (event:emit-event
                                  context
                                  (event:make-event
                                   :probe/concurrent
                                   :payload (list :thread tid :i i
                                                  :pad (make-string
                                                        2048
                                                        :initial-element #\x))
                                   :source :test))))))))
      (let ((lines (jsonl-lines path)))
        (is (= (* threads per-thread) (length lines)))
        (is (= (* threads per-thread) (observability:sink-write-count sink)))
        (is (every (lambda (line)
                     (ignore-errors
                      (hash-table-p (com.inuoe.jzon:parse line))))
                   lines)
            "every line parses as one JSON object, so no append interleaved")
        (is (null (observability::sink-last-error sink)))))))

(test observability-headless-contained-fault-reaches-sink
  "With no TUI and no manually bound hook, installing the events extension
alone wires the fault emitter, so a contained fault still produces a :fault
record."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*fault-note-hook* nil)
          (ext:*extension-fault-policy* nil))
      (with-observability-context (context root)
        (let ((path (merge-pathnames "headless-faults.jsonl" root))
              (sink (observability-sink-object context)))
          (setf (observability:sink-path sink) path)
          (is (null ext:*fault-note-hook*)
              "the installer routes via the emitter registry, never by setf")
          (is (functionp (event:protocol-fault-emitter
                          (kli:active-protocol context)))
              "the events extension stored the emitter in protocol storage")
          (is (null (ext:safely-invoke (lambda () (error "headless-boom"))
                                       :headless-seam :unit-1))
              "containment holds -- safely-invoke returns the fallback")
          (let ((records (mapcar #'com.inuoe.jzon:parse (jsonl-lines path))))
            (is (= 1 (length records)))
            (let ((record (first records)))
              (is (equal "fault" (gethash "type" record)))
              (is (search "headless-boom"
                          (gethash "condition"
                                   (gethash "payload" record)))))))))))

(test (observability-fault-note-hook-effect-reverts-on-deactivate :fixture interactive-authority)
  "Installing events never touches the seam variable, and deactivating drains
exactly this protocol's emitter wiring."
  (let ((ext:*fault-note-hook* :probe-previous))
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extension context obj:*standard-object-extension-manifest*)
      (let ((events (install-extension context
                                       event:*events-extension-manifest*)))
        (is (eq :probe-previous ext:*fault-note-hook*)
            "the seam variable is only ever let-bound, never setf'd")
        (is (functionp (event:protocol-fault-emitter protocol)))
        (ext:deactivate-extension protocol events context)
        (is (null (event:protocol-fault-emitter protocol))
            "deactivation drains the stored emitter")
        (is (eq :probe-previous ext:*fault-note-hook*))))))

(test (observability-coexisting-protocol-fault-routing-survives-unordered-retract :fixture interactive-authority)
  "Coexisting contexts each wire their own fault emitter. An attributed fault
reaches only its own context's stream, and retracting one context's events
extension cannot strand the other's routing, whatever the retract order."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*fault-note-hook* nil)
          (ext:*extension-fault-policy* nil)
          (faults-a '())
          (faults-b '()))
      (flet ((make-events-context (capture)
               (let* ((context (kli:make-kernel-host))
                      (protocol (switch-to-extension-protocol context)))
                 (install-extension context
                                    obj:*standard-object-extension-manifest*)
                 (let ((events (install-extension
                                context event:*events-extension-manifest*)))
                   (ext:install-contribution
                    protocol
                    (event:make-event-handler-contribution
                     :name :fault-capture
                     :event-type :fault
                     :handler (lambda (event context)
                                (declare (ignore context))
                                (funcall capture event))
                     :source :test)
                    context)
                   (values context protocol events)))))
        (multiple-value-bind (context-a protocol-a events-a)
            (make-events-context (lambda (event) (push event faults-a)))
          (multiple-value-bind (context-b protocol-b events-b)
              (make-events-context (lambda (event) (push event faults-b)))
            (declare (ignore context-b protocol-b events-b))
            (let ((ext:*fault-note-hook*
                    (event:protocol-fault-emitter protocol-a)))
              (ext:safely-invoke (lambda () (error "boom-for-a"))
                                 :coexist-seam :a-unit))
            (is (= 1 (length faults-a))
                "an attributed fault reaches its own context's stream")
            (is (= 0 (length faults-b))
                "an attributed fault stays off the other context's stream")
            (ext:deactivate-extension protocol-a events-a context-a)
            (ext:safely-invoke (lambda () (error "boom-bare"))
                               :coexist-seam :bare-unit)
            (is (= 1 (length faults-a))
                "the departed context gains nothing after its retract")
            (is (= 1 (length faults-b))
                "the survivor still routes -- a non-nested retract cannot strand it")
            (is (search "boom-bare"
                        (getf (event:event-payload (first faults-b))
                              :condition)))))))))

(test (observability-deactivate-drains-sink-command-and-method :fixture interactive-authority)
  "Deactivation reverses the full install: the sink live-object, the
/observability command, and the dispatch-event after-method all drain."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj")))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*))
    (let ((observability (install-extension
                          context
                          observability:*observability-extension-manifest*))
          (path (merge-pathnames "drain.jsonl" root))
          (sink (observability-sink-object context)))
      (setf (observability:sink-path sink) path)
      (event:emit-event context (event:make-event :probe/drain :source :test))
      (is (= 1 (observability:sink-write-count sink)))
      (ext:deactivate-extension protocol observability context)
      (is (null (observability-sink-object context))
          "deactivation removes the sink live-object")
      (is (null (ext:provider-call (command-provider context)
                                   :find-command :observability))
          "deactivation unregisters the /observability command")
      (event:emit-event context
                        (event:make-event :probe/after-drain :source :test))
      (is (= 1 (length (jsonl-lines path)))
          "the after-method no longer writes once retracted"))))

(test (observability-co-install-with-agent-session-fires-both-after-methods :fixture interactive-authority)
  "agent-session and observability contribute dispatch-event after-methods with
distinct specializers. Both must fire on the same emission: the session
listener captures it and the sink writes it."
  (let* ((context (agent-session-test-context))
         (service (agent-session-service context))
         (root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (seen '()))
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj")))
      (install-extensions context
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          observability:*observability-extension-manifest*))
    (agent-session:register-session-event-listener
     service
     (agent-session:make-session-event-listener
      :co-install-probe
      (lambda (event mode-id ctx)
        (declare (ignore mode-id ctx))
        (push (event:event-type event) seen)))
     context)
    (bind-agent-session-mode context)
    (let ((sink (observability-sink-object context))
          (path (merge-pathnames "co-install.jsonl" root)))
      (setf (observability:sink-path sink) path
            (observability::sink-filter sink) (list "session-reset"))
      (agent-session:reset-agent-session service :default-mode context)
      (is (member :session-reset seen)
          "the agent-session after-method captured the emission")
      (is (= 1 (observability:sink-write-count sink))
          "the observability after-method wrote the same emission")
      (let ((lines (jsonl-lines path)))
        (is (= 1 (length lines)))
        (is (equal "session-reset"
                   (gethash "type"
                            (com.inuoe.jzon:parse (first lines)))))))))
