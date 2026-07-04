(in-package #:kli/tests)

(defun make-tui-views-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tui-views:*tui-views-extension-manifest*)
    (values protocol context)))

(test tui-views-extension-registers-service
  (multiple-value-bind (protocol context) (make-tui-views-fixture)
    (declare (ignore context))
    (is (ext:extension-loaded-p protocol :tui-views))))

(test tui-views-are-live-objects
  (let ((generated (tui-views:make-tui-text "x"))
        (explicit (tui-views:make-tui-box :id :custom-box)))
    (is (typep generated 'kli:live-object))
    (is (kli:object-id generated))
    (is (typep explicit 'kli:live-object))
    (is (eq :custom-box (kli:object-id explicit)))))

(test tui-text-renders-with-padding-and-wrapping
  (let* ((protocol (make-tui-views-fixture))
         (text (tui-views:make-tui-text "hello world"
                                        :protocol protocol
                                        :padding-x 1
                                        :padding-y 0)))
    (is (equal (tui-core:render-lines text 8)
               '(" hello  " " world  ")))
    (setf (tui-views:view-text text) "x")
    (is (equal (tui-core:render-lines text 4)
               '(" x  ")))))

(test tui-text-suppresses-blank-content
  (let ((protocol (make-tui-views-fixture)))
    (is (equal '() (tui-core:render-lines
                    (tui-views:make-tui-text "" :protocol protocol :padding-y 1)
                    8)))
    (is (equal '() (tui-core:render-lines
                    (tui-views:make-tui-text "   " :protocol protocol :padding-y 1)
                    8)))))

(test tui-container-mutates-and-renders-children-in-order
  (let* ((protocol (make-tui-views-fixture))
         (container (tui-views:make-tui-container :protocol protocol))
         (first (tui-views:make-tui-text "a" :protocol protocol :padding-x 0 :padding-y 0))
         (second (tui-views:make-tui-text "b" :protocol protocol :padding-x 0 :padding-y 0)))
    (is (eq first (tui-core:add-child container first)))
    (is (eq second (tui-core:add-child container second)))
    (is (equal (list first second)
               (tui-core:children container)))
    (is (equal '("a " "b ")
               (tui-core:render-lines container 2)))
    (is (eq first (tui-core:remove-child container first)))
    (is (equal (list second) (tui-core:children container)))
    (is (eq container (tui-core:clear-children container)))
    (is (equal '() (tui-core:children container)))))

(test tui-box-renders-children-with-padding
  (let* ((protocol (make-tui-views-fixture))
         (box (tui-views:make-tui-box :protocol protocol :padding-x 1 :padding-y 1)))
    (tui-core:add-child box
                                 (tui-views:make-tui-text "x"
                                                          :protocol protocol
                                                          :padding-x 0
                                                          :padding-y 0))
    (is (equal (tui-core:render-lines box 5)
               '("     " " x   " "     ")))))

(test tui-views-default-input-handlers-decline-events
  (let ((protocol (make-tui-views-fixture))
        (view (tui-views:make-tui-text "x")))
    (is (null (tui-core:cursor-position view 80)))
    (is (null (tui-core:handle-input view "a")))
    (is (null (tui-core:handle-paste view "paste")))))

(defclass tui-roundtrip-probe-view (tui-views:tui-container-view) ())

(test tui-method-contribution-roundtrips-on-tui-class
  "Exercises the framework's :method contribution kind against the TUI class hierarchy. A dedicated probe subclass keeps refcount isolated, because other tests activate :tui-views (which contributes render-lines on tui-container-view) and nothing else specializes render-lines on the probe, so the clean 0->1->0 transitions are observable without the whole suite deactivating on exit."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (specializers (list (find-class 'tui-roundtrip-probe-view)
                             (find-class 't)))
         (contrib (ext:make-method-contribution
                   :gf-name 'tui-core:render-lines
                   :qualifiers '()
                   :specializer-names '(tui-roundtrip-probe-view t)
                   :lambda-list '(view width)
                   :body '((declare (ignore view width))
                           '("probe")))))
    (dotimes (cycle 3)
      (is (null (find-method (fdefinition 'tui-core:render-lines)
                             '() specializers nil))
          "no method before install in cycle ~A" cycle)
      (ext:install-contribution protocol contrib context)
      (is (find-method (fdefinition 'tui-core:render-lines)
                       '() specializers nil)
          "method present after install in cycle ~A" cycle)
      (is (equal '("probe")
                 (tui-core:render-lines
                  (make-instance 'tui-roundtrip-probe-view) 8))
          "probe instance dispatches to contributed body in cycle ~A" cycle)
      (ext:retract-contribution protocol contrib context)
      (is (null (find-method (fdefinition 'tui-core:render-lines)
                             '() specializers nil))
          "method absent after retract in cycle ~A" cycle))))

(defclass tui-coexist-probe-view (tui-views:tui-container-view) ())

(test tui-method-contribution-survives-overlapping-protocols
  (let* ((context (kli:make-kernel-host))
         (proto1 (switch-to-extension-protocol context))
         (proto2 (ext:make-extension-protocol :id :tui-coexist-protocol))
         (specializers (list (find-class 'tui-coexist-probe-view)
                             (find-class 't)))
         (body '((declare (ignore view width))
                 '("shared")))
         (c1 (ext:make-method-contribution
              :gf-name 'tui-core:render-lines
              :qualifiers '()
              :specializer-names '(tui-coexist-probe-view t)
              :lambda-list '(view width)
              :body body))
         (c2 (ext:make-method-contribution
              :gf-name 'tui-core:render-lines
              :qualifiers '()
              :specializer-names '(tui-coexist-probe-view t)
              :lambda-list '(view width)
              :body body)))
    (is (null (find-method (fdefinition 'tui-core:render-lines)
                           '() specializers nil)))
    (ext:install-contribution proto1 c1 context)
    (is (find-method (fdefinition 'tui-core:render-lines)
                     '() specializers nil)
        "method present after first install (refcount 0->1)")
    (ext:install-contribution proto2 c2 context)
    (is (find-method (fdefinition 'tui-core:render-lines)
                     '() specializers nil)
        "method present after second install (refcount 1->2)")
    (is (equal '("shared")
               (tui-core:render-lines
                (make-instance 'tui-coexist-probe-view) 8))
        "dispatch resolves while both protocols hold refs")
    (ext:retract-contribution proto1 c1 context)
    (is (find-method (fdefinition 'tui-core:render-lines)
                     '() specializers nil)
        "method survives first retract (refcount 2->1)")
    (is (equal '("shared")
               (tui-core:render-lines
                (make-instance 'tui-coexist-probe-view) 8))
        "dispatch still resolves after first retract")
    (ext:retract-contribution proto2 c2 context)
    (is (null (find-method (fdefinition 'tui-core:render-lines)
                           '() specializers nil))
        "method removed after final retract (refcount 1->0)")))
