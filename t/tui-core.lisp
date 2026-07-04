(in-package #:kli/tests)

(test (tui-core-recode-behavior-falls-back-to-cell-primitive :fixture interactive-authority)
  (let ((cell (tui-core:make-behavior-cell
               :id :recode-fallback-cell
               :function (lambda (x) (list :v0 x)))))
    (tui-core:recode-tui-behavior cell
                                  :function (lambda (x)
                                              (list :v1 x)))
    (is (equal (list :v1 :hi) (tui-core:call-behavior cell :hi)))))

(test (tui-core-behavior-cell-recodes-without-losing-identity :fixture interactive-authority)
  (let ((cell (tui-core:make-behavior-cell
               :id :test-behavior
               :state '(:count 1)
               :metadata '(:owner :test)
               :capabilities '(:behavior/hotpatch)
               :function (lambda (value)
                           (list :old value)))))
    (is (equal '(:old 3) (tui-core:call-behavior cell 3)))
    (is (= 0 (tui-core:behavior-version cell)))
    (is (tui-core:behavior-pandoric-p cell))
    (is (equal '(:count 1) (tui-core:behavior-state cell)))
    (is (equal '(:owner :test) (tui-core:behavior-metadata cell)))
    (is (equal '(:behavior/hotpatch) (tui-core:behavior-capabilities cell)))
    (is (equal '(:count 1)
               (tui-core:behavior-pandoric-value cell :state)))
    (is (eq cell
            (tui-core:recode-behavior
             cell
             :function (lambda (value)
                         (list :new value)))))
    (is (= 1 (tui-core:behavior-version cell)))
    (is (eq :test-behavior (kli:object-id cell)))
    (is (equal '(:new 3) (tui-core:call-behavior cell 3)))
    (is (eq cell
            (tui-core:recode-behavior
             cell
             :state '(:count 2)
             :metadata '(:owner :runtime)
             :capabilities '(:behavior/hotpatch :behavior/state))))
    (is (= 2 (tui-core:behavior-version cell)))
    (is (equal '(:count 2) (tui-core:behavior-state cell)))
    (is (equal '(:count 2)
               (tui-core:behavior-pandoric-value cell :state)))
    (is (equal '(:owner :runtime) (tui-core:behavior-metadata cell)))
    (is (equal '(:behavior/hotpatch :behavior/state)
               (tui-core:behavior-capabilities cell)))))

(test call-behavior-contains-a-faulting-cell-and-logs
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let ((cell (tui-core:make-behavior-cell
                   :id '(:probe :cell)
                   :function (lambda (x) (declare (ignore x)) (error "cell boom")))))
        (is (null (tui-core:call-behavior cell 1)))
        (let ((lines (fault-log-lines :behavior)))
          (is (= 1 (length lines)))
          (is (search "(:PROBE :CELL)" (first lines))))))))

(test call-behavior-honors-cell-fault-fallback-and-reify
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil)
          (reified '()))
      (let ((ext:*fault-reify-hook*
              (lambda (condition seam id)
                (declare (ignore condition))
                (push (list seam id) reified)))
            (cell (tui-core:make-behavior-cell
                   :id '(:probe :step)
                   :fault-policy :reify
                   :fault-fallback :error-handled
                   :function (lambda () (error "step boom")))))
        (is (eq :error-handled (tui-core:call-behavior cell)))
        (is (equal '((:behavior (:probe :step))) reified))))))

(test call-behavior-escalate-cell-propagates-but-logs
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let ((cell (tui-core:make-behavior-cell
                   :id '(:probe :renderer)
                   :fault-policy :escalate
                   :function (lambda () (error "renderer boom")))))
        (is (eq :propagated
                (handler-case (tui-core:call-behavior cell)
                  (error () :propagated))))
        (is (= 1 (length (fault-log-lines :behavior)))
            "an escalated cell fault still logs one line at the cell sink")))))
