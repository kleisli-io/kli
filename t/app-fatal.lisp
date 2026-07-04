(in-package #:kli/tests)

(defun tree-find (target tree)
  (cond
    ((equal target tree) t)
    ((atom tree) nil)
    (t (or (tree-find target (car tree))
           (tree-find target (cdr tree))))))

(defun tree-contains-setf-or-setq-of-p (var tree)
  (cond
    ((atom tree) nil)
    ((and (consp tree)
          (member (car tree) '(setf setq cl:setf cl:setq))
          (eq (second tree) var))
     t)
    (t (or (tree-contains-setf-or-setq-of-p var (car tree))
           (tree-contains-setf-or-setq-of-p var (cdr tree))))))

(test with-fatal-error-handler-binds-via-let-and-wraps-in-handler-case
  (let ((expansion (macroexpand-1
                    '(app:with-fatal-error-handler () :body))))
    (is (eq 'let* (first expansion))
        "outer form must establish dynamic bindings via let*")
    (is (tree-find '*debugger-hook* expansion)
        "expansion must bind *debugger-hook*")
    (is (tree-find 'handler-case expansion)
        "expansion must wrap body in handler-case")
    (is (not (tree-contains-setf-or-setq-of-p '*debugger-hook* expansion))
        "*debugger-hook* must be let-bound, never setf'd or setq'd")))
