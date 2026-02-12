;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; optimization/reactive-analysis.lisp - Compile-Time Reactive Dependency Analysis
;;;;
;;;; PURPOSE:
;;;;   Analyze reactive dependencies at macro-expansion time to generate optimal
;;;;   update paths. This enables:
;;;;   - Compile-time dependency graph construction
;;;;   - Topologically sorted update sequences
;;;;   - Dead code elimination for unused reactive bindings
;;;;   - Efficient signal wiring without runtime dependency tracking overhead
;;;;
;;;; KEY MACROS:
;;;;   REACTIVE-LET - Create local reactive scope with automatic dependency tracking
;;;;
;;;; DESIGN:
;;;;   Uses Let Over Lambda patterns throughout:
;;;;   - defmacro! for hygienic macro expansion
;;;;   - dlambda for state management in generated code
;;;;   - symb for symbol construction

(in-package :lol-reactive)

;;; ============================================================================
;;; COMPILE-TIME DEPENDENCY ANALYSIS
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-symbols-in-form (form)
    "Find all symbols referenced in a form (excluding special forms and keywords).
     Used at macro-expansion time to determine dependencies."
    (let ((symbols nil))
      (labels ((walk (x)
                 (cond
                   ((symbolp x)
                    (unless (or (keywordp x) (null x) (eq x t)
                                ;; Exclude common special forms
                                (member x '(quote function lambda if progn let let*
                                           setf setq block return-from tagbody go
                                           the locally declare)))
                      (pushnew x symbols)))
                   ((consp x)
                    ;; Don't walk into quote forms
                    (unless (eq (car x) 'quote)
                      (walk (car x))
                      (walk (cdr x)))))))
        (walk form))
      symbols))

  (defun analyze-dependencies (bindings)
    "Analyze reactive dependencies in a list of bindings at macro-expansion time.

     BINDINGS: List of (name value-form) pairs

     Returns a hash-table mapping each binding name to its dependencies
     (only dependencies that are other bindings in the same scope).

     Example:
       (analyze-dependencies '((count 0)
                               (doubled (* count 2))
                               (message (format nil \"~A\" doubled))))
       => Hash table: {count -> NIL, doubled -> (count), message -> (doubled)}"
    (let ((dep-graph (make-hash-table :test 'eq))
          (binding-names (mapcar #'car bindings)))
      (dolist (binding bindings)
        (let* ((name (car binding))
               (value-form (cadr binding))
               (refs (find-symbols-in-form value-form))
               ;; Only count references to other bindings as dependencies
               (deps (intersection refs binding-names)))
          (setf (gethash name dep-graph) deps)))
      dep-graph))

  (defun topological-sort (dep-graph)
    "Sort binding names so dependencies come before dependents.
     This gives us the correct order for updates: when a source signal changes,
     we update derived values in topological order.

     Returns a list of symbols in dependency order."
    (let ((visited (make-hash-table :test 'eq))
          (result nil))
      (labels ((visit (node)
                 (unless (gethash node visited)
                   ;; Visit dependencies first
                   (dolist (dep (gethash node dep-graph))
                     (visit dep))
                   (setf (gethash node visited) t)
                   (push node result))))
        (maphash (lambda (k v) (declare (ignore v)) (visit k)) dep-graph))
      (nreverse result)))

  (defun find-root-bindings (dep-graph)
    "Find bindings with no dependencies (root signals/values).
     These are the 'sources' in the reactive graph."
    (let ((roots nil))
      (maphash (lambda (name deps)
                 (when (null deps)
                   (push name roots)))
               dep-graph)
      roots))

  (defun find-dependent-bindings (name dep-graph)
    "Find all bindings that directly or transitively depend on NAME.
     Used to determine what needs updating when NAME changes."
    (let ((dependents nil))
      (maphash (lambda (other-name deps)
                 (when (member name deps)
                   (push other-name dependents)))
               dep-graph)
      dependents))

  (defun build-update-chains (dep-graph)
    "Build update chains: for each root binding, compute the sequence of
     derived values that need updating when the root changes.

     Returns an alist of (root-name . (dependent1 dependent2 ...))
     where dependents are in topological order."
    (let ((roots (find-root-bindings dep-graph))
          (update-order (topological-sort dep-graph))
          (chains nil))
      (dolist (root roots)
        ;; Find all bindings that (transitively) depend on this root
        (let ((chain nil))
          (labels ((collect-chain (name visited)
                     (unless (gethash name visited)
                       (setf (gethash name visited) t)
                       (dolist (dependent (find-dependent-bindings name dep-graph))
                         (push dependent chain)
                         (collect-chain dependent visited)))))
            (collect-chain root (make-hash-table :test 'eq)))
          ;; Sort chain in topological order
          (let ((sorted-chain (remove-if-not (lambda (x) (member x chain))
                                              update-order)))
            (push (cons root sorted-chain) chains))))
      chains)))

;;; ============================================================================
;;; REACTIVE-LET MACRO
;;; ============================================================================

(defmacro! reactive-let (bindings &body body)
  "Create a local reactive scope with compile-time dependency analysis.

   BINDINGS: List of (name value-form) pairs, similar to LET
   BODY: Code that can access the bindings and their derived values

   The macro analyzes which bindings depend on which at compile time,
   and generates optimal update code. When a 'root' binding (one with
   no dependencies) is changed via SET-<name>, all derived values are
   updated in topological order.

   Returns a dlambda that responds to:
     (:get name) - Get current value of binding
     (:set name value) - Set value (triggers cascade update if has dependents)
     (:deps) - Get dependency graph (for introspection)
     (:update-order) - Get topological update order
     (:render) - Execute body and return result
     (:inspect) - Full introspection

   Example:
     (reactive-let ((count 0)
                    (doubled (* count 2))
                    (message (format nil \"Count is ~A, doubled is ~A\" count doubled)))
       (format nil \"<div>~A</div>\" message))

   When count changes via (:set 'count 5):
   - doubled is recomputed to (* 5 2) = 10
   - message is recomputed to use new count and doubled values
   - All in correct dependency order

   Compile-time Analysis:
     The macro expands to code that:
     1. Creates a signal for each binding
     2. Generates update functions that recompute in topological order
     3. Wires setters to trigger the right cascade"
  (let* ((dep-graph (analyze-dependencies bindings))
         (update-order (topological-sort dep-graph))
         (update-chains (build-update-chains dep-graph))
         (binding-names (mapcar #'car bindings)))
    ;; Use let* so derived values can reference earlier bindings
    `(let* (;; Create storage for each binding in topological order
            ,@(mapcar (lambda (name)
                        (let ((binding (find name bindings :key #'car)))
                          `(,name ,(cadr binding))))
                      update-order))
       ;; Create the reactive controller using dlambda
       (dlambda
         ;; Get value
         (:get (,g!name)
          (ecase ,g!name
            ,@(mapcar (lambda (name) `((,name) ,name)) binding-names)))

         ;; Set value with cascade update
         (:set (,g!name ,g!value)
          (ecase ,g!name
            ,@(mapcar
               (lambda (name)
                 (let ((chain (cdr (assoc name update-chains))))
                   `((,name)
                     (setf ,name ,g!value)
                     ,@(mapcar (lambda (dep)
                                 (let ((binding (find dep bindings :key #'car)))
                                   `(setf ,dep ,(cadr binding))))
                               chain)
                     ,name)))
               binding-names)))

         ;; Get dependency graph (for introspection)
         (:deps ()
          (list ,@(mapcar (lambda (name)
                            `(cons ',name ',(gethash name dep-graph)))
                          binding-names)))

         ;; Get update order
         (:update-order () ',update-order)

         ;; Render body
         (:render () ,@body)

         ;; Full introspection
         (:inspect ()
          (list :bindings (list ,@(mapcar (lambda (name)
                                            `(cons ',name ,name))
                                          binding-names))
                :deps (list ,@(mapcar (lambda (name)
                                        `(cons ',name ',(gethash name dep-graph)))
                                      binding-names))
                :update-order ',update-order
                :update-chains ',update-chains))))))

;;; ============================================================================
;;; CONVENIENCE MACROS
;;; ============================================================================

(defmacro! with-reactive-bindings ((controller) &body body)
  "Execute body with all bindings from a reactive-let controller bound locally.

   Example:
     (let ((ctrl (reactive-let ((x 1) (y 2)) (+ x y))))
       (with-reactive-bindings (ctrl)
         (format t \"x=~A y=~A\" x y)))"
  (let ((bindings (funcall (eval controller) :inspect)))
    `(let ,(mapcar (lambda (b) `(,(car b) (funcall ,controller :get ',(car b))))
                   (getf bindings :bindings))
       ,@body)))

;;; ============================================================================
;;; ANALYSIS UTILITIES (Exported for debugging/introspection)
;;; ============================================================================

(defun visualize-dependencies (dep-graph)
  "Return a human-readable string representation of the dependency graph."
  (with-output-to-string (out)
    (format out "Dependency Graph:~%")
    (maphash (lambda (name deps)
               (if deps
                   (format out "  ~A <- ~{~A~^, ~}~%" name deps)
                   (format out "  ~A (root)~%" name)))
             dep-graph)))

(defun validate-no-cycles (dep-graph)
  "Check for circular dependencies. Returns NIL if valid, or signals error with cycle path."
  (let ((visiting (make-hash-table :test 'eq))
        (visited (make-hash-table :test 'eq)))
    (labels ((visit (node path)
               (cond
                 ((gethash node visiting)
                  (error "Circular dependency detected: ~{~A~^ -> ~} -> ~A"
                         (reverse path) node))
                 ((gethash node visited) t)
                 (t
                  (setf (gethash node visiting) t)
                  (dolist (dep (gethash node dep-graph))
                    (visit dep (cons node path)))
                  (setf (gethash node visiting) nil)
                  (setf (gethash node visited) t)))))
      (maphash (lambda (k v) (declare (ignore v)) (visit k nil)) dep-graph))
    nil))
