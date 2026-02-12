;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; DOM Diffing
;;;; Compute minimal patches to transform one HTML tree to another

(in-package :lol-reactive)

;;; ============================================================================
;;; HTML PARSING (Simple S-expression based)
;;; ============================================================================

;; Note: For simplicity, we work with cl-who style s-expressions directly
;; rather than parsing HTML strings. This keeps the diffing simple and fast.

(defun html-sexp-tag (sexp)
  "Get the tag name from an HTML s-expression."
  (when (consp sexp)
    (car sexp)))

(defun html-sexp-attrs (sexp)
  "Get attributes plist from an HTML s-expression."
  (when (consp sexp)
    (loop for (key val) on (cdr sexp) by #'cddr
          while (keywordp key)
          collect key
          collect val)))

(defun html-sexp-children (sexp)
  "Get children from an HTML s-expression."
  (when (consp sexp)
    (let ((rest (cdr sexp)))
      ;; Skip attribute pairs
      (loop while (and rest (keywordp (car rest)))
            do (setf rest (cddr rest)))
      rest)))

;;; ============================================================================
;;; DIFF-HTML-SEXP - Diff Two S-expression HTML Trees
;;; ============================================================================

(defun diff-html-sexp (old new &optional (path nil))
  "Compute patches to transform OLD s-expression to NEW.

   PATH: Current path in the tree (list of indices)

   Returns list of patches:
     (:replace path new-sexp)
     (:update-text path new-text)
     (:update-attr path attr-name new-value)
     (:remove-attr path attr-name)
     (:insert-child path position child-sexp)
     (:remove-child path position)
     (:update-child path position patches)"
  (cond
    ;; Both null - no change
    ((and (null old) (null new))
     nil)

    ;; Old null, new exists - replace
    ((null old)
     (list `(:replace ,path ,new)))

    ;; New null, old exists - remove (replace with nil)
    ((null new)
     (list `(:remove ,path)))

    ;; Both strings - check if equal
    ((and (stringp old) (stringp new))
     (if (string= old new)
         nil
         (list `(:update-text ,path ,new))))

    ;; One string, one sexp - replace
    ((or (stringp old) (stringp new))
     (list `(:replace ,path ,new)))

    ;; Both are s-expressions
    ((and (consp old) (consp new))
     (let ((old-tag (html-sexp-tag old))
           (new-tag (html-sexp-tag new)))
       (if (not (eq old-tag new-tag))
           ;; Different tags - replace entire subtree
           (list `(:replace ,path ,new))
           ;; Same tag - diff attributes and children
           (append
            (diff-attrs (html-sexp-attrs old)
                        (html-sexp-attrs new)
                        path)
            (diff-children (html-sexp-children old)
                           (html-sexp-children new)
                           path)))))

    ;; Fallback - replace
    (t (list `(:replace ,path ,new)))))

(defun diff-attrs (old-attrs new-attrs path)
  "Diff attribute plists, return patches."
  (let ((patches nil)
        (old-keys (loop for (k v) on old-attrs by #'cddr collect k))
        (new-keys (loop for (k v) on new-attrs by #'cddr collect k)))
    ;; Check removed attributes
    (dolist (k old-keys)
      (unless (member k new-keys)
        (push `(:remove-attr ,path ,k) patches)))
    ;; Check added/changed attributes
    (loop for (k v) on new-attrs by #'cddr
          do (let ((old-val (getf old-attrs k)))
               (unless (equal old-val v)
                 (push `(:update-attr ,path ,k ,v) patches))))
    (nreverse patches)))

(defun diff-children (old-children new-children path)
  "Diff child lists, return patches."
  (let ((patches nil)
        (old-len (length old-children))
        (new-len (length new-children))
        (max-len (max (length old-children) (length new-children))))
    (dotimes (i max-len)
      (let ((old-child (nth i old-children))
            (new-child (nth i new-children))
            (child-path (append path (list i))))
        (cond
          ;; Old exists, new doesn't - remove
          ((and old-child (not new-child))
           (push `(:remove-child ,path ,i) patches))
          ;; New exists, old doesn't - insert
          ((and new-child (not old-child))
           (push `(:insert-child ,path ,i ,new-child) patches))
          ;; Both exist - recursively diff
          ((and old-child new-child)
           (let ((child-patches (diff-html-sexp old-child new-child child-path)))
             (when child-patches
               (push `(:update-child ,path ,i ,child-patches) patches)))))))
    (nreverse patches)))

;;; ============================================================================
;;; APPLY-PATCHES-JS - Generate Parenscript for Patches
;;; ============================================================================

(defun generate-patch-js (patches container-selector)
  "Generate JavaScript to apply patches to a DOM container.

   PATCHES: List of patch operations
   CONTAINER-SELECTOR: CSS selector for the container element

   Returns a JavaScript string that can be executed on the client."
  (ps:ps*
    `(progn
       (let ((container ((ps:@ document query-selector) ,container-selector)))
         (when container
           ,@(mapcar #'patch-to-ps patches))))))

(defun patch-to-ps (patch)
  "Convert a single patch to Parenscript form."
  (destructuring-bind (op path &rest args) patch
    (let ((path-selector (path-to-selector path)))
      (ecase op
        (:replace
         `(let ((el (get-element-at-path container ,path-selector)))
            (when el
              (setf (ps:@ el outer-h-t-m-l) ,(sexp-to-html (car args))))))

        (:remove
         `(let ((el (get-element-at-path container ,path-selector)))
            (when el ((ps:@ el remove)))))

        (:update-text
         `(let ((el (get-element-at-path container ,path-selector)))
            (when el
              (setf (ps:@ el text-content) ,(car args)))))

        (:update-attr
         (destructuring-bind (attr value) args
           `(let ((el (get-element-at-path container ,path-selector)))
              (when el
                ((ps:@ el set-attribute) ,(string-downcase (symbol-name attr)) ,value)))))

        (:remove-attr
         `(let ((el (get-element-at-path container ,path-selector)))
            (when el
              ((ps:@ el remove-attribute) ,(string-downcase (symbol-name (car args)))))))

        (:insert-child
         (destructuring-bind (pos child) args
           `(let ((el (get-element-at-path container ,path-selector)))
              (when el
                (let ((new-el (create-element-from-html ,(sexp-to-html child)))
                      (ref-el (ps:@ el children ,pos)))
                  (if ref-el
                      ((ps:@ el insert-before) new-el ref-el)
                      ((ps:@ el append-child) new-el)))))))

        (:remove-child
         `(let ((el (get-element-at-path container ,path-selector))
                (child (ps:@ el children ,(car args))))
            (when child ((ps:@ child remove)))))

        (:update-child
         ;; Recursively apply child patches
         `(progn ,@(mapcar #'patch-to-ps (car args))))))))

(defun path-to-selector (path)
  "Convert a path list to a format usable in JS navigation."
  (or path '()))

(defun sexp-to-html (sexp)
  "Convert cl-who style s-expression to HTML string.
   Simple implementation for patch generation."
  (cond
    ((null sexp) "")
    ((stringp sexp) sexp)
    ((consp sexp)
     (let ((tag (car sexp))
           (rest (cdr sexp))
           (attrs "")
           (children ""))
       ;; Collect attributes
       (loop while (and rest (keywordp (car rest)))
             do (setf attrs (format nil "~A ~A=\"~A\""
                                    attrs
                                    (string-downcase (symbol-name (car rest)))
                                    (cadr rest)))
                (setf rest (cddr rest)))
       ;; Collect children
       (setf children (format nil "~{~A~}" (mapcar #'sexp-to-html rest)))
       ;; Generate HTML
       (format nil "<~A~A>~A</~A>"
               (string-downcase (symbol-name tag))
               attrs
               children
               (string-downcase (symbol-name tag)))))
    (t (princ-to-string sexp))))

;;; ============================================================================
;;; DOM DIFF RUNTIME JS
;;; ============================================================================

(defun dom-diff-runtime-js ()
  "Generate client-side JavaScript runtime for DOM diffing."
  (ps:ps
    ;; Helper to navigate to element at path
    (defun get-element-at-path (root path)
      (let ((el root))
        (dolist (idx path)
          (when el
            (setf el (aref (ps:@ el children) idx))))
        el))

    ;; Helper to create element from HTML string
    (defun create-element-from-html (html)
      (let ((template ((ps:@ document create-element) "template")))
        (setf (ps:@ template inner-h-t-m-l) html)
        (ps:@ template content first-element-child)))

    ;; Main patch application function
    (defun apply-lol-patches (container-selector patches)
      (let ((container ((ps:@ document query-selector) container-selector)))
        (when container
          (dolist (patch patches)
            (apply-single-patch container patch)))))

    ;; Apply a single patch operation
    (defun apply-single-patch (container patch)
      (let ((op (ps:@ patch op))
            (path (ps:@ patch path))
            (el (get-element-at-path container path)))
        (when el
          (case op
            ("replace"
             (setf (ps:@ el outer-h-t-m-l) (ps:@ patch html)))
            ("remove"
             ((ps:@ el remove)))
            ("updateText"
             (setf (ps:@ el text-content) (ps:@ patch text)))
            ("updateAttr"
             ((ps:@ el set-attribute) (ps:@ patch attr) (ps:@ patch value)))
            ("removeAttr"
             ((ps:@ el remove-attribute) (ps:@ patch attr)))
            ("insertChild"
             (let ((new-el (create-element-from-html (ps:@ patch html)))
                   (ref-el (aref (ps:@ el children) (ps:@ patch pos))))
               (if ref-el
                   ((ps:@ el insert-before) new-el ref-el)
                   ((ps:@ el append-child) new-el))))
            ("removeChild"
             (let ((child (aref (ps:@ el children) (ps:@ patch pos))))
               (when child ((ps:@ child remove)))))))))))

;;; ============================================================================
;;; WITH-DIFFING - Wrap Rendering in Diffing Context
;;; ============================================================================

(defvar *rendered-html* (make-hash-table :test 'equal)
  "Cache of rendered HTML for diffing.")

(defmacro! with-diffing ((container-id) &body body)
  "Wrap rendering in diffing context.
   Instead of full innerHTML replacement, computes and returns minimal patches.

   CONTAINER-ID: Unique identifier for the container

   Returns (values html patches) where patches can be applied incrementally."
  `(let* ((,g!new-html (progn ,@body))
          (,g!old-html (gethash ,container-id *rendered-html*))
          (,g!patches nil))
     ;; If we have old HTML, compute patches (basic string compare for now)
     (when (and ,g!old-html (not (string= ,g!old-html ,g!new-html)))
       ;; For string-based HTML, we just mark as changed
       ;; Full diffing would require parsing HTML to s-exp
       (setf ,g!patches (list `(:replace nil ,,g!new-html))))
     ;; Update cache
     (setf (gethash ,container-id *rendered-html*) ,g!new-html)
     ;; Return both HTML and patches
     (values ,g!new-html ,g!patches)))

(defun clear-html-cache (&optional container-id)
  "Clear the rendered HTML cache."
  (if container-id
      (remhash container-id *rendered-html*)
      (clrhash *rendered-html*)))
