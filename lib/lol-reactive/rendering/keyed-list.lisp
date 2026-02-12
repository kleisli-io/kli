;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Keyed List Rendering
;;;; Efficient list updates with key-based reconciliation

(in-package :lol-reactive)

;;; ============================================================================
;;; FOR-EACH - Keyed List Rendering Macro
;;; ============================================================================

(defmacro! for-each ((item-var collection &key (key '#'identity) (test '#'equal)) &body body)
  "Render a collection with keyed reconciliation.

   KEY: Function to extract unique key from each item (default: identity)
   TEST: Equality test for keys (default: equal)

   Returns list of (key . rendered-html) pairs for reconciliation.

   Example:
   (for-each (task tasks :key #'task-id)
     (htm-str (:li :data-key (task-id task)
               (task-title task))))"
  `(let* ((,g!items ,collection)
          (,g!key-fn ,key)
          (,g!results nil))
     (dolist (,item-var ,g!items)
       (let ((,g!item-key (funcall ,g!key-fn ,item-var)))
         (push (cons ,g!item-key (progn ,@body)) ,g!results)))
     (nreverse ,g!results)))

;;; ============================================================================
;;; RECONCILE-LIST - Compute Minimal Diff Operations
;;; ============================================================================

(defun reconcile-list (old-items new-items &key (key #'identity) (test #'equal))
  "Compute minimal diff between old and new item lists.

   OLD-ITEMS: List of items before update
   NEW-ITEMS: List of items after update
   KEY: Function to extract unique key from each item
   TEST: Equality test for keys

   Returns list of operations:
     (:insert position item)  - Insert item at position
     (:remove position)       - Remove item at position
     (:move from to)          - Move item from position to position
     (:update position item)  - Update item at position (same key, different content)

   Algorithm uses key-based tracking to minimize DOM operations."
  (let* ((old-keys (mapcar key old-items))
         (new-keys (mapcar key new-items))
         (old-map (make-hash-table :test test))
         (new-map (make-hash-table :test test))
         (ops nil))

    ;; Build position maps
    (loop for item in old-items
          for i from 0
          do (setf (gethash (funcall key item) old-map) (cons i item)))
    (loop for item in new-items
          for i from 0
          do (setf (gethash (funcall key item) new-map) (cons i item)))

    ;; Find removes (in old but not in new)
    (loop for k in old-keys
          for i from 0
          unless (gethash k new-map)
          do (push `(:remove ,i ,k) ops))

    ;; Find inserts and updates (in new)
    (loop for k in new-keys
          for new-pos from 0
          for new-item in new-items
          do (let ((old-entry (gethash k old-map)))
               (cond
                 ;; Not in old - insert
                 ((null old-entry)
                  (push `(:insert ,new-pos ,new-item) ops))
                 ;; In old - check if content changed (update)
                 ((not (funcall test (cdr old-entry) new-item))
                  (push `(:update ,new-pos ,new-item) ops)))))

    ;; Return operations in order (removes first, then inserts/updates)
    (let ((removes (remove-if-not (lambda (op) (eq (car op) :remove)) ops))
          (others (remove-if (lambda (op) (eq (car op) :remove)) ops)))
      (append (nreverse removes) (nreverse others)))))

;;; ============================================================================
;;; KEYED RENDER - Render with Key Tracking
;;; ============================================================================

(defvar *rendered-lists* (make-hash-table :test 'equal)
  "Cache of rendered keyed lists for diffing.")

(defun keyed-render (list-id items key-fn render-fn)
  "Render items with key tracking and return diff operations.

   LIST-ID: Unique identifier for this list
   ITEMS: List of items to render
   KEY-FN: Function to extract key from item
   RENDER-FN: Function to render item to HTML string

   Returns (values html-string operations) where operations
   can be applied incrementally on the client."
  (let* ((old-rendered (gethash list-id *rendered-lists*))
         (new-rendered (mapcar (lambda (item)
                                 (cons (funcall key-fn item)
                                       (funcall render-fn item)))
                               items))
         (ops (when old-rendered
                (reconcile-rendered-list old-rendered new-rendered))))
    ;; Update cache
    (setf (gethash list-id *rendered-lists*) new-rendered)
    ;; Return full HTML and operations
    (values (format nil "~{~A~}" (mapcar #'cdr new-rendered))
            ops)))

(defun reconcile-rendered-list (old-rendered new-rendered)
  "Reconcile two lists of (key . html) pairs.
   Returns operations for incremental DOM updates."
  (let ((old-keys (mapcar #'car old-rendered))
        (new-keys (mapcar #'car new-rendered))
        (old-map (make-hash-table :test 'equal))
        (new-map (make-hash-table :test 'equal))
        (ops nil))

    ;; Build maps: key -> (position . html)
    (loop for (k . html) in old-rendered
          for i from 0
          do (setf (gethash k old-map) (cons i html)))
    (loop for (k . html) in new-rendered
          for i from 0
          do (setf (gethash k new-map) (cons i html)))

    ;; Find removes
    (loop for k in old-keys
          for i from 0
          unless (gethash k new-map)
          do (push `(:remove ,i ,k) ops))

    ;; Find inserts and content updates
    (loop for k in new-keys
          for new-pos from 0
          for new-html in (mapcar #'cdr new-rendered)
          do (let ((old-entry (gethash k old-map)))
               (cond
                 ((null old-entry)
                  (push `(:insert ,new-pos ,k ,new-html) ops))
                 ((not (string= (cdr old-entry) new-html))
                  (push `(:update ,new-pos ,k ,new-html) ops)))))

    (nreverse ops)))

;;; ============================================================================
;;; CLEAR-LIST-CACHE
;;; ============================================================================

(defun clear-list-cache (&optional list-id)
  "Clear the rendered list cache.
   If LIST-ID provided, clear only that list."
  (if list-id
      (remhash list-id *rendered-lists*)
      (clrhash *rendered-lists*)))

;;; ============================================================================
;;; INSPECTION
;;; ============================================================================

(defun inspect-list-cache ()
  "Return info about cached rendered lists."
  (let ((lists nil))
    (maphash (lambda (id rendered)
               (push (list :id id
                           :count (length rendered)
                           :keys (mapcar #'car rendered))
                     lists))
             *rendered-lists*)
    lists))
