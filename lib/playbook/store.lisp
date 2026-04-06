;;; Playbook MCP Server - Pattern Store
;;; Thread-safe in-memory pattern storage

(in-package #:playbook)

;;; Global store

(defvar *pattern-store* (make-hash-table :test 'equal)
  "Global pattern store, keyed by pattern ID.")

(defvar *pattern-store-lock* (make-lock "pattern-store")
  "Lock for thread-safe pattern store access.")

;;; Derived caches — invalidated on mutation via invalidate-pattern-caches

(defvar *patterns-list-cache* nil
  "Cached list of all patterns. Invalidated on store mutation.")

(defvar *domain-index* (make-hash-table :test 'equalp)
  "Domain → list-of-patterns index. Case-insensitive via equalp. Invalidated on store mutation.")

(defun invalidate-pattern-caches ()
  "Clear derived caches. Must be called under *pattern-store-lock*."
  (setf *patterns-list-cache* nil)
  (clrhash *domain-index*))

;;; Store operations

(defun store-pattern (pattern)
  "Store a pattern in the global store. Returns the pattern ID."
  (let ((id (pattern-id pattern)))
    (with-lock-held (*pattern-store-lock*)
      (setf (gethash id *pattern-store*) pattern)
      (invalidate-pattern-caches))
    id))

(defun get-pattern (id)
  "Retrieve a pattern by ID. Returns NIL if not found."
  (with-lock-held (*pattern-store-lock*)
    (gethash id *pattern-store*)))

(defun remove-pattern (id)
  "Remove a pattern from the store. Returns T if removed, NIL if not found."
  (with-lock-held (*pattern-store-lock*)
    (prog1 (remhash id *pattern-store*)
      (invalidate-pattern-caches))))

(defun list-patterns ()
  "Return a cached list of all patterns in the store."
  (with-lock-held (*pattern-store-lock*)
    (or *patterns-list-cache*
        (setf *patterns-list-cache*
              (hash-table-values *pattern-store*)))))

(defun clear-patterns ()
  "Remove all patterns from the store."
  (with-lock-held (*pattern-store-lock*)
    (clrhash *pattern-store*)
    (invalidate-pattern-caches)))

(defun pattern-count ()
  "Return the number of patterns in the store."
  (with-lock-held (*pattern-store-lock*)
    (hash-table-count *pattern-store*)))

;;; Bulk operations

(defun ensure-domain-index ()
  "Build domain index if empty. Must be called under *pattern-store-lock*."
  (when (zerop (hash-table-count *domain-index*))
    (maphash (lambda (id pattern)
               (declare (ignore id))
               (let ((domain (or (pattern-domain pattern)
                                 (cl-ppcre:regex-replace "-[0-9]+$" (pattern-id pattern) ""))))
                 (push pattern (gethash domain *domain-index*))))
             *pattern-store*)))

(defun patterns-by-domain (domain)
  "Return all patterns for a given domain (case-insensitive).
   Uses domain→pattern index for O(1) lookup."
  (with-lock-held (*pattern-store-lock*)
    (ensure-domain-index)
    (gethash domain *domain-index*)))

(defun update-pattern (id update-fn)
  "Update a pattern in place using UPDATE-FN.
   UPDATE-FN receives the pattern and should modify it.
   Returns the updated pattern or NIL if not found."
  (with-lock-held (*pattern-store-lock*)
    (let ((pattern (gethash id *pattern-store*)))
      (when pattern
        (funcall update-fn pattern)
        (invalidate-pattern-caches)
        pattern))))
