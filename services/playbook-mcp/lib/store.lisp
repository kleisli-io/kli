;;; Playbook MCP Server - Pattern Store
;;; Thread-safe in-memory pattern storage

(in-package #:playbook-mcp)

;;; Global store

(defvar *pattern-store* (make-hash-table :test 'equal)
  "Global pattern store, keyed by pattern ID.")

(defvar *pattern-store-lock* (make-lock "pattern-store")
  "Lock for thread-safe pattern store access.")

;;; Store operations

(defun store-pattern (pattern)
  "Store a pattern in the global store. Returns the pattern ID."
  (let ((id (pattern-id pattern)))
    (with-lock-held (*pattern-store-lock*)
      (setf (gethash id *pattern-store*) pattern))
    id))

(defun get-pattern (id)
  "Retrieve a pattern by ID. Returns NIL if not found."
  (with-lock-held (*pattern-store-lock*)
    (gethash id *pattern-store*)))

(defun remove-pattern (id)
  "Remove a pattern from the store. Returns T if removed, NIL if not found."
  (with-lock-held (*pattern-store-lock*)
    (remhash id *pattern-store*)))

(defun list-patterns ()
  "Return a list of all patterns in the store."
  (with-lock-held (*pattern-store-lock*)
    (hash-table-values *pattern-store*)))

(defun clear-patterns ()
  "Remove all patterns from the store."
  (with-lock-held (*pattern-store-lock*)
    (clrhash *pattern-store*)))

(defun pattern-count ()
  "Return the number of patterns in the store."
  (with-lock-held (*pattern-store-lock*)
    (hash-table-count *pattern-store*)))

;;; Bulk operations

(defun patterns-by-domain (domain)
  "Return all patterns for a given domain (case-insensitive).
   Falls back to extracting domain from ID if pattern-domain is nil/empty."
  (remove-if-not
   (lambda (p)
     (let ((pdomain (or (pattern-domain p)
                        ;; Defensive fallback: extract from ID if domain field is nil/empty
                        (cl-ppcre:regex-replace "-[0-9]+$" (pattern-id p) ""))))
       (string-equal pdomain domain)))  ; case-insensitive comparison
   (list-patterns)))

(defun update-pattern (id update-fn)
  "Update a pattern in place using UPDATE-FN.
   UPDATE-FN receives the pattern and should modify it.
   Returns the updated pattern or NIL if not found."
  (with-lock-held (*pattern-store-lock*)
    (let ((pattern (gethash id *pattern-store*)))
      (when pattern
        (funcall update-fn pattern)
        pattern))))
