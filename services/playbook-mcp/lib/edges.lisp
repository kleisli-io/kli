;;; Playbook MCP Server - Edge Storage & Weight Functions
;;; Graph edge data model, adjacency-list store, weight conversion, and JSON persistence

(in-package #:playbook-mcp)

;;; Edge data model

(defstruct edge
  "A directed edge in the pattern graph."
  (source "" :type string)
  (target "" :type string)
  (relation :requires :type keyword)
  (weight 1.0 :type single-float)
  (evidence "" :type string))

;;; Edge store (adjacency list)

(defvar *edge-store* (make-hash-table :test 'equal)
  "Adjacency list: node-id → list of outgoing edges.")

(defvar *edge-store-lock* (make-lock "edge-store")
  "Lock for thread-safe edge store access.")

(defun add-edge (edge)
  "Add an edge to the store. Rejects self-links. Deduplicates by source+target+relation.
   If a matching edge exists, updates its weight and evidence. Returns the edge."
  (when (string= (edge-source edge) (edge-target edge))
    (return-from add-edge edge))
  (with-lock-held (*edge-store-lock*)
    (let* ((source (edge-source edge))
           (existing (find-if (lambda (e)
                                (and (string= (edge-target e) (edge-target edge))
                                     (eq (edge-relation e) (edge-relation edge))))
                              (gethash source *edge-store*))))
      (if existing
          (progn
            (setf (edge-weight existing) (edge-weight edge))
            (setf (edge-evidence existing) (edge-evidence edge)))
          (push edge (gethash source *edge-store*)))))
  edge)

(defun outgoing-edges (node-id)
  "Return all outgoing edges for a node."
  (with-lock-held (*edge-store-lock*)
    (gethash node-id *edge-store*)))

(defun remove-edges-for (node-id)
  "Remove all outgoing edges for a node."
  (with-lock-held (*edge-store-lock*)
    (remhash node-id *edge-store*)))

(defun all-edges ()
  "Return a flat list of all edges in the store."
  (with-lock-held (*edge-store-lock*)
    (let (result)
      (maphash (lambda (k edges)
                 (declare (ignore k))
                 (dolist (e edges) (push e result)))
               *edge-store*)
      result)))

(defun clear-edges ()
  "Remove all edges from the store."
  (with-lock-held (*edge-store-lock*)
    (clrhash *edge-store*)))

(defun edge-count ()
  "Return the total number of edges in the store."
  (with-lock-held (*edge-store-lock*)
    (let ((count 0))
      (maphash (lambda (k edges)
                 (declare (ignore k))
                 (incf count (length edges)))
               *edge-store*)
      count)))

(defun node-count ()
  "Return the number of nodes (sources) with outgoing edges."
  (with-lock-held (*edge-store-lock*)
    (hash-table-count *edge-store*)))

;;; Relation type weights

(defparameter *relation-weights*
  '((:requires . 1.0)      ; Strong dependency
    (:supersedes . 0.9)    ; Replacement
    (:conflicts . -0.5)    ; Negative inhibition
    (:co-applied . 0.7)    ; Usage correlation
    (:similar . 0.5))      ; Embedding similarity
  "Weight multipliers for edge relation types during spreading activation.
   Negative weights inhibit activation (floored at 0 in spreading).")

(defun relation-weight (relation)
  "Get weight multiplier for a relation type. Defaults to 0.5 for unknown relations."
  (or (cdr (assoc relation *relation-weights*)) 0.5))

;;; Weight functions

(defun co-application-weight (count &key (min-count 2) (half-life 5.8) (max-weight 0.9))
  "Convert co-application count to edge weight using exponential saturation.
   Counts below MIN-COUNT return 0. Approaches MAX-WEIGHT asymptotically."
  (if (< count min-count)
      0.0
      (let ((effective (coerce (- count min-count) 'single-float)))
        (coerce (* max-weight (- 1.0 (exp (- (/ effective half-life))))) 'single-float))))

(defun embedding-proximity-weight (cosine &key (threshold 0.5) (max-weight 0.5))
  "Convert cosine similarity to edge weight. Linear above THRESHOLD, zero below.
   Returns at most MAX-WEIGHT."
  (if (<= cosine threshold)
      0.0
      (coerce (* max-weight (/ (- cosine threshold) (- 1.0 threshold))) 'single-float)))

;;; JSON serialization

(defun edge-to-json-alist (edge)
  "Convert an edge to an alist for JSON encoding."
  (list (cons "source" (edge-source edge))
        (cons "target" (edge-target edge))
        (cons "relation" (string-downcase (symbol-name (edge-relation edge))))
        (cons "weight" (edge-weight edge))
        (cons "evidence" (edge-evidence edge))))

(defun json-ht-to-edge (ht)
  "Convert a yason-decoded hash-table to an edge struct."
  (make-edge
   :source (gethash "source" ht "")
   :target (gethash "target" ht "")
   :relation (intern (string-upcase (gethash "relation" ht "requires")) :keyword)
   :weight (coerce (gethash "weight" ht 1.0) 'single-float)
   :evidence (gethash "evidence" ht "")))

;;; File persistence

(defun load-edges-file (path)
  "Load edges from a JSON file. Returns list of edge structs.
   Returns NIL if file does not exist."
  (when (probe-file path)
    (let ((parsed (yason:parse (alexandria:read-file-into-string path))))
      (mapcar #'json-ht-to-edge parsed))))

(defun save-edges-file (path edges)
  "Save edge structs to a JSON file using atomic write."
  (let* ((abs-path (namestring (merge-pathnames path)))
         (temp-path (format nil "~A.tmp" abs-path)))
    (with-open-file (s temp-path :direction :output :if-exists :supersede)
      (yason:encode
       (mapcar (lambda (e)
                 (let ((ht (make-hash-table :test 'equal)))
                   (loop for (k . v) in (edge-to-json-alist e)
                         do (setf (gethash k ht) v))
                   ht))
               edges)
       s))
    (rename-file temp-path abs-path)))
