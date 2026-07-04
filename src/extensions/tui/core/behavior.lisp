(in-package #:kli/tui/core)

(defun call-behavior (cell &rest arguments)
  "Invoke CELL's function behind the extension fault barrier: every behavior
cell is a hot-patch seam, so a recoded function must not unwind past the
dispatch point. The cell's fault-policy and fault-fallback choose the
disposition."
  (with-extension-fault-barrier
      (:seam :behavior
       :id (object-id cell)
       :policy (behavior-fault-policy cell)
       :on-fault (behavior-fault-fallback cell))
    (apply (behavior-function cell) arguments)))

(defun behavior-pandoric-name (name)
  (case name
    (:state 'state)
    (:this 'this)
    (otherwise name)))

(defun behavior-pandoric-value (cell name)
  (unless (behavior-pandoric-p cell)
    (error "Behavior cell is not pandoric: ~S" cell))
  (get-pandoric (behavior-function cell)
                (behavior-pandoric-name name)))

(defun (setf behavior-pandoric-value) (value cell name)
  (unless (behavior-pandoric-p cell)
    (error "Behavior cell is not pandoric: ~S" cell))
  (let ((name (behavior-pandoric-name name)))
    (setf (get-pandoric (behavior-function cell) name) value)
    (when (eq name 'state)
      (setf (slot-value cell 'state) value)))
  value)

(defmethod (setf behavior-state) :after (value (cell behavior-cell))
  (when (behavior-pandoric-p cell)
    (setf (get-pandoric (behavior-function cell) 'state) value)))

(defun recode-behavior (cell &key function version (state nil state-p)
                               (metadata nil metadata-p)
                               (capabilities nil capabilities-p))
  (check-type cell behavior-cell)
  (kli/ext:require-capability :behavior/hotpatch)
  (when state-p (kli/ext:require-capability :behavior/state))
  (when function
    (if (behavior-pandoric-p cell)
        (pandoric-hotpatch (behavior-function cell) function)
        (setf (behavior-function cell) function)))
  (when state-p
    (setf (behavior-state cell) state))
  (when metadata-p
    (setf (behavior-metadata cell) metadata))
  (when capabilities-p
    (setf (behavior-capabilities cell) capabilities))
  (setf (behavior-version cell)
        (or version (1+ (behavior-version cell))))
  cell)
