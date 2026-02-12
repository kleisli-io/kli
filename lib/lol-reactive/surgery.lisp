;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Component Surgery - X-ray inspection and live modification of pandoric closures
;;;;
;;;; This is what makes LOL-REACTIVE unique: the ability to reach into any
;;;; component's closure, inspect its state, modify values, and even hotswap
;;;; behavior - all while the component is running.

(in-package :lol-reactive)

;;; ============================================================================
;;; SNAPSHOT SYSTEM
;;;
;;; Capture and restore component state at any point in time.
;;; Each snapshot is a frozen copy of all pandoric variables.
;;; ============================================================================

(defparameter *snapshots* (make-hash-table :test 'equal)
  "Component ID -> list of (timestamp . state-alist) snapshots.")

(defparameter *max-snapshots-per-component* 20
  "Maximum number of snapshots to keep per component.")

(defun capture-snapshot (component &optional description)
  "Capture current component state as a restorable snapshot.
   Returns the snapshot ID (timestamp)."
  (let* ((component-id (funcall component :id))
         (state (funcall component :inspect))
         (timestamp (get-universal-time))
         (snapshot (list :timestamp timestamp
                         :description (or description "Manual snapshot")
                         :state (getf state :state))))
    ;; Add to front of list
    (push snapshot (gethash component-id *snapshots*))
    ;; Trim if over limit
    (when (> (length (gethash component-id *snapshots*))
             *max-snapshots-per-component*)
      (setf (gethash component-id *snapshots*)
            (subseq (gethash component-id *snapshots*)
                    0 *max-snapshots-per-component*)))
    timestamp))

(defun list-snapshots (component)
  "List all snapshots for a component."
  (let ((component-id (if (stringp component)
                          component
                          (funcall component :id))))
    (gethash component-id *snapshots*)))

(defun find-snapshot (component timestamp)
  "Find a specific snapshot by timestamp."
  (let ((snapshots (list-snapshots component)))
    (find timestamp snapshots :key (lambda (s) (getf s :timestamp)))))

(defun restore-snapshot (component timestamp)
  "Restore component to a previous snapshot state.
   Uses with-pandoric to surgically update each state variable."
  (let* ((snapshot (find-snapshot component timestamp))
         (state-alist (getf snapshot :state)))
    (when snapshot
      ;; Iterate through saved state and restore each value
      (dolist (pair state-alist)
        (let ((key (car pair))
              (value (cdr pair)))
          (funcall component :set-state key value)))
      t)))

(defun clear-snapshots (component)
  "Clear all snapshots for a component."
  (let ((component-id (if (stringp component)
                          component
                          (funcall component :id))))
    (remhash component-id *snapshots*)))

;;; ============================================================================
;;; STATE TREE EXTRACTION
;;;
;;; Convert pandoric closure state to JSON-serializable format for the UI.
;;; ============================================================================

(defun component-state-tree (component)
  "Extract complete state tree from a component for JSON serialization."
  (let ((inspection (funcall component :inspect)))
    `((:id . ,(getf inspection :id))
      (:mounted . ,(getf inspection :mounted))
      (:subscribers . ,(getf inspection :subscribers))
      (:state . ,(mapcar (lambda (pair)
                           `((:key . ,(car pair))
                             (:value . ,(format-value-for-json (cdr pair)))
                             (:type . ,(type-of-value (cdr pair)))))
                         (getf inspection :state))))))

(defun format-value-for-json (value)
  "Format a Lisp value for JSON representation."
  (typecase value
    (null :null)
    ((eql t) t)
    (number value)
    (string value)
    (keyword (format nil "~a" value))
    (symbol (format nil "~a" value))
    (list (mapcar #'format-value-for-json value))
    (hash-table (hash-table-to-alist value))
    (t (format nil "~s" value))))

(defun type-of-value (value)
  "Return a string describing the type for UI display."
  (typecase value
    (null "nil")
    ((eql t) "boolean")
    (integer "integer")
    (float "float")
    (string "string")
    (keyword "keyword")
    (symbol "symbol")
    (cons "list")
    (hash-table "hash-table")
    (function "function")
    (t (format nil "~a" (type-of value)))))

(defun hash-table-to-alist (ht)
  "Convert hash table to alist for JSON."
  (let ((result '()))
    (maphash (lambda (k v)
               (push (cons (format nil "~a" k)
                           (format-value-for-json v))
                     result))
             ht)
    (nreverse result)))

;;; ============================================================================
;;; SURGERY OPERATIONS
;;;
;;; The core "x-ray" functionality - reaching into closures and modifying them.
;;; ============================================================================

(defun surgery-get-state (component-id key)
  "Get a specific state value from a component."
  (let ((component (find-component component-id)))
    (when component
      (funcall component :state key))))

(defun surgery-set-state (component-id key value)
  "Set a specific state value in a component.
   This is the 'magic' - directly modifying closure state."
  (let ((component (find-component component-id)))
    (when component
      ;; Capture before state for potential undo
      (capture-snapshot component "Before surgery")
      (funcall component :set-state key value)
      ;; Return updated state tree
      (component-state-tree component))))

(defun surgery-eval-in-context (component-id form-string)
  "Evaluate a Lisp form in the context of a component's state.
   This is the REPL integration - execute arbitrary code with access to state."
  (let ((component (find-component component-id)))
    (when component
      (let ((inspection (funcall component :inspect)))
        ;; Build a let binding with current state values
        (let ((bindings (mapcar (lambda (pair)
                                  (list (car pair) (cdr pair)))
                                (getf inspection :state))))
          (handler-case
              (let* ((form (read-from-string form-string))
                     ;; Create a lambda that has access to state setters
                     (result (eval `(let ,bindings
                                      (macrolet ((set-state (key val)
                                                   `(funcall (find-component ,',component-id)
                                                             :set-state ,key ,val)))
                                        ,form)))))
                `((:success . t)
                  (:result . ,(format nil "~s" result))
                  (:state . ,(component-state-tree component))))
            (error (e)
              `((:success . nil)
                (:error . ,(format nil "~a" e))))))))))

(defun surgery-dispatch (component-id action &rest args)
  "Dispatch an action to a component and return updated state."
  (let ((component (find-component component-id)))
    (when component
      (apply component :dispatch action args)
      (component-state-tree component))))

;;; ============================================================================
;;; BEHAVIOR HOTSWAP
;;;
;;; Replace a component's behavior while it's running.
;;; The ultimate Let Over Lambda trick.
;;; ============================================================================

(defparameter *behavior-presets* (make-hash-table :test 'equal)
  "Named behavior presets for hotswapping.")

(defun register-behavior-preset (name component-type dispatch-fn)
  "Register a behavior preset that can be hotswapped into components."
  (setf (gethash (cons component-type name) *behavior-presets*)
        dispatch-fn))

(defun list-behavior-presets (component-type)
  "List available behavior presets for a component type."
  (let ((presets '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (eq (car key) component-type)
                 (push (cdr key) presets)))
             *behavior-presets*)
    presets))

;; Note: Actual hotswap requires pandoric-hotpatch which modifies 'this'
;; in the closure. We'd need to extend defcomponent to support this.

;;; ============================================================================
;;; SURGERY X-RAY COMPONENT
;;;
;;; A meta-component that wraps other components and provides x-ray UI.
;;; ============================================================================

(defun xray-wrapper-html (component)
  "Generate wrapper HTML that adds x-ray functionality to a component."
  (let ((id (funcall component :id))
        (inner-html (funcall component :render)))
    (cl-who:with-html-output-to-string (s)
      (:div :class "xray-wrapper group relative"
            :data-component-id id
            :data-xray-enabled "true"
        ;; X-ray toggle button (appears on hover)
        (:button :class "xray-toggle absolute top-2 right-2 opacity-0 group-hover:opacity-100 transition-opacity bg-brutal-accent text-brutal-bg px-2 py-1 text-xs font-bold border-2 border-brutal-bg shadow-brutal-sm z-50"
                 :onclick (parenscript:ps* `(funcall toggle-xray ,id))
          "X-RAY")
        ;; Component content
        (:div :class "xray-content"
          (cl-who:str inner-html))))))

(defun surgery-panel-html (component)
  "Generate the surgery panel HTML for a component."
  (let* ((id (funcall component :id))
         (state-tree (component-state-tree component))
         (snapshots (list-snapshots id)))
    (cl-who:with-html-output-to-string (s)
      (:div :class "surgery-panel fixed right-0 top-0 h-full w-96 bg-brutal-surface border-l-4 border-brutal-accent p-4 overflow-y-auto z-50 transform translate-x-full transition-transform"
            :id (format nil "surgery-panel-~a" id)
            :data-for-component id
        ;; Header
        (:div :class "flex justify-between items-center mb-4 pb-4 border-b-2 border-brutal-muted"
          (:h3 :class "text-brutal-primary font-bold"
            "(SURGERY)")
          (:button :class "text-brutal-accent hover:text-brutal-error"
                   :onclick (parenscript:ps* `(funcall close-xray ,id))
            "CLOSE"))

        ;; Component ID
        (:div :class "mb-4 text-brutal-muted text-sm"
          (:span :class "text-brutal-secondary" "ID: ")
          (cl-who:str id))

        ;; State Inspector
        (:div :class "mb-6"
          (:h4 :class "text-brutal-secondary font-bold mb-2" "STATE")
          (:div :class "bg-brutal-bg p-3 border-2 border-brutal-primary font-mono text-sm"
            (dolist (var (cdr (assoc :state state-tree)))
              (let ((key (cdr (assoc :key var)))
                    (value (cdr (assoc :value var)))
                    (vtype (cdr (assoc :type var))))
                (cl-who:htm
                 (:div :class "flex justify-between items-center py-1 border-b border-brutal-surface"
                   (:span :class "text-brutal-accent" (cl-who:str key))
                   (:span :class "text-brutal-text cursor-pointer hover:bg-brutal-surface px-2"
                          :onclick (parenscript:ps* `(funcall edit-state ,id ,key))
                          :data-state-key key
                     (cl-who:str (format nil "~a" value)))
                   (:span :class "text-brutal-muted text-xs" (cl-who:str vtype))))))))

        ;; REPL Console
        (:div :class "mb-6"
          (:h4 :class "text-brutal-secondary font-bold mb-2" "REPL")
          (:div :class "bg-brutal-bg border-2 border-brutal-primary"
            (:div :class "p-2 max-h-32 overflow-y-auto font-mono text-xs"
                  :id (format nil "repl-output-~a" id))
            (:div :class "border-t border-brutal-primary flex"
              (:span :class "text-brutal-primary p-2" ">")
              (:input :type "text"
                      :class "flex-1 bg-brutal-bg text-brutal-text px-2 py-2 font-mono text-sm focus:outline-none"
                      :id (format nil "repl-input-~a" id)
                      :placeholder "(setf name \"New\")"
                      :onkeypress (parenscript:ps* `(when (string= (ps:@ event key) "Enter")
                                                      (funcall eval-in-context ,id)))))))

        ;; Snapshots
        (:div :class "mb-6"
          (:h4 :class "text-brutal-secondary font-bold mb-2"
            "SNAPSHOTS "
            (:span :class "text-brutal-muted text-sm"
              (cl-who:fmt "(~a)" (length snapshots))))
          (:button :class "brutal-btn bg-brutal-primary text-brutal-bg px-4 py-2 text-xs mb-2 w-full"
                   :onclick (parenscript:ps* `(funcall capture-snapshot ,id))
            "CAPTURE")
          (:div :class "space-y-2 max-h-40 overflow-y-auto"
            (dolist (snap (subseq snapshots 0 (min 5 (length snapshots))))
              (cl-who:htm
               (:div :class "flex justify-between items-center bg-brutal-bg p-2 text-xs border border-brutal-muted"
                 (:span :class "text-brutal-muted"
                   (cl-who:str (format-timestamp (getf snap :timestamp))))
                 (:button :class "text-brutal-accent hover:text-brutal-primary"
                          :onclick (let ((timestamp (getf snap :timestamp)))
                                     (parenscript:ps* `(funcall restore-snapshot ,id ,timestamp)))
                   "RESTORE"))))))

        ;; Actions
        (:div
          (:h4 :class "text-brutal-secondary font-bold mb-2" "ACTIONS")
          (:div :class "grid grid-cols-2 gap-2"
            (:button :class "brutal-btn bg-brutal-surface-el text-brutal-text px-2 py-2 text-xs border-2 border-brutal-muted"
                     :onclick (parenscript:ps* `(funcall dispatch ,id ":render"))
              "RE-RENDER")
            (:button :class "brutal-btn bg-brutal-surface-el text-brutal-text px-2 py-2 text-xs border-2 border-brutal-muted"
                     :onclick (parenscript:ps* `(funcall inspect-component ,id))
              "INSPECT")))))))

(defun format-timestamp (universal-time)
  "Format a universal time for display."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore year month day))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

;;; ============================================================================
;;; SURGERY MODE
;;;
;;; Global toggle for surgery/x-ray mode.
;;; ============================================================================

(defparameter *surgery-mode* nil
  "When true, all components render with x-ray wrappers.")

(defun enable-surgery-mode ()
  "Enable global surgery mode."
  (setf *surgery-mode* t))

(defun disable-surgery-mode ()
  "Disable global surgery mode."
  (setf *surgery-mode* nil))

(defun surgery-mode-p ()
  "Check if surgery mode is enabled."
  *surgery-mode*)

;;; ============================================================================
;;; COMPONENT METADATA
;;;
;;; Track component types for behavior presets and documentation.
;;; ============================================================================

(defparameter *component-metadata* (make-hash-table :test 'equal)
  "Component ID -> metadata about the component.")

(defun register-component-metadata (id metadata)
  "Store metadata about a component for surgery UI."
  (setf (gethash id *component-metadata*) metadata))

(defun get-component-metadata (id)
  "Get metadata for a component."
  (gethash id *component-metadata*))

;;; ============================================================================
;;; UNDO/REDO SYSTEM
;;;
;;; Track changes for undo functionality.
;;; ============================================================================

(defparameter *undo-stacks* (make-hash-table :test 'equal)
  "Component ID -> list of previous states for undo.")

(defparameter *redo-stacks* (make-hash-table :test 'equal)
  "Component ID -> list of undone states for redo.")

(defun push-undo (component)
  "Push current state onto undo stack before a change."
  (let* ((id (funcall component :id))
         (state (funcall component :inspect)))
    (push state (gethash id *undo-stacks*))
    ;; Clear redo stack on new action
    (setf (gethash id *redo-stacks*) nil)))

(defun can-undo-p (component-id)
  "Check if undo is available."
  (not (null (gethash component-id *undo-stacks*))))

(defun can-redo-p (component-id)
  "Check if redo is available."
  (not (null (gethash component-id *redo-stacks*))))

(defun surgery-undo (component-id)
  "Undo the last change to a component."
  (let ((component (find-component component-id)))
    (when (and component (can-undo-p component-id))
      ;; Push current state to redo
      (push (funcall component :inspect)
            (gethash component-id *redo-stacks*))
      ;; Pop and restore from undo
      (let ((prev-state (pop (gethash component-id *undo-stacks*))))
        (dolist (pair (getf prev-state :state))
          (funcall component :set-state (car pair) (cdr pair))))
      (component-state-tree component))))

(defun surgery-redo (component-id)
  "Redo a previously undone change."
  (let ((component (find-component component-id)))
    (when (and component (can-redo-p component-id))
      ;; Push current state to undo
      (push (funcall component :inspect)
            (gethash component-id *undo-stacks*))
      ;; Pop and restore from redo
      (let ((next-state (pop (gethash component-id *redo-stacks*))))
        (dolist (pair (getf next-state :state))
          (funcall component :set-state (car pair) (cdr pair))))
      (component-state-tree component))))
