;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; advanced/wizards.lisp - Continuation-Based Multi-Step Wizards
;;;;
;;;; PURPOSE:
;;;;   Write multi-step UI flows as linear code using session-based state.
;;;;   Each step suspends execution until user submits, enabling natural
;;;;   sequential code for complex workflows.
;;;;
;;;; KEY MACROS:
;;;;   DEFWIZARD - Define a multi-step wizard
;;;;   WIZARD-STEP - Single step that suspends until user input
;;;;
;;;; PATTERNS USED:
;;;;   - dlambda for wizard state machine (message-passing)
;;;;   - defmacro! for gensym hygiene
;;;;   - Session-based state storage (Hunchentoot)
;;;;   - aif for conditional handling

(in-package :lol-reactive)

;;; ============================================================================
;;; LOCAL UTILITIES
;;; ============================================================================

(defmacro awhen (test &body body)
  "Anaphoric when: if TEST is non-nil, bind IT to the result and execute BODY."
  `(let ((let-over-lambda:it ,test))
     (when let-over-lambda:it
       ,@body)))

;;; ============================================================================
;;; WIZARD REGISTRY
;;; ============================================================================

(defvar *wizards* (make-hash-table :test 'eq)
  "Registry of defined wizard specifications.")

(defvar *wizard-sessions* (make-hash-table :test 'equal)
  "Active wizard sessions: session-id -> wizard-state")

(defun register-wizard (name spec)
  "Register a wizard specification."
  (setf (gethash name *wizards*) spec))

(defun get-wizard-spec (name)
  "Retrieve a registered wizard specification."
  (gethash name *wizards*))

(defun list-wizards ()
  "List all registered wizards."
  (let (wizards)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k wizards))
             *wizards*)
    (nreverse wizards)))

;;; ============================================================================
;;; WIZARD SESSION STATE
;;; ============================================================================

(defun generate-wizard-session-id ()
  "Generate a unique session ID for a wizard instance."
  (format nil "wizard-~A-~A"
          (get-universal-time)
          (random 1000000)))

(defun make-wizard-state (wizard-name steps)
  "Create initial wizard state structure using dlambda pattern.
   Returns a closure that responds to messages."
  (let ((session-id (generate-wizard-session-id))
        (current-step 0)
        (step-data (make-hash-table :test 'equal))
        (completed nil)
        (cancelled nil)
        (step-history nil))
    (dlambda
      (:id () session-id)
      (:wizard-name () wizard-name)
      (:current-step () current-step)
      (:total-steps () (length steps))
      (:step-name () (aif (nth current-step steps)
                          (getf let-over-lambda:it :name)
                          nil))
      (:step-title () (aif (nth current-step steps)
                           (getf let-over-lambda:it :title)
                           nil))
      (:completed-p () completed)
      (:cancelled-p () cancelled)

      ;; Get data for a specific step
      (:get-step-data (step-name)
       (gethash step-name step-data))

      ;; Set data for current step
      (:set-step-data (data)
       (awhen (nth current-step steps)
         (setf (gethash (getf let-over-lambda:it :name) step-data) data)
         (push (cons current-step data) step-history)))

      ;; Get all collected data
      (:all-data ()
       (let (result)
         (maphash (lambda (k v) (push (cons k v) result)) step-data)
         (nreverse result)))

      ;; Navigation
      (:can-go-back-p () (> current-step 0))
      (:can-go-forward-p () (< current-step (1- (length steps))))

      (:next-step ()
       (when (< current-step (1- (length steps)))
         (incf current-step)
         current-step))

      (:prev-step ()
       (when (> current-step 0)
         (decf current-step)
         current-step))

      (:goto-step (n)
       (when (and (>= n 0) (< n (length steps)))
         (setf current-step n)
         current-step))

      ;; Completion
      (:complete ()
       (setf completed t))

      (:cancel ()
       (setf cancelled t))

      ;; Introspection
      (:inspect ()
       (list :session-id session-id
             :wizard-name wizard-name
             :current-step current-step
             :total-steps (length steps)
             :step-name (aif (nth current-step steps)
                             (getf let-over-lambda:it :name)
                             nil)
             :completed completed
             :cancelled cancelled
             :data-keys (let (keys)
                          (maphash (lambda (k v) (declare (ignore v)) (push k keys))
                                   step-data)
                          keys))))))

(defun get-wizard-session (session-id)
  "Get wizard state by session ID."
  (gethash session-id *wizard-sessions*))

(defun store-wizard-session (wizard-state)
  "Store wizard state in session registry."
  (setf (gethash (funcall wizard-state :id) *wizard-sessions*)
        wizard-state))

(defun remove-wizard-session (session-id)
  "Remove a wizard session."
  (remhash session-id *wizard-sessions*))

(defun cleanup-stale-sessions (&optional (max-age-seconds 3600))
  "Remove wizard sessions older than MAX-AGE-SECONDS (default 1 hour)."
  (let ((cutoff (- (get-universal-time) max-age-seconds))
        (to-remove nil))
    (maphash (lambda (id state)
               (declare (ignore state))
               ;; Session ID format: wizard-TIMESTAMP-RANDOM
               (let* ((parts (cl-ppcre:split "-" id))
                      (timestamp (ignore-errors (parse-integer (second parts)))))
                 (when (and timestamp (< timestamp cutoff))
                   (push id to-remove))))
             *wizard-sessions*)
    (dolist (id to-remove)
      (remhash id *wizard-sessions*))
    (length to-remove)))

;;; ============================================================================
;;; STEP RENDERING
;;; ============================================================================

(defun render-wizard-step (wizard-state &key (show-progress t) (extra-classes ""))
  "Render the current wizard step as HTML.
   Returns HTML for the step form with navigation buttons."
  (let* ((spec (get-wizard-spec (funcall wizard-state :wizard-name)))
         (steps (getf spec :steps))
         (current-step (funcall wizard-state :current-step))
         (step-spec (nth current-step steps))
         (step-name (getf step-spec :name))
         (step-title (getf step-spec :title))
         (step-form-fn (getf step-spec :form))
         (session-id (funcall wizard-state :id))
         (can-back (funcall wizard-state :can-go-back-p))
         (is-last (not (funcall wizard-state :can-go-forward-p)))
         (prev-data (funcall wizard-state :get-step-data step-name)))

    (htm-str
      (:div :class (classes "wizard" extra-classes)
        ;; Progress indicator
        (when show-progress
          (cl-who:htm
            (:div :class (classes "wizard-progress" "mb-6")
              (:div :class (classes "flex" "justify-between" "mb-2")
                (dotimes (i (length steps))
                  (let* ((step-info (nth i steps))
                         (is-current (= i current-step))
                         (is-done (< i current-step)))
                    (cl-who:htm
                      (:div :class (classes "wizard-step-indicator" "text-center" "flex-1"
                                            (when is-current "text-primary" "font-bold")
                                            (when is-done "text-success"))
                        (cl-who:esc (getf step-info :title (format nil "Step ~A" (1+ i))))))))))))

        ;; Step title
        (when step-title
          (cl-who:htm
            (:h2 :class (classes "text-xl" "font-bold" "mb-4")
              (cl-who:esc step-title))))

        ;; Hidden session ID
        (:input :type "hidden" :name "wizard-session-id"
                :value session-id)

        ;; Step content (from form function)
        (cl-who:str (aif step-form-fn
                         (funcall let-over-lambda:it prev-data)
                         ""))

        ;; Navigation buttons
        (:div :class (classes "wizard-nav" "flex" "justify-between" "mt-6" "pt-4" "border-t" "border-muted")
          ;; Back button
          (if can-back
              (cl-who:htm
                (:button :type "submit" :name "wizard-action" :value "back"
                         :class (classes "px-4" "py-2" "border" "border-muted" "rounded-md"
                                         "hover:bg-surface-alt")
                  "Back"))
              (cl-who:htm
                (:span)))  ; Spacer

          ;; Next/Submit button
          (:button :type "submit" :name "wizard-action"
                   :value (if is-last "complete" "next")
                   :class (classes "px-4" "py-2" "bg-primary" "text-surface" "rounded-md"
                                   "hover:brightness-90")
            (cl-who:esc (if is-last "Complete" "Next"))))))))

(defun render-wizard-complete (wizard-state result)
  "Render wizard completion page."
  (let ((wizard-name (funcall wizard-state :wizard-name)))
    (htm-str
      (:div :class (classes "wizard-complete" "text-center" "p-6")
        (:h2 :class (classes "text-xl" "font-bold" "mb-4" "text-success")
          "Complete!")
        (:p :class (classes "text-muted" "mb-4")
          "Your submission has been processed.")
        (when result
          (cl-who:htm
            (:div :class (classes "wizard-result" "p-4" "bg-surface-alt" "rounded-md")
              (cl-who:esc (princ-to-string result)))))))))

;;; ============================================================================
;;; WIZARD PROCESSING
;;; ============================================================================

(defun process-wizard-submission (wizard-name session-id action form-data)
  "Process a wizard form submission.
   ACTION: :next, :back, or :complete
   FORM-DATA: Plist of form field values
   Returns: (values :continue wizard-state) or (values :complete result)"
  (let* ((wizard-state (aif session-id
                            (get-wizard-session let-over-lambda:it)
                            nil)))

    ;; Start new wizard if no session
    (unless wizard-state
      (let ((spec (get-wizard-spec wizard-name)))
        (unless spec
          (error "Wizard ~A not found" wizard-name))
        (setf wizard-state (make-wizard-state wizard-name (getf spec :steps)))
        (store-wizard-session wizard-state)))

    ;; Save current step data (excluding wizard control fields)
    (let ((clean-data (loop for (k v) on form-data by #'cddr
                            unless (member k '(:wizard-session-id :wizard-action))
                              collect k and collect v)))
      (funcall wizard-state :set-step-data clean-data))

    ;; Process action
    (case action
      (:back
       (funcall wizard-state :prev-step)
       (values :continue wizard-state))

      (:next
       (let* ((spec (get-wizard-spec wizard-name))
              (steps (getf spec :steps))
              (current-step (funcall wizard-state :current-step))
              (step-spec (nth current-step steps))
              (validator (getf step-spec :validate)))

         ;; Validate if validator exists
         (when validator
           (let ((errors (funcall validator (funcall wizard-state :get-step-data
                                                     (getf step-spec :name)))))
             (when errors
               (return-from process-wizard-submission
                 (values :error errors)))))

         ;; Advance to next step
         (funcall wizard-state :next-step)
         (values :continue wizard-state)))

      (:complete
       (let* ((spec (get-wizard-spec wizard-name))
              (on-complete (getf spec :on-complete))
              (all-data (funcall wizard-state :all-data)))

         ;; Validate final step if needed
         (let* ((steps (getf spec :steps))
                (current-step (funcall wizard-state :current-step))
                (step-spec (nth current-step steps))
                (validator (getf step-spec :validate)))
           (when validator
             (let ((errors (funcall validator (funcall wizard-state :get-step-data
                                                       (getf step-spec :name)))))
               (when errors
                 (return-from process-wizard-submission
                   (values :error errors))))))

         ;; Mark complete and run completion handler
         (funcall wizard-state :complete)
         (let ((result (when on-complete
                         (funcall on-complete all-data))))
           ;; Clean up session
           (remove-wizard-session (funcall wizard-state :id))
           (values :complete result))))

      (t
       (values :continue wizard-state)))))

(defun start-wizard (wizard-name)
  "Start a new wizard session.
   Returns wizard state (a dlambda closure)."
  (let* ((spec (get-wizard-spec wizard-name)))
    (unless spec
      (error "Wizard ~A not found. Did you call DEFWIZARD?" wizard-name))
    (let ((wizard-state (make-wizard-state wizard-name (getf spec :steps))))
      (store-wizard-session wizard-state)
      wizard-state)))

;;; ============================================================================
;;; DEFWIZARD MACRO
;;; ============================================================================

(defmacro defwizard (name () &key steps on-complete)
  "Define a multi-step wizard.

   NAME: Wizard identifier
   STEPS: List of step specifications:
     (:name STEP-NAME :title \"Step Title\"
      :form (lambda (prev-data) ...) ; Returns HTML string
      :validate (lambda (data) ...)) ; Returns NIL or error list
   ON-COMPLETE: Handler receiving all collected data as alist

   Creates:
   - (start-wizard 'NAME) - Begin wizard, returns state
   - (render-wizard-step state) - Render current step
   - (process-wizard-submission 'NAME session-id action data) - Process form

   Example:
     (defwizard checkout ()
       :steps ((:name :address :title \"Shipping Address\"
                :form (lambda (data) (address-form-html data))
                :validate #'validate-address)
               (:name :payment :title \"Payment Method\"
                :form (lambda (data) (payment-form-html data))
                :validate #'validate-payment)
               (:name :confirm :title \"Confirm Order\"
                :form (lambda (data) (confirmation-html data))))
       :on-complete (lambda (data) (process-order data)))"

  ;; Validate step structure at compile time
  (dolist (step steps)
    (unless (getf step :name)
      (error "Each wizard step must have a :name"))
    (unless (getf step :form)
      (error "Each wizard step must have a :form function")))

  (let ((route-path (format nil "/wizard/~A" (string-downcase name))))
    `(progn
       ;; Register wizard specification
       (register-wizard ',name
                        (list :steps (list ,@(mapcar (lambda (step)
                                                       `(list :name ,(getf step :name)
                                                              :title ,(getf step :title)
                                                              :form ,(getf step :form)
                                                              :validate ,(getf step :validate)))
                                                     steps))
                              :on-complete ,on-complete))

       ;; Define route for wizard (optional - can also handle manually)
       (defroute ,route-path (:method :get)
         (let ((wizard-state (start-wizard ',name)))
           (htm-str
             (:form :method "POST" :action ,route-path
               (cl-who:str (render-wizard-step wizard-state))))))

       (defroute ,route-path (:method :post)
         (let* ((session-id (post-param "wizard-session-id"))
                (action (intern (string-upcase
                                  (or (post-param "wizard-action") "next"))
                                :keyword))
                (form-data (loop for (k . v) in (post-params)
                                 collect (intern (string-upcase k) :keyword)
                                 collect v)))
           (multiple-value-bind (status result)
               (process-wizard-submission ',name session-id action form-data)
             (case status
               (:continue
                (htm-str
                  (:form :method "POST" :action ,route-path
                    (cl-who:str (render-wizard-step result)))))
               (:complete
                (render-wizard-complete result result))
               (:error
                (htm-str
                  (:div :class "wizard-errors"
                    (:ul
                      (dolist (err result)
                        (cl-who:htm (:li (cl-who:esc err))))))))))))

       ',name)))

;;; ============================================================================
;;; WIZARD STEP HELPERS
;;; ============================================================================

(defun wizard-text-field (name &key label placeholder value required)
  "Generate a text input field for wizard forms."
  (let ((field-id (format nil "wizard-~A" (string-downcase name)))
        (field-name (string-downcase name)))
    (htm-str
      (:div :class (classes "mb-4")
        (when label
          (cl-who:htm
            (:label :for field-id :class (classes "block" "mb-2" "font-medium")
              (cl-who:esc label)
              (when required
                (cl-who:htm (:span :class "text-error" "*"))))))
        (:input :type "text"
                :id field-id
                :name field-name
                :class (classes "w-full" "p-2" "border" "border-muted" "rounded-md")
                :placeholder (or placeholder "")
                :value (or value "")
                :required (when required "required"))))))

(defun wizard-select-field (name options &key label value required)
  "Generate a select dropdown for wizard forms.
   OPTIONS: List of (value . label) pairs or just values"
  (let ((field-id (format nil "wizard-~A" (string-downcase name)))
        (field-name (string-downcase name)))
    (htm-str
      (:div :class (classes "mb-4")
        (when label
          (cl-who:htm
            (:label :for field-id :class (classes "block" "mb-2" "font-medium")
              (cl-who:esc label)
              (when required
                (cl-who:htm (:span :class "text-error" "*"))))))
        (:select :id field-id
                 :name field-name
                 :class (classes "w-full" "p-2" "border" "border-muted" "rounded-md")
                 :required (when required "required")
          (dolist (opt options)
            (let* ((opt-value (if (consp opt) (car opt) opt))
                   (opt-label (if (consp opt) (cdr opt) opt))
                   (selected (equal (princ-to-string opt-value)
                                    (princ-to-string value))))
              (cl-who:htm
                (:option :value (princ-to-string opt-value)
                         :selected (when selected "selected")
                  (cl-who:esc (princ-to-string opt-label)))))))))))

(defun wizard-radio-group (name options &key label value required)
  "Generate a radio button group for wizard forms."
  (let ((field-name (string-downcase name)))
    (htm-str
      (:div :class (classes "mb-4")
        (when label
          (cl-who:htm
            (:p :class (classes "mb-2" "font-medium")
              (cl-who:esc label)
              (when required
                (cl-who:htm (:span :class "text-error" "*"))))))
        (:div :class (classes "space-y-2")
          (dolist (opt options)
            (let* ((opt-value (if (consp opt) (car opt) opt))
                   (opt-label (if (consp opt) (cdr opt) opt))
                   (opt-id (format nil "wizard-~A-~A" field-name opt-value))
                   (checked (equal (princ-to-string opt-value)
                                   (princ-to-string value))))
              (cl-who:htm
                (:label :class (classes "flex" "items-center" "gap-2" "cursor-pointer")
                  (:input :type "radio"
                          :id opt-id
                          :name field-name
                          :value (cl-who:esc (princ-to-string opt-value))
                          :checked (when checked "checked")
                          :required (when required "required"))
                  (cl-who:esc (princ-to-string opt-label)))))))))))

;;; ============================================================================
;;; WIZARD INTROSPECTION
;;; ============================================================================

(defun inspect-wizard (name)
  "Return introspection data for a wizard."
  (let ((spec (get-wizard-spec name)))
    (when spec
      (list :name name
            :steps (mapcar (lambda (s)
                             (list :name (getf s :name)
                                   :title (getf s :title)
                                   :has-validator (not (null (getf s :validate)))))
                           (getf spec :steps))
            :has-completion-handler (not (null (getf spec :on-complete)))))))

(defun list-active-wizard-sessions ()
  "Return list of active wizard sessions."
  (let (sessions)
    (maphash (lambda (id state)
               (push (funcall state :inspect) sessions))
             *wizard-sessions*)
    (nreverse sessions)))
