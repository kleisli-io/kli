(in-package #:kli/model/registry)

(defvar *model-selection-counter* (make-id-counter))

(defun next-model-selection-id ()
  (next-keyword-id "MODEL-SELECTION" '*model-selection-counter*))

(defun normalize-provider-id (provider-id)
  (etypecase provider-id
    (string (string-downcase provider-id))
    (symbol (string-downcase (symbol-name provider-id)))))

(defun normalize-model-id (model-id)
  (etypecase model-id
    (string model-id)
    (symbol (string-downcase (symbol-name model-id)))))

(defun model-key (provider-id model-id)
  (list (normalize-provider-id provider-id)
        (normalize-model-id model-id)))

(defparameter +model-option-types+
  '(:enum :boolean :integer :number :string))

(defun normalize-option-id (option-id)
  (etypecase option-id
    (string (string-downcase option-id))
    (symbol (string-downcase (symbol-name option-id)))))

(defun %canonical-option-keyword (value)
  (etypecase value
    (keyword value)
    (symbol (intern (string-upcase (symbol-name value)) :keyword))
    (string (intern (string-upcase value) :keyword))))

(defclass model-option-definition ()
  ((option-id
    :initarg :option-id
    :reader model-option-definition-id)
   (type
    :initarg :type
    :reader model-option-definition-type)
   (label
    :initarg :label
    :initform nil
    :reader model-option-definition-label)
   (enum-values
    :initarg :enum-values
    :initform nil
    :reader model-option-definition-enum-values)))

(defclass model-option-schema ()
  ((option-id
    :initarg :option-id
    :reader model-option-schema-option-id)
   (type
    :initarg :type
    :reader model-option-schema-type)
   (values
    :initarg :values
    :initform nil
    :reader model-option-schema-values)
   (default
    :initarg :default
    :initform nil
    :reader model-option-schema-default)
   (default-supplied-p
    :initarg :default-supplied-p
    :initform nil
    :reader model-option-schema-default-supplied-p)
   (min
    :initarg :min
    :initform nil
    :reader model-option-schema-min)
   (max
    :initarg :max
    :initform nil
    :reader model-option-schema-max)))

(defun make-model-option-definition (option-id type &key label enum-values)
  (let ((type (or type :enum)))
    (unless (member type +model-option-types+)
      (error "Unknown model option type ~S for ~A." type option-id))
    (make-instance 'model-option-definition
                   :option-id (normalize-option-id option-id)
                   :type type
                   :label label
                   :enum-values (and enum-values
                                     (mapcar #'%canonical-option-keyword
                                             enum-values)))))

(defparameter *model-option-definitions*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (definition
              (list
	       (make-model-option-definition
	        "reasoning-effort" :enum
	        :label "Reasoning effort"
	        :enum-values '(:off :minimal :low :medium :high :xhigh))
	       (make-model-option-definition
	        "reasoning-summary" :enum
	        :label "Reasoning summary"
	        :enum-values '(:auto :concise :detailed :none))
	       (make-model-option-definition
	        "text-verbosity" :enum
                :label "Text verbosity"
                :enum-values '(:low :medium :high))
               (make-model-option-definition
                "service-tier" :enum
                :label "Service tier"
                :enum-values '(:auto :default :flex :priority))
               (make-model-option-definition
                "prompt-cache-retention" :enum
                :label "Prompt cache retention"
                :enum-values '(:off :in-memory :24h))
               (make-model-option-definition
                "transport" :enum
                :label "Transport"
                :enum-values '(:auto :sse :websocket :websocket-cached))))
      (setf (gethash (model-option-definition-id definition) table)
            definition))
    table))

(defun find-model-option-definition (option-id)
  (gethash (normalize-option-id option-id) *model-option-definitions*))

(defun registered-model-option-definitions ()
  (loop for definition being the hash-values of *model-option-definitions*
        collect definition))

(defun make-model-option-schema (option-id &key type values
                                             (default nil default-supplied-p)
                                             min max)
  (let* ((definition (find-model-option-definition option-id))
         (option-id (normalize-option-id option-id))
         (type (or type (and definition
                             (model-option-definition-type definition)))))
    (unless definition
      (error "Unknown model option id ~S." option-id))
    (unless (member type +model-option-types+)
      (error "Unknown model option type ~S for ~A." type option-id))
    (let ((schema (make-instance
                   'model-option-schema
                   :option-id option-id
                   :type type
                   :values (and values
                                (mapcar #'%canonical-option-keyword values))
                   :default (and default-supplied-p default)
                   :default-supplied-p default-supplied-p
                   :min min
                   :max max)))
      (validate-model-option-schema schema)
      schema)))

(defun model-option-schema-for (model option-id)
  (find (normalize-option-id option-id)
        (model-definition-option-schemas model)
        :key #'model-option-schema-option-id
        :test #'equal))

(defun model-supports-option-p (model option-id)
  (not (null (model-option-schema-for model option-id))))

(defun %value-in-enum-p (value values)
  (member value values :test #'eq))

(defun canonicalize-model-option-value (schema value)
  (ecase (model-option-schema-type schema)
    (:enum
     (let ((canonical (%canonical-option-keyword value)))
       (unless (%value-in-enum-p canonical (model-option-schema-values schema))
         (error "Invalid value ~S for model option ~A. Expected one of: ~{~A~^, ~}."
                value
                (model-option-schema-option-id schema)
                (model-option-schema-values schema)))
       canonical))
    (:boolean
     (cond ((typep value 'boolean) value)
           ((string-equal value "true") t)
           ((string-equal value "false") nil)
           (t (error "Invalid boolean value ~S for model option ~A."
                     value (model-option-schema-option-id schema)))))
    (:integer
     (let ((number (etypecase value
                     (integer value)
                     (string (parse-integer value)))))
       (unless (typep number 'integer)
         (error "Invalid integer value ~S for model option ~A."
                value (model-option-schema-option-id schema)))
       (when (and (model-option-schema-min schema)
                  (< number (model-option-schema-min schema)))
         (error "Value ~S is below minimum ~S for model option ~A."
                number (model-option-schema-min schema)
                (model-option-schema-option-id schema)))
       (when (and (model-option-schema-max schema)
                  (> number (model-option-schema-max schema)))
         (error "Value ~S is above maximum ~S for model option ~A."
                number (model-option-schema-max schema)
                (model-option-schema-option-id schema)))
       number))
    (:number
     (let ((number (etypecase value
                     (number value)
                     (string (let ((*read-eval* nil))
                               (read-from-string value))))))
       (unless (numberp number)
         (error "Invalid number value ~S for model option ~A."
                value (model-option-schema-option-id schema)))
       (when (and (model-option-schema-min schema)
                  (< number (model-option-schema-min schema)))
         (error "Value ~S is below minimum ~S for model option ~A."
                number (model-option-schema-min schema)
                (model-option-schema-option-id schema)))
       (when (and (model-option-schema-max schema)
                  (> number (model-option-schema-max schema)))
         (error "Value ~S is above maximum ~S for model option ~A."
                number (model-option-schema-max schema)
                (model-option-schema-option-id schema)))
       number))
    (:string
     (etypecase value
       (string value)
       (symbol (string-downcase (symbol-name value)))))))

(defun parse-model-option-value (schema string)
  (canonicalize-model-option-value schema string))

(defun validate-model-option-schema (schema)
  (let* ((option-id (model-option-schema-option-id schema))
         (definition (find-model-option-definition option-id))
         (type (model-option-schema-type schema)))
    (unless definition
      (error "Unknown model option id ~S." option-id))
    (unless (eq type (model-option-definition-type definition))
      (error "Model option ~A has schema type ~S, but global type is ~S."
             option-id type (model-option-definition-type definition)))
    (when (eq type :enum)
      (unless (model-option-schema-values schema)
        (error "Enum model option ~A must declare admitted values." option-id))
      (dolist (value (model-option-schema-values schema))
        (unless (%value-in-enum-p value
                                  (model-option-definition-enum-values definition))
          (error "Enum value ~S is not in the global universe for model option ~A."
                 value option-id))))
    (when (and (member type '(:integer :number))
               (model-option-schema-min schema)
               (model-option-schema-max schema)
               (> (model-option-schema-min schema)
                  (model-option-schema-max schema)))
      (error "Minimum is greater than maximum for model option ~A." option-id))
    (when (model-option-schema-default-supplied-p schema)
      (canonicalize-model-option-value schema
                                       (model-option-schema-default schema)))
    schema))

(defun %normalize-option-assignments (assignments)
  (loop for (option-id value) on assignments by #'cddr
        collect (cons (normalize-option-id option-id) value)))

(defun materialize-model-options (model &optional assignments)
  (let ((normalized (%normalize-option-assignments assignments))
        (result '()))
    (dolist (pair normalized)
      (unless (model-option-schema-for model (car pair))
        (error "Model ~A/~A does not support option ~A."
               (model-definition-provider-id model)
               (model-definition-model-id model)
               (car pair))))
    (dolist (schema (model-definition-option-schemas model))
      (let* ((option-id (model-option-schema-option-id schema))
             (assignment (assoc option-id normalized :test #'equal)))
        (cond (assignment
               (setf result
                     (append result
                             (list (intern (string-upcase option-id) :keyword)
                                   (canonicalize-model-option-value
                                    schema (cdr assignment))))))
              ((model-option-schema-default-supplied-p schema)
               (setf result
                     (append result
                             (list (intern (string-upcase option-id) :keyword)
                                   (canonicalize-model-option-value
                                    schema
                                    (model-option-schema-default schema)))))))))
    result))

(defun preserve-valid-model-options (model previous-options &optional assignments)
  (let ((preserved '()))
    (loop for (option-id value) on previous-options by #'cddr
          for schema = (model-option-schema-for model option-id)
          when schema
            do (handler-case
                   (setf preserved
                         (append preserved
                                 (list (intern (string-upcase
                                                (normalize-option-id option-id))
                                               :keyword)
                                       (canonicalize-model-option-value
                                        schema value))))
                 (error () nil)))
    (materialize-model-options model (append assignments preserved))))

(defclass model-registry (live-object)
  ((providers
    :initform (make-hash-table :test #'equal)
    :accessor registry-providers)
   (models
    :initform (make-hash-table :test #'equal)
    :accessor registry-models)
   (current-selection
    :initform nil
    :accessor registry-current-selection)))

(defclass provider-config ()
  ((base-url
    :initarg :base-url
    :initform nil
    :reader provider-config-base-url)
   (headers
    :initarg :headers
    :initform '()
    :reader provider-config-headers)))

(defclass model-provider (live-object)
  ((provider-id
    :initarg :provider-id
    :reader model-provider-provider-id)
   (api
    :initarg :api
    :reader model-provider-api)
   (auth-required-p
    :initarg :auth-required-p
    :initform t
    :reader model-provider-auth-required-p)
   (credential-provider-id
    :initarg :credential-provider-id
    :initform nil
    :reader model-provider-credential-provider-id)
   (config
    :initarg :config
    :initform nil
    :reader model-provider-config)
   (metadata
    :initarg :metadata
    :initform '()
    :reader model-provider-metadata)))

(defclass model-definition (live-object)
  ((provider-id
    :initarg :provider-id
    :reader model-definition-provider-id)
   (model-id
    :initarg :model-id
    :reader model-definition-model-id)
   (api
    :initarg :api
    :reader model-definition-api)
   (name
    :initarg :name
    :initform nil
    :reader model-definition-name)
   (context-window
    :initarg :context-window
    :initform nil
    :reader model-definition-context-window)
   (option-schemas
    :initarg :option-schemas
    :initform '()
    :reader model-definition-option-schemas)
   (metadata
    :initarg :metadata
    :initform '()
    :reader model-definition-metadata)))

(defclass model-selection (live-object)
  ((provider-id
    :initarg :provider-id
    :reader model-selection-provider-id)
   (model-id
    :initarg :model-id
    :reader model-selection-model-id)
   (options
    :initarg :options
    :initform '()
    :reader model-selection-options)
   (timestamp
    :initarg :timestamp
    :reader model-selection-timestamp)
   (metadata
    :initarg :metadata
    :initform '()
    :reader model-selection-metadata)))

(defun make-model-registry (&key (id :model-registry-service))
  (make-instance 'model-registry :id id))

(defun make-provider-config (&key base-url headers)
  (make-instance 'provider-config
                 :base-url base-url
                 :headers headers))

(defun make-model-provider (provider-id api &key id (auth-required-p t)
                                              credential-provider-id config
                                              metadata)
  (let ((provider-id (normalize-provider-id provider-id)))
    (make-instance 'model-provider
                   :id (or id (list :model-provider provider-id))
                   :provider-id provider-id
                   :api api
                   :auth-required-p auth-required-p
                   :credential-provider-id
                   (normalize-provider-id (or credential-provider-id
                                             provider-id))
                   :config config
                   :metadata metadata)))

(defun make-model-definition (provider-id model-id api &key id name
                                                     context-window
                                                     option-schemas
                                                     metadata)
  (let ((provider-id (normalize-provider-id provider-id))
        (model-id (normalize-model-id model-id))
        (option-schemas (mapcar #'validate-model-option-schema option-schemas)))
    (unless (= (length option-schemas)
               (length (remove-duplicates option-schemas
                                          :key #'model-option-schema-option-id
                                          :test #'equal)))
      (error "Duplicate option schema for model ~A/~A." provider-id model-id))
    (make-instance 'model-definition
                   :id (or id (list :model-definition provider-id model-id))
                   :provider-id provider-id
                   :model-id model-id
                   :api api
                   :name name
                   :context-window context-window
                   :option-schemas option-schemas
                   :metadata metadata)))

(defun model-option-keyword (option-id)
  (intern (string-upcase (normalize-option-id option-id)) :keyword))

(defun model-selection-option-value (selection option-id &optional default)
  (let* ((missing (list nil))
         (value (getf (model-selection-options selection)
                      (model-option-keyword option-id)
                      missing)))
    (if (eq value missing) default value)))

(defun make-model-selection (model &key id options metadata)
  (make-instance 'model-selection
                 :id (or id (next-model-selection-id))
                 :provider-id (model-definition-provider-id model)
                 :model-id (model-definition-model-id model)
                 :options (materialize-model-options model options)
                 :timestamp (get-universal-time)
                 :metadata metadata))
