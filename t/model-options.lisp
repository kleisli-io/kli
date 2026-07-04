(in-package #:kli/tests)

(test model-options-global-definitions
  (let ((reasoning (models:find-model-option-definition "reasoning-effort")))
    (is (not (null reasoning)))
    (is (eq :enum (models:model-option-definition-type reasoning)))
    (is (equal '(:off :minimal :low :medium :high :xhigh)
               (models:model-option-definition-enum-values reasoning))))
  (is (equal '(:low :medium :high)
             (models:model-option-definition-enum-values
              (models:find-model-option-definition :text-verbosity))))
  (is (equal '(:auto :default :flex :priority)
             (models:model-option-definition-enum-values
              (models:find-model-option-definition "service-tier"))))
  (is (equal '(:off :in-memory :24h)
             (models:model-option-definition-enum-values
              (models:find-model-option-definition "prompt-cache-retention")))))

(test model-options-schema-validation-and-canonicalization
  (let ((reasoning (models:make-model-option-schema
                    "reasoning-effort"
                    :values '("off" "low" "high")
                    :default "low")))
    (is (equal "reasoning-effort"
               (models:model-option-schema-option-id reasoning)))
    (is (equal '(:off :low :high)
               (models:model-option-schema-values reasoning)))
    (is (eq :high (models:canonicalize-model-option-value reasoning "high")))
    (signals error
      (models:canonicalize-model-option-value reasoning "xhigh")))
  (signals error
    (models:make-model-option-schema "reasoning-effort"
                                     :values '(:off :turbo)))
  (signals error
    (models:make-model-option-schema "reasoning-effort"
                                     :values '(:off :low)
                                     :default :high)))

(test model-options-scalar-validation
  (let* ((bool (make-instance 'models:model-option-schema
                              :option-id "local-bool"
                              :type :boolean))
         (int (make-instance 'models:model-option-schema
                             :option-id "local-int"
                             :type :integer
                             :min 1
                             :max 3))
         (num (make-instance 'models:model-option-schema
                             :option-id "local-num"
                             :type :number
                             :min 1/2
                             :max 2))
         (str (make-instance 'models:model-option-schema
                             :option-id "local-str"
                             :type :string)))
    (is (eq t (models:canonicalize-model-option-value bool "true")))
    (is (eq nil (models:canonicalize-model-option-value bool "false")))
    (signals error
      (models:canonicalize-model-option-value bool "yes"))
    (is (= 2 (models:canonicalize-model-option-value int "2")))
    (signals error
      (models:canonicalize-model-option-value int 4))
    (is (= 3/2 (models:canonicalize-model-option-value num "3/2")))
    (signals error
      (models:canonicalize-model-option-value num "5/2"))
    (is (string= "hello" (models:canonicalize-model-option-value str "hello")))
    (is (string= "hello" (models:canonicalize-model-option-value str :hello)))))

(test model-options-materialize-defaults-and-assignments
  (let* ((model (models:make-model-definition
                 "p" "m" :fake
                 :option-schemas
                 (list (models:make-model-option-schema
                        "reasoning-effort"
                        :values '(:off :low :high)
                        :default :off)
                       (models:make-model-option-schema
                        "text-verbosity"
                        :values '(:low :medium :high)
                        :default :medium))))
         (defaults (models:materialize-model-options model))
         (assigned (models:materialize-model-options
                    model '(:reasoning-effort "high"))))
    (is (equal '(:reasoning-effort :off :text-verbosity :medium) defaults))
    (is (equal '(:reasoning-effort :high :text-verbosity :medium) assigned))
    (signals error
      (models:materialize-model-options model '(:service-tier :auto)))
    (signals error
      (models:materialize-model-options model '(:reasoning-effort :xhigh)))))

(test model-options-unknown-option-id-rejected
  (signals error
    (models:make-model-option-schema "not-a-real-option"
                                     :type :string)))

(test model-options-preserve-valid-switching
  (let* ((model (models:make-model-definition
                 "p" "m" :fake
                 :option-schemas
                 (list (models:make-model-option-schema
                        "reasoning-effort"
                        :values '(:off :low)
                        :default :off)
                       (models:make-model-option-schema
                        "text-verbosity"
                        :values '(:low :medium :high)
                        :default :medium))))
         (preserved (models:preserve-valid-model-options
                     model
                     '(:reasoning-effort :high
                       :text-verbosity :high
                       :service-tier :priority))))
    (is (equal '(:reasoning-effort :off :text-verbosity :high)
               preserved))))

(test model-definition-inspection-includes-option-schemas
  (let* ((model (models:make-model-definition
                 "p" "m" :fake
                 :option-schemas
                 (list (models:make-model-option-schema
                        "reasoning-effort"
                        :values '(:off :low)
                        :default :off))))
         (inspection (models:inspect-model-definition model))
         (schemas (getf inspection :option-schemas)))
    (is (= 1 (length schemas)))
    (is (equal "reasoning-effort" (getf (first schemas) :option-id)))
    (is (equal '(:off :low) (getf (first schemas) :values)))
    (is (eq :off (getf (first schemas) :default)))))
