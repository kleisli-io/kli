;;;; The :settings contribution kind.
;;;;
;;;; An extension declares its subtree of the top-level "extensions" object in
;;;; settings.json -- key names and value schemas -- and validation, boot
;;;; diagnostics, /settings introspection, and tiering (via the existing
;;;; merge-settings) come free. A `(settings NAME SCHEMA)` clause inside
;;;; defextension installs into a per-protocol registry and retracts cleanly,
;;;; the same lifecycle as the :tool / :effect kinds.
;;;;
;;;; Describes, never grants: a declaration only describes configuration; no
;;;; settings key confers authority. Install touches the declaration registry
;;;; and nothing else -- in particular never the protocol's grant-set.

(in-package #:kli/config)

;;; The schema language. A spec is quoted data:
;;;
;;;   SPEC := (:object (KEY SPEC)...)          KEY a verbatim JSON key string
;;;         | (:string  &key default)
;;;         | (:boolean &key default)
;;;         | (:integer &key min max default)
;;;         | (:number  &key min max default)
;;;         | (:enum (VALUE...) &key default)  VALUEs are strings
;;;         | (:or SPEC...)
;;;
;;; Every declared key is optional; a leaf :default is what reads fall back to
;;; when settings carry no value. Malformed data warns (settings never break
;;; boot); a malformed schema is an author error and signals.

(defparameter +settings-spec-heads+
  '(:object :string :boolean :integer :number :enum :or))

(defun settings-spec-head (spec)
  (and (consp spec)
       (keywordp (first spec))
       (member (first spec) +settings-spec-heads+)
       (first spec)))

(defun settings-spec-options (spec)
  "The &key tail of a leaf SPEC, or NIL for :object and :or."
  (case (settings-spec-head spec)
    ((:string :boolean :integer :number) (rest spec))
    (:enum (cddr spec))
    (otherwise nil)))

(defun settings-spec-entries (spec)
  "The (KEY SPEC) entries of an :object spec."
  (rest spec))

(defun json-boolean-p (value)
  (typep value 'boolean))

(defun json-number-p (value)
  (and (realp value) (not (json-boolean-p value))))

(defun validate-settings-schema (spec)
  "Signal a descriptive error unless SPEC is a well-formed settings schema.
Returns SPEC."
  (labels
      ((bad (format &rest arguments)
         (error "Malformed settings schema at ~S: ~?" spec format arguments))
       (check-default (leaf predicate description)
         (let* ((marker (list :absent))
                (default (getf (settings-spec-options leaf) :default marker)))
           (unless (or (eq default marker) (funcall predicate default))
             (bad "default ~S is not ~A in ~S." default description leaf))))
       (check-bounds (leaf)
         (let ((min (getf (settings-spec-options leaf) :min))
               (max (getf (settings-spec-options leaf) :max)))
           (dolist (bound (list min max))
             (unless (or (null bound) (realp bound))
               (bad "bound ~S is not a number in ~S." bound leaf)))))
       (walk (node)
         (case (settings-spec-head node)
           (:object
            (dolist (entry (settings-spec-entries node))
              (unless (and (consp entry)
                           (stringp (first entry))
                           (consp (rest entry))
                           (null (cddr entry)))
                (bad "object entry ~S is not (KEY SPEC) with a string key."
                     entry))
              (walk (second entry))))
           (:string (check-default node #'stringp "a string"))
           (:boolean (check-default node #'json-boolean-p "a boolean"))
           (:integer (check-bounds node)
            (check-default node #'integerp "an integer"))
           (:number (check-bounds node)
            (check-default node #'json-number-p "a number"))
           (:enum
            (let ((values (second node)))
              (unless (and (consp values) (every #'stringp values))
                (bad "enum values ~S are not a list of strings." values))
              (check-default node
                             (lambda (default)
                               (member default values :test #'equal))
                             (format nil "one of ~{~S~^, ~}" values))))
           (:or
            (let ((alternatives (rest node)))
              (unless (rest alternatives)
                (bad "an :or needs at least two alternatives."))
              (mapc #'walk alternatives)))
           (otherwise
            (bad "~S is not a settings spec (heads: ~{~S~^, ~})."
                 node +settings-spec-heads+)))))
    (walk spec)
    spec))

(defun settings-spec-description (spec)
  "A short human phrase for what SPEC accepts, for diagnostics."
  (ecase (settings-spec-head spec)
    (:object "an object")
    (:string "a string")
    (:boolean "a boolean")
    (:integer "an integer")
    (:number "a number")
    (:enum (format nil "one of ~{~S~^, ~}" (second spec)))
    (:or (format nil "~{~A~^ or ~}"
                 (mapcar #'settings-spec-description (rest spec))))))

(defun render-settings-json-value (value)
  (cond ((eq value t) "true")
        ((eq value nil) "false")
        ((hash-table-p value) "an object")
        ((and (vectorp value) (not (stringp value))) "an array")
        (t (format nil "~S" value))))

(defun settings-schema-diagnostics (spec value path)
  "Diagnostics (a list of strings) for VALUE against SPEC. PATH names the
value's location, e.g. \"extensions.skar\". Empty when VALUE conforms.
Fail-soft by design: callers warn with these, never signal."
  (labels
      ((type-mismatch (spec value path)
         (list (format nil "~A: expected ~A, got ~A."
                       path
                       (settings-spec-description spec)
                       (render-settings-json-value value))))
       (walk (spec value path)
         (ecase (settings-spec-head spec)
           (:object
            (if (hash-table-p value)
                (let ((diagnostics '()))
                  (maphash
                   (lambda (key entry-value)
                     (let ((entry (assoc key (settings-spec-entries spec)
                                         :test #'equal))
                           (entry-path (format nil "~A.~A" path key)))
                       (setf diagnostics
                             (append diagnostics
                                     (if entry
                                         (walk (second entry) entry-value
                                               entry-path)
                                         (list (format nil "~A: unknown key."
                                                       entry-path)))))))
                   value)
                  diagnostics)
                (type-mismatch spec value path)))
           (:string
            (unless (stringp value) (type-mismatch spec value path)))
           (:boolean
            (unless (json-boolean-p value) (type-mismatch spec value path)))
           ((:integer :number)
            (let ((typed-p (if (eq (settings-spec-head spec) :integer)
                               (integerp value)
                               (json-number-p value)))
                  (min (getf (settings-spec-options spec) :min))
                  (max (getf (settings-spec-options spec) :max)))
              (cond
                ((not typed-p) (type-mismatch spec value path))
                ((and min (< value min))
                 (list (format nil "~A: ~A is below minimum ~A." path value min)))
                ((and max (> value max))
                 (list (format nil "~A: ~A is above maximum ~A." path value max))))))
           (:enum
            (unless (and (stringp value)
                         (member value (second spec) :test #'equal))
              (type-mismatch spec value path)))
           (:or
            (when (loop for alternative in (rest spec)
                        always (walk alternative value path))
              (type-mismatch spec value path))))))
    (walk spec value path)))

;;; The declaration value + its contribution wrapper.

(defun settings-declaration-key-string (key)
  "The verbatim JSON key for KEY: a string passes through (camelCase survives),
a symbol downcases."
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))))

(defclass settings-declaration ()
  ((key
    :initarg :key
    :reader settings-declaration-key
    :documentation "The subtree's key under the top-level \"extensions\"
object -- a verbatim JSON key string.")
   (schema
    :initarg :schema
    :reader settings-declaration-schema)))

(defun make-settings-declaration (&key key schema)
  (make-instance 'settings-declaration
                 :key (settings-declaration-key-string key)
                 :schema (validate-settings-schema schema)))

(defclass settings-declaration-contribution (contribution)
  ((declaration
    :initarg :declaration
    :reader contribution-settings-declaration)))

(defun make-settings-declaration-contribution (&key name declaration source)
  (make-instance 'settings-declaration-contribution
                 :kind :settings
                 :name (or name
                           (normalize-extension-id
                            (settings-declaration-key declaration)))
                 :declaration declaration
                 :source source))

;;; Per-protocol registry, keyed by the subtree key string, list-valued so
;;; re-installs stack and retract peels exactly one.

(defparameter +settings-declarations-storage-key+
  :kli/config.settings-declarations)

(defun protocol-settings-declarations (protocol)
  (ensure-protocol-storage protocol +settings-declarations-storage-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun find-settings-declaration (protocol key)
  (let ((contribution
          (first (gethash (settings-declaration-key-string key)
                          (protocol-settings-declarations protocol)))))
    (and contribution (contribution-settings-declaration contribution))))

(defun list-settings-declarations (protocol)
  "The active settings-declaration contribution per subtree key, so callers
can read each declaration and its owner via `contribution-extension`."
  (let ((contributions '()))
    (maphash (lambda (key cs)
               (declare (ignore key))
               (when cs (push (first cs) contributions)))
             (protocol-settings-declarations protocol))
    (sort contributions #'string<
          :key (lambda (c)
                 (settings-declaration-key
                  (contribution-settings-declaration c))))))

;;; Validation against loaded settings.

(defun settings-declaration-diagnostics (declaration settings)
  "Diagnostics for DECLARATION's subtree under SETTINGS. The subtree lives at
extensions.<key>; an absent subtree is clean -- every declared key is optional."
  (let ((key (settings-declaration-key declaration)))
    (multiple-value-bind (subtree present)
        (settings-value settings "extensions" key)
      (if present
          (settings-schema-diagnostics (settings-declaration-schema declaration)
                                       subtree
                                       (format nil "extensions.~A" key))
          '()))))

;;; Install / retract -- symmetric, the reversibility contract.

(defmethod check-contribution-precondition ((protocol extension-protocol)
                                            (contribution settings-declaration-contribution)
                                            context)
  (declare (ignore protocol context))
  (unless (typep (contribution-settings-declaration contribution)
                 'settings-declaration)
    (error "Not a settings declaration: ~S"
           (contribution-settings-declaration contribution))))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution settings-declaration-contribution)
                                 context)
  (check-contribution-precondition protocol contribution context)
  (let ((declaration (contribution-settings-declaration contribution)))
    (push contribution
          (gethash (settings-declaration-key declaration)
                   (protocol-settings-declarations protocol)))
    (push contribution (protocol-installed-contributions protocol))
    ;; Boot diagnostics: check the declared subtree against the settings
    ;; already loaded, so a misconfigured key warns at activation rather than
    ;; at first read. Fail-soft -- diagnostics never abort an activation.
    (let ((service (and context (find-config-service context))))
      (when service
        (dolist (diagnostic (settings-declaration-diagnostics
                             declaration (config-service-settings service)))
          (warn "kli settings: ~A" diagnostic))))
    contribution))

(defmethod refresh-runtime-contribution ((protocol extension-protocol)
                                         (contribution settings-declaration-contribution)
                                         context)
  "Refresh settings declaration diagnostics after boot snapshot reuse.

The declaration registry/topology is already installed in the snapshot. Runtime
config files have been rebound before refresh, so re-run only the fail-soft
diagnostic check against the current merged settings. Reads through
declared-settings-value continue to use the existing declaration and config
service."
  (declare (ignore protocol))
  (let ((service (and context (find-config-service context)))
        (declaration (contribution-settings-declaration contribution)))
    (when service
      (dolist (diagnostic (settings-declaration-diagnostics
                           declaration (config-service-settings service)))
        (warn "kli settings: ~A" diagnostic))))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution settings-declaration-contribution)
                                 context)
  (declare (ignore context))
  (let* ((key (settings-declaration-key
               (contribution-settings-declaration contribution)))
         (remaining (remove contribution
                            (gethash key (protocol-settings-declarations
                                          protocol)))))
    (if remaining
        (setf (gethash key (protocol-settings-declarations protocol)) remaining)
        (remhash key (protocol-settings-declarations protocol))))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

;;; Register :settings into kli's compiler registry, so (settings NAME SCHEMA)
;;; is legal inside any defextension's :provides.

(defcontribution-kind :settings (extension-id form)
  (destructuring-bind (_ key schema) form
    (declare (ignore _))
    `(make-settings-declaration-contribution
      :declaration (make-settings-declaration :key ',key :schema ',schema)
      :source ',extension-id)))

;;; Reads. Every declared key is optional: a read falls back to the leaf's
;;; declared :default, so an extension needs no presence checks of its own.

(defun settings-schema-default (schema keys)
  "The declared default at KEYS under SCHEMA. Values (default present-p).
Walking stops at anything but an :object interior node; only leaf :default
options are defaults."
  (let ((node schema))
    (dolist (key keys)
      (unless (eq (settings-spec-head node) :object)
        (return-from settings-schema-default (values nil nil)))
      (let ((entry (assoc (settings-declaration-key-string key)
                          (settings-spec-entries node)
                          :test #'equal)))
        (unless entry
          (return-from settings-schema-default (values nil nil)))
        (setf node (second entry))))
    (let* ((marker (list :absent))
           (default (getf (settings-spec-options node) :default marker)))
      (if (eq default marker)
          (values nil nil)
          (values default t)))))

(defun settings-declaration-value (declaration settings &rest keys)
  "Value at extensions.<key>.KEYS... in SETTINGS, else DECLARATION's schema
default at KEYS. Values (value source) with source :settings, :default, or NIL
when neither carries a value."
  (multiple-value-bind (value present)
      (apply #'settings-value settings "extensions"
             (settings-declaration-key declaration)
             (mapcar #'settings-declaration-key-string keys))
    (if present
        (values value :settings)
        (multiple-value-bind (default default-present)
            (settings-schema-default (settings-declaration-schema declaration)
                                     keys)
          (if default-present
              (values default :default)
              (values nil nil))))))

(defun declared-settings-value (context key &rest keys)
  "Value at extensions.KEY.KEYS... from CONTEXT's merged settings, else the
declared schema default. Values (value source) as in
`settings-declaration-value`; (values nil nil) when KEY has no declaration or
no config service is installed."
  (let* ((protocol (active-protocol context))
         (declaration (and protocol (find-settings-declaration protocol key)))
         (service (find-config-service context)))
    (if (and declaration service)
        (apply #'settings-declaration-value declaration
               (config-service-settings service)
               keys)
        (values nil nil))))

;;; Introspection.

(defun extension-settings-report (protocol settings)
  "One plist per declared subtree -- key, owning extension, whether SETTINGS
carries the subtree, and its diagnostics -- plus the keys under \"extensions\"
no declaration owns, under :undeclared."
  (let* ((declared (list-settings-declarations protocol))
         (extensions-object (settings-value settings "extensions"))
         (undeclared '()))
    (when (hash-table-p extensions-object)
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (unless (find-settings-declaration protocol key)
                   (push key undeclared)))
               extensions-object))
    (list
     :declarations
     (mapcar (lambda (contribution)
               (let ((declaration
                       (contribution-settings-declaration contribution)))
                 (list :key (settings-declaration-key declaration)
                       :extension (contribution-extension contribution)
                       :present (and (nth-value
                                      1 (settings-value
                                         settings "extensions"
                                         (settings-declaration-key declaration)))
                                     t)
                       :diagnostics (settings-declaration-diagnostics
                                     declaration settings))))
             declared)
     :undeclared (sort undeclared #'string<))))

(defun format-extension-settings-report (report)
  "Render REPORT (from `extension-settings-report`) for /settings. NIL when
there is nothing to say -- no declarations and no undeclared subtrees."
  (let ((declarations (getf report :declarations))
        (undeclared (getf report :undeclared)))
    (when (or declarations undeclared)
      (with-output-to-string (out)
        (format out "Extension settings:")
        (dolist (entry declarations)
          (format out "~%  ~A (~(~A~)) ~:[declared~;configured~]~
                       ~@[ -- ~D diagnostic~:P~]"
                  (getf entry :key)
                  (getf entry :extension)
                  (getf entry :present)
                  (let ((n (length (getf entry :diagnostics))))
                    (and (plusp n) n)))
          (dolist (diagnostic (getf entry :diagnostics))
            (format out "~%    ~A" diagnostic)))
        (when undeclared
          (format out "~%  undeclared: ~{~A~^, ~}" undeclared))))))
