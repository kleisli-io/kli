(in-package #:kli/model/providers/compatible)

(defvar *providers-path* nil "Override for providers.json. NIL means XDG default.")

(defun providers-path ()
  (or *providers-path* (uiop:xdg-config-home "kli" "providers.json")))

(defun %object->alist (ht)
  (when (hash-table-p ht)
    (loop for k being the hash-keys of ht using (hash-value v) collect (cons k v))))

(defun %intern-api (s)
  (cond ((null s) :openai-completions)
        ((string-equal s "openai-completions") :openai-completions)
        ((string-equal s "openai-responses") :openai-responses)
        (t (error "Unknown provider api ~S in providers config." s))))

(defun %json-field (object field)
  (multiple-value-bind (value present-p) (gethash field object)
    (values value present-p)))

(defun %json-null-p (value)
  (and (symbolp value)
       (string= "NULL" (symbol-name value))))

(defun %json-option-default (value)
  (if (stringp value)
      (intern (string-upcase value) :keyword)
      value))

(defun %json-array->list (array)
  (loop for value across (or array #()) collect value))

(defun %schema-arguments (schema)
  (unless (hash-table-p schema)
    (error "Model option schema must be a JSON object, got ~S." schema))
  (let ((args '()))
    (labels ((put (key value)
               (setf args (append args (list key value)))))
      (multiple-value-bind (type present-p) (%json-field schema "type")
        (when present-p
          (put :type (intern (string-upcase type) :keyword))))
      (multiple-value-bind (values present-p) (%json-field schema "values")
        (when present-p
          (put :values (%json-array->list values))))
      (multiple-value-bind (default present-p) (%json-field schema "default")
        (when present-p
          (put :default (%json-option-default default))))
      (multiple-value-bind (min present-p) (%json-field schema "min")
        (when present-p
          (put :min min)))
      (multiple-value-bind (max present-p) (%json-field schema "max")
        (when present-p
          (put :max max)))
      args)))

(defun %parse-option-schema (option-id schema)
  (apply #'make-model-option-schema option-id (%schema-arguments schema)))

(defun %parse-options-object (options &key allow-null)
  (cond ((null options) nil)
        ((hash-table-p options)
         (loop for option-id being the hash-keys of options using (hash-value schema)
               collect (cons (normalize-option-id option-id)
                             (if (or (null schema) (%json-null-p schema))
                                 (if allow-null
                                     :remove
                                     (error "Provider-level option ~A cannot be null." option-id))
                                 (%parse-option-schema option-id schema)))))
        (t
         (error "options must be a JSON object, got ~S." options))))

(defun %replace-option-schema (schemas directive)
  (destructuring-bind (option-id . schema) directive
    (append (remove option-id schemas
                    :key #'model-option-schema-option-id
                    :test #'equal)
            (list schema))))

(defun %remove-option-schema (schemas option-id)
  (remove option-id schemas
          :key #'model-option-schema-option-id
          :test #'equal))

(defun %merge-option-schemas (provider-directives model-directives)
  (let ((schemas '()))
    (dolist (directive provider-directives)
      (setf schemas (%replace-option-schema schemas directive)))
    (dolist (directive model-directives)
      (let ((option-id (car directive))
            (value (cdr directive)))
        (if (eq value :remove)
            (setf schemas (%remove-option-schema schemas option-id))
            (setf schemas (%replace-option-schema schemas directive)))))
    schemas))

(defun %parse-provider-config (pid spec)
  (let* ((provider-options (%parse-options-object (gethash "options" spec)))
         (models (loop for m across (or (gethash "models" spec) #())
                       for model-directives = (%parse-options-object
                                               (gethash "options" m)
                                               :allow-null t)
                       collect (list :id (gethash "id" m)
                                     :name (or (gethash "name" m) (gethash "id" m))
                                     :context-window (gethash "context-window" m)
                                     :option-schemas
                                     (%merge-option-schemas provider-options
                                                            model-directives)))))
    (list :provider-id pid
          :base-url (gethash "base-url" spec)
          :api (%intern-api (gethash "api" spec))
          :key-env (gethash "key-env" spec)
          :url-path (gethash "url-path" spec)
          :headers (%object->alist (gethash "headers" spec))
          :option-schemas (mapcar #'cdr provider-options)
          :models models)))

(defun parse-provider-configs (json-string)
  "providers.json text to list of provider spec plists. The file is a JSON
object keyed by provider-id and holds custom OpenAI-compatible providers.
Secrets never live in the file -- only the env-var name (key-env). The key
comes from that env var or a persisted static. Invalid provider entries are
skipped so other compatible providers can still load."
  (let ((top (com.inuoe.jzon:parse json-string)))
    (when (hash-table-p top)
      (loop for pid being the hash-keys of top using (hash-value spec)
            append (handler-case
                       (list (%parse-provider-config pid spec))
                     (error (condition)
                       (warn "Skipping compatible provider ~A: ~A" pid condition)
                       nil))))))

(defun load-provider-configs (&optional (path (providers-path)))
  (when (probe-file path)
    (parse-provider-configs (uiop:read-file-string path))))

(defun register-provider-config (spec context)
  "Register one provider spec via the shared installer, returning its
contribution-state. A credential-reference is registered only when the spec
names a key-env."
  (let* ((pid (getf spec :provider-id))
         (api (getf spec :api))
         (url-path (getf spec :url-path))
         (key-env (getf spec :key-env)))
    (install-provider-catalogue
     context
     :provider-id pid
     :display-name pid
     :api api
     :config (make-provider-config
              :base-url (getf spec :base-url)
              :headers (getf spec :headers))
     :credential (when key-env (list :env key-env))
     :transport-profile (when (and (eq api :openai-responses) url-path)
                          (list :provider (list :url-path url-path)))
     :models (loop for m in (getf spec :models)
                   collect (list :id (getf m :id)
                                 :name (getf m :name)
                                 :context-window (getf m :context-window)
                                 :option-schemas (getf m :option-schemas))))))

(defun install-compatible-providers (protocol contribution context)
  "Install every configured compatible provider, collecting the per-spec
contribution-states the retractor drains."
  (declare (ignore protocol contribution))
  (list :specs (loop for spec in (load-provider-configs)
                     collect (register-provider-config spec context))))

(defun installed-compatible-provider-contribution (protocol)
  "The installed compatible-provider effect contribution on PROTOCOL, if any."
  (find-if (lambda (contribution)
             (and (eq (contribution-kind contribution) :effect)
                  (eq (contribution-name contribution) :compatible-provider)))
           (protocol-installed-contributions protocol)))

(defun refresh-compatible-provider-contribution (protocol contribution context)
  "Reread providers.json for an already-loaded compatible-provider effect.

Boot snapshot reuse can carry a compatible-provider contribution installed in the
dump environment, before the user's runtime providers.json existed. Refreshing
drains the recorded per-spec registrations, then reruns the installer in place so
the contribution stays part of the loaded extension and remains retractable."
  (retract-compatible-providers protocol contribution context)
  (setf (contribution-state contribution)
        (install-compatible-providers protocol contribution context))
  contribution)

(defun refresh-compatible-providers (protocol context)
  "Refresh the installed compatible-provider effect, when present."
  (let ((contribution (installed-compatible-provider-contribution protocol)))
    (when contribution
      (refresh-compatible-provider-contribution protocol contribution context))))

(defun retract-compatible-providers (protocol contribution context)
  "Drain each per-spec contribution-state install returned, reversing every
registration."
  (declare (ignore protocol))
  (dolist (state (getf (contribution-state contribution) :specs))
    (retract-provider-catalogue state context))
  nil)
