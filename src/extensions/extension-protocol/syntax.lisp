(in-package #:kli/ext)

(defvar *defining-protocol* nil
  "Protocol receiving `define-extension` registrations. Bound by
`protocol-load-extension-source` and `load-extension-manifest`.")

(defvar *defined-extension-ids* nil
  "Dynamic accumulator of extension ids registered during a single
`protocol-load-extension-source` call. Rebound per source via `let`.")

(defparameter +extension-factories-storage-key+ :kli/ext.extension-factories
  "Storage key under which a protocol's per-id extension-factory hash lives.")

(defun extension-factories (protocol)
  "Hash mapping extension-id → factory thunk on PROTOCOL. Lazily created."
  (ensure-protocol-storage protocol
                           +extension-factories-storage-key+
                           (lambda () (make-hash-table :test #'equal))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *contribution-form-compilers*
    (make-hash-table :test #'equal))

  (define-condition unknown-contribution-kind (error)
    ((kind :initarg :kind :reader unknown-contribution-kind-kind)
     (form :initarg :form :reader unknown-contribution-kind-form))
    (:report (lambda (condition stream)
               (format stream "Unknown contribution kind ~S in ~S."
                       (unknown-contribution-kind-kind condition)
                       (unknown-contribution-kind-form condition)))))

  (defun contribution-head (form)
    (unless (and (consp form) (symbolp (first form)))
      (error "Malformed contribution form: ~S" form))
    (normalize-extension-id (first form)))

  (defun define-contribution-form-compiler (kind compiler)
    (unless (functionp compiler)
      (error "Contribution form compiler must be a function: ~S" compiler))
    (let ((key (normalize-extension-id kind)))
      (setf (gethash key *contribution-form-compilers*) compiler)
      key))

  (defun compile-requirement-form (extension-id form)
    (destructuring-bind (kind name &key contract provider-id) form
      `(make-requirement
        :kind ',(normalize-extension-id kind)
        :name ',(normalize-extension-id name)
        :contract ',(and contract
                         (normalize-extension-id contract))
        :provider-id ',(and provider-id
                            (normalize-extension-id provider-id))
        :source ',extension-id)))

  (defun compile-contribution-form (extension-id form)
    (let* ((kind (contribution-head form))
           (compiler (gethash kind *contribution-form-compilers*)))
      (unless compiler
        (error 'unknown-contribution-kind :kind kind :form form))
      (funcall compiler extension-id form)))

  (defun parse-defextension-clauses (clauses)
    (let ((requirements '())
          (contributions '())
          (metadata '()))
      (dolist (clause clauses)
        (unless (consp clause)
          (error "Malformed defextension clause: ~S" clause))
        (case (normalize-extension-id (first clause))
          (:requires
           (setf requirements (append requirements (rest clause))))
          (:provides
           (setf contributions (append contributions (rest clause))))
          (:metadata
           (setf metadata (append metadata (second clause))))
          (otherwise
           (error "Unknown defextension clause: ~S" clause))))
      (values requirements contributions metadata))))

(defmacro defcontribution-kind (kind (extension-id-var form-var) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-contribution-form-compiler
      ',kind
      (lambda (,extension-id-var ,form-var)
        ,@body))))

(defcontribution-kind :capability (extension-id form)
  (destructuring-bind (_ name &optional provider-form) form
    (declare (ignore _))
    `(make-capability-contribution
      :name ',(normalize-extension-id name)
      :provider ,provider-form
      :source ',extension-id)))

(defcontribution-kind :live-object (extension-id form)
  (destructuring-bind (_ name object-form) form
    (declare (ignore _))
    `(make-live-object-contribution
      :name ',(normalize-extension-id name)
      :object ,object-form
      :source ',extension-id)))

(defcontribution-kind :contract (extension-id form)
  (destructuring-bind (_ name contract-form) form
    (declare (ignore _))
    `(make-provider-contract-contribution
      :name ',(normalize-extension-id name)
      :contract ,contract-form
      :source ',extension-id)))

(defcontribution-kind :effect (extension-id form)
  (unless (= (length form) 4)
    (error "Effect contribution must specify both installer and retractor: ~S.~@
            Use :no-op as the retractor for effects that intentionally need no cleanup."
           form))
  (destructuring-bind (_ name installer-form retractor-form) form
    (declare (ignore _))
    `(make-effect-contribution
      :name ',(normalize-extension-id name)
      :installer ,installer-form
      :retractor ,(if (eq retractor-form :no-op)
                      '(lambda (protocol contribution context)
                         (declare (ignore protocol contribution context)))
                      retractor-form)
      :source ',extension-id)))

(defcontribution-kind :method (extension-id form)
  (destructuring-bind (_ gf-name qualifiers specializer-names lambda-list &body body) form
    (declare (ignore _))
    `(make-method-contribution
      :gf-name ',gf-name
      :qualifiers ',qualifiers
      :specializer-names ',specializer-names
      :lambda-list ',lambda-list
      :body ',body
      :source ',extension-id)))

(defcontribution-kind :tool (extension-id form)
  (destructuring-bind (_ name &key label description parameters runner
                         renderer metadata) form
    (declare (ignore _))
    `(make-tool-contribution
      :name ',(normalize-extension-id name)
      :tool (make-tool
             :id ',(list extension-id
                         :tool
                         (normalize-extension-id name))
             :name ',(normalize-extension-id name)
             :label ,label
             :description ,description
             :parameters ,parameters
             :runner ,runner
             :renderer ,renderer
             :metadata ,metadata)
      :source ',extension-id)))

(defcontribution-kind :grant (extension-id form)
  (destructuring-bind (_ principal grant-form) form
    (declare (ignore _))
    `(make-grant-contribution
      :principal ',(normalize-extension-id principal)
      :grant ,grant-form
      :source ',extension-id)))

(defun define-extension (id factory)
  "Register FACTORY for ID in `*defining-protocol*`. Errors if unbound."
  (assert *defining-protocol* ()
          "define-extension called outside a source-load context (no *defining-protocol*).")
  (let ((extension-id (normalize-extension-id id)))
    (define-extension-in *defining-protocol* extension-id factory)
    (pushnew extension-id *defined-extension-ids* :test #'equal)
    extension-id))

(defun define-extension-in (protocol id factory)
  "Register FACTORY for extension ID against PROTOCOL's factory cache.
Shadows any source-level factory with the same ID for this protocol only.
Other protocols are unaffected."
  (let ((extension-id (normalize-extension-id id)))
    (setf (gethash extension-id (extension-factories protocol)) factory)
    extension-id))

(defun find-extension-definition (protocol id)
  "Factory for ID in PROTOCOL. Errors if not registered."
  (let ((extension-id (normalize-extension-id id)))
    (or (gethash extension-id (extension-factories protocol))
        (error "Extension ~S is not defined in protocol ~S."
               id protocol))))

(defun make-defined-extension (protocol id)
  (funcall (find-extension-definition protocol id)))

(defmethod protocol-load-extension ((protocol extension-protocol)
                                    (extension-id symbol)
                                    context)
  (activate-extension protocol
                      (make-defined-extension protocol extension-id)
                      context))

(defgeneric protocol-load-extension-source (protocol source context))

(defun load-extension-source (context source)
  (require-capability :image/eval)
  (unless (active-protocol context)
    (error "No active protocol is installed."))
  (protocol-load-extension-source (active-protocol context) source context))

(defun evaluate-extension-source (source)
  (etypecase source
    ((or pathname string)
     (load source))
    (cons
     (eval source))))

(defmethod protocol-load-extension-source ((protocol extension-protocol) source context)
  (let ((*defining-protocol* protocol)
        (*defined-extension-ids* '()))
    (evaluate-extension-source source)
    (let ((extension-ids (reverse *defined-extension-ids*)))
      (unless extension-ids
        (error "Extension source did not define an extension: ~S" source))
      (mapcar (lambda (extension-id)
                (protocol-load-extension protocol extension-id context))
              extension-ids))))

(defmacro defextension (name &body clauses)
  "Bind `*<NAME>-extension-manifest*` to a factory thunk for an
`extension` value. When `*defining-protocol*` is bound at macroexpansion
also register the factory into that protocol."
  (let ((id (normalize-extension-id name)))
    (multiple-value-bind (requirements contribution-forms metadata)
        (parse-defextension-clauses clauses)
      (let* ((manifest-var (symb '#:* id '#:-extension-manifest*))
             (requirement-specs (derive-requirement-specs contribution-forms
                                                          requirements))
             (defparameter-form
               `(defparameter ,manifest-var
                  (lambda ()
                    (make-extension
                     :id ',id
                     :source ',name
                     :metadata ',metadata
                     :requirements
                     (list ,@(mapcar (lambda (spec)
                                       (compile-requirement-form id spec))
                                     requirement-specs))
                     :contributions
                     (list ,@(mapcar (lambda (form)
                                       (compile-contribution-form id form))
                                     contribution-forms)))))))
        (if *defining-protocol*
            `(progn
               ,defparameter-form
               (eval-when (:load-toplevel :execute)
                 (define-extension ',id ,manifest-var)))
            defparameter-form)))))

(defmacro define-capability-binding (name &key capability contract)
  "Define NAME-provider and NAME-call helpers that route capability lookups
   through the receiver-borne protocol slot.

   NAME-provider takes a protocol, looks up the capability+contract via
   find-capability-provider, and signals an error if no provider is
   installed. NAME-call takes (protocol method &rest arguments) and threads
   through provider-call against the resolved provider.

   Use at top level of an extension's model.lisp to declare its outgoing
   capability dependencies. CAPABILITY and CONTRACT are keyword designators
   identifying the required capability and contract."
  (let ((provider-fn (symb name '-provider))
        (call-fn     (symb name '-call)))
    `(progn
       (defun ,provider-fn (protocol)
         (or (find-capability-provider protocol ,capability :contract ,contract)
             (error "No provider for capability ~S under protocol ~S."
                    ,capability protocol)))
       (defun ,call-fn (protocol method &rest arguments)
         (apply #'provider-call (,provider-fn protocol) method arguments))
       ',name)))

(defun call-with-manifest-capture (loader)
  "Run LOADER, a thunk performing the actual load, inside an ephemeral
   protocol that accumulates `*defined-extension-ids*`. Require exactly one
   id and return its captured factory thunk. The protocol is discarded on
   return. LOADER may evaluate one source or load a sequence of files under a
   package regime -- either way it must define exactly one extension."
  (let* ((ephemeral (make-extension-protocol :id :ephemeral-manifest-capture))
         (*defining-protocol* ephemeral)
         (*defined-extension-ids* '()))
    (funcall loader)
    (let ((ids (reverse *defined-extension-ids*)))
      (cond
        ((null ids) (error "Source did not define an extension."))
        ((rest ids) (error "Source defined multiple extensions; ambiguous: ~S" ids))
        (t (or (gethash (first ids) (extension-factories ephemeral))
               (error "Manifest not captured for ~S" (first ids))))))))

(defun load-extension-manifest (source)
  "Pure load of SOURCE. Returns a factory thunk -- each call yields a fresh
   `extension` instance with independent storage. SOURCE is a pathname,
   string, or quoted `defextension` form defining exactly one extension."
  (call-with-manifest-capture (lambda () (evaluate-extension-source source))))

(defun install-manifest (manifest protocol context)
  "Install MANIFEST (a factory thunk) into PROTOCOL. Sole protocol-mutating
   step. Returns the activated `extension` as a handle for `retract-manifest`."
  (let ((extension (funcall manifest)))
    (activate-extension protocol extension context)
    extension))

(defun retract-manifest (handle protocol context)
  "Symmetric undo of `install-manifest`."
  (deactivate-extension protocol handle context)
  (values))

(defun install-manifest-list (manifest-symbols protocol context)
  "Install each manifest named in MANIFEST-SYMBOLS into PROTOCOL in order.
   Each element is a symbol bound to a manifest factory thunk. Returns the
   activated extensions as handles in install order."
  (loop for sym in manifest-symbols
        collect (install-manifest (symbol-value sym) protocol context)))

(defun retract-installed-extensions (installed-extensions protocol context)
  "Deactivate INSTALLED-EXTENSIONS in reverse activation order."
  (dolist (extension (reverse installed-extensions))
    (deactivate-extension protocol extension context)))

(defmacro kli-extension ((var id) &body body)
  "Imperative counterpart of `defextension`. BODY accumulates contributions on
the builder VAR through author verbs (`command`, `on`, …), yielding the same
manifest thunk a `defextension` produces. The body runs inside the thunk, so
every call materializes fresh contributions — the manifest stays installable
into many protocols. When `*defining-protocol*` is bound (a source load),
register it so the file auto-activates, exactly as `defextension` does."
  (let ((nid (normalize-extension-id id)))
    `(let ((manifest
             (lambda ()
               (let ((,var (make-extension-builder :id ',nid)))
                 ,@body
                 (build-extension ,var)))))
       (when *defining-protocol*
         (define-extension ',nid manifest))
       manifest)))
