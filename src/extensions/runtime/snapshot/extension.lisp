(in-package #:kli/runtime/snapshot)

(defparameter +snapshot-format-version+ 1)

(define-condition snapshot-unserializable (error)
  ((value :initarg :value :reader snapshot-unserializable-value))
  (:report (lambda (condition stream)
             (format stream "Value cannot be captured in a snapshot: ~S"
                     (snapshot-unserializable-value condition)))))

(defparameter +tagged-forms+
  '(:snapshot/symbol :snapshot/hash-table :snapshot/list))

(defun proper-list-p (object)
  (and (listp object)
       (loop for tail = object then (cdr tail)
             while (consp tail)
             finally (return (null tail)))))

(defun serialize-snapshot-value (value)
  "Convert VALUE into a durable datum built from keyword, string, integer,
nil, t, tagged forms, and proper lists of the same. Symbols and hash tables
become tagged forms that deserialize byte-faithfully. Anything else signals
`snapshot-unserializable` rather than encoding lossily."
  (typecase value
    (null nil)
    (keyword value)
    ((eql t) t)
    (integer value)
    (string value)
    (symbol
     (let ((package (symbol-package value)))
       (unless package
         (error 'snapshot-unserializable :value value))
       (list :snapshot/symbol (package-name package) (symbol-name value))))
    (hash-table
     (list :snapshot/hash-table
           (intern (symbol-name (hash-table-test value)) :keyword)
           (let ((entries '()))
             (maphash (lambda (key entry)
                        (push (list (serialize-snapshot-value key)
                                    (serialize-snapshot-value entry))
                              entries))
                      value)
             (nreverse entries))))
    (cons
     (unless (proper-list-p value)
       (error 'snapshot-unserializable :value value))
     (let ((items (mapcar #'serialize-snapshot-value value)))
       (if (member (first items) +tagged-forms+)
           (list* :snapshot/list items)
           items)))
    (t
     (error 'snapshot-unserializable :value value))))

(defun deserialize-snapshot-value (datum)
  "Inverse of `serialize-snapshot-value`."
  (cond
    ((atom datum) datum)
    ((eq (first datum) :snapshot/symbol)
     (destructuring-bind (package-name symbol-name) (rest datum)
       (let ((package (find-package package-name)))
         (unless package
           (error "Snapshot symbol package is not present in this image: ~A"
                  package-name))
         (intern symbol-name package))))
    ((eq (first datum) :snapshot/hash-table)
     (destructuring-bind (test entries) (rest datum)
       (let ((table (make-hash-table :test (ecase test
                                             (:eq 'eq)
                                             (:eql 'eql)
                                             (:equal 'equal)
                                             (:equalp 'equalp)))))
         (dolist (entry entries table)
           (setf (gethash (deserialize-snapshot-value (first entry)) table)
                 (deserialize-snapshot-value (second entry)))))))
    ((eq (first datum) :snapshot/list)
     (mapcar #'deserialize-snapshot-value (rest datum)))
    (t
     (mapcar #'deserialize-snapshot-value datum))))

(defun extension-manifest-symbol (extension)
  "Symbol whose value is EXTENSION's manifest thunk, derived from the
`defextension` naming scheme in the extension source's home package.
Returns NIL when EXTENSION has no such variable, as for builder-made
or directly constructed extensions."
  (let ((source (extension-source extension)))
    (when (and source
               (symbolp source)
               (not (keywordp source))
               (symbol-package source))
      (let ((symbol (find-symbol
                     (format nil "*~A-EXTENSION-MANIFEST*"
                             (symbol-name (object-id extension)))
                     (symbol-package source))))
        (and symbol (boundp symbol) symbol)))))

(defun snapshot-extension-records (protocol)
  "One record per root-activated extension, in activation order. A record
carries the extension id and the package and name of its manifest variable,
or a null :manifest when no variable is derivable."
  (loop for id in (protocol-root-activations protocol)
        for extension = (gethash id (protocol-extensions protocol))
        for manifest = (and extension (extension-manifest-symbol extension))
        collect (list :id id
                      :manifest (and manifest
                                     (list (package-name
                                            (symbol-package manifest))
                                           (symbol-name manifest))))))

(defun snapshot-protocol-storage (protocol)
  "Serialize PROTOCOL's storage table. Returns the captured entries and the
printed keys of entries whose key or value cannot be serialized. Skipped
entries hold code-derived structure that manifest reinstallation recreates."
  (let ((captured '())
        (skipped '()))
    (maphash (lambda (key value)
               (unless (snapshot-exempt-storage-key-p key)
                 (handler-case
                     (push (list (serialize-snapshot-value key)
                                 (serialize-snapshot-value value))
                           captured)
                   (snapshot-unserializable ()
                     (push (princ-to-string key) skipped)))))
             (protocol-storage-table protocol))
    (values (nreverse captured) (nreverse skipped))))

(defun class-slot-names (class)
  (unless (sb-mop:class-finalized-p class)
    (sb-mop:finalize-inheritance class))
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))

(defun kernel-managed-slot-names ()
  "Slots owned by the kernel's `live-object` base class. Identity and the
protocol back-pointer are re-established by reconstruction, not rehydrated."
  (class-slot-names (find-class 'live-object)))

;;;; Generics in protocol.lisp. Defaults below: slot-scrape / slot-writeback for
;;;; objects that declare no representation of their own.

(defmethod snapshot-representation ((object t))
  (let ((slots '())
        (skipped '()))
    (dolist (slot-name (class-slot-names (class-of object)))
      (unless (member slot-name (kernel-managed-slot-names))
        (when (slot-boundp object slot-name)
          (handler-case
              (push (list (serialize-snapshot-value slot-name)
                          (serialize-snapshot-value
                           (slot-value object slot-name)))
                    slots)
            (snapshot-unserializable ()
              (push (symbol-name slot-name) skipped))))))
    (list :slots (nreverse slots)
          :skipped-slots (nreverse skipped))))

(defmethod restore-representation ((object t) datum context)
  (declare (ignore context))
  (loop for (slot-datum value) in (getf datum :slots)
        do (setf (slot-value object (deserialize-snapshot-value slot-datum))
                 (deserialize-snapshot-value value))))

(defun snapshot-live-object (object)
  "OBJECT's id paired with its declared durable representation."
  (list* :id (serialize-snapshot-value (object-id object))
         (snapshot-representation object)))

(defun protocol-contributed-objects (protocol registry)
  "Registry-resident live objects materialized by PROTOCOL's installed
contributions plus its extension objects, deduplicated by id, in
installation order. Objects that live only in protocol slots, such as
providers and contracts, are excluded because manifest reinstallation
fully reconstructs them."
  (let ((objects '()))
    (flet ((collect (object)
             (when (and (typep object 'live-object)
                        (eq (find-live-object registry (object-id object))
                            object)
                        (not (find (object-id object) objects
                                   :key #'object-id :test #'equal)))
               (push object objects))))
      (dolist (contribution (reverse (protocol-installed-contributions
                                      protocol)))
        (typecase contribution
          (live-object-contribution
           (collect (contribution-object contribution)))
          (tool-contribution
           (collect (contribution-tool contribution)))
          (capability-contribution
           (let ((provider (contribution-provider contribution)))
             (when (typep provider 'live-object)
               (collect provider))))
          (provider-contract-contribution
           (collect (contribution-contract contribution)))))
      (maphash (lambda (id extension)
                 (declare (ignore id))
                 (collect extension))
               (protocol-extensions protocol)))
    (nreverse objects)))

(defvar *snapshot-nix-baseline-ids* nil
  "Thunk returning the string ids the running image baked in as present-at-boot
nix-declared baselines, or nil. Held as an indirection because the layer owning
that set loads after this package. Read live so a configured image's boot-time
setf is reflected at snapshot time, and so restore reads the RESTORING image's
baseline. Nil leaves :nix-baseline-ids empty, as on plain core.")

(defun image-nix-baseline-ids ()
  "String ids the running image bakes in as present-at-boot nix-declared
baselines, via the install-set hook, or nil on plain core."
  (and *snapshot-nix-baseline-ids* (funcall *snapshot-nix-baseline-ids*)))

(defvar *snapshot-isolated-ids* nil
  "Thunk of (protocol) returning the ids the snapshot must tolerate as
not-reconstructed for an isolated-server pin whose restore re-spawn fails: each
isolated pin's extension id plus the ids of the live objects it lifted. Held as
an indirection because the layer owning the pin ledger loads after this package.
Read live at capture, so a restore that cannot re-spawn the server defers these
rather than failing. Nil leaves the set empty.")

(defun image-isolated-ids (protocol)
  "Ids the snapshot tags as isolated-server-owned for PROTOCOL, via the
install-set hook, or nil when no hook is installed."
  (and *snapshot-isolated-ids* (funcall *snapshot-isolated-ids* protocol)))

(defun snapshot-context (context)
  "Capture CONTEXT's active protocol as durable data. For an extension
protocol the snapshot holds the manifest list in root activation order,
the serialized protocol storage, and the serializable slot state of every
contributed live object. The snapshot also names what it could not carry
under :unrestorable-extensions, :skipped-storage, and per-object
:skipped-slots. Other protocols are captured opaquely by id."
  (require-capability :protocol/snapshot)
  (let ((protocol (active-protocol context)))
    (typecase protocol
      (extension-protocol
       (multiple-value-bind (storage skipped-storage)
           (snapshot-protocol-storage protocol)
         (let ((extensions (snapshot-extension-records protocol)))
           (list :format-version +snapshot-format-version+
                 :kind :extension-protocol
                 :active-protocol (object-id protocol)
                 :nix-baseline-ids (image-nix-baseline-ids)
                 :isolated-server-ids (image-isolated-ids protocol)
                 :extensions extensions
                 :unrestorable-extensions
                 (loop for record in extensions
                       unless (getf record :manifest)
                         collect (getf record :id))
                 :storage storage
                 :skipped-storage skipped-storage
                 :objects (mapcar #'snapshot-live-object
                                  (protocol-contributed-objects
                                   protocol
                                   (context-registry context)))))))
      (t
       (list :format-version +snapshot-format-version+
             :kind :opaque
             :active-protocol (and protocol (object-id protocol)))))))

(defun resolve-manifest-symbol (reference)
  (destructuring-bind (package-name symbol-name) reference
    (let ((package (find-package package-name)))
      (unless package
        (error "Snapshot manifest package is not present in this image: ~A"
               package-name))
      (let ((symbol (find-symbol symbol-name package)))
        (unless (and symbol (boundp symbol))
          (error "Snapshot manifest is not present in this image: ~A::~A"
                 package-name symbol-name))
        symbol))))

(defun ensure-snapshot-manifests (protocol context snapshot)
  "Install, in captured order, every snapshot extension not already loaded into
PROTOCOL. A record with no manifest whose id is isolated-server-owned is skipped
rather than errored: its re-spawn is the step-0 replayer's job, and a re-spawn
that failed surfaces as a gap, not a fatal restore."
  (let ((isolated (getf snapshot :isolated-server-ids)))
    (dolist (record (getf snapshot :extensions))
      (let ((id (getf record :id)))
        (unless (extension-loaded-p protocol id)
          (let ((reference (getf record :manifest)))
            (cond
              (reference
               (install-manifest (symbol-value (resolve-manifest-symbol reference))
                                 protocol
                                 context))
              ((id-in-normalized-set-p id isolated))
              (t
               (error "Snapshot extension ~S has no manifest variable and ~
                       cannot be reconstructed." id)))))))))

(defun rehydrate-protocol-storage (protocol snapshot)
  (loop for (key value) in (getf snapshot :storage)
        do (setf (protocol-storage protocol (deserialize-snapshot-value key))
                 (deserialize-snapshot-value value))))

(defun cross-image-baseline-gap-ids (snapshot)
  "The snapshot's nix-declared baseline ids that THIS image does not bake -- a
cross-image restore where the baseline owner is absent, so the baseline's
captured objects cannot be reconstructed here. Compared under id normalization."
  (let ((image (mapcar #'normalize-extension-id (image-nix-baseline-ids))))
    (loop for id in (getf snapshot :nix-baseline-ids)
          unless (member (normalize-extension-id id) image)
            collect id)))

(defun id-in-normalized-set-p (id ids)
  "True when ID matches one of IDS under id normalization, so a keyword object
id and a string baseline or pin id agree."
  (and ids
       (member (normalize-extension-id id)
               (mapcar #'normalize-extension-id ids))
       t))

(defun rehydrate-live-objects (context snapshot gap-ids)
  "Rehydrate captured slot state onto each reconstructed live object. An object
no manifest reconstructed is a hard error -- unless its id is a baseline the
snapshot carried that this image does not bake (GAP-IDS), or it is owned by an
isolated-server pin whose re-spawn did not produce it, in which case it is
deferred for the gap diagnostic rather than fatal."
  (let ((isolated (getf snapshot :isolated-server-ids)))
    (dolist (record (getf snapshot :objects))
      (let* ((id (deserialize-snapshot-value (getf record :id)))
             (object (find-live-object (context-registry context) id)))
        (cond
          (object
           (restore-representation object record context))
          ((id-in-normalized-set-p id gap-ids))
          ((id-in-normalized-set-p id isolated))
          (t
           (error "Snapshot live object was not reconstructed by its ~
                   manifests: ~S" id)))))))

(defvar *replay-remote-installs* nil
  "Step-0 restore hook of (protocol context snapshot restorer-grant), installed
by the layer that owns the runtime install-set value model. Held as an
indirection because that layer loads after this runtime package, so
apply-snapshot cannot name it at load time. The restorer's grant bounds any
re-spawn so replay never confers authority the restorer lacked. Nil leaves
restore a manifest-only rehydrate.")

(defun replay-remote-installs (protocol context snapshot restorer-grant)
  "Re-establish the snapshot's runtime install-set before manifests rehydrate,
so an extension's code and CLOS package exist when ensure-snapshot-manifests
resolves its manifest variable. RESTORER-GRANT bounds any re-spawn. The
replayer's root activations are transient here -- the wholesale storage
overwrite below is authoritative for the end state, so a re-activated root is
not double-counted, while a mid-replay error still rolls back via the transient
root-activations list."
  (when *replay-remote-installs*
    (funcall *replay-remote-installs* protocol context snapshot restorer-grant)))

(defvar *reconcile-baseline-gap* nil
  "Restore hook of (protocol context snapshot gap-ids) recording a diagnostic for
each nix-declared baseline the snapshot carried but this image does not bake, so a
cross-image restore reports a gap rather than failing. Installed by the layer that
owns the install-set ledger; nil leaves restore without the diagnostic.")

(defun reconcile-baseline-gap (protocol context snapshot gap-ids)
  "Hand the cross-image baseline GAP-IDS to the diagnostic hook, if installed."
  (when (and gap-ids *reconcile-baseline-gap*)
    (funcall *reconcile-baseline-gap* protocol context snapshot gap-ids)))

(defvar *reconcile-isolated-gap* nil
  "Restore hook of (protocol context snapshot) recording a diagnostic for each
isolated-server pin whose restore re-spawn did not bring its extension live, so a
host missing the server binary reports a gap rather than silently dropping it.
Installed by the layer owning the install-set ledger; nil leaves restore without
the diagnostic.")

(defun reconcile-isolated-gap (protocol context snapshot)
  "Run the isolated-server gap diagnostic, if installed. Runs after storage
rehydrate so its gaps survive the wholesale overwrite."
  (when *reconcile-isolated-gap*
    (funcall *reconcile-isolated-gap* protocol context snapshot)))

(defun apply-snapshot (protocol context snapshot
                       &optional (restorer-grant
                                  (kli/ext:subject-grant kli/ext:*call-subject*)))
  (replay-remote-installs protocol context snapshot restorer-grant)
  (ensure-snapshot-manifests protocol context snapshot)
  (rehydrate-protocol-storage protocol snapshot)
  (let ((gap-ids (cross-image-baseline-gap-ids snapshot)))
    (rehydrate-live-objects context snapshot gap-ids)
    (reconcile-baseline-gap protocol context snapshot gap-ids))
  (reconcile-isolated-gap protocol context snapshot))

(defun reconstruct-snapshot-protocol (context snapshot protocol-id restorer-grant)
  (unless (eq (getf snapshot :kind) :extension-protocol)
    (error "Snapshot protocol ~S is not registered and the snapshot does ~
            not carry the data to reconstruct it." protocol-id))
  (let ((protocol (make-extension-protocol :id protocol-id)))
    (register-live-object (context-registry context) protocol)
    (handler-case
        (progn
          (apply-snapshot protocol context snapshot restorer-grant)
          protocol)
      (error (condition)
        (ignore-errors
          (retract-installed-extensions
           (loop for id in (protocol-root-activations protocol)
                 collect (gethash id (protocol-extensions protocol)))
           protocol
           context))
        (remove-live-object (context-registry context) (object-id protocol))
        (error condition)))))

(defun restore-active-protocol (context snapshot)
  "Re-establish SNAPSHOT's protocol as CONTEXT's active protocol. A
still-registered protocol is rehydrated in place. A discarded protocol, or
one absent because the image restarted, is reconstructed from scratch by
installing the captured manifest list and rehydrating the captured storage
and live-object state."
  (require-capability :protocol/restore)
  (let ((restorer-grant (kli/ext:subject-grant kli/ext:*call-subject*))
        (protocol-id (getf snapshot :active-protocol)))
    (unless protocol-id
      (error "Snapshot has no active protocol: ~S" snapshot))
    (let ((existing (find-live-object (context-registry context)
                                      protocol-id)))
      (let ((protocol
              (cond
                ((null existing)
                 (reconstruct-snapshot-protocol context snapshot protocol-id
                                                restorer-grant))
                ((typep existing 'extension-protocol)
                 (apply-snapshot existing context snapshot restorer-grant)
                 existing)
                (t existing))))
        (setf (active-protocol context) protocol)
        protocol))))

(defun make-snapshot-contract ()
  (make-provider-contract
   :id :runtime/snapshot/v1
   :capability :runtime/snapshot
   :required-entries
   '(:snapshot-context
     :restore-active-protocol)))

(defun make-snapshot-provider ()
  (make-provider
   :id :runtime-snapshot-provider
   :capability :runtime/snapshot
   :contracts '(:runtime/snapshot/v1)
   :entries
   (list :snapshot-context #'snapshot-context
         :restore-active-protocol #'restore-active-protocol)))

(defextension snapshot
  (:provides
   (contract runtime/snapshot/v1
     (make-snapshot-contract))
   (capability runtime/snapshot (make-snapshot-provider))))
