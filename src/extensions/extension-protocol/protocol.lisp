(in-package #:kli/ext)

(defun normalize-extension-id (name)
  (etypecase name
    (keyword name)
    (symbol (intern (string-upcase (symbol-name name)) :keyword))
    (string (intern (string-upcase name) :keyword))
    (cons (mapcar #'normalize-extension-id name))))

(defgeneric extension-contributions (protocol extension context))
(defgeneric check-contribution-precondition (protocol contribution context)
  (:documentation "Signal an error if CONTRIBUTION cannot be installed on
PROTOCOL, mutating nothing. Lets activate-extension validate every contribution
before installing any, so a failing precondition leaves no partial state. The
default passes; install-contribution calls it first, so a direct install is
validated identically."))
(defgeneric install-contribution (protocol contribution context))
(defgeneric retract-contribution (protocol contribution context))
(defgeneric protocol-load-extension (protocol extension context))
(defgeneric activate-extension (protocol extension context))
(defgeneric deactivate-extension (protocol extension context))
(defgeneric recode-extension (protocol extension new-source context))
(defgeneric requirement-satisfied-p (protocol requirement context))

(defclass extension-protocol (protocol)
  ((extensions
    :initform (make-hash-table :test #'equal)
    :accessor protocol-extensions)
   (installed-contributions
    :initform '()
    :accessor protocol-installed-contributions)
   (capabilities
    :initform (make-hash-table :test #'equal)
    :accessor protocol-capabilities)
   (provider-contracts
    :initform (make-hash-table :test #'equal)
    :accessor protocol-provider-contracts)
   (tools
    :initform (make-hash-table :test #'equal)
    :accessor protocol-tools)
   (storage
    :initform (make-hash-table :test #'eq)
    :accessor protocol-storage-table))
  (:documentation "Protocol with one typed slot per closed contribution kind.
All other per-protocol mutable state goes through `protocol-storage`."))

(defparameter +run-identity-storage-key+ :kli/ext.run-identity
  "Storage key for the per-process spill-namespace root, minted at protocol
construction. Snapshot-exempt: re-minted on reconstruction, never captured or
rehydrated, so two restores of one snapshot never share a root.")

(defun mint-run-identity ()
  "A fresh 32-hex identity from the OS CSPRNG."
  (ironclad:byte-array-to-hex-string
   (ironclad:random-data 16 (ironclad:make-prng :os))))

(defun make-extension-protocol (&key (id :extension-protocol))
  (let ((protocol (make-instance 'extension-protocol :id id)))
    (setf (gethash +run-identity-storage-key+ (protocol-storage-table protocol))
          (mint-run-identity))
    protocol))

(defun protocol-storage (protocol key &optional default)
  "Fetch the per-protocol value stored under KEY.

Extensions claim symbol-valued keys (typically keywords namespaced by
extension id) to store per-protocol state on PROTOCOL.  Returns DEFAULT
when the key is absent."
  (multiple-value-bind (value present-p)
      (gethash key (protocol-storage-table protocol))
    (if present-p
        value
        default)))

(defun (setf protocol-storage) (value protocol key)
  (setf (gethash key (protocol-storage-table protocol)) value))

(defun ensure-protocol-storage (protocol key thunk)
  "Return PROTOCOL's value for KEY, installing (FUNCALL THUNK) if absent.

Useful for lazy initialization of per-protocol mutable substructures
(e.g. registries, caches) that an extension wants to materialize on
first access."
  (multiple-value-bind (value present-p)
      (gethash key (protocol-storage-table protocol))
    (if present-p
        value
        (setf (gethash key (protocol-storage-table protocol))
              (funcall thunk)))))

(defun protocol-run-identity (protocol)
  "PROTOCOL's per-process spill-namespace root (see `+run-identity-storage-key+`)."
  (protocol-storage protocol +run-identity-storage-key+))

(defun snapshot-exempt-storage-key-p (key)
  "True for storage keys re-established by reconstruction rather than carried
across snapshot/restore, like the kernel-managed identity slots. Capturing such
a key would resurrect it on restore, so two restores of one snapshot would share
it."
  (eq key +run-identity-storage-key+))

(defvar *activating-extension* nil
  "Id of the extension whose contributions are currently being installed.
Bound by `activate-extension` so activations triggered by another
extension's contributions (manifest groups) are distinguishable from
root installs.")

(defvar *image-dump-in-progress* nil
  "True while build-time image dump code is materializing a boot snapshot.
Effects that acquire process-local resources which cannot survive
save-lisp-and-die may defer acquisition and reopen lazily at runtime.")

(defparameter +root-activations-storage-key+ :kli/ext.root-activations
  "Storage key holding the ordered list of root-activated extension ids.")

(defun protocol-root-activations (protocol)
  "Ids of extensions activated directly on PROTOCOL, in activation order.
Extensions installed by another extension's contributions (for example
profile manifest groups) are not roots. Together with each extension's
manifest this list is the protocol's reconstruction recipe."
  (protocol-storage protocol +root-activations-storage-key+))

(defun note-root-activation (protocol extension-id)
  (setf (protocol-storage protocol +root-activations-storage-key+)
        (append (protocol-root-activations protocol)
                (list extension-id))))

(defun forget-root-activation (protocol extension-id)
  (setf (protocol-storage protocol +root-activations-storage-key+)
        (remove extension-id (protocol-root-activations protocol)
                :test #'equal)))

;;; Named removable layers. An extension that recodes a shared slot -- a
;;; system-prompt transform, an authority grant -- installs a named layer keyed
;;; by its contribution id instead of capturing the slot's previous value.
;;; Retract is remove-by-label and is order independent: removing any layer
;;; recomposes the survivors from their labels, so retracting contributions out
;;; of install order can never strand a sibling -- the failure mode a
;;; captured-previous-value chain has whenever retraction is not strictly LIFO.
;;; This is the canonical replacement for the captured-previous-value pattern
;;; wherever a shared slot is recoded. The label is opaque and compared with
;;; EQUAL, so a raw contribution id or an (extension-id . name) pair both serve
;;; and a layer stays attributable to, and drainable by, the extension that owns
;;; it. A stack holds transforms (functions), so it is rebuilt on activation
;;; rather than serialized.

(defstruct (layer (:constructor %make-layer (label transform &optional (kind :transform))))
  (label nil :read-only t)
  (transform nil :read-only t)
  ;; :transform -- TRANSFORM is (running)->next, the full-rewrite escape hatch.
  ;; :append    -- TRANSFORM is a thunk ()->block concatenated onto the running
  ;;               value, so a layer that only adds a block never has to strip its
  ;;               own prior output to stay idempotent.
  (kind :transform :read-only t))

(defun make-layer-stack ()
  "An empty ordered layer stack. The stack is the composition order: the
earliest installed transform runs first, the latest last."
  '())

(defun layer-stack-labels (stack)
  "The labels in STACK, in composition order."
  (mapcar #'layer-label stack))

(defun layer-stack-member-p (stack label)
  "Non-nil when STACK carries a layer named LABEL."
  (and (member label stack :key #'layer-label :test #'equal) t))

(defun install-layer (stack label transform &key (position :last) (kind :transform))
  "STACK with TRANSFORM installed under LABEL. KIND is :transform (TRANSFORM takes
the running value and returns the next) or :append (TRANSFORM is a thunk whose
block is concatenated onto the running value). Re-installing an existing LABEL
replaces that layer in place, leaving composition order unchanged. A fresh LABEL
is placed last (default, latest in composition order), or first when POSITION is
:first."
  (let ((layer (%make-layer label transform kind)))
    (cond
      ((layer-stack-member-p stack label)
       (mapcar (lambda (existing)
                 (if (equal (layer-label existing) label) layer existing))
               stack))
      ((eq position :first) (cons layer stack))
      (t (append stack (list layer))))))

(defun remove-layer-by-label (stack label)
  "STACK without the layer named LABEL. Order independent: the surviving layers
keep their relative composition order, so retraction does not depend on the
order layers were installed."
  (remove label stack :key #'layer-label :test #'equal))

(defun compose-layers (stack base)
  "Fold each layer over BASE in composition order. A :transform layer's function
takes the running value and returns the next; an :append layer's function is a
thunk whose string is concatenated onto the running value. An empty stack returns
BASE."
  (reduce (lambda (value layer)
            (ecase (layer-kind layer)
              (:transform (funcall (layer-transform layer) value))
              (:append (concatenate 'string value (funcall (layer-transform layer))))))
          stack
          :initial-value base))

(defclass extension (live-object)
  ((source
    :initarg :source
    :initform nil
    :accessor extension-source)
   (version
    :initarg :version
    :initform 0
    :accessor extension-version)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor extension-metadata)
   (requirements
    :initarg :requirements
    :initform '()
    :accessor extension-requirement-list)
   (contributions
    :initarg :contributions
    :initform '()
    :accessor extension-contribution-list)))

(defun make-extension (&key id source version requirements contributions metadata)
  (make-instance 'extension
                 :id id
                 :source source
                 :version (or version 0)
                 :metadata metadata
                 :requirements requirements
                 :contributions contributions))

(defclass requirement ()
  ((kind
    :initarg :kind
    :reader requirement-kind)
   (name
    :initarg :name
    :reader requirement-name)
   (contract
    :initarg :contract
    :initform nil
    :reader requirement-contract)
   (provider-id
    :initarg :provider-id
    :initform nil
    :reader requirement-provider-id)
   (source
    :initarg :source
    :initform nil
    :reader requirement-source)))

(defun make-requirement (&key kind name contract provider-id source)
  (make-instance 'requirement
                 :kind (normalize-extension-id kind)
                 :name (normalize-extension-id name)
                 :contract (and contract
                                (normalize-extension-id contract))
                 :provider-id (and provider-id
                                   (normalize-extension-id provider-id))
                 :source source))

(defclass contribution ()
  ((kind
    :initarg :kind
    :reader contribution-kind)
   (name
    :initarg :name
    :reader contribution-name)
   (source
    :initarg :source
    :initform nil
    :reader contribution-source)
   (extension
    :initarg :extension
    :initform nil
    :accessor contribution-extension)))

(defclass live-object-contribution (contribution)
  ((object
    :initarg :object
    :reader contribution-object)))

(defclass capability-contribution (contribution)
  ((provider
    :initarg :provider
    :initform nil
    :reader contribution-provider)))

(defclass provider (live-object)
  ((capability
    :initarg :capability
    :reader provider-capability)
   (contracts
    :initarg :contracts
    :initform '()
    :reader provider-contract-list)
   (entries
    :initarg :entries
    :initform '()
    :accessor provider-entries)))

(defclass provider-contract (live-object)
  ((capability
    :initarg :capability
    :reader contract-capability)
   (required-entries
    :initarg :required-entries
    :initform '()
    :reader contract-required-entries)
   (validator
    :initarg :validator
    :initform nil
    :reader contract-validator)))

(defclass provider-contract-contribution (contribution)
  ((contract
    :initarg :contract
    :reader contribution-contract)))

(defclass effect-contribution (contribution)
  ((installer
    :initarg :installer
    :reader contribution-installer)
   (retractor
    :initarg :retractor
    :reader contribution-retractor)
   (state
    :initform nil
    :accessor contribution-state)))

(defclass tool (live-object)
  ((name
    :initarg :name
    :reader tool-name)
   (label
    :initarg :label
    :initform nil
    :accessor tool-label)
   (description
    :initarg :description
    :initform ""
    :accessor tool-description)
   (parameters
    :initarg :parameters
    :initform nil
    :accessor tool-parameters)
   (runner
    :initarg :runner
    :accessor tool-runner)
   (renderer
    :initarg :renderer
    :initform nil
    :accessor tool-renderer)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor tool-metadata)))

(defclass tool-result ()
  ((content
    :initarg :content
    :initform '()
    :reader tool-result-content)
   (details
    :initarg :details
    :initform nil
    :reader tool-result-details)
   (error-p
    :initarg :error-p
    :initform nil
    :reader tool-result-error-p)))

(defclass tool-contribution (contribution)
  ((tool
    :initarg :tool
    :reader contribution-tool)))

(defun make-live-object-contribution (&key name object source)
  (make-instance 'live-object-contribution
                 :kind :live-object
                 :name name
                 :object object
                 :source source))

(defun normalize-provider-entries (entries)
  (loop for (name value) on entries by #'cddr
        append (list (normalize-extension-id name) value)))

(defun make-provider (&key id capability contracts entries)
  (make-instance 'provider
                 :id (normalize-extension-id id)
                 :capability (normalize-extension-id capability)
                 :contracts (mapcar #'normalize-extension-id contracts)
                 :entries (normalize-provider-entries entries)))

(defun make-provider-contract (&key id capability required-entries validator)
  (make-instance 'provider-contract
                 :id (normalize-extension-id id)
                 :capability (normalize-extension-id capability)
                 :required-entries (mapcar #'normalize-extension-id
                                           required-entries)
                 :validator validator))

(defun make-provider-contract-contribution (&key name contract source)
  (make-instance 'provider-contract-contribution
                 :kind :contract
                 :name (normalize-extension-id name)
                 :contract contract
                 :source source))

(defun make-capability-contribution (&key name provider source)
  (make-instance 'capability-contribution
                 :kind :capability
                 :name (normalize-extension-id name)
                 :provider provider
                 :source source))

(defun make-effect-contribution (&key name installer retractor source)
  (unless installer
    (error "Effect contribution ~S requires an installer." name))
  (unless retractor
    (error "Effect contribution ~S requires a retractor. ~@
            Use a no-op lambda explicitly if the effect needs no cleanup."
           name))
  (make-instance 'effect-contribution
                 :kind :effect
                 :name (normalize-extension-id name)
                 :installer installer
                 :retractor retractor
                 :source source))

(defun tool-display-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))))

(defun make-tool (&key id name label description parameters runner renderer
                    metadata)
  (let* ((id (normalize-extension-id (or id name)))
         (name (tool-display-name (or name id))))
    (make-instance 'tool
                   :id id
                   :name name
                   :label (or label name)
                   :description (or description "")
                   :parameters parameters
                   :runner (or runner
                               (lambda (tool params context &key call-id
                                                               on-update)
                                 (declare (ignore tool params context
                                                  call-id on-update))
                                 (make-tool-result
                                  :content
                                  (list (make-tool-text-content
                                         "Tool has no runner."))
                                  :error-p t)))
                   :renderer renderer
                   :metadata metadata)))

(defun make-tool-result (&key content details error-p)
  (make-instance 'tool-result
                 :content content
                 :details details
                 :error-p error-p))

(defun make-tool-text-content (text)
  (list :type :text :text (princ-to-string text)))

(defun normalize-tool-result (result)
  (cond
    ((typep result 'tool-result)
     result)
    ((stringp result)
     (make-tool-result :content (list (make-tool-text-content result))))
    (t
     (make-tool-result :content (list (make-tool-text-content
                                       (prin1-to-string result)))
                       :details result))))

(defun make-tool-contribution (&key name tool source)
  (make-instance 'tool-contribution
                 :kind :tool
                 :name (normalize-extension-id name)
                 :tool tool
                 :source source))

(defun tool-parameter (parameters name &optional default)
  (let* ((keyword (normalize-extension-id name))
         (string (string-downcase (symbol-name keyword)))
         (marker (list :missing-parameter)))
    (cond
      ((hash-table-p parameters)
       (multiple-value-bind (value present-p)
           (gethash keyword parameters)
         (if present-p
             value
             (multiple-value-bind (value present-p)
                 (gethash string parameters)
               (if present-p value default)))))
      ((listp parameters)
       (let ((value (getf parameters keyword marker)))
         (cond
           ((not (eq value marker))
            value)
           ((not (eq (getf parameters string marker) marker))
            (getf parameters string))
           (t
            default))))
      (t default))))

(defun required-tool-parameter (parameters name)
  (let ((missing (list :missing-parameter)))
    (let ((value (tool-parameter parameters name missing)))
      (if (eq value missing)
          (error "Tool parameter ~S is required." name)
          value))))

(defun tool-text-result (text &key details error-p)
  (make-tool-result
   :content (list (make-tool-text-content text))
   :details details
   :error-p error-p))

(defmethod smoke-test-protocol ((protocol extension-protocol) context)
  (extension-contributions protocol
                           (make-extension :id :extension-protocol-smoke)
                           context)
  t)

(defmethod extension-contributions ((protocol extension-protocol)
                                    (extension extension)
                                    context)
  (declare (ignore protocol context))
  (copy-list (extension-contribution-list extension)))

(defun find-capabilities (protocol name)
  (copy-list (gethash (normalize-extension-id name)
                      (protocol-capabilities protocol))))

(defun find-provider-contract (protocol name)
  (gethash (normalize-extension-id name)
           (protocol-provider-contracts protocol)))

(defun find-tool (protocol name)
  (let ((contribution (first (gethash (normalize-extension-id name)
                                      (protocol-tools protocol)))))
    (and contribution (contribution-tool contribution))))

(defun list-tools (protocol)
  (let ((tools '()))
    (maphash (lambda (_ contributions)
               (declare (ignore _))
               (when contributions
                 (push (contribution-tool (first contributions)) tools)))
             (protocol-tools protocol))
    (nreverse tools)))

(defun list-tool-contributions (protocol)
  "The active tool-contribution per tool name. Unlike `list-tools` it keeps the
contribution, so callers can read each tool's origin via `contribution-extension`."
  (let ((contributions '()))
    (maphash (lambda (_ cs)
               (declare (ignore _))
               (when cs (push (first cs) contributions)))
             (protocol-tools protocol))
    (nreverse contributions)))

(defun list-tools-from (protocol extension-ids)
  "Tools contributed by an extension in EXTENSION-IDS, scoped by contribution
provenance. EXTENSION-IDS is one id or a list; each is normalized to a keyword."
  (let* ((normalized (normalize-extension-id extension-ids))
         (ids (if (listp normalized) normalized (list normalized))))
    (loop for contribution in (list-tool-contributions protocol)
          when (member (contribution-extension contribution) ids)
            collect (contribution-tool contribution))))

(defun maybe-emit-tool-event (protocol context type payload source)
  (let ((events (find-capability-provider protocol
                                          :events
                                          :contract :events/v1)))
    (when events
      (provider-call events
                     :emit-event
                     context
                     (provider-call events
                                    :make-event
                                    type
                                    :payload payload
                                    :source source)))))

(defun tool-error-result (condition)
  ;; :details must hold durable-log leaves (keyword/string/integer), so the
  ;; condition type is stringified rather than passed as a bare symbol.
  (make-tool-result
   :content (list (make-tool-text-content
                   (format nil "~A" condition)))
   :details (list :condition-type (princ-to-string (type-of condition)))
   :error-p t))

(defvar *tool-abort-predicate* nil
  "Function of no arguments, or NIL. When bound around invoke-tool, a true
return means the caller wants the running tool call to stop. Long-running
tool runners poll it through tool-abort-requested-p inside their wait and
walk loops and return early with an error result naming the abort. The
predicate rides a dynamic binding rather than a runner keyword so existing
runner lambda lists keep working unchanged.")

(defun tool-abort-requested-p ()
  "True when the current tool call should stop early. Always NIL outside a
binding of *tool-abort-predicate*, so direct tool invocations are
unaffected."
  (and *tool-abort-predicate*
       (and (funcall *tool-abort-predicate*) t)))

(defun mediate-tool-call (tool parameters)
  "Authorize a call to TOOL against *call-subject* before it reaches the runner.
A tool's :coordinate is its argument-aware demand: the atom under the
constraint(s) the live PARAMETERS place, which may fan out to several requests.
:capabilities is the supply-side authority a surface is built from; a coordinate
is the complete caller-side demand for its atom, so that atom is not re-demanded
unconstrained here. Atoms a coordinate does not name place no constraint and are
demanded as-is, so a coordinate-less tool gates exactly as before."
  (let* ((coordinate (getf (tool-metadata tool) :coordinate))
         (coordinate-atom (and coordinate (getf coordinate :atom))))
    (when coordinate
      (dolist (request (coordinate-requests coordinate parameters))
        (require-capability request)))
    (dolist (capability (getf (tool-metadata tool) :capabilities))
      (unless (eq capability coordinate-atom)
        (require-capability capability))))
  t)

(defgeneric invoke-tool (protocol tool-name parameters context
                         &key call-id on-update))

(defmethod invoke-tool ((protocol extension-protocol)
                        tool-name
                        parameters
                        context
                        &key call-id on-update)
  (let ((tool (or (and (typep tool-name 'tool) tool-name)
                  (find-tool protocol tool-name))))
    (unless tool
      (error "No tool named ~S." tool-name))
    (mediate-tool-call tool parameters)
    (let ((call-id (or call-id (gensym "TOOL-CALL-"))))
      (maybe-emit-tool-event protocol
                             context
                             :tool/call
                             (list :tool (tool-name tool)
                                   :call-id call-id
                                   :parameters parameters)
                             (object-id tool))
      (let ((result
              (handler-case
                  (normalize-tool-result
                   (funcall (tool-runner tool)
                            tool
                            parameters
                            context
                            :call-id call-id
                            :on-update on-update))
                (error (condition)
                  (tool-error-result condition)))))
        (maybe-emit-tool-event protocol
                               context
                               :tool/result
                               (list :tool (tool-name tool)
                                     :call-id call-id
                                     :result result
                                     :error-p (tool-result-error-p result))
                               (object-id tool))
        result))))

(defun recode-tool (tool &key runner label description parameters renderer
                           metadata)
  (when runner
    (setf (tool-runner tool) runner))
  (when label
    (setf (tool-label tool) label))
  (when description
    (setf (tool-description tool) description))
  (when parameters
    (setf (tool-parameters tool) parameters))
  (when renderer
    (setf (tool-renderer tool) renderer))
  (when metadata
    (setf (tool-metadata tool) metadata))
  tool)

(defun provider-entry-present-p (provider name)
  (let ((marker (list :missing-provider-entry))
        (name (normalize-extension-id name)))
    (cond
      ((typep provider 'provider)
       (not (eq (getf (provider-entries provider) name marker) marker)))
      ((hash-table-p provider)
       (multiple-value-bind (_ present-p) (gethash name provider)
         (declare (ignore _))
         present-p))
      ((listp provider)
       (not (eq (getf provider name marker) marker)))
      (t nil))))

(defun provider-contract-object (protocol contract)
  (etypecase contract
    (provider-contract contract)
    ((or keyword symbol string)
     (or (find-provider-contract protocol contract)
         (error "Unknown provider contract: ~S" contract)))))

(defun provider-satisfies-contract-p (protocol provider contract)
  (let ((contract (provider-contract-object protocol contract)))
    (and (eq (provider-capability provider)
             (contract-capability contract))
         (member (object-id contract)
                 (provider-contract-list provider)
                 :test #'equal)
         (every (lambda (entry)
                  (provider-entry-present-p provider entry))
                (contract-required-entries contract))
         (or (null (contract-validator contract))
             (funcall (contract-validator contract) provider protocol)))))

(defun validate-provider-contracts (protocol provider)
  (dolist (contract (provider-contract-list provider))
    (unless (provider-satisfies-contract-p protocol provider contract)
      (error "Provider ~S does not satisfy contract ~S."
             (object-id provider)
             contract)))
  provider)

(defun find-capability-providers (protocol name &key contract provider-id selector)
  (loop for contribution in (find-capabilities protocol name)
        for provider = (contribution-provider contribution)
        when (and provider
                  (typep provider 'provider)
                  (or (null provider-id)
                      (equal (normalize-extension-id provider-id)
                             (object-id provider)))
                  (or (null contract)
                      (provider-satisfies-contract-p protocol
                                                     provider
                                                     contract))
                  (or (null selector)
                      (funcall selector provider)))
          collect provider))

(defun find-capability-provider (protocol name &key contract provider-id selector)
  (let ((providers (find-capability-providers protocol
                                              name
                                              :contract contract
                                              :provider-id provider-id
                                              :selector selector)))
    (case (length providers)
      (0 nil)
      (1 (first providers))
      (otherwise
       (error "Multiple providers match capability ~S: ~S"
              name
              (mapcar #'object-id providers))))))

(defun require-capability-provider (protocol name &key contract provider-id selector)
  (or (find-capability-provider protocol
                                name
                                :contract contract
                                :provider-id provider-id
                                :selector selector)
      (error "No provider matches capability ~S." name)))

(defun provider-ref (provider name &optional (errorp t))
  (let ((marker (list :missing-provider-value))
        (name (normalize-extension-id name)))
    (cond
      ((typep provider 'provider)
       (provider-ref (provider-entries provider) name errorp))
      ((hash-table-p provider)
       (multiple-value-bind (value present-p) (gethash name provider)
         (if present-p
             value
             (when errorp
               (error "Provider has no value named ~S: ~S" name provider)))))
      ((listp provider)
       (let ((value (getf provider name marker)))
         (if (eq value marker)
             (when errorp
               (error "Provider has no value named ~S: ~S" name provider))
             value)))
      (t
       (when errorp
         (error "Unsupported provider object: ~S" provider))))))

(defun provider-call (provider name &rest arguments)
  "Dispatch NAME on PROVIDER behind a transparent supervision point:
conditions propagate to the caller's handlers untouched, the dispatch is
breadcrumbed for owner fault logs, and USE-VALUE can repair the result in
place. Lookup errors signal outside the dispatch scope."
  (let ((function (provider-ref provider name)))
    (with-supervised-dispatch (:provider provider :name name)
      (apply (if (and (symbolp function)
                      (fboundp function))
                 (symbol-function function)
                 function)
             arguments))))

(defun capability-provided-p (protocol name)
  (not (null (find-capabilities protocol name))))

(defun extension-loaded-p (protocol name)
  (not (null (gethash (normalize-extension-id name)
                      (protocol-extensions protocol)))))

(defmethod requirement-satisfied-p ((protocol extension-protocol)
                                    (requirement requirement)
                                    context)
  (case (requirement-kind requirement)
    (:capability
     (if (or (requirement-contract requirement)
             (requirement-provider-id requirement))
         (not (null (find-capability-provider
                     protocol
                     (requirement-name requirement)
                     :contract (requirement-contract requirement)
                     :provider-id (requirement-provider-id requirement))))
         (capability-provided-p protocol (requirement-name requirement))))
    (:provider
     (not (null (find-capability-provider
                 protocol
                 (requirement-name requirement)
                 :contract (requirement-contract requirement)
                 :provider-id (requirement-provider-id requirement)))))
    (:extension
     (extension-loaded-p protocol (requirement-name requirement)))
    (:live-object
     (not (null (find-live-object (context-registry context)
                                  (requirement-name requirement)))))
    (:tool
     (not (null (find-tool protocol (requirement-name requirement)))))
    (otherwise
     nil)))

(defun unsatisfied-extension-requirements (protocol extension context)
  (remove-if (lambda (requirement)
               (requirement-satisfied-p protocol requirement context))
             (extension-requirement-list extension)))

(defun requirement-designator (requirement)
  "Readable (kind name [:contract c] [:provider-id p]) list for error reports."
  (remove nil
          (list (requirement-kind requirement)
                (requirement-name requirement)
                (when (requirement-contract requirement)
                  :contract)
                (requirement-contract requirement)
                (when (requirement-provider-id requirement)
                  :provider-id)
                (requirement-provider-id requirement))))

(defun check-extension-requirements (protocol extension context)
  (let ((missing (unsatisfied-extension-requirements protocol extension context)))
    (when missing
      (error "Extension ~S has unsatisfied requirements: ~{~S~^, ~}"
             (object-id extension)
             (mapcar #'requirement-designator missing)))))

(defmethod protocol-load-extension ((protocol extension-protocol)
                                    (extension extension)
                                    context)
  (activate-extension protocol extension context))

(defmethod check-contribution-precondition (protocol contribution context)
  (declare (ignore protocol contribution context))
  nil)

(defmethod activate-extension ((protocol extension-protocol)
                               (extension extension)
                               context)
  (require-capability :manifest/install)
  (check-extension-requirements protocol extension context)
  (let ((installed '())
        (root-p (null *activating-extension*))
        (noted-p nil)
        (*activating-extension* (object-id extension)))
    (let ((contributions (extension-contributions protocol extension context)))
      ;; Validate every contribution before mutating, so a failing precondition
      ;; leaves no partial install rather than relying on a best-effort unwind.
      (dolist (contribution contributions)
        (check-contribution-precondition protocol contribution context))
      (handler-case
          (progn
            (register-live-object (context-registry context) extension)
            (setf (gethash (object-id extension) (protocol-extensions protocol))
                  extension)
            (when root-p
              (note-root-activation protocol (object-id extension))
              (setf noted-p t))
            (dolist (contribution contributions)
              (setf (contribution-extension contribution) (object-id extension))
              (push (install-contribution protocol contribution context)
                    installed))
            extension)
        (error (condition)
          (dolist (contribution installed)
            (ignore-errors
              (retract-contribution protocol contribution context)))
          (when noted-p
            (forget-root-activation protocol (object-id extension)))
          (remhash (object-id extension) (protocol-extensions protocol))
          (remove-live-object (context-registry context) (object-id extension))
          (error condition))))))

(defun installed-contributions-for-extension (protocol extension)
  (remove-if-not (lambda (contribution)
                   (equal (contribution-extension contribution)
                          (object-id extension)))
                 (protocol-installed-contributions protocol)))

(defmethod deactivate-extension ((protocol extension-protocol)
                                 (extension extension)
                                 context)
  (require-capability :manifest/retract)
  ;; Best-effort teardown: one failing retractor must not strand the rest, the
  ;; same way activation rolls back its installed contributions on error.
  (dolist (contribution (installed-contributions-for-extension protocol
                                                               extension))
    (ignore-errors
      (retract-contribution protocol contribution context)))
  (forget-root-activation protocol (object-id extension))
  (remhash (object-id extension) (protocol-extensions protocol))
  (remove-live-object (context-registry context) (object-id extension))
  extension)

(defmethod recode-extension ((protocol extension-protocol)
                             (extension extension)
                             new-source
                             context)
  (require-capability :image/recode)
  (deactivate-extension protocol extension context)
  (handler-case
      (activate-extension protocol new-source context)
    (error (condition)
      (activate-extension protocol extension context)
      (error condition))))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution live-object-contribution)
                                 context)
  (let ((object (contribution-object contribution)))
    (when (and (typep object 'live-object)
               (null (object-protocol object)))
      (setf (slot-value object 'protocol) protocol))
    (register-live-object (context-registry context) object))
  (push contribution (protocol-installed-contributions protocol))
  contribution)

(defmethod check-contribution-precondition ((protocol extension-protocol)
                                            (contribution capability-contribution)
                                            context)
  (declare (ignore protocol context))
  (when (typep (contribution-provider contribution) 'provider)
    (unless (eq (provider-capability (contribution-provider contribution))
                (contribution-name contribution))
      (error "Provider ~S has capability ~S, not ~S."
             (object-id (contribution-provider contribution))
             (provider-capability (contribution-provider contribution))
             (contribution-name contribution)))))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution capability-contribution)
                                 context)
  (check-contribution-precondition protocol contribution context)
  ;; Contract satisfaction is checked here, not as a precondition: it depends on
  ;; the provider-contract contributions installed earlier in this activation.
  (when (typep (contribution-provider contribution) 'provider)
    (validate-provider-contracts protocol (contribution-provider contribution)))
  (push contribution
        (gethash (contribution-name contribution)
                 (protocol-capabilities protocol)))
  (push contribution (protocol-installed-contributions protocol))
  contribution)

(defmethod check-contribution-precondition ((protocol extension-protocol)
                                            (contribution provider-contract-contribution)
                                            context)
  (declare (ignore protocol context))
  (let ((contract (contribution-contract contribution)))
    (unless (typep contract 'provider-contract)
      (error "Not a provider contract: ~S" contract))
    (unless (equal (object-id contract)
                   (contribution-name contribution))
      (error "Provider contract contribution name ~S does not match contract id ~S."
             (contribution-name contribution)
             (object-id contract)))))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution provider-contract-contribution)
                                 context)
  (check-contribution-precondition protocol contribution context)
  (let ((contract (contribution-contract contribution)))
    (setf (gethash (object-id contract)
                   (protocol-provider-contracts protocol))
          contract)
    (push contribution (protocol-installed-contributions protocol))
    contribution))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution effect-contribution)
                                 context)
  (setf (contribution-state contribution)
        (funcall (contribution-installer contribution)
                 protocol
                 contribution
                 context))
  (push contribution (protocol-installed-contributions protocol))
  contribution)

(defmethod check-contribution-precondition ((protocol extension-protocol)
                                            (contribution tool-contribution)
                                            context)
  (declare (ignore protocol context))
  (unless (typep (contribution-tool contribution) 'tool)
    (error "Not a tool: ~S" (contribution-tool contribution))))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution tool-contribution)
                                 context)
  (check-contribution-precondition protocol contribution context)
  (let ((tool (contribution-tool contribution)))
    (register-live-object (context-registry context) tool)
    (push contribution
          (gethash (contribution-name contribution)
                   (protocol-tools protocol)))
    (push contribution (protocol-installed-contributions protocol))
    contribution))

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution live-object-contribution)
                                 context)
  (remove-live-object (context-registry context)
                      (object-id (contribution-object contribution)))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution capability-contribution)
                                 context)
  (declare (ignore context))
  (let* ((name (contribution-name contribution))
         (remaining (remove contribution (gethash name
                                                  (protocol-capabilities protocol)))))
    (if remaining
        (setf (gethash name (protocol-capabilities protocol)) remaining)
        (remhash name (protocol-capabilities protocol))))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution provider-contract-contribution)
                                 context)
  (declare (ignore context))
  (let ((contract (contribution-contract contribution)))
    (when (eq contract
              (gethash (object-id contract)
                       (protocol-provider-contracts protocol)))
      (remhash (object-id contract)
               (protocol-provider-contracts protocol))))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution effect-contribution)
                                 context)
  (funcall (contribution-retractor contribution)
           protocol
           contribution
           context)
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution tool-contribution)
                                 context)
  (remove-live-object (context-registry context)
                      (object-id (contribution-tool contribution)))
  (let* ((name (contribution-name contribution))
         (remaining (remove contribution
                            (gethash name (protocol-tools protocol)))))
    (if remaining
        (setf (gethash name (protocol-tools protocol)) remaining)
        (remhash name (protocol-tools protocol))))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defclass method-contribution (contribution)
  ((gf-name
    :initarg :gf-name
    :reader contribution-gf-name)
   (qualifiers
    :initarg :qualifiers
    :initform '()
    :reader contribution-qualifiers)
   (specializer-names
    :initarg :specializer-names
    :reader contribution-specializer-names)
   (lambda-list
    :initarg :lambda-list
    :reader contribution-lambda-list)
   (body
    :initarg :body
    :reader contribution-body)))

(defun synthesize-method-contribution-name (gf-name qualifiers specializer-names)
  (intern (with-output-to-string (out)
            (format out "~A" gf-name)
            (dolist (q qualifiers) (format out "/~A" q))
            (format out "/")
            (format out "~{~A~^+~}" specializer-names))
          :keyword))

(defun make-method-contribution (&key name gf-name qualifiers specializer-names
                                   lambda-list body source)
  (make-instance 'method-contribution
                 :kind :method
                 :name (or (and name (normalize-extension-id name))
                           (synthesize-method-contribution-name
                            gf-name qualifiers specializer-names))
                 :gf-name gf-name
                 :qualifiers qualifiers
                 :specializer-names specializer-names
                 :lambda-list lambda-list
                 :body body
                 :source source))

(defvar *method-installations*
  (make-hash-table :test #'equal)
  "Per-key installation stack keyed on (gf-name qualifiers specializer-names).
Value is a newest-first list of frames (contribution refcount method); the top
frame's body is the live method. CLOS allows only one method per key, so a second
contribution with the same key but a different body warns and stacks on top;
retracting it reinstalls the surviving frame's body, leaving the gf intact.")

(defun method-installation-key (gf-name qualifiers specializer-names)
  (list gf-name qualifiers specializer-names))

(defun specialize-method-lambda-list (lambda-list specializer-names)
  "Zip SPECIALIZER-NAMES onto the required params of LAMBDA-LIST (T stays bare),
passing the lambda-list-keyword tail through verbatim."
  (let ((out '())
        (specs specializer-names)
        (tail lambda-list))
    (loop
      (when (or (null tail) (null specs)
                (member (car tail) lambda-list-keywords))
        (return))
      (let ((s (pop specs)))
        (push (if (eq s t) (car tail) (list (car tail) s)) out)
        (pop tail)))
    (append (nreverse out) tail)))

(defun install-method-via-defmethod (contribution)
  "Install a :method contribution as a real DEFMETHOD so SBCL wires
call-next-method / next-method-p / qualifier combination. Returns the method."
  (eval `(defmethod ,(contribution-gf-name contribution)
           ,@(contribution-qualifiers contribution)
           ,(specialize-method-lambda-list
             (contribution-lambda-list contribution)
             (contribution-specializer-names contribution))
           ,@(contribution-body contribution))))

(defun ensure-method-installed (contribution)
  (let* ((gf-name (contribution-gf-name contribution))
         (qualifiers (contribution-qualifiers contribution))
         (specializer-names (contribution-specializer-names contribution))
         (key (method-installation-key gf-name qualifiers specializer-names))
         (entry (gethash key *method-installations*)))
    (cond
      ((null entry)
       (let ((method (install-method-via-defmethod contribution)))
         (setf (gethash key *method-installations*)
               (list (list contribution 1 method)))
         method))
      ((equal (contribution-body (first (first entry)))
              (contribution-body contribution))
       (incf (second (first entry)))
       (third (first entry)))
      (t
       (warn "Method ~S already installed with a different body; the latest ~
              contribution's body shadows it until retracted." key)
       (let ((method (install-method-via-defmethod contribution)))
         (push (list contribution 1 method) (gethash key *method-installations*))
         method)))))

(defun release-method-installation (contribution)
  (let* ((gf-name (contribution-gf-name contribution))
         (key (method-installation-key gf-name
                                       (contribution-qualifiers contribution)
                                       (contribution-specializer-names contribution)))
         (entry (gethash key *method-installations*))
         (frame (and entry
                     (find (contribution-body contribution) entry
                           :key (lambda (f) (contribution-body (first f)))
                           :test #'equal))))
    (unless frame
      (error "No method installation recorded for ~S" key))
    (when (zerop (decf (second frame)))
      (let ((was-top (eq frame (first entry))))
        (setf (gethash key *method-installations*)
              (remove frame (gethash key *method-installations*)))
        (let ((remaining (gethash key *method-installations*)))
          (cond
            ((null remaining)
             (remove-method (fdefinition gf-name) (third frame))
             (remhash key *method-installations*))
            (was-top
             (setf (third (first remaining))
                   (install-method-via-defmethod (first (first remaining)))))))))
    nil))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution method-contribution)
                                 context)
  (declare (ignore context))
  (ensure-method-installed contribution)
  (push contribution (protocol-installed-contributions protocol))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution method-contribution)
                                 context)
  (declare (ignore context))
  (release-method-installation contribution)
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

;;; Authority as the install/retract morphism. The grant-set is the protocol's
;;; single authority registry -- a map from principal to grant, held in storage
;;; as a serializable datum so it rides snapshot/restore unchanged. Granting is
;;; an install, revoking a retract; the in-image guard and the snapshot read the
;;; one map. *call-subject* is its dynamic projection (see `principal-subject`),
;;; not a second authority.

(defparameter +grant-set-storage-key+ :kli/ext.grant-set
  "Storage key for the principal-to-grant authority map, held as a proper-list
datum ((principal grant-datum) ...) so snapshot storage serializes it whole.")

(defun protocol-grant-set (protocol)
  "PROTOCOL's authority map as the stored datum, or nil when none is recorded."
  (protocol-storage protocol +grant-set-storage-key+))

(defun grant-set-lookup (protocol principal)
  "The grant PROTOCOL records for PRINCIPAL, or the bottom grant when absent."
  (let ((entry (assoc principal (protocol-grant-set protocol))))
    (if entry
        (datum->grant (second entry))
        (grant-bottom))))

(defun grant-set-put (protocol principal grant)
  (setf (protocol-storage protocol +grant-set-storage-key+)
        (cons (list principal (grant->datum grant))
              (remove principal (protocol-grant-set protocol) :key #'first))))

(defun grant-set-remove (protocol principal)
  (setf (protocol-storage protocol +grant-set-storage-key+)
        (remove principal (protocol-grant-set protocol) :key #'first)))

(defun grant-set-has-p (protocol principal)
  "Non-nil when PROTOCOL's grant-set records an entry for PRINCIPAL."
  (and (assoc principal (protocol-grant-set protocol)) t))

(defun check-authority (protocol principal request)
  "Non-nil when PRINCIPAL's recorded grant covers REQUEST -- a capability
keyword or a grant. The single authority query over the grant-set."
  (grant-covers-p (grant-set-lookup protocol principal)
                  (capability-request request)))

(defun principal-subject (protocol principal)
  "A subject carrying PRINCIPAL's grant from PROTOCOL's grant-set. Bind
`*call-subject*` to it to run in-image under exactly the authority the
snapshot-able map records: the dynamic guard and the grant-set are one
lattice, not two."
  (make-subject :grant (grant-set-lookup protocol principal)))

(defclass grant-contribution (contribution)
  ((principal
    :initarg :principal
    :reader contribution-principal)
   (grant
    :initarg :grant
    :reader contribution-grant)))

(defun make-grant-contribution (&key name principal grant source)
  (let ((principal (normalize-extension-id principal)))
    (make-instance 'grant-contribution
                   :kind :grant
                   :name (or (and name (normalize-extension-id name)) principal)
                   :principal principal
                   :grant grant
                   :source source)))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution grant-contribution)
                                 context)
  (declare (ignore context))
  (grant-set-put protocol
                 (contribution-principal contribution)
                 (contribution-grant contribution))
  (push contribution (protocol-installed-contributions protocol))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution grant-contribution)
                                 context)
  (declare (ignore context))
  (grant-set-remove protocol (contribution-principal contribution))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

;;; Delegation. Conferring an attenuated grant from one principal to another is
;;; an install on the grant-set: confer rejects anything the parent does not
;;; cover (escalation unrepresentable), and the conferral lands as a reversible
;;; grant-contribution that snapshots with the rest of the map. Revoking is
;;; retracting that one contribution, which leaves siblings untouched -- the same
;;; remove-by-label property the named layer stack gives, with the contribution
;;; id as the label, so a conferral stays attributable to and drainable by its
;;; owner.

(defun delegation-owner-id (parent child)
  "The owning-extension id for the PARENT->CHILD delegation: a per-delegation
label so the conferral attributes to and drains by its own owner. Deactivating
an extension with this id retracts exactly this delegation, leaving siblings
untouched."
  (normalize-extension-id (list :kli/ext.delegation parent child)))

(defun delegate-grant (protocol parent child requested)
  "Confer REQUESTED from PARENT to CHILD on PROTOCOL's grant-set, attenuated to
PARENT's authority. Signals `grant-escalation` when PARENT does not cover
REQUESTED. The conferral lands as a reversible grant-contribution stamped with
its per-delegation owner id, so deactivating that owner drains exactly this
delegation. Returns CHILD."
  ;; Only attenuation is representable: confer rejects any grant PARENT does not
  ;; already hold, so an over-strong image delegation (debug where inspect was
  ;; meant) fails safe by the locked :image/* family order, not a runtime check.
  (let* ((sub (confer (grant-set-lookup protocol parent) requested))
         (contribution (make-grant-contribution :principal child :grant sub
                                                :source :delegation)))
    (setf (contribution-extension contribution)
          (delegation-owner-id parent child))
    (install-contribution protocol contribution nil)
    child))
