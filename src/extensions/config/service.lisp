(in-package #:kli/config)

(defclass config-service (live-object)
  ((global-dir
    :initarg :global-dir
    :reader config-service-global-dir)
   (project-dir
    :initarg :project-dir
    :reader config-service-project-dir)
   (file-settings
    :initarg :file-settings
    :accessor config-service-file-settings
    :documentation "The bare global < project file merge, kept so overlay
swaps and file reloads re-merge from a clean base.")
   (overlay
    :initarg :overlay
    :initform nil
    :reader config-service-overlay)
   (settings
    :initarg :settings
    :accessor config-service-settings)
   (resource-kinds
    :initform (make-hash-table :test #'eq)
    :reader config-service-resource-kinds)))

(defun make-config-service (&key (id :config-service)
                                 (global-dir (global-config-dir))
                                 (project-dir (project-config-dir)))
  (let ((file-settings (load-settings
                        :global-path (settings-path-in global-dir)
                        :project-path (settings-path-in project-dir))))
    (make-instance 'config-service
                   :id id
                   :global-dir global-dir
                   :project-dir project-dir
                   :file-settings file-settings
                   :settings file-settings)))

(defun find-config-service (context)
  (find-live-object (context-registry context) :config-service))

(defun remerge-service-settings (service)
  "Recompute the merged view global < project < overlay. Returns the table."
  (setf (config-service-settings service)
        (merge-settings (config-service-file-settings service)
                        (config-service-overlay service))))

(defun reload-settings (service)
  "Reread and remerge both settings files, keeping any overlay layer on top.
Returns the fresh table."
  (setf (config-service-file-settings service)
        (load-settings
         :global-path (settings-path-in (config-service-global-dir service))
         :project-path (settings-path-in (config-service-project-dir service))))
  (remerge-service-settings service))

(defun rebind-config-dirs (service &key (global-dir (global-config-dir))
                                        (project-dir (project-config-dir)))
  "Re-point SERVICE at the current environment's config dirs, then reload. The
dirs are captured when the service is constructed, so a service built in one
environment and reused in another (a config service baked into a dumped image and
reused at boot) must rebind before its files mean anything. Returns the fresh
merged table."
  (setf (slot-value service 'global-dir) global-dir
        (slot-value service 'project-dir) project-dir)
  (reload-settings service))

(defun set-settings-overlay (service overlay)
  "Install OVERLAY as the layer over the file merge, so the merged view reads
global < project < OVERLAY. NIL clears the layer. Returns the previous
overlay so a switch can restore it."
  (let ((previous (config-service-overlay service)))
    (setf (slot-value service 'overlay) overlay)
    (remerge-service-settings service)
    previous))

(defparameter +settings-overlay-key+ :kli/config/settings-overlay
  "Per-protocol storage key for a settings overlay recorded before the config
extension installs. The boot path records the selected profile's settings
here so the overlay is already merged when later extensions read settings.")

(defun record-settings-overlay (protocol overlay)
  "Record OVERLAY for the config extension to pick up at install. Returns
OVERLAY."
  (setf (protocol-storage protocol +settings-overlay-key+) overlay))

(defun protocol-settings-overlay (protocol)
  (protocol-storage protocol +settings-overlay-key+))

(defun install-recorded-overlay (protocol contribution context)
  "Effect installer: lift a recorded non-empty overlay onto the config
service the live-object contribution just registered."
  (declare (ignore contribution))
  (let ((overlay (protocol-settings-overlay protocol)))
    (when (and (hash-table-p overlay) (plusp (hash-table-count overlay)))
      (list :previous (set-settings-overlay (find-config-service context)
                                            overlay)
            :touched t))))

(defun revert-recorded-overlay (protocol contribution context)
  (declare (ignore protocol))
  (let ((state (contribution-state contribution)))
    (when (getf state :touched)
      (let ((service (find-config-service context)))
        (when service
          (set-settings-overlay service (getf state :previous)))))))

(defun register-resource-kind (service kind subdir)
  "Declare KIND discoverable under SUBDIR of each config directory, the hook
downstream resource types (prompts, skills, context files) register through.
Returns KIND."
  (setf (gethash kind (config-service-resource-kinds service))
        (uiop:ensure-directory-pathname subdir))
  kind)

(defun unregister-resource-kind (service kind)
  "Remove KIND from the resource-kinds table. No-op if absent. Returns KIND."
  (remhash kind (config-service-resource-kinds service))
  kind)

(defun resource-kind-subdir (service kind)
  (or (gethash kind (config-service-resource-kinds service))
      (error "Unknown resource kind: ~S" kind)))

(defun resource-paths (service kind &key (existing-only t))
  "Directories to search for KIND resources, global first so project entries
extend and shadow the global set. EXISTING-ONLY drops absent directories."
  (let* ((subdir (resource-kind-subdir service kind))
         (project (config-service-project-dir service))
         (dirs (append
                (list (merge-pathnames subdir
                                       (config-service-global-dir service)))
                (when project
                  (list (merge-pathnames subdir project))))))
    (if existing-only
        (loop for dir in dirs
              for existing = (uiop:directory-exists-p dir)
              when existing collect existing)
        dirs)))

(defvar *extension-resource-roots* '()
  "Alist of normalized extension id to a plist of bundled resource-root keys
by kind, e.g. (:prompts \"kli/<id>/prompts\" :skills \"kli/<id>/skills\"). An
extension declares its bundled roots here at load; discovery serves a root
only while that extension is loaded.")

(defun register-extension-resource-roots (id &key prompts skills)
  "Declare extension ID's bundled resource-root keys for prompts and/or
skills. Re-registering ID replaces its entry in place. Returns ID."
  (let* ((key (normalize-extension-id id))
         (entry (append (when prompts (list :prompts prompts))
                        (when skills (list :skills skills))))
         (cell (assoc key *extension-resource-roots* :test #'equal)))
    (if cell
        (setf (cdr cell) entry)
        (setf *extension-resource-roots*
              (append *extension-resource-roots* (list (cons key entry)))))
    id))

(defun installed-extension-resource-root-keys (protocol kind)
  "Resource-root keys declared for KIND by every currently-loaded extension,
in registration order. KIND is :prompts or :skills."
  (loop for (id . entry) in *extension-resource-roots*
        for root-key = (getf entry kind)
        when (and root-key (extension-loaded-p protocol id))
          collect root-key))

(defun installed-extension-resource-roots (protocol kind)
  "INSTALLED-EXTENSION-RESOURCE-ROOT-KEYS with each key paired with its
declaring extension id, so a resource can be attributed to its owner."
  (loop for (id . entry) in *extension-resource-roots*
        for root-key = (getf entry kind)
        when (and root-key (extension-loaded-p protocol id))
          collect (cons id root-key)))

(defun resource-kind-summary (service)
  (let ((kinds '()))
    (maphash (lambda (kind subdir) (push (cons kind subdir) kinds))
             (config-service-resource-kinds service))
    (sort kinds #'string< :key #'car)))

(defun config-summary (service)
  "Introspection plist over the service: directories, settings file paths
with their existence, the merged table, and registered resource kinds."
  (let ((global-path (settings-path-in (config-service-global-dir service)))
        (project-path (settings-path-in (config-service-project-dir service))))
    (list :global-dir (config-service-global-dir service)
          :global-settings-path global-path
          :global-settings-present (and global-path
                                        (uiop:file-exists-p global-path)
                                        t)
          :project-dir (config-service-project-dir service)
          :project-settings-path project-path
          :project-settings-present (and project-path
                                         (uiop:file-exists-p project-path)
                                         t)
          :settings (config-service-settings service)
          :resource-kinds (resource-kind-summary service))))
