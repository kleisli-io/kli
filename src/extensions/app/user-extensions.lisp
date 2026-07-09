(in-package #:kli/app)

(defparameter +user-config-dir+ "kli"
  "XDG subdirectory kli already owns for credentials and providers.")

(defparameter +extension-arg+ "--extension"
  "Command-line flag naming an extra extension file or directory to load.")

(defparameter +extension-marker+ "extension.lisp"
  "A directory holding this file is one extension unit, not a group.")

(defparameter +package-file+ "package.lisp"
  "When a unit directory holds this file, the author owns its package.")

(defparameter +available-extensions-key+ :kli/app.available-extensions
  "Storage key for the per-protocol registry of available user extensions.")

(defparameter +installed-user-handles-key+ :kli/app.installed-user-extensions
  "Storage key mapping an installed user extension's id to its retract handle.")

(defparameter +extension-diagnostics-key+ :kli/app.extension-diagnostics
  "Storage key for output captured while loading or installing user extensions.")

(defparameter +remote-install-pins-key+ :kli/app.remote-install-pins
  "Storage key for the install-set: the durable carrier mapping a
runtime-installed extension's string id to its snapshot-serializable pin. The
running image, the on-disk dir, and the snapshot are three projections of this
one value.")

(defparameter +restore-version-gaps-key+ :kli/app.restore-version-gaps
  "Storage key for restore-consistency gaps. A pin whose code is unavailable
on restore, or whose recorded version disagrees with the resolved entry,
records a gap here rather than crashing the restore or being silently
accepted.")

(defparameter +nix-shadowed-installs-key+ :kli/app.nix-shadowed-installs
  "Storage key for runtime installs skipped because a nix-declared baseline
already owns the id. The image owns presence, so a boot or restore install of a
baked id records a shadow here instead of double-installing.")

(defvar *extension-lifecycle-lock*
  (sb-thread:make-mutex :name "kli-extension-lifecycle"))

(defmacro with-extension-lifecycle-lock (&body body)
  `(sb-thread:with-mutex (*extension-lifecycle-lock*)
     ,@body))

(defun extension-diagnostics (protocol)
  "Captured (source . text) pairs from the most recent load pass, oldest first.
SOURCE is the unit's file or directory, the extension id, or :boot for stray
output outside any unit."
  (reverse (protocol-storage protocol +extension-diagnostics-key+)))

(defun clear-extension-diagnostics (protocol)
  (setf (protocol-storage protocol +extension-diagnostics-key+) nil))

(defun record-extension-diagnostic (protocol source text)
  (push (cons source text)
        (protocol-storage protocol +extension-diagnostics-key+)))

(defun replace-extension-diagnostics (protocol diagnostics)
  (setf (protocol-storage protocol +extension-diagnostics-key+)
        (reverse diagnostics)))

(defun extension-diagnostic-label (source)
  "Human-readable name for a diagnostic source: the file or directory name of
a unit, or the printed id."
  (if (pathnamep source)
      (let ((file (file-namestring source)))
        (if (and file (plusp (length file)))
            file
            (car (last (pathname-directory source)))))
      (string-downcase (princ-to-string source))))

(defun call-with-diagnostics-capture-to (record source thunk)
  "Run THUNK with both standard streams diverted into a capture. Any output is
recorded as a diagnostic for SOURCE instead of reaching the terminal, where
the TUI takeover would flash then wipe it (and a mid-session /reload would
corrupt the frame). Conditions still signal - only the printing is diverted -
so warning handlers observe them unchanged."
  (let ((capture (make-string-output-stream)))
    (unwind-protect
         (let ((*standard-output* capture)
               (*error-output* capture))
           (funcall thunk))
      (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (get-output-stream-string capture))))
        (when (plusp (length text))
          (funcall record source text))))))

(defun call-with-diagnostics-capture (protocol source thunk)
  (call-with-diagnostics-capture-to
   (lambda (source text)
     (record-extension-diagnostic protocol source text))
   source thunk))

(defstruct user-extension-entry
  id source manifest metadata)

(defun copy-extension-table (table)
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             table)
    copy))

(defun replace-extension-table (target source)
  (clrhash target)
  (maphash (lambda (key value)
             (setf (gethash key target) value))
           source)
  target)

(defun extension-table-keys (table)
  (let ((keys '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key keys))
             table)
    (sort keys #'string< :key #'princ-to-string)))

(defstruct user-extension-state-snapshot
  available
  installed-ids
  diagnostics)

(defun lisp-files-in (dir)
  (sort (directory (merge-pathnames "*.lisp" dir)) #'string< :key #'namestring))

(defun subdirs-in (dir)
  (sort (uiop:subdirectories dir) #'string< :key #'namestring))

(defun extension-marker-file (dir)
  "DIR's extension.lisp if present — the marker that DIR is one unit."
  (let ((marker (merge-pathnames +extension-marker+ dir)))
    (and (uiop:file-exists-p marker) marker)))

(defun dir-has-package-file (dir)
  (uiop:file-exists-p (merge-pathnames +package-file+ dir)))

(defun order-files (files)
  "package.lisp first, extension.lisp last, the rest alphabetical."
  (flet ((base (file) (file-namestring file)))
    (let ((package (find +package-file+ files :key #'base :test #'string=))
          (marker (find +extension-marker+ files :key #'base :test #'string=))
          (middle (sort (remove-if (lambda (file)
                                     (member (base file)
                                             (list +package-file+ +extension-marker+)
                                             :test #'string=))
                                   files)
                        #'string< :key #'namestring)))
      (append (and package (list package)) middle (and marker (list marker))))))

(defun dir-asd-manifest (dir)
  "DIR's sole .asd manifest, or NIL. Zero or several .asd files are not one
unambiguous system, so they fall through to the file-ordering convention."
  (let ((asds (directory (merge-pathnames "*.asd" dir))))
    (and (null (rest asds)) (first asds))))

(defun discover-units (dir)
  "Walk DIR into units. A unit is (:single FILE) or (:dir DIR ORDERED-FILES). A dir carrying a sole .asd manifest or an extension.lisp marker is one :dir unit over its top-level files and is never descended; the manifest wins when both are present so it roots the unit even when the marker is nested. An unmarked dir is a group whose loose files are :single units and whose subdirs recurse."
  (if (or (dir-asd-manifest dir) (extension-marker-file dir))
      (list (list :dir dir (order-files (lisp-files-in dir))))
      (append (mapcar (lambda (file) (list :single file)) (lisp-files-in dir))
              (loop for sub in (subdirs-in dir) append (discover-units sub)))))

(defun user-extension-dirs ()
  "Global XDG dir then project-local dir. Project files extend the global set."
  (remove-if-not
   #'uiop:directory-exists-p
   (list (uiop:xdg-config-home +user-config-dir+ "extensions/")
         (merge-pathnames ".kli/extensions/" (uiop:getcwd)))))

(defun user-extension-units (&optional (dirs (user-extension-dirs)))
  (loop for dir in dirs append (discover-units dir)))

(defun user-config-path ()
  (uiop:xdg-config-home +user-config-dir+ "config.json"))

(defun read-user-config (&optional (path (user-config-path)))
  "Parse the optional config.json into a plist (:enabled :disabled :extension-dirs). NIL when absent or malformed (fail-soft)."
  (when (uiop:file-exists-p path)
    (handler-case
        (let ((table (com.inuoe.jzon:parse path)))
          (flet ((strings (key)
                   (let ((value (gethash key table)))
                     (and value (coerce value 'list)))))
            (list :enabled (mapcar #'normalize-extension-id (strings "enabled"))
                  :disabled (mapcar #'normalize-extension-id (strings "disabled"))
                  :extension-dirs (mapcar #'uiop:parse-native-namestring
                                          (strings "extension-dirs")))))
      (error (condition)
        (warn "kli: ignoring config.json (~A)" condition)
        nil))))

(defun config-extension-units (config)
  (loop for dir in (getf config :extension-dirs)
        when (uiop:directory-exists-p dir)
          append (discover-units dir)))

(defun default-extension-units (config)
  (append (user-extension-units) (config-extension-units config)))

(defun available-extensions (protocol)
  (ensure-protocol-storage protocol +available-extensions-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun installed-user-handles (protocol)
  (ensure-protocol-storage protocol +installed-user-handles-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun snapshot-user-extension-state (protocol)
  (make-user-extension-state-snapshot
   :available (copy-extension-table (available-extensions protocol))
   :installed-ids (extension-table-keys (installed-user-handles protocol))
   :diagnostics (copy-list
                 (protocol-storage protocol +extension-diagnostics-key+))))

(defun unit-source (unit)
  "The file or directory naming UNIT, for bookkeeping and messages."
  (second unit))

(defun synth-package-name (dir)
  (format nil "KLI/USER-EXT/~:@(~A~)" (car (last (pathname-directory dir)))))

(defun ensure-synth-package (name)
  "Recreate NAME from scratch so a reload never carries stale symbols."
  (let ((existing (find-package name)))
    (when existing (delete-package existing)))
  (make-package name :use '(:cl :kli/author)))

(defun call-reporting-load-warnings (thunk)
  "Run THUNK (which loads unit source) reporting every load-time warning to
*error-output* itself - whether the compiler prints its diagnostics depends on
how the image was started (sbcl --script suppresses them), so the report
cannot be left to it. Each warning is muffled after reporting so environments
that do print do not duplicate it. Redefinition warnings are muffled without
report: a source unit legitimately installs its cross-file macros twice
(compile-time evaluation, then the fasl load), and a reload redefines every
definition by design."
  (handler-bind ((warning (lambda (condition)
                            (unless (typep condition 'sb-kernel:redefinition-warning)
                              (format *error-output* "~&~S: ~A~%"
                                      (type-of condition) condition))
                            (when (find-restart 'muffle-warning condition)
                              (muffle-warning condition)))))
    (funcall thunk)))

(defun extension-fasl-cache-root ()
  "Writable per-user XDG cache directory for compiled extension systems, so an
ASDF load never writes beside its source or into the read-only saved-core image."
  (uiop:xdg-cache-home +user-config-dir+ "extensions" :implementation))

(defun configure-extension-fasl-cache ()
  "Point ASDF's compilation output at the XDG extension cache and rebuild the
translation table, so every extension system compiles into a writable per-user
directory regardless of inherited ASDF configuration. Returns the cache root."
  (let ((root (extension-fasl-cache-root)))
    (ensure-directories-exist root)
    (setf asdf:*user-cache* root)
    (asdf:initialize-output-translations
     '(:output-translations :enable-user-cache :ignore-inherited-configuration))
    root))

(defun asd-system-name (asd)
  "The primary system name an .asd defines, by ASDF's file-name convention."
  (pathname-name asd))

(defun load-dir-via-asd (dir asd files)
  "Load DIR as an ASDF system: route fasls to the XDG cache, register ASD, then
load the system it names. The .asd's :components own load order, so honoring a
declared dependency order is the point — FILES, the convention ordering, is
unused."
  (declare (ignore dir files))
  (configure-extension-fasl-cache)
  (let ((*compile-verbose* nil) (*compile-print* nil)
        (*load-verbose* nil) (*load-print* nil))
    (asdf:load-asd asd)
    (asdf:load-system (asd-system-name asd) :verbose nil)))

(defun load-dir-by-convention (dir files)
  "Load DIR's FILES under the package convention: honor its own package.lisp if
present, else a synthesized per-unit package so cross-file definitions resolve."
  (if (dir-has-package-file dir)
      (dolist (file files) (load file))
      (let ((*package* (ensure-synth-package (synth-package-name dir))))
        (dolist (file files) (load file)))))

(defparameter +resource-manifest-file+ "resources.sexp"
  "Unit-root manifest mapping resource-root keys to unit-relative directories.
A source-distributed bundle carries this generated file in place of the
resource registrations a compiled build bakes into its fasls.")

(defun register-unit-resource-roots (dir)
  "Register each (KEY . RELATIVE-DIR) entry of DIR's resource manifest, so the
resource-root keys the unit's extensions declare resolve against the placed
tree. Fail-soft at every level: an unreadable manifest or an entry that is not
a pair of strings naming a relative directory contributes nothing, leaving
those keys unresolved -- the same state as carrying no manifest."
  (let ((manifest (merge-pathnames +resource-manifest-file+ dir)))
    (when (uiop:file-exists-p manifest)
      (dolist (entry (ignore-errors (uiop:safe-read-file-form manifest)))
        (when (and (consp entry) (stringp (car entry)) (stringp (cdr entry)))
          (let ((root (ignore-errors
                       (uiop:ensure-directory-pathname
                        (uiop:subpathname dir (cdr entry))))))
            (when root
              (buildlisp/resources:register-resource-root (car entry) root))))))))

(defstruct (manifest-loader (:constructor make-manifest-loader (kind detect load)))
  "A directory-unit manifest strategy. KIND names it; DETECT maps a directory to
a manifest value or NIL; LOAD loads the unit given (dir manifest files)."
  kind detect load)

(defparameter *manifest-loaders*
  (list (make-manifest-loader :asd #'dir-asd-manifest #'load-dir-via-asd))
  "Directory-unit manifest strategies, most specific first. The first whose
detector matches a unit directory governs its load; a directory matching none
loads by the file-ordering convention. New manifest kinds prepend here.")

(defun dir-manifest-loader (dir)
  "The first registered strategy whose detector matches DIR, with its detected
manifest value; (values nil nil) when none applies and the convention governs."
  (dolist (loader *manifest-loaders* (values nil nil))
    (let ((manifest (funcall (manifest-loader-detect loader) dir)))
      (when manifest
        (return (values loader manifest))))))

(defun load-files-for-unit (unit)
  "Load UNIT's source under its package regime. A single file loads in kli/author. A directory first registers any bundled resource roots its manifest declares, then dispatches on its manifest kind — an .asd loads via ASDF, honoring its declared component order; otherwise it honors its own package.lisp if present, else loads under a synthesized per-unit package so cross-file definitions resolve."
  (kli/ext:require-capability :image/eval)
  (call-reporting-load-warnings
   (lambda ()
     (ecase (first unit)
       (:single
        (let ((*package* (find-package :kli/author)))
          (load (second unit))))
       (:dir
        (destructuring-bind (dir files) (rest unit)
          (register-unit-resource-roots dir)
          (multiple-value-bind (loader manifest) (dir-manifest-loader dir)
            (if loader
                (funcall (manifest-loader-load loader) dir manifest files)
                (load-dir-by-convention dir files)))))))))

(defun index-unit-into (unit available record-diagnostic)
  "Load UNIT as a manifest (no install) and register it in AVAILABLE. Returns
the entry, or NIL on failure. Compiler output and the failure warning are
captured through RECORD-DIAGNOSTIC."
  (call-with-diagnostics-capture-to
   record-diagnostic (unit-source unit)
   (lambda ()
     (handler-case
         (let* ((manifest (call-with-manifest-capture
                           (lambda () (load-files-for-unit unit))))
                (probe (funcall manifest))
                (id (object-id probe))
                (entry (make-user-extension-entry
                        :id id :source (unit-source unit) :manifest manifest
                        :metadata (extension-metadata probe))))
           (setf (gethash id available) entry)
           entry)
       (error (condition)
         (warn "kli: extension ~A failed to load: ~A" (unit-source unit) condition)
         nil)))))

(defun index-unit (protocol unit)
  "Load UNIT as a manifest (no install) and register it as available. Returns
the entry, or NIL on failure — reported and isolated. Compiler output and the
failure warning are captured as a diagnostic for the unit rather than printed."
  (index-unit-into
   unit
   (available-extensions protocol)
   (lambda (source text)
     (record-extension-diagnostic protocol source text))))

(defun stage-user-extension-units (units)
  (let ((available (make-hash-table :test #'equal))
        (diagnostics '())
        (failed nil))
    (flet ((record (source text)
             (push (cons source text) diagnostics)))
      (dolist (unit units)
        (unless (index-unit-into unit available #'record)
          (setf failed t))))
    (values available (nreverse diagnostics) failed)))

(defun metadata-autoload (metadata)
  "The :autoload value in METADATA, or :default when unspecified."
  (let ((marker (list :unset)))
    (let ((value (getf metadata :autoload marker)))
      (if (eq value marker) :default value))))

(defun extension-enabled-p (entry config &optional profile)
  "Precedence: PROFILE's disable, then PROFILE's enable, then config
:disabled, then config :enabled, then per-extension :autoload metadata, then
enabled by default. PROFILE is the boot-resolved profile (or NIL), whose
sparse deltas take the first word without replacing the config layer."
  (let ((id (user-extension-entry-id entry)))
    (cond
      ((and profile (member id (resolved-profile-disable profile))) nil)
      ((and profile (member id (resolved-profile-enable profile))) t)
      ((member id (getf config :disabled) :test #'equal) nil)
      ((member id (getf config :enabled) :test #'equal) t)
      (t (let ((autoload (metadata-autoload (user-extension-entry-metadata entry))))
           (if (eq autoload :default) t autoload))))))

(defun install-user-extension (protocol entry context)
  (let ((handle (install-manifest (user-extension-entry-manifest entry)
                                  protocol context)))
    (setf (gethash (user-extension-entry-id entry)
                   (installed-user-handles protocol))
          handle)
    ;; The prompt-templates effect scans extension prompt roots at its own
    ;; install; an extension arriving later must trigger a re-scan or its
    ;; bundled prompts never become commands.
    (kli/prompts:refresh-prompt-template-commands context)
    handle))

(defun retract-user-extension (protocol id context)
  (let ((handle (gethash id (installed-user-handles protocol))))
    (when handle
      (retract-manifest handle protocol context)
      (remhash id (installed-user-handles protocol))
      (kli/prompts:refresh-prompt-template-commands context)
      t)))

(defun restore-user-extension-state (protocol context snapshot)
  (dolist (id (extension-table-keys (installed-user-handles protocol)))
    (ignore-errors
      (retract-user-extension protocol id context)))
  (replace-extension-table
   (available-extensions protocol)
   (user-extension-state-snapshot-available snapshot))
  (setf (protocol-storage protocol +extension-diagnostics-key+)
        (copy-list (user-extension-state-snapshot-diagnostics snapshot)))
  (clrhash (installed-user-handles protocol))
  (dolist (id (user-extension-state-snapshot-installed-ids snapshot))
    (let ((entry (gethash id (available-extensions protocol))))
      (when entry
        (install-user-extension protocol entry context))))
  protocol)

(defun remote-install-pins (protocol)
  "PROTOCOL's install-set: a string id -> pin hash-table. The :equal test both
lets string ids key it and survives the snapshot deserialize, whose ecase over
the recorded table test errors on anything but eq/eql/equal/equalp."
  (ensure-protocol-storage protocol +remote-install-pins-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun plain-list-p (object)
  "True for nil and proper lists. A dotted pair is not snapshot-safe."
  (and (listp object)
       (loop for tail = object then (cdr tail)
             while (consp tail)
             finally (return (null tail)))))

(defun pin-value-serializable-p (value)
  "True when VALUE rides the snapshot whitelist unchanged: nil, t, a keyword,
an integer, a string, or a proper list of such. A pathname, struct, CLOS
instance, function, float, or non-keyword symbol would drop the whole
install-set to skipped storage on capture, so they are rejected."
  (typecase value
    (null t)
    (keyword t)
    ((eql t) t)
    (integer t)
    (string t)
    (cons (and (plain-list-p value)
               (every #'pin-value-serializable-p value)))
    (t nil)))

(define-condition invalid-pin (error)
  ((pin :initarg :pin :reader invalid-pin-pin)
   (reason :initarg :reason :reader invalid-pin-reason))
  (:report (lambda (condition stream)
             (format stream "Install pin is not snapshot-serializable (~A): ~S"
                     (invalid-pin-reason condition)
                     (invalid-pin-pin condition)))))

(defun validate-pin (pin)
  "Return PIN when it is a snapshot-safe install pin, else signal invalid-pin.
A pin is a plist with keyword keys whose :id is a string and whose every value
is pin-value-serializable-p. Gating at construction keeps an unserializable
field from silently dropping the whole install-set on the next snapshot, since
one bad value drops the entire storage entry."
  (unless (plain-list-p pin)
    (error 'invalid-pin :pin pin :reason :not-a-plist))
  (unless (evenp (length pin))
    (error 'invalid-pin :pin pin :reason :odd-length))
  (loop for (key value) on pin by #'cddr
        do (unless (keywordp key)
             (error 'invalid-pin :pin pin :reason :non-keyword-key))
           (unless (pin-value-serializable-p value)
             (error 'invalid-pin :pin pin :reason :unserializable-value)))
  (unless (stringp (getf pin :id))
    (error 'invalid-pin :pin pin :reason :id-not-a-string))
  pin)

(defun record-remote-install-pin (protocol pin)
  "Validate PIN and record it in PROTOCOL's install-set under its :id. Returns
the recorded pin."
  (let ((valid (validate-pin pin)))
    (setf (gethash (getf valid :id) (remote-install-pins protocol)) valid)
    valid))

(defun restore-version-gaps (protocol)
  "Recorded restore-consistency gaps, oldest first."
  (protocol-storage protocol +restore-version-gaps-key+))

(defun record-restore-version-gap (protocol gap)
  "Append GAP, a plist describing an unavailable pin or a version
disagreement, to PROTOCOL's gap ledger."
  (setf (protocol-storage protocol +restore-version-gaps-key+)
        (append (restore-version-gaps protocol) (list gap))))

(defun nix-shadowed-installs (protocol)
  "Runtime installs skipped in favor of a nix-declared baseline, oldest first."
  (protocol-storage protocol +nix-shadowed-installs-key+))

(defun record-nix-shadowed-install (protocol record)
  "Append RECORD, a plist naming the shadowed id and the phase, to PROTOCOL's
shadow ledger."
  (setf (protocol-storage protocol +nix-shadowed-installs-key+)
        (append (nix-shadowed-installs protocol) (list record))))

(defun nix-declared-baseline-id-p (id)
  "True when ID matches an id the running image baked in as a present-at-boot
baseline. Both sides normalize, so a raw string id and the baked set agree."
  (let ((baseline kli/profiles:*nix-declared-baseline-ids*))
    (and baseline
         (member (normalize-extension-id id)
                 (mapcar #'normalize-extension-id baseline))
         t)))

(defvar *remote-install-resolver* nil
  "Restore-time hook resolving a pin to an installable entry when it is not in
the available registry. A function of (pin protocol context) returning a
user-extension-entry or nil. Nil here -- only already-available code is
reinstalled -- the registry fetch-and-verify path is wired separately.")

(defun isolated-pin-spawn-args (pin)
  "The optional lift keywords present in PIN, as a plist to apply onto
lift-mcp-server. The id and capability are passed separately."
  (loop for key in '(:arguments :directory :environment :timeout
                     :client-name :client-version :tool-coordinates)
        for value = (getf pin key)
        when value append (list key value)))

(defun reinstall-isolated-pin (pin protocol context restorer-grant)
  "Re-spawn and re-handshake the isolated MCP server PIN names, under the meet of
RESTORER-GRANT and the pin's carried install grant, so restore confers no more
authority than either the restorer holds or the original install recorded.
Returns :reinstalled on success, or :deferred when the restorer lacks spawn
authority, the pin carries no recorded grant, or the spawn or handshake fails --
all caught here so a missing server binary does not abort the whole restore. The
gap is recorded later by reconcile-isolated-gap-into, after storage rehydrate, so
it survives the wholesale overwrite."
  (let ((carried (getf pin :install-grant)))
    (if (or (null carried)
            (not (kli/ext:check-capability
                  (kli/ext:make-subject :grant restorer-grant)
                  :extension/spawn-process)))
        :deferred
        (handler-case
            (let ((kli/ext:*call-subject*
                    (kli/ext:make-subject
                     :grant (kli/ext:grant-meet
                             restorer-grant
                             (kli/ext:datum->grant carried)))))
              (install-manifest
               (apply #'kli/runtime/isolated:lift-mcp-server (getf pin :command)
                      :id (getf pin :id) :capability (getf pin :capability)
                      (isolated-pin-spawn-args pin))
               protocol context)
              :reinstalled)
          (error () :deferred)))))

(defun reinstall-remote-pin (pin protocol context
                             &optional (restorer-grant
                                        (kli/ext:subject-grant
                                         kli/ext:*call-subject*)))
  "Re-establish one install-set PIN on PROTOCOL during restore. Returns
:shadowed, :already-live, :reinstalled, or :deferred. A pin whose id is now
nix-declared in THIS image is shadowed: the baseline child already provides it,
so the pin's re-load and root activation are skipped (keyed on the current
image's baseline set, not the pin's source-kind, since a registry pin can
collide with a baseline declared on a different image). Otherwise resolves the
pin's code by its normalized id in the available registry, falling back to
*remote-install-resolver*. A pin whose id is now already loaded is left alone.
A version disagreement between the pin and the resolved entry records a gap
rather than crashing. Genuine activation errors propagate so a mid-restore
failure drives the caller's rollback."
  (let ((id (normalize-extension-id (getf pin :id))))
    (cond
      ((nix-declared-baseline-id-p id)
       (record-nix-shadowed-install
        protocol (list :id (getf pin :id) :reason :shadowed-by-nix-baseline
                       :phase :restore))
       :shadowed)
      ((extension-loaded-p protocol id) :already-live)
      ((eq (getf pin :source-kind) :isolated-server)
       (reinstall-isolated-pin pin protocol context restorer-grant))
      (t
       (let ((entry (or (gethash id (available-extensions protocol))
                         (and *remote-install-resolver*
                              (funcall *remote-install-resolver*
                                       pin protocol context)))))
          (cond
            ((null entry)
             (record-restore-version-gap
              protocol (list :id (getf pin :id) :reason :unavailable))
             :deferred)
            (t
             (let ((pinned (getf pin :version))
                   (available (getf (user-extension-entry-metadata entry)
                                    :version)))
               (when (and pinned available (not (equal pinned available)))
                 (record-restore-version-gap
                  protocol (list :id (getf pin :id) :reason :version-mismatch
                                 :pinned pinned :available available))))
             (install-user-extension protocol entry context)
             :reinstalled)))))))

(defun snapshot-remote-install-pins (snapshot)
  "Pins recorded in SNAPSHOT's storage under the install-set key, sorted by
string id for a deterministic replay order. Nil when the snapshot carries no
install-set."
  (loop for (key value) in (getf snapshot :storage)
        when (eq (kli/runtime/snapshot:deserialize-snapshot-value key)
                 +remote-install-pins-key+)
          do (let ((table (kli/runtime/snapshot:deserialize-snapshot-value value))
                   (pins '()))
               (maphash (lambda (id pin)
                          (declare (ignore id))
                          (push pin pins))
                        table)
               (return (sort pins #'string<
                             :key (lambda (pin) (getf pin :id)))))))

(defun replay-remote-installs-into (protocol context snapshot restorer-grant)
  "Re-establish every install-set pin in SNAPSHOT on PROTOCOL, bounding each
re-spawn by RESTORER-GRANT. Registered as the snapshot package's step-0 replay
hook so it runs before manifests rehydrate, giving each runtime install's code
(and CLOS package) a chance to exist when a captured manifest variable is
resolved."
  (dolist (pin (snapshot-remote-install-pins snapshot))
    (reinstall-remote-pin pin protocol context restorer-grant)))

(setf kli/runtime/snapshot:*replay-remote-installs* #'replay-remote-installs-into)

(defun reconcile-baseline-gap-into (protocol context snapshot gap-ids)
  "Record a :cross-image-baseline-gap for each nix-declared baseline the snapshot
carried that this image does not bake, so a snapshot from a differently configured
image restores with a diagnostic the user can see rather than a crash."
  (declare (ignore context snapshot))
  (dolist (id gap-ids)
    (record-restore-version-gap
     protocol (list :id id :reason :cross-image-baseline-gap))))

(setf kli/runtime/snapshot:*snapshot-nix-baseline-ids*
      (lambda () kli/profiles:*nix-declared-baseline-ids*))

(setf kli/runtime/snapshot:*reconcile-baseline-gap* #'reconcile-baseline-gap-into)

(defun isolated-server-pin-ids (protocol)
  "Normalized ids of PROTOCOL's install-set pins that re-spawn an isolated MCP
server."
  (let ((ids '()))
    (maphash (lambda (id pin)
               (declare (ignore id))
               (when (eq (getf pin :source-kind) :isolated-server)
                 (push (normalize-extension-id (getf pin :id)) ids)))
             (remote-install-pins protocol))
    ids))

(defun snapshot-isolated-object-ids (protocol)
  "Ids the snapshot must tolerate as not-reconstructed for PROTOCOL's
isolated-server pins on a failed restore re-spawn: each isolated pin's extension
id plus the ids of the tools it lifted. The lifted tools are registry-resident
objects captured under their own content-derived ids, so a host that cannot
re-spawn the server defers them rather than failing the restore."
  (let* ((servers (isolated-server-pin-ids protocol))
         (ids (copy-list servers)))
    (dolist (contribution (kli/ext:protocol-installed-contributions protocol))
      (when (and (typep contribution 'kli/ext:tool-contribution)
                 (member (normalize-extension-id
                          (kli/ext:contribution-extension contribution))
                         servers))
        (push (object-id (kli/ext:contribution-tool contribution)) ids)))
    ids))

(setf kli/runtime/snapshot:*snapshot-isolated-ids* #'snapshot-isolated-object-ids)

(defun reconcile-isolated-gap-into (protocol context snapshot)
  "Record an :isolated-server-unavailable gap for each isolated-server pin in the
rehydrated install-set whose extension is not live after restore, so a host that
could not re-spawn the server surfaces a visible gap rather than silently losing
its tools."
  (declare (ignore context snapshot))
  (maphash (lambda (id pin)
             (declare (ignore id))
             (when (and (eq (getf pin :source-kind) :isolated-server)
                        (not (extension-loaded-p
                              protocol (normalize-extension-id (getf pin :id)))))
               (record-restore-version-gap
                protocol (list :id (getf pin :id)
                               :reason :isolated-server-unavailable))))
           (remote-install-pins protocol)))

(setf kli/runtime/snapshot:*reconcile-isolated-gap* #'reconcile-isolated-gap-into)

(defun host-os ()
  #+darwin "darwin"
  #+linux "linux"
  #-(or darwin linux) "unknown")

(defun host-arch ()
  #+x86-64 "x86_64"
  #+arm64 "aarch64"
  #-(or x86-64 arm64) "unknown")

(defun host-triple ()
  "The os-arch tag identifying this host, e.g. linux-x86_64-glibc or
darwin-aarch64. The single producer of a pin's :install-triple and of every
:native-artifacts map key, so restore lookup is exact. The libc suffix is
appended only on linux, where glibc is assumed -- musl detection is a later
concern, latent while the blessed set leaves :native-artifacts empty."
  (let ((base (format nil "~A-~A" (host-os) (host-arch))))
    #+linux (concatenate 'string base "-glibc")
    #-linux base))

(defun install-enabled-extensions (protocol config context)
  (let ((profile (protocol-active-profile protocol)))
    (loop for entry being the hash-values of (available-extensions protocol)
          for id = (user-extension-entry-id entry)
          when (extension-enabled-p entry config profile)
            do (if (nix-declared-baseline-id-p id)
                   (record-nix-shadowed-install
                    protocol (list :id id :reason :shadowed-by-nix-baseline
                                   :phase :boot))
                   (call-with-diagnostics-capture
                    protocol id
                    (lambda ()
                      (handler-case (install-user-extension protocol entry context)
                        (error (condition)
                          (warn "kli: extension ~A failed to install: ~A"
                                id condition)))))))))

(defun enable-user-extension (context id)
  "Install the available extension named ID. Returns (values ok state)."
  (with-extension-lifecycle-lock
    (let* ((protocol (active-protocol context))
           (id (normalize-extension-id id))
           (entry (gethash id (available-extensions protocol))))
      (cond
        ((null entry) (values nil :unknown))
        ((gethash id (installed-user-handles protocol)) (values nil :already-enabled))
        (t (install-user-extension protocol entry context) (values t :enabled))))))

(defun disable-user-extension (context id)
  "Retract the installed extension named ID. Returns (values ok state)."
  (with-extension-lifecycle-lock
    (let ((protocol (active-protocol context))
          (id (normalize-extension-id id)))
      (if (retract-user-extension protocol id context)
          (values t :disabled)
          (values nil :not-enabled)))))

(defun drop-remote-install-pin (protocol id)
  "Remove every install-set pin whose normalized id matches ID. The store keys
on the raw pin id, so matching normalizes both sides. Returns T if any removed."
  (let ((target (normalize-extension-id id))
        (pins (remote-install-pins protocol))
        (keys '()))
    (maphash (lambda (key pin)
               (declare (ignore pin))
               (when (eql (normalize-extension-id key) target)
                 (push key keys)))
             pins)
    (dolist (key keys) (remhash key pins))
    (and keys t)))

(defun uninstall-remote-extension (context id)
  "Remove a runtime-installed (install-set) extension named ID: drop its live
retract handle and its durable pin. A nix-declared baseline id is refused --
the image owns its presence, so removal means editing the nix config and
rebuilding. Returns (values ok state) where state is :uninstalled,
:nix-declared, or :not-installed."
  (with-extension-lifecycle-lock
    (let ((protocol (active-protocol context))
          (id (normalize-extension-id id)))
      (if (nix-declared-baseline-id-p id)
          (values nil :nix-declared)
          (let ((had-handle (retract-user-extension protocol id context))
                (had-pin (drop-remote-install-pin protocol id)))
            (if (or had-handle had-pin)
                (values t :uninstalled)
                (values nil :not-installed)))))))

(defun user-extension-status (context)
  "Alist of (id . enabled-p) for every available user extension."
  (let ((protocol (active-protocol context)))
    (loop for entry being the hash-values of (available-extensions protocol)
          for id = (user-extension-entry-id entry)
          collect (cons id (and (gethash id (installed-user-handles protocol)) t)))))

(defun load-user-extensions (context &key (config (read-user-config))
                                          (units (default-extension-units config)))
  "Index UNITS as available extensions, then install the enabled subset.
Returns the available-extensions registry. Each unit sets its own package
regime in load-files-for-unit, so no package is bound here."
  (let ((protocol (active-protocol context)))
    (dolist (unit units)
      (index-unit protocol unit))
    (install-enabled-extensions protocol config context)
    (available-extensions protocol)))

(defun commit-user-extension-reload (context config target-available diagnostics)
  (let* ((protocol (active-protocol context))
         (snapshot (snapshot-user-extension-state protocol)))
    (handler-case
        (progn
          (replace-extension-table (available-extensions protocol)
                                   target-available)
          (replace-extension-diagnostics protocol diagnostics)
          (dolist (id (user-extension-state-snapshot-installed-ids snapshot))
            (retract-user-extension protocol id context))
          (install-enabled-extensions protocol config context)
          (available-extensions protocol))
      (error (condition)
        (restore-user-extension-state protocol context snapshot)
        (error condition)))))

(defun reload-user-extensions (context &key (config (read-user-config))
                                            (units (default-extension-units config)))
  "Re-index and re-install user extensions from the current units. A failed
prepare records diagnostics but leaves the live extension set in place."
  (with-extension-lifecycle-lock
    (let ((protocol (active-protocol context)))
      (multiple-value-bind (target-available diagnostics failed)
          (stage-user-extension-units units)
        (cond
          (failed
           (replace-extension-diagnostics protocol diagnostics)
           (available-extensions protocol))
          (t
           (commit-user-extension-reload context config
                                         target-available diagnostics)))))))

(defun format-extension-status (status &optional diagnostics)
  (if status
      (with-output-to-string (out)
        (format out "User extensions:")
        (dolist (pair status)
          (format out "~%  [~:[disabled~;enabled ~]] ~(~A~)" (cdr pair) (car pair)))
        (dolist (entry diagnostics)
          (format out "~%  [diagnostics] ~A" (extension-diagnostic-label (car entry)))))
      "No user extensions found."))

(defun enable-reply-text (context name)
  (if (null name)
      "Usage: /enable <extension>"
      (multiple-value-bind (ok state) (enable-user-extension context name)
        (declare (ignore ok))
        (ecase state
          (:enabled (format nil "Enabled ~A." name))
          (:already-enabled (format nil "~A is already enabled." name))
          (:unknown (format nil "No such extension: ~A." name))))))

(defun disable-reply-text (context name)
  (if (null name)
      "Usage: /disable <extension>"
      (multiple-value-bind (ok state) (disable-user-extension context name)
        (declare (ignore ok))
        (ecase state
          (:disabled (format nil "Disabled ~A." name))
          (:not-enabled (format nil "~A is not enabled." name))))))

(defun uninstall-reply-text (context name)
  (if (null name)
      "Usage: /uninstall <extension>"
      (multiple-value-bind (ok state) (uninstall-remote-extension context name)
        (declare (ignore ok))
        (ecase state
          (:uninstalled (format nil "Uninstalled ~A." name))
          (:nix-declared
           (format nil "~A is Nix-declared (present at boot, baked into this image). Remove it from your kli.extensions and rebuild to uninstall. Runtime uninstall is not available for Nix-declared extensions." name))
          (:not-installed (format nil "~A is not installed." name))))))

(defextension user-extension-commands
  (:provides
   (command "reload"
     :description "Reload user extensions from disk."
     :metadata '(:run-as :extension-load)
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments call-id on-update))
                (reload-user-extensions context)
                (reply (format-extension-status
                        (user-extension-status context)
                        (extension-diagnostics (active-protocol context))))))
   (command "extensions"
     :description "List available user extensions and their state."
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments call-id on-update))
                (reply (format-extension-status
                        (user-extension-status context)))))
   (command "enable"
     :description "Enable a user extension by name."
     :arguments '(:tail :extension)
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command call-id on-update))
                (reply (enable-reply-text context (rest-arg arguments)))))
   (command "disable"
     :description "Disable a user extension by name."
     :arguments '(:tail :extension)
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command call-id on-update))
                (reply (disable-reply-text context (rest-arg arguments)))))
   (command "uninstall"
     :description "Uninstall a runtime-installed extension by name."
     :arguments '(:tail :extension)
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command call-id on-update))
                (reply (uninstall-reply-text context (rest-arg arguments)))))))

;; Bound by defextensions in files that load after this one:
;; profile-command.lisp and install-command.lisp.
(defvar *profile-commands-extension-manifest*)
(defvar *remote-install-tool-extension-manifest*)
(defvar *remote-install-command-extension-manifest*)

(defun install-user-extension-commands (protocol context)
  "Install the runtime-control commands plus the remote-install command, tool,
and policy provider. Best-effort per manifest: absent on a profile without the
capabilities it requires."
  (flet ((best-effort (manifest)
           (handler-case
               (install-manifest manifest protocol context)
             (error (condition)
               (warn "kli: runtime-control commands unavailable (~A)" condition)
               nil))))
    (best-effort *user-extension-commands-extension-manifest*)
    (best-effort *profile-commands-extension-manifest*)
    (best-effort *remote-install-tool-extension-manifest*)
    (best-effort *remote-install-command-extension-manifest*)))

(defun extension-arg-units (argv)
  "Units named by --extension PATH flags: a directory is discovered, any other
path is a single-file unit."
  (loop for (flag value) on argv
        when (and value (string= flag +extension-arg+))
          append (let* ((raw (uiop:parse-native-namestring value))
                        (dir (uiop:directory-exists-p raw)))
                   (if dir
                       (discover-units dir)
                       (list (list :single raw))))))

(defun alias-settings (context)
  "The user's alias map (alias name -> target command name) from settings, or
NIL. A missing or non-object value yields NIL, so a malformed entry never
breaks boot."
  (let ((service (find-config-service context)))
    (when service
      (let ((value (settings-value (config-service-settings service) "aliases")))
        (and (hash-table-p value) value)))))

(defun register-one-alias (commands context name target)
  "Bind alias NAME to TARGET through COMMANDS, or return a human problem string
when it cannot. NAME must be a bare command name; TARGET (bare or SOURCE:NAME)
must resolve to exactly one command now, whose clone is registered under NAME at
the tightest tier so the alias outranks every other source."
  (cond
    ((not (and (stringp name) (plusp (length name))))
     (format nil "~S (alias name must be a non-empty string)" name))
    ((not (stringp target))
     (format nil "~A -> ~S (target must be a command name)" name target))
    ((find #\: name)
     (format nil "~A -> ~A (alias name must be bare, not qualified)"
             name target))
    (t
     (let* ((resolution (provider-call commands :resolve-command target))
            (command (getf resolution :command)))
       (cond
         (command
          (provider-call commands :register-command context name
                         (clone-command command :name name)
                         :source :user :tier :alias)
          nil)
         (t
          (format nil "~A -> ~A (~(~A~))"
                  name target (getf resolution :status))))))))

(defun register-user-aliases (context)
  "Register each user-declared alias as a top-precedence command binding, the
target resolved once now. A name that is not bare, or a target that does not
resolve to exactly one command, is skipped with a :boot diagnostic rather than
failing boot. Runs after user extensions load and before the collision read, so
a deliberately-bound name never surfaces as a contest."
  (let ((aliases (alias-settings context)))
    (when aliases
      (let* ((protocol (active-protocol context))
             (commands (find-capability-provider protocol :commands
                                                 :contract :commands/v1))
             (problems '()))
        (when commands
          (maphash (lambda (name target)
                     (let ((problem (register-one-alias commands context
                                                        name target)))
                       (when problem (push problem problems))))
                   aliases))
        (when problems
          (record-extension-diagnostic
           protocol :boot
           (format nil "Ignored alias~P that did not resolve:~{~%  ~A~}"
                   (length problems) (nreverse problems))))))))

(defun record-command-collisions (protocol)
  "Record any command-name contest as a :boot diagnostic, surfaced through the
same transcript channel as other boot warnings. A read of the settled registry
once every extension is installed, so a shadowed or ambiguous slash command is
visible at boot rather than silent."
  (let ((commands (find-capability-provider protocol :commands
                                            :contract :commands/v1)))
    (when commands
      (let ((text (format-command-collisions
                   (provider-call commands :command-collisions))))
        (when text
          (record-extension-diagnostic protocol :boot text))))))

(defun boot-user-extensions (context &key (argv (uiop:command-line-arguments)))
  "Install the runtime-control commands, then discover and install user
extensions (config dirs plus any --extension files or directories). Output
outside any unit (a config warning, the commands-install warning) is captured
as a :boot diagnostic, so nothing reaches the tty the TUI is about to take
over."
  (let ((protocol (active-protocol context)))
    (call-with-diagnostics-capture
     protocol :boot
     (lambda ()
       (let* ((config (read-user-config))
              (units (append (default-extension-units config)
                             (extension-arg-units argv))))
         ;; Bounded extension-load authority for the first-party runtime-control
         ;; commands and the user-extension load.
         (let ((*call-subject* *install-subject*))
           (install-user-extension-commands protocol context)
           (load-user-extensions context :config config :units units)))))
    (register-user-aliases context)
    (record-command-collisions protocol)
    context))
