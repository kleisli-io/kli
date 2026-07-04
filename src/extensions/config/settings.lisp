(in-package #:kli/config)

(defparameter +settings-filename+ "settings.json")

(defun settings-path-in (dir)
  (and dir (merge-pathnames +settings-filename+ dir)))

(defun global-settings-path ()
  (settings-path-in (global-config-dir)))

(defun project-settings-path (&key (start (project-start-directory)))
  (settings-path-in (project-config-dir :start start)))

(defun read-settings-file (path)
  "Parsed JSON object from PATH. NIL when PATH is NIL, absent, malformed, or
not an object at the top level (fail-soft)."
  (when (and path (uiop:file-exists-p path))
    (handler-case
        (let ((parsed (com.inuoe.jzon:parse path)))
          (if (hash-table-p parsed)
              parsed
              (warn "kli: ignoring ~A (top level is not an object)" path)))
      (error (condition)
        (warn "kli: ignoring ~A (~A)" path condition)
        nil))))

(defun merge-settings (base overlay)
  "Merge OVERLAY into BASE without mutating either. Nested objects merge
recursively, any other overlay value replaces the base value, so a project
file overrides its global counterpart key by key."
  (cond
    ((null overlay) base)
    ((null base) overlay)
    ((not (and (hash-table-p base) (hash-table-p overlay))) overlay)
    (t
     (let ((merged (make-hash-table :test #'equal)))
       (maphash (lambda (key value) (setf (gethash key merged) value)) base)
       (maphash (lambda (key value)
                  (let ((existing (gethash key merged)))
                    (setf (gethash key merged)
                          (if (and (hash-table-p existing)
                                   (hash-table-p value))
                              (merge-settings existing value)
                              value))))
                overlay)
       merged))))

(defun nix-config-object-plist-p (value)
  "True when VALUE is a baked object: a non-empty plist with keyword keys, as
distinct from a plain list value."
  (and (consp value)
       (keywordp (car value))
       (ignore-errors (evenp (list-length value)))))

(defun nix-config-plist->table (plist)
  "Convert a baked settings plist (keyword keys, possibly nested) to a
string-keyed table matching the settings.json shape, so it deep-merges with the
XDG layers. Keys are the keyword names verbatim (the serialiser bar-escapes them
so camelCase survives the reader); nested objects recurse."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (symbol-name key) table)
                   (if (nix-config-object-plist-p value)
                       (nix-config-plist->table value)
                       value)))
    table))

(defun read-nix-config-form ()
  "The baked nix-config plist, or NIL when no nix-config resource is registered
(plain core) or the file is absent or unreadable. Fail-soft: a broken baked
config must never break settings."
  (handler-case
      (let ((path (buildlisp/resources:resource-path "kli/nix-config" "config.sexp")))
        (when (uiop:file-exists-p path)
          (with-open-file (in path :direction :input :external-format :utf-8)
            (let ((*read-eval* nil))
              (read in nil nil)))))
    (error () nil)))

(defun nix-baked-settings ()
  "The nix-baked settings table, or NIL. The lowest-precedence settings layer:
a user's global settings.json always overrides these baked defaults."
  (let ((settings (getf (read-nix-config-form) :settings)))
    (and settings (nix-config-plist->table settings))))

(defun load-settings (&key (global-path (global-settings-path))
                           (project-path (project-settings-path))
                           (nix-baked (nix-baked-settings)))
  "Merged settings table, nix-baked < global < project. Empty table when no
layer exists. The nix-baked layer is the baked default a nix-configured image
supplies; a plain image has none."
  (or (merge-settings (merge-settings nix-baked (read-settings-file global-path))
                      (read-settings-file project-path))
      (make-hash-table :test #'equal)))

(defun settings-value (settings &rest keys)
  "Walk string KEYS through nested objects. Values (value present-p), so a
stored false stays distinguishable from a missing key."
  (let ((node settings))
    (loop for (key . more) on keys
          do (unless (hash-table-p node)
               (return-from settings-value (values nil nil)))
             (multiple-value-bind (value present) (gethash key node)
               (unless present
                 (return-from settings-value (values nil nil)))
               (if more
                   (setf node value)
                   (return-from settings-value (values value t)))))))
