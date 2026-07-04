(in-package #:kli/profiles)

(defparameter *default-profile-base* :interactive-terminal
  "Builtin profile an extends-less spec bottoms at during resolution.")

(defstruct (profile-spec (:constructor make-profile-spec))
  name
  extends
  enable
  disable
  settings)

(defstruct (resolved-profile (:constructor make-resolved-profile))
  name
  base
  enable
  disable
  settings)

(defun parse-id-list (value)
  "Values (ids ok). IDS is VALUE mapped through `normalize-extension-id` when
VALUE is a vector of strings, OK is NIL for any other shape."
  (if (and (vectorp value)
           (not (stringp value))
           (every #'stringp value))
      (values (map 'list #'normalize-extension-id value) t)
      (values nil nil)))

(defun parse-profile-entry (id value)
  "PROFILE-SPEC parsed from the settings object VALUE, or NIL with a warning
naming the malformed field (fail-soft)."
  (flet ((reject (field)
           (warn "kli: ignoring profile ~S (malformed ~A)" id field)
           (return-from parse-profile-entry nil)))
    (unless (hash-table-p value)
      (warn "kli: ignoring profile ~S (not an object)" id)
      (return-from parse-profile-entry nil))
    (let* ((extends
             (multiple-value-bind (raw present) (gethash "extends" value)
               (cond ((not present) nil)
                     ((stringp raw) (normalize-extension-id raw))
                     (t (reject "extends")))))
           (enable
             (multiple-value-bind (raw present) (gethash "enable" value)
               (if present
                   (multiple-value-bind (ids ok) (parse-id-list raw)
                     (if ok ids (reject "enable")))
                   '())))
           (disable
             (multiple-value-bind (raw present) (gethash "disable" value)
               (if present
                   (multiple-value-bind (ids ok) (parse-id-list raw)
                     (if ok ids (reject "disable")))
                   '())))
           (settings
             (multiple-value-bind (raw present) (gethash "settings" value)
               (cond ((not present) nil)
                     ((hash-table-p raw) raw)
                     (t (reject "settings"))))))
      (make-profile-spec :name id
                         :extends extends
                         :enable enable
                         :disable disable
                         :settings settings))))

(defun parse-profile-specs (settings &key (builtins (known-profile-names)))
  "Table of keyword profile name -> `profile-spec` parsed from the
\"profiles\" object in SETTINGS (a parsed settings table). Fail-soft: a
malformed entry warns and is skipped, an entry shadowing a builtin profile
name warns and is skipped, a missing or non-object \"profiles\" value yields
an empty table."
  (let ((specs (make-hash-table)))
    (multiple-value-bind (profiles present)
        (if (hash-table-p settings)
            (gethash "profiles" settings)
            (values nil nil))
      (cond
        ((not present))
        ((not (hash-table-p profiles))
         (warn "kli: ignoring profiles settings (not an object)"))
        (t
         (maphash
          (lambda (name value)
            (let ((id (normalize-extension-id name)))
              (if (member id builtins)
                  (warn "kli: ignoring profile ~S (shadows a builtin profile)"
                        id)
                  (let ((spec (parse-profile-entry id value)))
                    (when spec
                      (setf (gethash id specs) spec))))))
          profiles))))
    specs))

(defun fold-profile-deltas (chain)
  "Values (enable disable settings) folding CHAIN of `profile-spec`s base-most
first. Later specs have the last word per extension id and their settings
merge over earlier ones."
  (let ((enable '())
        (disable '())
        (settings nil))
    (dolist (spec chain)
      (dolist (id (profile-spec-enable spec))
        (setf disable (remove id disable))
        (pushnew id enable))
      (dolist (id (profile-spec-disable spec))
        (setf enable (remove id enable))
        (pushnew id disable))
      (setf settings (kli/config:merge-settings
                      settings (profile-spec-settings spec))))
    (values (nreverse enable)
            (nreverse disable)
            (or settings (make-hash-table :test #'equal)))))

(defun resolve-profile-spec (name specs
                             &key (builtins (known-profile-names))
                                  (default-base *default-profile-base*))
  "Resolved view of profile NAME against SPECS (a table from
`parse-profile-specs`). Walks the extends chain until it bottoms at a builtin
profile (an extends-less spec bottoms at DEFAULT-BASE), then folds the chain
base-most first into effective enable/disable delta sets and a merged
settings overlay. Fail-soft: an unknown name, a dangling extends target, or
an extends cycle warns and returns NIL. A builtin NAME resolves to itself
with empty deltas."
  (let ((id (normalize-extension-id name)))
    (if (member id builtins)
        (make-resolved-profile :name id
                               :base id
                               :enable '()
                               :disable '()
                               :settings (make-hash-table :test #'equal))
        (let ((chain '())
              (visited '())
              (base nil)
              (current id))
          (loop
            (when (member current visited)
              (warn "kli: profile ~S has a cyclic extends chain (~{~S~^ -> ~})"
                    id (reverse (cons current visited)))
              (return-from resolve-profile-spec nil))
            (push current visited)
            (let ((spec (gethash current specs)))
              (unless spec
                (if (eq current id)
                    (warn "kli: unknown profile ~S" id)
                    (warn "kli: profile ~S extends unknown profile ~S"
                          id current))
                (return-from resolve-profile-spec nil))
              (push spec chain)
              (let ((extends (profile-spec-extends spec)))
                (cond
                  ((null extends)
                   (setf base default-base)
                   (return))
                  ((member extends builtins)
                   (setf base extends)
                   (return))
                  (t (setf current extends))))))
          (multiple-value-bind (enable disable settings)
              (fold-profile-deltas chain)
            (make-resolved-profile :name id
                                   :base base
                                   :enable enable
                                   :disable disable
                                   :settings settings))))))
