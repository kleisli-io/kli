(in-package #:kli/ext)

(defvar *author-clause-requirements* (make-hash-table :test #'equal)
  "Clause head / contribution kind to list of requirement specs it implies.
A spec has the shape of a :requires clause -- (KIND NAME &key CONTRACT
PROVIDER-ID). Populated by `register-author-clause-requirements`.")

(defun register-author-clause-requirements (kind requirement-specs)
  "Declare that author clause / contribution KIND implies REQUIREMENT-SPECS."
  (setf (gethash (normalize-extension-id kind) *author-clause-requirements*)
        requirement-specs))

(defun requirement-spec-key (spec)
  (list (normalize-extension-id (first spec))
        (normalize-extension-id (second spec))))

(defun dedup-requirement-specs (specs)
  "Collapse specs sharing a (kind name) key. The earliest occurrence wins, so
an explicit :requires overrides a derived spec of the same identity."
  (remove-duplicates specs :key #'requirement-spec-key :test #'equal
                           :from-end t))

(defun clause-derived-specs (head)
  (gethash (normalize-extension-id head) *author-clause-requirements*))

(defun derive-requirement-specs (provides-clauses explicit-specs)
  "Union EXPLICIT-SPECS with the specs implied by the heads of PROVIDES-CLAUSES.
A clause head with no registered requirements contributes none -- derivation is
a total table lookup, never a guess. Macroexpand-time path for `defextension`."
  (dedup-requirement-specs
   (append explicit-specs
           (loop for clause in provides-clauses
                 when (and (consp clause) (symbolp (first clause)))
                   append (clause-derived-specs (first clause))))))

(defun spec->requirement (spec)
  "Build a requirement value from a SPEC, normalizing every designator to a
keyword so satisfaction checks (which dispatch on the kind keyword) match.
The builder path yields the same requirement the declarative path compiles."
  (destructuring-bind (kind name &key contract provider-id) spec
    (make-requirement :kind (normalize-extension-id kind)
                      :name (normalize-extension-id name)
                      :contract (and contract (normalize-extension-id contract))
                      :provider-id (and provider-id
                                        (normalize-extension-id provider-id)))))

(defun derive-requirements-from-contributions (contributions explicit-specs)
  "Builder counterpart of `derive-requirement-specs`: keyed on each
contribution's kind, returning requirement values. Runtime path for
`kli-extension`."
  (mapcar #'spec->requirement
          (dedup-requirement-specs
           (append explicit-specs
                   (loop for contribution in contributions
                         append (gethash (contribution-kind contribution)
                                         *author-clause-requirements*))))))

(defstruct extension-builder
    "Mutable accumulator that `kli-extension` drives through author verbs.
`build-extension` closes it into an `extension` with derived requirements,
the same value a `defextension` manifest thunk yields."
  (id nil)
  (contributions '())
  (requirements '())
  (metadata '()))

(defun builder-add-contribution (builder contribution)
  (push contribution (extension-builder-contributions builder))
  contribution)

(defun build-extension (builder)
  (let ((contributions (reverse (extension-builder-contributions builder))))
    (make-extension
     :id (extension-builder-id builder)
     :source (extension-builder-id builder)
     :metadata (extension-builder-metadata builder)
     :contributions contributions
     :requirements (derive-requirements-from-contributions
                    contributions
                    (reverse (extension-builder-requirements builder))))))
