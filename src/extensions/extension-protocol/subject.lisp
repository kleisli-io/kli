(in-package #:kli/ext)

;;; Constraint algebra: a decidable fragment bounding how a capability may be
;;; exercised. Constraints form a meet-semilattice with :any on top (no
;;; restriction) and :none at the bottom (never). Between them sit independent
;;; dimensions -- path prefixes, numeric bounds, enumerated sets -- each
;;; totally ordered within its kind and incomparable across kinds (a cross-kind
;;; meet drops to :none). Decidability keeps covers/meet total and cheap.
;;; Representations are serializable s-expressions.

(defun constraint-any () :any)
(defun constraint-none () :none)

(defun canonical-path (path)
  "Lexically resolve . and .. segments in PATH without touching disk, so a
path-prefix constraint cannot be escaped by an unnormalized .. that merely
string-matches. Leading and trailing slashes are preserved so a directory
prefix keeps its segment boundary. Symlink resolution is out of scope: it needs
disk and is TOCTOU-prone for not-yet-existing targets."
  (let ((absolute (and (plusp (length path)) (char= (char path 0) #\/)))
        (trailing (and (> (length path) 1)
                       (char= (char path (1- (length path))) #\/)))
        (segments '())
        (start 0))
    (loop for i from 0 to (length path)
          when (or (= i (length path)) (char= (char path i) #\/))
            do (let ((segment (subseq path start i)))
                 (cond
                   ((or (string= segment "") (string= segment ".")))
                   ((string= segment "..")
                    (if (and segments (not (string= (first segments) "..")))
                        (pop segments)
                        (unless absolute (push segment segments))))
                   (t (push segment segments)))
                 (setf start (1+ i))))
    (let ((body (format nil "~{~A~^/~}" (nreverse segments))))
      (concatenate 'string
                   (if absolute "/" "")
                   body
                   (if (and trailing (plusp (length body))) "/" "")))))

(defun path-prefix-constraint (prefix)
  "Restrict to paths under PREFIX (a string), lexically normalized so . and ..
cannot escape the prefix by string trickery."
  (list :path-prefix (canonical-path prefix)))

(defun numeric-bound-constraint (&key low high)
  "Restrict to numbers within [LOW, HIGH]; a NIL bound is unbounded on that side."
  (list :numeric-bound low high))

(defun enumerated-constraint (elements)
  "Restrict to membership in the finite set ELEMENTS (compared by EQUAL)."
  (cons :enum (remove-duplicates elements :test #'equal)))

(defun constraint-kind (constraint)
  (cond
    ((eq constraint :any) :any)
    ((eq constraint :none) :none)
    ((and (consp constraint) (eq (car constraint) :path-prefix)) :path-prefix)
    ((and (consp constraint) (eq (car constraint) :numeric-bound)) :numeric-bound)
    ((and (consp constraint) (eq (car constraint) :enum)) :enum)
    (t (error "Not a constraint: ~S" constraint))))

(defun string-prefix-p (prefix string)
  (let ((plen (length prefix)))
    (and (<= plen (length string))
         (string= prefix string :end2 plen))))

(defun %low-covers-p (outer inner)
  "Lower bound OUTER reaches at or below INNER (NIL is negative infinity)."
  (or (null outer) (and inner (<= outer inner))))

(defun %high-covers-p (outer inner)
  "Upper bound OUTER reaches at or above INNER (NIL is positive infinity)."
  (or (null outer) (and inner (>= outer inner))))

(defun constraint-covers-p (outer inner)
  "Non-nil when OUTER permits at least everything INNER permits."
  (let ((ko (constraint-kind outer))
        (ki (constraint-kind inner)))
    (cond
      ((eq ko :any) t)
      ((eq ki :none) t)
      ((eq ko :none) nil)
      ((eq ki :any) nil)
      ((not (eq ko ki)) nil)
      (t (ecase ko
           (:path-prefix
            (string-prefix-p (second outer) (second inner)))
           (:numeric-bound
            (and (%low-covers-p (second outer) (second inner))
                 (%high-covers-p (third outer) (third inner))))
           (:enum
            (subsetp (cdr inner) (cdr outer) :test #'equal)))))))

(defun %max-low (a b) (cond ((null a) b) ((null b) a) (t (max a b))))
(defun %min-high (a b) (cond ((null a) b) ((null b) a) (t (min a b))))

(defun constraint-meet (a b)
  "Greatest lower bound of A and B (attenuation): the strongest constraint
covered by both. Cross-kind meets collapse to :none."
  (let ((ka (constraint-kind a))
        (kb (constraint-kind b)))
    (cond
      ((eq ka :any) b)
      ((eq kb :any) a)
      ((or (eq ka :none) (eq kb :none)) :none)
      ((not (eq ka kb)) :none)
      (t (ecase ka
           (:path-prefix
            (let ((pa (second a)) (pb (second b)))
              (cond ((string-prefix-p pa pb) b)
                    ((string-prefix-p pb pa) a)
                    (t :none))))
           (:numeric-bound
            (let ((lo (%max-low (second a) (second b)))
                  (hi (%min-high (third a) (third b))))
              (if (and lo hi (> lo hi))
                  :none
                  (list :numeric-bound lo hi))))
           (:enum
            (let ((shared (intersection (cdr a) (cdr b) :test #'equal)))
              (if shared (cons :enum shared) :none))))))))

;;; Grant: a bounded meet-semilattice of authority. A grant maps capability
;;; atoms to the constraint under which each may be exercised; an absent atom
;;; is not granted at all. UNIVERSAL marks the top (system authority, covers
;;; everything); the empty grant is the bottom. The keyword bag is the
;;; degenerate instance -- every atom mapped to :any -- so authority orders by
;;; subset and attenuates by intersection. Delegation confers a grant the
;;; holder already covers; attenuation is the meet. Since meet only descends
;;; and conferral is gated by covers, no operation widens authority: privilege
;;; escalation is unrepresentable rather than policed.
;;;
;;; A universal grant may carry an EXCLUDED set: the atoms it withholds. This
;;; gives the lattice its co-finite elements -- the grants just below the top
;;; that confer everything but a finite set of atoms. They are the only shape
;;; able to express "all authority minus these actuators", since the atom
;;; universe is open (lifted-tool atoms are interned on demand) and so cannot
;;; be enumerated to subtract from. EXCLUDED is meaningful only when UNIVERSAL.

(defstruct (grant (:constructor %make-grant) (:copier nil))
  (universal nil :type boolean)
  (atoms '() :type list)
  (excluded '() :type list))

(defun grant-top ()
  "The system grant: covers every capability under every constraint."
  (%make-grant :universal t))

(defun grant-bottom ()
  "The empty grant: covers nothing."
  (%make-grant))

(defun make-grant (&key capabilities atoms)
  "Build a grant. CAPABILITIES is a keyword list, each mapped to :any. ATOMS is
an alist (capability . constraint) for finer authority."
  (%make-grant
   :universal nil
   :atoms (append (mapcar (lambda (cap) (cons cap (constraint-any))) capabilities)
                  (copy-alist atoms))))

(defun grant-capabilities (grant)
  "The capability keywords GRANT confers (constraints not reflected)."
  (mapcar #'car (grant-atoms grant)))

(defun grant-constraint (grant capability)
  "The constraint GRANT places on CAPABILITY: :any when universal (unless the
universal grant excludes the capability), :none when the capability is not
granted."
  (if (grant-universal grant)
      (if (member capability (grant-excluded grant)) :none :any)
      (let ((entry (assoc capability (grant-atoms grant))))
        (if entry (cdr entry) :none))))

(defun grant-covers-p (holder requested)
  "Non-nil when HOLDER confers at least everything REQUESTED confers."
  (cond
    ;; Both co-finite: holder covers requested iff holder withholds no atom
    ;; requested still confers -- i.e. holder's exclusions are a subset.
    ((and (grant-universal holder) (grant-universal requested))
     (subsetp (grant-excluded holder) (grant-excluded requested)))
    ;; Universal holder, bounded request: covered unless the request names an
    ;; atom the holder excludes (every non-excluded atom is conferred at :any,
    ;; which covers any constraint).
    ((grant-universal holder)
     (notany (lambda (pair) (member (car pair) (grant-excluded holder)))
             (grant-atoms requested)))
    ;; Bounded holder, universal request: the request confers infinitely many
    ;; atoms (the open universe minus a finite set); a finite holder cannot.
    ((grant-universal requested) nil)
    (t (every (lambda (pair)
                (let ((held (assoc (car pair) (grant-atoms holder))))
                  (and held (constraint-covers-p (cdr held) (cdr pair)))))
              (grant-atoms requested)))))

(defun %drop-excluded (grant excluded)
  "GRANT (bounded) with every atom named in EXCLUDED removed -- the meet of a
bounded grant against a co-finite grant withholding EXCLUDED."
  (%make-grant
   :atoms (remove-if (lambda (pair) (member (car pair) excluded))
                     (grant-atoms grant))))

(defun grant-meet (a b)
  "Greatest lower bound of A and B: the strongest grant covered by both."
  (cond
    ;; Co-finite meet co-finite: withhold the union of both exclusion sets.
    ((and (grant-universal a) (grant-universal b))
     (%make-grant :universal t
                  :excluded (union (grant-excluded a) (grant-excluded b))))
    ;; Co-finite meet bounded: the bounded side, minus the atoms the universal
    ;; side withholds (a top with no exclusions returns the bounded side whole).
    ((grant-universal a) (%drop-excluded b (grant-excluded a)))
    ((grant-universal b) (%drop-excluded a (grant-excluded b)))
    (t (%make-grant
        :universal nil
        :atoms (loop for pair in (grant-atoms a)
                     for entry = (assoc (car pair) (grant-atoms b))
                     when entry
                       append (let ((m (constraint-meet (cdr pair) (cdr entry))))
                                (unless (eq m :none)
                                  (list (cons (car pair) m)))))))))

(defun grant-without-atoms (grant atoms)
  "Strongest grant covered by GRANT that confers none of ATOMS. A bounded grant
drops the named atoms outright; a universal grant records them as exclusions,
yielding the co-finite grant that still confers every other capability. The
result is always covered by GRANT, so removing atoms only ever narrows it."
  (if (grant-universal grant)
      (%make-grant :universal t
                   :excluded (union (grant-excluded grant) atoms))
      (grant-meet grant
                  (make-grant :capabilities
                              (set-difference (grant-capabilities grant) atoms)))))

(defun grant-equiv-p (a b)
  "Non-nil when A and B confer the same authority (mutual cover)."
  (and (grant-covers-p a b) (grant-covers-p b a)))

;;; Grant serialization. Render a grant as a durable proper-list datum so an
;;; authority map can ride snapshot storage. The atom alist becomes proper
;;; 2-lists -- a dotted pair is unserializable -- and each constraint is
;;; already a serializable s-expression.

(defun grant->datum (grant)
  "Render GRANT as a serializable datum: (:universal) for the top,
(:universal-except (atom ...)) for a co-finite grant, otherwise
(:atoms ((capability constraint) ...))."
  (cond
    ((and (grant-universal grant) (grant-excluded grant))
     (list :universal-except (copy-list (grant-excluded grant))))
    ((grant-universal grant) (list :universal))
    (t (list :atoms
             (mapcar (lambda (atom) (list (car atom) (cdr atom)))
                     (grant-atoms grant))))))

(defun datum->grant (datum)
  "Reconstruct a grant from `grant->datum` output."
  (ecase (first datum)
    (:universal (grant-top))
    (:universal-except
     (%make-grant :universal t :excluded (copy-list (second datum))))
    (:atoms (%make-grant
             :atoms (mapcar (lambda (pair) (cons (first pair) (second pair)))
                            (second datum))))))

;;; Authority report. A neutral, presentation-free description of a grant for an
;;; operator deciding how to confine the process. Distinct from grant->datum
;;; (durable storage): this renders constraints to human strings and sorts atoms
;;; for stable output, carrying no IO and no serialization format of its own.

(defun constraint->string (constraint)
  "A short human rendering of CONSTRAINT."
  (ecase (constraint-kind constraint)
    (:any "any")
    (:none "none")
    (:path-prefix (format nil "path under ~A" (second constraint)))
    (:numeric-bound (format nil "in [~@[~A~]..~@[~A~]]"
                            (second constraint) (third constraint)))
    (:enum (format nil "one of ~{~S~^, ~}" (cdr constraint)))))

(defun grant-report (grant)
  "GRANT as a display plist. SCOPE is :universal (covers everything),
:universal-except (covers everything but a listed :excluded set), :none (covers
nothing), or :bounded (a listed :capabilities set). A :bounded entry is
(:atom NAME :constraint STRING); an :excluded entry is the downcased atom name.
All names are downcased and sorted so the output is stable."
  (cond
    ((and (grant-universal grant) (grant-excluded grant))
     (list :scope :universal-except
           :excluded (sort (mapcar (lambda (cap)
                                     (string-downcase (symbol-name cap)))
                                   (copy-list (grant-excluded grant)))
                           #'string<)))
    ((grant-universal grant)
     (list :scope :universal :capabilities '()))
    (t (let ((caps (sort (copy-list (grant-capabilities grant))
                         #'string< :key #'symbol-name)))
         (list :scope (if caps :bounded :none)
               :capabilities
               (mapcar (lambda (cap)
                         (list :atom (string-downcase (symbol-name cap))
                               :constraint (constraint->string
                                            (grant-constraint grant cap))))
                       caps))))))

;;; Conferral. Delegation only attenuates: a grant the holder does not cover is
;;; unrepresentable as a conferral, not policed after the fact.

(define-condition grant-escalation (error)
  ((holder :initarg :holder :reader grant-escalation-holder)
   (requested :initarg :requested :reader grant-escalation-requested))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Conferral rejected: requested grant is not ~
                             covered by the holder's authority."))))

(defun confer (holder requested)
  "Confer REQUESTED when HOLDER covers it, else signal `grant-escalation`."
  (if (grant-covers-p holder requested)
      requested
      (error 'grant-escalation :holder holder :requested requested)))

;;; Lifted-tool coordinates. Each lifted tool carries a lattice coordinate:
;;; a serializable descriptor naming its gate atom and, optionally, an
;;; argument-aware constraint. The constraint extractor is rebuilt from the
;;; descriptor at each call -- never serialized -- so the coordinate rides tool
;;; metadata, the install pin, and snapshot storage as one plain s-expression.
;;;
;;;   (:atom ATOM)                                   ; plain gate
;;;   (:atom ATOM :constraint :path-prefix :arg KEY) ; argument-aware, single arg
;;;   (:atom ATOM :constraint :enum        :arg KEY)
;;;   (:atom ATOM :deriver NAME)                     ; constraint(s) derived from the call
;;;
;;; A deriver names a function the tool registers at load; the descriptor still
;;; carries only the name, so it stays serializable while the function is rebuilt
;;; per image. A deriver may yield several constraints, so one coordinate can fan
;;; out to several requests (one per resource a single call touches).

(defun lifted-tool-atom (server tool-name)
  "The per-tool gate atom ISOLATED/<server>/<tool>: a lifted tool's own lattice
coordinate, distinct from its siblings so authority attenuates one tool at a time."
  (intern (format nil "ISOLATED/~A/~A"
                  (symbol-name server) (string-upcase tool-name))
          :keyword))

(defun lifted-server-grant (server tool-names)
  "Coarse authority over a whole lifted server: the enumerated set of its
per-tool atoms. Self-contained -- no implication entry -- so it stays
per-protocol and snapshot-durable; a re-lift rebuilds it from the live tools."
  (make-grant :capabilities (mapcar (lambda (name) (lifted-tool-atom server name))
                                    tool-names)))

;; Defined in protocol.lisp, loaded after this file; forward-declared so the
;; calls below compile without a forward-reference warning.
(declaim (ftype (function (t t &optional t) t) tool-parameter)
         (ftype (function (t) t) normalize-extension-id))

(defun coordinate-request (coordinate args)
  "The request grant a call demands: COORDINATE's atom, constrained by the live
argument the descriptor names. ARGS is read through `tool-parameter`, so a plist
or a string-keyed hash-table (the agent's parsed-JSON arg shape) both work. Built
fresh per call so the constraint extractor never rides serialization."
  (destructuring-bind (&key atom constraint arg) coordinate
    (if constraint
        (let ((c (ecase constraint
                   (:path-prefix (path-prefix-constraint (tool-parameter args arg)))
                   (:enum        (enumerated-constraint (list (tool-parameter args arg)))))))
          (make-grant :atoms (list (cons atom c))))
        (make-grant :capabilities (list atom)))))

;;; Coordinate derivers. A tool whose gate constraints are not live named
;;; arguments -- they live in a patch body, or are a resolved command word --
;;; registers a function by name; the kernel holds the registry and stays
;;; ignorant of tool specifics. The descriptor carries the name only, so it
;;; serializes; the function is rebuilt per image at tool load.

(defvar *coordinate-derivers* (make-hash-table :test #'eq)
  "Maps a coordinate deriver name to a function of the call parameters returning
a list of constraints.")

(defun register-coordinate-deriver (name function)
  "Bind coordinate deriver NAME to FUNCTION. Idempotent on reload."
  (setf (gethash name *coordinate-derivers*) function))

(defun coordinate-deriver (name)
  "The function registered for deriver NAME, or an error if none is."
  (or (gethash name *coordinate-derivers*)
      (error "No coordinate deriver registered for ~S." name)))

(defun coordinate-requests (coordinate args)
  "The request grants a call demands: COORDINATE's atom under the constraint(s)
the live ARGS place. A literal-arg or plain coordinate yields one request; a
deriver may yield several. Built fresh per call so no extractor rides
serialization."
  (let ((atom (getf coordinate :atom))
        (deriver (getf coordinate :deriver)))
    (if deriver
        (mapcar (lambda (constraint) (make-grant :atoms (list (cons atom constraint))))
                (funcall (coordinate-deriver deriver) args))
        (list (coordinate-request coordinate args)))))

;;; Capability implications: a hand-written fragment of the order relation --
;;; holding a coarse capability confers the finer ones it implies. Resolved
;;; into the grant at construction so checks stay flat lookups.

(defvar *capability-implications*
  '(:image/recode (:manifest/install :manifest/retract)
    :tools/standard (:file/read :file/write :file/edit :process/exec)
    :manifest/install-remote (:manifest/install :image/eval)
    :image/load-native (:image/load-dep :image/eval)
    :image/load-dep (:image/eval)
    ;; Debug is the strongest :image/* atom: it implies eval (hence, transitively,
    ;; inspect) and adds the authority to pause and resume the running image.
    :image/debug (:image/eval)
    ;; Evaluating in the live image is full first-party authority: a form can
    ;; write/edit files, run processes, and spawn helpers. Eval cannot be held
    ;; while confined to read-only first-party tools. Eval does NOT imply the
    ;; strictly stronger debug atom.
    :image/eval (:file/write :file/edit :process/exec :extension/spawn-process)
    ;; Eval confers read-only inspect (the weakest :image/* atom; a leaf). A
    ;; repeated key is additive -- expand-implications unions its values -- so
    ;; this leaves the entry above untouched.
    :image/eval (:image/inspect)
    ;; Paging a truncated tool result back (:result/read) re-exposes output the
    ;; producing tool already returned, so any authority that can produce a
    ;; spill confers it. Eval reaches it transitively via process/exec and
    ;; inspect.
    :file/read (:result/read)
    :process/exec (:result/read)
    :image/inspect (:result/read))
  "Plist mapping a coarse capability to the finer capabilities it implies.
Resolved at subject construction so `check-capability` stays an O(1) flat
lookup. Additive: a fresh key, or a repeated key whose values
`expand-implications` unions.")

(defun expand-implications (capabilities)
  "Close CAPABILITIES under *capability-implications* (transitive)."
  (let ((set (remove-duplicates capabilities))
        (changed t))
    (loop while changed do
      (setf changed nil)
      (dolist (cap (copy-list set))
        (dolist (implied (loop for (key value) on *capability-implications* by #'cddr
                               when (eq key cap) append value))
          (unless (member implied set)
            (push implied set)
            (setf changed t)))))
    set))

(defun grant-from-capabilities (capabilities)
  "A grant conferring the implication-closure of CAPABILITIES, each
unconstrained (the keyword bag as the degenerate powerset instance)."
  (make-grant :capabilities (expand-implications capabilities)))

(defun capability-request (capability)
  "Lift a check target to a request grant. A keyword becomes a single-atom
unconstrained request; a grant passes through, so argument-aware checks can
supply their own constraints."
  (if (grant-p capability)
      capability
      (make-grant :capabilities (list capability))))

(defclass subject ()
  ((grant
    :reader subject-grant
    :initarg :grant
    :initform (grant-bottom)))
  (:documentation "A capability-bearing caller. The kernel knows nothing about
callers beyond the authority their grant carries."))

(defclass system-subject (subject) ()
  (:documentation "A subject that passes every capability check -- the top of
the authority lattice. Reserved for boot and substrate that legitimately holds
full authority, taken explicitly via `with-system-authority`."))

(defmethod subject-grant ((subject system-subject))
  (grant-top))

(define-condition capability-denied (error)
  ((subject :initarg :subject :reader capability-denied-subject)
   (capability :initarg :capability :reader capability-denied-capability))
  (:report (lambda (condition stream)
             (format stream "Subject ~S lacks capability ~S."
                     (capability-denied-subject condition)
                     (capability-denied-capability condition)))))

(defgeneric check-capability (subject capability)
  (:documentation "Return non-nil if SUBJECT's grant covers CAPABILITY -- a
keyword, or a request grant for argument-aware checks. Specialize on a subject
subclass to implement a different policy."))

(defmethod check-capability ((subject subject) capability)
  (grant-covers-p (subject-grant subject) (capability-request capability)))

(defmethod check-capability ((subject system-subject) capability)
  (declare (ignore capability))
  t)

(defgeneric subject-capabilities (subject)
  (:documentation "The capability keywords SUBJECT confers."))

(defmethod subject-capabilities ((subject subject))
  (grant-capabilities (subject-grant subject)))

(defmethod subject-capabilities ((subject system-subject))
  '())

(defun make-subject (&key capabilities grant)
  "Construct a subject. GRANT, when supplied, is used verbatim; otherwise the
keyword CAPABILITIES list is closed under implications and embedded as an
unconstrained grant."
  (make-instance 'subject
                 :grant (or grant (grant-from-capabilities capabilities))))

(defun make-system-subject ()
  "Construct the privileged system subject: the top of the lattice."
  (make-instance 'system-subject))

(defun make-default-subject ()
  "The narrow default caller: empty authority, denies every gated capability.
The ambient *call-subject* and an agent built without explicit authority both
start here, so untrusted work is attenuated unless authority is granted."
  (make-subject :grant (grant-bottom)))

(defun make-unrestricted-subject ()
  "An ordinary subject holding the full lattice: every capability, unconstrained.
The fallback authority a user-facing root agent boots with when no \"capabilities\"
key bounds it -- capability enforcement begins only once a capabilities policy is
declared. Distinct from `make-system-subject`: this is a policy grant, an ordinary
`subject` over `grant-top`, not the substrate authority `system-subject` marks, so
it attenuates like any other grant (profile overlays, headless flags)."
  (make-subject :grant (grant-top)))

(defun subject-meet (a b)
  "Greatest lower bound of two subjects: a subject conferring exactly the
authority both confer. The result is covered by each input, so meeting only
ever attenuates -- it cannot widen either side."
  (make-subject :grant (grant-meet (subject-grant a) (subject-grant b))))

(defun capabilities-subject (value)
  "Subject from the raw \"capabilities\" settings VALUE, plus a second value that
is true exactly when the key was present.

Returns (values NIL NIL) when VALUE is NIL (key absent), so the caller applies
its own fallback. When VALUE is present returns (values SUBJECT T):

 - a scalar string grants that one capability (singleton shorthand);
 - a flat sequence of strings grants exactly the implication-closure of those
   capabilities; an empty sequence grants nothing (the bottom subject);
 - non-string entries -- including nested sequences -- warn and are dropped; if
   no valid entry remains the subject is bottom;
 - a present value that is neither a string nor a sequence warns and yields the
   bottom subject.

Presence thus opts into enforcement: a present-but-malformed key still bounds
authority (to the valid subset, or to nothing) rather than silently disabling
the restriction. Capability strings are normalized through `normalize-extension-id`,
so \"file/read\", \":file/read\", and \"FILE/READ\" denote the same atom."
  (cond
    ((null value) (values nil nil))
    ((stringp value)
     (values (make-subject
              :capabilities (list (normalize-extension-id value)))
             t))
    ((typep value 'sequence)
     (let ((valid '())
           (any-invalid nil))
       (map nil (lambda (entry)
                  (if (stringp entry)
                      (push (normalize-extension-id entry) valid)
                      (setf any-invalid t)))
            value)
       (when any-invalid
         (warn "kli: ignoring invalid capabilities entries (each must be a string)"))
       (values (make-subject :capabilities (nreverse valid)) t)))
    (t
     (warn "kli: ignoring capabilities settings (not a string or array of strings)")
     (values (make-subject :grant (grant-bottom)) t))))

(defvar *call-subject* (make-default-subject)
  "The current capability-bearing caller. Per-thread, stack-scoped. Rebind via
`let` at API entry points or before sub-calls that should run under restricted
authority. Defaults to empty authority; substrate that legitimately needs the
full lattice takes it explicitly with `with-system-authority`.")

(defmacro with-system-authority (&body body)
  "Run BODY with *call-subject* bound to the system subject. Names the boundary
where substrate -- boot, restore, trusted fixtures -- deliberately holds the
full lattice."
  `(let ((*call-subject* (make-system-subject)))
     ,@body))

(defparameter *install-subject*
  (make-subject :capabilities '(:manifest/install
                                :manifest/retract
                                :image/eval
                                :extension/spawn-process
                                :manifest/install-remote
                                :auth/register-reference))
  "Bounded authority for loading extensions: installing manifests, evaluating
source extensions, spawning the helper processes a remote pin replay needs, and
registering the credential references a provider contributes at install time.
Evaluating source is first-party image authority, so this subject also confers
the file and process atoms eval implies; it carries no interactive session
authority and is not the lattice top. Bind it around extension-load boundaries
instead of taking the full lattice.")

(defparameter *ui-subject*
  (make-subject :capabilities '(:agent/session/submit
                                :agent/session/switch
                                :agent/session/branch
                                :agent/session/listen
                                :agent/session/compact
                                :agent/session/focus
                                :context/read
                                :context/stage-edit
                                :context/commit-edit
                                :context/seal
                                :auth/register-reference
                                :auth/forget-secret
                                :auth/resolve-secret
                                :auth/read-metadata
                                :protocol/create
                                :protocol/switch
                                :protocol/rollback
                                :protocol/recover
                                :protocol/snapshot
                                :protocol/restore
                                :manifest/install
                                :manifest/retract
                                :behavior/hotpatch
                                :behavior/state))
  "Bounded authority for the interactive first-party surface: the commands a
user drives through the terminal and the session machinery the terminal builds
and runs. It omits the model's own actuators (file write/edit, process exec,
in-image eval), which are conferred per tool call on a narrower subject. A test
certifies that every registered first-party command's required authority is
covered here, so a new command that needs new authority fails the suite rather
than a user's session.")

(defun require-capability (capability)
  "Signal `capability-denied` if *call-subject* does not pass CAPABILITY."
  (unless (check-capability *call-subject* capability)
    (error 'capability-denied
           :subject *call-subject*
           :capability capability)))
