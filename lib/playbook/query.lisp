;;; PQ — Playbook Query Language
;;;
;;; A Lisp-native query language for the playbook pattern graph.
;;; Two interfaces:
;;;   1. Direct functions: for Lisp programmers
;;;   2. `interpret-query`: for MCP/Claude, safely interprets S-expressions
;;;
;;; The interpreter uses read-from-string with *read-eval* nil to get the
;;; S-expression AST, then walks it — never calling eval.

(defpackage #:pq
  (:use #:cl)
  (:export
   ;; Dynamic context
   #:*patterns*
   #:*mutation-handler*
   #:with-patterns

   ;; Starting expressions (produce pattern-sets)
   #:all-patterns
   #:pattern-by-id
   #:search-patterns
   #:proven-patterns
   #:warning-patterns
   #:activate-patterns

   ;; Named queries
   #:*named-queries*
   #:define-query
   #:register-query
   #:list-queries

   ;; Pipeline primitives
   #:where-step
   #:sort-by-field
   #:take-n
   #:ids-step
   #:count-step
   #:group-by-step
   #:edges-step
   #:history-step
   #:full-step
   #:*full-display*
   #:full-result
   #:full-result-p
   #:full-result-patterns
   #:group-by-result
   #:group-by-result-p
   #:group-by-result-groups
   #:group-by-result-field

   ;; Predicates (function-based)
   #:domain=
   #:field>
   #:field<
   #:field>=
   #:has-field
   #:content-matches
   #:pred-and
   #:pred-or
   #:pred-not

   ;; Safe interpreter (for MCP)
   #:safe-read-query
   #:interpret-query
   #:format-query-result

   ;; Mutation results
   #:mutation-log
   #:mutation-log-p
   #:mutation-log-entries
   #:mutation-log-errors
   #:mutation-entry
   #:mutation-entry-pattern-id
   #:mutation-entry-operation
   #:mutation-entry-result
   #:mutation-entry-message

   ;; Conditions
   #:pq-error
   #:pq-error-message
   #:pq-parse-error
   #:mutation-without-handler))

(in-package #:pq)

;;; ============================================================
;;; DYNAMIC CONTEXT
;;; ============================================================

(defvar *patterns* nil
  "Function returning list of all patterns. Bound by server.")

(defvar *mutation-handler* nil
  "Function (op pattern-id &rest args) for executing mutations.
   Nil in lib/playbook, bound by playbook-mcp server.")

(defvar *search-fn* nil
  "Function (query &key limit) for semantic search. Bound by server.")

(defvar *activate-fn* nil
  "Function (query domains &key top-k) for graph activation. Bound by server.")

(defvar *edges-fn* nil
  "Function (pattern-id) returning edges for a pattern. Bound by server.")

(defvar *history-fn* nil
  "Function (pattern-id) returning evolution history. Bound by server.")

(defvar *full-display* t
  "When T, format-pattern-entry shows full content without truncation.
   Default is T so patterns display fully by default.")

(defun current-patterns ()
  "Return all patterns from the bound function."
  (if *patterns*
      (funcall *patterns*)
      (error 'pq-error :message "*patterns* not bound")))

(defmacro with-patterns ((&key patterns mutation-handler search-fn
                               activate-fn edges-fn history-fn)
                         &body body)
  "Execute BODY with pattern context bound."
  `(let ((*patterns* (or ,patterns *patterns*))
         (*mutation-handler* (or ,mutation-handler *mutation-handler*))
         (*search-fn* (or ,search-fn *search-fn*))
         (*activate-fn* (or ,activate-fn *activate-fn*))
         (*edges-fn* (or ,edges-fn *edges-fn*))
         (*history-fn* (or ,history-fn *history-fn*)))
     ,@body))

;;; ============================================================
;;; PATTERN-SET TYPE
;;; A pattern-set is a list of pattern structs (from playbook-mcp).
;;; We access pattern fields via generic accessors.
;;; ============================================================

(defun pattern-set-count (pattern-set)
  "Count patterns in a pattern-set."
  (length pattern-set))

(defun pattern-set-ids (pattern-set)
  "Extract just the IDs from a pattern-set."
  (mapcar #'pattern-id-accessor pattern-set))

;;; Generic accessors that work with playbook-mcp::pattern structs
(defun pattern-id-accessor (p)
  "Get pattern ID. Works with playbook-mcp::pattern."
  (funcall (find-accessor :id) p))

(defun pattern-domain-accessor (p)
  "Get pattern domain."
  (funcall (find-accessor :domain) p))

(defun pattern-content-accessor (p)
  "Get pattern content."
  (funcall (find-accessor :content) p))

(defun pattern-helpful-accessor (p)
  "Get pattern helpful count."
  (funcall (find-accessor :helpful) p))

(defun pattern-harmful-accessor (p)
  "Get pattern harmful count."
  (funcall (find-accessor :harmful) p))

(defun pattern-embedding-accessor (p)
  "Get pattern embedding."
  (funcall (find-accessor :embedding) p))

(defun pattern-history-accessor (p)
  "Get pattern evolution history."
  (funcall (find-accessor :evolution-history) p))

(defvar *accessor-cache* (make-hash-table :test 'eq)
  "Cache for pattern field accessors.")

(defvar *pattern-packages* '(:playbook-mcp :kli-pattern)
  "Packages to search for pattern struct accessors.
   Checked in order; first package found with the accessor wins.")

(defun find-accessor (field)
  "Find or create accessor function for FIELD.
   Searches *pattern-packages* for a PATTERN-<field> accessor."
  (or (gethash field *accessor-cache*)
      (setf (gethash field *accessor-cache*)
            (let ((accessor-name (format nil "PATTERN-~A"
                                         (string-upcase
                                          (if (eq field :id) "ID"
                                              (symbol-name field))))))
              ;; Try each known pattern package
              (dolist (pkg-name *pattern-packages*)
                (let* ((pkg (find-package pkg-name))
                       (sym (when pkg (find-symbol accessor-name pkg))))
                  (when (and sym (fboundp sym))
                    (return-from find-accessor
                      (setf (gethash field *accessor-cache*)
                            (symbol-function sym))))))
              ;; Fallback: slot access from first available package
              (let ((pkg (some #'find-package *pattern-packages*)))
                (lambda (p)
                  (if (and pkg (typep p (find-symbol "PATTERN" pkg)))
                      (slot-value p (intern (symbol-name field) pkg))
                      nil)))))))

(defun pattern-field (pattern field)
  "Get FIELD value from PATTERN."
  (case field
    (:id (pattern-id-accessor pattern))
    (:domain (pattern-domain-accessor pattern))
    (:content (pattern-content-accessor pattern))
    (:helpful (pattern-helpful-accessor pattern))
    (:harmful (pattern-harmful-accessor pattern))
    (:embedding (pattern-embedding-accessor pattern))
    (:evolution-history (pattern-history-accessor pattern))
    (t (funcall (find-accessor field) pattern))))

;;; ============================================================
;;; STARTING EXPRESSIONS
;;; These produce initial pattern-sets.
;;; ============================================================

(defun all-patterns ()
  "Return all patterns as a pattern-set."
  (current-patterns))

(defun pattern-by-id (id)
  "Return pattern with matching ID, or nil."
  (let ((patterns (current-patterns)))
    (find id patterns :key #'pattern-id-accessor :test #'string=)))

(defun search-patterns (query &key (limit 10))
  "Semantic search for patterns matching QUERY."
  (if *search-fn*
      (funcall *search-fn* query :limit limit)
      (error 'pq-error :message "Semantic search requires *search-fn* binding")))

(defun proven-patterns (&key (min-helpful 3) (limit 20))
  "Return patterns with helpful >= MIN-HELPFUL."
  (let ((results (remove-if-not
                  (lambda (p) (>= (or (pattern-helpful-accessor p) 0) min-helpful))
                  (current-patterns))))
    (subseq (sort results #'> :key #'pattern-helpful-accessor)
            0 (min limit (length results)))))

(defun warning-patterns (&key (limit 20))
  "Return patterns with harmful > 0."
  (let ((results (remove-if-not
                  (lambda (p) (> (or (pattern-harmful-accessor p) 0) 0))
                  (current-patterns))))
    (subseq (sort results #'> :key #'pattern-harmful-accessor)
            0 (min limit (length results)))))

(defun activate-patterns (query &key boost (top-k 5))
  "Graph-powered pattern retrieval (domain-agnostic).
   QUERY: natural language query
   BOOST: optional list of domain strings to emphasize
   TOP-K: number of results"
  (if *activate-fn*
      (funcall *activate-fn* query :boost boost :top-k top-k)
      (error 'pq-error :message "Activation requires *activate-fn* binding")))

;;; ============================================================
;;; NAMED QUERIES
;;; Pre-defined queries accessible by name via (query "name").
;;; ============================================================

(defvar *named-queries* (make-hash-table :test 'equal)
  "Registry of named queries. Maps name -> thunk returning pattern-set.")

(defmacro define-query (name &body body)
  "Define a named query. BODY should return a pattern-set."
  `(setf (gethash ,name *named-queries*)
         (lambda () ,@body)))

(defun register-query (name thunk)
  "Register a named query function."
  (setf (gethash name *named-queries*) thunk))

(defun query (name)
  "Execute a named query by NAME. Returns a pattern-set."
  (let ((thunk (gethash name *named-queries*)))
    (unless thunk
      (error 'pq-error :message (format nil "Unknown query: ~A" name)))
    (funcall thunk)))

(defun list-queries ()
  "List all registered query names."
  (let ((names nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k names))
             *named-queries*)
    (sort names #'string<)))

;; Pre-registered queries
(define-query "proven"
  "Patterns with helpful >= 3."
  (proven-patterns :min-helpful 3))

(define-query "warnings"
  "Patterns marked harmful."
  (warning-patterns))

(define-query "orphans"
  "Patterns with no graph edges."
  (if *edges-fn*
      (remove-if (lambda (p)
                   (let ((edges (funcall *edges-fn* (pattern-id-accessor p))))
                     (> (length edges) 0)))
                 (current-patterns))
      (error 'pq-error :message "orphans query requires *edges-fn* binding")))

(define-query "embedded"
  "Patterns with embeddings."
  (remove-if-not #'pattern-embedding-accessor (current-patterns)))

(define-query "unembedded"
  "Patterns without embeddings."
  (remove-if #'pattern-embedding-accessor (current-patterns)))

;; Domain shortcuts
(define-query "lisp"
  "Patterns in the lisp domain."
  (remove-if-not (lambda (p) (string-equal (pattern-domain-accessor p) "lisp"))
                 (current-patterns)))

(define-query "nix"
  "Patterns in the nix domain."
  (remove-if-not (lambda (p) (string-equal (pattern-domain-accessor p) "nix"))
                 (current-patterns)))

(define-query "ace"
  "Patterns in the ace domain."
  (remove-if-not (lambda (p) (string-equal (pattern-domain-accessor p) "ace"))
                 (current-patterns)))

(define-query "nixos"
  "Patterns in the nixos domain."
  (remove-if-not (lambda (p) (string-equal (pattern-domain-accessor p) "nixos"))
                 (current-patterns)))

;;; ============================================================
;;; PIPELINE PRIMITIVES
;;; These transform pattern-sets.
;;; ============================================================

(defun where-step (pattern-set predicate)
  "Filter PATTERN-SET to patterns satisfying PREDICATE.
   PREDICATE is a function (pattern) -> boolean."
  (remove-if-not predicate pattern-set))

(defun sort-by-field (pattern-set field &key (descending t))
  "Sort PATTERN-SET by FIELD value. Detects numeric vs string fields."
  (when (null pattern-set) (return-from sort-by-field nil))
  (let* ((numeric-p (loop for p in pattern-set
                          for val = (pattern-field p field)
                          when val return (numberp val))))
    (sort (copy-list pattern-set)
          (if numeric-p
              (if descending #'> #'<)
              (if descending #'string> #'string<))
          :key (lambda (p)
                 (let ((val (pattern-field p field)))
                   (if numeric-p
                       (or val 0)
                       (if (stringp val) val (princ-to-string (or val "")))))))))

(defun take-n (pattern-set n)
  "Return first N patterns from PATTERN-SET."
  (subseq pattern-set 0 (min n (length pattern-set))))

(defun ids-step (pattern-set)
  "Extract just the IDs from PATTERN-SET. Returns a list of strings."
  (pattern-set-ids pattern-set))

(defun count-step (pattern-set)
  "Count patterns in PATTERN-SET. Returns an integer."
  (pattern-set-count pattern-set))

(defstruct group-by-result
  "Result of a group-by operation. Wraps alist to avoid format heuristic ambiguity."
  (groups nil :type list)    ; alist of (value . patterns)
  (field nil :type symbol))  ; field grouped by

(defun group-by-step (pattern-set field)
  "Group PATTERN-SET by FIELD value. Returns a group-by-result struct."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (p pattern-set)
      (let ((val (or (pattern-field p field) :nil)))
        (push p (gethash val groups))))
    ;; Convert to sorted alist
    (let ((result nil))
      (maphash (lambda (k v)
                 (push (cons k (nreverse v)) result))
               groups)
      (make-group-by-result
       :groups (sort result #'> :key (lambda (g) (length (cdr g))))
       :field field))))

(defun edges-step (pattern-set)
  "Get edges for each pattern in PATTERN-SET."
  (unless *edges-fn*
    (error 'pq-error :message ":edges requires *edges-fn* binding"))
  (mapcar (lambda (p)
            (cons (pattern-id-accessor p)
                  (funcall *edges-fn* (pattern-id-accessor p))))
          pattern-set))

(defun history-step (pattern-set)
  "Get evolution history for each pattern in PATTERN-SET."
  (unless *history-fn*
    (error 'pq-error :message ":history requires *history-fn* binding"))
  (mapcar (lambda (p)
            (cons (pattern-id-accessor p)
                  (funcall *history-fn* (pattern-id-accessor p))))
          pattern-set))

(defstruct full-result
  "Wrapper indicating pattern-set should be displayed with full content."
  patterns)

(defun full-step (pattern-set)
  "Mark pattern set for full content display (no truncation).
   Wraps result in FULL-RESULT struct recognized by format-query-result."
  (make-full-result :patterns pattern-set))

;;; ============================================================
;;; PREDICATES (function-based)
;;; These return functions suitable for WHERE-STEP.
;;; ============================================================

(defun domain= (domain)
  "Return predicate: pattern domain equals DOMAIN."
  (lambda (p)
    (string-equal (pattern-domain-accessor p)
                  (if (keywordp domain)
                      (string-downcase (symbol-name domain))
                      domain))))

(defun field> (field value)
  "Return predicate: field > value."
  (lambda (p)
    (let ((v (pattern-field p field)))
      (and (numberp v) (> v value)))))

(defun field< (field value)
  "Return predicate: field < value."
  (lambda (p)
    (let ((v (pattern-field p field)))
      (and (numberp v) (< v value)))))

(defun field>= (field value)
  "Return predicate: field >= value."
  (lambda (p)
    (let ((v (pattern-field p field)))
      (and (numberp v) (>= v value)))))

(defun has-field (field)
  "Return predicate: field exists and is non-nil."
  (lambda (p)
    (pattern-field p field)))

(defun content-matches (substring)
  "Return predicate: content contains SUBSTRING."
  (lambda (p)
    (let ((content (pattern-content-accessor p)))
      (and content (search substring content :test #'char-equal)))))

(defun pred-and (&rest predicates)
  "Return predicate: all PREDICATES are true."
  (lambda (p)
    (every (lambda (pred) (funcall pred p)) predicates)))

(defun pred-or (&rest predicates)
  "Return predicate: any PREDICATE is true."
  (lambda (p)
    (some (lambda (pred) (funcall pred p)) predicates)))

(defun pred-not (predicate)
  "Return predicate: PREDICATE is false."
  (lambda (p)
    (not (funcall predicate p))))

;;; ============================================================
;;; CONDITIONS
;;; ============================================================

(define-condition pq-error (error)
  ((message :initarg :message :reader pq-error-message))
  (:report (lambda (c s)
             (format s "PQ error: ~A" (pq-error-message c)))))

(define-condition pq-parse-error (pq-error)
  ()
  (:report (lambda (c s)
             (format s "PQ parse error: ~A" (pq-error-message c)))))

(define-condition mutation-without-handler (pq-error)
  ((operation :initarg :operation :reader mutation-operation))
  (:report (lambda (c s)
             (format s "Mutation ~A requires *mutation-handler* binding"
                     (mutation-operation c)))))

;;; ============================================================
;;; SAFE INTERPRETER (for MCP/Claude)
;;; ============================================================

(defun safe-read-query (query-string)
  "Safely read a query string into an S-expression.
   Disables *read-eval* to prevent #. attacks."
  (let ((*read-eval* nil)
        (*package* (find-package :pq)))
    (handler-case
        (read-from-string query-string)
      (error (c)
        (error 'pq-parse-error :message (format nil "~A" c))))))

(defun interpret-query (form)
  "Interpret a PQ query form. Returns a pattern-set, list, or scalar."
  (interpret-expr form))

(defun sym= (sym name)
  "Check if SYM has the given NAME (case-insensitive, ignoring package)."
  (and (symbolp sym)
       (string-equal (symbol-name sym) name)))

(defun interpret-expr (form)
  "Interpret a single expression."
  (cond
    ;; Keywords
    ((eq form :all) (all-patterns))

    ;; Atoms - return as-is
    ((atom form) form)

    ;; Pipeline
    ((sym= (car form) "->")
     (interpret-pipeline (cdr form)))

    ;; Starting expressions
    ((sym= (car form) "PATTERN")
     (let ((p (pattern-by-id (second form))))
       (if p (list p) nil)))

    ((sym= (car form) "SEARCH")
     (let* ((query (second form))
            (limit (or (getf (cddr form) :limit) 10)))
       (search-patterns query :limit limit)))

    ((sym= (car form) "PROVEN")
     (let ((min-helpful (or (getf (cdr form) :min) 3))
           (limit (or (getf (cdr form) :limit) 20)))
       (proven-patterns :min-helpful min-helpful :limit limit)))

    ((sym= (car form) "WARNINGS")
     (let ((limit (or (getf (cdr form) :limit) 20)))
       (warning-patterns :limit limit)))

    ((sym= (car form) "ACTIVATE")
     (let* ((query (second form))
            (args (cddr form))
            (domains (getf args :domains))
            (boost (getf args :boost))
            (top-k (or (getf args :top-k) 5)))
       ;; Reject legacy :domains syntax
       (when domains
         (error 'pq-error
                :message ":domains is no longer supported. Use (activate \"query\") or (activate \"query\" :boost (domain ...))"))
       ;; Convert boost to list of strings
       (let ((boost-list (when boost
                           (if (listp boost)
                               (mapcar (lambda (d)
                                         (if (keywordp d)
                                             (string-downcase (symbol-name d))
                                             d))
                                       boost)
                               (list (if (keywordp boost)
                                         (string-downcase (symbol-name boost))
                                         boost))))))
         (activate-patterns query :boost boost-list :top-k top-k))))

    ((sym= (car form) "QUERY")
     (query (second form)))

    ;; Mutation starting expression: (add! :domain :lisp :content "...")
    ((sym= (car form) "ADD!")
     (execute-add-mutation (cdr form)))

    (t
     (error 'pq-error :message (format nil "Unknown expression: ~S" form)))))

;;; ============================================================
;;; MUTATION EXECUTION
;;; ============================================================

(defstruct mutation-log
  "Result of mutation execution."
  (entries nil :type list)     ; list of mutation-entry
  (errors 0 :type integer))    ; count of errors

(defstruct mutation-entry
  "Single mutation result."
  pattern-id
  operation
  args
  result    ; :success or error message
  message)  ; human-readable message

(defun mutation-step-p (sym)
  "Check if SYM is a mutation step (ends with !)."
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 0)
              (char= (char name (1- (length name))) #\!)))))

(defun normalize-mutation-op (op)
  "Convert :feedback! keyword to :feedback symbol for handler dispatch."
  (let ((name (symbol-name op)))
    ;; Strip trailing !
    (intern (subseq name 0 (1- (length name))) :keyword)))

(defun execute-mutation-step (pattern-set step)
  "Execute a mutation step on all patterns in PATTERN-SET.
   Returns a mutation-log with results for each pattern."
  (unless *mutation-handler*
    (error 'mutation-without-handler :operation (car step)))
  (let* ((op (car step))
         (args (cdr step))
         (op-name (normalize-mutation-op op))
         (log (make-mutation-log)))
    (dolist (p pattern-set)
      (let ((pattern-id (pattern-id-accessor p)))
        (handler-case
            (let ((result (apply *mutation-handler* op-name pattern-id args)))
              (push (make-mutation-entry
                     :pattern-id pattern-id
                     :operation op-name
                     :args args
                     :result :success
                     :message (or result "ok"))
                    (mutation-log-entries log)))
          (error (c)
            (push (make-mutation-entry
                   :pattern-id pattern-id
                   :operation op-name
                   :args args
                   :result :error
                   :message (format nil "~A" c))
                  (mutation-log-entries log))
            (incf (mutation-log-errors log))))))
    (setf (mutation-log-entries log) (nreverse (mutation-log-entries log)))
    log))

(defun execute-add-mutation (args)
  "Execute (add! :domain :lisp :content \"...\").
   Returns a mutation-log."
  (unless *mutation-handler*
    (error 'mutation-without-handler :operation 'add!))
  (let* ((domain (getf args :domain))
         (content (getf args :content))
         (log (make-mutation-log)))
    (unless domain
      (error 'pq-error :message "add! requires :domain"))
    (unless content
      (error 'pq-error :message "add! requires :content"))
    (let ((domain-str (if (keywordp domain)
                          (string-downcase (symbol-name domain))
                          domain)))
      (handler-case
          (let ((result (funcall *mutation-handler* :add nil
                                 :domain domain-str :content content)))
            (push (make-mutation-entry
                   :pattern-id (or result "new")
                   :operation :add
                   :args (list :domain domain-str :content content)
                   :result :success
                   :message (format nil "Added pattern ~A" result))
                  (mutation-log-entries log)))
        (error (c)
          (push (make-mutation-entry
                 :pattern-id nil
                 :operation :add
                 :args (list :domain domain-str :content content)
                 :result :error
                 :message (format nil "~A" c))
                (mutation-log-entries log))
          (incf (mutation-log-errors log)))))
    log))

(defun interpret-pipeline (forms)
  "Interpret a pipeline: (start step1 step2 ...)"
  (let ((result (interpret-expr (first forms))))
    (dolist (step (rest forms))
      (setf result (interpret-step result step)))
    result))

(defun interpret-step (pattern-set step)
  "Interpret a single pipeline step."
  (cond
    ;; Bare keywords
    ((eq step :ids) (ids-step pattern-set))
    ((eq step :count) (count-step pattern-set))
    ((eq step :edges) (edges-step pattern-set))
    ((eq step :history) (history-step pattern-set))
    ((eq step :full) (full-step pattern-set))

    ;; List forms with keyword car
    ((and (listp step) (eq (car step) :where))
     (where-step pattern-set (interpret-where (second step))))
    ((and (listp step) (eq (car step) :sort))
     (sort-by-field pattern-set (second step)))
    ((and (listp step) (eq (car step) :take))
     (take-n pattern-set (second step)))
    ((and (listp step) (eq (car step) :group-by))
     (group-by-step pattern-set (second step)))

    ;; Mutation steps - execute via handler
    ((and (listp step) (mutation-step-p (car step)))
     (execute-mutation-step pattern-set step))

    (t
     (error 'pq-error :message (format nil "Unknown step: ~S" step)))))

(defun interpret-where (pred-form)
  "Convert a declarative predicate form to a function."
  (cond
    ;; (domain= :lisp)
    ((and (listp pred-form) (sym= (car pred-form) "DOMAIN="))
     (domain= (second pred-form)))

    ;; (> :helpful N)
    ((and (listp pred-form) (sym= (car pred-form) ">"))
     (field> (second pred-form) (third pred-form)))

    ;; (< :harmful N)
    ((and (listp pred-form) (sym= (car pred-form) "<"))
     (field< (second pred-form) (third pred-form)))

    ;; (>= :helpful N)
    ((and (listp pred-form) (sym= (car pred-form) ">="))
     (field>= (second pred-form) (third pred-form)))

    ;; (has :embedding)
    ((and (listp pred-form) (sym= (car pred-form) "HAS"))
     (has-field (second pred-form)))

    ;; (matches "pattern")
    ((and (listp pred-form) (sym= (car pred-form) "MATCHES"))
     (content-matches (second pred-form)))

    ;; (and pred1 pred2 ...)
    ((and (listp pred-form) (sym= (car pred-form) "AND"))
     (apply #'pred-and (mapcar #'interpret-where (cdr pred-form))))

    ;; (or pred1 pred2 ...)
    ((and (listp pred-form) (sym= (car pred-form) "OR"))
     (apply #'pred-or (mapcar #'interpret-where (cdr pred-form))))

    ;; (not pred)
    ((and (listp pred-form) (sym= (car pred-form) "NOT"))
     (pred-not (interpret-where (second pred-form))))

    (t
     (error 'pq-error
            :message (format nil "Unknown predicate: ~S" pred-form)))))

;;; ============================================================
;;; RESULT FORMATTING (for MCP response)
;;; ============================================================

(defun format-query-result (result)
  "Format a query result for MCP text response."
  (cond
    ;; Full-result wrapper (from :full step) - use full content display
    ((full-result-p result)
     (let ((*full-display* t))
       (format-query-result (full-result-patterns result))))

    ;; Integer (from :count)
    ((integerp result)
     (format nil "~D" result))

    ;; Mutation log
    ((mutation-log-p result)
     (format-mutation-log result))

    ;; List of strings (from :ids)
    ((and (listp result) (every #'stringp result))
     (format nil "~D pattern~:P:~%~{- ~A~%~}"
             (length result) result))

    ;; Edges result: alist of (id . edges)
    ((and (listp result)
          (every #'consp result)
          (every (lambda (pair)
                   (stringp (car pair)))
                 result)
          (some (lambda (pair)
                  (and (listp (cdr pair))
                       (or (null (cdr pair))
                           (and (consp (cadr pair))
                                ;; Check for edge-like structure
                                (not (stringp (cadr pair)))))))
                 result))
     (format-edges-result result))

    ;; History result: alist of (id . history)
    ((and (listp result)
          (every #'consp result)
          (every (lambda (pair)
                   (and (stringp (car pair))
                        (listp (cdr pair))))
                 result)
          (some (lambda (pair)
                  (and (cdr pair)
                       (listp (cadr pair))
                       (>= (length (cadr pair)) 2)
                       (integerp (first (cadr pair)))))  ; timestamp
                 result))
     (format-history-result result))

    ;; Group-by result (struct-based dispatch)
    ((group-by-result-p result)
     (let ((groups (group-by-result-groups result)))
       (with-output-to-string (s)
         (format s "~D group~:P:~%" (length groups))
         (dolist (group groups)
           (let ((key (car group))
                 (patterns (cdr group)))
             (format s "~%## ~A (~D)~%" key (length patterns))
             (dolist (p patterns)
               (format s "~A~%" (format-pattern-entry p))))))))

    ;; Pattern-set (list of pattern structs)
    ((and (listp result)
          (every #'pattern-like-p result))
     (format nil "~D pattern~:P:~%~{~A~%~}"
             (length result)
             (mapcar #'format-pattern-entry result)))

    ;; Other
    (t
     (format nil "~S" result))))

(defun pattern-like-p (obj)
  "Check if OBJ looks like a pattern struct."
  (and obj
       (not (stringp obj))
       (not (numberp obj))
       (handler-case
           (and (pattern-id-accessor obj)
                (pattern-domain-accessor obj))
         (error () nil))))

(defun format-mutation-log (log)
  "Format a mutation-log for display."
  (with-output-to-string (s)
    (let ((entries (mutation-log-entries log))
          (errors (mutation-log-errors log)))
      (format s "Mutation log: ~D operation~:P" (length entries))
      (when (> errors 0)
        (format s ", ~D error~:P" errors))
      (format s "~%")
      (dolist (entry entries)
        (format s "  ~A ~A: ~A~%"
                (if (eq (mutation-entry-result entry) :success) "✓" "✗")
                (or (mutation-entry-pattern-id entry) "new")
                (mutation-entry-message entry))))))

(defun format-edges-result (result)
  "Format edges result."
  (with-output-to-string (s)
    (format s "Edges for ~D pattern~:P:~%" (length result))
    (dolist (pair result)
      (let ((id (car pair))
            (edges (cdr pair)))
        (format s "~%[~A]: ~D edge~:P~%" id (length edges))
        (dolist (e edges)
          (format s "  → ~A~%" e))))))

(defun format-history-result (result)
  "Format history result."
  (with-output-to-string (s)
    (format s "History for ~D pattern~:P:~%" (length result))
    (dolist (pair result)
      (let ((id (car pair))
            (history (cdr pair)))
        (format s "~%[~A]: ~D version~:P~%" id (length history))
        (loop for entry in history
              for i from 1
              do (format s "  v~D: ~A~%" i
                         (if (listp entry)
                             (second entry)  ; reason
                             entry)))))))

(defun format-pattern-entry (pattern)
  "Format a single pattern for display.
   Uses *full-display* to control truncation: when T shows full content,
   otherwise truncates to 150 chars."
  (handler-case
      (let ((id (pattern-id-accessor pattern))
            (domain (pattern-domain-accessor pattern))
            (helpful (pattern-helpful-accessor pattern))
            (harmful (pattern-harmful-accessor pattern))
            (content (pattern-content-accessor pattern)))
        (with-output-to-string (s)
          (format s "- [~A] (~A) helpful=~D harmful=~D"
                  id domain (or helpful 0) (or harmful 0))
          (when content
            (format s "~%  ~A"
                    (if *full-display*
                        content
                        (subseq content 0 (min 150 (length content))))))))
    (error ()
      (format nil "- ~S" pattern))))
