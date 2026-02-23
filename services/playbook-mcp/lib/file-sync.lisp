;;; Playbook MCP Server - File Sync
;;; Read/write playbook.md files with atomic updates

(in-package #:playbook-mcp)

;;; File update operations

(defun update-pattern-in-file (path pattern-id update-fn)
  "Update a single pattern in a playbook.md file.
   UPDATE-FN receives the current line and returns the new line.
   Uses atomic write (write to temp, then rename)."
  (let* ((abs-path (namestring (merge-pathnames path)))
         (temp-path (format nil "~A.tmp" abs-path))
         (updated nil))
    (with-open-file (in abs-path :direction :input)
      (with-open-file (out temp-path :direction :output
                                     :if-exists :supersede)
        (loop for line = (read-line in nil nil)
              while line
              do (let ((pattern (parse-pattern-line line)))
                   (if (and pattern (string= (pattern-id pattern) pattern-id))
                       (progn
                         (write-line (funcall update-fn line) out)
                         (setf updated t))
                       (write-line line out))))))
    ;; Atomic rename
    (when updated
      (rename-file temp-path abs-path))
    updated))

;;; Evidence ledger

(defun evidence-ledger-path (source-file)
  "Return path to playbook-evidence.json alongside SOURCE-FILE."
  (when source-file
    (merge-pathnames "playbook-evidence.json"
                     (uiop:pathname-directory-pathname
                      (uiop:ensure-pathname source-file)))))

(defun append-pattern-evidence (pattern-id feedback-type evidence-text)
  "Append an evidence entry to playbook-evidence.json alongside the pattern's source file.
   Format: {pattern-id: [{type, text, ts}, ...]}. No-op if evidence is empty. Atomic write."
  (when (and evidence-text (string/= evidence-text ""))
    (let* ((pattern (get-pattern pattern-id))
           (ledger-path (when pattern
                          (evidence-ledger-path (pattern-source-file pattern)))))
      (when ledger-path
        (handler-case
            (let* ((abs-path (namestring ledger-path))
                   (temp-path (format nil "~A.tmp" abs-path))
                   (existing (if (probe-file abs-path)
                                 (yason:parse (alexandria:read-file-into-string abs-path))
                                 (make-hash-table :test 'equal)))
                   (entry (let ((ht (make-hash-table :test 'equal)))
                            (setf (gethash "type" ht)
                                  (string-downcase (symbol-name feedback-type)))
                            (setf (gethash "text" ht) evidence-text)
                            (setf (gethash "ts" ht) (get-universal-time))
                            ht))
                   (prev (gethash pattern-id existing))
                   (updated (concatenate 'vector (if prev prev #()) (vector entry))))
              (setf (gethash pattern-id existing) updated)
              (ensure-directories-exist abs-path)
              (with-open-file (s temp-path :direction :output :if-exists :supersede)
                (yason:encode existing s))
              (rename-file temp-path abs-path))
          (error (e)
            (format *error-output* "Warning: append-pattern-evidence ~A: ~A~%"
                    pattern-id e)))))))

;;; Feedback persistence

(defun save-pattern-feedback (pattern-id feedback-type &optional evidence)
  "Save feedback (helpful/harmful) for a pattern.
   FEEDBACK-TYPE is :helpful or :harmful. EVIDENCE is an optional explanatory string
   appended to playbook-evidence.json for audit and future training use.
   Always updates in-memory; only writes to file if pattern has source-file."
  (let ((pattern (get-pattern pattern-id)))
    (when pattern
      ;; Always update in-memory
      (case feedback-type
        (:helpful (incf (pattern-helpful pattern)))
        (:harmful (incf (pattern-harmful pattern))))
      ;; Update in file only if pattern has a source file
      (when (pattern-source-file pattern)
        (update-pattern-in-file
         (pattern-source-file pattern)
         pattern-id
         (lambda (line)
           (declare (ignore line))
           (pattern-to-line pattern))))
      ;; Append evidence to ledger if provided
      (append-pattern-evidence pattern-id feedback-type evidence))))

;;; Evolution persistence

(defun save-pattern-evolution (pattern-id new-content reason)
  "Save pattern evolution to its source file.
   Records old content in evolution history."
  (let ((pattern (get-pattern pattern-id)))
    (when (and pattern (pattern-source-file pattern))
      (let ((old-content (pattern-content pattern))
            (timestamp (get-universal-time)))
        ;; Update in-memory
        (push (list timestamp reason old-content)
              (pattern-evolution-history pattern))
        (setf (pattern-content pattern) new-content)
        ;; Update in file
        (update-pattern-in-file
         (pattern-source-file pattern)
         pattern-id
         (lambda (line)
           (declare (ignore line))
           (pattern-to-line pattern)))))))

;;; Add new pattern to file

(defun append-pattern-to-file (path pattern)
  "Append a new pattern to a playbook.md file."
  (with-open-file (stream path :direction :output
                               :if-exists :append
                               :if-does-not-exist :create)
    (write-line "" stream)  ; Blank line before
    (write-line (pattern-to-line pattern) stream))
  (setf (pattern-source-file pattern) (namestring path))
  (store-pattern pattern))

;;; =========================================================================
;;; Relevance feedback (query-scoped, separate from quality counters)
;;; =========================================================================
;;; Storage path: playbook-relevance-feedback.json (sidecar alongside playbook.md)
;;; In-memory: *relevance-store* hash-table (pattern-id → list of relevance-entry)
;;; Convention: follows co-app-ledger pattern (hash-table + lock + atomic I/O)

(defstruct (relevance-entry (:conc-name relevance-entry-))
  "A query-scoped relevance feedback signal for a pattern.
   Records that a pattern was activated but not relevant to a particular query context."
  (pattern-id "" :type string)
  (domains nil :type list)
  (timestamp 0 :type integer)
  (signal :not-relevant :type keyword))

(defvar *relevance-store* (make-hash-table :test 'equal)
  "Pattern-ID → list of relevance-entry. Thread-safe via *relevance-store-lock*.")

(defvar *relevance-store-lock* (make-lock "relevance-store")
  "Lock for thread-safe relevance store access.")

(defun record-relevance-feedback (pattern-id domains)
  "Record a :not-relevant signal for PATTERN-ID in the given DOMAINS context.
   Appends to the list of entries for that pattern. Returns the entry created."
  (let ((entry (make-relevance-entry
                :pattern-id pattern-id
                :domains domains
                :timestamp (get-universal-time)
                :signal :not-relevant)))
    (with-lock-held (*relevance-store-lock*)
      (push entry (gethash pattern-id *relevance-store*)))
    entry))

(defun relevance-entry-to-ht (entry)
  "Convert relevance-entry to a yason-compatible hash-table."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "domains" ht) (relevance-entry-domains entry))
    (setf (gethash "timestamp" ht) (relevance-entry-timestamp entry))
    (setf (gethash "signal" ht) (string-downcase (symbol-name (relevance-entry-signal entry))))
    ht))

(defun ht-to-relevance-entry (pattern-id ht)
  "Convert a parsed JSON hash-table back to a relevance-entry."
  (make-relevance-entry
   :pattern-id pattern-id
   :domains (coerce (gethash "domains" ht) 'list)
   :timestamp (gethash "timestamp" ht)
   :signal (intern (string-upcase (gethash "signal" ht)) :keyword)))

(defun relevance-feedback-path ()
  "Return path to playbook-relevance-feedback.json in depot's metadata directory.
   Returns NIL if depot not detected."
  (let ((meta-path (detect-depot-meta-path)))
    (when meta-path
      (namestring (merge-pathnames "playbook-relevance-feedback.json" meta-path)))))

(defun save-relevance-feedback-file (path)
  "Save *relevance-store* to JSON sidecar file (atomic write).
   Format: {pattern-id: [{domains, timestamp, signal}, ...], ...}"
  (let* ((abs-path (namestring (merge-pathnames path)))
         (temp-path (format nil "~A.tmp" abs-path))
         (json-ht (make-hash-table :test 'equal)))
    (with-lock-held (*relevance-store-lock*)
      (maphash (lambda (pattern-id entries)
                 (setf (gethash pattern-id json-ht)
                       (coerce (mapcar #'relevance-entry-to-ht entries) 'vector)))
               *relevance-store*))
    (ensure-directories-exist abs-path)
    (with-open-file (s temp-path :direction :output :if-exists :supersede)
      (yason:encode json-ht s))
    (rename-file temp-path abs-path)))

(defun load-relevance-feedback-file (path)
  "Load relevance feedback from JSON sidecar into *relevance-store*.
   Replaces existing store contents. Returns count of patterns loaded."
  (when (probe-file path)
    (let ((parsed (yason:parse (alexandria:read-file-into-string path)))
          (count 0))
      (with-lock-held (*relevance-store-lock*)
        (clrhash *relevance-store*)
        (maphash (lambda (pattern-id entries-vec)
                   (let ((entries (map 'list
                                      (lambda (ht) (ht-to-relevance-entry pattern-id ht))
                                      entries-vec)))
                     (setf (gethash pattern-id *relevance-store*) entries)
                     (incf count)))
                 parsed))
      count)))

(defun merge-relevance-feedback-file (path)
  "Additive merge of relevance feedback from PATH into *relevance-store*.
   Unlike load-relevance-feedback-file, does NOT clrhash — safe for multiple depots.
   Deduplicates by timestamp. Returns count of patterns merged."
  (handler-case
      (when (probe-file path)
        (let ((parsed (yason:parse (alexandria:read-file-into-string path)))
              (count 0))
          (with-lock-held (*relevance-store-lock*)
            (maphash (lambda (pattern-id entries-vec)
                       (let ((entries (map 'list
                                          (lambda (ht) (ht-to-relevance-entry pattern-id ht))
                                          entries-vec)))
                         (let* ((existing (gethash pattern-id *relevance-store*))
                                (existing-ts (mapcar #'relevance-entry-timestamp existing))
                                (new (remove-if (lambda (e)
                                                  (member (relevance-entry-timestamp e) existing-ts))
                                                entries)))
                           (when new
                             (setf (gethash pattern-id *relevance-store*)
                                   (append existing new))
                             (incf count)))))
                     parsed))
          count))
    (error (e)
      (format *error-output* "Warning: merge-relevance-feedback ~A: ~A~%" path e)
      0)))

(defun prune-relevance-feedback (path valid-pattern-ids)
  "Remove relevance entries for patterns not in VALID-PATTERN-IDS.
   Atomic write. Returns count of removed patterns."
  (unless (probe-file path)
    (return-from prune-relevance-feedback 0))
  (let ((parsed (yason:parse (alexandria:read-file-into-string path)))
        (valid-set (make-hash-table :test 'equal))
        (to-remove nil))
    (dolist (id valid-pattern-ids)
      (setf (gethash id valid-set) t))
    (maphash (lambda (pattern-id entries)
               (declare (ignore entries))
               (unless (gethash pattern-id valid-set)
                 (push pattern-id to-remove)))
             parsed)
    (dolist (id to-remove)
      (remhash id parsed))
    (when to-remove
      (let ((temp-path (format nil "~A.tmp" (namestring path))))
        (with-open-file (s temp-path :direction :output :if-exists :supersede)
          (yason:encode parsed s))
        (rename-file temp-path path))
      (with-lock-held (*relevance-store-lock*)
        (dolist (id to-remove)
          (remhash id *relevance-store*))))
    (length to-remove)))

(defun save-pattern-relevance-feedback (pattern-id domains &optional evidence)
  "Record :not-relevant feedback for PATTERN-ID in DOMAINS context.
   1. Record in-memory (relevance store)
   2. Persist to sidecar file (atomic write)
   3. Append evidence to evidence ledger
   Returns the relevance-entry created."
  (let ((entry (record-relevance-feedback pattern-id domains)))
    (let ((path (relevance-feedback-path)))
      (when path
        (save-relevance-feedback-file path)))
    (append-pattern-evidence pattern-id :not-relevant evidence)
    entry))
