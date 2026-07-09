(in-package #:kli/session/log)

(defun estimate-content-tokens (content)
  (ceiling (length (if (stringp content) content (princ-to-string content))) 4))

(defun estimate-message-tokens (message)
  (+ (estimate-content-tokens (message-content message))
     (loop for call in (getf (message-metadata message) :tool-calls)
           sum (+ (estimate-content-tokens (getf call :id))
                  (estimate-content-tokens (getf call :name))
                  (estimate-content-tokens (getf call :arguments-json))))))

(defun entry-cut-message (entry)
  (when (typep entry 'message-entry)
    (entry-message entry)))

(defun entry-token-estimate (entry)
  (typecase entry
    (message-entry (estimate-message-tokens (entry-message entry)))
    (compaction-entry (estimate-content-tokens (entry-summary entry)))
    (branch-summary-entry (estimate-content-tokens (entry-summary entry)))
    (t 0)))

(defun cut-point-entry-p (entry)
  "True for entries a kept window may begin at. Never a tool result, which must
follow the assistant tool call that produced it."
  (let ((message (entry-cut-message entry)))
    (or (typep entry 'branch-summary-entry)
        (and message
             (member (message-role message) '(:user :assistant :custom))))))

(defun turn-start-entry-p (entry)
  (let ((message (entry-cut-message entry)))
    (or (typep entry 'branch-summary-entry)
        (and message
             (member (message-role message) '(:user :custom))))))

(defun find-valid-cut-points (entries start end)
  (loop for i from start below end
        when (cut-point-entry-p (aref entries i))
          collect i))

(defun find-turn-start-index (entries entry-index start)
  (loop for i from entry-index downto start
        when (turn-start-entry-p (aref entries i))
          do (return-from find-turn-start-index i))
  -1)

(defstruct (cut-point (:constructor %make-cut-point))
  first-kept-index
  turn-start-index
  split-turn-p)

(defun entry-token-sum (entries start end entry-token-fn)
  (loop for i from start below end
        sum (funcall entry-token-fn (aref entries i))))

(defun effective-cut-budget (keep-recent-tokens summary-reserve-tokens)
  (max 0 (- keep-recent-tokens (max 0 summary-reserve-tokens))))

(defun find-cut-point (entries start end keep-recent-tokens
                       &key (entry-token-fn #'entry-token-estimate)
                         (summary-reserve-tokens 0))
  "Locate the cut that keeps roughly the last KEEP-RECENT-TOKENS of
conversation, minus SUMMARY-RESERVE-TOKENS, and summarizes everything older.
Snaps forward to a valid cut point so the kept window never begins on a tool
result; when no cut point lies at or after the boundary (trailing tool results
alone exceed the cut budget) snaps backward to the nearest cut point at or
before it."
  (let ((cut-points (find-valid-cut-points entries start end)))
    (when (null cut-points)
      (return-from find-cut-point
        (%make-cut-point :first-kept-index start
                         :turn-start-index -1
                         :split-turn-p nil)))
    (let ((budget (effective-cut-budget keep-recent-tokens summary-reserve-tokens))
          (accumulated 0)
          (cut-index (first cut-points)))
      (loop for i from (1- end) downto start
            do (incf accumulated (funcall entry-token-fn (aref entries i)))
               (when (>= accumulated budget)
                 (let ((snapped (or (find-if (lambda (c) (>= c i)) cut-points)
                                    (find-if (lambda (c) (<= c i)) cut-points
                                             :from-end t))))
                   (when snapped (setf cut-index snapped)))
                 (loop-finish)))
      (loop while (> cut-index start)
            for prev = (aref entries (1- cut-index))
            until (or (typep prev 'compaction-entry)
                      (entry-cut-message prev))
            do (decf cut-index))
      (let* ((cut-entry (aref entries cut-index))
             (cut-message (entry-cut-message cut-entry))
             (user-p (and cut-message (eq (message-role cut-message) :user)))
             (turn-start (if user-p
                             -1
                             (find-turn-start-index entries cut-index start))))
        (%make-cut-point :first-kept-index cut-index
                         :turn-start-index turn-start
                         :split-turn-p (and (not user-p) (/= turn-start -1)))))))

(defstruct (compaction-preparation (:constructor %make-compaction-preparation))
  first-kept-id
  messages-to-summarize
  turn-prefix-messages
  split-turn-p
  tokens-before
  previous-summary)

(defun prepare-session-compaction (path-entries keep-recent-tokens
                                   &key (entry-token-fn #'entry-token-estimate)
                                     (summary-reserve-tokens 0))
  "Compute the compaction cut over PATH-ENTRIES (root->leaf). Returns a
COMPACTION-PREPARATION, or NIL when the leaf is already a compaction (nothing to
compact)."
  (let* ((entries (coerce path-entries 'vector))
         (count (length entries)))
    (when (or (zerop count)
              (typep (aref entries (1- count)) 'compaction-entry))
      (return-from prepare-session-compaction nil))
    (let* ((prev-compaction-index
             (position-if (lambda (e) (typep e 'compaction-entry))
                          entries :from-end t))
           (boundary-start (if prev-compaction-index (1+ prev-compaction-index) 0))
           (usage-start (or prev-compaction-index 0))
           (tokens-before
             (entry-token-sum entries usage-start count entry-token-fn))
           (cut (find-cut-point entries boundary-start count keep-recent-tokens
                                :entry-token-fn entry-token-fn
                                :summary-reserve-tokens summary-reserve-tokens))
           (first-kept-index (cut-point-first-kept-index cut))
           (first-kept-id (object-id (aref entries first-kept-index)))
           (history-end (if (cut-point-split-turn-p cut)
                            (cut-point-turn-start-index cut)
                            first-kept-index))
           (messages-to-summarize
             (loop for i from boundary-start below history-end
                   for message = (entry-cut-message (aref entries i))
                   when message collect message))
           (turn-prefix-messages
             (when (cut-point-split-turn-p cut)
               (loop for i from (cut-point-turn-start-index cut)
                       below first-kept-index
                     for message = (entry-cut-message (aref entries i))
                     when message collect message)))
           (previous-summary
             (when prev-compaction-index
               (entry-summary (aref entries prev-compaction-index)))))
      (when (and (null messages-to-summarize)
                 (null turn-prefix-messages))
        (return-from prepare-session-compaction nil))
      (%make-compaction-preparation
       :first-kept-id first-kept-id
       :messages-to-summarize messages-to-summarize
       :turn-prefix-messages turn-prefix-messages
       :split-turn-p (cut-point-split-turn-p cut)
       :tokens-before tokens-before
       :previous-summary previous-summary))))

(defun branch-summary-common-ancestor (store session old-leaf-id target-leaf-id)
  (when old-leaf-id
    (let ((old-ids (mapcar #'object-id
                           (session-branch store session old-leaf-id))))
      (loop for entry in (reverse (session-branch store session target-leaf-id))
            when (member (object-id entry) old-ids :test #'eql)
              do (return (object-id entry))))))

(defun collect-branch-summary-entries (store session old-leaf-id target-leaf-id)
  (when old-leaf-id
    (let ((ancestor (branch-summary-common-ancestor store session
                                                     old-leaf-id target-leaf-id))
          (entries '())
          (current old-leaf-id))
      (loop while (and current (not (eql current ancestor)))
            for entry = (session-entry-by-id store session current)
            while entry
            do (push entry entries)
               (setf current (entry-parent-id entry)))
      entries)))

(defun entry-tracked-files (entry)
  (when (typep entry '(or compaction-entry branch-summary-entry))
    (let ((data (entry-data entry)))
      (when (listp data)
        (values (getf data :read-files) (getf data :modified-files))))))

(defun union-files (&rest file-lists)
  (remove-duplicates (apply #'append file-lists) :test #'equal :from-end t))

(defstruct (branch-summary-preparation (:constructor %make-branch-summary-preparation))
  from-id
  messages
  read-files
  modified-files)

(defun prepare-branch-summary (store session old-leaf-id target-leaf-id
                               &key (token-budget 0))
  "Collect the entries unique to OLD-LEAF-ID for a branch summary anchored at
the common ancestor with TARGET-LEAF-ID. File tracking accumulates across
summaries, so prior summaries on the collected path seed the new entry's data."
  (let ((ancestor (branch-summary-common-ancestor store session
                                                   old-leaf-id target-leaf-id))
        (entries (collect-branch-summary-entries store session
                                                 old-leaf-id target-leaf-id))
        (read-files '())
        (modified-files '())
        (messages '())
        (total 0))
    (dolist (entry entries)
      (multiple-value-bind (read modified) (entry-tracked-files entry)
        (setf read-files (append read-files read)
              modified-files (append modified-files modified))))
    (loop for entry in (reverse entries)
          for message = (entry-cut-message entry)
          when message
            do (let ((tokens (estimate-message-tokens message)))
                 (when (and (plusp token-budget) (> (+ total tokens) token-budget))
                   (loop-finish))
                 (push message messages)
                 (incf total tokens)))
    (%make-branch-summary-preparation
     :from-id (or ancestor old-leaf-id)
     :messages messages
     :read-files (union-files read-files)
     :modified-files (union-files modified-files))))

(defun default-branch-summarize (&rest args)
  (declare (ignore args))
  nil)

(defun produce-branch-summary (store session old-leaf-id target-leaf-id
                               &key context (token-budget 0) source
                                 (summarizer #'default-branch-summarize))
  "Summarize the entries unique to OLD-LEAF-ID and append a branch-summary entry
at TARGET-LEAF-ID. SUMMARIZER returns (values summary details), where a
non-string or empty summary yields no entry. Returns the appended entry or NIL."
  (let ((prep (prepare-branch-summary store session old-leaf-id target-leaf-id
                                      :token-budget token-budget)))
    (multiple-value-bind (summary details)
        (funcall summarizer
                 :messages (branch-summary-preparation-messages prep)
                 :read-files (branch-summary-preparation-read-files prep)
                 :modified-files (branch-summary-preparation-modified-files prep)
                 :from-id (branch-summary-preparation-from-id prep))
      (when (and (stringp summary) (plusp (length summary)))
        (let ((entry (make-branch-summary-entry
                      (branch-summary-preparation-from-id prep)
                      summary
                      :source source
                      :data (list :read-files
                                  (union-files
                                   (branch-summary-preparation-read-files prep)
                                   (getf details :read-files))
                                  :modified-files
                                  (union-files
                                   (branch-summary-preparation-modified-files prep)
                                   (getf details :modified-files))))))
          (setf (session-leaf-id session) target-leaf-id)
          (append-session-entry store session entry context)
          entry)))))
