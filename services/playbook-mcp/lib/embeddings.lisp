;;; Playbook MCP Server - Embeddings
;;; Ollama integration for semantic search

(in-package #:playbook-mcp)

(defvar *log-verbose*)  ; forward declaration — defined in package.lisp

;;; Configuration

(defparameter *ollama-url* "http://localhost:11434"
  "Base URL for Ollama API.")

(defparameter *embedding-model* "nomic-embed-text"
  "Model to use for embeddings (768 dimensions).")

(defparameter *ollama-timeout* 30
  "Timeout in seconds for Ollama HTTP requests.
   Prevents blocking indefinitely when Ollama is loading a model or unresponsive.")

(defun initialize-ollama-config ()
  "Read OLLAMA_HOST env var and configure *ollama-url*.
   Strips trailing slash for consistent URL construction.
   No-op if env var is unset or empty."
  (let ((env (uiop:getenv "OLLAMA_HOST")))
    (when (and env (plusp (length env)))
      (setf *ollama-url* (string-right-trim "/" env)))))

;;; Embedding cache (separate from pattern store for persistence flexibility)

(defvar *embedding-cache* (make-hash-table :test 'equal)
  "Cache of embeddings, keyed by content hash.")

(defvar *embedding-cache-lock* (make-lock "embedding-cache")
  "Lock for thread-safe embedding cache access.")

;;; Ollama API

(defun ollama-embed (text)
  "Get embedding vector from Ollama for TEXT.
   Returns a list of 768 doubles, or NIL on error."
  (handler-case
      (let ((payload (make-hash-table :test 'equal)))
        (setf (gethash "model" payload) *embedding-model*)
        (setf (gethash "input" payload) text)
        (let* ((json-str (with-output-to-string (s)
                           (yason:encode payload s)))
               (response (dexador:post
                          (format nil "~A/api/embed" *ollama-url*)
                          :content json-str
                          :headers '(("Content-Type" . "application/json"))
                          :read-timeout *ollama-timeout*
                          :connect-timeout 5))
               (parsed (yason:parse response))
               (embeddings (gethash "embeddings" parsed)))
          ;; Ollama returns list of embeddings, we want the first
          (first embeddings)))
    (error (e)
      (format *error-output* "Ollama embedding error: ~A~%" e)
      nil)))

(defun ollama-embed-batch (texts)
  "Get embedding vectors from Ollama for a list of TEXTS in a single API call.
   Returns a list of embeddings (each a list of 768 doubles), or NIL on error.
   This is significantly faster than calling ollama-embed per text."
  (when (null texts)
    (return-from ollama-embed-batch nil))
  (handler-case
      (let ((payload (make-hash-table :test 'equal)))
        (setf (gethash "model" payload) *embedding-model*)
        (setf (gethash "input" payload) texts)  ; List of strings
        (let* ((json-str (with-output-to-string (s)
                           (yason:encode payload s)))
               (response (dexador:post
                          (format nil "~A/api/embed" *ollama-url*)
                          :content json-str
                          :headers '(("Content-Type" . "application/json"))
                          :read-timeout *ollama-timeout*
                          :connect-timeout 5))
               (parsed (yason:parse response)))
          (gethash "embeddings" parsed)))
    (error (e)
      (format *error-output* "Ollama batch embedding error: ~A~%" e)
      nil)))

;;; Content hashing for cache keys

(defun content-hash (text)
  "Simple hash for cache key (could use SHA256 for production)."
  (sxhash text))

;;; Cached embedding retrieval

(defun get-embedding (text)
  "Get embedding for TEXT, using cache if available."
  (let ((key (content-hash text)))
    (with-lock-held (*embedding-cache-lock*)
      (or (gethash key *embedding-cache*)
          (let ((embedding (ollama-embed text)))
            (when embedding
              (setf (gethash key *embedding-cache*) embedding))
            embedding)))))

(defun ensure-pattern-embedding (pattern)
  "Ensure pattern has an embedding, computing if needed.
   Stores in pattern struct and cache."
  (or (pattern-embedding pattern)
      (let ((embedding (get-embedding (pattern-content pattern))))
        (when embedding
          (with-lock-held (*pattern-store-lock*)
            (setf (pattern-embedding pattern) embedding)))
        embedding)))

;;; Cache management

(defun clear-embedding-cache ()
  "Clear all cached embeddings."
  (with-lock-held (*embedding-cache-lock*)
    (clrhash *embedding-cache*))
  ;; Also clear from patterns (protected by pattern store lock)
  (with-lock-held (*pattern-store-lock*)
    (dolist (pattern (hash-table-values *pattern-store*))
      (setf (pattern-embedding pattern) nil))))

(defun embedding-cache-size ()
  "Return number of cached embeddings."
  (with-lock-held (*embedding-cache-lock*)
    (hash-table-count *embedding-cache*)))

;;; Cache persistence

(defun save-embedding-cache (path)
  "Save embedding cache to a JSON file (atomic write).
   Format: {\"<content-hash-as-string>\": [floats...], ...}"
  (let* ((abs-path (namestring (merge-pathnames path)))
         (temp-path (format nil "~A.tmp" abs-path))
         (json-ht (make-hash-table :test 'equal)))
    (with-lock-held (*embedding-cache-lock*)
      (maphash (lambda (k v)
                 (setf (gethash (princ-to-string k) json-ht) v))
               *embedding-cache*))
    (with-open-file (s temp-path :direction :output :if-exists :supersede)
      (yason:encode json-ht s))
    (rename-file temp-path abs-path)))

(defun load-embedding-cache (path)
  "Load embedding cache from a JSON file into *embedding-cache*.
   Handles both formats:
   - Versioned: {\"version\": 1, \"embeddings\": {pattern-id: [...]}}
   - Hash-keyed: {content-hash-string: [...]}
   No-op if file does not exist."
  (when (probe-file path)
    (let ((data (yason:parse (alexandria:read-file-into-string path))))
      (with-lock-held (*embedding-cache-lock*)
        ;; Detect format by checking for \"version\" key
        (if (gethash "version" data)
            ;; Versioned format - load embeddings by pattern ID into pattern structs
            ;; (Cannot populate content-hash cache without pattern structs loaded)
            (let ((embeddings (gethash "embeddings" data)))
              (when embeddings
                (maphash (lambda (pattern-id embedding)
                           ;; Look up pattern and set its embedding
                           (let ((pattern (get-pattern pattern-id)))
                             (when (and pattern embedding)
                               (setf (pattern-embedding pattern) embedding)
                               ;; Also cache by content hash for future lookups
                               (let ((key (content-hash (pattern-content pattern))))
                                 (setf (gethash key *embedding-cache*) embedding)))))
                         embeddings)))
            ;; Hash-keyed format (legacy)
            (maphash (lambda (k v)
                       (handler-case
                           (let ((key (parse-integer k)))
                             (setf (gethash key *embedding-cache*) v))
                         (error () nil)))  ; Skip non-integer keys
                     data))))))

(defun prune-embedding-cache-file (cache-path valid-pattern-ids)
  "Remove embeddings for patterns not in VALID-PATTERN-IDS.
   Handles versioned format. Returns count of pruned entries."
  (unless (probe-file cache-path)
    (return-from prune-embedding-cache-file 0))

  (let* ((data (yason:parse (alexandria:read-file-into-string cache-path)))
         (valid-set (make-hash-table :test 'equal))
         (pruned 0))

    ;; Build valid set
    (dolist (id valid-pattern-ids)
      (setf (gethash id valid-set) t))

    ;; Handle versioned format
    (when (gethash "version" data)
      (let ((embeddings (gethash "embeddings" data)))
        (when embeddings
          (let ((to-remove nil))
            (maphash (lambda (pattern-id embedding)
                       (declare (ignore embedding))
                       (unless (gethash pattern-id valid-set)
                         (push pattern-id to-remove)
                         (incf pruned)))
                     embeddings)
            ;; Remove orphans
            (dolist (id to-remove)
              (remhash id embeddings)))))

      ;; Write back if changed
      (when (plusp pruned)
        (let ((temp-path (format nil "~A.tmp" (namestring cache-path))))
          (with-open-file (s temp-path :direction :output :if-exists :supersede)
            (yason:encode data s))
          (rename-file temp-path cache-path))))

    pruned))

(defun batch-embed-patterns (patterns)
  "Compute embeddings for all patterns missing embeddings using batched API call.
   Returns the number of new embeddings computed."
  ;; Collect patterns needing embeddings
  (let ((needs-embedding (remove-if #'pattern-embedding patterns)))
    (when (null needs-embedding)
      (return-from batch-embed-patterns 0))
    ;; Extract content strings
    (let* ((contents (mapcar #'pattern-content needs-embedding))
           ;; Single batched API call
           (embeddings (ollama-embed-batch contents)))
      (if embeddings
          ;; Assign embeddings back to patterns and cache
          ;; Lock protects both pattern slots and embedding cache
          (with-lock-held (*pattern-store-lock*)
            (with-lock-held (*embedding-cache-lock*)
              (loop for pattern in needs-embedding
                    for content in contents
                    for embedding in embeddings
                    for count from 0
                    when embedding
                    do (setf (pattern-embedding pattern) embedding)
                       (setf (gethash (content-hash content) *embedding-cache*) embedding)
                    finally (return count))))
          0))))

(defun compute-all-embeddings (&key callback)
  "Ensure all patterns have embeddings. Calls CALLBACK when done (if provided).
   Returns the number of new embeddings computed."
  (let ((new-count (batch-embed-patterns (list-patterns))))
    (when (and (> new-count 0) *log-verbose*)
      (format *error-output* "Computed ~D new embeddings~%" new-count))
    (when callback (funcall callback))
    new-count))

;;; Cosine similarity

(defun dot-product (v1 v2)
  "Compute dot product of two vectors (lists of numbers)."
  (reduce #'+ (mapcar #'* v1 v2)))

(defun vector-magnitude (v)
  "Compute magnitude (L2 norm) of a vector."
  (sqrt (dot-product v v)))

(defun cosine-similarity (v1 v2)
  "Compute cosine similarity between two list vectors.
   Returns value in range [-1, 1], where 1 is identical.
   Package-local to PLAYBOOK-MCP — operates on list representations from Ollama.
   The canonical typed-array version is task-mcp:cosine-sim (and task-mcp:vec-dot
   for pre-normalized embedding vectors)."
  (let ((mag1 (vector-magnitude v1))
        (mag2 (vector-magnitude v2)))
    (if (or (zerop mag1) (zerop mag2))
        0.0
        (/ (dot-product v1 v2) (* mag1 mag2)))))

;;; Semantic search

(defun semantic-search-patterns (query &key domain (limit 10) (threshold 0.3))
  "Search patterns by semantic similarity to QUERY.
   Returns list of (pattern . similarity) pairs, sorted by similarity."
  (let* ((query-embedding (get-embedding query))
         (patterns (if domain
                       (patterns-by-domain domain)
                       (list-patterns))))
    (when query-embedding
      ;; Compute similarities for all patterns with embeddings
      (let ((scored (loop for pattern in patterns
                          for embedding = (ensure-pattern-embedding pattern)
                          when embedding
                          collect (cons pattern
                                        (cosine-similarity query-embedding embedding)))))
        ;; Filter by threshold and sort
        (let* ((filtered (remove-if (lambda (pair) (< (cdr pair) threshold))
                                    scored))
               (sorted (sort filtered #'> :key #'cdr)))
          (if limit
              (subseq sorted 0 (min limit (length sorted)))
              sorted))))))
