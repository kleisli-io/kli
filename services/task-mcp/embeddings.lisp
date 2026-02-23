;;; Task MCP Server - Embedding Primitives
;;; Ollama integration via raw sb-bsd-sockets (zero external HTTP deps)

(in-package #:task-mcp)

(defvar *log-verbose*)  ; forward declaration — defined in server.lisp

;;; Configuration

(defparameter *ollama-host* #(127 0 0 1)
  "Ollama server IP address as octet vector.")

(defparameter *ollama-port* 11434
  "Ollama server port.")

(defparameter *embedding-model* "nomic-embed-text"
  "Model for embeddings (768 dimensions).")

(defun parse-ollama-host-env (url-string)
  "Parse OLLAMA_HOST URL into (values host-octets port).
   Accepts 'http://host:port', 'host:port', or 'http://host'.
   Returns NIL if parsing fails."
  (handler-case
      (let* ((stripped (string-trim '(#\Space #\Tab) url-string))
             ;; Strip http:// or https:// prefix
             (host-part (if (search "://" stripped)
                            (subseq stripped (+ 3 (search "://" stripped)))
                            stripped))
             ;; Split host:port
             (colon-pos (position #\: host-part :from-end t))
             (hostname (if colon-pos
                           (subseq host-part 0 colon-pos)
                           host-part))
             (port (if colon-pos
                       (parse-integer (subseq host-part (1+ colon-pos))
                                      :junk-allowed t)
                       11434)))
        (when (and hostname (plusp (length hostname)) port (plusp port))
          (let ((addr (sb-bsd-sockets:host-ent-address
                       (sb-bsd-sockets:get-host-by-name hostname))))
            (when addr
              (values addr port)))))
    (error () nil)))

(defun initialize-ollama-config ()
  "Read OLLAMA_HOST env var and configure *ollama-host* and *ollama-port*.
   No-op if env var is unset or empty."
  (let ((env (uiop:getenv "OLLAMA_HOST")))
    (when (and env (plusp (length env)))
      (multiple-value-bind (host port) (parse-ollama-host-env env)
        (when (and host port)
          (setf *ollama-host* host
                *ollama-port* port))))))

;;; Embedding cache

(defvar *embedding-cache* (make-hash-table :test 'equal)
  "Cache of embeddings, keyed by text string.")

(defvar *embedding-cache-lock* (bt:make-lock "embedding-cache")
  "Lock for thread-safe embedding cache access.")

;;; Embedding cache persistence

(defvar *embedding-cache-path* nil
  "Path where embedding cache is persisted to disk. Set during initialization.")

(defvar *embedding-cache-dirty* 0
  "Count of new embeddings since last save. Accessed under *embedding-cache-lock*.")

(defparameter *embedding-save-threshold* 50
  "Save embedding cache to disk after this many new embeddings.")

(defun dvec-to-list (v)
  "Convert a double-float vector to a list for JSON serialization."
  (declare (type (simple-array double-float (*)) v))
  (loop for i below (length v) collect (aref v i)))

(defun embedding-cache-size ()
  "Return number of cached embeddings."
  (bt:with-lock-held (*embedding-cache-lock*)
    (hash-table-count *embedding-cache*)))

(defun save-embedding-cache (&optional (path *embedding-cache-path*))
  "Save embedding cache to a JSON file (atomic write).
   Format: {\"text-key\": [float, ...], ...}
   No-op if path is NIL or cache is empty."
  (when (and path (plusp (bt:with-lock-held (*embedding-cache-lock*)
                           (hash-table-count *embedding-cache*))))
    (let* ((abs-path (namestring (merge-pathnames path)))
           (temp-path (format nil "~A.tmp" abs-path))
           (json-ht (make-hash-table :test 'equal)))
      ;; Snapshot cache under lock (convert dvecs to lists for yason)
      (bt:with-lock-held (*embedding-cache-lock*)
        (maphash (lambda (k v)
                   (setf (gethash k json-ht)
                         (if (typep v '(simple-array double-float (*)))
                             (dvec-to-list v)
                             v)))
                 *embedding-cache*)
        (setf *embedding-cache-dirty* 0))
      ;; Write JSON atomically
      (ensure-directories-exist (pathname abs-path))
      (with-open-file (s temp-path :direction :output :if-exists :supersede)
        (yason:encode json-ht s))
      (rename-file temp-path abs-path)
      (when *log-verbose*
        (format *error-output* "Saved embedding cache: ~D entries to ~A~%"
                (hash-table-count json-ht) abs-path)))))

(defun load-embedding-cache (&optional (path *embedding-cache-path*))
  "Load embedding cache from a JSON file into *embedding-cache*.
   Converts JSON number lists back to typed double-float arrays.
   No-op if path is NIL or file does not exist."
  (when (and path (probe-file path))
    (handler-case
        (let ((data (yason:parse (alexandria:read-file-into-string
                                  (namestring (merge-pathnames path))))))
          (when (hash-table-p data)
            (bt:with-lock-held (*embedding-cache-lock*)
              (maphash (lambda (k v)
                         (when (and (stringp k) (listp v) (plusp (length v)))
                           (setf (gethash k *embedding-cache*)
                                 (list-to-dvec v))))
                       data))
            (when *log-verbose*
              (format *error-output* "Loaded embedding cache: ~D entries from ~A~%"
                      (hash-table-count data) path))))
      (error (e)
        (format *error-output* "Failed to load embedding cache from ~A: ~A~%" path e)))))

(defun maybe-save-embedding-cache ()
  "Save embedding cache if enough new embeddings have accumulated.
   Called after adding new embeddings to trigger periodic persistence."
  (when (and *embedding-cache-path*
             (>= *embedding-cache-dirty* *embedding-save-threshold*))
    (save-embedding-cache)))

(defun initialize-embedding-cache-path ()
  "Set *embedding-cache-path* based on *tasks-root*.
   Uses <tasks-root>/../.task-cache/embeddings.json."
  (when task:*tasks-root*
    (let* ((tasks-dir (pathname task:*tasks-root*))
           (parent (make-pathname :directory (butlast (pathname-directory tasks-dir))
                                  :defaults tasks-dir))
           (cache-path (merge-pathnames ".task-cache/embeddings.json" parent)))
      (setf *embedding-cache-path* (namestring cache-path)))))

;;; Raw HTTP client (localhost only, no TLS)

(defparameter *ollama-timeout* 30
  "Timeout in seconds for Ollama HTTP requests.
   Prevents blocking indefinitely when Ollama is loading a model or unresponsive.")

(defun http-post (path body-string)
  "HTTP/1.1 POST to Ollama via raw TCP socket. Returns response body or NIL on error."
  (handler-case
      (let* ((body-octets (babel:string-to-octets body-string :encoding :utf-8))
             (request (format nil "POST ~A HTTP/1.1~C~CHost: localhost:~D~C~CContent-Type: application/json~C~CConnection: close~C~CContent-Length: ~D~C~C~C~C"
                              path #\Return #\Linefeed
                              *ollama-port* #\Return #\Linefeed
                              #\Return #\Linefeed
                              #\Return #\Linefeed
                              (length body-octets) #\Return #\Linefeed
                              #\Return #\Linefeed))
             (sock (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp)))
        (unwind-protect
             (progn
               (sb-bsd-sockets:socket-connect sock *ollama-host* *ollama-port*)
               (let ((stream (sb-bsd-sockets:socket-make-stream
                              sock :output t :input t
                              :element-type '(unsigned-byte 8)
                              :buffering :full
                              :timeout *ollama-timeout*)))
                 (write-sequence (babel:string-to-octets request :encoding :ascii) stream)
                 (write-sequence body-octets stream)
                 (force-output stream)
                 ;; Read response
                 (let ((buf (make-array 65536 :element-type '(unsigned-byte 8)))
                       (chunks nil)
                       (total 0))
                   (loop for n = (read-sequence buf stream)
                         while (plusp n)
                         do (push (subseq buf 0 n) chunks)
                            (incf total n))
                   (let* ((full (make-array total :element-type '(unsigned-byte 8)))
                          (pos 0))
                     (dolist (chunk (nreverse chunks))
                       (replace full chunk :start1 pos)
                       (incf pos (length chunk)))
                     (let ((text (babel:octets-to-string full :encoding :utf-8)))
                       ;; Strip HTTP headers (split on double CRLF)
                       (let ((body-start (search (format nil "~C~C~C~C"
                                                         #\Return #\Linefeed
                                                         #\Return #\Linefeed)
                                                 text)))
                         (when body-start
                           (maybe-decode-chunked
                            (subseq text (+ body-start 4))))))))))
          (sb-bsd-sockets:socket-close sock)))
    (error (e)
      (format *error-output* "HTTP request error: ~A~%" e)
      nil)))

(defun maybe-decode-chunked (body)
  "Decode chunked transfer encoding if detected, otherwise return as-is.
   Chunked format: hex-size CRLF chunk-data CRLF ... 0 CRLF CRLF"
  (let ((first-newline (position #\Return body)))
    (if (and first-newline
             (plusp first-newline)
             (< first-newline 10)
             (every (lambda (c) (digit-char-p c 16))
                    (subseq body 0 first-newline)))
        ;; Chunked: concatenate all chunk data
        (with-output-to-string (out)
          (let ((pos 0) (len (length body)))
            (loop while (< pos len)
                  for nl = (position #\Return body :start pos)
                  while nl
                  for size = (parse-integer body :start pos :end nl :radix 16
                                                 :junk-allowed t)
                  while (and size (plusp size))
                  do (let ((data-start (+ nl 2))) ; skip CRLF after size
                       (write-string body out :start data-start
                                              :end (min (+ data-start size) len))
                       (setf pos (min (+ data-start size 2) len)))))) ; skip trailing CRLF
        ;; Not chunked
        body)))

;;; Ollama API

(defun list-to-dvec (list)
  "Convert a list of numbers to a (simple-array double-float (*))."
  (let ((arr (make-array (length list) :element-type 'double-float)))
    (loop for x in list for i from 0
          do (setf (aref arr i) (coerce x 'double-float)))
    arr))

(defun ollama-embed (text &key (model *embedding-model*))
  "Get embedding vector from Ollama for TEXT.
   Returns a (simple-array double-float (768)), or NIL on error."
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model)
    (setf (gethash "input" payload) text)
    (let ((response (http-post "/api/embed"
                               (with-output-to-string (s)
                                 (yason:encode payload s)))))
      (when response
        (handler-case
            (let* ((parsed (yason:parse response))
                   (error-msg (gethash "error" parsed)))
              (if error-msg
                  (progn
                    (format *error-output* "Ollama error: ~A~%" error-msg)
                    nil)
                  (let ((raw (first (gethash "embeddings" parsed))))
                    (when raw (vec-normalize (list-to-dvec raw))))))
          (error (e)
            (format *error-output* "Ollama parse error: ~A~%" e)
            nil))))))

(defun ollama-embed-batch (texts &key (model *embedding-model*))
  "Get embeddings for a list of TEXTS in a single API call.
   Returns list of (simple-array double-float (*)) vectors, or NIL on error."
  (when (null texts)
    (return-from ollama-embed-batch nil))
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model)
    (setf (gethash "input" payload) texts)
    (let ((response (http-post "/api/embed"
                               (with-output-to-string (s)
                                 (yason:encode payload s)))))
      (when response
        (handler-case
            (let* ((parsed (yason:parse response))
                   (error-msg (gethash "error" parsed)))
              (if error-msg
                  (progn
                    (format *error-output* "Ollama batch error: ~A~%" error-msg)
                    nil)
                  (mapcar (lambda (raw) (vec-normalize (list-to-dvec raw)))
                          (gethash "embeddings" parsed))))
          (error (e)
            (format *error-output* "Ollama batch parse error: ~A~%" e)
            nil))))))

;;; Cached embedding retrieval

(defun get-embedding (text)
  "Get embedding for TEXT, using cache. Thread-safe.
   Two-phase lock: check cache → release → embed → re-acquire → store.
   Prevents lock contention when Ollama is slow or down.
   Triggers periodic disk persistence after new embeddings accumulate."
  ;; Phase 1: Check cache (keyed by full text string — no sxhash collisions)
  (let ((cached (bt:with-lock-held (*embedding-cache-lock*)
                  (gethash text *embedding-cache*))))
    (or cached
        ;; Phase 2: Embed without lock, then store
        (let ((embedding (ollama-embed text)))
          (when embedding
            (bt:with-lock-held (*embedding-cache-lock*)
              ;; Another thread may have cached it while we were embedding
              (or (gethash text *embedding-cache*)
                  (progn
                    (setf (gethash text *embedding-cache*) embedding)
                    (incf *embedding-cache-dirty*)
                    embedding)))
            ;; Periodic save outside lock
            (maybe-save-embedding-cache)
            embedding)))))

;;; Vector math (typed arrays for ~40-86x speedup over lists)

(defun vec-dot (v1 v2)
  "Dot product of two double-float vectors."
  (declare (type (simple-array double-float (*)) v1 v2)
           (optimize (speed 3) (safety 0)))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i (length v1) sum)
      (incf sum (* (aref v1 i) (aref v2 i))))))

(defun vec-mag (v)
  "Magnitude (L2 norm) of a double-float vector."
  (declare (type (simple-array double-float (*)) v)
           (optimize (speed 3) (safety 0)))
  (the double-float (sqrt (the (double-float 0.0d0) (vec-dot v v)))))

(defun vec-normalize (v)
  "Return a new unit-length vector (L2 normalized). Returns zero vector if magnitude is zero."
  (declare (type (simple-array double-float (*)) v)
           (optimize (speed 3) (safety 1)))
  (let ((mag (vec-mag v)))
    (if (zerop mag)
        (make-array (length v) :element-type 'double-float :initial-element 0.0d0)
        (let ((result (make-array (length v) :element-type 'double-float)))
          (dotimes (i (length v) result)
            (setf (aref result i) (/ (aref v i) mag)))))))

(defun cosine-sim (v1 v2)
  "Cosine similarity between two double-float vectors. Returns value in [-1, 1].
   NOTE: For pre-normalized vectors (from ollama-embed / ollama-embed-batch),
   use vec-dot instead — it gives the same result without recomputing magnitudes."
  (declare (type (simple-array double-float (*)) v1 v2)
           (optimize (speed 3) (safety 1)))
  (let ((dot 0.0d0) (m1 0.0d0) (m2 0.0d0))
    (declare (type double-float dot m1 m2))
    (dotimes (i (length v1))
      (let ((x (aref v1 i)) (y (aref v2 i)))
        (declare (type double-float x y))
        (incf dot (* x y))
        (incf m1 (* x x))
        (incf m2 (* y y))))
    (if (or (zerop m1) (zerop m2))
        0.0d0
        (/ dot (* (the double-float (sqrt (the (double-float 0.0d0) m1)))
                  (the double-float (sqrt (the (double-float 0.0d0) m2))))))))
