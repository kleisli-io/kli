(in-package #:task)

(defstruct event
  (id "" :type string)
  (timestamp 0 :type integer)
  (session "" :type string)
  (clock (make-vector-clock) :type vector-clock)
  (type :unknown :type keyword)
  (data nil :type list))

;;; --- Serialization helpers ---

(defun plist-p (obj)
  "Check if OBJ is a plist (non-empty list starting with a keyword)."
  (and (consp obj)
       (keywordp (car obj))
       (evenp (length obj))))

(defun plist-to-ht (plist)
  "Convert plist to hash-table for yason serialization.
   Recursively converts nested plists."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (string-downcase (symbol-name k)) ht)
                   (if (plist-p v)
                       (plist-to-ht v)  ; Recursively convert nested plists
                       v)))
    ht))

(defun alist-p (obj)
  "Check if OBJ is an alist (list of cons cells with string keys)."
  (and (consp obj)
       (consp (car obj))
       (stringp (caar obj))))

(defun alist-to-plist (alist)
  "Convert alist with string keys to plist with keyword keys.
   Recursively converts nested alists."
  (loop for (k . v) in alist
        collect (intern (string-upcase k) :keyword)
        collect (if (alist-p v)
                    (alist-to-plist v)  ; Recursively convert nested alists
                    v)))

(defun vc-to-ht (vc)
  "Convert vector-clock entries to hash-table for JSON."
  (let ((ht (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k ht) v))
             (vc-entries vc))
    ht))

;;; --- JSON serialization ---

(defun event-to-json-string (ev)
  "Serialize event to JSON string."
  (with-output-to-string (s)
    (yason:encode-alist
     (list (cons "id" (event-id ev))
           (cons "timestamp" (event-timestamp ev))
           (cons "session" (event-session ev))
           (cons "clock" (vc-to-ht (event-clock ev)))
           (cons "type" (string-downcase (symbol-name (event-type ev))))
           (cons "data" (if (event-data ev)
                            (plist-to-ht (event-data ev))
                            (make-hash-table))))
     s)))

;;; --- Event id synthesis ---
;;; Old-format events lacking a top-level "id" are accepted by re-deriving
;;; a stable id from (timestamp,type,data). Replay stays deterministic.

(declaim (fixnum *synthesized-event-ids*))
(sb-ext:defglobal *synthesized-event-ids* 0
  "Counter of events whose id was synthesized during parse. Inspectable
   for diagnostics via the verify-events admin endpoint.")

(defun synthesize-event-id (timestamp type-string data-repr)
  "Derive a deterministic id from (TIMESTAMP TYPE-STRING DATA-REPR).
   Same inputs → same id, so re-parsing the same line is idempotent.
   8-hex-char digest keeps ids compact and recognisable."
  (format nil "synth-~A-~A-~8,'0X"
          (or timestamp 0)
          (or type-string "unknown")
          (logand most-positive-fixnum
                  (sxhash (list timestamp type-string data-repr)))))

(define-condition event-parse-error (error)
  ((reason :initarg :reason :reader event-parse-error-reason)
   (raw :initarg :raw :reader event-parse-error-raw))
  (:report (lambda (c s)
             (format s "event-parse-error: ~A (raw: ~S)"
                     (event-parse-error-reason c)
                     (let ((r (event-parse-error-raw c)))
                       (if (and (stringp r) (> (length r) 80))
                           (concatenate 'string (subseq r 0 80) "...")
                           r))))))

(defun json-string-to-event (str)
  "Parse JSON string to event struct, tolerant of old-format lines.

   Returns (VALUES EVENT SYNTHESIZED-P).  SYNTHESIZED-P is T iff THIS
   call had to synthesize the id (i.e. the raw line lacked one).  It
   stays NIL when the raw line already carried an id, even one whose
   value matches the synth- prefix (i.e. already baked in by a prior
   repair pass).  Repair callers use the second value to detect a
   genuine fix-up vs an idempotent re-parse.

   Recovery rules:
   - Missing/null id        → synthesised from (timestamp,type,data); counter incremented.
   - Missing/null session   → defaults to \"\".
   - Missing/null clock     → defaults to empty vector-clock.
   - Missing/null data      → defaults to NIL.
   - Missing/null timestamp → REJECTED (signals event-parse-error).
   - Missing/null type      → REJECTED.
   - JSON parse failure     → REJECTED."
  (let* ((alist (handler-case (yason:parse str :object-as :alist)
                  (error (e)
                    (error 'event-parse-error
                           :reason (format nil "yason: ~A" e)
                           :raw str))))
         (timestamp (cdr (assoc "timestamp" alist :test #'string=)))
         (type-str (cdr (assoc "type" alist :test #'string=)))
         (id-raw (cdr (assoc "id" alist :test #'string=)))
         (session (or (cdr (assoc "session" alist :test #'string=)) ""))
         (clock-alist (cdr (assoc "clock" alist :test #'string=)))
         (data-alist (cdr (assoc "data" alist :test #'string=)))
         (vc (make-vector-clock))
         (synth-p nil))
    (unless (integerp timestamp)
      (error 'event-parse-error
             :reason "missing or non-integer timestamp" :raw str))
    (unless (and type-str (stringp type-str))
      (error 'event-parse-error
             :reason "missing or non-string type" :raw str))
    (when clock-alist
      (loop for (k . v) in clock-alist
            do (setf (gethash k (vc-entries vc)) v)))
    (let ((id (cond
                ((and id-raw (stringp id-raw) (> (length id-raw) 0)) id-raw)
                (t
                 (setf synth-p t)
                 (sb-ext:atomic-incf *synthesized-event-ids*)
                 (synthesize-event-id timestamp type-str
                                      (when data-alist
                                        (write-to-string data-alist
                                                         :readably nil
                                                         :pretty nil
                                                         :length 32
                                                         :level 4)))))))
      (values
       (make-event
        :id id
        :timestamp timestamp
        :session session
        :clock vc
        :type (intern (string-upcase type-str) :keyword)
        :data (when data-alist (alist-to-plist data-alist)))
       synth-p))))
