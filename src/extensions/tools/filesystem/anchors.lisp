(in-package #:kli/tools/filesystem)

(defun fnv-fold-byte (string)
  "FNV-1a/32 of STRING xor-folded to 8 bits. Codepoints are fed
low-byte-first so the hash is unicode-safe and deterministic across
images."
  (let ((h 2166136261))
    (declare (type (unsigned-byte 32) h))
    (loop for ch across string
          for code = (char-code ch)
          do (loop
               (setf h (ldb (byte 32 0)
                            (* (logxor h (logand code #xff)) 16777619)))
               (setf code (ash code -8))
               (when (zerop code) (return))))
    (logxor (ldb (byte 8 0) h) (ldb (byte 8 8) h)
            (ldb (byte 8 16) h) (ldb (byte 8 24) h))))

(defun line-hash (line)
  "Two lowercase hex digits hashing LINE's raw content — the HH in a
LINE:HH anchor. Edits reference anchors and validation re-hashes disk."
  (format nil "~(~2,'0x~)" (fnv-fold-byte line)))

(defun split-file-lines (content)
  "Split CONTENT into a simple-vector of lines. A trailing newline
terminates the last line rather than opening an empty one. Empty
content has no lines."
  (let ((lines (make-array 16 :adjustable t :fill-pointer 0))
        (start 0))
    (loop for position = (position #\Newline content :start start)
          while position
          do (vector-push-extend (subseq content start position) lines)
             (setf start (1+ position)))
    (when (< start (length content))
      (vector-push-extend (subseq content start) lines))
    (coerce lines 'simple-vector)))

(defun clamp-range (line-count start end path)
  "Resolve START/END (1-based, inclusive, NIL = file bound) against
LINE-COUNT. END clamps down silently. A START past EOF is an error with
guidance so the model can recover."
  (let ((from (max 1 (or start 1)))
        (to (min (or end line-count) line-count)))
    (when (> from line-count)
      (error "Range starts at line ~D but ~A has only ~D line~:P. ~
              Re-read without a range or use start <= ~D."
             from path line-count line-count))
    (when (< to from)
      (error "Range end ~D is before start ~D." to from))
    (values from to)))

(defun render-window (line mstart mend)
  "LINE windowed to *render-line-limit* chars around the match [MSTART,MEND),
with [+N chars] head and tail markers, each omitted at a line boundary. A
line within the limit passes through. A match wider than the limit shows its
first *render-line-limit* chars."
  (let ((limit *render-line-limit*)
        (len (length line)))
    (if (<= len limit)
        line
        (let ((mlen (- mend mstart)))
          (multiple-value-bind (wstart wend)
              (if (>= mlen limit)
                  (values mstart (min len (+ mstart limit)))
                  (let* ((w0 (max 0 (- mstart (floor (- limit mlen) 2))))
                         (we (min len (+ w0 limit))))
                    (values (max 0 (- we limit)) we)))
            (format nil "~@[[+~D chars]~]~A~@[[+~D chars]~]"
                    (when (plusp wstart) wstart)
                    (subseq line wstart wend)
                    (when (< wend len) (- len wend))))))))

(defun render-anchored-row (line-number line &key (prefix "") rendered-line)
  "Model-facing anchored source row. PREFIX is metadata before the anchor; the
first | after LINE:HH is syntax, and source content starts after it."
  (format nil "~A~D:~A|~A"
          prefix line-number (line-hash line)
          (or rendered-line (render-truncate-front line))))

(defun render-anchored-lines (lines start end &optional (line-offset 0))
  (with-output-to-string (out)
    (loop for index from (1- start) below end
          for line = (svref lines index)
          do (unless (= index (1- start))
               (terpri out))
             (write-string
              (render-anchored-row (+ line-offset index 1) line)
              out))))

(defun render-raw-lines (lines start end)
  (with-output-to-string (out)
    (loop for index from (1- start) below end
          do (unless (= index (1- start))
               (terpri out))
             (write-string (render-truncate-front (svref lines index)) out))))

(defconstant +anchor-cache-key+ :filesystem/anchor-cache)

(defun anchor-cache (protocol)
  "Truename-keyed table of what the model last saw per file. Enforces
read-before-edit and powers error guidance, never validity. Edit
validation re-hashes disk content."
  (ensure-protocol-storage protocol +anchor-cache-key+
                           (lambda () (make-hash-table :test #'equal))))

(defstruct anchor-entry
  (hashes #() :type simple-vector)
  line-hashes
  (read-time 0 :type integer))

(defun anchor-cache-key (path)
  (namestring (truename path)))

(defun remember-read (protocol path lines)
  "Record LINES as the model's last view of PATH."
  (setf (gethash (anchor-cache-key path) (anchor-cache protocol))
        (make-anchor-entry
         :hashes (map 'simple-vector #'line-hash lines)
         :read-time (get-universal-time))))

(defun remember-read-hashes (protocol path line-hashes)
  "Record LINE-HASHES as the model's last view of PATH without retaining the file's lines."
  (let ((hashes (coerce line-hashes 'simple-vector)))
    (setf (gethash (anchor-cache-key path) (anchor-cache protocol))
          (make-anchor-entry
           :hashes hashes
           :line-hashes hashes
           :read-time (get-universal-time)))))

(defun anchor-known-p (protocol path line hash)
  "Classify LINE:HASH for PATH against what the model last saw:
:VALID on match, :STALE on mismatch, :UNREAD when PATH was never read."
  (let ((entry (gethash (anchor-cache-key path) (anchor-cache protocol))))
    (cond ((null entry) :unread)
          ((let ((hashes (anchor-entry-hashes entry)))
             (and (<= 1 line (length hashes))
                  (string= hash (svref hashes (1- line)))))
           :valid)
          (t :stale))))
