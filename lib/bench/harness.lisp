(in-package #:kli-bench)

;;; Timing harness for KLI benchmarks
;;; Captures wall-time (µs) and consing (bytes) per iteration.

(defun median (sorted-vec)
  "Median of a sorted vector of numbers."
  (let ((n (length sorted-vec)))
    (if (oddp n)
        (aref sorted-vec (floor n 2))
        (/ (+ (aref sorted-vec (1- (floor n 2)))
              (aref sorted-vec (floor n 2)))
           2.0d0))))

(defun percentile (sorted-vec p)
  "P-th percentile (0-100) of a sorted vector."
  (let* ((n (length sorted-vec))
         (idx (min (1- n) (floor (* n p) 100))))
    (aref sorted-vec idx)))

(defmacro run-benchmark (name &key (iterations 100) (warmup 10) body)
  "Run BODY for ITERATIONS (after WARMUP), collect wall-time µs and consing bytes.
   Returns plist (:name :iterations :times :consing :stats)."
  (let ((times (gensym "TIMES"))
        (cons-vec (gensym "CONS"))
        (i (gensym "I"))
        (t0 (gensym "T0"))
        (c0 (gensym "C0"))
        (elapsed (gensym "ELAPSED")))
    `(let ((,times (make-array ,iterations :element-type 'double-float))
           (,cons-vec (make-array ,iterations :element-type 'fixnum)))
       ;; Warmup
       (dotimes (,i ,warmup)
         (declare (ignorable ,i))
         ,body)
       ;; Timed runs
       (dotimes (,i ,iterations)
         (let ((,c0 (sb-ext:get-bytes-consed))
               (,t0 (get-internal-real-time)))
           ,body
           (let ((,elapsed (- (get-internal-real-time) ,t0)))
             (setf (aref ,times ,i)
                   (* 1.0d6 (/ (coerce ,elapsed 'double-float)
                                internal-time-units-per-second)))
             (setf (aref ,cons-vec ,i)
                   (- (sb-ext:get-bytes-consed) ,c0)))))
       ;; Sort for percentile computation
       (sort ,times #'<)
       (sort ,cons-vec #'<)
       (list :name ,name
             :iterations ,iterations
             :stats (list :min (aref ,times 0)
                          :median (median ,times)
                          :mean (/ (reduce #'+ ,times) ,iterations)
                          :p99 (percentile ,times 99)
                          :max (aref ,times (1- ,iterations)))
             :consing (list :min (aref ,cons-vec 0)
                            :median (median ,cons-vec)
                            :max (aref ,cons-vec (1- ,iterations)))))))

;;; Result formatting

(defun format-result-row (stream result)
  "Format a single benchmark result as a table row."
  (let ((stats (getf result :stats))
        (consing (getf result :consing)))
    (format stream "  ~40A ~10,1F ~10,1F ~10,1F ~10,1F ~12,0F~%"
            (getf result :name)
            (getf stats :min)
            (getf stats :median)
            (getf stats :p99)
            (getf stats :max)
            (getf consing :median))))

(defun format-results (results &key (title "Benchmark Results"))
  "Pretty-print a list of benchmark results as a table."
  (format t "~%~A (~A)~%" title
          (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
            (format nil "~2,'0D:~2,'0D:~2,'0D" h m s)))
  (format t "  ~40A ~10A ~10A ~10A ~10A ~12A~%"
          "Benchmark" "Min µs" "Med µs" "P99 µs" "Max µs" "Med bytes")
  (format t "  ~40,,,'-A ~10,,,'-A ~10,,,'-A ~10,,,'-A ~10,,,'-A ~12,,,'-A~%"
          "" "" "" "" "" "")
  (dolist (r results)
    (format-result-row t r))
  (terpri)
  results)

;;; Suite runner infrastructure

(defvar *suites* (make-hash-table :test 'equal)
  "Registry of benchmark suites: name -> thunk returning results list.")

(defmacro define-suite (name &body body)
  "Register a benchmark suite. BODY should return a list of results."
  `(setf (gethash ,(string-downcase (symbol-name name)) *suites*)
         (lambda () ,@body)))

(defun run-suite (name)
  "Run a named suite and format results."
  (let ((thunk (gethash (string-downcase name) *suites*)))
    (if thunk
        (format-results (funcall thunk) :title (format nil "~A Benchmarks" name))
        (format t "Unknown suite: ~A~%" name))))

(defun run-all ()
  "Run all registered benchmark suites."
  (let ((all-results nil))
    (maphash (lambda (name thunk)
               (format t "~%=== ~A ===~%" name)
               (let ((results (funcall thunk)))
                 (format-results results :title name)
                 (setf all-results (append all-results results))))
             *suites*)
    all-results))
