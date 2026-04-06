(in-package #:kli-bench)

;;; CRDT Benchmarks
;;; OR-Set, Vector Clock, G-Set at varying scale points.

(defvar *crdt-scales* '(100 1000 10000)
  "Element counts for CRDT benchmarks.")

(defvar *vc-dimensions* '(2 10 50 100)
  "Session counts for vector clock merge benchmarks.")

;;; OR-Set benchmarks

(defun bench-ors-add (n &key (iterations 100))
  "Benchmark adding N elements to an OR-Set."
  (run-benchmark (format nil "ors-add ~D" n)
    :iterations iterations :warmup 5
    :body (let ((ors (make-or-set)))
            (dotimes (i n)
              (ors-add ors (format nil "e~D" i) (format nil "t~D" i))))))

(defun bench-ors-remove (n &key (iterations 100))
  "Benchmark removing N/2 elements from an N-element OR-Set."
  (let ((ors (make-or-set)))
    (dotimes (i n)
      (ors-add ors (format nil "e~D" i) (format nil "t~D" i)))
    (run-benchmark (format nil "ors-remove ~D/~D" (floor n 2) n)
      :iterations iterations :warmup 5
      :body (let ((copy (make-or-set)))
              ;; Rebuild each time for fair measurement
              (dotimes (i n)
                (ors-add copy (format nil "e~D" i) (format nil "t~D" i)))
              (dotimes (i (floor n 2))
                (ors-remove copy (format nil "e~D" i)))))))

(defun bench-ors-members (n &key (iterations 100))
  "Benchmark ors-members scan on N-element OR-Set (half removed)."
  (let ((ors (make-or-set)))
    (dotimes (i n)
      (ors-add ors (format nil "e~D" i) (format nil "t~D" i)))
    (dotimes (i (floor n 2))
      (ors-remove ors (format nil "e~D" i)))
    (run-benchmark (format nil "ors-members ~D" n)
      :iterations iterations :warmup 5
      :body (ors-members ors))))

(defun bench-ors-merge (n &key (iterations 100))
  "Benchmark merging two N-element OR-Sets."
  (let ((ors1 (make-or-set))
        (ors2 (make-or-set)))
    (dotimes (i n)
      (ors-add ors1 (format nil "e~D" i) (format nil "s1:~D" i)))
    (dotimes (i n)
      (ors-add ors2 (format nil "e~D" i) (format nil "s2:~D" i)))
    (run-benchmark (format nil "ors-merge ~D+~D" n n)
      :iterations iterations :warmup 5
      :body (ors-merge ors1 ors2))))

;;; Vector Clock benchmarks

(defun make-populated-vc (n-sessions &optional (prefix "s"))
  "Create a vector clock with N-SESSIONS entries."
  (let ((vc (make-vector-clock)))
    (dotimes (i n-sessions)
      (let ((sid (format nil "~A~D" prefix i)))
        (dotimes (j (1+ (random 10)))
          (vc-increment vc sid))))
    vc))

(defun bench-vc-merge (n-sessions &key (iterations 200))
  "Benchmark merging two vector clocks with N-SESSIONS dimensions."
  (let ((vc1 (make-populated-vc n-sessions "a"))
        (vc2 (make-populated-vc n-sessions "b")))
    (run-benchmark (format nil "vc-merge ~D dims" n-sessions)
      :iterations iterations :warmup 10
      :body (vc-merge vc1 vc2))))

;;; G-Set benchmarks

(defun bench-gs-add (n &key (iterations 100))
  "Benchmark adding N elements to a G-Set."
  (run-benchmark (format nil "gs-add ~D" n)
    :iterations iterations :warmup 5
    :body (let ((gs (make-g-set)))
            (dotimes (i n)
              (gs-add gs (format nil "e~D" i))))))

(defun bench-gs-merge (n &key (iterations 100))
  "Benchmark merging two N-element G-Sets."
  (let ((gs1 (make-g-set))
        (gs2 (make-g-set)))
    (dotimes (i n)
      (gs-add gs1 (format nil "e~D" i)))
    (dotimes (i n)
      (gs-add gs2 (format nil "f~D" i)))
    (run-benchmark (format nil "gs-merge ~D+~D" n n)
      :iterations iterations :warmup 5
      :body (gs-merge gs1 gs2))))

;;; Suite definition

(defun run-crdt ()
  "Run all CRDT benchmarks."
  (let ((results nil))
    ;; OR-Set
    (dolist (n *crdt-scales*)
      (push (bench-ors-add n) results)
      (push (bench-ors-members n) results)
      (push (bench-ors-merge n) results))
    ;; Vector Clock
    (dolist (d *vc-dimensions*)
      (push (bench-vc-merge d) results))
    ;; G-Set
    (dolist (n *crdt-scales*)
      (push (bench-gs-add n) results)
      (push (bench-gs-merge n) results))
    (format-results (nreverse results) :title "CRDT")))

(define-suite crdt (run-crdt))
