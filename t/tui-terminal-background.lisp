(in-package #:kli/tests)

(in-suite all)

(defun bg-approx (a b)
  (< (abs (- a b)) 1d-3))

(defun bg-gray (byte)
  (let ((c (/ byte 255d0)))
    (list c c c)))

(test osc-color-report-parses-16-bit-channels
  (let ((rgb (tui-terminal:parse-osc-color-report
              (format nil "~C]11;rgb:1111/1111/1111~C" #\Esc #\Bel))))
    (is (= 3 (length rgb)))
    (is (every (lambda (c) (bg-approx c 0.0667d0)) rgb))))

(test osc-color-report-parses-full-white
  (let ((rgb (tui-terminal:parse-osc-color-report "rgb:ffff/ffff/ffff")))
    (is (every (lambda (c) (bg-approx c 1.0d0)) rgb))))

(test osc-color-report-parses-8-bit-channels
  (let ((rgb (tui-terminal:parse-osc-color-report "rgb:1c/2b/3a")))
    (is (bg-approx (first rgb) 0.1098d0))
    (is (bg-approx (second rgb) 0.1686d0))
    (is (bg-approx (third rgb) 0.2275d0))))

(test osc-color-report-handles-st-terminator
  (let ((rgb (tui-terminal:parse-osc-color-report
              (format nil "~C]11;rgb:0000/0000/0000~C\\" #\Esc #\Esc))))
    (is (= 3 (length rgb)))
    (is (eq :dark (tui-terminal:classify-background rgb)))))

(test osc-color-report-rejects-garbage
  (is (null (tui-terminal:parse-osc-color-report "garbage")))
  (is (null (tui-terminal:parse-osc-color-report
             (format nil "~C]11;rgb:0000/00" #\Esc)))))

(test relative-luminance-matches-wcag
  (is (bg-approx (tui-terminal:relative-luminance (bg-gray #x11)) 0.0056d0))
  (is (bg-approx (tui-terminal:relative-luminance (bg-gray #x80)) 0.216d0)))

(test classify-background-splits-at-mid-luminance
  (is (eq :dark (tui-terminal:classify-background (bg-gray #x11))))
  (is (eq :light (tui-terminal:classify-background (list 1d0 1d0 1d0))))
  (is (eq :dark (tui-terminal:classify-background (list 0d0 0d0 0d0))))
  (is (eq :dark (tui-terminal:classify-background (bg-gray #x80))))
  (is (eq :light (tui-terminal:classify-background
                  (list (/ #xfd 255d0) (/ #xf6 255d0) (/ #xe3 255d0)))))
  (is (eq :dark (tui-terminal:classify-background
                 (list (/ #x00 255d0) (/ #x2b 255d0) (/ #x36 255d0))))))

(test colorfgbg-classifies-by-background-index
  (is (eq :dark (tui-terminal:parse-colorfgbg "15;0")))
  (is (eq :light (tui-terminal:parse-colorfgbg "0;15")))
  (is (eq :dark (tui-terminal:parse-colorfgbg "8")))
  (is (eq :light (tui-terminal:parse-colorfgbg "7")))
  (is (null (tui-terminal:parse-colorfgbg "default;default")))
  (is (null (tui-terminal:parse-colorfgbg nil)))
  (is (null (tui-terminal:parse-colorfgbg ""))))

(test da1-reply-position-finds-complete-reply
  (is (= 7 (tui-terminal:da1-reply-position (format nil "~C[?1;2c" #\Esc))))
  (is (null (tui-terminal:da1-reply-position (format nil "~C[?1;2" #\Esc)))))

(defun bg-run-handshake (input &key (timeout 0.05))
  (let* ((written (make-string-output-stream))
         (write-fn (lambda (s) (write-string s written))))
    (multiple-value-bind (status residual)
        (tui-terminal:query-terminal-background
         (make-string-input-stream input) write-fn :timeout timeout)
      (values status residual (get-output-stream-string written)))))

(test handshake-writes-osc-query-with-da1-sentinel
  (multiple-value-bind (status residual query)
      (bg-run-handshake (format nil "~C]11;rgb:1111/1111/1111~C" #\Esc #\Bel))
    (declare (ignore status residual))
    (is (string= (format nil "~C]11;?~C\\~C[c" #\Esc #\Esc #\Esc) query))))

(test handshake-parses-supporting-terminal-reply
  (let ((status (bg-run-handshake
                 (format nil "~C]11;rgb:1111/1111/1111~C" #\Esc #\Bel))))
    (is (eq :dark (tui-terminal:classify-background status)))))

(test handshake-detects-no-support-via-da1-sentinel
  (is (eq :no-support (bg-run-handshake (format nil "~C[?1;2c" #\Esc)))))

(test handshake-times-out-on-silence
  (is (eq :timeout (bg-run-handshake "")))
  (is (eq :timeout (bg-run-handshake "xyz"))))

(test handshake-preserves-typed-ahead-in-residual
  (multiple-value-bind (status residual)
      (bg-run-handshake (format nil "abc~C]11;rgb:1111/1111/1111~C" #\Esc #\Bel))
    (is (eq :dark (tui-terminal:classify-background status)))
    (is (search "abc" residual))))

(test resolve-background-mode-prefers-rgb-then-colorfgbg-then-dark
  (is (eq :dark (tui-terminal:resolve-background-mode
                 (list 0.06d0 0.06d0 0.06d0) nil)))
  (is (eq :light (tui-terminal:resolve-background-mode (list 1d0 1d0 1d0) nil)))
  (is (eq :dark (tui-terminal:resolve-background-mode :no-support "15;0")))
  (is (eq :light (tui-terminal:resolve-background-mode :no-support "0;15")))
  (is (eq :dark (tui-terminal:resolve-background-mode :timeout nil))))
