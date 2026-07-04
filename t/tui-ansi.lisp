(in-package #:kli/tests)
(in-suite all)

(defun seg-triple (s)
  "Seg -> (text (r g b)|nil attrs) for compact assertions."
  (list (tui-markdown:seg-text s)
        (let ((fg (tui-markdown:seg-fg s)))
          (and fg (list (tui-style:color-r fg) (tui-style:color-g fg) (tui-style:color-b fg))))
        (tui-markdown:seg-attrs s)))

(defun triples (string)
  (mapcar #'seg-triple (tui-ansi:ansi->segs string)))

(defun esc (&rest codes)
  (apply #'tui-style:sgr codes))

(test ansi-plain-text-one-seg
  (is (equal '(("hello" nil nil)) (triples "hello")))
  (is (null (tui-ansi:ansi->segs ""))))

(test ansi-truecolor-fg-and-reset-splits-run
  ;; 38;2 decodes to make-color; ESC[0m ends the run and clears the style.
  (is (equal `(("red" (200 80 80) nil) (" tail" nil nil))
             (triples (concatenate 'string (esc 38 2 200 80 80) "red" (esc 0) " tail")))))

(test ansi-256-color-cube-basic-and-gray
  (flet ((fg (n) (second (car (triples (concatenate 'string (esc 38 5 n) "x"))))))
    (is (equal '(255 0 0) (fg 196)))   ; 6x6x6 cube
    (is (equal '(128 0 0) (fg 1)))     ; basic palette
    (is (equal '(8 8 8) (fg 232)))))   ; gray ramp

(test ansi-basic-and-bright-fg
  (is (equal '(128 0 0) (second (car (triples (concatenate 'string (esc 31) "x"))))))
  (is (equal '(255 0 0) (second (car (triples (concatenate 'string (esc 91) "x")))))))

(test ansi-fg-reset-39-clears-colour
  (is (equal `(("a" (200 80 80) nil) ("b" nil nil))
             (triples (concatenate 'string (esc 38 2 200 80 80) "a" (esc 39) "b")))))

(test ansi-attrs-decode-in-canonical-order
  (is (equal '(("x" nil (:bold :italic)))
             (triples (concatenate 'string (esc 1 3) "x"))))
  ;; 22 clears bold+dim, leaving the run boundary.
  (is (equal '(("a" nil (:bold)) ("b" nil nil))
             (triples (concatenate 'string (esc 1) "a" (esc 22) "b")))))

(test ansi-non-sgr-csi-stripped-without-breaking-run
  (is (equal '(("ab" nil nil))
             (triples (format nil "a~C[2Kb" #\Esc))))
  (is (equal '(("ab" (128 0 0) nil))
             (triples (concatenate 'string (esc 31) "a" (format nil "~C[K" #\Esc) "b"))))
  (is (equal '(("ab" nil nil))
             (triples (format nil "a~C[10;20Hb" #\Esc)))))

(test ansi-osc-stripped
  (is (equal '(("ab" nil nil))
             (triples (format nil "a~C]0;title~Cb" #\Esc (code-char 7))))))

(test ansi-tabs-expanded-to-plain-text
  (let ((segs (tui-ansi:ansi->segs (format nil "a~Cb" #\Tab))))
    (is (= 1 (length segs)))
    (is (string= "a   b" (tui-markdown:seg-text (car segs))))))

(test ansi-background-consumed-not-carried
  ;; 48;2 consumes r;g;b; the trailing 31 still applies as fg; 49 leaves fg alone.
  (is (equal '(("x" (128 0 0) nil))
             (triples (concatenate 'string (esc 48 2 10 20 30 31) "x"))))
  (is (equal '(("x" nil nil))
             (triples (concatenate 'string (esc 48 2 40 50 40) "x" (esc 49))))))

(test ansi-adjacent-identical-style-merged
  (is (equal '(("ab" (128 0 0) nil))
             (triples (concatenate 'string (esc 31) "a" (esc 31) "b")))))

(test ansi-seg-text-is-pure-visible-content
  (let ((segs (tui-ansi:ansi->segs
               (concatenate 'string (esc 38 2 1 2 3) (format nil "he~Cllo" #\Tab) (esc 0) "!"))))
    (dolist (s segs)
      (is (null (find #\Esc (tui-markdown:seg-text s))))
      (is (= (text:visible-width (tui-markdown:seg-text s))
             (length (tui-markdown:seg-text s)))))))
