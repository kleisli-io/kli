(in-package #:kli/tui/markdown)

(defun lang->coloring (lang)
  "Fenced-block language tag → a colorize coloring type, or NIL (render plain)."
  (when (and lang (plusp (length lang)))
    (let ((l (string-downcase lang)))
      (cond ((member l '("cl" "lisp" "common-lisp" "commonlisp") :test #'string=) :common-lisp)
            ((string= l "elisp") :elisp)
            ((string= l "scheme") :scheme)
            ((string= l "clojure") :clojure)
            ((string= l "python") :python)
            ((string= l "c") :c)
            ((member l '("c++" "cpp" "cxx") :test #'string=) :c++)
            ((string= l "java") :java)
            ((string= l "haskell") :haskell)
            (t nil)))))

(defun number-token-p (s)
  (and (plusp (length s))
       (let ((*read-eval* nil))
         (multiple-value-bind (v pos) (ignore-errors (read-from-string s nil nil))
           (and (numberp v) (eql pos (length s)))))))

(defun lisp-symbol-token (text)
  "Refine colorize's coarse :symbol class into the Dark+ keyword/function/
   variable split that a plain CL scan can't give."
  (cond ((number-token-p text) "syntaxNumber")
        ((let ((s (find-symbol (string-upcase text) :cl)))
           (and s (or (special-operator-p s) (macro-function s))))
         "syntaxKeyword")
        ((let ((s (find-symbol (string-upcase text) :cl)))
           (and s (fboundp s)))
         "syntaxFunction")
        (t "syntaxVariable")))

(defun fragment-token (mode text lispp)
  "Colorize scan MODE → a theme token name, or NIL for default text."
  (case mode
    ((:comment) "syntaxComment")
    ((:string :character) "syntaxString")
    ((:keyword) (if lispp "syntaxType" "syntaxKeyword"))
    ((:symbol) (if lispp (lisp-symbol-token text) "syntaxVariable"))
    (t nil)))

(defvar *highlight-cache* (make-hash-table :test 'equal)
  "Memoizes HIGHLIGHT-CODE-LINES keyed by (code lang theme), each entry a
   (seg-lines . tick) cons. The transcript re-renders committed events on
   redraws and reflows, so without this colorize re-scans every fenced block
   per repaint. Cached seg-lines are never mutated downstream (CHAR-WRAP-SEGS
   copies before emitting), so sharing them across frames is safe. THEME is a
   struct, compared by identity. Bounded by *HIGHLIGHT-CACHE-LIMIT* with
   least-recently-used eviction.")

(defparameter *highlight-cache-limit* 256
  "Entries kept in *HIGHLIGHT-CACHE* before the least recently used one is
   evicted, bounding retained seg-lines over a long session.")

(defvar *highlight-cache-tick* 0
  "Monotonic use counter stamped on cache entries for LRU eviction.")

(defvar *highlight-memoize-p* t
  "When NIL, HIGHLIGHT-CODE-LINES still reads the cache but never inserts.
   Bound off around the open streaming event's render, where a growing fenced
   block would otherwise insert one entry per delta for each partial prefix.")

(defvar *hl-stream* nil
  "When bound to an HL-STREAM, HIGHLIGHT-CODE-LINES renders the growing fenced
   block incrementally against it instead of re-scanning the whole block each
   delta. The streaming transcript path binds it per open reply alongside
   *MD-STREAM*, so an open code block — which mdstream cannot advance past, as it
   has no safe markdown boundary inside the fence — re-scans only its open tail.")

(defun evict-stalest-highlight ()
  "Drop the least recently used *HIGHLIGHT-CACHE* entry."
  (let ((victim nil) (stalest most-positive-fixnum))
    (maphash (lambda (key entry)
               (when (< (cdr entry) stalest)
                 (setf stalest (cdr entry)
                       victim key)))
             *highlight-cache*)
    (when victim
      (remhash victim *highlight-cache*))))

(defmacro with-quiet-output (&body body)
  "Run BODY with *standard-output* and *trace-output* sunk to a discarding
   stream. The renderer runs on the main thread where these are the real process
   terminal, so a third-party text routine writing to them would corrupt the
   frame."
  (let ((sink (gensym "SINK")))
    `(let* ((,sink (make-broadcast-stream))
            (*standard-output* ,sink)
            (*trace-output* ,sink))
       ,@body)))

(defun compute-highlight-code-lines (code lang theme)
  "Scan CODE for LANG and map colorize fragments to themed seg-lines, one per
   source line. An unknown LANG yields one plain seg per line. Tabs are expanded
   so visible width equals character count. Colorize's debug default is bound off
   and its streams sunk so a scan can never scribble on the TTY."
  (let* ((code (normalize-text code))
         (kw (lang->coloring lang))
         (ct (and kw (find-coloring-type kw))))
    (if (null ct)
        (mapcar (lambda (l) (list (make-seg l))) (split-lines code))
        (let* ((lispp (and (member kw '(:common-lisp :elisp :scheme :clojure)) t))
               (lines '()) (cur '())
               (frags (with-quiet-output
                        (let ((colorize::*debug* nil))
                          (scan-string ct code)))))
          (dolist (frag frags)
            (let* ((mode (caar frag))
                   (text (cdr frag))
                   (token (fragment-token mode text lispp))
                   (fg (and token (md theme token)))
                   (start 0))
              (loop for nl = (position #\Newline text :start start)
                    do (let ((piece (subseq text start (or nl (length text)))))
                         (when (plusp (length piece))
                           (push (make-seg piece fg) cur)))
                       (when nl
                         (push (nreverse cur) lines)
                         (setf cur '() start (1+ nl)))
                    while nl)))
          (push (nreverse cur) lines)
          (nreverse lines)))))

;;; Incremental highlight for one growing fenced code block (the open streaming
;;; reply). compute-highlight-code-lines re-scans the whole block each delta, and
;;; colorize:scan-string dominates that render, so a long streamed block costs
;;; O(block^2). Instead commit completed code lines up to the latest SAFE
;;; scan-restart boundary and re-scan only the open tail.
;;;
;;; SAFE BOUNDARY (sound, language-agnostic): scan-string is a stateful pushdown
;;; lexer; every fragment carries its full state (current-mode . mode-stack). A
;;; newline is a safe restart point iff EVERY mode in the fragment containing it
;;; is line-neutral. Any construct mode (:in-list :string :multiline :comment ...)
;;; anywhere in the state means an open construct spans the newline -> unsafe.
;;; Conservative by construction: an unrecognized mode reads as a construct, which
;;; costs an optimization, never correctness. The committed prefix is final by the
;;; standard lexer-restart property: at a boundary in the default state with no
;;; open construct, scan(prefix ++ tail) == scan(prefix) ++ scan(tail), so the
;;; stitched output is byte-identical to a full render. The safe offsets are read
;;; out of the SAME tail scan that builds the seg-lines — no extra scan.

(defparameter *neutral-hl-modes* '(:normal :first-char-on-line :newline)
  "Line-neutral colorize scan modes. A newline whose fragment state (mode and
   whole mode-stack) is entirely neutral is a safe scan-restart boundary; any
   construct mode present means an open construct spans it.")

(defun hl-frag-neutral-p (key)
  "KEY is a fragment's (current-mode . mode-stack). T when every mode in it is
   line-neutral, so a newline inside this fragment is a safe restart boundary."
  (and (member (car key) *neutral-hl-modes*)
       (every (lambda (m) (member m *neutral-hl-modes*)) (cdr key))))

(defun scan-seglines+safenls (ntext lang theme base)
  "NTEXT must already be NORMALIZE-TEXT'd. Returns (values seglines
   safe-nl-offsets): seglines exactly as COMPUTE-HIGHLIGHT-CODE-LINES yields, plus
   ABSOLUTE (BASE + local) offsets just past each newline that sits in an
   all-neutral fragment — the safe scan-restart boundaries — read straight out of
   the one scan."
  (let* ((kw (lang->coloring lang)) (ct (and kw (find-coloring-type kw))))
    (if (null ct)
        (values (mapcar (lambda (l) (list (make-seg l))) (split-lines ntext))
                (loop for i below (length ntext)
                      when (char= (char ntext i) #\Newline) collect (+ base i 1)))
        (let* ((lispp (and (member kw '(:common-lisp :elisp :scheme :clojure)) t))
               (lines '()) (cur '()) (safe '())
               (frags (with-quiet-output
                        (let ((colorize::*debug* nil)) (scan-string ct ntext))))
               (pos 0))
          (dolist (frag frags)
            (let* ((key (car frag)) (mode (car key)) (text (cdr frag))
                   (neutralp (hl-frag-neutral-p key))
                   (token (fragment-token mode text lispp))
                   (fg (and token (md theme token)))
                   (start 0))
              (loop for nl = (position #\Newline text :start start)
                    do (let ((piece (subseq text start (or nl (length text)))))
                         (when (plusp (length piece)) (push (make-seg piece fg) cur)))
                       (when nl
                         (push (nreverse cur) lines)
                         (when neutralp (push (+ pos base nl 1) safe))
                         (setf cur '() start (1+ nl)))
                    while nl)
              (incf pos (length text))))
          (push (nreverse cur) lines)
          (values (nreverse lines) (nreverse safe))))))

(defstruct (hl-stream (:constructor make-hl-stream))
  "Incremental highlight state for one growing code block. STABLE-LINES caches the
   committed prefix's seg-lines; STABLE-TAIL is its last cons for O(commit)
   splicing. ANCHOR is an offset into the NORMALIZED text, always 0 or a proven
   safe boundary. LAST-NCODE is the previous render's normalized code: a new code
   that does not extend it (a different block reusing the stream) forces a reset,
   so the committed prefix is never mistaken for an unrelated block's head."
  (lang :unset) (theme :unset) (anchor 0) (stable-lines '()) (stable-tail nil)
  (last-ncode ""))

(defun hl-reset (st lang theme)
  (setf (hl-stream-lang st) lang (hl-stream-theme st) theme
        (hl-stream-anchor st) 0 (hl-stream-stable-lines st) '()
        (hl-stream-stable-tail st) nil))

(defun hlstream-render (st code lang theme)
  "Incremental analogue of (COMPUTE-HIGHLIGHT-CODE-LINES CODE LANG THEME) for a
   growing CODE. Resets when LANG/THEME change or CODE fails to extend the
   committed prefix (NORMALIZE-TEXT is a per-char map, so a true append always
   extends and a different block never does); otherwise commits completed lines up
   to the latest safe boundary and re-scans only the open tail. Output is
   byte-identical to a full render."
  (let ((ncode (normalize-text code)))
    (when (or (not (equal lang (hl-stream-lang st)))
              (not (eql theme (hl-stream-theme st)))
              (not (string-prefix-p (hl-stream-last-ncode st) ncode)))
      (hl-reset st lang theme))
    (setf (hl-stream-last-ncode st) ncode)
    (let ((anchor (hl-stream-anchor st)))
      (multiple-value-bind (tail-segs safe-nls)
          (scan-seglines+safenls (subseq ncode anchor) lang theme anchor)
        (let ((b nil))
          (dolist (o safe-nls)              ; largest safe newline strictly inside
            (when (and (> o anchor) (< o (length ncode))) (setf b o)))
          (when b
            (let* ((k (count #\Newline ncode :start anchor :end b))
                   (commit (subseq tail-segs 0 k)))
              (if (hl-stream-stable-tail st)
                  (setf (cdr (hl-stream-stable-tail st)) commit)
                  (setf (hl-stream-stable-lines st) commit))
              (when commit (setf (hl-stream-stable-tail st) (last commit)))
              (setf (hl-stream-anchor st) b
                    tail-segs (nthcdr k tail-segs))))
          (if (hl-stream-stable-lines st)
              (append (hl-stream-stable-lines st) tail-segs)
              tail-segs))))))

(defun highlight-code-lines (code lang theme)
  "→ a list of seg-lines for CODE, syntax-coloured when LANG is a known colorize
   type. Memoized on (code lang theme) so a re-rendered frame reuses prior
   work, bounded to *HIGHLIGHT-CACHE-LIMIT* entries (LRU). When *HL-STREAM* is
   bound (the open streaming reply), a miss renders incrementally against it
   instead. With *HIGHLIGHT-MEMOIZE-P* off, a miss computes without inserting."
  (let* ((key (list code lang theme))
         (hit (gethash key *highlight-cache*)))
    (if hit
        (progn (setf (cdr hit) (incf *highlight-cache-tick*)) (car hit))
        (handler-case
            (cond
              (*hl-stream*
               (hlstream-render *hl-stream* code lang theme))
              ((not *highlight-memoize-p*)
               (compute-highlight-code-lines code lang theme))
              (t
               (when (>= (hash-table-count *highlight-cache*)
                         *highlight-cache-limit*)
                 (evict-stalest-highlight))
               (let ((lines (compute-highlight-code-lines code lang theme)))
                 (setf (gethash key *highlight-cache*)
                       (cons lines (incf *highlight-cache-tick*)))
                 lines)))
          (error ()
            ;; colorize's lexer can over-read a partial construct — input ending
            ;; mid-escape (a trailing backslash inside a string) drives
            ;; :single-escape past end. A growing streamed block hits this
            ;; whenever a delta lands there. Degrade that one frame to plain lines
            ;; rather than fault the render; the next delta re-highlights.
            (mapcar (lambda (l) (list (make-seg l)))
                    (split-lines (normalize-text code))))))))
