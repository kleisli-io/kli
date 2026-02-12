;;;; css/tokens.lisp - Design token system using Let Over Lambda patterns
;;;;
;;;; PURPOSE:
;;;;   Design tokens as introspectable closures with validation and suggestions.
;;;;   Each token set responds to messages (:get, :set, :validate, :inspect, :all).
;;;;   Provides Levenshtein-distance "Did you mean?" suggestions for typos.
;;;;
;;;; USAGE:
;;;;   ;; Create a token set
;;;;   (defvar *my-colors*
;;;;     (make-token-set :colors
;;;;       '((:primary . "#00FF41")
;;;;         (:secondary . "#FF006E"))))
;;;;
;;;;   (funcall *my-colors* :get :primary)     ; => "#00FF41"
;;;;   (funcall *my-colors* :set :primary "#FF0000")
;;;;   (funcall *my-colors* :validate :primry) ; => Error: Did you mean :primary?
;;;;   (funcall *my-colors* :inspect)          ; => Full state dump
;;;;
;;;; GLOBAL TOKENS:
;;;;   *colors*, *typography*, *spacing*, *effects* - Current active tokens
;;;;   *default-colors*, etc. - Immutable defaults

(in-package :lol-reactive)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Levenshtein Distance Algorithm
;;; ─────────────────────────────────────────────────────────────────────────────

(defun levenshtein-distance (s1 s2)
  "Calculate edit distance between two strings.
   Returns minimum single-character edits (insert, delete, substitute)
   required to change s1 into s2."
  (let* ((len1 (length s1))
         (len2 (length s2))
         (matrix (make-array (list (1+ len1) (1+ len2))
                             :initial-element 0)))
    ;; Initialize first column (deletions from s1)
    (iter (for i from 0 to len1)
      (setf (aref matrix i 0) i))
    ;; Initialize first row (insertions to match s2)
    (iter (for j from 0 to len2)
      (setf (aref matrix 0 j) j))
    ;; Fill matrix using dynamic programming
    (iter (for i from 1 to len1)
      (iter (for j from 1 to len2)
        (let ((cost (if (char= (char s1 (1- i))
                               (char s2 (1- j)))
                        0
                        1)))
          (setf (aref matrix i j)
                (min (1+ (aref matrix (1- i) j))      ; deletion
                     (1+ (aref matrix i (1- j)))      ; insertion
                     (+ (aref matrix (1- i) (1- j))   ; substitution
                        cost))))))
    (aref matrix len1 len2)))

(defun find-closest-match (token alist)
  "Find the closest matching token from alist using Levenshtein distance.
   Returns the keyword with the smallest edit distance."
  (let* ((token-str (string-upcase (symbol-name token)))
         (distances (mapcar (lambda (entry)
                              (cons (car entry)
                                    (levenshtein-distance
                                     token-str
                                     (string-upcase (symbol-name (car entry))))))
                            alist))
         (sorted (sort distances #'< :key #'cdr)))
    (caar sorted)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Token Validation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun validate-token (token alist token-type-name)
  "Validate token exists in alist with helpful error on failure.
   - token: Keyword to validate
   - alist: Valid (keyword . value) pairs
   - token-type-name: Human-readable name for errors
   Returns token if valid, signals descriptive error if invalid."
  (unless (keywordp token)
    (error "~A token must be a keyword, got ~A of type ~A"
           token-type-name token (type-of token)))
  (let ((valid-entry (assoc token alist)))
    (unless valid-entry
      (let ((closest (find-closest-match token alist)))
        (error "Invalid ~A token: ~A. Did you mean: ~A?"
               token-type-name token closest)))
    token))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Token Set Factory (Let Over Lambda Pattern)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-token-set (name initial-tokens)
  "Create a token set with pandoric introspection.

   NAME: Keyword identifying the set (e.g., :colors)
   INITIAL-TOKENS: Alist of (keyword . value) pairs

   Returns a dlambda responding to:
     :name      - Get set name
     :get       - Get token value (validates)
     :get-raw   - Get token value (no validation, returns nil if missing)
     :set       - Set token value
     :all       - Get all tokens as alist
     :keys      - Get all token keys
     :validate  - Validate token exists (returns token or errors)
     :merge     - Merge additional tokens
     :inspect   - Full state dump

   Example:
   (make-token-set :colors
     '((:primary . \"#00FF41\")
       (:secondary . \"#FF006E\")))"
  (let ((set-name name)
        (tokens (copy-alist initial-tokens))
        (created-at (get-universal-time)))
    (dlambda
      (:name () set-name)

      (:get (key)
       (validate-token key tokens (symbol-name set-name))
       (cdr (assoc key tokens)))

      (:get-raw (key)
       (cdr (assoc key tokens)))

      (:set (key value)
       (let ((entry (assoc key tokens)))
         (if entry
             (setf (cdr entry) value)
             (push (cons key value) tokens)))
       value)

      (:all () tokens)

      (:keys () (mapcar #'car tokens))

      (:validate (key)
       (validate-token key tokens (symbol-name set-name)))

      (:merge (new-tokens)
       (iter (for (key . value) in new-tokens)
         (let ((entry (assoc key tokens)))
           (if entry
               (setf (cdr entry) value)
               (push (cons key value) tokens))))
       tokens)

      (:inspect ()
       (list :name set-name
             :created-at created-at
             :token-count (length tokens)
             :tokens tokens))

      (otherwise (message &rest args)
       (declare (ignore args))
       (error "Unknown token-set message: ~S" message)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Default Token Definitions
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *default-colors*
  '(;; Core palette
    (:background  . "#0A0A0A")
    (:surface     . "#1A1A1A")
    (:surface-alt . "#2A2A2A")
    (:text        . "#F0F4F8")
    (:muted       . "#9EB3C8")

    ;; Accents
    (:primary   . "#00FF41")    ; Terminal green
    (:secondary . "#FF006E")    ; Hot pink
    (:accent    . "#FFB000")    ; Amber

    ;; Semantic
    (:success . "#00FF41")
    (:warning . "#FFB000")
    (:error   . "#FF3333")
    (:info    . "#00BFFF"))
  "Default color tokens. Apps can override *colors* with custom values.")

(defparameter *default-typography*
  '(;; Font family
    (:family . "\"JetBrains Mono\", monospace")

    ;; Type scale (responsive clamp values)
    (:massive . "clamp(4rem, 15vw, 12rem)")
    (:huge    . "clamp(2rem, 5vw, 3.5rem)")
    (:large   . "clamp(1.1rem, 2.5vw, 1.4rem)")
    (:base    . "1rem")
    (:small   . "0.875rem")
    (:tiny    . "0.75rem")

    ;; Weights
    (:light    . "300")
    (:regular  . "400")
    (:medium   . "500")
    (:semibold . "600")
    (:bold     . "700")

    ;; Line heights
    (:leading-tight  . "0.9")
    (:leading-snug   . "1.15")
    (:leading-normal . "1.5")

    ;; Letter spacing
    (:tracking-tight  . "-0.05em")
    (:tracking-normal . "0"))
  "Default typography tokens.")

(defparameter *default-spacing*
  '((:0  . "0")
    (:1  . "0.25rem")    ; 4px
    (:2  . "0.5rem")     ; 8px
    (:3  . "0.75rem")    ; 12px
    (:4  . "1rem")       ; 16px
    (:5  . "1.25rem")    ; 20px
    (:6  . "1.5rem")     ; 24px
    (:8  . "2rem")       ; 32px
    (:10 . "2.5rem")     ; 40px
    (:12 . "3rem")       ; 48px
    (:16 . "4rem")       ; 64px
    (:20 . "5rem")       ; 80px
    (:24 . "6rem"))      ; 96px
  "Default spacing tokens using 8px base scale.")

(defparameter *default-effects*
  '(;; Transitions
    (:transition-fast . "0.15s ease")
    (:transition-base . "0.2s ease-out")
    (:transition-slow . "0.3s ease")

    ;; Shadows
    (:shadow-sm . "0 1px 2px rgba(0,0,0,0.1)")
    (:shadow-md . "0 4px 6px rgba(0,0,0,0.1)")
    (:shadow-lg . "0 10px 15px rgba(0,0,0,0.1)")
    (:shadow-brutal . "4px 4px 0px")

    ;; Blur
    (:blur-sm . "4px")
    (:blur-md . "8px")
    (:blur-lg . "16px")

    ;; Borders
    (:border-thin   . "1px")
    (:border-medium . "2px")
    (:border-thick  . "4px")
    (:border-brutal . "8px")

    ;; Z-index scale
    (:z-base    . "0")
    (:z-content . "10")
    (:z-nav     . "50")
    (:z-modal   . "100")
    (:z-toast   . "200"))
  "Default effects tokens (transitions, shadows, etc.).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Active Token Sets (Apps Override These)
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *colors* (copy-alist *default-colors*)
  "Active color tokens. Override per-application or rebind per-request.")

(defparameter *typography* (copy-alist *default-typography*)
  "Active typography tokens.")

(defparameter *spacing* (copy-alist *default-spacing*)
  "Active spacing tokens.")

(defparameter *effects* (copy-alist *default-effects*)
  "Active effects tokens.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Accessor Functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun get-color (key)
  "Get color value by keyword with validation.
   (get-color :primary) => \"#00FF41\"
   (get-color :primry)  => Error: Did you mean :primary?"
  (validate-token key *colors* "color")
  (cdr (assoc key *colors*)))

(defun get-font (key)
  "Get typography value by keyword with validation.
   (get-font :massive) => \"clamp(4rem, 15vw, 12rem)\"
   (get-font :massiv)  => Error: Did you mean :massive?"
  (validate-token key *typography* "font")
  (cdr (assoc key *typography*)))

(defun get-spacing (key)
  "Get spacing value by keyword with validation.
   (get-spacing :8) => \"2rem\"
   (get-spacing :7) => Error: Did you mean :8?"
  (validate-token key *spacing* "spacing")
  (cdr (assoc key *spacing*)))

(defun get-effect (key)
  "Get effect value by keyword with validation.
   (get-effect :blur-lg) => \"16px\"
   (get-effect :blur)    => Error: Did you mean :blur-sm?"
  (validate-token key *effects* "effect")
  (cdr (assoc key *effects*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Variable Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun generate-css-variables (&key (colors *colors*)
                                    (typography *typography*)
                                    (spacing *spacing*)
                                    (effects *effects*))
  "Generate :root CSS variables from token sets.
   Returns a CSS string for embedding in <style> tags."
  (with-output-to-string (out)
    (format out ":root {~%")
    ;; Colors
    (iter (for (key . value) in colors)
      (format out "  --color-~A: ~A;~%"
              (string-downcase (symbol-name key)) value))
    ;; Typography
    (iter (for (key . value) in typography)
      (format out "  --font-~A: ~A;~%"
              (string-downcase (symbol-name key)) value))
    ;; Spacing
    (iter (for (key . value) in spacing)
      (format out "  --space-~A: ~A;~%"
              (string-downcase (symbol-name key)) value))
    ;; Effects
    (iter (for (key . value) in effects)
      (format out "  --effect-~A: ~A;~%"
              (string-downcase (symbol-name key)) value))
    (format out "}~%")))
