(in-package #:kli/ext)

;;;; Tool-presentation ADT.
;;;;
;;;; A tool result has two projections the renderer must not conflate: the
;;;; model-facing content/details, and the human-facing view. The view is
;;;; tool-owned. A tool declares how its call and result should render via a
;;;; presenter on its `renderer` slot; the presenter is run at the execution
;;;; boundary and yields a serializable TERM. The TUI -- and any headless
;;;; formatter -- is a pure interpreter over the term, dispatching on a declared
;;;; kind rather than sniffing result structure.
;;;;
;;;; A term is a tagged plist whose leaves are keyword/string/integer/nil/t, so
;;;; it persists in the session log under the same discipline as a result's
;;;; details. Terms carry only the kind tag and small discriminating fields; the
;;;; bulk payload (diff bodies, search listings, generic text) is already on the
;;;; transcript event as its text/details and is read from there by kind.

(defparameter *call-header-scalar-limit* 160
  "Longest single-line string argument shown in a call header. Longer or
   multi-line values are content/patch blobs and are dropped from the header.")

(defparameter *call-argument-preview-line-limit* 8
  "Number of omitted argument lines retained in the expanded tool-call preview.")

(defparameter *call-argument-preview-char-limit* 2000
  "Maximum characters retained across an expanded tool-call argument preview.")

;;; Term constructors.

(defun call-hidden () (list :kind :hidden))
(defun call-command (text) (list :kind :command :text (or text "")))
(defun call-header (text) (list :kind :header :text (or text "")))

(defun result-summary (&key path lines start end raw truncated)
  (list :kind :summary :path path :lines lines :start start :end end
        :raw (and raw t) :truncated (and truncated t)))
(defun result-diff (&key note updates)
  (append (list :kind :diff)
          (when note (list :note note))
          (when updates (list :updates updates))))
(defun result-listing () (list :kind :listing))
(defun result-filesystem-summary (summary-kind &rest fields)
  (list* :kind :filesystem-summary :summary-kind summary-kind fields))

(defun result-box () (list :kind :box))

(defun presentation-kind (term)
  "The tag of a presentation TERM, or NIL when TERM carries none -- e.g. an
   old-session record stored before presentations existed, which degrades to a
   generic box."
  (and (consp term) (getf term :kind)))

;;; Argument access and scalar rendering for the default call header.

(defun tool-argument-ref (arguments key)
  "Read KEY from ARGUMENTS, a parsed-JSON hash-table (string keys) or a plist
   (keyword keys)."
  (cond ((hash-table-p arguments)
         (or (gethash (string-downcase (string key)) arguments)
             (gethash (string key) arguments)))
        ((consp arguments) (getf arguments key))))

(defun tool-argument-value-string (value)
  "Compact one-line rendering of a single argument VALUE."
  (substitute #\Space #\Newline
              (typecase value (string value) (t (princ-to-string value)))))

(defun tool-argument-scalar-p (value)
  "A VALUE worth showing in a call header: a number, boolean, keyword, or a
   short single-line string. Content and patch blobs -- long or multi-line
   strings -- are excluded so the header never inlines a file body."
  (or (numberp value) (eq value t) (eq value nil) (keywordp value)
      (and (stringp value)
           (not (find #\Newline value))
           (<= (length value) *call-header-scalar-limit*))))

(defun tool-argument-pairs (arguments)
  "Sorted (key-string . value) pairs from ARGUMENTS, a parsed-JSON hash-table
   or a plist."
  (let ((entries '()))
    (cond
      ((hash-table-p arguments)
       (maphash (lambda (k v) (push (cons (princ-to-string k) v) entries))
                arguments))
      ((consp arguments)
       (loop for (k v) on arguments by #'cddr
             do (push (cons (string-downcase (string k)) v) entries))))
    (sort entries #'string< :key #'car)))

(defun present-default-call-text (arguments)
  "Header text for ARGUMENTS: scalar arguments only. A single scalar renders as
   its value alone; several render as space-separated key=value."
  (let ((entries (remove-if-not #'tool-argument-scalar-p
                                (tool-argument-pairs arguments)
                                :key #'cdr)))
    (cond
      ((null entries) "")
      ((null (cdr entries)) (tool-argument-value-string (cdr (first entries))))
      (t (format nil "~{~A~^ ~}"
                 (mapcar (lambda (e)
                           (format nil "~A=~A" (car e)
                                   (tool-argument-value-string (cdr e))))
                         entries))))))

(defun preview-lines (text limit)
  (let ((lines '())
        (truncated nil))
    (with-input-from-string (in (or text ""))
      (loop for line = (read-line in nil nil)
            while line
            for i from 0
            do (if (< i limit)
                   (push line lines)
                   (setf truncated t))))
    (values (nreverse lines) truncated)))

(defun truncate-preview-chars (text limit)
  (if (<= (length text) limit)
      (values text nil)
      (values (subseq text 0 limit) t)))

(defun bounded-preview-text (text)
  (multiple-value-bind (char-text char-truncated)
      (truncate-preview-chars (or text "") *call-argument-preview-char-limit*)
    (multiple-value-bind (lines line-truncated)
        (preview-lines char-text *call-argument-preview-line-limit*)
      (let ((body (format nil "~{~A~^~%~}" lines)))
        (if (or char-truncated line-truncated)
            (format nil "~A~%… (truncated · Ctrl+O)" body)
            body)))))

(defun tool-argument-preview-string (value)
  (typecase value (string value) (t (princ-to-string value))))

(defun present-default-call-preview (arguments)
  "Bounded expanded preview for non-scalar arguments omitted from the header."
  (let ((entries (remove-if #'tool-argument-scalar-p
                            (tool-argument-pairs arguments)
                            :key #'cdr)))
    (when entries
      (format nil "~{~A~^~%~}"
              (mapcar (lambda (entry)
                        (format nil "~A:~%~A" (car entry)
                                (bounded-preview-text
                                 (tool-argument-preview-string (cdr entry)))))
                      entries)))))

;;; Presenter wiring. The tool `renderer` slot carries a presenter plist
;;; (:present-call fn :present-result fn); either may be omitted and falls back
;;; to the default. Presenters receive the tool plus the call arguments or the
;;; result, and return a term.

(defun make-tool-presenter (&key present-call present-result)
  (list :present-call present-call :present-result present-result))

(defun default-call-term (arguments)
  (let ((text (present-default-call-text arguments))
        (preview (present-default-call-preview arguments)))
    (if (and preview (plusp (length preview)))
        (append (call-header text) (list :preview preview))
        (call-header text))))

(defun present-call (tool arguments)
  "Call-term for TOOL invoked with ARGUMENTS. Defaults to a scalar-args header."
  (let ((fn (and (consp (tool-renderer tool))
                 (getf (tool-renderer tool) :present-call))))
    (or (and fn (funcall fn tool arguments))
        (default-call-term arguments))))

(defun present-result (tool result)
  "Result-term for TOOL's RESULT. Defaults to a generic text box."
  (let ((fn (and (consp (tool-renderer tool))
                 (getf (tool-renderer tool) :present-result))))
    (or (and fn (funcall fn tool result))
        (result-box))))

;;; Reusable presenters declared by the built-in tools.

(defun hidden-call-presenter (tool arguments)
  (declare (ignore tool arguments))
  (call-hidden))

(defun command-call-presenter (tool arguments)
  "Render the call as a `$ cmd` line from the :command argument."
  (declare (ignore tool))
  (call-command (tool-argument-value-string
                 (or (tool-argument-ref arguments :command) ""))))

(defun summary-result-presenter (tool result)
  "One-line summary built from a read-shaped result's details. The result text
   stays full for the model; the view shows only the summary."
  (declare (ignore tool))
  (let ((d (tool-result-details result)))
    (result-summary :path (getf d :path) :lines (getf d :lines)
                    :start (getf d :start) :end (getf d :end)
                    :raw (getf d :raw) :truncated (getf d :truncated))))

(defun result-mentions-repair-p (result)
  (some (lambda (item)
          (let ((text (getf item :text)))
            (and (stringp text) (search "paren-repair:" text))))
        (tool-result-content result)))

(defun diff-result-presenter (tool result)
  "File-update diff, noting a paren-repair when the runner appended one."
  (declare (ignore tool))
  (let ((term (tool-result-presentation result))
        (note (and (result-mentions-repair-p result)
                   "paren-repair: balanced delimiters")))
    (cond ((and term note (not (getf term :note)))
           (append term (list :note note)))
          (term term)
          (t (result-diff :note note)))))

(defun listing-result-presenter (tool result)
  (declare (ignore tool result))
  (result-listing))

(defun find-summary-result-presenter (tool result)
  "Compact human view for find; model-visible result text/details stay intact."
  (declare (ignore tool))
  (if (tool-result-error-p result)
      (result-box)
      (let ((d (tool-result-details result)))
        (result-filesystem-summary :find
                                   :pattern (getf d :pattern)
                                   :count (getf d :count)
                                   :result-handle (getf d :result-handle)
                                   :truncated (getf d :truncated)))))

(defun search-summary-result-presenter (tool result)
  "Compact human view for search; model-visible result text/details stay intact."
  (declare (ignore tool))
  (if (tool-result-error-p result)
      (result-box)
      (let ((d (tool-result-details result)))
        (result-filesystem-summary :search
                                   :pattern (getf d :pattern)
                                   :path (getf d :path)
                                   :matches (getf d :matches)
                                   :file-count (getf d :file-count)
                                   :skipped (getf d :skipped)
                                   :timed-out (getf d :timed-out)
                                   :result-handle (getf d :result-handle)
                                   :truncated (getf d :truncated)))))
