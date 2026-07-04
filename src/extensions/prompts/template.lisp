(in-package #:kli/prompts)

(defun normalize-newlines (text)
  (substitute #\Newline #\Return
              (cl-ppcre:regex-replace-all
               (coerce '(#\Return #\Newline) 'string)
               text
               (string #\Newline))))

(defun strip-value-quotes (value)
  (let ((length (length value)))
    (if (and (>= length 2)
             (member (char value 0) '(#\" #\'))
             (char= (char value (1- length)) (char value 0)))
        (subseq value 1 (1- length))
        value)))

(defparameter +frontmatter-whitespace+ '(#\Space #\Tab))

(defun parse-frontmatter-block (block)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string block :separator '(#\Newline)) table)
      (let ((colon (position #\: line)))
        (when colon
          (let ((key (string-trim +frontmatter-whitespace+
                                  (subseq line 0 colon)))
                (value (string-trim +frontmatter-whitespace+
                                    (subseq line (1+ colon)))))
            (when (plusp (length key))
              (setf (gethash key table) (strip-value-quotes value)))))))))

(defun parse-frontmatter (text)
  "Split TEXT into a frontmatter table and the body. Frontmatter is the
block between a leading --- line and a closing --- fence, read as flat
key/value pairs with surrounding quotes stripped. Without a complete
fence pair the whole text is body and the table is empty. The body is
trimmed only when a fence pair was found, matching pi."
  (let ((normalized (normalize-newlines text))
        (open (format nil "---~%"))
        (close (format nil "~%---")))
    (if (and (>= (length normalized) 4)
             (string= open normalized :end2 4))
        (let ((end (search close normalized :start2 3)))
          (if end
              (values (parse-frontmatter-block (subseq normalized 4 end))
                      (string-trim '(#\Space #\Tab #\Newline)
                                   (subseq normalized
                                           (min (length normalized)
                                                (+ end (length close))))))
              (values (make-hash-table :test #'equal) normalized)))
        (values (make-hash-table :test #'equal) normalized))))

(defun parse-command-arguments (text)
  "Split TEXT into arguments the way pi does: whitespace separates, single
or double quotes group text into one argument and are dropped, with no
escape handling. Empty arguments never appear."
  (let ((arguments '())
        (current (make-array 0 :element-type 'character
                               :adjustable t :fill-pointer t))
        (quote-char nil))
    (flet ((finish ()
             (when (plusp (length current))
               (push (copy-seq current) arguments)
               (setf (fill-pointer current) 0))))
      (loop for char across text
            do (cond
                 (quote-char
                  (if (char= char quote-char)
                      (setf quote-char nil)
                      (vector-push-extend char current)))
                 ((member char '(#\" #\'))
                  (setf quote-char char))
                 ((member char '(#\Space #\Tab))
                  (finish))
                 (t
                  (vector-push-extend char current))))
      (finish))
    (nreverse arguments)))

(defun join-arguments (arguments)
  (format nil "~{~A~^ ~}" arguments))

(defun positional-argument (arguments digits)
  (let ((index (1- (parse-integer digits))))
    (if (minusp index)
        ""
        (or (nth index arguments) ""))))

(defun argument-slice (arguments start-digits length-digits)
  (let ((tail (nthcdr (max 0 (1- (parse-integer start-digits))) arguments)))
    (join-arguments
     (if length-digits
         (subseq tail 0 (min (parse-integer length-digits) (length tail)))
         tail))))

(defun substitute-arguments (template arguments &key (raw nil raw-p))
  "Expand pi prompt-template placeholders in TEMPLATE. The passes run in
pi's order — positionals, slices, $ARGUMENTS, $@, then $RAW_ARGUMENTS
last — so placeholder-looking content inside the raw text stays literal.
RAW defaults to the parsed arguments joined."
  (let ((all (join-arguments arguments))
        (result template))
    (setf result
          (cl-ppcre:regex-replace-all
           "\\$(\\d+)" result
           (lambda (match digits)
             (declare (ignore match))
             (positional-argument arguments digits))
           :simple-calls t))
    (setf result
          (cl-ppcre:regex-replace-all
           "\\$\\{@:(\\d+)(?::(\\d+))?\\}" result
           (lambda (match start length)
             (declare (ignore match))
             (argument-slice arguments start length))
           :simple-calls t))
    (flet ((replace-literally (pattern value)
             (setf result
                   (cl-ppcre:regex-replace-all
                    pattern result
                    (lambda (match)
                      (declare (ignore match))
                      value)
                    :simple-calls t))))
      (replace-literally "\\$ARGUMENTS" all)
      (replace-literally "\\$@" all)
      (replace-literally "\\$RAW_ARGUMENTS" (if raw-p raw all)))
    result))
