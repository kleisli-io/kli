(in-package #:kli/tui/input)

(defparameter +bracketed-paste-start+
  (format nil "~C[200~~" #\Esc))

(defparameter +bracketed-paste-end+
  (format nil "~C[201~~" #\Esc))

(defun make-default-input-decoder-behavior (decoder-id)
  (make-behavior-cell
   :id (list decoder-id :input-decoder-behavior)
   :state '(:strategy :default-input-decoder)
   :metadata '(:owner :tui-input)
   :capabilities '(:input/decoder :behavior/hotpatch :behavior/state)
   :fault-policy :continue
   :function #'default-input-decoder-feed))

(defun make-input-decoder (&key id protocol behavior)
  (let ((id (or id (make-input-decoder-id))))
    (make-instance 'input-decoder
                   :id id
                   :protocol protocol
                   :behavior (or behavior
                                 (make-default-input-decoder-behavior id)))))

(defun decode-decoder-input (decoder data)
  (call-behavior (input-decoder-behavior decoder) decoder data))

(defun recode-input-decoder (decoder
                             &key function version (state nil state-p)
                               (metadata nil metadata-p)
                               (capabilities nil capabilities-p))
  (apply #'recode-tui-behavior
         (input-decoder-behavior decoder)
         (append (when function
                   (list :function function))
                 (when version
                   (list :version version))
                 (when state-p
                   (list :state state))
                 (when metadata-p
                   (list :metadata metadata))
                 (when capabilities-p
                   (list :capabilities capabilities))))
  decoder)

(defun input-decoder-feed (decoder data)
  (decode-decoder-input decoder data))

(defun make-paste-buffer (&optional (capacity 0))
  (make-array capacity :element-type 'character :adjustable t :fill-pointer 0))

(defun ensure-paste-buffer (decoder)
  "The paste buffer as an adjustable fill-pointer string, upgrading the \"\"
   initform on first use while preserving any content."
  (let ((buffer (input-decoder-paste-buffer decoder)))
    (if (and (adjustable-array-p buffer) (array-has-fill-pointer-p buffer))
        buffer
        (let ((upgraded (make-paste-buffer (length buffer))))
          (loop for char across buffer do (vector-push-extend char upgraded))
          (setf (input-decoder-paste-buffer decoder) upgraded)
          upgraded))))

(defun input-decoder-clear (decoder)
  (setf (input-decoder-buffer decoder) ""
        (input-decoder-paste-mode decoder) nil
        (input-decoder-paste-buffer decoder) (make-paste-buffer))
  nil)

(defun input-decoder-flush (decoder)
  (cond
    ((input-decoder-paste-mode decoder)
     (let ((text (coerce (input-decoder-paste-buffer decoder) 'simple-string)))
       (input-decoder-clear decoder)
       (if (plusp (length text))
           (list (make-paste-input-event text))
           '())))
    ((plusp (length (input-decoder-buffer decoder)))
     (let ((events (decode-input-sequence (object-protocol decoder)
                                          (input-decoder-buffer decoder))))
       (setf (input-decoder-buffer decoder) "")
       events))
    (t
     '())))

(defun reset-paste-state (decoder)
  (setf (input-decoder-paste-mode decoder) nil
        (input-decoder-paste-buffer decoder) (make-paste-buffer)))

(defun feed-paste-data (decoder data events)
  "Append DATA to the paste buffer in one pass and locate the end marker with a
   single bounded SEARCH. The (end-length - 1) overlap catches a marker split
   across the DATA boundary; on a hit, emit the paste (marker stripped) and
   re-feed the trailing remainder."
  (let* ((buffer (ensure-paste-buffer decoder))
         (end-length (length +bracketed-paste-end+))
         (search-start (max 0 (- (fill-pointer buffer) (1- end-length)))))
    (loop for char across data do (vector-push-extend char buffer))
    (let ((position (search +bracketed-paste-end+ buffer
                            :start2 search-start :test #'char=)))
      (if position
          (let ((pasted (coerce (subseq buffer 0 position) 'simple-string))
                (remainder (coerce (subseq buffer (+ position end-length))
                                   'simple-string)))
            (reset-paste-state decoder)
            (append events
                    (list (make-paste-input-event pasted))
                    (default-input-decoder-feed decoder remainder)))
          events))))

(defun decode-sequences (protocol sequences)
  (loop for sequence in sequences
        append (decode-input-sequence protocol sequence)))

(defun default-input-decoder-feed (decoder data)
  (let ((data (input-data-string data))
        (protocol (object-protocol decoder)))
    (if (input-decoder-paste-mode decoder)
        (feed-paste-data decoder data '())
        (progn
          (setf (input-decoder-buffer decoder)
                (concatenate 'string (input-decoder-buffer decoder) data))
          (let ((start-index (search +bracketed-paste-start+
                                     (input-decoder-buffer decoder)
                                     :test #'char=)))
            (if start-index
                (let* ((buffer (input-decoder-buffer decoder))
                       (before (subseq buffer 0 start-index))
                       (after (subseq buffer
                                      (+ start-index
                                         (length +bracketed-paste-start+)))))
                  (multiple-value-bind (sequences remainder)
                      (extract-complete-input-sequences protocol before)
                    (setf (input-decoder-buffer decoder) ""
                          (input-decoder-paste-mode decoder) t)
                    (feed-paste-data decoder
                                     (concatenate 'string remainder after)
                                     (decode-sequences protocol sequences))))
                (multiple-value-bind (sequences remainder)
                    (extract-complete-input-sequences
                     protocol
                     (input-decoder-buffer decoder))
                  (setf (input-decoder-buffer decoder) remainder)
                  (decode-sequences protocol sequences))))))))

(defun input-data-string (data)
  (etypecase data
    (string data)
    (character (string data))))

(defun decode-input-sequence (protocol sequence)
  (let ((sequence (input-data-string sequence)))
    (cond
      ((zerop (length sequence))
       '())
      ((terminal-response-sequence-p sequence)
       (list (make-terminal-response-input-event sequence)))
      ((parse-terminal-key protocol sequence)
       ;; ctrl+c is the interrupt key in every encoding: the legacy 0x03 byte
       ;; and the CSI u form the kitty keyboard protocol re-encodes it to.
       (let ((key-id (parse-terminal-key protocol sequence)))
         (list (if (string= key-id "ctrl+c")
                   (make-interrupt-input-event :raw sequence)
                   (event-from-key-id key-id sequence)))))
      ((printable-string-p sequence)
       (list (make-text-input-event sequence :raw sequence)))
      (t
       '()))))

(defun terminal-response-sequence-p (sequence)
  (and (plusp (length sequence))
       (char= #\Esc (char sequence 0))
       (or (and (> (length sequence) 1)
                (member (char sequence 1) '(#\] #\P #\_) :test #'char=))
           (kitty-keyboard-response-p sequence))))

(defun kitty-keyboard-response-p (sequence)
  (and (>= (length sequence) 5)
       (char= #\Esc (char sequence 0))
       (char= #\[ (char sequence 1))
       (char= #\? (char sequence 2))
       (char= #\u (char sequence (1- (length sequence))))
       (loop for index from 3 below (1- (length sequence))
             always (digit-char-p (char sequence index)))))

(defun extract-complete-input-sequences (protocol buffer)
  "Split BUFFER into complete input sequences, returning them plus an
   incomplete trailing escape sequence as the remainder. Indexes into BUFFER
   rather than re-slicing the unconsumed tail each step, and coalesces a maximal
   run of printable characters into one sequence so an unbracketed paste decodes
   to a single text event -- and a single INSERT-TEXT -- instead of one per
   character (which would rebuild the buffer and snapshot undo N times, O(n^2))."
  (let ((length (length buffer))
        (sequences '())
        (position 0))
    (loop while (< position length)
          do (cond
               ((char= #\Esc (char buffer position))
                (let ((matched nil))
                  (loop for end from (1+ position) to length
                        for candidate = (subseq buffer position end)
                        for status = (input-sequence-status protocol candidate)
                        do (case status
                             ((:complete :not-escape)
                              (push candidate sequences)
                              (setf position end matched t)
                              (return))))
                  (unless matched
                    (return-from extract-complete-input-sequences
                      (values (nreverse sequences) (subseq buffer position))))))
               ((printable-character-p (char buffer position))
                (let ((run-start position))
                  (loop while (and (< position length)
                                   (printable-character-p (char buffer position)))
                        do (incf position))
                  (push (subseq buffer run-start position) sequences)))
               (t
                (push (subseq buffer position (1+ position)) sequences)
                (incf position))))
    (values (nreverse sequences) "")))

(defun input-escape-sequence-start-p (string)
  (and (plusp (length string))
       (char= #\Esc (char string 0))))

(defun input-sequence-status (protocol sequence)
  (cond
    ((not (input-escape-sequence-start-p sequence))
     :not-escape)
    ((= 1 (length sequence))
     :incomplete)
    ((char= #\[ (char sequence 1))
     (csi-sequence-status sequence))
    ((char= #\] (char sequence 1))
     (string-control-sequence-status protocol sequence))
    ((char= #\P (char sequence 1))
     (string-control-sequence-status protocol sequence))
    ((char= #\_ (char sequence 1))
     (string-control-sequence-status protocol sequence))
    ((char= #\O (char sequence 1))
     (if (>= (length sequence) 3) :complete :incomplete))
    ((= 2 (length sequence))
     :complete)
    (t
     :complete)))

(defun csi-sequence-status (sequence)
  (if (< (length sequence) 3)
      :incomplete
      (let* ((payload (subseq sequence 2))
             (last (char payload (1- (length payload))))
             (code (char-code last)))
        (if (and (>= code #x40) (<= code #x7e))
            :complete
            :incomplete))))

(defun string-control-sequence-status (protocol sequence)
  (declare (ignore protocol))
  (if (or (string-suffix-p (format nil "~C\\" #\Esc) sequence)
          (char= #\Bel (char sequence (1- (length sequence)))))
      :complete
      :incomplete))

(defun parse-terminal-key (protocol sequence)
  (or (parse-modified-csi-key protocol sequence)
      (parse-csi-u-key protocol sequence)
      (lookup-terminal-key sequence)
      (parse-alt-key sequence)
      (parse-control-key sequence)))

(defun lookup-terminal-key (sequence)
  (cdr (assoc sequence
              (list
               (cons (string #\Esc) "escape")
               (cons (string #\Tab) "tab")
               (cons (string #\Return) "enter")
               (cons (string #\Rubout) "backspace")
               (cons (string #\Backspace) "backspace")
               (cons (format nil "~C[A" #\Esc) "up")
               (cons (format nil "~C[B" #\Esc) "down")
               (cons (format nil "~C[C" #\Esc) "right")
               (cons (format nil "~C[D" #\Esc) "left")
               (cons (format nil "~COA" #\Esc) "up")
               (cons (format nil "~COB" #\Esc) "down")
               (cons (format nil "~COC" #\Esc) "right")
               (cons (format nil "~COD" #\Esc) "left")
               (cons (format nil "~C[H" #\Esc) "home")
               (cons (format nil "~C[F" #\Esc) "end")
               (cons (format nil "~COH" #\Esc) "home")
               (cons (format nil "~COF" #\Esc) "end")
               (cons (format nil "~C[Z" #\Esc) "shift+tab")
               (cons (format nil "~C[2~~" #\Esc) "insert")
               (cons (format nil "~C[3~~" #\Esc) "delete")
               (cons (format nil "~C[5~~" #\Esc) "page-up")
               (cons (format nil "~C[6~~" #\Esc) "page-down"))
              :test #'string=)))

(defun parse-control-key (sequence)
  (when (= 1 (length sequence))
    (let ((code (char-code (char sequence 0))))
      (cond
        ((and (>= code 1) (<= code 26))
         (format nil "ctrl+~C" (code-char (+ code 96))))
        ((= code 28) "ctrl+\\")
        ((= code 29) "ctrl+]")
        ((= code 31) "ctrl+-")))))

(defun parse-alt-key (sequence)
  (when (and (= 2 (length sequence))
             (char= #\Esc (char sequence 0)))
    (let ((char (char sequence 1)))
      (cond
        ((or (char= char #\Rubout)
             (char= char #\Backspace))
         "alt+backspace")
        ((printable-character-p char)
         (format nil "alt+~A" (string-downcase (string char))))))))

(defun parse-modified-csi-key (protocol sequence)
  (declare (ignore protocol))
  (when (and (>= (length sequence) 6)
             (char= #\Esc (char sequence 0))
             (char= #\[ (char sequence 1))
             (char= #\1 (char sequence 2))
             (char= #\; (char sequence 3)))
    (let* ((final (char sequence (1- (length sequence))))
           (base (cdr (assoc final
                             '((#\A . "up")
                               (#\B . "down")
                               (#\C . "right")
                               (#\D . "left")
                               (#\H . "home")
                               (#\F . "end"))
                             :test #'char=)))
           (modifier-text (subseq sequence 4 (1- (length sequence))))
           (event-separator (position #\: modifier-text))
           (modifier-value (parse-positive-integer
                            (if event-separator
                                (subseq modifier-text 0 event-separator)
                                modifier-text))))
      (when (and base modifier-value)
        (key-id-with-modifiers base
                               (modifier-mask-to-list
                                (1- modifier-value)))))))

(defun parse-csi-u-key (protocol sequence)
  (declare (ignore protocol))
  (when (and (>= (length sequence) 4)
             (char= #\Esc (char sequence 0))
             (char= #\[ (char sequence 1))
             (char= #\u (char sequence (1- (length sequence)))))
    (let* ((payload (subseq sequence 2 (1- (length sequence))))
           (semicolon (position #\; payload))
           (codepoint-text (if semicolon
                               (subseq payload 0 semicolon)
                               payload))
           (colon (position #\: codepoint-text))
           (codepoint (parse-positive-integer
                       (if colon
                           (subseq codepoint-text 0 colon)
                           codepoint-text)))
           (modifier-text (and semicolon
                               (subseq payload (1+ semicolon))))
           (event-separator (and modifier-text
                                 (position #\: modifier-text)))
           (modifier-value (if modifier-text
                               (parse-positive-integer
                                (if event-separator
                                    (subseq modifier-text 0 event-separator)
                                    modifier-text))
                               1))
           (base (and codepoint (codepoint-key-name codepoint))))
      (when base
        (key-id-with-modifiers base
                               (modifier-mask-to-list
                                (1- (or modifier-value 1))))))))

(defun codepoint-key-name (codepoint)
  (case codepoint
    (9 "tab")
    (13 "enter")
    (27 "escape")
    (32 "space")
    (127 "backspace")
    (otherwise
     (when (or (and (>= codepoint 97) (<= codepoint 122))
               (member (code-char codepoint)
                       '(#\- #\= #\[ #\] #\\ #\; #\' #\, #\. #\/)
                       :test #'char=))
       (string (code-char codepoint))))))

(defun modifier-mask-to-list (mask)
  (let ((modifiers '()))
    (when (logtest mask 1)
      (push :shift modifiers))
    (when (logtest mask 2)
      (push :alt modifiers))
    (when (logtest mask 4)
      (push :ctrl modifiers))
    (nreverse modifiers)))

(defun key-id-with-modifiers (base modifiers)
  (if modifiers
      (format nil "~{~A~^+~}+~A"
              (mapcar (lambda (modifier)
                        (string-downcase (symbol-name modifier)))
                      modifiers)
              base)
      base))

(defun event-from-key-id (key-id raw)
  (multiple-value-bind (base modifiers)
      (parse-key-id key-id)
    (make-key-input-event (key-name-keyword base)
                          :raw raw
                          :key-id key-id
                          :modifiers modifiers)))

(defun parse-key-id (key-id)
  (let* ((parts (split-on-character key-id #\+))
         (base (car (last parts)))
         (modifiers (loop for part in (butlast parts)
                          collect (intern (string-upcase part) :keyword))))
    (values base modifiers)))

(defun key-name-keyword (name)
  (or (cdr (assoc name
                  '(("escape" . :escape)
                    ("enter" . :enter)
                    ("tab" . :tab)
                    ("space" . :space)
                    ("backspace" . :backspace)
                    ("delete" . :delete)
                    ("insert" . :insert)
                    ("home" . :home)
                    ("end" . :end)
                    ("page-up" . :page-up)
                    ("page-down" . :page-down)
                    ("up" . :up)
                    ("down" . :down)
                    ("left" . :left)
                    ("right" . :right))
                  :test #'string=))
      (intern (string-upcase name) :keyword)))

(defun split-on-character (string delimiter)
  (let ((parts '())
        (start 0))
    (loop for index from 0 below (length string)
          when (char= delimiter (char string index))
            do (progn
                 (push (subseq string start index) parts)
                 (setf start (1+ index))))
    (push (subseq string start) parts)
    (nreverse parts)))
