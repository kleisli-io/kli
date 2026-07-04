(in-package #:kli/tui/input)

(defun printable-character-p (char)
  (let ((code (char-code char)))
    (and (>= code 32) (/= code 127))))

(defun normalize-insertable-text (string)
  (with-output-to-string (out)
    (loop with index = 0
          while (< index (length string))
          for char = (char string index)
          do (cond
               ((char= char #\Return)
                (write-char #\Newline out)
                (when (and (< (1+ index) (length string))
                           (char= (char string (1+ index)) #\Newline))
                  (incf index)))
               ((char= char #\Newline)
                (write-char #\Newline out))
               ((char= char #\Tab)
                (write-string "    " out))
               ((printable-character-p char)
                (write-char char out)))
             (incf index))))

(defun insertable-input-string (string)
  (let ((text (normalize-insertable-text string)))
    (when (plusp (length text))
      text)))

(defun submit-input-p (data)
  (or (string= data "enter")
      (string= data (string #\Return))))

(defun newline-input-p (data)
  (or (string= data "newline")
      (string= data (string #\Newline))))

(defun backspace-input-p (data)
  (or (string= data "backspace")
      (string= data (string #\Backspace))
      (string= data (string #\Rubout))))
