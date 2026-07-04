(in-package #:kli/tui/commands)

(defun slash-command-input-p (input)
  (and (stringp input)
       (plusp (length input))
       (char= #\/ (char input 0))))

(defun command-name-keyword (name)
  (intern (string-upcase name) :keyword))

(defun split-command-tail (tail)
  (let ((words '())
        (start nil))
    (labels ((finish-word (end)
               (when start
                 (push (subseq tail start end) words)
                 (setf start nil))))
      (loop for index from 0 below (length tail)
            for char = (char tail index)
            do (if (whitespace-char-p char)
                   (finish-word index)
                   (unless start
                     (setf start index))))
      (finish-word (length tail)))
    (nreverse words)))

(defun parse-slash-command (input)
  (unless (slash-command-input-p input)
    (return-from parse-slash-command nil))
  (let* ((body (trim-whitespace (subseq input 1)))
         (space (position-if #'whitespace-char-p body))
         (name-text (if space
                        (subseq body 0 space)
                        body))
         (tail (if space
                   (trim-whitespace (subseq body space))
                   "")))
    (list :raw input
          :name (unless (zerop (length name-text))
                  (command-name-keyword name-text))
          :name-text name-text
          :tail tail
          :words (split-command-tail tail))))
