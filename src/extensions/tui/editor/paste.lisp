(in-package #:kli/tui/editor)

(defun replace-substrings (string needle replacement)
  (if (zerop (length needle))
      string
      (with-output-to-string (out)
        (loop with start = 0
              for position = (search needle string
                                     :start2 start
                                     :test #'char=)
              while position
              do (progn
                   (write-string string out
                                 :start start
                                 :end position)
                   (write-string replacement out)
                   (setf start (+ position (length needle))))
              finally (write-string string out
                                    :start start)))))

(defun paste-line-count (text)
  (1+ (count #\Newline text)))

(defun large-paste-p (text)
  (or (> (paste-line-count text) 10)
      (> (length text) 1000)))

(defun paste-marker-for (id text)
  (let ((line-count (paste-line-count text)))
    (if (> line-count 10)
        (format nil "[paste #~D +~D lines]" id line-count)
        (format nil "[paste #~D ~D chars]" id (length text)))))

(defun store-paste-block (editor text)
  (incf (editor-paste-counter editor))
  (let* ((number (editor-paste-counter editor))
         (marker (paste-marker-for number text))
         (block (make-paste-block :number number
                                  :marker marker
                                  :text text)))
    (setf (editor-paste-blocks editor)
          (append (editor-paste-blocks editor) (list block)))
    marker))

(defun editor-expanded-value (editor)
  (reduce (lambda (result block)
            (replace-substrings result
                                (paste-block-marker block)
                                (paste-block-text block)))
          (editor-paste-blocks editor)
          :initial-value (editor-value editor)))

(defun editor-paste-marker-ranges (editor)
  (loop for block in (editor-paste-blocks editor)
        for marker = (paste-block-marker block)
        append (loop with start = 0
                     for position = (search marker
                                            (editor-value editor)
                                            :start2 start
                                            :test #'char=)
                     while position
                     collect (cons position (+ position (length marker)))
                     do (setf start (+ position (length marker))))))

(defun paste-marker-starting-at (editor position)
  (find position (editor-paste-marker-ranges editor) :key #'car))

(defun paste-marker-ending-at (editor position)
  (find position (editor-paste-marker-ranges editor) :key #'cdr))
