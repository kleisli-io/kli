(in-package #:kli/skills)

(defun sigil-boundary-char-p (char)
  (not (or (alphanumericp char) (eql char #\-))))

(defun sigil-site-p (text position)
  "True when the dollar at POSITION can open a skill reference."
  (or (zerop position)
      (let ((before (char text (1- position))))
        (not (or (alphanumericp before) (eql before #\$))))))

(defun sigil-match-at (text start skills)
  "The skill whose name reads from START, the longest discovered name
winning. The match must end where no skill name could continue."
  (let ((best nil))
    (dolist (skill skills best)
      (let* ((name (skill-name skill))
             (end (+ start (length name))))
        (when (and (<= end (length text))
                   (string= name text :start2 start :end2 end)
                   (or (= end (length text))
                       (sigil-boundary-char-p (char text end)))
                   (or (null best)
                       (> (length name) (length (skill-name best)))))
          (setf best skill))))))

(defun find-sigil-skills (text skills)
  "Skills referenced by dollar sigils in TEXT, distinct, in order of first
appearance. Names not matching a discovered skill stay prose."
  (let ((found '()))
    (loop for position = (position #\$ text)
            then (position #\$ text :start (1+ position))
          while position
          when (sigil-site-p text position)
            do (let ((skill (sigil-match-at text (1+ position) skills)))
                 (when (and skill (not (member skill found)))
                   (push skill found))))
    (nreverse found)))

(defun sigil-reference-line (skill)
  (format nil "Referenced as $~A in the prompt that follows."
          (skill-name skill)))

(defun expand-skill-sigils (text skills)
  "TEXT with one skill block prepended per referenced skill, the prose
itself untouched. Text without references passes through unchanged and an
unreadable skill file drops only its own block."
  (let ((blocks (loop for skill in (find-sigil-skills text skills)
                      for block = (ignore-errors
                                   (skill-block
                                    skill
                                    :reference (sigil-reference-line skill)))
                      when block collect block)))
    (if (null blocks)
        text
        (format nil "~{~A~%~%~}~A" blocks text))))
