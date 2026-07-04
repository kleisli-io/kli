(in-package #:kli/app)

(defun terminal-input-tty-p ()
  "True when fd 0 is a terminal whose attributes can be read. A dispatch-level
copy of the predicate so command routing decides interactive-vs-piped without
depending on the terminal-UI module."
  #+sbcl (handler-case (and (sb-posix:tcgetattr 0) t)
           (error () nil))
  #-sbcl nil)
