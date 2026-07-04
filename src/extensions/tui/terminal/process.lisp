(in-package #:kli/tui/terminal)

(defun env-var (name)
  #+sbcl (sb-ext:posix-getenv name)
  #-sbcl (progn
           (declare (ignore name))
           nil))

#+sbcl
(sb-alien:define-alien-type terminal-winsize
  (sb-alien:struct terminal-winsize
    (rows sb-alien:unsigned-short)
    (columns sb-alien:unsigned-short)
    (x-pixels sb-alien:unsigned-short)
    (y-pixels sb-alien:unsigned-short)))

(defconstant +tiocgwinsz+ #x5413)

(defun terminal-window-size-from-fd (fd)
  #+sbcl
  (handler-case
      (sb-alien:with-alien ((winsize (sb-alien:struct terminal-winsize)))
        (multiple-value-bind (ok errno)
            (sb-unix:unix-ioctl fd +tiocgwinsz+
                                (sb-alien:alien-sap
                                 (sb-alien:addr winsize)))
          (declare (ignore errno))
          (when ok
            (let ((columns (sb-alien:slot winsize 'columns))
                  (rows (sb-alien:slot winsize 'rows)))
              (when (and (plusp columns) (plusp rows))
                (list columns rows))))))
    (error ()
      nil))
  #-sbcl
  (declare (ignore fd))
  #-sbcl
  nil)

(defun current-terminal-window-size ()
  (let ((size (or (terminal-window-size-from-fd 1)
                  (terminal-window-size-from-fd 0))))
    (if size
        (values (first size) (second size) t)
        (values nil nil nil))))

(defun make-process-terminal (&key protocol id (columns 80) (rows 24))
  (make-instance 'process-terminal
                 :id (or id (make-terminal-id :process))
                 :protocol protocol
                 :columns columns
                 :rows rows))

(defun write-process-terminal (terminal string)
  "Buffer the write while a frame is batching so the whole frame reaches the tty
   as one write, else send it straight through. A multiplexer that ignores
   synchronized output (mode 2026) then sees a frame in a single read and cannot
   redraw the pane part way through the cursor walk."
  (if (plusp (process-terminal-batch-depth terminal))
      (push string (process-terminal-frame-chunks terminal))
      (progn
        (write-string string *standard-output*)
        (finish-output *standard-output*)))
  string)

(defun begin-process-terminal-frame (terminal)
  "Open a batch scope. Scopes nest so an outer scope folds pre-frame writes such
   as a screen clear into the same single write as the frame. Only the outermost
   scope resets the buffer."
  (when (zerop (process-terminal-batch-depth terminal))
    (setf (process-terminal-frame-chunks terminal) nil))
  (incf (process-terminal-batch-depth terminal))
  terminal)

(defun end-process-terminal-frame (terminal)
  "Close a batch scope. The outermost close emits the accumulated writes as one
   write. Drains even a partial frame so a faulted render never wedges batching
   on or strands bytes."
  (when (plusp (process-terminal-batch-depth terminal))
    (when (zerop (decf (process-terminal-batch-depth terminal)))
      (let ((chunks (process-terminal-frame-chunks terminal)))
        (setf (process-terminal-frame-chunks terminal) nil)
        ;; Joined via a stream: apply'ing concatenate puts every chunk in
        ;; the argument list, which call-arguments-limit caps.
        (write-string (with-output-to-string (joined)
                        (dolist (chunk (nreverse chunks))
                          (write-string chunk joined)))
                      *standard-output*)
        (finish-output *standard-output*))))
  terminal)

(defun terminal-begin-frame (terminal)
  "Begin a batched frame. A no-op for terminals that already accumulate."
  (when (typep terminal 'process-terminal)
    (begin-process-terminal-frame terminal))
  terminal)

(defun terminal-end-frame (terminal)
  "Flush a batched frame as one write. A no-op for terminals that already
   accumulate."
  (when (typep terminal 'process-terminal)
    (end-process-terminal-frame terminal))
  terminal)

(defun process-terminal-size (terminal)
  (multiple-value-bind (detected-columns detected-rows found)
      (current-terminal-window-size)
    (values (or (and found detected-columns)
                (parse-positive-integer (env-var "COLUMNS"))
                (terminal-columns terminal))
            (or (and found detected-rows)
                (parse-positive-integer (env-var "LINES"))
                (terminal-rows terminal)))))

(defparameter +enable-enhanced-keyboard+
  (format nil "~C[>1u" #\Esc)
  "Push kitty keyboard progressive enhancement (disambiguate flag) so modified
   keys such as Shift+Enter arrive as distinct CSI u sequences. Terminals that
   implement the protocol enable it. The rest ignore the push (and the matching
   pop) and report only unmodified keys.")

(defparameter +disable-enhanced-keyboard+
  (format nil "~C[<u" #\Esc)
  "Pop the kitty keyboard enhancement set at startup.")

(defun write-terminal-control (string)
  (write-string string *standard-output*)
  (finish-output *standard-output*))

(defun terminal-input-tty-p ()
  "True when fd 0 is a terminal whose attributes can be read."
  #+sbcl (handler-case (and (sb-posix:tcgetattr 0) t)
           (error () nil))
  #-sbcl nil)

(defun call-with-raw-terminal (thunk &key fallback)
  "Run THUNK with fd 0 in raw mode (ICANON, ISIG, ECHO, and ICRNL cleared,
VMIN=1 VTIME=0), restoring the original attributes afterward. When fd 0 is
not a terminal, run FALLBACK if supplied, otherwise THUNK, untouched."
  #+sbcl
  (let ((original (handler-case
                      (sb-posix:tcgetattr 0)
                    (error ()
                      nil))))
    (if original
        (let ((updated (sb-posix:tcgetattr 0)))
          (setf (sb-posix:termios-lflag updated)
                (logandc2 (sb-posix:termios-lflag updated)
                          (logior sb-posix:icanon
                                  sb-posix:isig
                                  sb-posix:echo)))
          (setf (sb-posix:termios-iflag updated)
                (logandc2 (sb-posix:termios-iflag updated)
                          sb-posix:icrnl))
          (let ((control-chars (sb-posix:termios-cc updated)))
            (setf (aref control-chars sb-posix:vmin) 1
                  (aref control-chars sb-posix:vtime) 0))
          (unwind-protect
               (progn
                 (sb-posix:tcsetattr 0 sb-posix:tcsanow updated)
                 (funcall thunk))
            (ignore-errors
              (sb-posix:tcsetattr 0 sb-posix:tcsanow original))))
        (if fallback
            (funcall fallback)
            (funcall thunk))))
  #-sbcl
  (declare (ignore fallback))
  #-sbcl
  (funcall thunk))

(defun call-with-terminal-character-input (thunk &key fallback)
  "Run THUNK in raw mode with the kitty keyboard enhancement pushed for the
duration. Falls back to FALLBACK (or THUNK) when fd 0 is not a terminal,
leaving the enhancement untouched."
  (call-with-raw-terminal
   (lambda ()
     (unwind-protect
          (progn
            (write-terminal-control +enable-enhanced-keyboard+)
            (funcall thunk))
       (ignore-errors
         (write-terminal-control +disable-enhanced-keyboard+))))
   :fallback (or fallback thunk)))
