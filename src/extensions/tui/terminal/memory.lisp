(in-package #:kli/tui/terminal)

(defun make-memory-terminal (&key protocol id (columns 80) (rows 24))
  (make-instance 'memory-terminal
                 :id (or id (make-terminal-id :memory))
                 :protocol protocol
                 :columns columns
                 :rows rows))

(defun write-memory-terminal (terminal string)
  (push string (terminal-output-chunks terminal))
  string)

(defun terminal-output (terminal)
  (apply #'concatenate 'string (reverse (terminal-output-chunks terminal))))

(defun terminal-clear (terminal)
  (setf (terminal-output-chunks terminal) '())
  terminal)

(defun terminal-size* (terminal)
  (values (terminal-columns terminal)
          (terminal-rows terminal)))

(defun terminal-clear-screen (terminal)
  (write-terminal terminal (format nil "~C[2J~C[H" #\Esc #\Esc)))

(defun terminal-clear-line (terminal)
  (write-terminal terminal (format nil "~C[K" #\Esc)))

(defun terminal-clear-scrollback (terminal)
  (write-terminal terminal (format nil "~C[3J" #\Esc)))

(defun terminal-move-cursor (terminal row column)
  (write-terminal terminal
                  (format nil "~C[~D;~DH" #\Esc (1+ row) (1+ column))))

(defun terminal-hide-cursor (terminal)
  (write-terminal terminal (format nil "~C[?25l" #\Esc)))

(defun terminal-show-cursor (terminal)
  (write-terminal terminal (format nil "~C[?25h" #\Esc)))

(defun terminal-begin-synchronized-update (terminal)
  (write-terminal terminal (format nil "~C[?2026h" #\Esc)))

(defun terminal-end-synchronized-update (terminal)
  (write-terminal terminal (format nil "~C[?2026l" #\Esc)))

(defun terminal-enable-bracketed-paste (terminal)
  (write-terminal terminal (format nil "~C[?2004h" #\Esc)))

(defun terminal-disable-bracketed-paste (terminal)
  (write-terminal terminal (format nil "~C[?2004l" #\Esc)))
