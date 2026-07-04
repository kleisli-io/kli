(in-package #:kli/output-spill)

;;; The retrieval surface over the materialization backend. When a tool result is
;;; truncated to the context window, it ends with an opaque handle; the model
;;; pages or searches the full, lossless result through these two tools. Neither
;;; touches the filesystem Read/search path, so neither is subject to its 2 MB
;;; cliff. Both dispatch on the backing kind -- a :file backing streams (bounded
;;; heap, never slurped); a :sequence backing slices in image. The lifecycle
;;; effect reaps a run's backings on teardown or rollback and runs the
;;; crash-backstop age sweep at install.

(defun read-result-file (entry handle start limit)
  (multiple-value-bind (lines next eof)
      (page-lines (spill-entry-path entry) start limit)
    (declare (ignore next))
    (let ((next-line (+ start (length lines))))
      (tool-text-result
       (format nil "~{~A~%~}~A"
               lines
               (if eof
                   (format nil "[handle ~A: end of result]" handle)
                   (format nil "[handle ~A: lines ~D..~D shown; more output — ~
read-result handle ~A start ~D]"
                           handle start next-line handle next-line)))
       :details (list :result-handle handle :kind :file
                      :start start :next next-line :eof eof)))))

(defun read-result-sequence (entry handle start limit)
  (multiple-value-bind (items next eof)
      (read-sequence-window (spill-entry-elements entry) start limit)
    (declare (ignore next))
    (let ((next-index (+ start (length items))))
      (tool-text-result
       (format nil "~{~A~%~}~A"
               items
               (if eof
                   (format nil "[handle ~A: end of result]" handle)
                   (format nil "[handle ~A: elements ~D..~D shown; more — ~
read-result handle ~A start ~D]"
                           handle start next-index handle next-index)))
       :details (list :result-handle handle :kind :sequence
                      :start start :next next-index :eof eof)))))

(defun run-read-result-tool (tool parameters context &key call-id on-update)
  "Page the full result behind HANDLE. START is the 0-based line (:file) or
element (:sequence) to begin at; LIMIT how many to return."
  (declare (ignore tool call-id on-update))
  (let* ((protocol (active-protocol context))
         (handle (required-tool-parameter parameters :handle))
         (start (max 0 (or (tool-parameter parameters :start) 0)))
         (limit (let ((l (tool-parameter parameters :limit)))
                  (if (and (integerp l) (plusp l)) l *output-spill-page-lines*)))
         (entry (and protocol (find-spill-entry protocol handle))))
    (if (null entry)
        (tool-text-result
         (format nil "No result for handle ~A (unknown or already evicted)." handle)
         :error-p t)
        (ecase (spill-entry-kind entry)
          (:file (read-result-file entry handle start limit))
          (:sequence (read-result-sequence entry handle start limit))))))

(defun format-search-result (handle matches next eof unit)
  (let* ((count (length matches))
         (phrase (if (= count 1) "1 match" (format nil "~D matches" count))))
    (tool-text-result
     (format nil "~{~A~%~}~A"
             (mapcar (lambda (match) (format nil "~D: ~A" (car match) (cdr match)))
                     matches)
             (cond
               ((and (null matches) eof)
                (format nil "[handle ~A: no matches]" handle))
               (eof
                (format nil "[handle ~A: ~A; end of result]" handle phrase))
               (t
                (format nil "[handle ~A: ~A; more ~A — search-result ~
handle ~A after ~D]"
                        handle phrase unit handle next))))
     :details (list :result-handle handle :matches count
                    :next next :eof eof))))

(defun run-search-result-tool (tool parameters context &key call-id on-update)
  "Search the full result behind HANDLE for the cl-ppcre PATTERN. AFTER is the
0-based line/element to resume past; MAX caps matches per page."
  (declare (ignore tool call-id on-update))
  (let* ((protocol (active-protocol context))
         (handle (required-tool-parameter parameters :handle))
         (pattern (required-tool-parameter parameters :pattern))
         (after (max 0 (or (tool-parameter parameters :after) 0)))
         (max (let ((m (tool-parameter parameters :max)))
                (if (and (integerp m) (plusp m)) m *output-spill-search-matches*)))
         (entry (and protocol (find-spill-entry protocol handle))))
    (cond
      ((null entry)
       (tool-text-result
        (format nil "No result for handle ~A (unknown or already evicted)." handle)
        :error-p t))
      (t
       (handler-case
           (ecase (spill-entry-kind entry)
             (:file
              (multiple-value-bind (matches next eof)
                  (search-within (spill-entry-path entry) pattern
                                 :start-line after :max-matches max)
                (format-search-result handle matches next eof "lines")))
             (:sequence
              (multiple-value-bind (matches next eof)
                  (search-sequence (spill-entry-elements entry) pattern after max)
                (format-search-result handle matches next eof "elements"))))
         (error (condition)
           (tool-text-result
            (format nil "Could not search result ~A: ~A" handle condition)
            :error-p t)))))))

(defextension output-spill
  (:provides
   (tool read-result
     :label "Read result"
     :description "Retrieve more of a large tool result by its handle. When a tool
result is truncated to fit the context, it ends with a handle token; pass that
token here to page the full, lossless output. start is the 0-based line (text
results) or element (list results) to begin at; limit is how many to return
(default 200). Returns the window plus a continuation cursor. Reaches any part of
the result, however large."
     :parameters '(:object (:handle :string)
                   (:start :integer :optional t)
                   (:limit :integer :optional t))
     :runner #'run-read-result-tool
     :metadata '(:capabilities (:result/read)))
   (tool search-result
     :label "Search result"
     :description "Search the full text of a large tool result by its handle for a
regular expression, without paging through it. The handle is the token a truncated
tool result ends with. pattern is the regex; after the 0-based line/element to
resume past; max the most matches to return (default 100). Returns each match as
\"N: text\" with its line/element number, plus a continuation cursor. Reaches
matches anywhere in the result — including past the inline window."
     :parameters '(:object (:handle :string)
                   (:pattern :string)
                   (:after :integer :optional t)
                   (:max :integer :optional t))
     :runner #'run-search-result-tool
     :metadata '(:capabilities (:result/read)))
   (effect output-spill-reap
     #'install-output-spill-reap
     #'retract-output-spill-reap)))
