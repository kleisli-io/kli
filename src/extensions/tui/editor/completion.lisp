(in-package #:kli/tui/editor)

(defconstant +completion-limit+ 6)

(defstruct completion-candidate
  insert
  match
  hint
  description
  value)

(defstruct completion-popup
  kind
  candidates
  (selected 0)
  start
  end
  action)

(defun completion-boundary-p (string index)
  (or (zerop index)
      (member (char string (1- index)) '(#\- #\_ #\. #\/ #\:))))

(defun fuzzy-score (query candidate)
  "Subsequence match score, NIL when QUERY is not a subsequence of
CANDIDATE. Higher is better: consecutive runs and boundary hits score
above scattered hits and a full prefix dominates."
  (let ((query (string-downcase query))
        (candidate (string-downcase candidate)))
    (cond
      ((zerop (length query)) 0)
      ((string-prefix-p query candidate)
       (+ 1000 (- 100 (length candidate))))
      (t
       (let ((score 0)
             (from 0)
             (previous-hit -2))
         (loop for char across query
               do (let ((hit (position char candidate :start from)))
                    (unless hit
                      (return-from fuzzy-score nil))
                    (incf score
                          (cond ((= hit (1+ previous-hit)) 3)
                                ((completion-boundary-p candidate hit) 2)
                                (t 1)))
                    (setf previous-hit hit
                          from (1+ hit))))
         score)))))

(defun rank-candidates (query candidates)
  "Matching candidates by descending score. Every match is kept; the
render window slides over the full list. The sort is stable so source
order survives empty-query ties."
  (let* ((scored (loop for candidate in candidates
                       for score = (fuzzy-score
                                    query
                                    (completion-candidate-match candidate))
                       when score
                         collect (cons score candidate)))
         (ranked (stable-sort scored #'> :key #'car)))
    (mapcar #'cdr ranked)))

(defun completion-token-constituent-p (char)
  (or (alphanumericp char)
      (member char '(#\- #\_ #\. #\/ #\~ #\:))))

(defun completion-sigil-site-p (value start sigil)
  (or (= start 1)
      (let ((before (char value (- start 2))))
        (not (or (alphanumericp before)
                 (char= before sigil))))))

(defun command-tail-start (value)
  "Index where a slash command's tail begins, or NIL when VALUE has no
complete first token."
  (when (and (> (length value) 1)
             (char= (char value 0) #\/))
    (let ((space (position-if (lambda (char)
                                (member char '(#\Space #\Tab #\Newline)))
                              value
                              :start 1)))
      (and space (> space 1) (1+ space)))))

(defun argument-completion-trigger (value cursor)
  "Argument completion over the whole tail of a slash command. The query
carries (name . tail) so the command's completer can answer, and accepted
candidates replace the full tail."
  (let ((start (command-tail-start value)))
    (when (and start (>= cursor start))
      (list :argument
            (cons (subseq value 1 (1- start)) (subseq value start))
            start
            (length value)))))

(defun completion-trigger (value cursor)
  "Completion context at CURSOR as (kind query start end), or NIL. A
leading slash completes command names over the first token only, then
command arguments over the tail. A dollar or at sign opening the token
at the cursor completes skills or files, at the same sites sigil
matching accepts."
  (let ((end cursor))
    (loop while (and (< end (length value))
                     (completion-token-constituent-p (char value end)))
          do (incf end))
    (cond
      ((and (plusp (length value))
            (char= (char value 0) #\/)
            (plusp cursor)
            (<= cursor (or (position-if-not #'completion-token-constituent-p
                                            value
                                            :start 1)
                           (length value))))
       (list :command (subseq value 1 cursor) 0 end))
      ((let ((start cursor))
         (loop while (and (plusp start)
                          (completion-token-constituent-p
                           (char value (1- start))))
               do (decf start))
         (let ((sigil (and (plusp start) (char value (1- start)))))
           (when (and (member sigil '(#\$ #\@))
                      (completion-sigil-site-p value start sigil))
             (list (if (char= sigil #\$) :skill :file)
                   (subseq value start cursor)
                   (1- start)
                   end)))))
      ((argument-completion-trigger value cursor)))))

(defun completion-provider (editor)
  (let ((protocol (object-protocol editor)))
    (and protocol
         (find-capability-provider protocol
                                   :completion
                                   :contract :completion/v1))))

(defun completion-candidates (editor kind query)
  "Ranked candidates for KIND from the :completion capability, or NIL
when no provider is installed. Argument queries carry (name . tail) and
rank against the tail, so completers may return unfiltered candidates."
  (let ((provider (completion-provider editor)))
    (when provider
      (if (eq kind :argument)
          (destructuring-bind (name . tail) query
            (rank-candidates
             tail
             (loop for entry in (getf (provider-call provider
                                                     :argument-help
                                                     (object-protocol editor)
                                                     name
                                                     tail)
                                      :candidates)
                   for insert = (if (consp entry) (car entry) entry)
                   collect (make-completion-candidate
                            :insert insert
                            :match insert
                            :description (and (consp entry) (cdr entry))))))
          (rank-candidates query
                           (provider-call provider
                                          :candidates
                                          (object-protocol editor)
                                          kind
                                          query))))))

(defun refresh-editor-completion (editor)
  (let ((trigger (completion-trigger (editor-value editor)
                                     (editor-cursor editor))))
    (setf (editor-completion editor)
          (when trigger
            (destructuring-bind (kind query start end) trigger
              (let ((candidates (completion-candidates editor kind query)))
                (when candidates
                  (make-completion-popup :kind kind
                                         :candidates candidates
                                         :start start
                                         :end end))))))))

(defun dismiss-editor-completion (editor)
  "Close the completion popup without touching the buffer. True when one
was open. Deliberately not routed through handle-input: the editor
behavior refreshes completion after every plain input, which would
re-open the popup from the still-present trigger."
  (when (editor-completion editor)
    (setf (editor-completion editor) nil)
    t))

(defun move-completion-selection (editor delta)
  (let ((popup (editor-completion editor)))
    (setf (completion-popup-selected popup)
          (mod (+ (completion-popup-selected popup) delta)
               (length (completion-popup-candidates popup)))))
  t)

(defun accept-completion (editor)
  "Splice the selected candidate over the trigger token and close the
popup, then re-run the trigger over the new buffer so the next menu opens
at once: accepting a command name pops its argument candidates, accepting
a directory chains into its entries. A trailing slash keeps the token
open so directory completion chains, anything else appends a separating
space. A popup carrying an action is a selection menu, not a text
completion: the action runs with the chosen candidate and the buffer
stays untouched, with no refresh, so anything the action does to the
editor wins."
  (let* ((popup (editor-completion editor))
         (candidate (nth (completion-popup-selected popup)
                         (completion-popup-candidates popup)))
         (action (completion-popup-action popup)))
    (cond
      (action
       (setf (editor-completion editor) nil)
       (funcall action candidate))
      (t
       (let* ((insert (completion-candidate-insert candidate))
              (insert (if (string-suffix-p "/" insert)
                          insert
                          (concatenate 'string insert " ")))
              (value (editor-value editor)))
         (push-undo-snapshot editor)
         (setf (editor-value editor)
               (concatenate 'string
                            (subseq value 0 (completion-popup-start popup))
                            insert
                            (subseq value (completion-popup-end popup)))
               (editor-cursor editor) (+ (completion-popup-start popup)
                                         (length insert))
               (editor-desired-column editor) nil
               (editor-completion editor) nil)
         (refresh-editor-completion editor)))))
  t)

(defun open-editor-selection (editor candidates &key action)
  "Open the popup as a selection menu over CANDIDATES: it renders and
navigates like completion and Esc dismisses it the same way, but Enter
calls ACTION with the chosen candidate instead of splicing. Plain typing
falls through to the buffer, whose refresh replaces the menu."
  (setf (editor-completion editor)
        (make-completion-popup :kind :selection
                               :candidates candidates
                               :action action))
  t)

(defun open-path-completion (editor)
  "Path completion of the token before the cursor. One match accepts
immediately, several open the popup, no token or no match declines."
  (let* ((value (editor-value editor))
         (cursor (editor-cursor editor))
         (start cursor))
    (loop while (and (plusp start)
                     (completion-token-constituent-p (char value (1- start))))
          do (decf start))
    (when (< start cursor)
      (let ((candidates (completion-candidates editor
                                               :path
                                               (subseq value start cursor))))
        (when candidates
          (setf (editor-completion editor)
                (make-completion-popup :kind :path
                                       :candidates candidates
                                       :start start
                                       :end cursor))
          (if (rest candidates)
              t
              (accept-completion editor)))))))

(defun handle-completion-input (editor data)
  "Popup keys ahead of plain editing, Tab opening path completion when
no popup is up. True when DATA was consumed."
  (let ((popup (editor-completion editor))
        (action (keymap-action (object-protocol editor) data)))
    (if popup
        (cond
          ((eq action :move-line-up) (move-completion-selection editor -1))
          ((eq action :move-line-down) (move-completion-selection editor 1))
          ((or (eq action :insert-tab) (submit-input-p data))
           (accept-completion editor)))
        (when (eq action :insert-tab)
          (open-path-completion editor)))))

(defun argument-hint-lines (editor width)
  "Dim argument help lines for the slash command being typed, when the
command (or its spec) has something to say about the tail. A help answer
carrying candidates renders as the popup instead, so the hint only shows
for tails that genuinely have no menu."
  (let* ((value (editor-value editor))
         (start (command-tail-start value))
         (provider (and start (completion-provider editor)))
         (help (and provider
                    (provider-call provider
                                   :argument-help
                                   (object-protocol editor)
                                   (subseq value 1 (1- start))
                                   (subseq value start))))
         (hint (and (null (getf help :candidates))
                    (getf help :hint))))
    (when (and hint (plusp (length hint)))
      (loop for line in (wrap-text hint (max 1 (- width 2)))
            collect (style-span (concatenate 'string "  " line)
                                :attrs '(:dim))))))

(defun completion-row-text (candidate)
  (let ((hint (completion-candidate-hint candidate)))
    (format nil "  ~A~@[ ~A~]"
            (completion-candidate-insert candidate)
            (and (stringp hint) (plusp (length hint)) hint))))

(defun completion-row-lines (text description column width)
  "Lines for one popup row: TEXT padded to COLUMN, DESCRIPTION wrapped
beside it with continuation lines indented to COLUMN."
  (let ((indent (make-string column :initial-element #\Space)))
    (loop for line in (wrap-text description (- width column))
          for prefix = (pad-right text column) then indent
          collect (concatenate 'string prefix line))))

(defun completion-window (candidates selected limit)
  "The visible slice of CANDIDATES, capped at LIMIT rows and slid to keep
SELECTED in view. Returns (values slice offset). Ranked completion keeps
every match, so the window slides for completion and selection menus alike."
  (let ((count (length candidates)))
    (if (<= count limit)
        (values candidates 0)
        (let ((start (min (max 0 (- selected (floor limit 2)))
                          (- count limit))))
          (values (subseq candidates start (+ start limit)) start)))))

(defun render-completion-lines (editor width)
  "Popup rows below the prompt: indented insert and string argument hint,
then descriptions aligned in one column and wrapped to the remaining
width. The selected row renders inverse, with styling applied after
plain-text layout so width math never sees escape bytes. A list longer
than the window scrolls with the selection and reports its position on a
dim trailer line. Without a popup, the argument hint lines render
instead."
  (let ((popup (editor-completion editor)))
    (if (null popup)
        (argument-hint-lines editor width)
        (multiple-value-bind (candidates offset)
            (completion-window (completion-popup-candidates popup)
                               (completion-popup-selected popup)
                               +completion-limit+)
          (let* ((total (length (completion-popup-candidates popup)))
                 (selected (- (completion-popup-selected popup) offset))
                 (texts (mapcar #'completion-row-text candidates))
                 (column (+ 2 (reduce #'max texts :key #'visible-width)))
                 (roomy (<= (+ column 10) width)))
            (append
             (loop for candidate in candidates
                   for text in texts
                   for index from 0
                   for description = (let ((d (completion-candidate-description
                                               candidate)))
                                       (and d (plusp (length d)) d))
                   for lines = (cond
                                 ((and description roomy)
                                  (completion-row-lines text description
                                                        column width))
                                 (description
                                  (list (truncate-to-width
                                         (format nil "~A  ~A" text description)
                                         width)))
                                 (t (list (truncate-to-width text width))))
                   append (if (= index selected)
                              (mapcar (lambda (line)
                                        (style-span line :attrs '(:inverse)))
                                      lines)
                              lines))
             (when (> total +completion-limit+)
               (list (style-span
                      (format nil "  ~D/~D"
                              (1+ (completion-popup-selected popup))
                              total)
                      :attrs '(:dim))))))))))
