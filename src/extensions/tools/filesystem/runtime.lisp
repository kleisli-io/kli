(in-package #:kli/tools/filesystem)

(defun file-pathname (parameters)
  (pathname (required-tool-parameter parameters :path)))

(defparameter *file-byte-limit* (* 2 1024 1024)
  "Largest file the filesystem tools read whole, in bytes. File contents
become full strings and anchor-cache line vectors in the image, at four
bytes per character, so an unbounded read of one large artifact can
exhaust the dynamic space.")

(defun file-byte-length (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun oversized-file-p (path)
  (> (file-byte-length path) *file-byte-limit*))

(defun assert-file-within-limit (path)
  (let ((size (file-byte-length path)))
    (when (> size *file-byte-limit*)
      (error "~A is ~:D bytes, over the ~:D byte limit for reading a file ~
whole. Search it with a pattern instead."
             path size *file-byte-limit*))))

(defun read-file-string (path)
  (with-open-file (stream path
                          :direction :input
                          :external-format :utf-8)
    (with-output-to-string (content)
      (loop for char = (read-char stream nil nil)
            while char
            do (write-char char content)))))

(defun write-file-string (path content)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  content)

(defun truthy-parameter-p (value)
  (and value
       (not (member value '(nil :false :no "false" "no" "0")
                    :test #'equalp))))


(defparameter *read-page-scan-lines* 1024
  "Lines per bounded-heap batch when a filesystem tool scans a file directly.")

(defun file-line-hashes (path)
  "Return all line hashes for PATH without retaining the file's line text."
  (let ((hashes (make-array 1024 :adjustable t :fill-pointer 0))
        (byte 0))
    (loop
      (multiple-value-bind (lines next eof)
          (read-lines-from-offset path byte *read-page-scan-lines*)
        (dolist (line lines)
          (vector-push-extend (line-hash line) hashes))
        (setf byte next)
        (when eof
          (return (coerce hashes 'simple-vector)))))))

(defparameter *read-line-limit* 1000
  "Most lines run-read-tool renders before soft-clamping the span. A file
within *file-byte-limit* can still be tens of thousands of normal lines, and
rendering them all floods the context window. Over the limit the read shows
the first *read-line-limit* lines of the requested span and surfaces a notice
to page further; it never errors.")

(defun run-read-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool call-id on-update))
  (let* ((path (file-pathname parameters))
         (raw (truthy-parameter-p (tool-parameter parameters :raw)))
         (hashes (file-line-hashes path))
         (line-count (length hashes)))
    (remember-read-hashes (active-protocol context) path hashes)
    (if (zerop line-count)
        (tool-text-result
         "(empty file)"
         :details (list :path (namestring path) :lines 0
                        :start nil :end nil :raw (and raw t)))
        (multiple-value-bind (start end)
            (clamp-range line-count
                         (tool-parameter parameters :start)
                         (tool-parameter parameters :end)
                         path)
          (let* ((clamped (> (1+ (- end start)) *read-line-limit*))
                 (shown-end (if clamped (1- (+ start *read-line-limit*)) end))
                 (page-size (1+ (- shown-end start))))
            (multiple-value-bind (page-lines next eof)
                (page-lines path (1- start) page-size)
              (declare (ignore next eof))
              (let* ((lines (coerce page-lines 'simple-vector))
                     (body (if raw
                               (render-raw-lines lines 1 (length lines))
                               (render-anchored-lines lines 1 (length lines)
                                                      (1- start)))))
                (tool-text-result
                 (if clamped
                     (format nil "~A~%(showing lines ~D..~D of ~D -- pass start/end ~
to page further)"
                             body start shown-end line-count)
                     body)
                 :details (list :path (namestring path) :lines line-count
                                :start start :end shown-end :raw (and raw t)
                                :truncated clamped)))))))))

(defun run-write-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool context call-id on-update))
  (let* ((path (file-pathname parameters))
         (content (required-tool-parameter parameters :content))
         (existed-p (probe-file path))
         (old (and existed-p
                   (not (oversized-file-p path))
                   (read-file-string path))))
    (write-file-string path content)
    (let ((old-known-p (or (not existed-p) (stringp old))))
      (tool-text-result
       (format nil "Wrote ~D characters to ~A." (length content) path)
       :details (compact-file-change-detail
                 (namestring path)
                 old
                 content
                 :old-known-p old-known-p
                 :characters (length content)
                 :created-p (not existed-p)
                 :overwritten-p (and existed-p t))
       :presentation
       (result-diff
        :updates (list (file-diff-presentation-update
                        (namestring path) old content
                        :old-known-p old-known-p)))))))

(defparameter *walk-entry-limit* 100000
  "Most filesystem entries one find/search glob walk visits. A store-scale
glob like /nix/store/**/*.lisp covers millions of inodes, and the agent
worker has no abort check inside a tool run, so an unbounded walk wedges
the session for its full duration. The bound turns it into a fast partial
result with the stop surfaced.")

(defparameter *walk-deadline-seconds* 10
  "Wall-clock bound on one find/search glob walk, for trees that are slow
rather than wide (cold cache, network filesystems). Checked per entry
alongside *walk-entry-limit*.")

(defun split-on-char (char string)
  "Substrings of STRING between occurrences of CHAR, keeping empty pieces."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start (or pos (length string)))
        while pos
        do (setf start (1+ pos))))

(defun wild-segment-p (segment)
  (find #\* segment))

(defun match-wild-segment (segment name)
  "True when NAME matches SEGMENT, with each * spanning any run of
characters."
  (let* ((parts (split-on-char #\* segment))
         (first-part (pop parts))
         (pos (length first-part)))
    (when (or (> pos (length name))
              (string/= first-part name :end2 pos))
      (return-from match-wild-segment nil))
    (loop while parts
          for part = (pop parts)
          for lastp = (null parts)
          do (cond
               ((zerop (length part))
                (when lastp (setf pos (length name))))
               (lastp
                (let ((start (- (length name) (length part))))
                  (unless (and (>= start pos)
                               (string= part name :start2 start))
                    (return-from match-wild-segment nil))
                  (setf pos (length name))))
               (t
                (let ((found (search part name :start2 pos)))
                  (unless found
                    (return-from match-wild-segment nil))
                  (setf pos (+ found (length part)))))))
    (= pos (length name))))

(defun glob-segment-matches-p (segment name)
  (if (wild-segment-p segment)
      (match-wild-segment segment name)
      (string= segment name)))

(defun close-glob-positions (components positions)
  "POSITIONS closed under zero-width ** matches: a position at a **
component also admits the position after it."
  (let ((closed (copy-list positions)))
    (loop for changed = nil
          do (dolist (p closed)
               (when (and (< p (length components))
                          (string= (svref components p) "**")
                          (not (member (1+ p) closed)))
                 (push (1+ p) closed)
                 (setf changed t)))
             (unless changed (return closed)))))

(defun advance-glob-positions (components positions name)
  "Position set after descending into subdirectory NAME, NIL when the
pattern cannot match anything below it."
  (let ((next '()))
    (dolist (p positions)
      (when (< p (length components))
        (let ((component (svref components p)))
          (if (string= component "**")
              (pushnew p next)
              (when (glob-segment-matches-p component name)
                (pushnew (1+ p) next))))))
    (and next (close-glob-positions components next))))

(defun glob-positions-accept-p (components positions)
  (and (member (length components) positions) t))

(defun directory-entry-names (dir)
  "Sorted entry names of directory namestring DIR, or :UNREADABLE when it
cannot be opened. Dot and dot-dot are dropped."
  (handler-case
      (let ((stream (sb-posix:opendir dir))
            (names '()))
        (unwind-protect
             (loop for entry = (sb-posix:readdir stream)
                   until (sb-alien:null-alien entry)
                   do (let ((name (sb-posix:dirent-name entry)))
                        (unless (or (string= name ".") (string= name ".."))
                          (push name names))))
          (sb-posix:closedir stream))
        (sort names #'string<))
    (sb-posix:syscall-error () :unreadable)))

(defun walk-glob (pattern on-match)
  "Walk filesystem entries matching glob PATTERN, calling ON-MATCH with
path namestring, byte size, and kind (:file or :directory) per match.
Directory components support * and a bare ** component spans any depth,
including zero. A bare directory path is expanded to dir/**/* so find and
search visit its contents. Symlinks to files match. Symlinked directories are never
descended, so link cycles cannot loop the walk. ON-MATCH returning :STOP
ends the walk early. Primary value is NIL for a completed walk or a
truncation plist (:reason :entries | :deadline | :consumer | :aborted),
secondary value the count of unreadable directories skipped."
  (let ((segments (remove "" (split-on-char #\/ pattern) :test #'string=))
        (absolute (and (plusp (length pattern)) (char= (char pattern 0) #\/)))
        (visited 0)
        (unreadable 0)
        (deadline (+ (get-internal-real-time)
                     (round (* *walk-deadline-seconds*
                               internal-time-units-per-second)))))
    (block walk
      (flet ((emit (path size kind)
               (when (eq :stop (funcall on-match path size kind))
                 (return-from walk
                   (values (list :reason :consumer) unreadable)))))
        (unless (find-if #'wild-segment-p segments)
          (let ((truename (ignore-errors (probe-file pattern))))
            (cond
              ((null truename)
               (return-from walk (values nil unreadable)))
              ((or (pathname-name truename) (pathname-type truename))
               (let* ((name (namestring truename))
                      (stat (ignore-errors (sb-posix:stat name))))
                 (when stat
                   (emit name (sb-posix:stat-size stat) :file)))
               (return-from walk (values nil unreadable)))
              (t
               ;; Bare directory: expand to a recursive walk so find/search
               ;; visit its contents instead of the directory entry itself.
               (setf segments (append segments '("**" "*")))))))
        (let* ((leaf (car (last segments)))
               (dir-segments (butlast segments))
               (root-parts (loop for segment in dir-segments
                                 while (not (wild-segment-p segment))
                                 collect segment))
               (wild-components (coerce (nthcdr (length root-parts)
                                                dir-segments)
                                        'vector))
               (root-string (format nil "~:[~;/~]~{~A/~}" absolute root-parts))
               (root (ignore-errors
                      (truename (merge-pathnames
                                 (if (string= root-string "")
                                     "./"
                                     root-string))))))
          (unless root
            (return-from walk (values nil unreadable)))
          (labels ((check-budget ()
                     (incf visited)
                     (when (tool-abort-requested-p)
                       (return-from walk
                         (values (list :reason :aborted) unreadable)))
                     (when (> visited *walk-entry-limit*)
                       (return-from walk
                         (values (list :reason :entries) unreadable)))
                     (when (>= (get-internal-real-time) deadline)
                       (return-from walk
                         (values (list :reason :deadline) unreadable))))
                   (walk-directory (dir positions)
                     (let ((names (directory-entry-names dir)))
                       (when (eq names :unreadable)
                         (incf unreadable)
                         (return-from walk-directory))
                       (let ((accept (glob-positions-accept-p wild-components
                                                              positions)))
                         (dolist (name names)
                           (check-budget)
                           (let* ((path (concatenate 'string dir name))
                                  (stat (ignore-errors (sb-posix:lstat path))))
                             (when stat
                               (let ((ftype (logand (sb-posix:stat-mode stat)
                                                    sb-posix:s-ifmt))
                                     (leaf-match
                                       (and accept
                                            (glob-segment-matches-p leaf
                                                                    name))))
                                 (cond
                                   ((= ftype sb-posix:s-ifdir)
                                    (when leaf-match
                                      (emit (concatenate 'string path "/")
                                            0 :directory))
                                    (let ((next (advance-glob-positions
                                                 wild-components
                                                 positions
                                                 name)))
                                      (when next
                                        (walk-directory
                                         (concatenate 'string path "/")
                                         next))))
                                   ((not leaf-match))
                                   ((= ftype sb-posix:s-ifreg)
                                    (emit path (sb-posix:stat-size stat)
                                          :file))
                                   ((= ftype sb-posix:s-iflnk)
                                    (let ((target (ignore-errors
                                                   (sb-posix:stat path))))
                                      (when (and target
                                                 (= (logand
                                                     (sb-posix:stat-mode
                                                      target)
                                                     sb-posix:s-ifmt)
                                                    sb-posix:s-ifreg))
                                        (emit path
                                              (sb-posix:stat-size target)
                                              :file)))))))))))))
            (walk-directory (namestring root)
                            (close-glob-positions wild-components (list 0))))
          (values nil unreadable))))))

(defun walk-notices (truncation unreadable)
  "User-facing notice lines for a bounded walk's stop reason and skipped
unreadable directories. A :consumer stop is the caller's own cap, messaged
by the caller."
  (append
   (case (getf truncation :reason)
     (:entries
      (list (format nil "(walk stopped at the ~:D-entry limit -- results ~
are partial, narrow the path)"
                    *walk-entry-limit*)))
     (:deadline
      (list (format nil "(walk stopped after ~D second~:P -- results are ~
partial, narrow the path)"
                    *walk-deadline-seconds*)))
     (:aborted
      (list "(walk stopped -- abort was requested, results are partial)"))
     (t nil))
   (when (plusp unreadable)
     (list (format nil "(~D director~:@P could not be read)" unreadable)))))

(defparameter *find-result-limit* 1000
  "Most paths the find tool reports for one glob. A store-scale glob
matches tens of thousands of files, and every reported path lands in the
image as a namestring inside one result blob, so an uncapped listing can
exhaust the dynamic space.")

(defun run-find-tool (tool parameters context &key call-id on-update)
  "List paths matching a glob pattern, names only with no content read.
The walk is bounded by *WALK-ENTRY-LIMIT* and *WALK-DEADLINE-SECONDS*, and
at most *FIND-RESULT-LIMIT* paths are reported sorted, with the overflow
counted in the text and any stop flagged in details."
  (declare (ignore tool call-id on-update))
  (let* ((pattern (required-tool-parameter parameters :pattern))
         (protocol (active-protocol context))
         (tee (and protocol
                   (open-spill-tee protocol
                                   :producer-uuid :find
                                   :window-limit most-positive-fixnum)))
         (tee-closed nil)
         (paths '())
         (count 0))
    (unwind-protect
         (multiple-value-bind (truncation unreadable)
             (walk-glob pattern
                        (lambda (path size kind)
                          (declare (ignore size kind))
                          (incf count)
                          (when tee
                            (write-line path tee))
                          (when (<= count *find-result-limit*)
                            (push path paths))
                          nil))
           (let* ((sorted (sort paths #'string<))
                  (over (max 0 (- count *find-result-limit*)))
                  (truncated (or (plusp over) (and truncation t)))
                  (entry (and truncated
                              tee
                              (finalize-spill-tee tee)))
                  (handle (and entry (spill-entry-token entry)))
                  (bytes (and entry (spill-entry-bytes entry)))
                  (notices (append
                            (when (plusp over)
                              (list (format nil "(and ~D more)" over)))
                            (walk-notices truncation unreadable)
                            (when truncated
                              (list (format-spill-marker
                                     "find results"
                                     :shown (min count *find-result-limit*)
                                     :shown-unit "paths"
                                     :total count
                                     :handle handle
                                     :unit "path"
                                     :degraded (null handle)))))))
             (unless truncated
               (discard-spill-tee tee))
             (setf tee-closed t)
             (tool-text-result
              (format nil "~{~A~^~%~}"
                      (append (or sorted (list "(no matches)")) notices))
              :details (list :pattern pattern :count count
                             :result-handle handle
                             :result-bytes bytes
                             :truncated truncated))))
      (unless tee-closed
        (discard-spill-tee tee)))))

(defun line-match-count (scanner line)
  "Non-empty non-overlapping matches of SCANNER in LINE. Computed only for
over-limit lines, to count the matches a single window cannot show. Zero-width
matches are stepped over but not counted: they display nothing, so counting
them would inflate the hidden-match tally for an empty-capable pattern like a*."
  (loop with start = 0 with count = 0
        do (multiple-value-bind (mstart mend) (scan scanner line :start start)
             (unless mstart (return count))
             (when (> mend mstart) (incf count))
             (setf start (if (> mend mstart) mend (1+ mstart)))
             (when (> start (length line)) (return count)))))

(defun search-line-record (scanner line index)
  (multiple-value-bind (mstart mend) (scan scanner line)
    (when mstart
      (list index mstart mend
            (if (> (length line) *render-line-limit*)
                (line-match-count scanner line)
                0)))))


(defun search-match-indices (scanner lines)
  "Per matching line, a record (INDEX MSTART MEND COUNT): the line's index,
the first match's bounds for windowing, and the on-line match count. COUNT is
computed only for over-limit lines, where the window hides later matches; it
is 0 otherwise."
  (let ((records '()))
    (dotimes (index (length lines) (nreverse records))
      (let ((record (search-line-record scanner (svref lines index) index)))
        (when record
          (push record records))))))

(defun render-search-block (path lines records surrounding out char-budget
                            &optional (line-offset 0))
  "One file's search block: the path, then anchored lines ascending. Match
lines carry a * prefix, context lines a space prefix, and overlapping context
regions merge. An over-limit match line is windowed around its first match,
with a count of any matches the window hides; an over-limit context line is
front-truncated. Stops emitting once CHAR-BUDGET is spent -- but always emits
at least one line, so the block is never a bare path -- and returns T when it
cut the block short, letting the caller bound total search output to within one
line of the budget."
  (let ((show (make-array (length lines) :element-type 'bit :initial-element 0))
        (match (make-array (length lines) :element-type 'bit :initial-element 0))
        (windows (make-hash-table))
        (path-string (namestring path))
        (spent 0)
        (emitted 0)
        (truncated nil))
    (dolist (record records)
      (destructuring-bind (index mstart mend count) record
        (setf (sbit match index) 1)
        (setf (gethash index windows) (list mstart mend count))
        (loop for i from (max 0 (- index surrounding))
                to (min (1- (length lines)) (+ index surrounding))
              do (setf (sbit show i) 1))))
    (write-string path-string out)
    (incf spent (length path-string))
    (loop for i from 0 below (length lines)
          for line = (svref lines i)
          when (= 1 (sbit show i))
            do (let ((text (if (= 1 (sbit match i))
                               (destructuring-bind (mstart mend count) (gethash i windows)
                                 (format nil "~%~A~@[ (+~D more matches on this line)~]"
                                         (render-anchored-row
                                          (+ line-offset i 1) line
                                          :prefix "*"
                                          :rendered-line (render-window line mstart mend))
                                         (when (> count 1) (1- count))))
                               (format nil "~%~A"
                                       (render-anchored-row
                                        (+ line-offset i 1) line
                                        :prefix " ")))))
                 (when (and (plusp emitted)
                            (> (+ spent (length text)) char-budget))
                   (setf truncated t)
                   (loop-finish))
                 (write-string text out)
                 (incf spent (length text))
                 (incf emitted)))
    truncated))

(defun render-search-record-block (path records surrounding out char-budget)
  "Render a search block from absolute line RECORDS, paging only the context ranges needed."
  (let ((ranges '()))
    (dolist (record records)
      (destructuring-bind (index mstart mend count) record
        (declare (ignore mstart mend count))
        (let ((range (cons (max 0 (- index surrounding)) (+ index surrounding))))
          (if (and ranges (<= (car range) (1+ (cdar ranges))))
              (setf (cdar ranges) (max (cdar ranges) (cdr range)))
              (push range ranges)))))
    (let ((record-table (make-hash-table))
          (path-string (namestring path))
          (spent 0)
          (emitted 0)
          (truncated nil))
      (dolist (record records)
        (setf (gethash (first record) record-table) record))
      (write-string path-string out)
      (incf spent (length path-string))
      (dolist (range (nreverse ranges))
        (multiple-value-bind (page-lines next eof)
            (page-lines path (car range) (1+ (- (cdr range) (car range))))
          (declare (ignore next eof))
          (loop for line in page-lines
                for index from (car range)
                do (let* ((record (gethash index record-table))
                          (text (if record
                                    (destructuring-bind (rindex mstart mend count) record
                                      (declare (ignore rindex))
                                      (format nil "~%~A~@[ (+~D more matches on this line)~]"
                                              (render-anchored-row
                                               (1+ index) line
                                               :prefix "*"
                                               :rendered-line (render-window line mstart mend))
                                              (when (> count 1) (1- count))))
                                    (format nil "~%~A"
                                            (render-anchored-row
                                             (1+ index) line
                                             :prefix " ")))))
                     (when (and (plusp emitted)
                                (> (+ spent (length text)) char-budget))
                       (setf truncated t)
                       (return))
                     (write-string text out)
                     (incf spent (length text))
                     (incf emitted)))
          (when truncated
            (return))))
      truncated)))

(defun search-file-records (path scanner)
  "Scan PATH in bounded batches. Returns matching records and line hashes."
  (let ((records '())
        (hashes (make-array 1024 :adjustable t :fill-pointer 0))
        (byte 0)
        (lineno 0))
    (loop
      (multiple-value-bind (lines next eof)
          (read-lines-from-offset path byte *read-page-scan-lines*)
        (dolist (line lines)
          (vector-push-extend (line-hash line) hashes)
          (let ((record (search-line-record scanner line lineno)))
            (when record
              (push record records)))
          (incf lineno))
        (setf byte next)
        (when eof
          (return (values (nreverse records)
                          (coerce hashes 'simple-vector))))))))

(defparameter *search-file-limit* 100
  "Most matching files one search renders. Each matching file is scanned in
bounded batches and rendered as a block, so a broad pattern over a broad glob
would otherwise build an unbounded result.")

(defparameter *search-match-limit* 1000
  "Most match lines one search renders across all files. Bounds the
result blob when a pattern matches densely inside the file limit.")

(defparameter *search-output-limit* 100000
  "Most characters one search renders across all file blocks. A backstop over
the *search-match-limit* count cap: dense matches, wide match windows, and
context lines each inflate a block, so a search under the match-line cap could
still flood the context window. render-search-record-block stops mid-block once
this budget is spent, so a single dense file cannot overrun it -- total output
overshoots by at most one line.")

(defparameter *search-scan-timeout-seconds* 2
  "Wall-clock bound on scanning one file. The walk deadline is checked between
files, never inside one cl-ppcre:scan, so a catastrophically backtracking
pattern over a wide line would wedge the session. On timeout the file is
skipped and the count surfaced. An interrupt-based timeout is the only thing
that defeats backtracking; cooperative checks cannot break into one scan.")

(defun run-search-tool (tool parameters context &key call-id on-update)
  "Regex search across a file or glob. Matching files are scanned in bounded
batches and arm the anchor cache with line hashes, so anchors are directly
editable without retaining full file contents. Files that fail to read as text
are skipped silently. A file whose scan exceeds
*SEARCH-SCAN-TIMEOUT-SECONDS* is skipped with the count surfaced. The walk is
bounded by *WALK-ENTRY-LIMIT* and *WALK-DEADLINE-SECONDS*, the result by
*SEARCH-FILE-LIMIT*, *SEARCH-MATCH-LIMIT*, and the *SEARCH-OUTPUT-LIMIT*
character backstop, with every stop surfaced in the text and flagged in
details."
  (declare (ignore tool call-id on-update))
  (let* ((pattern (required-tool-parameter parameters :pattern))
         (path (required-tool-parameter parameters :path))
         (surrounding (max 0 (or (tool-parameter parameters :context) 0)))
         (scanner (create-scanner pattern))
         (protocol (active-protocol context))
         (tee (and protocol
                   (open-spill-tee protocol
                                   :producer-uuid :search
                                   :window-limit most-positive-fixnum)))
         (tee-closed nil)
         (spill-blocks 0)
         (blocks '())
         (match-count 0)
         (file-count 0)
         (skipped-count 0)
         (timed-out-count 0)
         (output-chars 0)
         (render-budget *search-match-limit*)
         (cap nil))
    (unwind-protect
         (multiple-value-bind (truncation unreadable)
             (walk-glob
              path
              (lambda (file size kind)
                (declare (ignore size))
                (block scan-file
                  (when (eq kind :directory)
                    (return-from scan-file nil))
                  (multiple-value-bind (indices hashes)
                      (handler-case
                          (sb-ext:with-timeout *search-scan-timeout-seconds*
                            (search-file-records file scanner))
                        (sb-ext:timeout ()
                          (incf timed-out-count)
                          (return-from scan-file nil))
                        (error ()
                          (return-from scan-file nil)))
                    (unless indices
                      (return-from scan-file nil))
                    (remember-read-hashes protocol file hashes)
                    (incf file-count)
                    (incf match-count (length indices))
                    (when tee
                      (when (plusp spill-blocks)
                        (terpri tee))
                      (render-search-record-block file indices surrounding tee
                                                  most-positive-fixnum)
                      (incf spill-blocks))
                    (let* ((shown (if (< render-budget (length indices))
                                      (subseq indices 0 render-budget)
                                      indices))
                           (block-truncated nil)
                           (block-text (with-output-to-string (out)
                                         (setf block-truncated
                                               (render-search-record-block
                                                file shown surrounding out
                                                (- *search-output-limit*
                                                   output-chars))))))
                      (decf render-budget (length shown))
                      (incf output-chars (length block-text))
                      (push block-text blocks)
                      (cond
                        ((or block-truncated
                             (>= output-chars *search-output-limit*))
                         (setf cap :output) :stop)
                        ((<= render-budget 0) (setf cap :matches) :stop)
                        ((>= file-count *search-file-limit*)
                         (setf cap :files) :stop)
                        (t nil)))))))
           (let* ((truncated (and (or cap truncation
                                      (plusp timed-out-count))
                                  t))
                  (entry (and truncated
                              tee
                              (finalize-spill-tee tee)))
                  (handle (and entry (spill-entry-token entry)))
                  (bytes (and entry (spill-entry-bytes entry)))
                  (shown (case cap
                           (:matches (min match-count *search-match-limit*))
                           (:files (min file-count *search-file-limit*))
                           (t output-chars)))
                  (shown-unit (case cap
                                (:matches "matches")
                                (:files "files")
                                (t "chars")))
                  (notices
                    (append
                     (when (plusp skipped-count)
                       (list (format nil
                                     "(skipped ~D file~:P over ~:D bytes)"
                                     skipped-count *file-byte-limit*)))
                     (when (plusp timed-out-count)
                       (list (format nil "(scan timed out on ~D file~:P -- ~
simplify the pattern)"
                                     timed-out-count)))
                     (case cap
                       (:files
                        (list (format nil "(stopped at the ~D matching-file ~
limit -- narrow the pattern or path)"
                                      *search-file-limit*)))
                       (:matches
                        (list (format nil "(stopped at the ~:D rendered-match ~
limit -- narrow the pattern or path)"
                                      *search-match-limit*)))
                       (:output
                        (list (format nil "(stopped at the ~:D-character ~
output limit -- narrow the pattern or path)"
                                      *search-output-limit*)))
                       (t nil))
                     (walk-notices truncation unreadable)
                     (when truncated
                       (list (format-spill-marker
                              "search results"
                              :shown shown
                              :shown-unit shown-unit
                              :total bytes
                              :handle handle
                              :unit "byte"
                              :degraded (null handle)))))))
             (unless truncated
               (discard-spill-tee tee))
             (setf tee-closed t)
             (tool-text-result
              (format nil "~{~A~^~%~}"
                      (append (or (nreverse blocks) (list "(no matches)"))
                              notices))
              :details (list :pattern pattern :path path
                             :matches match-count :file-count file-count
                             :skipped skipped-count
                             :timed-out timed-out-count
                             :result-handle handle
                             :result-bytes bytes
                             :truncated truncated))))
      (unless tee-closed
        (discard-spill-tee tee)))))


(defconstant +repair-candidates-key+ :filesystem/repair-candidates)
(defparameter *repair-candidate-limit* 16
  "Most pending edit repair candidates retained in one protocol.")

(defparameter +cl-source-types+ '("lisp" "asd" "cl" "lsp")
  "Pathname types treated as Common Lisp source by hashline edit repair.")

(defun cl-source-path-p (path)
  (let ((type (pathname-type (pathname path))))
    (and (stringp type)
         (member type +cl-source-types+ :test #'string-equal))))

(defun file-content-sha256 (content)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (sb-ext:string-to-octets content :external-format :utf-8))))

(defun canonical-file-namestring (path)
  (handler-case
      (namestring (truename path))
    (error ()
      (namestring (pathname path)))))

(defun lines->content (lines)
  (format nil "~{~A~%~}" (coerce lines 'list)))

(defun repair-candidates (protocol)
  (ensure-protocol-storage protocol +repair-candidates-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun prune-repair-candidates (table)
  (let ((entries '()))
    (maphash (lambda (id candidate)
               (push (cons (getf candidate :timestamp) id) entries))
             table)
    (dolist (entry (nthcdr *repair-candidate-limit*
                           (sort entries #'> :key #'car)))
      (remhash (cdr entry) table))))

(defun make-repair-candidate-id (path base-hash patched-hash repaired-hash)
  (subseq (file-content-sha256
           (format nil "~A~%~A~%~A~%~A~%~A"
                   path base-hash patched-hash repaired-hash
                   (random most-positive-fixnum)))
          0 16))

(defun store-repair-candidate (protocol candidate)
  (let ((table (repair-candidates protocol)))
    (setf (gethash (getf candidate :id) table) candidate)
    (prune-repair-candidates table)
    candidate))

(defun parse-repair-policy (value)
  (let ((policy (cond ((null value) "safe")
                      ((keywordp value) (string-downcase (symbol-name value)))
                      (t (string-downcase (princ-to-string value))))))
    (cond ((string= policy "reject") :reject)
          ((string= policy "preview") :preview)
          ((string= policy "safe") :safe)
          (t (error "Unknown edit repair policy ~S. Use reject, preview, or safe. Unsafe repair previews are accepted with accept_repair plus path."
                    value)))))

(defun changed-line-range (old-lines new-lines)
  "Inclusive 1-based changed range in NEW-LINES coordinates, or NIL."
  (let* ((old-len (length old-lines))
         (new-len (length new-lines))
         (prefix 0))
    (loop while (and (< prefix old-len)
                     (< prefix new-len)
                     (string= (svref old-lines prefix) (svref new-lines prefix)))
          do (incf prefix))
    (unless (and (= prefix old-len) (= prefix new-len))
      (let ((old-suffix 0)
            (new-suffix 0))
        (loop while (and (< (+ prefix old-suffix) old-len)
                         (< (+ prefix new-suffix) new-len)
                         (string= (svref old-lines (- old-len old-suffix 1))
                                  (svref new-lines (- new-len new-suffix 1))))
              do (incf old-suffix)
                 (incf new-suffix))
        (cons (1+ prefix)
              (max (1+ prefix) (- new-len new-suffix)))))))

(defun line-change-summary (old-lines new-lines)
  "Return START END ADDED REMOVED for one changed hunk in NEW-LINES coordinates."
  (let* ((ol (length old-lines))
         (nl (length new-lines))
         (common (min ol nl))
         (prefix (loop for i below common
                       while (string= (svref old-lines i) (svref new-lines i))
                       count t))
         (suffix (loop for i below (- common prefix)
                       while (string= (svref old-lines (- ol 1 i))
                                      (svref new-lines (- nl 1 i)))
                       count t)))
    (values (1+ prefix) (- nl suffix)
            (- nl suffix prefix) (- ol suffix prefix))))

(defun compact-changed-ranges (start end added removed)
  (when (plusp (+ added removed))
    (list (list :start start :end end))))

(defparameter *file-diff-presentation-context-lines* 3
  "Unchanged lines retained around each private file-diff hunk.")

(defparameter *file-diff-presentation-line-cap* 200
  "Most diff rows persisted in one private file-diff presentation update.")

(defparameter *file-diff-presentation-line-char-cap* 1000
  "Longest single diff row text persisted in private presentation data.")

(defun diff-presentation-opcodes (old-lines new-lines)
  (let ((matcher (make-instance 'sequence-matcher :test-function #'equal)))
    (set-sequences matcher (coerce old-lines 'list) (coerce new-lines 'list))
    (get-opcodes matcher)))

(defun diff-presentation-window (op context-lines)
  (list :i-start (max 0 (- (opcode-i1 op) context-lines))
        :i-end (+ (opcode-i2 op) context-lines)
        :j-start (max 0 (- (opcode-j1 op) context-lines))
        :j-end (+ (opcode-j2 op) context-lines)
        :hunks 1))

(defun merge-diff-presentation-windows (windows)
  (let ((out '()))
    (dolist (window windows (nreverse out))
      (let ((last (first out)))
        (if (and last
                 (<= (getf window :i-start) (getf last :i-end))
                 (<= (getf window :j-start) (getf last :j-end)))
            (setf (getf last :i-end) (max (getf last :i-end)
                                          (getf window :i-end))
                  (getf last :j-end) (max (getf last :j-end)
                                          (getf window :j-end))
                  (getf last :hunks) (+ (getf last :hunks)
                                        (getf window :hunks)))
            (push (copy-list window) out))))))

(defun clip-diff-presentation-segment (op window)
  (let ((tag (opcode-tag op))
        (i1 (opcode-i1 op))
        (i2 (opcode-i2 op))
        (j1 (opcode-j1 op))
        (j2 (opcode-j2 op))
        (wi1 (getf window :i-start))
        (wi2 (getf window :i-end))
        (wj1 (getf window :j-start))
        (wj2 (getf window :j-end)))
    (ecase tag
      (:equal
       (let* ((lo (max i1 wi1 (+ i1 (- wj1 j1))))
              (hi (min i2 wi2 (+ i1 (- wj2 j1)))))
         (when (< lo hi)
           (list :equal lo hi (+ j1 (- lo i1)) (+ j1 (- hi i1))))))
      (:delete
       (when (< (max i1 wi1) (min i2 wi2))
         (list :delete i1 i2 j1 j2)))
      (:insert
       (when (< (max j1 wj1) (min j2 wj2))
         (list :insert i1 i2 j1 j2)))
      (:replace
       (when (or (< (max i1 wi1) (min i2 wi2))
                 (< (max j1 wj1) (min j2 wj2)))
         (list :replace i1 i2 j1 j2))))))

(defun bounded-diff-presentation-line (line)
  (if (<= (length line) *file-diff-presentation-line-char-cap*)
      line
      (format nil "~A …"
              (subseq line 0 *file-diff-presentation-line-char-cap*))))

(defun diff-presentation-row (kind old-line new-line text)
  (append (list :kind kind)
          (when old-line (list :old-line old-line))
          (when new-line (list :new-line new-line))
          (list :text (bounded-diff-presentation-line text))))

(defun diff-presentation-segment-rows (segment old-lines new-lines)
  (destructuring-bind (tag i1 i2 j1 j2) segment
    (ecase tag
      (:equal
       (loop for i from i1 below i2
             for j from j1 below j2
             collect (diff-presentation-row :context (1+ i) (1+ j)
                                            (svref old-lines i))))
      (:delete
       (loop for i from i1 below i2
             collect (diff-presentation-row :remove (1+ i) nil
                                            (svref old-lines i))))
      (:insert
       (loop for j from j1 below j2
             collect (diff-presentation-row :add nil (1+ j)
                                            (svref new-lines j))))
      (:replace
       (append
        (loop for i from i1 below i2
              collect (diff-presentation-row :remove (1+ i) nil
                                             (svref old-lines i)))
        (loop for j from j1 below j2
              collect (diff-presentation-row :add nil (1+ j)
                                             (svref new-lines j))))))))

(defun diff-presentation-window-rows (window opcodes old-lines new-lines)
  (loop for op in opcodes
        for clipped = (clip-diff-presentation-segment op window)
        when clipped
          append (diff-presentation-segment-rows clipped old-lines new-lines)))

(defun sum-window-hunks (windows)
  (loop for window in windows sum (or (getf window :hunks) 1)))

(defun diff-presentation-hunks (old-lines new-lines)
  (let* ((opcodes (diff-presentation-opcodes old-lines new-lines))
         (windows (merge-diff-presentation-windows
                   (loop for op in opcodes
                         unless (eq (opcode-tag op) :equal)
                           collect (diff-presentation-window
                                    op *file-diff-presentation-context-lines*))))
         (cap *file-diff-presentation-line-cap*)
         (used 0)
         (blocks '())
         (truncated-p nil)
         (hidden-hunks 0))
    (loop while windows
          for window = (pop windows)
          for rows = (diff-presentation-window-rows window opcodes
                                                    old-lines new-lines)
          for row-count = (length rows)
          do (cond ((<= (+ used row-count) cap)
                    (push (list :rows rows :hunks (getf window :hunks))
                          blocks)
                    (incf used row-count))
                   (t
                    (let ((space (max 0 (- cap used))))
                      (when (plusp space)
                        (push (list :rows (subseq rows 0 (min space row-count))
                                    :hunks (getf window :hunks))
                              blocks))
                      (setf truncated-p t
                            hidden-hunks (+ (or (getf window :hunks) 1)
                                            (sum-window-hunks windows))
                            windows nil)))))
    (values (nreverse blocks) truncated-p hidden-hunks)))

(defun file-diff-presentation-update (path old-content new-content
                                      &key old-known-p added removed)
  (let* ((old-known (if old-known-p t (stringp old-content)))
         (new-lines (split-file-lines new-content))
         (old-lines (and old-known
                         (split-file-lines (or old-content "")))))
    (multiple-value-bind (start end computed-added computed-removed)
        (if old-known
            (line-change-summary old-lines new-lines)
            (values nil nil nil nil))
      (declare (ignore start end))
      (let ((base (append
                   (list :path (namestring (pathname path)))
                   (list :old-known-p old-known)
                   (when (or added computed-added)
                     (list :added (or added computed-added)))
                   (when (or removed computed-removed)
                     (list :removed (or removed computed-removed))))))
        (if old-known
            (multiple-value-bind (hunks truncated-p hidden-hunks)
                (diff-presentation-hunks old-lines new-lines)
              (append base
                      (list :hunks hunks)
                      (when truncated-p
                        (list :truncated-p t
                              :hidden-hunks hidden-hunks
                              :notice "Diff truncated; re-read the file for full context."))))
            (append base
                    (list :truncated-p t
                          :notice "Diff omitted because previous content was not retained; re-read the file for current content.")))))))

(defun compact-file-change-detail (path old-content new-content
                                   &key old-known-p characters created-p
                                     overwritten-p added removed repair status
                                     candidate-id classification dry-run-p)
  (let* ((old-known (if old-known-p t (stringp old-content)))
         (new-lines (split-file-lines new-content))
         (old-lines (and old-known
                         (split-file-lines (or old-content "")))))
    (multiple-value-bind (start end computed-added computed-removed)
        (if old-known
            (line-change-summary old-lines new-lines)
            (values nil nil nil nil))
      (let ((added-count (or added computed-added))
            (removed-count (or removed computed-removed)))
        (append
         (list :path (namestring (pathname path)))
         (when characters
           (list :characters characters))
         (when (not (null created-p))
           (list :created-p (and created-p t)))
         (when (not (null overwritten-p))
           (list :overwritten-p (and overwritten-p t)))
         (when status
           (list :status status))
         (when dry-run-p
           (list :dry-run-p t))
         (when candidate-id
           (list :candidate-id candidate-id))
         (when added-count
           (list :added added-count))
         (when removed-count
           (list :removed removed-count))
         (when (and start end added-count removed-count)
           (list :changed-ranges
                 (compact-changed-ranges start end added-count removed-count)))
         (when (stringp old-content)
           (list :old-sha256 (file-content-sha256 old-content)))
         (list :new-sha256 (file-content-sha256 new-content))
         (when repair
           (list :repair repair))
         (when classification
           (list :classification classification)))))))

(defun final-edit-change (old-content new-content)
  "Return NEW-LINES, REGIONS, ADDED, and REMOVED for content-level writes."
  (let ((old-lines (split-file-lines old-content))
        (new-lines (split-file-lines new-content)))
    (multiple-value-bind (start end added removed)
        (line-change-summary old-lines new-lines)
      (declare (ignore end))
      (values new-lines
              (when (plusp added) (list (cons start added)))
              added
              removed))))

(defun patch-edit-range (regions)
  "Inclusive changed range in patched coordinates from apply-patch-ops regions."
  (when regions
    (loop with start = nil
          with end = nil
          for (line . count) in regions
          when (plusp count)
            do (setf start (if start (min start line) line)
                     end (if end (max end (+ line count -1)) (+ line count -1)))
          finally (return (and start (cons start end))))))

(defun range-subset-p (inner outer &key (slop 0))
  (and inner outer
       (>= (car inner) (- (car outer) slop))
       (<= (cdr inner) (+ (cdr outer) slop))))

(defun classify-repair (old-content patched-content repaired-content regions)
  "Conservative auto-accept classifier for an in-memory repair. Form-count
stability is trusted only for diagnostics-free source, so a source-invalid
original is never auto-safe."
  (flet ((range-details (range)
                        (when range
                          (list :start (car range) :end (cdr range)))))
    (let* ((patched-lines (split-file-lines patched-content))
           (repaired-lines (split-file-lines repaired-content))
           (repair-range (changed-line-range patched-lines repaired-lines))
           (edit-range (patch-edit-range regions))
           (old-forms (and (source-syntax-valid-p old-content)
                           (source-form-count old-content)))
           (new-forms (source-form-count repaired-content))
           (inside-p (or (null repair-range)
                         (range-subset-p repair-range edit-range)))
           (same-forms-p (and old-forms (= old-forms new-forms))))
      (list :safe-p (and inside-p same-forms-p)
            :repair-range (range-details repair-range)
            :edit-range (range-details edit-range)
            :old-top-level-forms old-forms
            :new-top-level-forms new-forms
            :reason (cond
                      ((not inside-p) :repair-touched-outside-edit-range)
                      ((not same-forms-p) :source-form-count-changed)
                      (t :repair-local-and-form-count-stable))))))

(defun repair-preview-diff (patched repaired path)
  (with-output-to-string (out)
    (unified-diff out (coerce (split-file-lines patched) 'list)
                  (coerce (split-file-lines repaired) 'list)
                  :test-function #'equal
                  :from-file (format nil "~A patched" path)
                  :to-file (format nil "~A repaired" path))))

(defun render-edit-success-block (path added removed lines regions &key repaired)
  (with-output-to-string (out)
    (format out "~:[Edited~;Edited (auto-repaired)~] ~A (+~D -~D)"
            repaired path added removed)
    (dolist (region regions)
      (when (plusp (cdr region))
        (terpri out)
        (write-string (render-anchored-lines
                       lines
                       (car region)
                       (+ (car region) (cdr region) -1))
                      out)))))

(defun edit-syntax-report (content)
  "First Common Lisp source-syntax diagnostic in CONTENT as \"line L col C: MSG\"."
  (let ((diagnostics (source-syntax-diagnostics content)))
    (when diagnostics
      (let ((diagnostic (first diagnostics)))
        (multiple-value-bind (line col)
            (source-position content (cst-diagnostic-start diagnostic))
          (format nil "line ~D col ~D: ~A" line col
                  (cst-diagnostic-message diagnostic)))))))

(defun build-edit-decision (protocol edit repair-policy)
  (declare (ignore protocol))
  (multiple-value-bind (patched-lines regions added removed)
      (apply-patch-ops (file-edit-old-lines edit) (file-edit-ops edit))
    (let* ((path (file-edit-path edit))
           (old-content (file-edit-old-content edit))
           (patched-content (lines->content patched-lines))
           (cl-p (cl-source-path-p path)))
      (cond
        ((or (not cl-p)
             (source-syntax-valid-p patched-content))
         (list :status :write
               :path path
               :old old-content
               :new patched-content
               :new-lines patched-lines
               :regions regions
               :added added
               :removed removed
               :repair nil))
        ((not (delimiter-imbalanced-p patched-content))
         (error "Patch result for ~A is not valid Common Lisp source (~A). No files were written. Replace a complete form/range or use edit-sexp."
                path (edit-syntax-report patched-content)))
        ((eq repair-policy :reject)
         (error "Patch result for ~A is unbalanced. No files were written. Replace a complete Lisp form/range, use edit-sexp, or rerun with repair=preview or repair=safe."
                path))
        (t
         (let ((repaired-content (repair patched-content)))
           (unless (and (string/= repaired-content patched-content)
                        (source-syntax-valid-p repaired-content))
             (error "Patch result for ~A is unbalanced and could not be repaired into valid Common Lisp source. No files were written."
                    path))
           (let* ((classification (classify-repair old-content
                                                   patched-content
                                                   repaired-content
                                                   regions))
                  (safe-p (getf classification :safe-p))
                  (accept-p (and (eq repair-policy :safe) safe-p))
                  (diff (repair-preview-diff patched-content repaired-content path)))
             (if accept-p
                 (multiple-value-bind (repaired-lines final-regions final-added final-removed)
                     (final-edit-change old-content repaired-content)
                   (list :status :write
                         :path path
                         :old old-content
                         :new repaired-content
                         :new-lines repaired-lines
                         :regions final-regions
                         :added final-added
                         :removed final-removed
                         :repair :accepted
                         :classification classification
                         :repair-diff diff))
                 (multiple-value-bind (repaired-start repaired-end
                                       repaired-added repaired-removed)
                     (line-change-summary (split-file-lines old-content)
                                          (split-file-lines repaired-content))
                   (let* ((canonical-path (canonical-file-namestring path))
                          (base-hash (file-content-sha256 old-content))
                          (patched-hash (file-content-sha256 patched-content))
                          (repaired-hash (file-content-sha256 repaired-content))
                          (id (make-repair-candidate-id canonical-path base-hash
                                                        patched-hash repaired-hash))
                          (candidate (list :id id
                                           :path canonical-path
                                           :display-path path
                                           :base-hash base-hash
                                           :patched-hash patched-hash
                                           :repaired-hash repaired-hash
                                           :repaired repaired-content
                                           :classification classification
                                           :diff diff
                                           :timestamp (get-universal-time))))
                     (list :status :preview
                           :path path
                           :candidate-id id
                           :candidate candidate
                           :base-hash base-hash
                           :patched-hash patched-hash
                           :repaired-hash repaired-hash
                           :added repaired-added
                           :removed repaired-removed
                           :changed-ranges
                           (compact-changed-ranges repaired-start repaired-end
                                                   repaired-added repaired-removed)
                           :classification classification
                           :repair-diff diff
                           :presentation-update
                           (file-diff-presentation-update
                            path old-content repaired-content
                            :added repaired-added
                            :removed repaired-removed))))))))))))

(defun render-repair-preview (preview)
  (with-output-to-string (out)
    (format out "Repair preview for ~A (~A). No files were written.~%Candidate: ~A~%"
            (getf preview :path)
            (if (getf (getf preview :classification) :safe-p) "safe" "unsafe")
            (getf preview :candidate-id))
    (format out "Accept with: {\"accept_repair\":\"~A\",\"path\":\"~A\"}~%"
            (getf preview :candidate-id)
            (getf preview :path))
    (write-string (getf preview :repair-diff) out)))

(defun run-accept-repair-tool (parameters context)
  (let* ((candidate-id (tool-parameter parameters :accept_repair))
         (path-param (tool-parameter parameters :path)))
    (unless path-param
      (error "edit accept_repair requires :path so file/edit authority is path-gated."))
    (let* ((protocol (active-protocol context))
           (table (repair-candidates protocol))
           (candidate (gethash candidate-id table)))
      (unless candidate
        (error "No pending edit repair candidate ~S." candidate-id))
      (let ((canonical-path (canonical-file-namestring path-param)))
        (unless (string= canonical-path (getf candidate :path))
          (error "Repair candidate ~A targets ~A, not ~A."
                 candidate-id (getf candidate :path) canonical-path))
        (let* ((current (read-file-string canonical-path))
               (current-hash (file-content-sha256 current))
               (repaired (getf candidate :repaired)))
          (unless (string= current-hash (getf candidate :base-hash))
            (error "Repair candidate ~A is stale because ~A changed after preview. Re-read and resend the edit."
                   candidate-id canonical-path))
          (unless (string= (file-content-sha256 repaired)
                           (getf candidate :repaired-hash))
            (error "Repair candidate ~A failed its content hash check." candidate-id))
          (when (and (cl-source-path-p (getf candidate :display-path))
                     (not (source-syntax-valid-p repaired)))
            (error "Repair candidate ~A no longer produces valid Common Lisp source; the write was refused. Re-read and resend the edit."
                   candidate-id))
          (let* ((old-lines (split-file-lines current))
                 (new-lines (split-file-lines repaired)))
            (multiple-value-bind (start end added removed)
                (line-change-summary old-lines new-lines)
              (write-file-string canonical-path repaired)
              (remember-read protocol canonical-path new-lines)
              (remhash candidate-id table)
              (let ((regions (when (plusp added) (list (cons start added)))))
                (tool-text-result
                 (with-output-to-string (out)
                   (format out "Accepted repair ~A for ~A (+~D -~D)"
                           candidate-id canonical-path added removed)
                   (when (and regions (<= start end))
                     (terpri out)
                     (write-string (render-anchored-lines new-lines start end) out)))
                 :details (list :files
                                (list (compact-file-change-detail
                                       canonical-path current repaired
                                       :added added
                                       :removed removed
                                       :status :repair-accepted
                                       :repair :accepted
                                       :candidate-id candidate-id)))
                 :presentation
                 (result-diff
                  :updates (list (file-diff-presentation-update
                                  canonical-path current repaired
                                  :added added
                                  :removed removed))))))))))))

(defun run-transactional-edit-tool (parameters context)
  (let* ((input (required-tool-parameter parameters :input))
         (repair-policy (parse-repair-policy (tool-parameter parameters :repair)))
         (sections (parse-hashline-patch input))
         (protocol (active-protocol context)))
    (multiple-value-bind (edits problems)
        (collect-file-edits protocol sections)
      (let ((all-problems
              (append problems
                      (loop for edit in edits
                            append (validate-file-edit edit)))))
        (when all-problems
          (error "~{~A~^~%~}" all-problems)))
      (let* ((decisions (loop for edit in edits
                              collect (build-edit-decision protocol edit repair-policy)))
             (previews (remove-if-not (lambda (decision)
                                        (eq :preview (getf decision :status)))
                                      decisions)))
        (if previews
            (progn
              (dolist (preview previews)
                (store-repair-candidate protocol (getf preview :candidate)))
              (tool-text-result
               (format nil "~{~A~^~%~}"
                       (mapcar #'render-repair-preview previews))
               :details (list :repair-previews
                              (mapcar (lambda (preview)
                                        (list :path (getf preview :path)
                                              :status :repair-preview
                                              :candidate-id (getf preview :candidate-id)
                                              :base-sha256 (getf preview :base-hash)
                                              :patched-sha256 (getf preview :patched-hash)
                                              :repaired-sha256 (getf preview :repaired-hash)
                                              :added (getf preview :added)
                                              :removed (getf preview :removed)
                                              :changed-ranges
                                              (getf preview :changed-ranges)
                                              :classification
                                              (getf preview :classification)))
                                      previews))
               :presentation
               (result-diff
                :updates (mapcar (lambda (preview)
                                   (getf preview :presentation-update))
                                 previews))))
            (let ((blocks '())
                  (files '())
                  (updates '()))
              (dolist (decision decisions)
                (let ((path (getf decision :path))
                      (new-content (getf decision :new))
                      (new-lines (getf decision :new-lines)))
                  (write-file-string path new-content)
                  (remember-read protocol path new-lines)
                  (push (compact-file-change-detail
                         path
                         (getf decision :old)
                         new-content
                         :added (getf decision :added)
                         :removed (getf decision :removed)
                         :repair (getf decision :repair)
                         :classification (getf decision :classification))
                        files)
                  (push (file-diff-presentation-update
                         path (getf decision :old) new-content
                         :added (getf decision :added)
                         :removed (getf decision :removed))
                        updates)
                  (push (render-edit-success-block path
                                                   (getf decision :added)
                                                   (getf decision :removed)
                                                   new-lines
                                                   (getf decision :regions)
                                                   :repaired (eq (getf decision :repair)
                                                                 :accepted))
                        blocks)))
              (tool-text-result
               (format nil "~{~A~^~%~}" (nreverse blocks))
               :details (list :files (nreverse files))
               :presentation (result-diff :updates (nreverse updates)))))))))

(defun run-edit-tool (tool parameters context &key call-id on-update)
  "Apply or accept a hashline edit transaction. Invalid, unsafe, or stale edits
leave all target files untouched until an explicit candidate is accepted."
  (declare (ignore tool call-id on-update))
  (let ((input (tool-parameter parameters :input))
        (accept (tool-parameter parameters :accept_repair)))
    (cond
      ((and input accept)
       (error "edit accepts either :input or :accept_repair, not both."))
      (accept
       (run-accept-repair-tool parameters context))
      (input
       (run-transactional-edit-tool parameters context))
      (t
       (error "edit requires either :input or :accept_repair.")))))

(defun edit-patch-path-constraints (parameters)
  "Path-prefix constraints for a hashline patch or repair candidate acceptance."
  (cond
    ((tool-parameter parameters :input)
     (handler-case
         (mapcar (lambda (section) (path-prefix-constraint (car section)))
                 (parse-hashline-patch (tool-parameter parameters :input)))
       (error () '())))
    ((tool-parameter parameters :accept_repair)
     (let ((path (tool-parameter parameters :path)))
       (if path
           (list (path-prefix-constraint path))
           (list (path-prefix-constraint "/__kli_missing_accept_repair_path__")))))
    (t '())))

(register-coordinate-deriver :edit-patch-paths #'edit-patch-path-constraints)
