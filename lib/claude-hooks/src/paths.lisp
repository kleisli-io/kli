;;;; claude-hooks - Session Path Helpers
;;;;
;;;; Construct .claude/sessions/{id}/ paths from hook input hash-tables.
;;;; Uses depot detection for consistent paths regardless of CWD changes.
;;;; Also provides normalize-file-path for consistent path comparison.

(in-package :claude-hooks)

;;; --------------------------------------------------------------------------
;;; File path normalization
;;; --------------------------------------------------------------------------

(defun normalize-file-path (path)
  "Canonicalize a file path for consistent string= comparison.
   Resolves symlinks, relative paths, trailing slashes, and double slashes
   via CL:TRUENAME when the file exists. For non-existent files, normalizes
   syntactically via MERGE-PATHNAMES.
   Returns NIL for NIL or empty-string input."
  (when (and path (stringp path) (plusp (length path)))
    (handler-case
        (namestring (truename path))
      (error ()
        (let* ((absolute-p (char= (char path 0) #\/))
               (pn (if absolute-p
                       (pathname path)
                       (merge-pathnames path (uiop:getcwd)))))
          (namestring pn))))))

(defun session-dir (input)
  "Session directory with depot-aware detection.

   Priority:
   1. Existing session in current depot (fast path)
   2. Existing session in sibling depots (cross-depot traversal)
   3. Depot root (new session or session not found)
   4. Hook cwd fallback (non-depot context)

   Returns path like \"/depot/.claude/sessions/session-id\", or NIL
   if session_id missing from INPUT."
  (let* ((sid (jref input "session_id"))
         (hook-cwd (jref input "cwd"))
         (base (when sid
                 (or (find-session-depot sid)
                     (find-depot-root)
                     hook-cwd))))
    (when (and base sid)
      (format nil "~a.claude/sessions/~a"
              (namestring (uiop:ensure-directory-pathname base))
              sid))))

(defun session-file (input &rest path-parts)
  "Build a path under the session directory from hook INPUT.
PATH-PARTS are joined with /. Returns NIL if session-dir returns NIL.

Examples:
  (session-file input \"playbook\" \"activated.jsonl\")
  (session-file input \"ace\" \"state.json\")"
  (let ((dir (session-dir input)))
    (when dir
      (format nil "~a/~{~a~^/~}" dir path-parts))))

(defun ensure-session-dir (input &rest subdirs)
  "Ensure session directory (with optional SUBDIRS) exists on disk.
Returns the directory path, or NIL if input lacks cwd/session_id.

Example:
  (ensure-session-dir input \"playbook\") → creates and returns path"
  (let ((dir (if subdirs
                 (apply #'session-file input subdirs)
                 (session-dir input))))
    (when dir
      (ensure-directories-exist (make-pathname :directory (pathname-directory
                                                            (parse-namestring (format nil "~a/" dir)))))
      dir)))
