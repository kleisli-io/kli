;;;; claude-hooks - Depot Root Detection
;;;;
;;;; Find depot root for consistent session state paths.
;;;; Handles single-depot and depot-of-depots (world) structures.

(in-package :claude-hooks)

;;; ---------------------------------------------------------------------------
;;; Git Root Detection
;;; ---------------------------------------------------------------------------

(defun find-git-root ()
  "Find git repository root via git rev-parse.
   Returns absolute path string or NIL if not in a git repo."
  (let ((result (ignore-errors
                  (uiop:run-program '("git" "rev-parse" "--show-toplevel")
                                    :output :string
                                    :ignore-error-status t))))
    (when (and result (not (string= result "")))
      (string-trim '(#\Newline #\Return #\Space) result))))

;;; ---------------------------------------------------------------------------
;;; Depot Root Detection
;;; ---------------------------------------------------------------------------

(defun find-depot-root ()
  "Find depot root via DEPOT_ROOT env, .depot marker, or git fallback.

   Detection priority:
   1. DEPOT_ROOT environment variable (explicit override)
   2. Walk up from CWD looking for .depot marker file
   3. Git root if it contains default.nix

   Returns absolute path string with trailing slash, or NIL."
  ;; 1. Check DEPOT_ROOT environment variable
  (let ((env (uiop:getenv "DEPOT_ROOT")))
    (when (and env (not (string= env "")) (probe-file env))
      (return-from find-depot-root
        (namestring (truename (uiop:ensure-directory-pathname env))))))
  ;; 2. Walk up from CWD looking for .depot marker
  (loop with current = (uiop:getcwd)
        while (and current (not (equal (namestring current) "/")))
        do (let ((marker (merge-pathnames ".depot" current)))
             (when (probe-file marker)
               (return-from find-depot-root
                 (namestring (truename current)))))
           (setf current (uiop:pathname-parent-directory-pathname current)))
  ;; 3. Git fallback with default.nix validation
  (let ((git-root (find-git-root)))
    (when git-root
      (let* ((git-dir (uiop:ensure-directory-pathname git-root))
             (default-nix (merge-pathnames "default.nix" git-dir)))
        (when (probe-file default-nix)
          (namestring (truename git-dir)))))))

;;; ---------------------------------------------------------------------------
;;; World Root Detection (depot-of-depots)
;;; ---------------------------------------------------------------------------

(defun find-world-root ()
  "Find the world root for depot-of-depots structure.

   Checks if current depot is part of a depot-of-depots ecosystem by
   looking for depot-graph.nix in the parent directory.

   Returns world root path string, or NIL if standalone depot."
  (let ((depot-root (find-depot-root)))
    (when depot-root
      (let* ((depot-path (uiop:ensure-directory-pathname depot-root))
             (parent (uiop:pathname-parent-directory-pathname depot-path))
             (depot-graph (merge-pathnames "depot-graph.nix" parent)))
        (when (probe-file depot-graph)
          (namestring (truename parent)))))))

;;; ---------------------------------------------------------------------------
;;; Session Depot Detection (cross-depot aware)
;;; ---------------------------------------------------------------------------

(defun find-session-depot (session-id)
  "Find depot root where SESSION-ID's state directory exists.

   Search strategy (optimized for common case):
   1. Check current depot (fast, handles 99% of cases)
   2. Search sibling depots in world (handles cross-depot traversal)

   Returns depot root path string, or NIL if session not found."
  (when (and session-id (plusp (length session-id)))
    ;; 1. Check current depot first (fast path)
    (let ((current-depot (find-depot-root)))
      (when current-depot
        (let ((session-dir (merge-pathnames
                            (format nil ".claude/sessions/~a/" session-id)
                            (uiop:ensure-directory-pathname current-depot))))
          (when (uiop:directory-exists-p session-dir)
            (return-from find-session-depot current-depot)))))
    ;; 2. Search sibling depots in world (slow path)
    (let ((world-root (find-world-root)))
      (when world-root
        (let ((world-path (uiop:ensure-directory-pathname world-root)))
          (dolist (depot-dir (uiop:subdirectories world-path))
            (let* ((depot-root (namestring (truename depot-dir)))
                   (session-dir (merge-pathnames
                                 (format nil ".claude/sessions/~a/" session-id)
                                 (uiop:ensure-directory-pathname depot-root))))
              (when (uiop:directory-exists-p session-dir)
                (return-from find-session-depot depot-root)))))))))
