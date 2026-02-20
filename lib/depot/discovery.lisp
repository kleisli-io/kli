;;;; discovery.lisp - Depot discovery and resolution utilities
;;;;
;;;; Provides functions to detect depot roots, world roots, and resolve
;;;; depot names to filesystem paths. Extracted from swank-mcp/security.lisp
;;;; for reuse by task library and other components.

(in-package #:depot)

;;; ==========================================================================
;;; Utility Functions
;;; ==========================================================================

(defun strip-trailing-slash (path)
  "Remove trailing slash from path string if present."
  (if (and (> (length path) 1)
           (char= (char path (1- (length path))) #\/))
      (subseq path 0 (1- (length path)))
      path))

;;; ==========================================================================
;;; Git Detection
;;; ==========================================================================

(defun find-git-root ()
  "Find the git root directory from current working directory.
   Returns the git root path as a string, or NIL if not in a git repository."
  (let ((result (ignore-errors
                  (uiop:run-program '("git" "rev-parse" "--show-toplevel")
                                    :output :string
                                    :ignore-error-status t))))
    (when (and result (not (string= result "")))
      (string-trim '(#\Newline #\Return #\Space) result))))

;;; ==========================================================================
;;; Depot Detection
;;; ==========================================================================

(defun find-depot-root ()
  "Find depot root via DEPOT_ROOT env, .depot marker, or git fallback.

   Detection algorithm (mirrors nixos/tools/depot-root):
   1. Check DEPOT_ROOT environment variable (explicit override)
   2. Walk up from CWD looking for .depot marker file
   3. Fallback: git root if it has default.nix

   Returns the depot root path as a string, or NIL if not found."
  ;; 1. Check DEPOT_ROOT environment variable override
  (let ((depot-root-env (uiop:getenv "DEPOT_ROOT")))
    (when (and depot-root-env
               (not (string= depot-root-env ""))
               (probe-file depot-root-env))
      (return-from find-depot-root
        (namestring (truename depot-root-env)))))
  ;; 2. Walk up from CWD looking for .depot marker
  (loop with current = (uiop:getcwd)
        while (and current
                   (not (equal (namestring current) "/")))
        do (let ((marker (merge-pathnames ".depot" current)))
             (when (probe-file marker)
               (return-from find-depot-root
                 (namestring (truename current)))))
           (setf current (uiop:pathname-parent-directory-pathname current)))
  ;; 3. Fallback: git root with default.nix validation
  (let ((git-root (find-git-root)))
    (when git-root
      (let ((default-nix (merge-pathnames "default.nix" (pathname git-root))))
        (when (probe-file default-nix)
          git-root)))))

;;; ==========================================================================
;;; World Detection (Depot-of-Depots)
;;; ==========================================================================

(defun find-world-root ()
  "Find the world root for depot-of-depots structure.

   Checks if the current depot is part of a depot-of-depots ecosystem by
   looking for depot-graph.nix in the parent directory of the depot root.

   Returns:
     - World root path (string, no trailing slash) if in depot-of-depots structure
     - NIL if this is a standalone depot"
  (let ((depot-root (find-depot-root)))
    (when depot-root
      (let* ((depot-path (pathname depot-root))
             (parent (uiop:pathname-parent-directory-pathname depot-path))
             (depot-graph (merge-pathnames "depot-graph.nix" parent)))
        (when (probe-file depot-graph)
          (strip-trailing-slash (namestring (truename parent))))))))

(defun coordination-root ()
  "Return the root directory for cross-session coordination files.

   Resolution order:
   1. KLI_COORDINATION_DIR env (explicit override for standalone)
   2. World root (depot-of-depots: all depots share one .claude/)
   3. Depot root (standalone depot)
   4. Git root (bare git repo)

   This is used for:
     - .claude/active-pids/  (live Claude process tracking)
     - .claude/sessions/     (session state files)

   The key insight: Claude processes span depots (one PPID regardless of
   which depot is active), so coordination files belong at world level."
  (or (let ((override (uiop:getenv "KLI_COORDINATION_DIR")))
        (when (and override (not (string= override "")))
          override))
      (find-world-root)
      (find-depot-root)
      (find-git-root)))

;;; ==========================================================================
;;; Path-Aware Variants (for hooks that receive cwd as parameter)
;;; ==========================================================================

(defun find-depot-root-from (path)
  "Find depot root by walking up from PATH (not process CWD).

   Like find-depot-root but starts from specified PATH instead of getcwd.
   Used by hooks which receive cwd as a parameter.

   Returns depot root path as string, or NIL if not found."
  (when path
    (loop with current = (uiop:ensure-directory-pathname path)
          while (and current
                     (not (equal (namestring current) "/"))
                     (not (equal current (user-homedir-pathname))))
          do (let ((marker (merge-pathnames ".depot" current)))
               (when (probe-file marker)
                 (return-from find-depot-root-from
                   (strip-trailing-slash (namestring (truename current))))))
             (setf current (uiop:pathname-parent-directory-pathname current)))))

(defun find-world-root-from (path)
  "Find world root by walking up from PATH looking for depot-graph.nix.

   Like find-world-root but starts from specified PATH.
   Returns world root path as string, or NIL if not in depot-of-depots."
  (when path
    (loop with current = (uiop:ensure-directory-pathname path)
          while (and current
                     (not (equal (namestring current) "/"))
                     (not (equal current (user-homedir-pathname))))
          do (let ((depot-graph (merge-pathnames "depot-graph.nix" current)))
               (when (probe-file depot-graph)
                 (return-from find-world-root-from
                   (strip-trailing-slash (namestring (truename current))))))
             (setf current (uiop:pathname-parent-directory-pathname current)))))

(defun coordination-root-from (path)
  "Return coordination root by walking up from PATH.

   Prefers world root (depot-graph.nix) over depot root (.depot).
   This is the path-aware variant for hooks.

   Returns:
     - World root if found (depot-of-depots structure)
     - Depot root if found (standalone depot)
     - NIL if neither found"
  (or (find-world-root-from path)
      (find-depot-root-from path)))

;;; ==========================================================================
;;; Depot Validation and Resolution
;;; ==========================================================================

(defun valid-depot-name-p (name)
  "Validate depot name for security.
   Only alphanumeric, hyphen, underscore allowed.
   Must start with alphabetic character.

   Returns T if valid, NIL otherwise."
  (and (stringp name)
       (> (length name) 0)
       (alpha-char-p (char name 0))
       (every (lambda (c)
                (or (alphanumericp c)
                    (char= c #\-)
                    (char= c #\_)))
              name)))

(defun resolve-depot-root (depot)
  "Resolve depot to filesystem path.

   If DEPOT is NIL, returns current depot root via find-depot-root.
   If DEPOT specified, requires world root and returns <world>/<depot>.

   Arguments:
     depot - Depot name string or NIL for current depot

   Returns:
     Depot root path as string

   Signals error if:
     - DEPOT is invalid name (path traversal, special chars)
     - World root not found (when depot specified)
     - Depot directory not found or missing default.nix"
  (if (null depot)
      (find-depot-root)
      (let ((world-root (find-world-root)))
        (unless world-root
          (error "Cannot resolve depot ~A: not in depot-of-depots structure (no depot-graph.nix found)" depot))
        (unless (valid-depot-name-p depot)
          (error "Invalid depot name: ~A (must start with letter, contain only alphanumeric, hyphen, underscore)" depot))
        (let ((depot-path (format nil "~A/~A" world-root depot)))
          (unless (probe-file (merge-pathnames "default.nix"
                                               (pathname (format nil "~A/" depot-path))))
            (error "Depot not found: ~A (expected default.nix at ~A)" depot depot-path))
          depot-path))))

;;; ==========================================================================
;;; Sibling Depot Discovery
;;; ==========================================================================

(defun list-sibling-depots ()
  "List all sibling depots in the world.

   Scans the world root directory for subdirectories that have a default.nix
   file (indicating they are depots).

   Returns:
     List of depot name strings, or NIL if not in depot-of-depots structure."
  (let ((world-root (find-world-root)))
    (when world-root
      (let ((depots '()))
        (dolist (subdir (uiop:subdirectories (pathname (format nil "~A/" world-root))))
          (let* ((name (car (last (pathname-directory subdir))))
                 (default-nix (merge-pathnames "default.nix" subdir)))
            (when (and (valid-depot-name-p name)
                       (probe-file default-nix))
              (push name depots))))
        (sort depots #'string<)))))

(defun depot-has-tasks-p (depot)
  "Check if a depot has a tasks directory (.kli/tasks/ or ace/tasks/).

   Arguments:
     depot - Depot name string

   Returns:
     T if depot has a tasks directory, NIL otherwise."
  (let ((depot-root (resolve-depot-root depot)))
    (when depot-root
      (let ((depot-path (pathname (format nil "~A/" depot-root))))
        (or (uiop:directory-exists-p (merge-pathnames ".kli/tasks/" depot-path))
            (uiop:directory-exists-p (merge-pathnames "ace/tasks/" depot-path)))))))

;;; ==========================================================================
;;; Task-Specific Depot Utilities
;;; ==========================================================================

(defun depot-tasks-root (depot-name)
  "Get the tasks directory for a depot, or NIL if it doesn't exist.

   Checks .kli/tasks/ first (kli standalone), then ace/tasks/ (depot-of-depots).

   Arguments:
     depot-name - Depot name string (or NIL for current depot)

   Returns:
     Path to tasks directory as string, or NIL if not found."
  (let ((depot-root (resolve-depot-root depot-name)))
    (when depot-root
      (let ((depot-path (pathname (format nil "~A/" depot-root))))
        ;; Prefer .kli/tasks/ (kli standalone mode)
        (let ((kli-tasks (merge-pathnames ".kli/tasks/" depot-path)))
          (when (uiop:directory-exists-p kli-tasks)
            (return-from depot-tasks-root (namestring kli-tasks))))
        ;; Fallback: ace/tasks/ (depot-of-depots mode)
        (let ((ace-tasks (merge-pathnames "ace/tasks/" depot-path)))
          (when (uiop:directory-exists-p ace-tasks)
            (namestring ace-tasks)))))))

(defun depot-meta-root (depot-name)
  "Get the metadata directory for a depot (.kli/ or ace/), or NIL.

   This is the parent of the tasks directory — where playbook files,
   dashboard state, and other per-depot metadata live.

   Checks .kli/ first (kli standalone), then ace/ (depot-of-depots).
   Derived from depot-tasks-root to ensure consistent resolution.

   Arguments:
     depot-name - Depot name string (or NIL for current depot)

   Returns:
     Path to metadata directory as string, or NIL if not found."
  (let ((tasks-root (depot-tasks-root depot-name)))
    (when tasks-root
      (namestring (truename (merge-pathnames "../" (pathname tasks-root)))))))

(defun list-depots-with-tasks ()
  "List all depots/projects that have tasks directories.

   In depot-of-depots mode: scans sibling depots for ace/tasks/ or .kli/tasks/.
   In standalone mode: returns singleton list with git root as default project.

   Returns:
     List of plists with :depot and :tasks-root keys."
  (let ((siblings (list-sibling-depots)))
    (if siblings
        ;; Depot-of-depots mode
        (loop for depot in siblings
              for tasks-root = (depot-tasks-root depot)
              when tasks-root
              collect (list :depot depot :tasks-root tasks-root))
        ;; Standalone mode: check git root for .kli/tasks/
        (let ((git-root (find-git-root)))
          (when git-root
            (let ((kli-tasks (merge-pathnames ".kli/tasks/"
                                              (pathname (format nil "~A/" git-root)))))
              (when (uiop:directory-exists-p kli-tasks)
                (list (list :depot "default" :tasks-root (namestring kli-tasks))))))))))
