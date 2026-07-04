(in-package #:kli/config)

(defparameter +global-config-subdir+ "kli/"
  "XDG subdirectory kli owns, shared with credentials and user extensions.")

(defparameter +project-config-dirname+ ".kli/"
  "Per-project configuration directory name.")

(defvar *global-config-dir* nil
  "Override for the global config directory. NIL resolves to the XDG default.")

(defvar *project-start-directory* nil
  "Override for where the project walk-up starts. NIL resolves to the cwd.")

(defun global-config-dir ()
  (or *global-config-dir*
      (uiop:xdg-config-home +global-config-subdir+)))

(defun project-start-directory ()
  (or *project-start-directory* (uiop:getcwd)))

(defun parent-directory (dir)
  "DIR's parent, or NIL at the filesystem root."
  (let ((parent (uiop:pathname-parent-directory-pathname dir)))
    (unless (equal parent dir) parent)))

(defun directory-ancestors (start)
  "START and every parent up to the filesystem root, nearest first. Pure
pathname arithmetic, no directory needs to exist."
  (loop for dir = (uiop:ensure-directory-pathname start)
          then (parent-directory dir)
        while dir
        collect dir))

(defun repo-root-p (dir)
  "A directory holding .git in either directory or file form (worktrees)."
  (let ((marker (merge-pathnames ".git" dir)))
    (and (or (uiop:directory-exists-p marker)
             (uiop:file-exists-p marker))
         t)))

(defun project-ancestors (&key (start (project-start-directory)))
  "Directories from START up to the repo root inclusive, nearest first. The
full ancestor chain when no repo root bounds the walk."
  (loop for dir in (directory-ancestors start)
        collect dir
        until (repo-root-p dir)))

(defun locate-dominating (name &key (start (project-start-directory)))
  "The nearest ancestor's NAME within the repo-root-bounded walk, as the
existing file or directory path. NIL when no ancestor holds NAME."
  (loop for dir in (project-ancestors :start start)
        for probe = (merge-pathnames name dir)
        for found = (or (uiop:directory-exists-p probe)
                        (uiop:file-exists-p probe))
        when found return found))

(defun project-config-dir (&key (start (project-start-directory)))
  "The nearest .kli directory at or above START, bounded by the repo root.
NIL outside any configured project."
  (locate-dominating +project-config-dirname+ :start start))

(defun expand-config-path (value &key directory)
  "Absolute pathname for the settings string VALUE, expanding a leading ~
against the home directory and a relative path against the working directory.
With DIRECTORY, coerce VALUE to a directory pathname. The single resolver for
every path-valued setting, so all consumers expand the same string the same
way."
  (let ((expanded (cond ((string= value "~") (user-homedir-pathname))
                        ((and (>= (length value) 2)
                              (string= "~/" value :end2 2))
                         (merge-pathnames (subseq value 2)
                                          (user-homedir-pathname)))
                        (t value))))
    (uiop:ensure-absolute-pathname
     (if directory
         (uiop:ensure-directory-pathname expanded)
         (pathname expanded))
     (uiop:getcwd))))
