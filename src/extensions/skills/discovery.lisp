(in-package #:kli/skills)

(defvar *user-agents-skills-directory* nil
  "Override for the user-level agents skills directory. NIL resolves to
.agents/skills under the home directory.")

(defun user-agents-skills-directory ()
  (or *user-agents-skills-directory*
      (merge-pathnames ".agents/skills/" (user-homedir-pathname))))

(defun relative-posix-path (path root)
  (let ((path-namestring (namestring path))
        (root-namestring (namestring (uiop:ensure-directory-pathname root))))
    (if (and (< (length root-namestring) (length path-namestring))
             (string= root-namestring path-namestring
                      :end2 (length root-namestring)))
        (subseq path-namestring (length root-namestring))
        path-namestring)))

(defun sorted-subdirectories (dir)
  (handler-case
      (sort (uiop:subdirectories dir) #'string< :key #'namestring)
    (error () '())))

(defun sorted-markdown-files (dir)
  (handler-case
      (sort (uiop:directory-files dir "*.md") #'string< :key #'namestring)
    (error () '())))

(defun canonical-path-key (path)
  (handler-case
      (namestring (truename path))
    (error () (namestring path))))

(defun walk-skill-directory (dir include-root-files matcher root visited)
  "Pi's walk. A directory holding a visible SKILL.md is a skill root and
yields exactly that skill. Otherwise subdirectories recurse and, when
INCLUDE-ROOT-FILES, top-level markdown files load as individual skills.
VISITED records the truename of every directory already entered so a symlink
cycle terminates instead of recursing without bound."
  (let ((key (canonical-path-key (uiop:ensure-directory-pathname dir))))
    (when (gethash key visited)
      (return-from walk-skill-directory (values '() '())))
    (setf (gethash key visited) t))
  (add-ignore-rules matcher dir root)
  (let ((skill-file (uiop:file-exists-p (merge-pathnames "SKILL.md" dir))))
    (when (and skill-file
               (not (path-ignored-p matcher
                                    (relative-posix-path skill-file root))))
      (multiple-value-bind (skill diagnostics) (load-skill skill-file)
        (return-from walk-skill-directory
          (values (if skill (list skill) '()) diagnostics)))))
  (let ((skills '())
        (diagnostics '()))
    (dolist (subdir (sorted-subdirectories dir))
      (let ((name (directory-leaf-name subdir)))
        (unless (or (uiop:string-prefix-p "." name)
                    (equal "node_modules" name)
                    (path-ignored-p matcher
                                    (relative-posix-path
                                     (uiop:ensure-directory-pathname subdir)
                                     root)))
          (multiple-value-bind (sub-skills sub-diagnostics)
              (walk-skill-directory subdir nil matcher root visited)
            (setf skills (append skills sub-skills))
            (setf diagnostics (append diagnostics sub-diagnostics))))))
    (when include-root-files
      (dolist (file (sorted-markdown-files dir))
        (unless (or (uiop:string-prefix-p "." (pathname-name file))
                    (path-ignored-p matcher (relative-posix-path file root)))
          (multiple-value-bind (skill file-diagnostics) (load-skill file)
            (when skill (setf skills (append skills (list skill))))
            (setf diagnostics (append diagnostics file-diagnostics))))))
    (values skills diagnostics)))

(defun discover-skills-in-directory (directory &key root-files-p)
  "Skills under DIRECTORY plus diagnostics. Dot entries, node_modules, and
ignore-file matches are skipped. An absent directory yields nothing."
  (let ((root (uiop:ensure-directory-pathname directory)))
    (if (uiop:directory-exists-p root)
        (walk-skill-directory root root-files-p (make-ignore-matcher) root
                              (make-hash-table :test #'equal))
        (values '() '()))))

(defun discover-skills (entries)
  "Skills from ENTRIES of (:directory dir :root-files-p flag) plists in
order, deduplicated by name with the first occurrence winning, matching
pi. The same file reached twice through links drops silently, a name
collision keeps the earlier skill and surfaces a collision diagnostic."
  (let ((by-name (make-hash-table :test #'equal))
        (seen-paths (make-hash-table :test #'equal))
        (skills '())
        (diagnostics '())
        (collisions '()))
    (dolist (entry entries)
      (multiple-value-bind (found entry-diagnostics)
          (discover-skills-in-directory (getf entry :directory)
                                        :root-files-p
                                        (getf entry :root-files-p))
        (setf diagnostics (append diagnostics entry-diagnostics))
        (dolist (skill found)
          (let ((key (canonical-path-key (skill-path skill))))
            (unless (gethash key seen-paths)
              (if (gethash (skill-name skill) by-name)
                  (push (list :type :collision
                              :message (format nil "name ~S collision"
                                               (skill-name skill))
                              :path (skill-path skill))
                        collisions)
                  (progn
                    (setf (gethash (skill-name skill) by-name) skill)
                    (setf (gethash key seen-paths) t)
                    (push skill skills))))))))
    (values (nreverse skills)
            (append diagnostics (nreverse collisions)))))
