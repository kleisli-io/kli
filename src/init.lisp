;;; init.lisp — `kli init` command: configure Claude Code for current project
;;;
;;; Reads plugin data from KLI_DATA_DIR (set by Nix wrapper or install script)
;;; and writes MCP config, hooks, commands, agents, skills, and CLAUDE.md
;;; into the project directory.

(in-package #:kli)

;;; --- Data directory discovery ---

(defun data-dir ()
  "Return the KLI_DATA_DIR path as a directory pathname.
   Errors if not set or directory doesn't exist."
  (let ((dir (uiop:getenv "KLI_DATA_DIR")))
    (unless dir
      (format *error-output* "Error: KLI_DATA_DIR not set.~%")
      (format *error-output* "Install kli via the installer or set KLI_DATA_DIR manually.~%")
      (uiop:quit 1))
    (let ((path (uiop:ensure-directory-pathname dir)))
      (unless (uiop:directory-exists-p path)
        (format *error-output* "Error: KLI_DATA_DIR does not exist: ~A~%" dir)
        (uiop:quit 1))
      path)))

(defun data-file (relative-path)
  "Read a file from the data directory. Returns string content."
  (uiop:read-file-string (merge-pathnames relative-path (data-dir))))

;;; --- File writing ---

(defun ensure-parent-dirs (path)
  "Create parent directories for PATH if they don't exist."
  (ensure-directories-exist path)
  path)

(defun write-file-if-changed (path content)
  "Write CONTENT to PATH. Skip if file exists with identical content.
   Returns :wrote, :skipped, or :created."
  (ensure-parent-dirs path)
  (cond
    ((not (probe-file path))
     (write-string-to-file content path)
     :created)
    ((string= (uiop:read-file-string path) content)
     :skipped)
    (t
     (write-string-to-file content path)
     :wrote)))

(defun write-string-to-file (content path)
  "Write CONTENT string to PATH, overwriting if exists."
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string content s)))

;;; --- JSON helpers ---

(defun read-json-file (path)
  "Parse JSON file at PATH into a hash-table. Returns NIL if file missing."
  (when (probe-file path)
    (yason:parse (uiop:read-file-string path))))

(defun write-json-file (path data)
  "Write DATA hash-table as indented JSON to PATH."
  (ensure-parent-dirs path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (yason:encode data (yason:make-json-output-stream s :indent 2))
    (terpri s)))

;;; --- Hook merging ---

(defun extract-hook-command (entry)
  "Extract the command string from a hook entry hash-table.
   Entry shape: {\"hooks\": [{\"type\": \"command\", \"command\": \"...\"}], ...}"
  (let ((hooks (gethash "hooks" entry)))
    (when (and hooks (first hooks))
      (gethash "command" (first hooks)))))

(defun hook-entry-present-p (entry entries)
  "Check if an equivalent hook entry exists in ENTRIES (by command string)."
  (let ((cmd (extract-hook-command entry)))
    (and cmd (find cmd entries :test #'string= :key #'extract-hook-command))))

(defun merge-hooks (settings kli-hooks)
  "Merge KLI-HOOKS (hash-table of event→entries) into SETTINGS hash-table.
   Idempotent: duplicate entries (by command string) are skipped."
  (let ((existing-hooks (gethash "hooks" settings)))
    (unless existing-hooks
      (setf existing-hooks (make-hash-table :test 'equal))
      (setf (gethash "hooks" settings) existing-hooks))
    (maphash
     (lambda (event-name kli-entries)
       (let ((existing-entries (or (gethash event-name existing-hooks) '())))
         (dolist (entry kli-entries)
           (unless (hook-entry-present-p entry existing-entries)
             (setf existing-entries (append existing-entries (list entry)))))
         (setf (gethash event-name existing-hooks) existing-entries)))
     kli-hooks)
    settings))

(defun count-hook-entries (settings)
  "Count total hook entries across all event types."
  (let ((hooks (gethash "hooks" settings))
        (count 0))
    (when hooks
      (maphash (lambda (k v) (declare (ignore k)) (incf count (length v))) hooks))
    count))

;;; --- MCP server merging ---

(defun merge-mcp-servers (project-dir)
  "Ensure .mcp.json at PROJECT-DIR has kli MCP server entries.
   Reads template from data dir. Returns count of servers added."
  (let* ((mcp-path (merge-pathnames ".mcp.json" project-dir))
         (kli-config (yason:parse (data-file "mcp.json")))
         (kli-servers (gethash "mcpServers" kli-config))
         (added 0))
    (if (probe-file mcp-path)
        (let* ((existing (yason:parse (uiop:read-file-string mcp-path)))
               (existing-servers (or (gethash "mcpServers" existing)
                                     (make-hash-table :test 'equal))))
          (maphash (lambda (name config)
                     (unless (gethash name existing-servers)
                       (setf (gethash name existing-servers) config)
                       (incf added)))
                   kli-servers)
          (when (> added 0)
            (setf (gethash "mcpServers" existing) existing-servers)
            (write-json-file mcp-path existing)))
        ;; No existing file — create
        (progn
          (write-json-file mcp-path kli-config)
          (setf added (hash-table-count kli-servers))))
    added))

;;; --- CLAUDE.md section management ---

(defparameter *kli-section-begin* "<!-- kli:begin -->")
(defparameter *kli-section-end* "<!-- kli:end -->")

(defun write-claude-md (project-dir)
  "Write or update kli section in CLAUDE.md at project root.
   Reads template from data dir. Uses marker comments for idempotent updates.
   Returns :created, :updated, :skipped."
  (let* ((md-path (merge-pathnames "CLAUDE.md" project-dir))
         (content (data-file "CLAUDE.md"))
         (section (format nil "~A~%~A~%~A" *kli-section-begin* content *kli-section-end*)))
    (if (probe-file md-path)
        (let* ((existing (uiop:read-file-string md-path))
               (begin-pos (search *kli-section-begin* existing))
               (end-pos (search *kli-section-end* existing)))
          (cond
            ;; Section exists — check if content changed
            ((and begin-pos end-pos)
             (let* ((old-section (subseq existing begin-pos
                                         (+ end-pos (length *kli-section-end*))))
                    (new-section section))
               (if (string= old-section new-section)
                   :skipped
                   (progn
                     (write-string-to-file
                      (concatenate 'string
                                   (subseq existing 0 begin-pos)
                                   new-section
                                   (subseq existing (+ end-pos (length *kli-section-end*))))
                      md-path)
                     :updated))))
            ;; No section — append
            (t
             (with-open-file (s md-path :direction :output
                                        :if-exists :append
                                        :external-format :utf-8)
               (format s "~%~%~A~%" section))
             :updated)))
        ;; No CLAUDE.md — create
        (progn
          (write-string-to-file (format nil "~A~%" section) md-path)
          :created))))

;;; --- File discovery and copying ---

(defun list-data-files (subdir &key (namespace t))
  "List all files in a data directory subdirectory.
   Returns list of (relative-dest . data-relative-path) pairs.
   When NAMESPACE is T, files are namespaced under .claude/<subdir>/kli/.
   When NIL, files go directly under .claude/<subdir>/."
  (let* ((dir (merge-pathnames (uiop:ensure-directory-pathname subdir) (data-dir)))
         (files (uiop:directory-files dir))
         (result '()))
    (dolist (file files (nreverse result))
      (let* ((name (file-namestring file))
             (dest (if namespace
                       (format nil ".claude/~A/kli/~A" subdir name)
                       (format nil ".claude/~A/~A" subdir name)))
             (src (format nil "~A/~A" subdir name)))
        (push (cons dest src) result)))))

(defun list-skill-files ()
  "List all skill files in the data directory.
   Skills are in skills/<name>/SKILL.md format.
   Returns list of (relative-dest . data-relative-path) pairs."
  (let* ((skills-dir (merge-pathnames "skills/" (data-dir)))
         (subdirs (uiop:subdirectories skills-dir))
         (result '()))
    (dolist (subdir subdirs (nreverse result))
      (let* ((skill-name (car (last (pathname-directory subdir))))
             (skill-file (merge-pathnames "SKILL.md" subdir)))
        (when (probe-file skill-file)
          (let ((dest (format nil ".claude/skills/~A/SKILL.md" skill-name))
                (src (format nil "skills/~A/SKILL.md" skill-name)))
            (push (cons dest src) result)))))))

;;; --- Main init command ---

(defun init ()
  "Initialize kli in the current project directory.
   Writes MCP servers, hooks, commands, agents, skills, and CLAUDE.md."
  (let* ((project-dir (uiop:ensure-directory-pathname (uiop:getcwd)))
         (wrote 0)
         (skipped 0)
         (total 0))
    (format t "Initializing kli in ~A~%~%" (namestring project-dir))

    ;; 1. MCP servers (.mcp.json)
    (let ((added (merge-mcp-servers project-dir)))
      (if (> added 0)
          (progn (format t "  write  .mcp.json (~D servers)~%" added) (incf wrote))
          (format t "  skip   .mcp.json (servers present)~%"))
      (incf total))

    ;; 2. Hooks (.claude/settings.local.json)
    (let* ((settings-path (merge-pathnames ".claude/settings.local.json" project-dir))
           (kli-hooks-full (yason:parse (data-file "hooks/hooks.json")))
           (kli-hooks (gethash "hooks" kli-hooks-full))
           (settings (or (read-json-file settings-path)
                         (make-hash-table :test 'equal)))
           (before-count (count-hook-entries settings))
           (merged (merge-hooks settings kli-hooks))
           (after-count (count-hook-entries merged)))
      (if (> after-count before-count)
          (progn
            (write-json-file settings-path merged)
            (format t "  write  .claude/settings.local.json (+~D hooks)~%"
                    (- after-count before-count))
            (incf wrote))
          (format t "  skip   .claude/settings.local.json (hooks present)~%"))
      (incf total))

    ;; 3. Commands (namespaced under kli/) and agents (flat)
    (dolist (entry (append (list-data-files "commands")
                           (list-data-files "agents" :namespace nil)))
      (let* ((dest (car entry))
             (content (data-file (cdr entry)))
             (result (write-file-if-changed
                      (merge-pathnames dest project-dir)
                      content)))
        (incf total)
        (ecase result
          (:created (format t "  create ~A~%" dest) (incf wrote))
          (:wrote   (format t "  update ~A~%" dest) (incf wrote))
          (:skipped (incf skipped)))))

    ;; 4. Skills
    (dolist (entry (list-skill-files))
      (let* ((dest (car entry))
             (content (data-file (cdr entry)))
             (result (write-file-if-changed
                      (merge-pathnames dest project-dir)
                      content)))
        (incf total)
        (ecase result
          (:created (format t "  create ~A~%" dest) (incf wrote))
          (:wrote   (format t "  update ~A~%" dest) (incf wrote))
          (:skipped (incf skipped)))))

    ;; 5. CLAUDE.md at project root
    (let ((result (write-claude-md project-dir)))
      (incf total)
      (ecase result
        (:created (format t "  create CLAUDE.md~%") (incf wrote))
        (:updated (format t "  update CLAUDE.md~%") (incf wrote))
        (:skipped (incf skipped))))

    ;; Summary
    (format t "~%Done. ~D written, ~D unchanged (of ~D total).~%"
            wrote skipped total)
    (when (> wrote 0)
      (format t "~%Run `claude` to start a session with kli.~%"))))
