(in-package #:kli/context/files)

(defparameter +context-file-candidates+
  '("AGENTS.md" "AGENTS.MD" "CLAUDE.md" "CLAUDE.MD")
  "Per-directory context file names in precedence order, first existing wins.")

(defparameter *context-file-byte-limit* (* 2 1024 1024)
  "Largest context or override file read into the system prompt, in bytes.
Deliberately generous - the ceiling guards against pathological files (a
checked-in binary, a runaway log), not against large prompts, so it sits
at or beyond what a model context can ingest.")

(defun read-file-safely (path)
  "PATH's content as a string, NIL when the read fails or the file is over
*CONTEXT-FILE-BYTE-LIMIT* - an oversized file is treated as absent."
  (handler-case
      (let ((bytes (with-open-file (stream path
                                           :element-type '(unsigned-byte 8))
                     (file-length stream))))
        (and (<= bytes *context-file-byte-limit*)
             (uiop:read-file-string path)))
    (error () nil)))

(defun context-file-in-directory (dir)
  "The first readable candidate in DIR as (:path namestring :content string),
NIL when none. A failed read falls through to the next candidate."
  (loop for name in +context-file-candidates+
        for existing = (uiop:file-exists-p (merge-pathnames name dir))
        for content = (and existing (read-file-safely existing))
        when content
          return (list :path (namestring existing) :content content)))

(defun collect-context-files (&key global-dir start)
  "Context files with the GLOBAL-DIR file first, then the repo-root-bounded
ancestor walk outermost first with the start directory last, deduplicated
by path."
  (let ((files '())
        (seen '()))
    (flet ((collect (dir)
             (let ((file (context-file-in-directory dir)))
               (when (and file
                          (not (member (getf file :path) seen
                                       :test #'string=)))
                 (push (getf file :path) seen)
                 (push file files)))))
      (when global-dir
        (collect global-dir))
      (dolist (dir (reverse (apply #'project-ancestors
                                   (and start (list :start start)))))
        (collect dir)))
    (nreverse files)))

(defun render-project-context (files)
  "FILES rendered as the project context prompt section, empty string when
none."
  (if files
      (format nil "~2%# Project Context~2%Project-specific instructions ~
                   and guidelines:~2%~{## ~A~2%~A~2%~}"
              (loop for file in files
                    collect (getf file :path)
                    collect (getf file :content)))
      ""))

(defun %discover-config-file (dirs name)
  "NAME's content from the project config directory first then the global
one, first found wins. Blank or unreadable files are treated as absent."
  (destructuring-bind (global &optional project) dirs
    (loop for dir in (list project global)
          for existing = (and dir (uiop:file-exists-p
                                   (merge-pathnames name dir)))
          for content = (and existing (read-file-safely existing))
          when (and content (not (blank-string-p content)))
            return content)))

(defun discover-system-prompt (dirs)
  "SYSTEM.md content replacing the base system prompt, NIL when absent."
  (%discover-config-file dirs "SYSTEM.md"))

(defun discover-append-system-prompt (dirs)
  "APPEND_SYSTEM.md content appended to the system prompt, NIL when absent."
  (%discover-config-file dirs "APPEND_SYSTEM.md"))

(defun install-context-files (protocol contribution context)
  "Read context and override files once, then install their blocks as named
system-prompt layers keyed by this contribution: a :transform that lets a SYSTEM.md
override replace the running prompt, and an :append carrying the APPEND_SYSTEM.md
and project-context blocks that rides after it. Returns restoration state, or NIL
when no session service is registered."
  (declare (ignore protocol))
  (let ((config (require-capability-provider (active-protocol context)
                                             :config
                                             :contract :config/v1)))
    (provider-call config :register-resource-kind context :context-files "")
    (let* ((dirs (provider-call config :resource-paths context
                                :context-files :existing-only nil))
           (system-content (discover-system-prompt dirs))
           (append-content (discover-append-system-prompt dirs))
           (append-block (if append-content
                             (format nil "~2%~A" append-content)
                             ""))
           (context-block (render-project-context
                           (collect-context-files :global-dir (first dirs))))
           (service (find-live-object (context-registry context)
                                      :agent-session-service)))
      (when service
        (let ((system-label (list (contribution-name contribution) :system))
              (append-label (list (contribution-name contribution) :append)))
          ;; SYSTEM.md replaces the running prompt; absent, it passes through.
          (add-system-prompt-layer
           service system-label
           (lambda (running) (or system-content running))
           :kind :transform)
          (add-system-prompt-layer
           service append-label
           (lambda () (concatenate 'string append-block context-block))
           :kind :append)
          (list :transform (list :service service
                                 :labels (list system-label append-label))))))))

(defun uninstall-context-files (protocol contribution context)
  (declare (ignore protocol context))
  (let ((transform (getf (contribution-state contribution) :transform)))
    (when transform
      (dolist (label (getf transform :labels))
        (remove-system-prompt-layer (getf transform :service) label)))))

(defun refresh-context-files (protocol contribution context)
  "Refresh context-files prompt layers after boot snapshot reuse.

Runtime SYSTEM.md, APPEND_SYSTEM.md, and AGENTS.md are install-time reads owned
by this contribution. Refresh removes only the previously installed prompt layers
recorded in this contribution's state, then rebuilds them from the current
runtime config/project files without changing extension topology."
  (uninstall-context-files protocol contribution context)
  (setf (contribution-state contribution)
        (install-context-files protocol contribution context))
  contribution)
