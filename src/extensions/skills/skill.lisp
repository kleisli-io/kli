(in-package #:kli/skills)

(defparameter +skill-name-limit+ 64
  "Longest skill name the Agent Skills spec allows.")

(defparameter +skill-description-limit+ 1024
  "Longest skill description the Agent Skills spec allows.")

(defstruct skill
  name
  description
  path
  base-dir
  (disable-model-invocation-p nil))

(defun validate-skill-name (name parent-dir-name)
  "Warnings for NAME against the Agent Skills spec, empty when valid."
  (let ((warnings '()))
    (flet ((warn-about (control &rest arguments)
             (push (apply #'format nil control arguments) warnings)))
      (unless (equal name parent-dir-name)
        (warn-about "name ~S does not match parent directory ~S"
                    name parent-dir-name))
      (when (> (length name) +skill-name-limit+)
        (warn-about "name exceeds ~D characters (~D)"
                    +skill-name-limit+ (length name)))
      (unless (cl-ppcre:scan "^[a-z0-9-]+$" name)
        (warn-about "name contains invalid characters (must be lowercase ~
a-z, 0-9, hyphens only)"))
      (when (or (uiop:string-prefix-p "-" name)
                (uiop:string-suffix-p name "-"))
        (warn-about "name must not start or end with a hyphen"))
      (when (search "--" name)
        (warn-about "name must not contain consecutive hyphens")))
    (nreverse warnings)))

(defun validate-skill-description (description)
  "Warnings for DESCRIPTION against the Agent Skills spec, empty when valid."
  (cond
    ((blank-string-p description)
     (list "description is required"))
    ((> (length description) +skill-description-limit+)
     (list (format nil "description exceeds ~D characters (~D)"
                   +skill-description-limit+ (length description))))
    (t '())))

(defun warning-diagnostic (message path)
  (list :type :warning :message message :path path))

(defun directory-leaf-name (dir)
  (let ((component (first (last (pathname-directory
                                 (uiop:ensure-directory-pathname dir))))))
    (if (stringp component) component "")))

(defparameter *skill-file-byte-limit* (* 2 1024 1024)
  "Largest SKILL.md the skills extension reads whole, in bytes. Skill
bodies land in the image as full strings and in prompts verbatim, so one
oversized file at discovery or invocation can exhaust the dynamic space.")

(defun skill-file-byte-length (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun assert-skill-file-within-limit (path)
  (let ((size (skill-file-byte-length path)))
    (when (> size *skill-file-byte-limit*)
      (error "~A is ~:D bytes, over the ~:D byte limit for a skill file."
             path size *skill-file-byte-limit*))))

(defun load-skill (path)
  "The skill at PATH plus a list of diagnostics. Validation is lenient and
   warns, except a missing description which drops the skill. An unreadable or
   oversized file yields NIL with one diagnostic."
  (handler-case
      (let ((frontmatter (progn
                           (assert-skill-file-within-limit path)
                           (parse-frontmatter (uiop:read-file-string path)))))
        (let* ((base-dir (uiop:pathname-directory-pathname path))
               (parent (directory-leaf-name base-dir))
               (explicit (gethash "name" frontmatter))
               (name (if (blank-string-p explicit) parent explicit))
               (description (gethash "description" frontmatter))
               (diagnostics
                 (append
                  (mapcar (lambda (message)
                            (warning-diagnostic message path))
                          (validate-skill-description description))
                  (mapcar (lambda (message)
                            (warning-diagnostic message path))
                          (validate-skill-name name parent)))))
          (if (blank-string-p description)
              (values nil diagnostics)
              (values (make-skill
                       :name name
                       :description description
                       :path path
                       :base-dir base-dir
                       :disable-model-invocation-p
                       (equal "true"
                              (gethash "disable-model-invocation"
                                       frontmatter)))
                      diagnostics))))
    (error (condition)
      (values nil
              (list (warning-diagnostic (princ-to-string condition) path))))))

(defun read-skill-body (skill)
  "The SKILL.md body read fresh, frontmatter stripped, whitespace trimmed.
Errors when the file has grown over *SKILL-FILE-BYTE-LIMIT* since
discovery, never reading it whole."
  (assert-skill-file-within-limit (skill-path skill))
  (string-trim '(#\Space #\Tab #\Newline)
               (nth-value 1 (parse-frontmatter
                             (uiop:read-file-string (skill-path skill))))))

(defun skill-block (skill &key reference tail)
  "Pi's skill block around the freshly read body. REFERENCE adds a preamble
line and TAIL appends after the closing tag."
  (format nil "<skill name=\"~A\" location=\"~A\">~%References are relative to ~A.~@[~%~A~]~%~%~A~%</skill>~@[~%~%~A~]"
          (skill-name skill)
          (namestring (skill-path skill))
          (namestring (skill-base-dir skill))
          reference
          (read-skill-body skill)
          tail))

(defun escape-xml (string)
  (with-output-to-string (out)
    (loop for char across string
          do (case char
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&apos;" out))
               (t (write-char char out))))))

(defparameter +skills-prompt-preamble+
  (list "The following skills provide specialized instructions for specific tasks."
        "Use the read tool to load a skill's file when the task matches its description."
        "When a skill file references a relative path, resolve it against the skill directory (parent of SKILL.md / dirname of the path) and use that absolute path in tool commands."))

(defun skill-advertisement-entry (skill)
  (format nil "  <skill>~%    <name>~A</name>~%    <description>~A</description>~%    <location>~A</location>~%  </skill>"
          (escape-xml (skill-name skill))
          (escape-xml (skill-description skill))
          (escape-xml (namestring (skill-path skill)))))

(defun format-skills-for-prompt (skills)
  "Pi's XML advertisement for SKILLS, empty when none are visible. Skills
marked disable-model-invocation stay out and remain command-only."
  (let ((visible (remove-if #'skill-disable-model-invocation-p skills)))
    (if (null visible)
        ""
        (format nil "~%~%~{~A~%~}~%<available_skills>~%~{~A~%~}</available_skills>"
                +skills-prompt-preamble+
                (mapcar #'skill-advertisement-entry visible)))))
