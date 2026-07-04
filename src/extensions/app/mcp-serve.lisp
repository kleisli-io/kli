(in-package #:kli/app)

;;; Serve one extension's tool surface to an external MCP client over stdio. The
;;; named extension's tools, and only those, are exposed; their capabilities
;;; bound the serve subject. One process serves one client; cwd selects the
;;; project. stdout is the JSON-RPC channel, so every diagnostic goes to stderr.

(defparameter +mcp-serve-mode-id+ :mcp-serve
  "Mode the serve loop binds its lone agent session under, so a host session id
is available for the tools that record one.")

(defun bind-mcp-serve-session (context)
  "Bind one agent session so writes record a real session id. Best-effort: a
profile without the agent-session service, or a boot with no model, leaves the
session unbound and the bridge still serves."
  (let ((service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (when service
      (handler-case
          (reset-agent-session service +mcp-serve-mode-id+ context)
        (error (condition)
          (format *error-output* "kli mcp-serve: session bind skipped (~A)~%"
                  condition))))))

;;; The MCP server is provider-agnostic: it serves whatever prompt and resource
;;; descriptors a surface carries. The discovery and md->MCP mapping live here,
;;; in the app layer, which loads after the prompts/skills/config packages -- the
;;; headless serve profile omits their extension installs, but their pure
;;; discovery functions are compiled in and usable directly.

(defun mcp-serve-resource-directory (key)
  "The bundled resource directory for KEY, or NIL. resource-root errors on a
missing key, so an extension that declares a root it did not bundle contributes
nothing rather than aborting the serve."
  (and key (ignore-errors (buildlisp/resources:resource-root key))))

(defun mcp-prompt-tail (arguments)
  "The raw argument string for a prompts/get call: the lone declared 'arguments'
value when present, else empty. expand-prompt-template parses it into $1/$@/
$ARGUMENTS itself."
  (or (and (hash-table-p arguments) (gethash "arguments" arguments)) ""))

(defun prompt-template->prompt (template)
  (let* ((hint (kli/prompts:prompt-template-argument-hint template))
         (arguments (when (and hint (plusp (length hint)))
                      (list (list :name "arguments" :description hint :required nil)))))
    (list :name (kli/prompts:prompt-template-name template)
          :description (kli/prompts:prompt-template-description template)
          :arguments arguments
          :expand (lambda (call-arguments)
                    (kli/prompts:expand-prompt-template
                     template (mcp-prompt-tail call-arguments))))))

(defun prompt-template->resource (template scheme)
  (let ((name (kli/prompts:prompt-template-name template)))
    (list :uri (format nil "~A://prompts/~A" scheme name)
          :name name
          :description (kli/prompts:prompt-template-description template)
          :mime "text/markdown"
          :text (kli/prompts:prompt-template-body template))))

(defun skill->prompt (skill)
  (list :name (kli/skills:skill-name skill)
        :description (kli/skills:skill-description skill)
        :arguments nil
        :expand (lambda (call-arguments)
                  (declare (ignore call-arguments))
                  (kli/skills:read-skill-body skill))))

(defun skill->resource (skill scheme)
  (let ((name (kli/skills:skill-name skill)))
    (list :uri (format nil "~A://skills/~A" scheme name)
          :name name
          :description (kli/skills:skill-description skill)
          :mime "text/markdown"
          :text (kli/skills:read-skill-body skill))))

(defun mcp-serve-corpus (args)
  "Discover the prompts and skills bundled by the ARGS extensions and map each to
both an MCP prompt and a markdown resource, scoped to ARGS for parity with the
tool surface. Returns (values PROMPTS RESOURCES)."
  (let ((prompts '())
        (resources '()))
    (dolist (id (normalize-extension-id args))
      (let* ((entry (cdr (assoc id kli/config:*extension-resource-roots*
                                :test #'equal)))
             (scheme (string-downcase (symbol-name id)))
             (prompt-dir (mcp-serve-resource-directory (getf entry :prompts)))
             (skill-dir (mcp-serve-resource-directory (getf entry :skills))))
        (when prompt-dir
          (dolist (template (kli/prompts:discover-prompt-templates (list prompt-dir)))
            (push (prompt-template->prompt template) prompts)
            (push (prompt-template->resource template scheme) resources)))
        (when skill-dir
          (dolist (skill (kli/skills:discover-skills
                          (list (list :directory skill-dir :root-files-p nil))))
            (push (skill->prompt skill) prompts)
            (push (skill->resource skill scheme) resources)))))
    (values (nreverse prompts) (nreverse resources))))

(defun run-mcp-serve (&optional args)
  "Serve the extension(s) named in ARGS to an MCP client over stdin/stdout. Boot
the headless profile plus user extensions, scope the surface to those extensions'
tools, prompts, and resources, and run under a subject carrying exactly their
capabilities."
  (with-fatal-error-handler ()
    (with-headless-io ()
      (unless args
        (format *error-output* "kli mcp-serve: name at least one extension to expose~%")
        (uiop:quit 2))
      (let* ((settings (load-settings))
             (context (main :profile :headless :settings settings)))
        (boot-user-extensions context)
        (report-boot-diagnostics context)
        (bind-mcp-serve-session context)
        (multiple-value-bind (prompts resources) (mcp-serve-corpus args)
          (let ((surface (kli/runtime/mcp-server:extension-surface
                          (active-protocol context) args
                          :prompts prompts :resources resources)))
            (unless (kli/runtime/mcp-server:surface-tools surface)
              (format *error-output*
                      "kli mcp-serve: no tools exposed for ~{~A~^, ~} -- is it installed?~%"
                      args)
              (uiop:quit 3))
            (let ((kli/runtime/mcp-server:*server-version* (current-version))
                  (kli/ext:*call-subject* (kli/runtime/mcp-server:surface-subject surface)))
              (kli/runtime/mcp-server:serve-stream
               surface context *standard-input* *standard-output*))))
        (uiop:quit 0)))))
