(in-package #:kli/runtime/isolated)

;;; Lift a connected MCP server into a uniform kli manifest. The wrapper is one
;;; lifecycle effect: on install it connects, then tags and installs the
;;; server's tools and resources as live contributions; on retract it
;;; disconnects, reaping the child. Each lifted tool carries a per-server
;;; capability so the in-image guard mediates every call. Server notifications
;;; reach kli as events through the connect context.

(defun lifted-server-id (id)
  "Normalize a lifted server's extension id, defaulting an absent id."
  (normalize-extension-id (or id :isolated-server)))

(defun lifted-server-capability (id &optional capability)
  "The capability that gates a lifted server's tools: CAPABILITY when supplied,
else a per-server keyword derived from the normalized ID. The single source for
both the lift and any consent flow that reports the gate."
  (or capability
      (intern (concatenate 'string "ISOLATED/" (symbol-name id)) :keyword)))

(defun %lift-install-tools (client protocol extension-id capability context
                            tool-coordinates)
  "Install each server tool as a contribution under EXTENSION-ID, stamping a
per-tool lattice :coordinate so invoke-tool mediates each call against the
caller's grant. The coordinate is TOOL-COORDINATES' entry for the tool name --
a list of (NAME DESCRIPTOR) -- when the installer supplies one, else the
name-derived per-tool atom. Coarse authority over the server is the enumerated
set of those atoms (lifted-server-grant); the server CAPABILITY is reported by
the consent flow, not stamped per tool."
  (declare (ignore capability))
  (dolist (tool (mcp-list-tools client))
    (let ((coordinate (or (second (assoc (tool-name tool) tool-coordinates
                                         :test #'string=))
                          (list :atom (kli/ext:lifted-tool-atom
                                       extension-id (tool-name tool))))))
      (setf (tool-metadata tool)
            (list* :coordinate coordinate (tool-metadata tool))))
    (let ((contribution (make-tool-contribution :name (tool-name tool)
                                                :tool tool
                                                :source extension-id)))
      (setf (contribution-extension contribution) extension-id)
      (install-contribution protocol contribution context))))

(defun %lift-install-resources (client protocol extension-id context)
  "Install each server resource as a contribution under EXTENSION-ID whose
reader reads the resource back over CLIENT."
  (dolist (entry (mcp-list-resources client))
    (let* ((uri (getf entry :uri))
           (resource (make-resource
                      :uri uri
                      :name (getf entry :name)
                      :mime-type (getf entry :mime-type)
                      :reader (let ((u uri))
                                (lambda () (mcp-read-resource client u)))))
           (contribution (make-resource-contribution :resource resource
                                                     :source extension-id)))
      (setf (contribution-extension contribution) extension-id)
      (install-contribution protocol contribution context))))

(defun lift-mcp-server (command &key arguments directory environment id
                                     capability client-name client-version
                                     timeout tool-coordinates)
  "Return a manifest thunk lifting the MCP server reached by COMMAND into a kli
extension. The manifest is one lifecycle effect: install connects the server
and installs its tools (each under a per-tool lattice coordinate) and resources;
retract disconnects. ID names the extension; CAPABILITY defaults to a per-server
keyword the consent flow reports. TOOL-COORDINATES optionally overrides a tool's
coordinate by name -- a list of (NAME DESCRIPTOR) -- letting the installer
classify or argument-constrain a tool at trust time."
  (let* ((extension-id (lifted-server-id id))
         (capability (lifted-server-capability extension-id capability)))
    (lambda ()
      (make-extension
       :id extension-id
       :source extension-id
       :contributions
       (list
        (make-effect-contribution
         :name :lifecycle
         :source extension-id
         :installer
         (lambda (protocol contribution context)
           ;; Gate the spawn itself: no manifest may start the subprocess
           ;; unless the installing subject's grant covers it.
           (kli/ext:require-capability :extension/spawn-process)
           (let ((client (apply #'mcp-connect command
                                :arguments arguments
                                :directory directory
                                :environment environment
                                :context context
                                (append
                                 (when client-name
                                   (list :client-name client-name))
                                 (when client-version
                                   (list :client-version client-version))
                                 (when timeout
                                   (list :timeout timeout))))))
             (unless client
               (error "Failed to connect MCP server: ~S" command))
             (let ((extension-id (contribution-extension contribution)))
               (%lift-install-tools client protocol extension-id
                                    capability context tool-coordinates)
               (%lift-install-resources client protocol extension-id context))
             client))
         :retractor
         (lambda (protocol contribution context)
           (declare (ignore protocol context))
           (mcp-disconnect (contribution-state contribution)))))))))
