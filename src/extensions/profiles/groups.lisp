(in-package #:kli/profiles)

(defparameter *baseline-extension-manifests*
  '(kli/object:*standard-object-extension-manifest*
    kli/event:*events-extension-manifest*
    kli/interaction/commands:*commands-extension-manifest*
    kli/config:*config-extension-manifest*
    kli/session/log:*session-log-extension-manifest*
    kli/context/lens:*context-lens-extension-manifest*
    kli/auth/core:*auth-extension-manifest*
    kli/auth/oauth:*oauth-extension-manifest*
    kli/model/registry:*model-registry-extension-manifest*
    kli/model/runtime:*model-runtime-extension-manifest*
    kli/agent/loop:*agent-loop-extension-manifest*
    kli/agent/session:*agent-session-extension-manifest*
    kli/tools/eval:*eval-tool-extension-manifest*
    kli/tools/eval:*eval-continue-tool-extension-manifest*
    kli/tools/eval:*eval-abort-tool-extension-manifest*
    kli/tools/eval:*recompile-rerun-tool-extension-manifest*
    kli/tools/bash:*bash-tool-extension-manifest*
    kli/tools/bash:*persistent-shell-extension-manifest*
    kli/tools/bash:*bash-jobs-extension-manifest*
    kli/tools/filesystem:*read-tool-extension-manifest*
    kli/tools/filesystem:*write-tool-extension-manifest*
    kli/tools/filesystem:*edit-tool-extension-manifest*
    kli/tools/filesystem:*find-tool-extension-manifest*
    kli/tools/filesystem:*search-tool-extension-manifest*
    kli/tools/filesystem:*filesystem-anchor-lifecycle-extension-manifest*
    kli/tools/lisp:*edit-sexp-extension-manifest*
    kli/runtime/control:*control-extension-manifest*
    kli/runtime/snapshot:*snapshot-extension-manifest*
    kli/runtime/history:*history-extension-manifest*
    kli/runtime/introspection:*introspection-extension-manifest*
    kli/tools/introspect:*list-objects-tool-extension-manifest*
    kli/tools/introspect:*context-summary-tool-extension-manifest*
    kli/tools/introspect:*inspect-tool-extension-manifest*
    kli/tools/trace:*trace-tool-extension-manifest*
    kli/tools/trace:*untrace-tool-extension-manifest*
    kli/tools/trace:*trace-read-tool-extension-manifest*
    kli/output-spill:*output-spill-extension-manifest*))

(defparameter *model-provider-extension-manifests*
  '(kli/model/providers/codex:*codex-provider-extension-manifest*
    kli/model/providers/openai:*openai-provider-extension-manifest*
    kli/model/providers/anthropic:*anthropic-provider-extension-manifest*
    kli/model/providers/compatible:*compatible-provider-extension-manifest*))

(defparameter *tui-app-extension-manifests*
  '(kli/tui/style:*tui-style-extension-manifest*
    kli/tui/keymap:*tui-keymap-extension-manifest*
    kli/tui/status:*tui-status-extension-manifest*
    kli/tui/views:*tui-views-extension-manifest*
    kli/tui/input:*tui-input-extension-manifest*
    kli/tui/editor:*tui-editor-extension-manifest*
    kli/tui/terminal:*tui-terminal-extension-manifest*
    kli/tui/markdown:*tui-markdown-extension-manifest*
    kli/tui/transcript:*tui-transcript-extension-manifest*
    kli/tui/app:*tui-app-extension-manifest*
    kli/interaction/basic-commands:*basic-commands-extension-manifest*
    kli/interaction/session-commands:*session-commands-extension-manifest*
    kli/tools/eval:*eval-command-extension-manifest*
    kli/tools/bash:*bash-command-extension-manifest*
    kli/tools/bash:*persistent-shell-command-extension-manifest*
    kli/model/commands:*model-commands-extension-manifest*
    kli/config:*config-commands-extension-manifest*
    kli/prompts:*prompt-templates-extension-manifest*
    kli/context/files:*context-files-extension-manifest*
    kli/skills:*skills-extension-manifest*
    kli/tui/completion:*tui-completion-extension-manifest*
    kli/config/wiring:*settings-wiring-extension-manifest*
    kli/observability:*observability-extension-manifest*))

(defparameter *nix-declared-extension-manifests* nil
  "Manifest symbols a nix-configured image declares present-at-boot. Empty in
plain core -- only a configured image's baked boot shim populates it, and each
profile body splices it in right after the baseline so the declared extensions
boot as baseline children. A list of package-qualified manifest symbols, the
same shape as *baseline-extension-manifests*.")

(defparameter *nix-declared-baseline-ids* nil
  "String ids of the extensions a nix-configured image baked in as
present-at-boot. Empty in plain core; a configured image bakes the set in as
compiled-in Lisp (never a resource path, so it survives a relocatable-bundle
move). The runtime install-set keys its collision guard on this set -- a baked
id is shadowed on install and refused on uninstall, since the image owns its
presence.")

;;; Surface on #:kli/ext at load time (it's upstream, can't name these statically)
;;; so external extensions compose profiles through one author vocabulary.
(eval-when (:load-toplevel :execute)
  (import '(*baseline-extension-manifests*
            *model-provider-extension-manifests*)
          '#:kli/ext)
  (export '(*baseline-extension-manifests*
            *model-provider-extension-manifests*)
          '#:kli/ext))
