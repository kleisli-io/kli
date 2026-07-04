(in-package #:kli/app)

(defun dispatch-command (argv &optional (interactive (terminal-input-tty-p)))
  "Route the first ARGV token to a command by walking +cli-grammar+. Returns
(values command args) where COMMAND is a grammar :id (:version, :update, :docs,
:mcp-serve, :help, :print, :print-authority, ...) and ARGS is the remaining
arguments for that command. A token that names a command dispatches the same way
regardless of INTERACTIVE. With no command -- or a UI flag or unrecognized token
-- the route follows the input channel: an interactive terminal falls through to
:tui with ARGV intact so the UI still sees its own flags, while piped stdin
routes to :print for one-shot use. INTERACTIVE defaults to whether fd 0 is a
terminal; callers and tests may pass it explicitly."
  (let* ((head (first argv))
         (command (and head (cli-command-for-token head))))
    (if command
        (values (cli-command-id command) (rest argv))
        (if interactive (values :tui argv) (values :print argv)))))

(defun print-version (&optional (stream *standard-output*))
  "Write the running image's version line."
  (format stream "kli ~A~%" (current-version)))

(defun print-usage (&optional (stream *standard-output*))
  "Write the command-line usage summary."
  (format stream "~&~{~A~%~}"
          '("Usage: kli [command] [flags]"
            ""
            "Commands:"
            "  version    print the version and build id"
            "  update     update kli to the latest release"
            "  docs       print kli documentation from docs.kleisli.io"
            "  mcp-serve  serve a named extension's tools to an MCP client over stdio"
            "  install    install an extension from a url, pinned to its git-tree-sha1"
            "  help       show this message"
            ""
            "Flags:"
            "  -p, --print        run one prompt and print the reply, then exit"
            "  --print-authority  print the authority a run would hold, then exit"
            "  --read-only        in print mode, deny file write, edit, and bash"
            "  --no-bash          in print mode, deny bash"
            "  --grants N         in print mode, run under named grant set N"
            "  --json             with --print-authority, emit one JSON object"
            ""
            "With no command kli launches the terminal UI; flags such as"
            "--profile and -c pass through to it. When stdin is piped, kli runs"
            "in print mode, reading the prompt from the argument or stdin.")))

(defun dispatch-main ()
  "Program toplevel: route the first command-line argument to the version,
update, or help command, defaulting to the terminal UI."
  (with-fatal-error-handler ()
    (multiple-value-bind (command args)
        (dispatch-command (uiop:command-line-arguments))
      (case command
        (:version (print-version) (uiop:quit 0))
        (:update (uiop:quit (run-self-update args)))
        (:docs (uiop:quit (run-docs args)))
        (:mcp-serve (run-mcp-serve args))
        (:install (uiop:quit (run-install args)))
        (:print (run-print args))
        (:print-authority (uiop:quit (run-print-authority args)))
        (:complete (uiop:quit (run-complete args)))
        (:completion (uiop:quit (run-completion args)))
        (:help (print-usage) (uiop:quit 0))
        (t (run-tui-main))))))
