# Shared option declarations for the configured-image producer, consumed by
# the module surfaces that wrap mkConfiguredKli.
{ lib }:

let
  inherit (lib) mkOption types;
in
{
  extensions = mkOption {
    type = types.listOf types.package;
    default = [ ];
    description = ''
      Extensions to compile into the image. Each is a buildLisp library
      carrying passthru.name (its id) and passthru.manifestSymbol (the
      package-qualified manifest variable).
    '';
  };

  settings = mkOption {
    type = types.attrsOf types.anything;
    default = { };
    description = "Settings baked into the image's configuration resource.";
  };

  registries = mkOption {
    type = types.listOf types.anything;
    default = [ ];
    description = "Registry entries baked into the image's configuration resource.";
  };

  blessedNativeLibs = mkOption {
    type = types.listOf types.package;
    default = [ ];
    description = "Native libraries placed on the image's library search path.";
  };

  dataDir = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = ''
      Project-local data directory. When null, the image resolves its data
      directory from the runtime environment.
    '';
  };

  sandbox = mkOption {
    default = null;
    type = types.nullOr (types.submodule {
      options = {
        network = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Keep network access. Set false to unshare the network namespace;
            this also cuts the model API, so it is usable only with a local
            model. Per-destination filtering is the operator's proxy job.
          '';
        };
        nestedUserns = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Set when the wrapped process runs inside an outer unprivileged user
            namespace whose /proc is masked (a systemd-nspawn / OCI container).
            There the kernel refuses a fresh procfs mount (mount_too_revealing),
            so the sandbox binds the container's existing masked /proc instead of
            mounting a new instance, and skips the PID-namespace unshare: the
            outer container already isolates PIDs, and a bound /proc cannot
            reflect a fresh PID namespace. Binding the masked /proc keeps the
            container's /proc hardening (kcore, kallsyms, sysrq-trigger, ...)
            inside the sandbox, where a fresh proc mount would re-expose it.
          '';
        };
        writablePaths = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Extra paths bound writable, beyond the working directory.";
        };
        denyRead = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Paths hidden from the confined process: a file reads as empty, a
            directory as empty. The at-rest mask for secrets such as
            credential files, covering shell, file tools, and eval uniformly.
          '';
        };
        denyEnv = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Environment variable names unset before the confined process
            starts. The in-environment twin of denyRead: it keeps named
            secrets out of every tool's environment at the process boundary,
            covering shell, file tools, and eval uniformly.
          '';
        };
      };
    });
    description = ''
      Whole-process confinement via bwrap. Null leaves the binary unwrapped.
      An attribute set wraps the entrypoint: the working directory is the
      writable root, the rest of the filesystem read-only, process and IPC
      namespaces unshared.
    '';
  };
}
