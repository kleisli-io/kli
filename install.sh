#!/bin/sh
# kli installer — https://kli.kleisli.io
#
# Usage:
#   curl -fsSL https://kli.kleisli.io | sh
#
# Environment variables:
#   KLI_VERSION        Pin to a specific version (e.g., v0.1.0). Default: latest.
#   KLI_INSTALL_DIR    Override install directory. Default: ~/.local
#   KLI_DOWNLOAD_BASE  Override the download origin (private mirror, air-gapped
#                      copy, or a local server). Default: the GitHub release.

set -eu

REPO="kleisli-io/kli"
GITHUB_API="https://api.github.com/repos/${REPO}/releases/latest"
GITHUB_DL="https://github.com/${REPO}/releases/download"

# --- Platform detection ---

detect_os() {
  case "$(uname -s)" in
    Linux*)  echo "linux" ;;
    Darwin*) echo "darwin" ;;
    *)
      printf "Error: unsupported operating system '%s'\n" "$(uname -s)" >&2
      printf "kli supports Linux and macOS.\n" >&2
      exit 1
      ;;
  esac
}

detect_arch() {
  case "$(uname -m)" in
    x86_64|amd64)   echo "x86_64" ;;
    aarch64|arm64)   echo "aarch64" ;;
    *)
      printf "Error: unsupported architecture '%s'\n" "$(uname -m)" >&2
      printf "kli supports x86_64 and aarch64 (arm64).\n" >&2
      exit 1
      ;;
  esac
}

validate_platform() {
  _os="$1"
  _arch="$2"

  case "${_os}-${_arch}" in
    linux-x86_64|linux-aarch64|darwin-aarch64|darwin-x86_64) ;;
    *)
      printf "Error: unsupported platform '%s-%s'\n" "${_os}" "${_arch}" >&2
      exit 1
      ;;
  esac
}

# --- Version resolution ---

get_latest_version() {
  _response=$(curl --proto '=https' --tlsv1.2 -fsSL "${GITHUB_API}" 2>/dev/null) || {
    printf "Error: failed to fetch latest release from GitHub.\n" >&2
    printf "Check your network connection or set KLI_VERSION manually.\n" >&2
    exit 1
  }

  # Extract tag_name without jq — match "tag_name": "v..."
  _version=$(printf '%s' "${_response}" | grep -o '"tag_name"[[:space:]]*:[[:space:]]*"[^"]*"' | head -1 | grep -o '"v[^"]*"' | tr -d '"')

  if [ -z "${_version}" ]; then
    printf "Error: could not determine latest version from GitHub API.\n" >&2
    printf "Set KLI_VERSION manually (e.g., KLI_VERSION=v0.1.0).\n" >&2
    exit 1
  fi

  echo "${_version}"
}

# --- Install ---

get_install_dir() {
  if [ -n "${KLI_INSTALL_DIR:-}" ]; then
    echo "${KLI_INSTALL_DIR}"
  elif [ "$(id -u)" = "0" ]; then
    echo "/usr/local"
  else
    echo "${HOME}/.local"
  fi
}

main() {
  printf "kli installer\n\n"

  _os=$(detect_os)
  _arch=$(detect_arch)
  validate_platform "${_os}" "${_arch}"

  _artifact="kli-${_os}-${_arch}.tar.gz"

  # Resolve the download origin. KLI_DOWNLOAD_BASE overrides it wholesale (a
  # private mirror, an air-gapped copy, or a local server); otherwise it is the
  # GitHub release for the resolved version.
  if [ -n "${KLI_DOWNLOAD_BASE:-}" ]; then
    _base="${KLI_DOWNLOAD_BASE}"
    printf "Source:   %s\n" "${_base}"
  else
    _version="${KLI_VERSION:-}"
    if [ -z "${_version}" ]; then
      printf "Fetching latest version... "
      _version=$(get_latest_version)
      printf "%s\n" "${_version}"
    else
      printf "Version: %s (pinned)\n" "${_version}"
    fi
    _base="${GITHUB_DL}/${_version}"
  fi

  _url="${_base}/${_artifact}"
  _install_dir=$(get_install_dir)
  _lib_dir="${_install_dir}/lib/kli"
  _bin_dir="${_install_dir}/bin"
  _wrapper="${_bin_dir}/kli"

  printf "Platform: %s-%s\n" "${_os}" "${_arch}"
  printf "Install:  %s\n\n" "${_lib_dir}"

  # Ensure directories exist
  mkdir -p "${_lib_dir}" "${_bin_dir}"

  # Download tarball
  printf "Downloading %s... " "${_artifact}"
  _tmpfile=$(mktemp)
  trap 'rm -f "${_tmpfile}"' EXIT

  if ! curl --proto '=https' --tlsv1.2 -fsSL "${_url}" -o "${_tmpfile}"; then
    printf "failed\n" >&2
    printf "\nError: download failed for %s\n" "${_url}" >&2
    if [ -z "${KLI_DOWNLOAD_BASE:-}" ]; then
      printf "Check that version '%s' exists at https://github.com/%s/releases\n" "${_version}" "${REPO}" >&2
    fi
    exit 1
  fi
  printf "ok\n"

  # Verify checksum if available
  _checksums_url="${_base}/checksums.txt"
  if _checksums=$(curl --proto '=https' --tlsv1.2 -fsSL "${_checksums_url}" 2>/dev/null); then
    _expected=$(printf '%s' "${_checksums}" | grep "${_artifact}" | cut -d' ' -f1)
    if [ -n "${_expected}" ]; then
      printf "Verifying checksum... "
      if command -v sha256sum >/dev/null 2>&1; then
        _actual=$(sha256sum "${_tmpfile}" | cut -d' ' -f1)
      elif command -v shasum >/dev/null 2>&1; then
        _actual=$(shasum -a 256 "${_tmpfile}" | cut -d' ' -f1)
      else
        printf "skipped (no sha256sum or shasum)\n"
        _actual="${_expected}"
      fi

      if [ "${_actual}" != "${_expected}" ]; then
        printf "FAILED\n" >&2
        printf "\nError: checksum mismatch\n" >&2
        printf "  expected: %s\n" "${_expected}" >&2
        printf "  actual:   %s\n" "${_actual}" >&2
        exit 1
      fi
      printf "ok\n"
    fi
  fi

  # Clean previous install (files may be read-only from Nix store copies)
  if [ -d "${_lib_dir}/bin" ] || [ -d "${_lib_dir}/lib" ] || [ -d "${_lib_dir}/share" ]; then
    chmod -R u+w "${_lib_dir}" 2>/dev/null || true
    rm -rf "${_lib_dir:?}/bin" "${_lib_dir:?}/lib" "${_lib_dir:?}/share" "${_lib_dir:?}/VERSION"
  fi

  # Extract tarball. The release tarball's top-level is kli-${os}-${arch}/;
  # strip it so the payload lands directly under lib/kli (the layout the
  # relocation wrapper and self-updater both resolve against).
  printf "Extracting... "
  tar -xzf "${_tmpfile}" -C "${_lib_dir}" --strip-components=1
  chmod +x "${_lib_dir}/bin/kli"
  printf "ok\n"
  rm -f "${_tmpfile}"
  trap - EXIT

  # The bundled bin/kli launcher self-locates from its own path and sets
  # KLI_DATA_DIR plus the dynamic-library search path before exec'ing the image,
  # so the installed wrapper only has to invoke it -- by absolute path, never a
  # symlink: the launcher roots its lookups off its own directory and does not
  # resolve symlinks, so a symlinked entry would mis-root.
  cat > "${_wrapper}" <<WRAPPER
#!/bin/sh
exec "${_lib_dir}/bin/kli" "\$@"
WRAPPER
  chmod +x "${_wrapper}"

  # Verify
  printf "\n"
  if "${_wrapper}" version 2>/dev/null; then
    printf "\nkli installed successfully.\n"
    printf "  Binary: %s/bin/kli\n" "${_lib_dir}"
    printf "  Data:   %s/share/kli/\n" "${_lib_dir}"
    printf "  Wrapper: %s\n" "${_wrapper}"
  else
    printf "kli installed to %s\n" "${_wrapper}"
  fi

  # PATH check
  case ":${PATH}:" in
    *":${_bin_dir}:"*) ;;
    *)
      printf "\nAdd kli to your PATH:\n"
      printf "  export PATH=\"%s:\$PATH\"\n" "${_bin_dir}"
      printf "\nTo make this permanent, add the line above to ~/.bashrc or ~/.zshrc\n"
      ;;
  esac

  printf "\nNext: run 'kli' in any project to launch.\n"
}

main
