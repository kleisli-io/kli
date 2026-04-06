#!/bin/sh
# kli installer — https://kli.kleisli.io
#
# Usage:
#   curl -fsSL https://kli.kleisli.io/install | sh
#
# Environment variables:
#   KLI_VERSION      Pin to a specific version (e.g., v0.1.0). Default: latest.
#   KLI_INSTALL_DIR  Override install directory. Default: ~/.local

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
    linux-x86_64|darwin-aarch64) ;;
    linux-aarch64)
      printf "Error: linux-aarch64 is not yet supported.\n" >&2
      printf "See https://github.com/%s/issues for updates.\n" "${REPO}" >&2
      exit 1
      ;;
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

  _version="${KLI_VERSION:-}"
  if [ -z "${_version}" ]; then
    printf "Fetching latest version... "
    _version=$(get_latest_version)
    printf "%s\n" "${_version}"
  else
    printf "Version: %s (pinned)\n" "${_version}"
  fi

  _artifact="kli-${_os}-${_arch}.tar.gz"
  _url="${GITHUB_DL}/${_version}/${_artifact}"
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
    printf "Check that version '%s' exists at https://github.com/%s/releases\n" "${_version}" "${REPO}" >&2
    exit 1
  fi
  printf "ok\n"

  # Verify checksum if available
  _checksums_url="${GITHUB_DL}/${_version}/checksums.txt"
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
    rm -rf "${_lib_dir}/bin" "${_lib_dir}/lib" "${_lib_dir}/share"
  fi

  # Extract tarball to lib dir
  printf "Extracting... "
  tar -xzf "${_tmpfile}" -C "${_lib_dir}"
  chmod +x "${_lib_dir}/bin/kli"
  printf "ok\n"
  rm -f "${_tmpfile}"
  trap - EXIT

  # Create wrapper script (sets library path for bundled shared libs)
  cat > "${_wrapper}" <<WRAPPER
#!/bin/sh
export KLI_DATA_DIR="${_lib_dir}/share/kli"
KLI_LIB_DIR="${_lib_dir}/lib"
case "\$(uname -s)" in
  Darwin*) export DYLD_LIBRARY_PATH="\${KLI_LIB_DIR}\${DYLD_LIBRARY_PATH:+:\$DYLD_LIBRARY_PATH}" ;;
  *)       export LD_LIBRARY_PATH="\${KLI_LIB_DIR}\${LD_LIBRARY_PATH:+:\$LD_LIBRARY_PATH}" ;;
esac
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

  # --- Optional dependency: Ollama (semantic search) ---
  printf "\nChecking optional dependencies...\n"
  if command -v ollama >/dev/null 2>&1; then
    if ollama list 2>/dev/null | grep -q "nomic-embed-text"; then
      printf "  Ollama: ready (nomic-embed-text model available)\n"
    else
      printf "  Ollama: installed, but embedding model missing\n"
      printf "  Run: ollama pull nomic-embed-text\n"
    fi
  else
    printf "  Ollama: not found\n"
    printf "\n  Ollama enables semantic search and intelligent pattern matching.\n"
    printf "  Install it (optional):\n"
    printf "    curl -fsSL https://ollama.com/install.sh | sh\n"
    printf "    ollama pull nomic-embed-text\n"
    printf "\n  kli works without Ollama (uses keyword search instead).\n"
  fi

  printf "\nNext: run 'kli init' in any project to configure Claude Code.\n"
}

main
