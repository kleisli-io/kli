#!/bin/sh
# Prove the published installer actually works before a release is sealed.
#
# The bundle-boot gate already proves the tarball runs; this proves install.sh
# -- the thing users pipe into a shell -- installs that tarball and the result
# boots. It runs the real install.sh, unmodified except for the origin it
# fetches from, against the freshly built artifact served over a local HTTPS
# endpoint. On Linux it additionally installs inside clean, store-free
# containers whose libc predates the build's (Ubuntu 22.04) or is musl
# (Alpine): the bundle ships its own loader and libc, so booting on both is the
# executed witness of host-libc independence that replaces a hard-coded floor.
# Finally it exercises the one path the origin override skips -- the
# unauthenticated "latest version" parse -- against a captured API response.
#
# Inputs (env):
#   ARTIFACT           tarball base name, e.g. kli-linux-x86_64 (.tar.gz assumed)
#   EXPECT_VERSION     version line the installed binary must print (default below)
#   RUN_CONTAINERS     "true" to run the old-glibc + musl container installs
#   RUN_RESOLVER_TEST  "true" to run the latest-version parse test
#
# Run from the repo root, with install.sh and ${ARTIFACT}.tar.gz present there.

set -eu

work="$PWD"
here="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
artifact="${ARTIFACT}"
expect="${EXPECT_VERSION:-kli 0.1.0}"
port=8443
base="https://localhost:${port}"

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1"
  else
    shasum -a 256 "$1"
  fi
}

# A throwaway cert for localhost, trusted only by this job via CURL_CA_BUNDLE,
# so install.sh's `curl --proto '=https' --tlsv1.2` runs over real TLS with
# nothing in its fetch/verify/extract logic stubbed. The config-file SAN form
# works on both OpenSSL and LibreSSL (macOS).
cat > cert.cnf <<'CNF'
[req]
distinguished_name = dn
x509_extensions = ext
prompt = no
[dn]
CN = localhost
[ext]
basicConstraints = critical, CA:TRUE
subjectAltName = DNS:localhost
CNF
openssl req -x509 -newkey rsa:2048 -nodes -keyout key.pem -out cert.pem \
  -days 1 -config cert.cnf 2>/dev/null
export CURL_CA_BUNDLE="$work/cert.pem" SSL_CERT_FILE="$work/cert.pem"

# macOS curl may validate through the system keychain (Secure Transport) rather
# than honoring a CA file, so on Darwin also trust the throwaway cert there. The
# env vars above cover LibreSSL/OpenSSL builds; one of the two paths is the
# operative one, so the fetch validates regardless of how curl was built.
if [ "$(uname -s)" = "Darwin" ]; then
  sudo security add-trusted-cert -d -r trustRoot \
    -k /Library/Keychains/System.keychain "$work/cert.pem"
fi

# checksums.txt exactly as the release generates it; install.sh greps out its
# own line, so a single-artifact file is faithful to the multi-artifact one.
sha256 "${artifact}.tar.gz" > checksums.txt

python3 "$here/serve-https.py" "$port" cert.pem key.pem &
server=$!
trap 'kill "$server" 2>/dev/null || true' EXIT

ready=
i=0
while [ "$i" -lt 30 ]; do
  if curl -fsS "${base}/checksums.txt" >/dev/null 2>&1; then
    ready=1
    break
  fi
  i=$((i + 1))
  sleep 1
done
[ -n "$ready" ] || { echo "local HTTPS origin did not come up" >&2; exit 1; }

# 1) Install on this target's own native runner and boot the installed wrapper.
echo "== native install (${artifact}) =="
native="$(mktemp -d)"
KLI_DOWNLOAD_BASE="$base" KLI_INSTALL_DIR="$native" sh install.sh
"$native/bin/kli" version | grep -q "$expect"

# 2) Host-libc independence: old glibc (Ubuntu 22.04, 2.35) and musl (Alpine),
#    clean and store-free. --network=host reaches the origin on the runner.
if [ "${RUN_CONTAINERS:-}" = "true" ]; then
  for img in ubuntu:22.04 alpine:latest; do
    echo "== install on ${img} =="
    docker run --rm --network=host \
      -v "$work:/work" -w /work \
      -e KLI_DOWNLOAD_BASE="$base" \
      -e EXPECT="$expect" \
      "$img" sh -c '
        set -eu
        # Install fetch tools using the container own CA store. The test cert is
        # scoped to install.sh alone so it never shadows the package manager CA
        # -- apk honors SSL_CERT_FILE, apt does not.
        if command -v apk >/dev/null 2>&1; then
          apk add --no-cache curl tar >/dev/null
        else
          apt-get update -qq >/dev/null
          apt-get install -y -qq curl ca-certificates >/dev/null
        fi
        d="$(mktemp -d)"
        CURL_CA_BUNDLE=/work/cert.pem SSL_CERT_FILE=/work/cert.pem \
          KLI_INSTALL_DIR="$d" sh install.sh
        "$d/bin/kli" version | grep -q "$EXPECT"
      '
  done
fi

# 3) The one path the origin override skips: the unauthenticated "latest"
#    parse. Shim curl to serve a captured releases/latest body and the local
#    files, so the real get_latest_version runs end to end with no network.
if [ "${RUN_RESOLVER_TEST:-}" = "true" ]; then
  echo "== latest-version resolver =="
  cat > releases-latest.json <<'JSON'
{
  "url": "https://api.github.com/repos/kleisli-io/kli/releases/123",
  "tag_name": "v0.1.0",
  "name": "v0.1.0",
  "draft": false,
  "prerelease": false,
  "html_url": "https://github.com/kleisli-io/kli/releases/tag/v0.1.0"
}
JSON
  mkdir -p shim
  cat > shim/curl <<'SHIM'
#!/bin/sh
url=
out=
while [ $# -gt 0 ]; do
  case "$1" in
    -o) out="$2"; shift 2 ;;
    https://*) url="$1"; shift ;;
    *) shift ;;
  esac
done
case "$url" in
  */releases/latest)
    cat "$WORK/releases-latest.json" ;;
  *)
    f="${url##*/}"
    if [ -n "$out" ]; then cp "$WORK/$f" "$out"; else cat "$WORK/$f"; fi ;;
esac
SHIM
  chmod +x shim/curl
  d="$(mktemp -d)"
  out=$(WORK="$work" PATH="$work/shim:$PATH" KLI_INSTALL_DIR="$d" sh install.sh)
  printf '%s\n' "$out" | grep -q "Fetching latest version... v0.1.0"
  "$d/bin/kli" version | grep -q "$expect"
fi

echo "install gate passed for ${artifact}"
