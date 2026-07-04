#!/bin/sh
# Prove `kli update` authenticates its downloads over real TLS before a release
# is sealed -- on a musl libc (Alpine) and an older glibc (Ubuntu 22.04) than the
# build host, the two host environments the install gate's boot checks exercise
# but its verify path does not.
#
# The self-updater fetches the release tarball AND its checksums.txt over the
# same channel; if that channel is unverified, a network attacker swaps both and
# the checksum only detects corruption, not tampering. The fix verifies against a
# CA the relocated image discovers in the host trust store. This gate exercises
# that end to end: it builds two versions from source -- the current v1 and a
# bumped v2 -- serves them from a local origin behind a throwaway CA whose leaf
# covers the GitHub hostnames, then in each container installs v1 and runs the
# real `kli update`:
#   * with the CA absent from the trust store, the update MUST fail closed -- an
#     untrusted origin is rejected, never silently accepted;
#   * with the CA installed into the container's real trust store and every CA
#     environment override unset, the update MUST succeed and flip v1 -> v2, so a
#     green result is caused by the on-host CA discovery, not an override.
# The v1 -> v2 flip forces the tarball + checksums download -- the path the fix
# hardens -- rather than the early "already up to date" return.
#
# Inputs (env):
#   RUN_CONTAINERS   "true" to run the ubuntu:22.04 + alpine update checks
#   KLI_V1_BUNDLE    prebuilt relocatable bundle for v1 (skips the v1 nix build)
#   KLI_V2_BUNDLE    prebuilt relocatable bundle for v2 (skips the v2 nix build)
#
# Run from the repo root.

set -eu

work="$PWD"
here="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

case "$(uname -s)" in
  Linux)  os=linux ;;
  Darwin) os=darwin ;;
  *) echo "unsupported OS $(uname -s)" >&2; exit 1 ;;
esac
case "$(uname -m)" in
  x86_64|amd64)  arch=x86_64 ;;
  aarch64|arm64) arch=aarch64 ;;
  *) echo "unsupported arch $(uname -m)" >&2; exit 1 ;;
esac
artifact="kli-${os}-${arch}"
system="${arch}-${os}"

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1"; else shasum -a 256 "$1"; fi
}

# v1 is the checked-in version; v2 bumps its patch component so the two bundles
# differ only in the compiled +kli-version+.
v1=$(tr -d '"[:space:]' < version.sexp)
v2="${v1%.*}.$(( ${v1##*.} + 1 ))"

# Bundles: use the prebuilt overrides when given, else build both from source.
# v2 reuses v1's sources with version.sexp and its generated constant moved
# together to v2 (so the drift gate stays satisfied), then restored.
if [ -n "${KLI_V1_BUNDLE:-}" ] && [ -n "${KLI_V2_BUNDLE:-}" ]; then
  b1="$KLI_V1_BUNDLE"
  b2="$KLI_V2_BUNDLE"
else
  echo "== build v1 (${v1}) and v2 (${v2}) bundles =="
  restore() { git checkout -- version.sexp src/extensions/app/version-const.lisp 2>/dev/null || true; }
  trap restore EXIT
  nix build ".#packages.${system}.kli-relocatable" -o result-v1
  printf '"%s"\n' "$v2" > version.sexp
  printf '(in-package #:kli/app)\n(defparameter +kli-version+ "%s")\n' "$v2" \
    > src/extensions/app/version-const.lisp
  nix build ".#packages.${system}.kli-relocatable" -o result-v2
  restore
  trap - EXIT
  b1="$work/result-v1"
  b2="$work/result-v2"
fi

# A throwaway CA and an origin leaf covering both GitHub hosts the updater hits.
echo "== fixture CA + origin cert =="
rm -rf gate && mkdir -p gate/certs
openssl req -x509 -newkey rsa:2048 -nodes -days 1 \
  -subj "/CN=kli update-gate CA" \
  -keyout gate/certs/ca.key -out gate/certs/ca.crt 2>/dev/null
openssl req -newkey rsa:2048 -nodes \
  -subj "/CN=api.github.com" \
  -keyout gate/certs/origin.key -out gate/origin.csr 2>/dev/null
printf 'subjectAltName=DNS:api.github.com,DNS:github.com\n' > gate/ext.cnf
openssl x509 -req -in gate/origin.csr -CA gate/certs/ca.crt -CAkey gate/certs/ca.key \
  -CAcreateserial -days 1 -extfile gate/ext.cnf -out gate/certs/origin.crt 2>/dev/null

# Origin docroot mirroring the two GitHub hosts: releases/latest JSON on the api
# path, the per-tag tarball + checksums on the download path. Each tarball's top
# level is ${artifact}/, which install.sh strips and the updater reconstructs.
echo "== assemble release origin =="
api="gate/docroot/repos/kleisli-io/kli/releases"
dl="gate/docroot/kleisli-io/kli/releases/download"
mkdir -p "$api"
printf '{"tag_name":"v%s"}' "$v2" > "$api/latest"
pack() { # <version> <bundle>
  d="$dl/v$1"
  mkdir -p "$d"
  rm -rf gate/payload && mkdir -p "gate/payload/${artifact}"
  cp -r "$2"/. "gate/payload/${artifact}/"
  chmod -R u+w gate/payload
  tar -C gate/payload -czf "$d/${artifact}.tar.gz" "${artifact}"
  ( cd "$d" && sha256 "${artifact}.tar.gz" > checksums.txt )
}
pack "$v1" "$b1"
pack "$v2" "$b2"

if [ "${RUN_CONTAINERS:-}" != "true" ]; then
  echo "update gate built; container checks skipped (set RUN_CONTAINERS=true)"
  exit 0
fi

for img in ubuntu:22.04 alpine:latest; do
  echo "== kli update verify on ${img} =="
  docker run --rm -v "$work:/work:ro" \
    -e V1="$v1" -e V2="$v2" -e IMG="$img" \
    "$img" sh -c '
      set -eu

      # Fetch/serve tools from the container own trust store; the fixture CA is
      # not yet trusted, so nothing here shadows the real store.
      if command -v apk >/dev/null 2>&1; then
        apk add --no-cache curl tar python3 ca-certificates >/dev/null
      else
        apt-get update -qq >/dev/null
        apt-get install -y -qq curl tar python3 ca-certificates >/dev/null
      fi

      # Both GitHub hosts resolve to the in-container origin.
      printf "127.0.0.1 api.github.com github.com\n" >> /etc/hosts

      # Serve the release surface over the fixture cert on 443.
      ( cd /work/gate/docroot \
        && python3 /work/.github/ci/serve-https.py 443 \
             /work/gate/certs/origin.crt /work/gate/certs/origin.key ) &
      i=0
      while [ "$i" -lt 30 ]; do
        curl -sf --cacert /work/gate/certs/ca.crt \
          https://api.github.com/repos/kleisli-io/kli/releases/latest -o /dev/null \
          && break
        i=$((i + 1)); sleep 1
      done
      [ "$i" -lt 30 ] || { echo "origin did not come up" >&2; exit 1; }

      kli=/root/kli/bin/kli

      # Install v1 via install.sh, trusting the fixture CA for this fetch only;
      # the real store stays clean so the update honesty check below holds.
      CURL_CA_BUNDLE=/work/gate/certs/ca.crt \
        KLI_DOWNLOAD_BASE="https://github.com/kleisli-io/kli/releases/download/v${V1}" \
        KLI_INSTALL_DIR=/root/kli sh /work/install.sh >/dev/null
      "$kli" version | grep -q "$V1"

      # Untrusted origin: the fixture CA is absent from the real store, so a real
      # CA-discovery probe (every override unset) must reject the update.
      if env -u SSL_CERT_FILE -u SSL_CERT_DIR -u CURL_CA_BUNDLE "$kli" update -y \
           >/dev/null 2>&1; then
        echo "SECURITY: update accepted an untrusted origin on ${IMG}" >&2
        exit 1
      fi
      "$kli" version | grep -q "$V1"

      # Trust the fixture CA in the container real store.
      cp /work/gate/certs/ca.crt /usr/local/share/ca-certificates/kli-update-gate.crt
      update-ca-certificates >/dev/null 2>&1

      # Trusted origin, every override unset: a green update is caused by the
      # on-host discovery finding the just-installed anchor, and it flips to v2.
      env -u SSL_CERT_FILE -u SSL_CERT_DIR -u CURL_CA_BUNDLE "$kli" update -y >/dev/null
      "$kli" version | grep -q "$V2"

      # The updated image is a relocated copy, never a store binary.
      case "$(readlink -f /root/kli/lib/kli/bin/.kli-wrapped)" in
        /nix/store/*) echo "updated image resolved into the store on ${IMG}" >&2; exit 1 ;;
      esac

      echo "update verify passed on ${IMG} (v${V1} -> v${V2})"
    '
done

echo "update gate passed"
