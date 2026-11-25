#!/usr/bin/env bash

# lib-signing.sh — the ONE choice of the identity agent-repl's macOS apps are
# signed with.
#
# An app signed ad hoc is known to macOS by its code's hash, so a permission
# macOS gave it (Location Services, Accessibility) is lost at every rebuild
# (2026-11-18, the Wi-Fi helper). Signed with one certificate, the permission
# names the certificate, which a rebuild keeps. The certificate is the
# requirements manifest's `local-signing` entry (requirements.json), which
# bin/requirements.sh makes once in the login keychain.
#
# test-lib-signing.sh fails a build script that runs codesign without asking
# local_signing_identity for its identity.

# local_signing_identity MODULE_DIR: the SHA-1 of the login keychain's
# code-signing identity named by MODULE_DIR/requirements.json's local-signing
# entry. The identity is self-signed, so codesign refuses it by name and is
# handed its hash. Fails, saying so on stderr, when the keychain holds none.
local_signing_identity() {
  local name identities identity
  name="$(jq -er '.requirements[] | select(.id == "local-signing") | .name' "$1/requirements.json")" || {
    echo "lib-signing.sh: $1/requirements.json names no local-signing identity" >&2
    return 1
  }
  identities="$(security find-identity -p codesigning)"
  identity="$(awk -v n="\"$name\"" 'index($0, n) {print $2; exit}' <<<"$identities")"
  if [ -z "$identity" ]; then
    echo "lib-signing.sh: the login keychain holds no code-signing identity \"$name\"; run bin/requirements.sh install" >&2
    return 1
  fi
  printf '%s\n' "$identity"
}
