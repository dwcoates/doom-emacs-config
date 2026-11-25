#!/usr/bin/env bash
# Hermetic tests for bin/lib-signing.sh, the one choice of the identity
# agent-repl's macOS apps are signed with. security is a stub that lists the
# case's identities.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="$(cd "$THIS_DIR/.." && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin" "$TMP/module"
# The stub prints $TMP/identities as security find-identity's listing.
cat >"$TMP/bin/security" <<STUB
#!/usr/bin/env bash
[ "\$*" = "find-identity -p codesigning" ] || { echo "security: unexpected \$*" >&2; exit 64; }
cat "$TMP/identities"
STUB
chmod +x "$TMP/bin/security"
export PATH="$TMP/bin:$PATH"

# shellcheck source=lib-signing.sh
. "$THIS_DIR/lib-signing.sh"
# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"

printf '{"requirements": [{"id": "local-signing", "kind": "signing_identity", "name": "case signing"}]}\n' >"$TMP/module/requirements.json"

test_the_named_identity_is_answered_by_its_hash() {
  local got
  printf '  1) AAAA "Apple Development: someone"\n  2) BBBB "case signing" (CSSMERR_TP_NOT_TRUSTED)\n     2 identities found\n' >"$TMP/identities"
  got="$(local_signing_identity "$TMP/module")"
  if [ "$got" = BBBB ]; then pass "the identity the manifest names is answered by its hash"; else fail "named identity" "$got"; fi
}

test_a_keychain_without_the_identity_fails_naming_it() {
  local out rc=0
  printf '  1) AAAA "Apple Development: someone"\n     1 identity found\n' >"$TMP/identities"
  out="$(local_signing_identity "$TMP/module" 2>&1)" || rc=$?
  if [ "$rc" -ne 0 ] && [[ $out == *'no code-signing identity "case signing"; run bin/requirements.sh install'* ]]; then
    pass "a keychain without the identity fails, naming it and the install that makes it"
  else fail "missing identity" "rc=$rc" "$out"; fi
}

test_a_manifest_without_the_entry_fails_naming_it() {
  local out rc=0
  mkdir -p "$TMP/bare"
  printf '{"requirements": []}\n' >"$TMP/bare/requirements.json"
  out="$(local_signing_identity "$TMP/bare" 2>&1)" || rc=$?
  if [ "$rc" -ne 0 ] && [[ $out == *"names no local-signing identity"* ]]; then
    pass "a manifest without the local-signing entry fails, naming it"
  else fail "missing entry" "rc=$rc" "$out"; fi
}

test_the_module_manifest_names_the_identity() {
  if jq -e '.requirements[] | select(.id == "local-signing" and .kind == "signing_identity" and (.name | length) > 0)' "$MODULE_DIR/requirements.json" >/dev/null; then
    pass "the module's requirements.json names the local-signing identity"
  else fail "the module's requirements.json has no local-signing signing_identity entry"; fi
}

# The call sites share the choice: a script that signs with a certificate
# takes it from local_signing_identity.
test_every_certificate_signer_asks_the_library() {
  local f rel code bad=""
  while IFS= read -r f; do
    rel="${f#"$MODULE_DIR"/}"
    case "$rel" in bin/lib-signing.sh|*/test-*.sh) continue ;; esac
    code="$(grep -vE '^[[:space:]]*#' "$f")"
    grep -qE 'codesign .*--sign [^-]' <<<"$code" || continue
    grep -qF 'local_signing_identity' <<<"$code" || bad="$bad $rel"
  done < <(find "$MODULE_DIR/bin" "$MODULE_DIR/wifi-helper" -name '*.sh' -type f)
  if [ -z "$bad" ]; then pass "every script signing with a certificate takes it from local_signing_identity"; else fail "these scripts choose their signing identity themselves; source lib-signing.sh instead:$bad"; fi
}

# Every app macOS grants a permission is signed with the local identity, never
# ad hoc, so a rebuild keeps the permission: the Wi-Fi helper (Location
# Services) and Zettel (Accessibility).
test_every_permission_holding_app_is_signed_with_the_local_identity() {
  local build code bad=""
  for build in wifi-helper/build.sh bin/build-launcher.sh; do
    code="$(grep -vE '^[[:space:]]*#' "$MODULE_DIR/$build")"
    if grep -qE 'codesign .*--sign -( |$)' <<<"$code" || ! grep -qE 'codesign .*--sign "\$identity"|codesign .*--sign "\$IDENTITY"' <<<"$code" \
      || ! grep -qF 'local_signing_identity' <<<"$code"; then
      bad="$bad $build"
    fi
  done
  if [ -z "$bad" ]; then pass "the Wi-Fi helper and Zettel are signed with the local identity, never ad hoc"; else fail "these builds of an app holding a permission do not sign with the local identity:$bad"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

harness_finish
