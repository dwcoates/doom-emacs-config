#!/usr/bin/env bash
# Hermetic tests for bin/requirements.sh.
#
# NOTHING REAL IS INSTALLED, LOADED OR READ. Each case builds its own module
# dir (AGENT_REPL_REQUIREMENTS_ROOT) holding a manifest written for the case, a
# fake HOME, and a stub dir that is the only non-system dir on PATH: brew, go,
# launchctl, emacs, terminal-notifier and the build script are stubs that
# record their argv in a transcript. The skill_link and git_hook kinds and the
# legacy-hook purge are covered by .claude/test-install.sh, which drives this
# runner through install.sh against a fake-git repository.

# shellcheck disable=SC2016,SC2329
# SC2016: every stub body is single-quoted on purpose, so it expands when the
# stub runs, not when the case writes it. SC2329: the tests are found and run
# by name (declare -F), so shellcheck sees no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
RUNNER="$THIS_DIR/requirements.sh"
MODULE="$(cd "$THIS_DIR/.." && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0

pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; shift; [ $# -eq 0 ] || printf '%s\n' "$@" | sed 's/^/        /' >&2; FAIL=$((FAIL + 1)); }

# ---------------------------------------------------------------------------
# Fixture.
# ---------------------------------------------------------------------------

# new_case: a fresh module dir, HOME and stub dir under CASE.
CASE=""
new_case() {
  CASE="$(mktemp -d "$TMP/case.XXXXXX")"
  mkdir -p "$CASE/module/bin" "$CASE/home" "$CASE/stubs"
  : >"$CASE/transcript"
  write_stub brew '
case "$1" in
  list) [ -f "$STUB_DIR/.brew-has-$3" ] ;;
  tap) ;;
  install|upgrade)
    shift
    [ "$1" = --cask ] && shift
    : >"$STUB_DIR/.brew-has-$1"
    if [ -n "${STUB_BREW_PROVIDES:-}" ]; then
      printf "#!/bin/sh\necho \"%s\"\n" "${STUB_BREW_VERSION:-tool 9.9.9}" >"$STUB_DIR/$STUB_BREW_PROVIDES"
      chmod +x "$STUB_DIR/$STUB_BREW_PROVIDES"
    fi ;;
esac'
  write_stub go ''
  write_stub build ''
  write_stub launchctl '
case "$1" in
  print) [ -f "$STUB_DIR/.loaded-${2##*/}" ] ;;
  bootstrap) label="$(basename "$3" .plist)"; : >"$STUB_DIR/.loaded-$label" ;;
  bootout) rm -f "$STUB_DIR/.loaded-$(basename "$3" .plist)" ;;
esac'
}

# write_stub NAME BODY: a stub on PATH that records its argv, then runs BODY.
write_stub() {
  printf '#!/bin/bash\nprintf "%%s %%s\\n" "%s" "$*" >>"$STUB_DIR/../transcript"\n%s\n' "$1" "$2" >"$CASE/stubs/$1"
  chmod +x "$CASE/stubs/$1"
}

# tool NAME OUTPUT [EXIT]: a stub program that prints OUTPUT and exits EXIT.
tool() {
  printf '#!/bin/sh\nprintf "%%s\\n" "%s"\nexit %s\n' "$2" "${3:-0}" >"$CASE/stubs/$1"
  chmod +x "$CASE/stubs/$1"
}

# manifest JSON-ARRAY [INCLUDES-JSON]: the case's root manifest.
manifest() {
  printf '{"system": "root", "includes": %s, "requirements": %s}\n' "${2:-[]}" "$1" >"$CASE/module/requirements.json"
}

# run VERB ARGS...: the runner in the case's world. Sets RC and OUT.
RC=0
OUT=""
run() {
  local extra=()
  read -r -a extra <<<"${EXTRA_ENV:-}"
  set +e
  OUT="$(env -i HOME="$CASE/home" PATH="$CASE/stubs:/usr/bin:/bin" STUB_DIR="$CASE/stubs" \
    AGENT_REPL_REQUIREMENTS_ROOT="$CASE/module" AGENT_REPL_REQUIREMENTS_PLATFORM="${PLATFORM:-darwin}" \
    AGENT_REPL_REQUIREMENTS_BUILD="$CASE/stubs/build" AGENT_REPL_LAUNCHCTL="$CASE/stubs/launchctl" \
    AGENT_REPL_BACKGROUND_PRIORITY=1 ${extra[@]+"${extra[@]}"} \
    /bin/bash "$RUNNER" "$@" 2>&1)"
  RC=$?
  set -e
}

has() { grep -qF -- "$1" <<<"$OUT"; }
ran() { grep -qF -- "$1" "$CASE/transcript"; }

PROGRAM='{"id": "tool", "kind": "program", "why": "the case needs it", "profile": "run", "platforms": ["darwin"], "command": "tool", "install": {"brew": "tool"}}'
VERSIONED='{"id": "tool", "kind": "program", "why": "the case needs it", "profile": "run", "platforms": ["darwin"], "command": "tool", "version": {"args": ["--version"], "min": "3.0"}, "install": {"brew": "tool"}}'

# ---------------------------------------------------------------------------
# The real manifests.
# ---------------------------------------------------------------------------

test_the_real_manifests_honor_the_contract() {
  set +e
  OUT="$(env -i HOME="$TMP" PATH="/usr/bin:/bin" AGENT_REPL_REQUIREMENTS_PLATFORM=darwin /bin/bash "$RUNNER" list 2>&1)"
  RC=$?
  set -e
  if [ "$RC" -eq 0 ] && grep -q '^terminal-notifier	' <<<"$OUT" && grep -q '^service-shim-store	' <<<"$OUT"; then
    pass "the real manifests load, validate and list"
  else
    fail "the real manifests" "rc=$RC" "$OUT"
  fi
}

# A live service the editor waits on must never rank below a test suite:
# launchd's `Background` is darwinbg (efficiency cores, throttled disk I/O),
# which loses every I/O contention to a `nice 19` suite.
test_every_launchd_service_runs_at_standard_priority() {
  local bad="" plist type
  while IFS= read -r plist; do
    type="$(plutil -extract ProcessType raw -o - "$MODULE/$plist" 2>/dev/null || echo "<unset>")"
    [ "$type" = Standard ] || bad="$bad $plist=$type"
  done < <(find "$MODULE" -name requirements.json -not -path '*/node_modules/*' -not -path '*/testdata/*' -print0 \
             | xargs -0 jq -r '.requirements[]? | select(.kind == "launchd_service") | .plist')
  if [ -z "$bad" ]; then pass "every launchd service runs at Standard priority"; else fail "launchd services not at Standard priority:$bad"; fi
}

test_every_system_with_a_manifest_is_included() {
  local missing="" f rel
  while IFS= read -r f; do
    rel="${f#"$MODULE"/}"
    [ "$rel" = requirements.json ] && continue
    jq -e --arg r "$rel" '.includes | index($r)' "$MODULE/requirements.json" >/dev/null || missing="$missing $rel"
  done < <(find "$MODULE" -name requirements.json -not -path '*/node_modules/*' -not -path '*/testdata/*')
  if [ -z "$missing" ]; then pass "every system manifest is included by the root"; else fail "manifests the root does not include:$missing"; fi
}

# Every hook checked in under .githooks is installed by a git_hook entry, so a
# machine whose hooks directory holds copies (no core.hooksPath) has them all.
test_every_checked_in_hook_has_a_git_hook_entry() {
  local missing="" hook name
  for hook in "$MODULE/../../../.githooks"/*; do
    name="${hook##*/}"
    case "$name" in test-*) continue ;; esac
    jq -e --arg h "$name" '[.requirements[]? | select(.kind == "git_hook" and .hook == $h)] | length == 1' \
      "$MODULE/requirements.json" >/dev/null || missing="$missing $name"
  done
  if [ -z "$missing" ]; then pass "every checked-in hook has exactly one git_hook entry"; else fail "checked-in hooks without a git_hook entry:$missing"; fi
}

# ---------------------------------------------------------------------------
# The manifest contract.
# ---------------------------------------------------------------------------

test_an_unknown_kind_is_refused() {
  new_case
  manifest '[{"id": "x", "kind": "magic", "why": "w", "profile": "run"}]'
  run list
  if [ "$RC" -eq 2 ] && has "x: unknown kind magic"; then pass "an unknown kind is refused"; else fail "unknown kind" "rc=$RC" "$OUT"; fi
}

test_a_missing_field_is_refused() {
  new_case
  manifest '[{"id": "x", "kind": "program", "why": "w", "profile": "run", "platforms": ["darwin"], "install": {"brew": "x"}}]'
  run list
  if [ "$RC" -eq 2 ] && has 'x: program needs "command"'; then pass "a missing field is refused"; else fail "missing field" "rc=$RC" "$OUT"; fi
}

test_a_bad_profile_is_refused() {
  new_case
  manifest '[{"id": "x", "kind": "env", "why": "w", "profile": "sometimes", "name": "X", "doc": "d"}]'
  run list
  if [ "$RC" -eq 2 ] && has "profile must be run or dev, not sometimes"; then pass "a bad profile is refused"; else fail "bad profile" "rc=$RC" "$OUT"; fi
}

test_a_missing_include_is_refused() {
  new_case
  manifest '[]' '["nowhere/requirements.json"]'
  run list
  if [ "$RC" -eq 2 ] && has "includes nowhere/requirements.json, which does not exist"; then pass "a missing include is refused"; else fail "missing include" "rc=$RC" "$OUT"; fi
}

test_a_shared_id_declared_differently_is_refused() {
  new_case
  mkdir -p "$CASE/module/sub"
  printf '{"system": "sub", "requirements": [%s]}\n' "${PROGRAM/brew\": \"tool/brew\": \"other}" >"$CASE/module/sub/requirements.json"
  manifest "[$PROGRAM]" '["sub/requirements.json"]'
  run list
  if [ "$RC" -eq 2 ] && has "tool: declared differently by"; then pass "a shared id declared differently is refused"; else fail "differing shared id" "rc=$RC" "$OUT"; fi
}

test_a_shared_id_declared_identically_loads_once() {
  new_case
  mkdir -p "$CASE/module/sub"
  printf '{"system": "sub", "requirements": [%s]}\n' "$PROGRAM" >"$CASE/module/sub/requirements.json"
  manifest "[$PROGRAM]" '["sub/requirements.json"]'
  run list
  if [ "$RC" -eq 0 ] && [ "$(printf '%s\n' "$OUT" | grep -c '^tool	')" -eq 1 ]; then pass "a shared id declared identically loads once"; else fail "identical shared id" "rc=$RC" "$OUT"; fi
}

# ---------------------------------------------------------------------------
# Selection.
# ---------------------------------------------------------------------------

test_the_run_profile_leaves_dev_entries_out() {
  new_case
  manifest "[$PROGRAM, ${PROGRAM/\"id\": \"tool\"/\"id\": \"devtool\"}]"
  sed -i '' 's/"id": "devtool", "kind": "program", "why": "the case needs it", "profile": "run"/"id": "devtool", "kind": "program", "why": "the case needs it", "profile": "dev"/' "$CASE/module/requirements.json"
  run list --profile run
  if [ "$RC" -eq 0 ] && has "tool	" && ! has "devtool"; then pass "--profile run leaves dev entries out"; else fail "profile selection" "rc=$RC" "$OUT"; fi
}

test_another_platforms_entry_is_left_out() {
  new_case
  manifest "[$PROGRAM]"
  PLATFORM=linux run list
  if [ "$RC" -eq 0 ] && [ -z "$OUT" ]; then pass "a darwin-only entry is left out on linux"; else fail "platform selection" "rc=$RC" "$OUT"; fi
}

# ---------------------------------------------------------------------------
# program.
# ---------------------------------------------------------------------------

test_a_present_program_is_ok() {
  new_case; manifest "[$PROGRAM]"; tool tool "hi"
  run check
  if [ "$RC" -eq 0 ] && has "ok          tool: $CASE/stubs/tool"; then pass "a present program is ok"; else fail "present program" "rc=$RC" "$OUT"; fi
}

test_a_missing_program_fails_the_check_with_its_fix() {
  new_case; manifest "[$PROGRAM]"
  run check
  if [ "$RC" -eq 1 ] && has "missing     tool: tool is not on PATH" && has "Fix: brew install tool" && [ ! -s "$CASE/transcript" ]; then
    pass "a missing program fails the check, names its fix, and installs nothing"
  else fail "missing program" "rc=$RC" "$OUT"; fi
}

test_a_program_at_its_minimum_is_ok() {
  new_case; manifest "[$VERSIONED]"; tool tool "tool 3.0"
  run check
  if [ "$RC" -eq 0 ] && has "ok          tool: $CASE/stubs/tool 3.0"; then pass "a program at its minimum version is ok"; else fail "minimum version" "rc=$RC" "$OUT"; fi
}

test_a_program_below_its_minimum_is_outdated() {
  new_case; manifest "[$VERSIONED]"; tool tool "tool 2.0.0"
  run check
  if [ "$RC" -eq 1 ] && has "outdated    tool: $CASE/stubs/tool is 2.0.0, older than 3.0"; then pass "a program below its minimum is outdated"; else fail "outdated program" "rc=$RC" "$OUT"; fi
}

test_a_failing_version_query_is_failed() {
  new_case; manifest "[$VERSIONED]"; tool tool "boom" 3
  run check
  if [ "$RC" -eq 1 ] && has "failed      tool: $CASE/stubs/tool --version failed: boom"; then pass "a failing version query is failed"; else fail "failing version query" "rc=$RC" "$OUT"; fi
}

test_a_version_answer_with_no_number_is_failed() {
  new_case; manifest "[$VERSIONED]"; tool tool "usage: tool"
  run check
  if [ "$RC" -eq 1 ] && has "answered no version: usage: tool"; then pass "a version answer with no number is failed"; else fail "no version number" "rc=$RC" "$OUT"; fi
}

test_install_brews_a_missing_program() {
  new_case; manifest "[$PROGRAM]"
  EXTRA_ENV="STUB_BREW_PROVIDES=tool" run install
  if [ "$RC" -eq 0 ] && ran "brew install tool" && has "install: tool is missing" && has "ok          tool:"; then
    pass "install brews a missing program and re-checks it"
  else fail "brew install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_install_upgrades_an_outdated_program() {
  new_case; manifest "[$VERSIONED]"; tool tool "tool 2.0"; : >"$CASE/stubs/.brew-has-tool"
  EXTRA_ENV="STUB_BREW_PROVIDES=tool STUB_BREW_VERSION=tool-3.1" run install
  if [ "$RC" -eq 0 ] && ran "brew upgrade tool" && ! ran "brew install"; then pass "install upgrades an outdated program"; else fail "brew upgrade" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_install_taps_and_passes_options() {
  new_case
  manifest '[{"id": "tool", "kind": "program", "why": "w", "profile": "run", "platforms": ["darwin"], "command": "tool", "install": {"brew": "tool@3", "tap": "some/tap", "options": ["--with-x"]}}]'
  EXTRA_ENV="STUB_BREW_PROVIDES=tool" run install
  if [ "$RC" -eq 0 ] && ran "brew tap some/tap" && ran "brew install tool@3 --with-x"; then pass "install taps and passes the formula's options"; else fail "tap and options" "rc=$RC" "$(cat "$CASE/transcript")"; fi
}

test_install_brews_a_cask() {
  new_case
  manifest '[{"id": "tool", "kind": "program", "why": "w", "profile": "run", "platforms": ["darwin"], "command": "tool", "install": {"brew_cask": "toolapp"}}]'
  EXTRA_ENV="STUB_BREW_PROVIDES=tool" run install
  if ran "brew install --cask toolapp"; then pass "install brews a cask"; else fail "cask" "rc=$RC" "$(cat "$CASE/transcript")"; fi
}

test_install_go_installs() {
  new_case
  manifest '[{"id": "tool", "kind": "program", "why": "w", "profile": "run", "platforms": ["darwin"], "command": "tool", "install": {"go_install": "example.com/tool@v1"}}]'
  run install
  if ran "go install example.com/tool@v1"; then pass "install go-installs"; else fail "go install" "rc=$RC" "$(cat "$CASE/transcript")"; fi
}

test_a_manual_program_needs_the_user() {
  new_case
  manifest '[{"id": "tool", "kind": "program", "why": "w", "profile": "run", "platforms": ["darwin"], "command": "tool", "install": {"manual": "Download it from example.com."}}]'
  run install
  if [ "$RC" -eq 1 ] && has "tool has no install step; it needs you: Download it from example.com." && [ ! -s "$CASE/transcript" ]; then
    pass "a manual program needs the user and fails the install"
  else fail "manual program" "rc=$RC" "$OUT"; fi
}

test_an_unmet_optional_entry_does_not_fail_the_run() {
  new_case
  manifest '[{"id": "tool", "kind": "program", "why": "nice to have", "profile": "run", "optional": true, "platforms": ["darwin"], "command": "tool", "install": {"brew": "tool"}}]'
  run check
  if [ "$RC" -eq 0 ] && has "optional    tool (missing): tool is not on PATH; nice to have"; then pass "an unmet optional entry is reported and does not fail the run"; else fail "optional entry" "rc=$RC" "$OUT"; fi
}

# ---------------------------------------------------------------------------
# env.
# ---------------------------------------------------------------------------

ENV_ENTRY='{"id": "var", "kind": "env", "why": "w", "profile": "run", "name": "SOME_VAR", "doc": "docs/X.md"}'

test_a_set_variable_is_ok() {
  new_case; manifest "[$ENV_ENTRY]"
  EXTRA_ENV="SOME_VAR=yes" run check
  if [ "$RC" -eq 0 ] && has "ok          var: SOME_VAR is set"; then pass "a set variable is ok"; else fail "set variable" "rc=$RC" "$OUT"; fi
}

test_an_unset_variable_needs_the_user() {
  new_case; manifest "[$ENV_ENTRY]"
  run install
  if [ "$RC" -eq 1 ] && has "needs-user  var: SOME_VAR is not set" && has "set SOME_VAR in your shell profile, then run \`doom env\` (see docs/X.md)" && ! has "installing"; then
    pass "an unset variable needs the user and is never installed"
  else fail "unset variable" "rc=$RC" "$OUT"; fi
}

# ---------------------------------------------------------------------------
# emacs.
# ---------------------------------------------------------------------------

EMACS_ENTRY='{"id": "emacs", "kind": "emacs", "why": "w", "profile": "run", "platforms": ["darwin"], "command": "emacs", "min": "30.1", "features": ["xwidget-internal"], "install": {"brew": "emacs-plus@30"}}'

# fake_emacs VERSION LACKING: an emacs stub answering --batch's two questions.
fake_emacs() {
  cat >"$CASE/stubs/emacs" <<EOF
#!/bin/sh
case "\$3" in
  *emacs-version*) printf '%s' "$1" ;;
  *featurep*) printf '%s' "$2" ;;
esac
EOF
  chmod +x "$CASE/stubs/emacs"
}

test_an_emacs_with_its_features_is_ok() {
  new_case; manifest "[$EMACS_ENTRY]"; fake_emacs 30.2 ""
  run check
  if [ "$RC" -eq 0 ] && has "ok          emacs: $CASE/stubs/emacs Emacs 30.2"; then pass "an emacs at its version with its features is ok"; else fail "emacs ok" "rc=$RC" "$OUT"; fi
}

test_an_emacs_lacking_a_feature_is_outdated() {
  new_case; manifest "[$EMACS_ENTRY]"; fake_emacs 30.2 "xwidget-internal"
  run check
  if [ "$RC" -eq 1 ] && has "outdated    emacs: $CASE/stubs/emacs (Emacs 30.2) lacks: xwidget-internal"; then pass "an emacs lacking a feature is outdated, naming it"; else fail "emacs feature" "rc=$RC" "$OUT"; fi
}

test_an_old_emacs_is_outdated() {
  new_case; manifest "[$EMACS_ENTRY]"; fake_emacs 29.4 ""
  run check
  if [ "$RC" -eq 1 ] && has "is Emacs 29.4, older than 30.1"; then pass "an old emacs is outdated"; else fail "old emacs" "rc=$RC" "$OUT"; fi
}

# ---------------------------------------------------------------------------
# notification_permission.
# ---------------------------------------------------------------------------

PERMISSION='{"id": "notify", "kind": "notification_permission", "why": "w", "profile": "run", "platforms": ["darwin"], "command": "terminal-notifier"}'

test_an_authorized_notifier_is_ok() {
  new_case; manifest "[$PERMISSION]"; tool terminal-notifier "No problems found."
  run check
  if [ "$RC" -eq 0 ] && has "ok          notify: terminal-notifier may notify"; then pass "an authorized notifier is ok"; else fail "authorized notifier" "rc=$RC" "$OUT"; fi
}

test_install_asks_for_an_unauthorized_notifiers_permission() {
  new_case; manifest "[$PERMISSION]"
  write_stub terminal-notifier 'case "$1" in -diagnose) echo "authorization not determined"; exit 3 ;; esac'
  run install
  if [ "$RC" -eq 1 ] && ran "terminal-notifier -title agent-repl" && has "may not notify: authorization not determined"; then
    pass "install posts one banner to ask, and reports the still-missing permission"
  else fail "unauthorized notifier" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

# ---------------------------------------------------------------------------
# launchd_service.
# ---------------------------------------------------------------------------

service_case() {
  new_case
  mkdir -p "$CASE/module/launchd"
  printf '<plist><string>~/.cache/agent-repl/bin/svc</string></plist>\n' >"$CASE/module/launchd/com.example.svc.plist"
  manifest '[{"id": "svc", "kind": "launchd_service", "why": "w", "profile": "run", "platforms": ["darwin"], "label": "com.example.svc", "plist": "launchd/com.example.svc.plist", "build_target": "svc", "binary": "svc"}]'
  write_stub build 'mkdir -p "$HOME/.cache/agent-repl/bin" && printf "#!/bin/sh\n" >"$HOME/.cache/agent-repl/bin/svc" && chmod +x "$HOME/.cache/agent-repl/bin/svc"'
}

test_install_builds_installs_and_loads_a_service() {
  service_case
  run install
  local plist="$CASE/home/Library/LaunchAgents/com.example.svc.plist"
  if [ "$RC" -eq 0 ] && ran "build svc" && grep -qF "$CASE/home/.cache/agent-repl/bin/svc" "$plist" \
     && ran "launchctl bootstrap gui/$(id -u) $plist" && ran "launchctl kickstart -k gui/$(id -u)/com.example.svc" \
     && ! ran "launchctl bootout" && has "ok          svc: com.example.svc loaded"; then
    pass "install builds the service, installs its plist with \$HOME expanded, and loads it"
  else fail "service install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_install_boots_a_loaded_stale_service_out_first() {
  service_case
  run install
  printf 'stale\n' >"$CASE/home/Library/LaunchAgents/com.example.svc.plist"
  : >"$CASE/transcript"
  run install
  if [ "$RC" -eq 0 ] && has "svc is outdated" && ran "launchctl bootout gui/$(id -u)"; then pass "a loaded service with a stale plist is booted out and reloaded"; else fail "service refresh" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_uninstall_removes_the_service() {
  service_case
  run install
  run uninstall
  if [ "$RC" -eq 0 ] && [ ! -e "$CASE/home/Library/LaunchAgents/com.example.svc.plist" ] && [ ! -e "$CASE/home/.cache/agent-repl/bin/svc" ] \
     && ran "launchctl bootout gui/$(id -u)"; then pass "uninstall boots the service out and removes its plist and binary"; else fail "service uninstall" "rc=$RC" "$OUT"; fi
}

# ---------------------------------------------------------------------------
# download.
# ---------------------------------------------------------------------------

# download_case [PIN]: a download entry whose zip holds words.dic, pinned to
# PIN (the real sha256 of words.dic when absent), served by a curl stub that
# copies the case's fixture zip to its -o path.
download_case() {
  new_case
  mkdir -p "$CASE/zip"
  printf 'hello\nworld\n' >"$CASE/zip/words.dic"
  (cd "$CASE/zip" && /usr/bin/zip -q fixture.zip words.dic)
  local pin="${1:-$(shasum -a 256 "$CASE/zip/words.dic" | cut -d' ' -f1)}"
  manifest "[{\"id\": \"dict\", \"kind\": \"download\", \"why\": \"w\", \"profile\": \"run\", \"url\": \"https://example.test/dict.zip\", \"archive\": \"zip\", \"dest\": \"dictionaries\", \"files\": [{\"from\": \"words.dic\", \"to\": \"en-test.dic\", \"sha256\": \"$pin\"}]}]"
  write_stub curl 'while [ "$#" -gt 0 ]; do [ "$1" = -o ] && { cp "$STUB_DIR/../zip/fixture.zip" "$2"; exit 0; }; shift; done; exit 7'
}

DICT_FILE() { printf '%s\n' "$CASE/home/.cache/agent-repl/dictionaries/en-test.dic"; }

test_a_missing_download_fails_the_check_with_its_fix() {
  download_case
  run check
  if [ "$RC" -eq 1 ] && has "missing     dict:" && has "lacks en-test.dic" && has "Fix: download https://example.test/dict.zip"; then
    pass "a missing download fails the check and names its URL"
  else fail "missing download" "rc=$RC" "$OUT"; fi
}

test_install_downloads_checks_and_unpacks_a_download() {
  download_case
  run install
  if [ "$RC" -eq 0 ] && ran "curl -fsSL --retry 2 -o" && ran "https://example.test/dict.zip" \
     && [ "$(cat "$(DICT_FILE)")" = "$(printf 'hello\nworld')" ] && has "ok          dict:"; then
    pass "install downloads the zip and unpacks each pinned file under its cache name"
  else fail "download install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_download_that_differs_from_its_pin_is_outdated_and_replaced() {
  download_case
  mkdir -p "$(dirname "$(DICT_FILE)")"
  printf 'stale\n' >"$(DICT_FILE)"
  run check
  local checked="$OUT" checked_rc="$RC"
  run install
  if [ "$checked_rc" -eq 1 ] && grep -qF "outdated    dict:" <<<"$checked" && [ "$RC" -eq 0 ] \
     && [ "$(cat "$(DICT_FILE)")" = "$(printf 'hello\nworld')" ]; then
    pass "a downloaded file unlike its pin is outdated, and install replaces it"
  else fail "outdated download" "rc=$RC" "$checked" "$OUT"; fi
}

test_install_refuses_a_download_unlike_its_pin() {
  download_case 0000000000000000000000000000000000000000000000000000000000000000
  run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: dict: words.dic from https://example.test/dict.zip has sha256" \
     && has "re-pin the entry" && [ ! -e "$(DICT_FILE)" ]; then
    pass "install refuses an upstream copy unlike its pin and installs nothing"
  else fail "pin mismatch" "rc=$RC" "$OUT"; fi
}

test_install_refuses_an_archive_lacking_a_pinned_file() {
  download_case
  (cd "$CASE/zip" && rm fixture.zip && printf 'x\n' >other.txt && /usr/bin/zip -q fixture.zip other.txt)
  run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: dict: https://example.test/dict.zip holds no words.dic" && [ ! -e "$(DICT_FILE)" ]; then
    pass "install refuses an archive that lacks a pinned file"
  else fail "archive lacks file" "rc=$RC" "$OUT"; fi
}

test_install_reports_a_failed_download() {
  download_case
  write_stub curl 'exit 22'
  run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: dict: downloading https://example.test/dict.zip failed" && [ ! -e "$(DICT_FILE)" ]; then
    pass "a failed download is reported and installs nothing"
  else fail "failed download" "rc=$RC" "$OUT"; fi
}

test_install_reports_an_archive_unzip_cannot_unpack() {
  download_case
  printf 'not a zip\n' >"$CASE/zip/fixture.zip"
  run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: dict: https://example.test/dict.zip is not a zip unzip can unpack" && [ ! -e "$(DICT_FILE)" ]; then
    pass "an archive unzip cannot unpack is reported and installs nothing"
  else fail "bad zip" "rc=$RC" "$OUT"; fi
}

test_uninstall_removes_a_download() {
  download_case
  run install
  run uninstall
  if [ "$RC" -eq 0 ] && [ ! -e "$(DICT_FILE)" ] && [ ! -d "$CASE/home/.cache/agent-repl/dictionaries" ] && has "uninstall: removed"; then
    pass "uninstall removes the downloaded files and their emptied dir"
  else fail "download uninstall" "rc=$RC" "$OUT"; fi
}

test_a_download_with_a_bad_archive_or_file_is_refused() {
  local bad=""
  new_case
  manifest '[{"id": "d", "kind": "download", "why": "w", "profile": "run", "url": "u", "archive": "tar", "dest": "x", "files": []}]'
  run list; [ "$RC" -eq 2 ] && has "download archive must be zip, not tar" || bad="$bad archive"
  manifest '[{"id": "d", "kind": "download", "why": "w", "profile": "run", "url": "u", "archive": "zip", "dest": "x", "files": [{"from": "a", "to": "b"}]}]'
  run list; [ "$RC" -eq 2 ] && has 'every download file needs "from", "to" and "sha256"' || bad="$bad file"
  manifest '[{"id": "d", "kind": "download", "why": "w", "profile": "run", "archive": "zip", "dest": "x", "files": []}]'
  run list; [ "$RC" -eq 2 ] && has 'download needs "url"' || bad="$bad url"
  if [ -z "$bad" ]; then pass "a download entry with a bad archive, file or field is refused"; else fail "download validation:$bad" "$OUT"; fi
}

# ---------------------------------------------------------------------------
# Output and usage.
# ---------------------------------------------------------------------------

test_check_json_reports_each_entry() {
  new_case; manifest "[$PROGRAM]"
  run check --json
  if [ "$RC" -eq 1 ] && printf '%s\n' "$OUT" | jq -e '.[0] | .id == "tool" and .status == "missing" and .fix == "brew install tool" and .system == "root" and .optional == false' >/dev/null; then
    pass "check --json reports each entry's status and fix"
  else fail "json" "rc=$RC" "$OUT"; fi
}

test_usage_errors_exit_two() {
  new_case; manifest "[$PROGRAM]"
  local bad=""
  run frobnicate; [ "$RC" -eq 2 ] && has "unknown verb: frobnicate" || bad="$bad verb"
  run install --json; [ "$RC" -eq 2 ] && has "--json is for check" || bad="$bad json"
  run check --profile sometimes; [ "$RC" -eq 2 ] && has "--profile must be run, dev or all" || bad="$bad profile"
  run; [ "$RC" -eq 2 ] && has "no verb given" || bad="$bad none"
  if [ -z "$bad" ]; then pass "usage errors exit 2 naming the mistake"; else fail "usage errors:$bad"; fi
}

test_version_ge() {
  local bad="" have want expect
  # shellcheck source=/dev/null
  REQUIREMENTS_SH_LIB=1 source "$RUNNER"
  while read -r have want expect; do
    if version_ge "$have" "$want"; then got=yes; else got=no; fi
    [ "$got" = "$expect" ] || bad="$bad $have>=$want(want $expect)"
  done <<'EOF'
3.1.0 3.0 yes
3.0 3.0 yes
3 3.0.0 yes
2.9.9 3.0 no
10.0 9.9 yes
1.24.6 1.24 yes
1.23.9 1.24 no
EOF
  if [ -z "$bad" ]; then pass "version_ge compares part by part"; else fail "version_ge:$bad"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
