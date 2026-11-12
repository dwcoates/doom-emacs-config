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
# shellcheck source=lib-harness-report.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/lib-harness-report.sh"


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
  --prefix) printf "/brew%s\n" "${2:+/opt/$2}" ;;
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
  # systemctl --user VERB ...: a unit is active once restarted, until disabled;
  # STUB_SYSTEMCTL_FAIL names one verb that fails.
  write_stub systemctl '
shift
[ "$1" = "${STUB_SYSTEMCTL_FAIL:-}" ] && exit 1
case "$1" in
  is-active) [ -f "$STUB_DIR/.active-$3" ] ;;
  restart) : >"$STUB_DIR/.active-$2" ;;
  disable) rm -f "$STUB_DIR/.active-$3" ;;
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
    AGENT_REPL_SYSTEMCTL="$CASE/stubs/systemctl" \
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

test_every_linux_program_names_its_package_for_each_main_linux_package_manager() {
  # A Linux program comes from apt, dnf and pacman alike, so no distribution
  # family is left with "add install.<manager>"; only a program no package
  # manager carries (a go_install, a pinned release, or a manual step on every
  # platform) names none of them.
  local missing
  missing="$(cd "$MODULE" && find . -name requirements.json -not -path '*/node_modules/*' -print0 \
    | xargs -0 jq -r '.requirements[]
        | select(.kind == "program" and ((.platforms // []) | index("linux")))
        | select(.install.go_install == null and .install.release == null)
        | select((.install | keys | map(select(. == "apt" or . == "dnf" or . == "pacman" or . == "brew" or . == "brew_cask")) | length) > 0)
        | select(.install.apt == null or .install.dnf == null or .install.pacman == null)
        | .id' | sort -u)"
  if [ -z "$missing" ]; then pass "every linux program names its apt, dnf and pacman packages"; else fail "linux programs missing a package:" "$missing"; fi
}

test_the_real_manifests_load_on_linux_with_every_service() {
  set +e
  OUT="$(env -i HOME="$TMP" PATH="/usr/bin:/bin" AGENT_REPL_REQUIREMENTS_PLATFORM=linux /bin/bash "$RUNNER" list 2>&1)"
  RC=$?
  set -e
  if [ "$RC" -eq 0 ] && grep -q '^service-shim-store	' <<<"$OUT" && grep -q '^service-shim-claude-sidecar	' <<<"$OUT" \
     && grep -q '^service-zettel	' <<<"$OUT" && grep -q '^service-victoriametrics	' <<<"$OUT" && grep -q '^emacs	' <<<"$OUT"; then
    pass "the real manifests load on linux and list every service"
  else
    fail "the real manifests on linux" "rc=$RC" "$OUT"
  fi
}

# A service is one entry rendered for either service manager, so none may be
# left to one platform.
test_every_real_service_runs_on_macos_and_linux() {
  local bad
  bad="$(find "$MODULE" -name requirements.json -not -path '*/node_modules/*' -not -path '*/testdata/*' -print0 \
    | xargs -0 jq -r '.requirements[]? | select(.kind == "service") | select((.platforms | index("darwin") and index("linux")) | not) | .id')"
  if [ -z "$bad" ]; then pass "every real service runs on macOS and Linux"; else fail "services left to one platform:" "$bad"; fi
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
# The skill availability table (skills/agent-repl-install/SKILL.md).
# ---------------------------------------------------------------------------

# availability_rows AVAILABILITY: the skill names the install skill's table
# lists with AVAILABILITY (global or repository), one per line, sorted.
availability_rows() {
  sed -n 's/^| `\/\([^`]*\)` | '"$1"' | .*/\1/p' "$MODULE/skills/agent-repl-install/SKILL.md" | sort
}

# live_skill_links: the names of the skills the manifest links globally from
# this module and has not retired, one per line, sorted.
live_skill_links() {
  jq -r '.requirements[] | select(.kind == "skill_link" and (.retired | not) and ((.source.repo // "") | startswith("modules/app/agent-repl/skills/"))) | .name' \
    "$MODULE/requirements.json" | sort
}

test_every_global_skill_in_the_table_has_a_live_link_and_no_other() {
  local table links
  table="$(availability_rows global)"
  links="$(live_skill_links)"
  if [ -n "$table" ] && [ "$table" = "$links" ]; then
    pass "the availability table's global skills are exactly the manifest's live skill links"
  else
    fail "global skills and live skill links differ" "table:" "$table" "links:" "$links"
  fi
}

test_no_repository_skill_has_a_live_global_link() {
  local linked
  linked="$(comm -12 <(availability_rows repository) <(live_skill_links))"
  if [ -z "$linked" ]; then pass "no repository-only skill is linked globally"; else fail "repository-only skills linked globally:" "$linked"; fi
}

test_every_shipped_skill_has_one_availability_row() {
  local shipped listed
  shipped="$(find "$MODULE/skills" -mindepth 2 -maxdepth 2 -name SKILL.md | sed 's#.*/skills/\([^/]*\)/SKILL.md#\1#' | sort)"
  listed="$( { availability_rows global; availability_rows repository; } | sort)"
  if [ "$shipped" = "$listed" ]; then
    pass "every skill the module ships has exactly one availability row"
  else
    fail "shipped skills and availability rows differ" "shipped:" "$shipped" "listed:" "$listed"
  fi
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

test_a_command_link_with_no_name_is_refused() {
  new_case
  manifest '[{"id": "x", "kind": "command_link", "why": "w", "profile": "run", "source": {"repo": "bin/x"}}]'
  run list
  if [ "$RC" -eq 2 ] && has 'x: command_link needs "name"'; then pass "a command link with no name is refused"; else fail "command link name" "rc=$RC" "$OUT"; fi
}

test_a_command_link_sourced_outside_the_repository_is_refused() {
  new_case
  manifest '[{"id": "x", "kind": "command_link", "why": "w", "profile": "run", "name": "x", "source": {"env": "X_ROOT", "path": "x"}}]'
  run list
  if [ "$RC" -eq 2 ] && has 'x: a command_link source is a path in the repository ("repo")'; then
    pass "a command link sourced outside the repository is refused"
  else
    fail "command link source" "rc=$RC" "$OUT"
  fi
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
  if [ "$RC" -eq 1 ] && has "tool was not installed; it needs you: Download it from example.com." && [ ! -s "$CASE/transcript" ]; then
    pass "a manual program needs the user and fails the install"
  else fail "manual program" "rc=$RC" "$OUT"; fi
}

LINUX_PROGRAM='{"id": "tool", "kind": "program", "why": "the case needs it", "profile": "run", "platforms": ["linux"], "command": "tool", "install": {"apt": "tool-deb", "dnf": "tool-rpm", "pacman": ["tool-arch", "tool-extra"], "zypper": "tool-suse"}}'

# fake_linux_package_managers: sudo, apt-get, dnf, pacman and zypper stubs. sudo
# drops its -n and runs the rest, failing instead when STUB_SUDO_EXIT is set;
# an install puts `tool` on PATH.
fake_linux_package_managers() {
  write_stub sudo '[ -n "${STUB_SUDO_EXIT:-}" ] && exit "$STUB_SUDO_EXIT"; [ "$1" = -n ] && shift; exec "$@"'
  local pm
  for pm in apt-get dnf pacman zypper; do
    write_stub "$pm" 'case " $* " in *" install "*|*" -S "*) printf "#!/bin/sh\necho tool\n" >"$STUB_DIR/tool"; chmod +x "$STUB_DIR/tool" ;; esac'
  done
}

test_install_apt_installs_a_missing_linux_program_after_refreshing_its_lists() {
  new_case; manifest "[$LINUX_PROGRAM]"; fake_linux_package_managers
  PLATFORM=linux run install
  if [ "$RC" -eq 0 ] && ran "sudo -n apt-get update -q" && ran "sudo -n apt-get -o DPkg::Lock::Timeout=600 install -y tool-deb" \
     && ! ran "dnf " && has "ok          tool:"; then
    pass "install refreshes apt's lists, apt-installs a missing linux program without prompting, and re-checks it"
  else fail "apt install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_install_runs_each_linux_package_managers_own_install() {
  local pm want
  while IFS='|' read -r pm want; do
    new_case; manifest "[$LINUX_PROGRAM]"; fake_linux_package_managers
    EXTRA_ENV="AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER=$pm" PLATFORM=linux run install
    if [ "$RC" -eq 0 ] && ran "sudo -n $want" && ! ran "apt-get update"; then
      pass "install through $pm runs: $want"
    else fail "$pm install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
  done <<'CASES'
dnf|dnf install -y tool-rpm
pacman|pacman -S --needed --noconfirm tool-arch tool-extra
zypper|zypper --non-interactive install tool-suse
CASES
}

test_the_linux_package_manager_is_apt_before_dnf() {
  new_case; manifest "[$LINUX_PROGRAM]"; fake_linux_package_managers
  PLATFORM=linux run check
  if [ "$RC" -eq 1 ] && has "Fix: sudo apt-get -o DPkg::Lock::Timeout=600 install -y tool-deb"; then pass "with apt and dnf both on PATH, apt is the package manager"; else fail "manager order" "rc=$RC" "$OUT"; fi
}

test_a_failed_linux_install_names_the_command_to_run() {
  new_case; manifest "[$LINUX_PROGRAM]"; fake_linux_package_managers
  EXTRA_ENV="STUB_SUDO_EXIT=1 AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER=dnf" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: sudo dnf install -y tool-rpm failed" \
     && has "install: tool was not installed; it needs you: sudo dnf install -y tool-rpm" && has "missing     tool:"; then
    pass "a linux install sudo cannot run fails and names the command for the user"
  else fail "sudo failure" "rc=$RC" "$OUT"; fi
}

test_a_linux_entry_without_this_managers_package_says_to_add_it() {
  new_case; manifest "[$(jq -c 'del(.install.dnf)' <<<"$LINUX_PROGRAM")]"; fake_linux_package_managers
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER=dnf" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "Fix: no dnf package is declared for it; add install.dnf to its manifest entry" && ! ran "sudo"; then
    pass "a linux entry with no package for the machine's manager installs nothing and says to add one"
  else fail "no package" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_linux_entry_go_installs_without_a_package() {
  new_case
  manifest '[{"id": "tool", "kind": "program", "why": "w", "profile": "run", "platforms": ["linux"], "command": "tool", "install": {"go_install": "example.com/tool@v1"}}]'
  fake_linux_package_managers
  PLATFORM=linux run install
  if ran "go install example.com/tool@v1" && ! ran "sudo"; then pass "a linux entry with no package go-installs"; else fail "linux go install" "rc=$RC" "$(cat "$CASE/transcript")"; fi
}

test_an_unknown_package_manager_override_is_refused() {
  new_case; manifest "[$LINUX_PROGRAM]"
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER=yum" PLATFORM=linux run check
  if [ "$RC" -eq 2 ] && has "AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER=yum is not one of: apt dnf pacman zypper"; then
    pass "an unknown package manager override is refused"
  else fail "pm override" "rc=$RC" "$OUT"; fi
}

# release_case [PIN [FILE]]: a linux program installed from a pinned release: a
# tar.gz holding `tool-prod`, pinned to PIN (the archive's real sha256 when
# absent), its entry naming FILE (tool-prod when absent) as the program, served
# by a curl stub that copies the fixture to its -o path.
release_case() {
  new_case
  mkdir -p "$CASE/rel"
  printf '#!/bin/sh\necho tool 1.2.3\n' >"$CASE/rel/tool-prod"
  (cd "$CASE/rel" && /usr/bin/tar -czf fixture.tar.gz tool-prod)
  local pin="${1:-$(shasum -a 256 "$CASE/rel/fixture.tar.gz" | cut -d' ' -f1)}"
  manifest "[{\"id\": \"tool\", \"kind\": \"program\", \"why\": \"w\", \"profile\": \"run\", \"platforms\": [\"linux\"], \"command\": \"tool\", \"install\": {\"release\": {\"file\": \"${2:-tool-prod}\", \"linux-arm64\": {\"url\": \"https://example.test/tool-arm64.tar.gz\", \"sha256\": \"$pin\"}}}}]"
  write_stub curl 'while [ "$#" -gt 0 ]; do [ "$1" = -o ] && { cp "$STUB_DIR/../rel/fixture.tar.gz" "$2"; exit 0; }; shift; done; exit 7'
}

RELEASED() { printf '%s\n' "$CASE/home/.local/bin/tool"; }

test_install_installs_a_pinned_release_into_the_bin_dir() {
  release_case
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_ARCH=aarch64" PLATFORM=linux run install
  if [ "$RC" -eq 0 ] && [ -x "$(RELEASED)" ] && [ "$("$(RELEASED)")" = "tool 1.2.3" ] && ran "curl -fsSL --retry 2 -o" \
     && has "install: tool: installed $(RELEASED) from https://example.test/tool-arm64.tar.gz" && has "ok          tool: $(RELEASED)" \
     && [ ! -e "$CASE/home/.local/bin/.tool.new" ]; then
    pass "install downloads the release pinned for this machine, checks it, and installs its program into the bin dir, where check finds it"
  else fail "release install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_missing_release_program_names_the_download_as_its_fix() {
  release_case
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_ARCH=arm64" PLATFORM=linux run check
  if [ "$RC" -eq 1 ] && has "Fix: download the pinned https://example.test/tool-arm64.tar.gz and install its tool-prod as $(RELEASED)" && [ ! -s "$CASE/transcript" ]; then
    pass "a missing release program's fix names the pinned download, and check downloads nothing"
  else fail "release fix" "rc=$RC" "$OUT"; fi
}

test_a_release_unlike_its_pin_installs_nothing() {
  release_case 0000000000000000000000000000000000000000000000000000000000000000
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_ARCH=arm64" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: tool: https://example.test/tool-arm64.tar.gz has sha256" \
     && has "not the pinned 0000000000000000000000000000000000000000000000000000000000000000; the upstream copy changed, so re-pin the entry" \
     && [ ! -e "$(RELEASED)" ]; then
    pass "a release whose archive is not the pinned one is refused and installs nothing"
  else fail "release pin" "rc=$RC" "$OUT"; fi
}

test_a_machine_with_no_pinned_release_installs_nothing_and_says_to_pin_one() {
  release_case
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_ARCH=x86_64" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: tool: no release is pinned for linux-amd64; add install.release.linux-amd64 to its manifest entry" \
     && has "Fix: no release is pinned for linux-amd64" && ! ran "curl" && [ ! -e "$(RELEASED)" ]; then
    pass "a machine the entry pins no release for downloads nothing and says which pin to add"
  else fail "release machine" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_release_lacking_its_program_installs_nothing() {
  release_case "" other-name
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_ARCH=arm64" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: tool: https://example.test/tool-arm64.tar.gz holds no other-name; fix the entry's install.release.file" && [ ! -e "$(RELEASED)" ]; then
    pass "a release archive lacking the entry's program is refused and installs nothing"
  else fail "release file" "rc=$RC" "$OUT"; fi
}

test_a_failed_release_download_installs_nothing() {
  release_case
  write_stub curl 'exit 22'
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_ARCH=arm64" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: tool: downloading https://example.test/tool-arm64.tar.gz failed" && [ ! -e "$(RELEASED)" ]; then
    pass "a failed release download is reported and installs nothing"
  else fail "release download" "rc=$RC" "$OUT"; fi
}

test_a_release_that_is_not_an_archive_installs_nothing() {
  release_case
  printf 'not an archive\n' >"$CASE/rel/fixture.tar.gz"
  manifest "$(jq -c --arg pin "$(shasum -a 256 "$CASE/rel/fixture.tar.gz" | cut -d' ' -f1)" '.requirements | .[0].install.release["linux-arm64"].sha256 = $pin' "$CASE/module/requirements.json")"
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_ARCH=arm64" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: tool: https://example.test/tool-arm64.tar.gz is not a tar.gz tar can unpack" && [ ! -e "$(RELEASED)" ]; then
    pass "a release that is not a tar.gz is reported and installs nothing"
  else fail "release archive" "rc=$RC" "$OUT"; fi
}

test_a_release_lacking_a_field_is_refused() {
  local path want
  while IFS='|' read -r path want; do
    release_case
    manifest "$(jq -c "del($path) | .requirements" "$CASE/module/requirements.json")"
    PLATFORM=linux run check
    if [ "$RC" -eq 2 ] && has "$want"; then pass "a release lacking $path is refused, naming it"; else fail "release field $path" "rc=$RC" "$OUT"; fi
  done <<'CASES'
.requirements[0].install.release.file|tool: an install.release needs "file"
.requirements[0].install.release["linux-arm64"].sha256|tool: install.release.linux-arm64 needs "sha256"
.requirements[0].install.release["linux-arm64"].url|tool: install.release.linux-arm64 needs "url"
CASES
}

test_a_program_in_the_bin_dir_is_found_when_the_dir_is_not_on_path() {
  new_case; manifest "[$PROGRAM]"
  mkdir -p "$CASE/home/.local/bin" && printf '#!/bin/sh\n' >"$CASE/home/.local/bin/tool" && chmod +x "$CASE/home/.local/bin/tool"
  run check
  if [ "$RC" -eq 0 ] && has "ok          tool: $CASE/home/.local/bin/tool"; then
    pass "a program in the user's bin dir is found even when that dir is not on PATH"
  else fail "bin dir lookup" "rc=$RC" "$OUT"; fi
}

test_machine_names_the_platform_and_the_publishers_arch() {
  local bad="" arch want got
  # shellcheck source=/dev/null
  REQUIREMENTS_SH_LIB=1 source "$RUNNER"
  while read -r arch want; do
    got="$(AGENT_REPL_REQUIREMENTS_PLATFORM=linux AGENT_REPL_REQUIREMENTS_ARCH="$arch" machine)"
    [ "$got" = "$want" ] || bad="$bad [$arch -> $got, want $want]"
  done <<'CASES'
aarch64 linux-arm64
arm64 linux-arm64
x86_64 linux-amd64
riscv64 linux-riscv64
CASES
  if [ -z "$bad" ]; then pass "machine names the platform and the arch as publishers spell it"; else fail "machine:$bad"; fi
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

# fake_emacs VERSION LACKING [REVISION [CONFIGURED]]: an emacs stub answering
# --batch's questions: its version, the features it lacks, the commit it was
# built from (the one a source install's `make install` records in
# .emacs-revision, once there is one), and its configuration features.
fake_emacs() {
  cat >"$CASE/stubs/emacs" <<EOF
#!/bin/sh
case "\$3" in
  *emacs-version*) printf '%s' "$1" ;;
  *featurep*) printf '%s' "$2" ;;
  *emacs-repository-version*)
    if [ -f "\$STUB_DIR/.emacs-revision" ]; then cat "\$STUB_DIR/.emacs-revision"; else printf '%s' "${3:-}"; fi ;;
  *system-configuration-features*) printf '%s' "${4:-}" ;;
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

PINNED=cc023ace88da219c9e77740b9e180ea648fc4759

# source_emacs_entry: an emacs entry built from a source repository at PINNED,
# for macOS and Linux: its macOS app under the case's own Applications dir, its
# Linux build installed under the case's home.
source_emacs_entry() {
  printf '%s' '{"id": "emacs", "kind": "emacs", "why": "w", "profile": "run", "platforms": ["darwin", "linux"], "command": "emacs", "min": "30.2", "features": ["xwidget-internal"], "revision": "'"$PINNED"'", "configuration_features": ["XWIDGETS", "NATIVE_COMP"], "install": {"source": {"repo": "https://example.test/fork/zettel-emacs.git", "darwin": {"configuration_features": ["NS"], "deps": {"brew": ["autoconf", "tree-sitter"]}, "configure": ["--with-xwidgets", "LIBS={brew:tree-sitter}/lib/x.a", "CPPFLAGS=-I{brew}/include"], "app": "'"$CASE"'/Applications/Emacs.app"}, "linux": {"configuration_features": ["GTK3"], "deps": {"apt": ["autoconf", "libgccjit-{gcc-major}-dev", "libgtk-3-dev"], "dnf": ["autoconf", "libgccjit-devel"]}, "configure": ["--with-x-toolkit=gtk3", "--prefix={home}/.local"]}}}}'
}

# fake_source_build: git, make and the checkout's own autogen.sh and configure
# as stubs. The checkout writes autogen.sh and configure into the work tree;
# configure exits STUB_CONFIGURE_EXIT; `make install` makes the app (unless
# STUB_MAKE_NO_APP) and records the checked-out revision as the one the fake
# emacs was built from.
fake_source_build() {
  write_stub git '
dir="$2"; shift 2
case "$1" in
  checkout)
    printf "%s" "$4" >"$dir/.checked-out"
    for s in autogen.sh configure; do
      printf "#!/bin/bash\nprintf \"%%s %%s\\\\n\" \"%s\" \"\$*\" >>\"\$STUB_DIR/../transcript\"\n" "$s" >"$dir/$s"
      chmod +x "$dir/$s"
    done
    printf "exit \${STUB_CONFIGURE_EXIT:-0}\n" >>"$dir/configure" ;;
esac'
  write_stub make '
if [ "$1" = install ] && [ -z "${STUB_MAKE_NO_APP:-}" ]; then
  mkdir -p nextstep/Emacs.app && echo new >nextstep/Emacs.app/marker
  cp .checked-out "$STUB_DIR/.emacs-revision"
fi'
  mkdir -p "$CASE/Applications/Emacs.app" && echo old >"$CASE/Applications/Emacs.app/marker"
}

# no_backup: whether the case's Applications dir holds no moved-aside app.
no_backup() { ! ls -d "$CASE/Applications/Emacs-"*.app >/dev/null 2>&1; }

test_an_emacs_at_its_pinned_revision_is_ok() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" "$PINNED" "NS XWIDGETS NATIVE_COMP"
  run check
  if [ "$RC" -eq 0 ] && has "ok          emacs: $CASE/stubs/emacs Emacs 30.2 at $PINNED"; then pass "an emacs built from its pinned revision with its configuration is ok"; else fail "pinned emacs ok" "rc=$RC" "$OUT"; fi
}

test_an_emacs_built_from_another_revision_is_outdated() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "NS XWIDGETS NATIVE_COMP"
  run check
  if [ "$RC" -eq 1 ] && has "outdated    emacs: $CASE/stubs/emacs (Emacs 30.2) was built from 636f166cfc86, not the pinned $PINNED"; then pass "an emacs built from another revision is outdated, naming both"; else fail "other revision" "rc=$RC" "$OUT"; fi
}

test_an_emacs_with_no_recorded_revision_is_outdated() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" "" "NS XWIDGETS NATIVE_COMP"
  run check
  if [ "$RC" -eq 1 ] && has "was built from no recorded commit, not the pinned $PINNED"; then pass "an emacs with no recorded commit is outdated"; else fail "no revision" "rc=$RC" "$OUT"; fi
}

test_an_emacs_configured_without_a_feature_is_outdated() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" "$PINNED" "NS XWIDGETS NATIVE_COMP_X"
  run check
  if [ "$RC" -eq 1 ] && has "outdated    emacs: $CASE/stubs/emacs (Emacs 30.2) was configured without: NATIVE_COMP"; then pass "an emacs configured without a feature is outdated, naming it"; else fail "configuration feature" "rc=$RC" "$OUT"; fi
}

test_an_emacs_source_fix_names_the_build() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "NS XWIDGETS NATIVE_COMP"
  run check
  if has "Fix: build https://example.test/fork/zettel-emacs.git at the pinned revision and install it as $CASE/Applications/Emacs.app"; then pass "a source emacs's fix names the build"; else fail "source fix" "$OUT"; fi
}

test_an_emacs_source_without_a_revision_is_refused() {
  new_case; manifest "[$(source_emacs_entry | jq -c 'del(.revision)')]"
  run check
  if [ "$RC" -eq 2 ] && has 'emacs: emacs needs "revision"'; then pass "an emacs source without a revision is refused"; else fail "source no revision" "rc=$RC" "$OUT"; fi
}

test_an_emacs_source_lacking_a_field_is_refused() {
  local path want
  while IFS='|' read -r path want; do
    new_case; manifest "[$(source_emacs_entry | jq -c "del($path)")]"
    run check
    if [ "$RC" -eq 2 ] && has "$want"; then pass "an emacs source lacking $path is refused, naming it"; else fail "source field $path" "rc=$RC" "$OUT"; fi
  done <<'CASES'
.install.source.repo|emacs: an emacs install.source needs "repo"
.install.source.darwin.app|emacs: an emacs install.source.darwin needs "app"
.install.source.linux.configure|emacs: an emacs install.source.linux needs "configure"
.install.source.linux|emacs: an emacs install.source needs a "linux" build block
CASES
}

test_an_emacs_without_its_platform_blocks_configuration_feature_is_outdated() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" "$PINNED" "XWIDGETS NATIVE_COMP NS"
  PLATFORM=linux run check
  if [ "$RC" -eq 1 ] && has "was configured without: GTK3"; then pass "a linux emacs lacking the linux block's configuration feature is outdated"; else fail "platform configuration feature" "rc=$RC" "$OUT"; fi
}

# fake_linux_source_build: fake_source_build, the linux package managers, and a
# gcc whose major version is 14.
fake_linux_source_build() {
  fake_source_build
  fake_linux_package_managers
  write_stub gcc 'echo 14.2.0'
}

test_installing_a_linux_emacs_source_installs_deps_then_builds_into_the_prefix() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "GTK3 XWIDGETS NATIVE_COMP"; fake_linux_source_build
  PLATFORM=linux run install
  local src="$CASE/home/.cache/agent-repl/src/zettel-emacs" deps gccjit
  deps="$(grep -n "apt-get -o DPkg::Lock::Timeout=600 install -y autoconf libgtk-3-dev" "$CASE/transcript" | sed -n 1p | cut -d: -f1)"
  gccjit="$(grep -n "apt-get -o DPkg::Lock::Timeout=600 install -y libgccjit-14-dev" "$CASE/transcript" | sed -n 1p | cut -d: -f1)"
  if [ "$RC" -eq 0 ] && ran "sudo -n apt-get update -q" && [ -n "$deps" ] && [ -n "$gccjit" ] && [ "$deps" -lt "$gccjit" ] \
     && ran "git -C $src checkout -q --force $PINNED" \
     && ran "configure --with-x-toolkit=gtk3 --prefix=$CASE/home/.local" && ran "make install" \
     && ! ran "brew " && [ "$(cat "$CASE/Applications/Emacs.app/marker")" = old ] \
     && has "installed Emacs built from $PINNED into its configure prefix" && has "ok          emacs:"; then
    pass "installing a linux source emacs installs its deps (gccjit after gcc), then builds into the prefix"
  else fail "linux source install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_linux_emacs_source_without_this_managers_deps_builds_nothing() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "GTK3 XWIDGETS NATIVE_COMP"; fake_linux_source_build
  EXTRA_ENV="AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER=pacman" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: emacs: no pacman build dependencies are declared for it; add install.source.linux.deps.pacman to its manifest entry" \
     && ! ran "sudo" && ! ran "configure " && ! ran "make "; then
    pass "a linux emacs source with no deps for the machine's manager builds nothing and says to add them"
  else fail "linux source no deps" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_failed_linux_emacs_dependency_install_builds_nothing() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "GTK3 XWIDGETS NATIVE_COMP"; fake_linux_source_build
  EXTRA_ENV="STUB_SUDO_EXIT=1 AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER=dnf" PLATFORM=linux run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: sudo dnf install -y autoconf libgccjit-devel failed" && ! ran "configure " && ! ran "make "; then
    pass "a failed linux dependency install builds nothing"
  else fail "linux deps failure" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_installing_an_emacs_source_builds_and_replaces_the_app() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "NS XWIDGETS NATIVE_COMP"; fake_source_build
  : >"$CASE/stubs/.brew-has-autoconf"
  run install
  local src="$CASE/home/.cache/agent-repl/src/zettel-emacs" backup
  backup="$(ls -d "$CASE/Applications/Emacs-"*.app 2>/dev/null || true)"
  if [ "$RC" -eq 0 ] && ran "brew install tree-sitter" && ! ran "brew install autoconf" \
     && ran "git -C $src fetch -q --depth 1 https://example.test/fork/zettel-emacs.git $PINNED" \
     && ran "git -C $src checkout -q --force $PINNED" && ran "autogen.sh " \
     && ran "configure --with-xwidgets LIBS=/brew/opt/tree-sitter/lib/x.a CPPFLAGS=-I/brew/include" \
     && ran "make install" && [ "$(cat "$CASE/Applications/Emacs.app/marker")" = new ] \
     && [ -n "$backup" ] && [ "$(cat "$backup/marker")" = old ] \
     && has "restart Emacs to run it" && has "ok          emacs: $CASE/stubs/emacs Emacs 30.2 at $PINNED"; then
    pass "installing a source emacs builds the pinned revision and moves the old app aside"
  else fail "source install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_failed_emacs_source_configure_leaves_the_app() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "NS XWIDGETS NATIVE_COMP"; fake_source_build
  EXTRA_ENV="STUB_CONFIGURE_EXIT=1" run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: emacs: configure in $CASE/home/.cache/agent-repl/src/zettel-emacs failed; config.log there says why" \
     && ! ran "make " && [ "$(cat "$CASE/Applications/Emacs.app/marker")" = old ] && no_backup; then
    pass "a failed configure stops the build and leaves the installed app"
  else fail "configure failure" "rc=$RC" "$OUT"; fi
}

test_a_source_build_that_makes_no_app_leaves_the_app() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "NS XWIDGETS NATIVE_COMP"; fake_source_build
  EXTRA_ENV="STUB_MAKE_NO_APP=1" run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: emacs: the build made no $CASE/home/.cache/agent-repl/src/zettel-emacs/nextstep/Emacs.app" \
     && [ "$(cat "$CASE/Applications/Emacs.app/marker")" = old ] && no_backup; then
    pass "a build that makes no app leaves the installed app"
  else fail "no app" "rc=$RC" "$OUT"; fi
}

test_a_failed_emacs_source_fetch_stops_the_build() {
  new_case; manifest "[$(source_emacs_entry)]"; fake_emacs 30.2 "" 636f166cfc86 "NS XWIDGETS NATIVE_COMP"; fake_source_build
  write_stub git '[ "$3" = fetch ] && exit 1; exit 0'
  run install
  if [ "$RC" -eq 1 ] && has "ERROR: install: emacs: fetching $PINNED from https://example.test/fork/zettel-emacs.git failed" \
     && ! ran "make " && [ "$(cat "$CASE/Applications/Emacs.app/marker")" = old ] && no_backup; then
    pass "a failed fetch stops the build and leaves the installed app"
  else fail "fetch failure" "rc=$RC" "$OUT"; fi
}

test_expand_placeholders() {
  local bad="" in want got
  # shellcheck source=/dev/null
  REQUIREMENTS_SH_LIB=1 source "$RUNNER"
  brew() { if [ -n "${2:-}" ]; then echo "/hb/opt/$2"; else echo /hb; fi; }
  gcc() { echo 15.1.0; }
  while IFS='|' read -r in want; do
    got="$(HOME=/h expand_placeholders "$in")"
    [ "$got" = "$want" ] || bad="$bad [$in -> $got, want $want]"
  done <<'CASES'
--with-ns|--with-ns
CPPFLAGS=-I{brew}/include|CPPFLAGS=-I/hb/include
X={brew:tree-sitter}/lib/a.a|X=/hb/opt/tree-sitter/lib/a.a
{brew}:{brew:gmp}|/hb:/hb/opt/gmp
--prefix={home}/.local|--prefix=/h/.local
libgccjit-{gcc-major}-dev|libgccjit-15-dev
CASES
  unset -f brew gcc
  if [ -z "$bad" ]; then pass "expand_placeholders replaces every placeholder"; else fail "expand_placeholders:$bad"; fi
}

test_expand_placeholders_fails_when_gcc_cannot_say_its_version() {
  local rc=0
  # shellcheck source=/dev/null
  REQUIREMENTS_SH_LIB=1 source "$RUNNER"
  gcc() { return 1; }
  expand_placeholders "libgccjit-{gcc-major}-dev" >/dev/null 2>&1 || rc=$?
  unset -f gcc
  if [ "$rc" -ne 0 ]; then pass "expand_placeholders fails when gcc cannot say its version"; else fail "gcc-major failure was expanded"; fi
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
# service.
# ---------------------------------------------------------------------------

SVC_ENTRY='{"id": "svc", "kind": "service", "why": "w", "profile": "run", "platforms": ["darwin", "linux"], "label": "com.example.svc", "args": ["--socket", "~/.cache/agent-repl/sock/svc.sock", "--roots", "~/a,~/b", "--say", "5% of $HOME & <more>"], "log": {"stdout": "svc.out.log", "stderr": "svc.err.log"}, "build_target": "svc", "binary": "svc"}'

service_case() {
  new_case
  manifest "[$SVC_ENTRY]"
  write_stub build 'mkdir -p "$HOME/.cache/agent-repl/bin" && printf "#!/bin/sh\n" >"$HOME/.cache/agent-repl/bin/svc" && chmod +x "$HOME/.cache/agent-repl/bin/svc"'
}

test_install_builds_renders_and_loads_a_launchd_service() {
  service_case
  run install
  local plist="$CASE/home/Library/LaunchAgents/com.example.svc.plist" h="$CASE/home"
  if [ "$RC" -eq 0 ] && ran "build svc" \
     && ran "launchctl bootstrap gui/$(id -u) $plist" && ran "launchctl kickstart -k gui/$(id -u)/com.example.svc" \
     && ! ran "launchctl bootout" && ! ran "systemctl" && has "ok          svc: com.example.svc running from $plist"; then
    pass "install builds the service, renders its plist, and loads it"
  else fail "service install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
  # The rendering: env first, then the built program and its arguments with
  # every ~/ expanded and XML's own characters escaped; kept alive at
  # Standard priority; its logs under the cache.
  local want bad=""
  while IFS= read -r want; do
    grep -qF -- "$want" "$plist" || bad="$bad [$want]"
  done <<EOF
<string>com.example.svc</string>
        <string>/usr/bin/env</string>
        <string>$h/.cache/agent-repl/bin/svc</string>
        <string>$h/.cache/agent-repl/sock/svc.sock</string>
        <string>$h/a,$h/b</string>
        <string>5% of \$HOME &amp; &lt;more&gt;</string>
    <key>RunAtLoad</key>
    <key>KeepAlive</key>
    <integer>10</integer>
    <integer>20</integer>
    <string>Standard</string>
    <string>$h/.cache/agent-repl/log/svc.out.log</string>
    <string>$h/.cache/agent-repl/log/svc.err.log</string>
EOF
  if [ -z "$bad" ] && ! grep -q "EnvironmentVariables" "$plist" \
     && [ "$(grep -n '<string>' "$plist" | sed -n 2p | grep -c '/usr/bin/env')" -eq 1 ]; then
    pass "a rendered plist launches through /usr/bin/env at Standard priority with ~/ expanded and XML escaped"
  else fail "rendered plist lacks:$bad" "$(cat "$plist")"; fi
}

test_install_builds_renders_and_starts_a_systemd_service() {
  service_case
  PLATFORM=linux run install
  local unit="$CASE/home/.config/systemd/user/com.example.svc.service" h="$CASE/home"
  if [ "$RC" -eq 0 ] && ran "build svc" && ran "systemctl --user daemon-reload" && ran "systemctl --user enable com.example.svc.service" \
     && ran "systemctl --user restart com.example.svc.service" && ! ran "launchctl" \
     && has "ok          svc: com.example.svc running from $unit"; then
    pass "install builds the service, renders its unit, and enables and starts it"
  else fail "systemd service install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
  local want bad=""
  while IFS= read -r want; do
    grep -qxF -- "$want" "$unit" || bad="$bad [$want]"
  done <<EOF
ExecStart="/usr/bin/env" "$h/.cache/agent-repl/bin/svc" "--socket" "$h/.cache/agent-repl/sock/svc.sock" "--roots" "$h/a,$h/b" "--say" "5%% of \$\$HOME & <more>"
Restart=always
RestartSec=10
TimeoutStopSec=20
StandardOutput=append:$h/.cache/agent-repl/log/svc.out.log
StandardError=append:$h/.cache/agent-repl/log/svc.err.log
WantedBy=default.target
EOF
  if [ -z "$bad" ] && ! grep -q "^Environment=" "$unit"; then
    pass "a rendered unit runs through /usr/bin/env, kept alive, with ~/ expanded and systemd's % and \$ escaped"
  else fail "rendered unit lacks:$bad" "$(cat "$unit")"; fi
}

test_a_systemd_service_that_is_not_active_is_missing() {
  service_case
  PLATFORM=linux run install
  rm -f "$CASE/stubs/.active-com.example.svc.service"
  PLATFORM=linux run check
  if [ "$RC" -eq 1 ] && has "missing     svc: com.example.svc is not running under the service manager"; then
    pass "a systemd service that is not active is missing"
  else fail "inactive unit" "rc=$RC" "$OUT"; fi
}

test_a_failed_systemctl_step_fails_the_install_naming_it() {
  local verb
  for verb in daemon-reload enable restart; do
    service_case
    EXTRA_ENV="STUB_SYSTEMCTL_FAIL=$verb" PLATFORM=linux run install
    if [ "$RC" -eq 1 ] && has "ERROR: install: svc: systemctl --user $verb" && has "failed"; then
      pass "a failed systemctl $verb fails the install and names the step"
    else fail "systemctl $verb failure" "rc=$RC" "$OUT"; fi
  done
}

test_install_reloads_a_stale_service_definition() {
  service_case
  run install
  printf 'stale\n' >"$CASE/home/Library/LaunchAgents/com.example.svc.plist"
  : >"$CASE/transcript"
  run install
  if [ "$RC" -eq 0 ] && has "svc is outdated" && has "differs from the svc entry's rendering" && ran "launchctl bootout gui/$(id -u)"; then
    pass "a loaded launchd service with a stale plist is booted out and reloaded"
  else fail "service refresh" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
  service_case
  PLATFORM=linux run install
  printf 'stale\n' >"$CASE/home/.config/systemd/user/com.example.svc.service"
  : >"$CASE/transcript"
  PLATFORM=linux run install
  if [ "$RC" -eq 0 ] && has "svc is outdated" && ran "systemctl --user daemon-reload" && ran "systemctl --user restart com.example.svc.service"; then
    pass "a systemd service with a stale unit is rendered again, reloaded and restarted"
  else fail "systemd service refresh" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_uninstall_removes_the_service() {
  service_case
  run install
  run uninstall
  if [ "$RC" -eq 0 ] && [ ! -e "$CASE/home/Library/LaunchAgents/com.example.svc.plist" ] && [ ! -e "$CASE/home/.cache/agent-repl/bin/svc" ] \
     && ran "launchctl bootout gui/$(id -u)"; then pass "uninstall boots the launchd service out and removes its plist and binary"; else fail "service uninstall" "rc=$RC" "$OUT"; fi
  service_case
  PLATFORM=linux run install
  PLATFORM=linux run uninstall
  if [ "$RC" -eq 0 ] && [ ! -e "$CASE/home/.config/systemd/user/com.example.svc.service" ] && [ ! -e "$CASE/home/.cache/agent-repl/bin/svc" ] \
     && ran "systemctl --user disable --now com.example.svc.service" && [ "$(grep -c "systemctl --user daemon-reload" "$CASE/transcript")" -eq 2 ]; then
    pass "uninstall stops and disables the systemd service, removes its unit and binary, and reloads systemd"
  else fail "systemd service uninstall" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

# command_service_case: a service whose program is an installed command, with
# one config file it copies into the cache and one environment variable.
command_service_case() {
  new_case
  mkdir -p "$CASE/module/cfg"
  printf 'scrape_interval: 15s\n' >"$CASE/module/cfg/scrape.yml"
  manifest '[{"id": "svcd", "kind": "service", "why": "w", "profile": "run", "platforms": ["darwin", "linux"], "label": "com.example.svcd", "args": ["-data=~/d"], "env": {"SVCD_ROOT": "~/r"}, "log": {"stdout": "svcd.log", "stderr": "svcd.log"}, "command": "svcd", "files": [{"from": "cfg/scrape.yml", "to": "metrics/scrape.yml"}]}]'
}

test_install_loads_a_command_service_without_building_and_copies_its_files() {
  command_service_case
  tool svcd ""
  run install
  local plist="$CASE/home/Library/LaunchAgents/com.example.svcd.plist"
  if [ "$RC" -eq 0 ] && ! ran "build " && cmp -s "$CASE/module/cfg/scrape.yml" "$CASE/home/.cache/agent-repl/metrics/scrape.yml" \
     && ran "launchctl bootstrap gui/$(id -u) $plist" && has "ok          svcd: com.example.svcd running" \
     && grep -qF "<string>svcd</string>" "$plist" && grep -qF "<string>-data=$CASE/home/d</string>" "$plist" \
     && grep -qF "<string>/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin</string>" "$plist" && grep -qF "<string>$CASE/home/r</string>" "$plist"; then
    pass "install loads a command service without building it, with Homebrew on its PATH and its env, and copies its files"
  else fail "command service install" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")" "$(cat "$plist" 2>&1)"; fi
}

test_a_systemd_command_service_finds_its_command_on_the_users_path() {
  command_service_case
  tool svcd ""
  PLATFORM=linux run install
  local unit="$CASE/home/.config/systemd/user/com.example.svcd.service"
  if [ "$RC" -eq 0 ] && grep -qxF "ExecStart=\"/usr/bin/env\" \"svcd\" \"-data=$CASE/home/d\"" "$unit" \
     && grep -qxF "Environment=\"PATH=$CASE/home/.local/bin:/usr/local/bin:/usr/bin:/bin\"" "$unit" \
     && grep -qxF "Environment=\"SVCD_ROOT=$CASE/home/r\"" "$unit"; then
    pass "a systemd command service runs its command by name, with the user's bin dir on its PATH and its env"
  else fail "systemd command service" "rc=$RC" "$OUT" "$(cat "$unit" 2>&1)"; fi
}

test_a_command_service_whose_command_is_missing_is_missing_and_not_installed() {
  command_service_case
  run install
  if [ "$RC" -eq 1 ] && has "svcd is not on PATH" && ! ran "launchctl bootstrap"; then
    pass "a command service whose command is absent is missing, and install loads nothing"
  else fail "missing command service" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_changed_service_file_is_outdated_and_reinstalled() {
  command_service_case
  tool svcd ""
  run install
  printf 'scrape_interval: 30s\n' >"$CASE/module/cfg/scrape.yml"
  : >"$CASE/transcript"
  run install
  if [ "$RC" -eq 0 ] && has "svcd is outdated" && has "lacks the module's copy of metrics/scrape.yml" \
     && cmp -s "$CASE/module/cfg/scrape.yml" "$CASE/home/.cache/agent-repl/metrics/scrape.yml" && ran "launchctl kickstart -k gui/$(id -u)/com.example.svcd"; then
    pass "a service file that differs from the module's is outdated, copied again and the service restarted"
  else fail "stale service file" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_uninstall_removes_a_command_services_files_and_leaves_its_command() {
  command_service_case
  tool svcd ""
  run install
  run uninstall
  if [ "$RC" -eq 0 ] && [ ! -e "$CASE/home/.cache/agent-repl/metrics/scrape.yml" ] && [ -x "$CASE/stubs/svcd" ] \
     && [ ! -e "$CASE/home/Library/LaunchAgents/com.example.svcd.plist" ]; then
    pass "uninstall removes a command service's plist and files and leaves the installed command alone"
  else fail "command service uninstall" "rc=$RC" "$OUT"; fi
}

test_a_service_with_both_a_command_and_a_build_is_refused() {
  new_case
  manifest '[{"id": "x", "kind": "service", "why": "w", "profile": "run", "platforms": ["darwin"], "label": "l", "args": [], "log": {"stdout": "o", "stderr": "e"}, "command": "c", "build_target": "b", "binary": "b"}]'
  run check
  if [ "$RC" -eq 2 ] && has 'x: service needs either "command" or both "build_target" and "binary"'; then pass "a service naming both a command and a build is refused"; else fail "command and build" "rc=$RC" "$OUT"; fi
}

test_a_service_with_neither_a_command_nor_a_build_is_refused() {
  new_case
  manifest '[{"id": "x", "kind": "service", "why": "w", "profile": "run", "platforms": ["darwin"], "label": "l", "args": [], "log": {"stdout": "o", "stderr": "e"}, "binary": "b"}]'
  run check
  if [ "$RC" -eq 2 ] && has 'x: service needs either "command" or both "build_target" and "binary"'; then pass "a service naming neither a command nor a whole build is refused"; else fail "neither command nor build" "rc=$RC" "$OUT"; fi
}

test_a_service_file_without_a_destination_is_refused() {
  new_case
  manifest '[{"id": "x", "kind": "service", "why": "w", "profile": "run", "platforms": ["darwin"], "label": "l", "args": [], "log": {"stdout": "o", "stderr": "e"}, "command": "c", "files": [{"from": "a"}]}]'
  run check
  if [ "$RC" -eq 2 ] && has 'x: every service file needs "from" and "to"'; then pass "a service file without a destination is refused"; else fail "file without to" "rc=$RC" "$OUT"; fi
}

test_a_service_lacking_its_arguments_or_a_log_is_refused() {
  local path want
  while IFS='|' read -r path want; do
    new_case; manifest "[$(jq -c "del($path)" <<<"$SVC_ENTRY")]"
    run check
    if [ "$RC" -eq 2 ] && has "$want"; then pass "a service lacking $path is refused, naming it"; else fail "service field $path" "rc=$RC" "$OUT"; fi
  done <<'CASES'
.args|svc: service needs "args"
.log|svc: service needs "log"
.log.stderr|svc: a service log needs "stderr"
.label|svc: service needs "label"
CASES
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

harness_finish
