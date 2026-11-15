#!/usr/bin/env bash
# Hermetic tests for bin/shellcheck.sh, the shell scripts' lint gate: each
# case copies the script into a fixture repository (repo/modules/app/agent-repl)
# whose roots hold the scripts the case names, and runs the real shellcheck.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"

CLEAN='#!/usr/bin/env bash
echo "clean"'
# SC2043: a loop over one literal, the warning the gate first caught.
WARNS='#!/usr/bin/env bash
for x in one; do echo "$x"; done'

# fixture PATH=BODY...: a repository whose agent-repl module holds the gate,
# with every root the gate reads present and PATH (relative to the
# repository) holding BODY for each argument. Prints the module's path.
fixture() {
  local repo module root pair
  repo="$(mktemp -d "$TMP/repo.XXXXXX")"
  module="$repo/modules/app/agent-repl"
  mkdir -p "$module/bin"
  cp "$THIS_DIR/shellcheck.sh" "$module/bin/"
  while IFS= read -r root; do
    case "$root" in
      /*) mkdir -p "$repo/${root#/}" ;;
      *) mkdir -p "$module/$root" ;;
    esac
  done < <("$module/bin/shellcheck.sh" --roots)
  for pair in "$@"; do
    mkdir -p "$(dirname "$repo/${pair%%=*}")"
    printf '%s\n' "${pair#*=}" >"$repo/${pair%%=*}"
  done
  printf '%s\n' "$module"
}

# run_gate MODULE [ARG...]: the gate's output and exit code, in OUT and RC.
run_gate() {
  local module="$1"
  shift
  RC=0
  OUT="$("$module/bin/shellcheck.sh" "$@" 2>&1)" || RC=$?
}

test_the_roots_are_the_ones_the_roster_declares() {
  local module
  module="$(fixture)"
  run_gate "$module" --roots
  if [ "$RC" -eq 0 ] && grep -qx 'bin/' <<<"$OUT" && grep -qx '/.githooks/' <<<"$OUT"; then
    pass "--roots prints the module's and the repository's roots"
  else
    fail "--roots prints the module's and the repository's roots" "rc=$RC" "$OUT"
  fi
}

test_files_lists_scripts_and_bin_shebang_files_alone() {
  local module want
  module="$(fixture \
    "modules/app/agent-repl/bin/a.sh=$CLEAN" \
    "modules/app/agent-repl/bin/tool=$CLEAN" \
    "modules/app/agent-repl/bin/data.txt=plain text" \
    "modules/app/agent-repl/bin/helper.py=#!/usr/bin/env python3" \
    "modules/app/agent-repl/skills/s/run.sh=$CLEAN" \
    "modules/app/agent-repl/skills/s/node_modules/dep/x.sh=$WARNS" \
    ".githooks/pre-commit=$CLEAN" \
    ".claude/install.sh=$CLEAN" \
    "bin/workspace=$CLEAN")"
  run_gate "$module" --files
  want='.claude/install.sh
.githooks/pre-commit
bin/workspace
modules/app/agent-repl/bin/a.sh
modules/app/agent-repl/bin/shellcheck.sh
modules/app/agent-repl/bin/tool
modules/app/agent-repl/skills/s/run.sh'
  if [ "$RC" -eq 0 ] && [ "$OUT" = "$want" ]; then
    pass "--files lists every *.sh and every sh shebang file of a bin/ or hooks directory, sorted, skipping node_modules"
  else
    fail "--files lists every *.sh and every sh shebang file of a bin/ or hooks directory, sorted, skipping node_modules" "rc=$RC" "$OUT"
  fi
}

test_files_never_descends_into_another_checkout() {
  local module
  module="$(fixture \
    ".claude/install.sh=$CLEAN" \
    ".claude/worktrees/copy/.claude/install.sh=$WARNS" \
    ".claude/skills/nested/.git=gitdir: elsewhere" \
    ".claude/skills/nested/run.sh=$WARNS")"
  run_gate "$module" --files
  if [ "$RC" -eq 0 ] && grep -qx '.claude/install.sh' <<<"$OUT" && ! grep -qF 'worktrees' <<<"$OUT" && ! grep -qF 'nested' <<<"$OUT"; then
    pass "--files skips .claude/worktrees and every directory holding a .git of its own"
  else
    fail "--files skips .claude/worktrees and every directory holding a .git of its own" "rc=$RC" "$OUT"
  fi
}

test_a_clean_tree_passes_and_counts_its_scripts() {
  local module
  module="$(fixture "modules/app/agent-repl/bin/a.sh=$CLEAN" ".githooks/pre-push=$CLEAN")"
  run_gate "$module"
  if [ "$RC" -eq 0 ] && grep -qF 'shellcheck.sh: 3 scripts are clean' <<<"$OUT"; then
    pass "a clean tree exits 0 and counts the scripts it checked"
  else
    fail "a clean tree exits 0 and counts the scripts it checked" "rc=$RC" "$OUT"
  fi
}

test_a_warning_fails_the_gate_naming_its_script() {
  local module
  module="$(fixture "modules/app/agent-repl/bin/a.sh=$CLEAN" ".claude/loop.sh=$WARNS")"
  run_gate "$module"
  if [ "$RC" -eq 1 ] && grep -qF '.claude/loop.sh' <<<"$OUT" && grep -qF 'SC2043' <<<"$OUT" && grep -qF 'shellcheck reported the warnings above' <<<"$OUT"; then
    pass "a script that warns fails the gate, which names the script and the warning"
  else
    fail "a script that warns fails the gate, which names the script and the warning" "rc=$RC" "$OUT"
  fi
}

test_a_missing_root_is_an_error_naming_it() {
  local module
  module="$(fixture)"
  rmdir "$module/wifi-helper"
  run_gate "$module"
  if [ "$RC" -eq 1 ] && grep -qF 'the root wifi-helper/' <<<"$OUT"; then
    pass "a root that is not a directory fails the gate, naming the root"
  else
    fail "a root that is not a directory fails the gate, naming the root" "rc=$RC" "$OUT"
  fi
}

test_a_missing_shellcheck_is_an_error_naming_the_installer() {
  local module bin rc=0 out
  module="$(fixture "modules/app/agent-repl/bin/a.sh=$CLEAN")"
  bin="$(mktemp -d "$TMP/bin.XXXXXX")"
  ln -s "$(command -v dirname)" "$bin/dirname"
  out="$(PATH="$bin" "$BASH" "$module/bin/shellcheck.sh" 2>&1)" || rc=$?
  if [ "$rc" -eq 1 ] && grep -qF 'shellcheck is not installed' <<<"$out"; then
    pass "a host without shellcheck fails the gate, saying so"
  else
    fail "a host without shellcheck fails the gate, saying so" "rc=$rc" "$out"
  fi
}

test_an_unknown_argument_is_refused() {
  local module
  module="$(fixture)"
  run_gate "$module" --bogus
  if [ "$RC" -eq 2 ] && grep -qF 'usage:' <<<"$OUT"; then
    pass "an unknown argument exits 2 with the usage"
  else
    fail "an unknown argument exits 2 with the usage" "rc=$RC" "$OUT"
  fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

harness_finish
