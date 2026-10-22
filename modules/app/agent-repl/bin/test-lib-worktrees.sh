#!/usr/bin/env bash
# Hermetic tests for bin/lib-worktrees.sh, the one reading of a repository's
# worktree list, run against the fake git (no test runs real git).

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
REPO_DIR="$(cd "$THIS_DIR/../../../.." && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin"
ln -s "$THIS_DIR/fake-git.sh" "$TMP/bin/git"
export PATH="$TMP/bin:$PATH"
[ "$(command -v git)" = "$TMP/bin/git" ] || { echo "test-lib-worktrees.sh: the fake git is not the git on PATH; refusing to run real git" >&2; exit 2; }

# shellcheck source=/dev/null
. "$THIS_DIR/lib-worktrees.sh"

# shellcheck source=lib-harness-report.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/lib-harness-report.sh"

# repo_with_linked N: a fake-git repository with N linked worktrees.
repo_with_linked() {
  local repo i
  repo="$(mktemp -d "$TMP/repo.XXXXXX")"
  repo="$(cd "$repo" && pwd -P)"
  git -C "$repo" init >/dev/null
  echo x >"$repo/file"
  git -C "$repo" add -A
  git -C "$repo" commit -qm init >/dev/null
  for ((i = 1; i <= $1; i++)); do
    git -C "$repo" worktree add -q --detach "$repo/wt$i" >/dev/null
  done
  printf '%s\n' "$repo"
}

test_worktree_list_names_the_main_worktree_first() {
  local repo got
  repo="$(repo_with_linked 2)"
  got="$(worktree_list "$repo")"
  if [ "$got" = "$repo"$'\n'"$repo/wt1"$'\n'"$repo/wt2" ]; then pass "worktree_list names the main worktree first, then each linked one"; else fail "worktree_list" "$got"; fi
}

test_main_worktree_is_the_first_entry_from_a_linked_worktree_too() {
  local repo got
  repo="$(repo_with_linked 1)"
  got="$(main_worktree "$repo/wt1")"
  if [ "$got" = "$repo" ]; then pass "main_worktree answers the main worktree from inside a linked one"; else fail "main_worktree" "$got"; fi
}

test_linked_worktrees_leaves_the_main_worktree_out() {
  local repo got
  repo="$(repo_with_linked 2)"
  got="$(linked_worktrees "$repo")"
  if [ "$got" = "$repo/wt1"$'\n'"$repo/wt2" ]; then pass "linked_worktrees leaves the main worktree out"; else fail "linked_worktrees" "$got"; fi
}

test_linked_worktrees_is_empty_with_none() {
  local repo got
  repo="$(repo_with_linked 0)"
  got="$(linked_worktrees "$repo")"
  if [ -z "$got" ]; then pass "linked_worktrees is empty with no linked worktree"; else fail "no linked worktrees" "$got"; fi
}

test_every_function_fails_outside_a_repository() {
  local dir bad=""
  dir="$(mktemp -d "$TMP/plain.XXXXXX")"
  worktree_list "$dir" >/dev/null 2>&1 && bad="$bad worktree_list"
  main_worktree "$dir" >/dev/null 2>&1 && bad="$bad main_worktree"
  linked_worktrees "$dir" >/dev/null 2>&1 && bad="$bad linked_worktrees"
  if [ -z "$bad" ]; then pass "every function answers non-zero outside a repository"; else fail "these answered zero outside a repository:$bad"; fi
}

# The call sites share the helper: no script but the library reads the
# worktree list's roots itself. ALLOWED would name a script that reads the
# list's BRANCH lines instead (which worktree holds which branch), a different
# question the library does not answer, with its reason; none does today.
ALLOWED=()

test_no_script_parses_the_worktree_list_itself() {
  local found f rel allowed entry real=""
  found="$(grep -rlE 'worktree list --porcelain' --include='*.sh' "$REPO_DIR/.claude" "$REPO_DIR/modules/app/agent-repl/bin" "$REPO_DIR/modules/app/agent-repl/scripts" || true)"
  for f in $found; do
    rel="${f#"$REPO_DIR"/}"
    case "$rel" in */lib-worktrees.sh|*/fake-git.sh|*/test-*.sh) continue ;; esac
    allowed=0
    for entry in ${ALLOWED[@]+"${ALLOWED[@]}"}; do
      [ "${entry%%|*}" = "$rel" ] && allowed=1
    done
    [ "$allowed" -eq 1 ] || real="$real $rel"
  done
  if [ -z "$real" ]; then pass "no script reads the worktree list's roots outside lib-worktrees.sh"; else fail "these scripts parse the worktree list themselves; source lib-worktrees.sh instead:$real"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

harness_finish
