#!/usr/bin/env bash
# test-land-direct.sh — hermetic tests for bin/land-direct.sh and the gate it
# lands through, bin/lib-land-gate.sh.
#
# Each fixture is a throwaway repository with a branch checked out in a linked
# worktree, either rebased onto master's head or left behind it. The full
# suite is a stub `agent-repld` on PATH that records how it was run and exits
# as the case asks.
#
# NO REAL GIT (owner rule: no test of any form runs real git). The fixtures are
# bin/fake-git.sh repositories, and the fake is the only `git` on PATH.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-grep-in.sh"
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-fake-git-fixture.sh"
clear_live_git_bindings

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$THIS_DIR/land-direct.sh"
# shellcheck source=lib-harness-report.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/lib-harness-report.sh"

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-land-direct-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# The only `git` and `agent-repld` any fixture or the script can reach.
FAKE_BIN="$TMP/bin"
mkdir -p "$FAKE_BIN"
"$THIS_DIR/fake-git.sh" --install "$FAKE_BIN/git"
cat >"$FAKE_BIN/agent-repld" <<'EOF'
#!/usr/bin/env bash
# The stub full suite: records its working directory and arguments, moves the
# branch when asked, and exits FAKE_SUITE_RC.
printf '%s|%s\n' "$PWD" "$*" >>"$FAKE_SUITE_CALLS"
if [ -n "${FAKE_SUITE_MOVES:-}" ]; then
  git -C "$FAKE_SUITE_REPO" update-ref "refs/heads/$FAKE_SUITE_MOVES" "$FAKE_SUITE_MOVE_TO"
fi
exit "${FAKE_SUITE_RC:-0}"
EOF
chmod +x "$FAKE_BIN/agent-repld"
export PATH="$FAKE_BIN:$PATH"
[ "$(command -v git)" = "$FAKE_BIN/git" ] || { echo "test-land-direct.sh: the fake git is not the git on PATH; refusing to run real git" >&2; exit 2; }

# shellcheck source=lib-land-gate.sh
. "$THIS_DIR/lib-land-gate.sh"

# mkrepo — a fake repository with a versioned lifecycle config, `fresh`
# checked out at master's head in REPO/fresh-wt, and `stale` checked out in
# REPO/stale-wt one commit behind it, at two_commit_repo's SECOND commit;
# THIRD is master's head.
mkrepo() {
  two_commit_repo "$TMP" pre-push
  REPO="$FIXTURE_REPO" SECOND="$FIXTURE_SECOND"
  # PHYS is REPO as git's worktree list names it: its physical path.
  PHYS="$(cd "$REPO" && pwd -P)"
  git -C "$REPO" worktree add -q -b stale "$REPO/stale-wt"
  mkdir -p "$REPO/.agent-repl"
  printf 'pre_deploy:\n  local_merge:\n    version_tag: {}\n' >"$REPO/.agent-repl/lifecycle.yaml"
  (cd "$REPO" && git add -A && git commit -qm three)
  THIRD="$(git -C "$REPO" rev-parse HEAD)"
  git -C "$REPO" worktree add -q -b fresh "$REPO/fresh-wt"
  export FAKE_SUITE_CALLS="$TMP/suite-calls.$RANDOM" FAKE_SUITE_REPO="$REPO" FAKE_GIT_LOG="$TMP/git-log.$RANDOM"
  unset FAKE_SUITE_RC FAKE_SUITE_MOVES FAKE_SUITE_MOVE_TO
  : >"$FAKE_SUITE_CALLS"
  : >"$FAKE_GIT_LOG"
}

# gate BRANCH — run land_gate on BRANCH. RUN_RC and RUN_OUT hold its outcome.
gate() {
  set +e
  RUN_OUT="$(land_gate "$REPO" "$1" 2>/dev/null)"
  RUN_RC=$?
  set -e
}

# land ARGS... — run the script. RUN_RC and RUN_OUT hold its outcome.
land() {
  set +e
  RUN_OUT="$("$SCRIPT" "$@" 2>&1)"
  RUN_RC=$?
  set -e
}

suite_calls() { cat "$FAKE_SUITE_CALLS"; }
merged() { grep -qF "merge --no-ff" "$FAKE_GIT_LOG"; }

# expect_refused NAME WHY — the gate refused naming WHY, before any merge.
expect_refused() {
  local name="$1" why="$2"
  if [ "$RUN_RC" -eq "$LAND_GATE_REFUSED" ] && grep_in "$RUN_OUT" -qF "$why" && ! merged; then
    pass "$name"
  else
    fail "$name" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_a_rebased_clean_branch_passes_after_the_full_suite_in_its_worktree() {
  mkrepo
  gate fresh
  if [ "$RUN_RC" -eq 0 ] && [ "$(suite_calls)" = "$PHYS/fresh-wt|test run -workspace $PHYS/fresh-wt -every-suite" ]; then
    pass "a rebased, clean branch passes once the full suite passed in its own worktree"
  else
    fail "a rebased, clean branch passes once the full suite passed in its own worktree" "exit=$RUN_RC" "$RUN_OUT" "suite: $(suite_calls)"
  fi
}

test_a_branch_behind_the_main_head_is_refused_before_the_suite() {
  mkrepo
  gate stale
  expect_refused "a branch not rebased onto the main branch's head is refused" "stale is not rebased onto the main branch's head $THIRD"
  if [ -z "$(suite_calls)" ]; then pass "a branch not rebased runs no suite"; else fail "a branch not rebased runs no suite" "$(suite_calls)"; fi
}

test_a_branch_with_uncommitted_changes_is_refused() {
  mkrepo
  echo dirt >"$REPO/fresh-wt/file"
  gate fresh
  expect_refused "a branch whose worktree holds uncommitted changes is refused" "$PHYS/fresh-wt has uncommitted changes"
}

test_a_branch_in_no_worktree_is_refused() {
  mkrepo
  git -C "$REPO" update-ref refs/heads/loose "$THIRD"
  gate loose
  expect_refused "a branch checked out in no worktree is refused" "loose is checked out in no worktree of $REPO"
}

test_a_failing_suite_is_refused() {
  mkrepo
  export FAKE_SUITE_RC=1
  gate fresh
  expect_refused "a branch the full suite fails on is refused" "the full suite failed on fresh (exit 1"
}

test_a_branch_that_moved_during_the_suite_is_refused() {
  mkrepo
  export FAKE_SUITE_MOVES=fresh FAKE_SUITE_MOVE_TO="$SECOND"
  gate fresh
  expect_refused "a branch that moved while the suite ran is refused" "moved while the full suite ran"
}

test_the_script_refuses_a_gated_branch_with_exit_4_and_merges_nothing() {
  mkrepo
  land "$REPO" patch "a line" stale
  if [ "$RUN_RC" -eq 4 ] && grep_in "$RUN_OUT" -qF "stale is not rebased" && ! merged; then
    pass "land-direct.sh exits 4 naming the gate's refusal, merging nothing"
  else
    fail "land-direct.sh exits 4 naming the gate's refusal, merging nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_the_script_lands_one_branch_only() {
  mkrepo
  land "$REPO" patch "a line" fresh stale
  if [ "$RUN_RC" -eq 2 ] && grep_in "$RUN_OUT" -qF "name exactly one branch to land, not 2" && [ -z "$(suite_calls)" ]; then
    pass "land-direct.sh refuses more than one branch before gating any"
  else
    fail "land-direct.sh refuses more than one branch before gating any" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_the_script_reads_the_version_from_the_lifecycle_config() {
  mkrepo
  land "$REPO" none "a line" fresh
  if [ "$RUN_RC" -eq 3 ] && grep_in "$RUN_OUT" -qF "the lifecycle config of $PHYS versions every landing on main" && [ -z "$(suite_calls)" ]; then
    pass "land-direct.sh reads whether main is versioned from .agent-repl/lifecycle.yaml"
  else
    fail "land-direct.sh reads whether main is versioned from .agent-repl/lifecycle.yaml" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

# The script's one merge runs only behind the gate: a merge the script makes
# before land_gate answers, or a second merge site, fails here.
test_the_script_gates_before_its_only_merge() {
  local gate_line merge_lines
  # shellcheck disable=SC2016 # the pattern names the script's own variables
  gate_line="$(grep -n 'land_gate "\$main" "\$branch"' "$SCRIPT" | cut -d: -f1)"
  merge_lines="$(grep -n 'merge --no-ff' "$SCRIPT" | cut -d: -f1)"
  if [ -n "$gate_line" ] && [ "$(wc -l <<<"$merge_lines")" -eq 1 ] && [ "$merge_lines" -gt "$gate_line" ]; then
    pass "land-direct.sh's one merge comes after its gate"
  else
    fail "land-direct.sh's one merge comes after its gate" "gate at: ${gate_line:-none}" "merges at: $merge_lines"
  fi
}

echo "test-land-direct.sh"
for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done
harness_finish
