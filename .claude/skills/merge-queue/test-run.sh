#!/usr/bin/env bash
# test-run.sh — hermetic tests for the merge-queue skill's run.sh.
#
# git, the daemon binary and the log reader are all fakes: git answers from
# FAKE_GIT_* bindings, the daemon records its argv and exits as told, and the
# log reader prints one scripted record. No real git runs.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../../../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
RUN="$THIS_DIR/run.sh"
PASS=0
FAIL=0

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-merge-queue-skill-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

pass() {
  printf '  PASS: %s\n' "$1"
  PASS=$((PASS + 1))
}

fail() {
  printf '  FAIL: %s\n' "$1" >&2
  FAIL=$((FAIL + 1))
  shift
  if [ "$#" -gt 0 ]; then
    printf '%s\n' "$@" | sed 's/^/        /' >&2
  fi
}

# mkfixture — a main worktree holding the fake log reader, a workspace
# worktree, a stub directory first on PATH, and the fake daemon binary.
mkfixture() {
  FX="$(mktemp -d "$TMP/fx.XXXXXX")"
  mkdir -p "$FX/main/modules/app/agent-repl/bin" "$FX/ws/sub" "$FX/stubs"
  cat >"$FX/stubs/git" <<'EOF'
#!/usr/bin/env bash
case "$*" in
  "worktree list --porcelain") printf 'worktree %s\nHEAD 0000\n' "$FAKE_GIT_MAIN" ;;
  "rev-parse --show-toplevel") printf '%s\n' "$FAKE_GIT_TOP" ;;
  *"status --porcelain") printf '%s' "${FAKE_GIT_DIRTY:-}" ;;
  *) printf 'unexpected git %s\n' "$*" >&2; exit 2 ;;
esac
EOF
  cat >"$FX/daemon" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >"$FAKE_DAEMON_ARGS"
printf 'merge-queue: enqueued x (command file f)\nmerge-queue: worktree: %s\n' "$FAKE_DAEMON_WORKTREE"
exit "${FAKE_DAEMON_EXIT:-0}"
EOF
  cat >"$FX/main/modules/app/agent-repl/bin/logs.sh" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >"$FAKE_LOGS_ARGS"
printf '{"operation":"daemon.merge.abort","context":{"summary":"the gate failed: TestX"}}\n'
printf '{"operation":"daemon.other","context":{}}\n'
EOF
  chmod +x "$FX/stubs/git" "$FX/daemon" "$FX/main/modules/app/agent-repl/bin/logs.sh"
}

# invoke ARGS... — run run.sh in the fixture.
invoke() {
  set +e
  RUN_OUT="$(
    PATH="$FX/stubs:$PATH" \
      FAKE_GIT_MAIN="$FX/main" \
      FAKE_GIT_TOP="${TOP:-$FX/ws}" \
      FAKE_GIT_DIRTY="${DIRTY:-}" \
      FAKE_DAEMON_ARGS="$FX/daemon-args" \
      FAKE_DAEMON_WORKTREE="$FX/ws" \
      FAKE_DAEMON_EXIT="${DAEMON_EXIT:-0}" \
      FAKE_LOGS_ARGS="$FX/logs-args" \
      AGENT_REPL_DAEMON_BIN="$FX/daemon" \
      bash "$RUN" "$@" 2>&1
  )"
  RUN_RC=$?
  set -e
}

daemon_args() {
  cat "$FX/daemon-args" 2>/dev/null || true
}

test_enqueue_own_merges_the_workspace_without_waiting() {
  mkfixture
  invoke --enqueue-own
  if [ "$RUN_RC" -eq 0 ] && [ "$(daemon_args)" = "merge-queue -dir $FX/ws" ]; then
    pass "--enqueue-own enqueues this workspace without -wait"
  else
    fail "--enqueue-own enqueues this workspace without -wait" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

test_enqueue_own_refuses_uncommitted_work() {
  mkfixture
  DIRTY=" M file.el" invoke --enqueue-own
  if [ "$RUN_RC" -eq 6 ] && [ -z "$(daemon_args)" ]; then
    pass "--enqueue-own refuses uncommitted work and enqueues nothing"
  else
    fail "--enqueue-own refuses uncommitted work and enqueues nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enqueue_own_refuses_the_main_worktree() {
  mkfixture
  TOP="$FX/main" invoke --enqueue-own
  if [ "$RUN_RC" -eq 2 ] && printf '%s' "$RUN_OUT" | grep -q "main worktree"; then
    pass "--enqueue-own refuses the repository's main worktree"
  else
    fail "--enqueue-own refuses the repository's main worktree" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_workspace_waits() {
  mkfixture
  TOP="$FX/main" invoke --land-workspace "$FX/other"
  if [ "$RUN_RC" -eq 0 ] && [ "$(daemon_args)" = "merge-queue -dir $FX/other -wait" ]; then
    pass "--land-workspace enqueues another workspace and waits"
  else
    fail "--land-workspace enqueues another workspace and waits" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

test_land_workspace_refuses_its_own() {
  mkfixture
  invoke --land-workspace "$FX/ws"
  if [ "$RUN_RC" -eq 7 ] && [ -z "$(daemon_args)" ]; then
    pass "--land-workspace refuses to wait on its own workspace"
  else
    fail "--land-workspace refuses to wait on its own workspace" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_branch_names_the_main_worktree() {
  mkfixture
  invoke --land-branch feat/x
  if [ "$RUN_RC" -eq 0 ] && [ "$(daemon_args)" = "merge-queue -branch feat/x -repo $FX/main -wait" ]; then
    pass "--land-branch lands the branch through the repository's main worktree"
  else
    fail "--land-branch lands the branch through the repository's main worktree" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

test_each_outcome_maps_to_its_exit() {
  local verb_exit want
  for pair in "4:3" "6:5" "2:2" "0:0"; do
    verb_exit="${pair%%:*}"
    want="${pair##*:}"
    mkfixture
    TOP="$FX/main" DAEMON_EXIT="$verb_exit" invoke --land-branch feat/x
    if [ "$RUN_RC" -eq "$want" ]; then
      pass "the verb's exit $verb_exit is the skill's exit $want"
    else
      fail "the verb's exit $verb_exit is the skill's exit $want" "exit=$RUN_RC" "$RUN_OUT"
    fi
  done
}

test_failure_prints_the_recorded_reason() {
  mkfixture
  TOP="$FX/main" DAEMON_EXIT=5 invoke --land-branch feat/x
  if [ "$RUN_RC" -eq 4 ] && printf '%s' "$RUN_OUT" | grep -q "the gate failed: TestX" &&
    grep -q -- "--workspace $FX/ws" "$FX/logs-args"; then
    pass "a failed merge prints the reason from the worktree the verb named"
  else
    fail "a failed merge prints the reason from the worktree the verb named" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_passes_the_verbs_output_through() {
  mkfixture
  invoke --enqueue-own
  if printf '%s' "$RUN_OUT" | grep -q "merge-queue: enqueued x"; then
    pass "the verb's own lines reach the caller"
  else
    fail "the verb's own lines reach the caller" "$RUN_OUT"
  fi
}

test_missing_daemon_binary_is_an_error() {
  mkfixture
  rm "$FX/daemon"
  invoke --enqueue-own
  if [ "$RUN_RC" -eq 2 ] && printf '%s' "$RUN_OUT" | grep -q "missing or not executable"; then
    pass "a missing daemon binary is an error"
  else
    fail "a missing daemon binary is an error" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_unknown_verb_prints_usage() {
  mkfixture
  invoke --bogus
  if [ "$RUN_RC" -eq 1 ] && printf '%s' "$RUN_OUT" | grep -q "usage:"; then
    pass "an unknown verb prints usage"
  else
    fail "an unknown verb prints usage" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enqueue_own_merges_the_workspace_without_waiting
test_enqueue_own_refuses_uncommitted_work
test_enqueue_own_refuses_the_main_worktree
test_land_workspace_waits
test_land_workspace_refuses_its_own
test_land_branch_names_the_main_worktree
test_each_outcome_maps_to_its_exit
test_failure_prints_the_recorded_reason
test_passes_the_verbs_output_through
test_missing_daemon_binary_is_an_error
test_unknown_verb_prints_usage

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
