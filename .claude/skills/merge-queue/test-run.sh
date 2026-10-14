#!/usr/bin/env bash
# test-run.sh — hermetic tests for the merge-queue skill's run.sh.
#
# git and the daemon binary are fakes: git answers from FAKE_GIT_* bindings,
# and the daemon records its argv and exits as told. No real git runs.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../../../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/../../../modules/app/agent-repl/bin/lib-grep-in.sh"

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

# mkfixture — a main worktree, a workspace worktree, a stub directory first on
# PATH, and the fake daemon binary.
mkfixture() {
  FX="$(mktemp -d "$TMP/fx.XXXXXX")"
  mkdir -p "$FX/main" "$FX/ws/sub" "$FX/stubs"
  cat >"$FX/stubs/git" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$FAKE_GIT_LOG"
case "$*" in
  *"worktree list --porcelain")
    printf 'worktree %s\nHEAD 0000\nbranch refs/heads/master\n\n' "$FAKE_GIT_MAIN"
    [ -z "${FAKE_GIT_BRANCH_TREE:-}" ] || printf 'worktree %s\nHEAD 0001\nbranch refs/heads/feat/x\n\n' "$FAKE_GIT_BRANCH_TREE"
    ;;
  *"worktree remove "*) ;;
  *"worktree add "*) ;;
  *"branch -d "*) exit "${FAKE_GIT_BRANCH_D_EXIT:-0}" ;;
  *"branch -D "*) ;;
  "-C $FAKE_GIT_MAIN rev-parse --abbrev-ref HEAD") printf 'master\n' ;;
  *"rev-parse --abbrev-ref HEAD") printf '%s\n' "${FAKE_GIT_OWN_BRANCH:-feat/big}" ;;
  *"rev-parse --verify "*)
    c="${*: -1}"; c="${c%"^{commit}"}"
    case " ${FAKE_GIT_UNKNOWN:-} " in *" $c "*) exit 128 ;; esac
    printf 'full-%s\n' "$c"
    ;;
  *"merge-base --is-ancestor "*" HEAD")
    c="${*: -2:1}"; c="${c#full-}"
    case " ${FAKE_GIT_OFF_BRANCH:-} " in *" $c "*) exit 1 ;; esac
    ;;
  *"merge-base --is-ancestor "*" master")
    c="${*: -2:1}"; c="${c#full-}"
    case " ${FAKE_GIT_ON_TARGET:-} " in *" $c "*) exit 0 ;; esac
    exit 1
    ;;
  *"rev-list --reverse master..HEAD") printf '%s\n' ${FAKE_GIT_BRANCH_COMMITS:-} ;;
  *"cherry-pick --abort") ;;
  *"cherry-pick "*) exit "${FAKE_GIT_PICK_EXIT:-0}" ;;
  "rev-parse --show-toplevel") printf '%s\n' "$FAKE_GIT_TOP" ;;
  *"status --porcelain") printf '%s' "${FAKE_GIT_DIRTY:-}" ;;
  *) printf 'unexpected git %s\n' "$*" >&2; exit 2 ;;
esac
EOF
  cat >"$FX/daemon" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >"$FAKE_DAEMON_ARGS"
printf 'merge-queue: ws asked to merge x (command file f)\n'
exit "${FAKE_DAEMON_EXIT:-0}"
EOF
  chmod +x "$FX/stubs/git" "$FX/daemon"
}

# invoke ARGS... — run run.sh in the fixture.
invoke() {
  set +e
  RUN_OUT="$(
    PATH="$FX/stubs:$PATH" \
      FAKE_GIT_MAIN="$FX/main" \
      FAKE_GIT_TOP="${TOP:-$FX/ws}" \
      FAKE_GIT_DIRTY="${DIRTY:-}" \
      FAKE_GIT_LOG="$FX/git-log" \
      FAKE_GIT_BRANCH_TREE="${BRANCH_TREE:-}" \
      FAKE_GIT_BRANCH_D_EXIT="${BRANCH_D_EXIT:-0}" \
      FAKE_GIT_OWN_BRANCH="${OWN_BRANCH:-feat/big}" \
      FAKE_GIT_UNKNOWN="${UNKNOWN:-}" \
      FAKE_GIT_OFF_BRANCH="${OFF_BRANCH:-}" \
      FAKE_GIT_ON_TARGET="${ON_TARGET:-}" \
      FAKE_GIT_BRANCH_COMMITS="${BRANCH_COMMITS:-full-c1 full-c2 full-c3}" \
      FAKE_GIT_PICK_EXIT="${PICK_EXIT:-0}" \
      FAKE_DAEMON_ARGS="$FX/daemon-args" \
      FAKE_DAEMON_EXIT="${DAEMON_EXIT:-0}" \
      AGENT_REPL_DAEMON_BIN="$FX/daemon" \
      bash "$RUN" "$@" 2>&1
  )"
  RUN_RC=$?
  set -e
}

daemon_args() {
  cat "$FX/daemon-args" 2>/dev/null || true
}

# expect_args NAME VERB_ARGS — the run's exit is 0 and the daemon saw VERB_ARGS.
expect_args() {
  if [ "$RUN_RC" -eq 0 ] && [ "$(daemon_args)" = "$2" ]; then
    pass "$1"
  else
    fail "$1" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

# expect_refused NAME ARGS... — the run exits 2 and requests nothing.

test_each_outcome_maps_to_its_exit() {
  local verb_exit want
  for pair in "7:8" "6:5" "5:2" "2:2" "0:0"; do
    verb_exit="${pair%%:*}"
    want="${pair##*:}"
    mkfixture
    DAEMON_EXIT="$verb_exit" invoke --async fix-merge feat/x
    if [ "$RUN_RC" -eq "$want" ]; then
      pass "the verb's exit $verb_exit is the skill's exit $want"
    else
      fail "the verb's exit $verb_exit is the skill's exit $want" "exit=$RUN_RC" "$RUN_OUT"
    fi
  done
}

test_passes_the_verbs_output_through() {
  mkfixture
  invoke --one-shot
  if grep_in "$RUN_OUT" -q "merge-queue: ws asked to merge x"; then
    pass "the verb's own lines reach the caller"
  else
    fail "the verb's own lines reach the caller" "$RUN_OUT"
  fi
}

test_missing_daemon_binary_is_an_error() {
  mkfixture
  rm "$FX/daemon"
  invoke --one-shot
  if [ "$RUN_RC" -eq 2 ] && grep_in "$RUN_OUT" -q "missing or not executable"; then
    pass "a missing daemon binary is an error"
  else
    fail "a missing daemon binary is an error" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_unknown_verb_prints_usage() {
  mkfixture
  invoke --bogus
  if [ "$RUN_RC" -eq 1 ] && grep_in "$RUN_OUT" -q "usage:"; then
    pass "an unknown verb prints usage"
  else
    fail "an unknown verb prints usage" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_dequeue_own_evicts_the_own_merge() {
  mkfixture
  invoke --dequeue-own
  expect_args "--dequeue-own asks to evict this workspace's own merge" "merge-queue -evict"
}

test_dequeue_own_refuses_an_argument() {
  mkfixture
  invoke --dequeue-own extra
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ]; then
    pass "--dequeue-own refuses an argument and requests nothing"
  else
    fail "--dequeue-own refuses an argument and requests nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_dequeue_workspace_names_the_evicted_worktree() {
  mkfixture
  invoke --dequeue-workspace "$FX/other"
  expect_args "--dequeue-workspace names the other workspace's worktree" "merge-queue -evict-dir $FX/other"
}

test_dequeue_workspace_needs_a_directory() {
  mkfixture
  invoke --dequeue-workspace
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ]; then
    pass "--dequeue-workspace without a directory is an error and requests nothing"
  else
    fail "--dequeue-workspace without a directory is an error and requests nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_move_own_asks_for_its_direction() {
  local verb want
  for pair in "--move-up-own:merge-queue -move-up" "--move-down-own:merge-queue -move-down"; do
    verb="${pair%%:*}"
    want="${pair#*:}"
    mkfixture
    invoke "$verb"
    expect_args "$verb asks to move this workspace's own merge one place" "$want"
  done
}

test_move_own_refuses_an_argument() {
  local verb
  for verb in --move-up-own --move-down-own; do
    mkfixture
    invoke "$verb" extra
    if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ]; then
      pass "$verb refuses an argument and requests nothing"
    else
      fail "$verb refuses an argument and requests nothing" "exit=$RUN_RC" "$RUN_OUT"
    fi
  done
}

test_move_workspace_names_the_moved_worktree() {
  local verb want
  for pair in "--move-up-workspace:-move-up" "--move-down-workspace:-move-down"; do
    verb="${pair%%:*}"
    want="${pair#*:}"
    mkfixture
    invoke "$verb" "$FX/other"
    expect_args "$verb names the other workspace's worktree" "merge-queue $want -move-dir $FX/other"
  done
}

test_move_workspace_needs_a_directory() {
  local verb
  for verb in --move-up-workspace --move-down-workspace; do
    mkfixture
    invoke "$verb"
    if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ]; then
      pass "$verb without a directory is an error and requests nothing"
    else
      fail "$verb without a directory is an error and requests nothing" "exit=$RUN_RC" "$RUN_OUT"
    fi
  done
}

test_move_refuses_front() {
  mkfixture
  invoke --move-up-own --front
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ]; then
    pass "--move-up-own refuses --front and requests nothing"
  else
    fail "--move-up-own refuses --front and requests nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_pause_queue_pauses_every_repository() {
  mkfixture
  invoke --pause-queue
  expect_args "--pause-queue with no directory pauses every repository" "merge-queue -pause"
}

test_pause_queue_names_one_repository() {
  mkfixture
  invoke --pause-queue "$FX/main"
  expect_args "--pause-queue <dir> pauses that repository only" "merge-queue -pause -repository-dir $FX/main"
}

test_resume_queue_resumes_every_repository() {
  mkfixture
  invoke --resume-queue
  expect_args "--resume-queue with no directory resumes every repository" "merge-queue -resume"
}

test_resume_queue_names_one_repository() {
  mkfixture
  invoke --resume-queue "$FX/main"
  expect_args "--resume-queue <dir> resumes that repository only" "merge-queue -resume -repository-dir $FX/main"
}

test_hold_rollouts_holds_the_rollouts() {
  mkfixture
  invoke --hold-rollouts
  expect_args "--hold-rollouts asks the daemon to hold the rollouts" "merge-queue -hold-rollouts"
}

test_release_rollouts_releases_the_rollouts() {
  mkfixture
  invoke --release-rollouts
  expect_args "--release-rollouts asks the daemon to release the rollouts" "merge-queue -release-rollouts"
}

test_hold_rollouts_refuses_an_argument() {
  mkfixture
  invoke --hold-rollouts "$FX/main"
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ] && grep_in "$RUN_OUT" -q "daemon-wide"; then
    pass "--hold-rollouts with an argument is an error and requests nothing"
  else
    fail "--hold-rollouts with an argument is an error and requests nothing" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

test_hold_rollouts_refuses_front() {
  mkfixture
  invoke --hold-rollouts --front
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ] && grep_in "$RUN_OUT" -q "goes with a merge verb"; then
    pass "--front is refused on --hold-rollouts and nothing is asked"
  else
    fail "--front is refused on --hold-rollouts and nothing is asked" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

# git_logged LINE — the fake git saw exactly LINE.
git_logged() {
  grep -qxF -- "$1" "$FX/git-log" 2>/dev/null
}

# built_nothing — no worktree was added and the daemon was never asked.
built_nothing() {
  ! grep -q "worktree add" "$FX/git-log" 2>/dev/null && [ -z "$(daemon_args)" ]
}

# THE SKILL CALLS THE INSTALLED SCRIPT ONLY: a worktree's own copy is as old as
# its branch's base, and once refused a form master's script accepts.
test_the_skill_calls_only_the_installed_script() {
  local skill
  skill="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)/SKILL.md"
  if grep -nE '(call `|Bash\()\.claude/skills/merge-queue/run\.sh' "$skill" >/dev/null; then
    fail "SKILL.md names a worktree's own run.sh; it must call ~/.claude/skills/merge-queue/run.sh"
  else
    pass "SKILL.md calls only the installed ~/.claude/skills/merge-queue/run.sh"
  fi
}

test_front_is_refused_on_a_queue_control() {
  mkfixture
  invoke --pause-queue --front
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ] && grep_in "$RUN_OUT" -q "goes with a merge verb"; then
    pass "--front is refused on a queue control and nothing is asked"
  else
    fail "--front is refused on a queue control and nothing is asked" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

test_one_shot_announces_the_own_branch() {
  mkfixture
  invoke --one-shot
  expect_args "--one-shot announces this workspace's own branch a one-shot" "merge-queue -one-shot"
}

test_one_shot_pr_merged_names_the_upstream_landing() {
  mkfixture
  invoke --one-shot --pr-merged
  expect_args "--one-shot --pr-merged asks for the upstream update" "merge-queue -one-shot -pr-merged"
}

test_one_shot_allows_uncommitted_work() {
  mkfixture
  DIRTY=" M file.el" invoke --one-shot
  expect_args "--one-shot announces with work still uncommitted" "merge-queue -one-shot"
}

test_one_shot_refuses_the_main_worktree() {
  mkfixture
  TOP="$FX/main" invoke --one-shot
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ] && grep_in "$RUN_OUT" -q "never one-shots"; then
    pass "--one-shot refuses the repository's main worktree and asks nothing"
  else
    fail "--one-shot refuses the repository's main worktree and asks nothing" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

test_one_shot_refusal_is_exit_5() {
  mkfixture
  DAEMON_EXIT=6 invoke --one-shot
  if [ "$RUN_RC" -eq 5 ]; then
    pass "a refused one-shot is exit 5"
  else
    fail "a refused one-shot is exit 5" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_async_names_its_merge_workspace_and_branch() {
  mkfixture
  invoke --async fix-merge feat/x
  expect_args "--async asks for the branch under its merge workspace's name" "merge-queue -async -name fix-merge -branch feat/x"
}

test_async_carries_every_option() {
  mkfixture
  invoke --async fix-merge feat/x --instruction "rebase it onto master" --close "$FX/other" --cleanup --compact-after
  expect_args "--async carries its instruction, its one-shot workspace, its cleanup and its compaction" \
    "merge-queue -async -name fix-merge -branch feat/x -instruction rebase it onto master -close-dir $FX/other -cleanup-worktree -compact-after"
}

test_async_carries_its_own_merge_actions() {
  mkfixture
  invoke --async fix-merge feat/x --before-merge "run the linters" --after-merge "post the summary"
  expect_args "--async carries the merge workspace's own before and after merge prompts" \
    "merge-queue -async -name fix-merge -branch feat/x -before-merge run the linters -after-merge post the summary"
}

test_async_runs_from_the_main_worktree() {
  mkfixture
  TOP="$FX/main" invoke --async fix-merge feat/x
  expect_args "--async runs from the repository's main worktree" "merge-queue -async -name fix-merge -branch feat/x"
}

expect_refused() {
  local name="$1"
  shift
  mkfixture
  invoke "$@"
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ]; then
    pass "$name"
  else
    fail "$name" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
  fi
}

test_merge_verbs_refuse_malformed_forms() {
  expect_refused "--one-shot refuses an option other than --pr-merged" --one-shot --cleanup
  expect_refused "--one-shot --pr-merged refuses anything after it" --one-shot --pr-merged --compact-after
  expect_refused "--async refuses a missing name" --async
  expect_refused "--async refuses a missing branch" --async fix-merge
  expect_refused "--async refuses an option as its name" --async --cleanup feat/x
  expect_refused "--async refuses a missing instruction" --async fix-merge feat/x --instruction
  expect_refused "--async refuses a missing --close directory" --async fix-merge feat/x --close
  expect_refused "--async refuses an unknown option" --async fix-merge feat/x --keep-open
  expect_refused "--async refuses a missing --before-merge prompt" --async fix-merge feat/x --before-merge
  expect_refused "--async refuses a missing --after-merge prompt" --async fix-merge feat/x --after-merge
  expect_refused "--one-shot refuses --before-merge" --one-shot --before-merge "run the linters"
}

test_front_rides_a_one_shot() {
  mkfixture
  invoke --front --one-shot
  expect_args "--front before the verb still rides --one-shot" "merge-queue -one-shot -front"
}

test_front_rides_an_async_merge() {
  mkfixture
  invoke --async fix-merge feat/x --front
  expect_args "--async --front asks for the front of the queue" "merge-queue -async -name fix-merge -branch feat/x -front"
}

test_minor_rides_a_one_shot() {
  mkfixture
  invoke --one-shot --minor "Merges record a version"
  expect_args "--one-shot --minor moves the version's minor with its changelog line" "merge-queue -one-shot -version-part minor -changelog-line Merges record a version"
}

test_patch_rides_an_async_merge_beside_front() {
  mkfixture
  invoke --patch "Fix the footer" --async fix-merge feat/x --front
  expect_args "--patch before the verb rides --async beside --front" "merge-queue -async -name fix-merge -branch feat/x -front -version-part patch -changelog-line Fix the footer"
}

test_version_modifiers_refuse_malformed_forms() {
  expect_refused "--minor refuses a missing changelog line" --one-shot --minor
  expect_refused "--patch refuses an option as its changelog line" --one-shot --patch --front
  expect_refused "--minor and --patch together are refused" --one-shot --minor a --patch b
  expect_refused "--patch is refused on a queue control" --pause-queue --patch "Fix the footer"
}

test_removed_merge_verbs_print_usage() {
  local verb ok=1
  for verb in --enqueue-own --announce-one-shot --land-workspace --land-branch --land-commits --pr-merged --remove-branch; do
    mkfixture
    invoke "$verb"
    if [ "$RUN_RC" -ne 1 ] || [ -n "$(daemon_args)" ]; then
      ok=0
      fail "$verb is no verb any more" "exit=$RUN_RC" "$RUN_OUT"
    fi
  done
  [ "$ok" -eq 0 ] || pass "every removed merge verb prints usage and asks nothing"
}

test_each_outcome_maps_to_its_exit
test_passes_the_verbs_output_through
test_missing_daemon_binary_is_an_error
test_unknown_verb_prints_usage
test_dequeue_own_evicts_the_own_merge
test_dequeue_own_refuses_an_argument
test_dequeue_workspace_names_the_evicted_worktree
test_dequeue_workspace_needs_a_directory
test_move_own_asks_for_its_direction
test_move_own_refuses_an_argument
test_move_workspace_names_the_moved_worktree
test_move_workspace_needs_a_directory
test_move_refuses_front
test_pause_queue_pauses_every_repository
test_pause_queue_names_one_repository
test_resume_queue_resumes_every_repository
test_resume_queue_names_one_repository
test_hold_rollouts_holds_the_rollouts
test_release_rollouts_releases_the_rollouts
test_hold_rollouts_refuses_an_argument
test_hold_rollouts_refuses_front
test_the_skill_calls_only_the_installed_script
test_front_is_refused_on_a_queue_control
test_one_shot_announces_the_own_branch
test_one_shot_pr_merged_names_the_upstream_landing
test_one_shot_allows_uncommitted_work
test_one_shot_refuses_the_main_worktree
test_one_shot_refusal_is_exit_5
test_async_names_its_merge_workspace_and_branch
test_async_carries_every_option
test_async_carries_its_own_merge_actions
test_async_runs_from_the_main_worktree
test_merge_verbs_refuse_malformed_forms
test_front_rides_a_one_shot
test_front_rides_an_async_merge
test_minor_rides_a_one_shot
test_patch_rides_an_async_merge_beside_front
test_version_modifiers_refuse_malformed_forms
test_removed_merge_verbs_print_usage

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
