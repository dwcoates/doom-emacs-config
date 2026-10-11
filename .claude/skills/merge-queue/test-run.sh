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

test_enqueue_own_requests_the_own_branch() {
  mkfixture
  invoke --enqueue-own
  expect_args "--enqueue-own requests this workspace's own branch without waiting" "merge-queue -own"
}

test_enqueue_own_keep_open_keeps_the_workspace() {
  mkfixture
  invoke --enqueue-own --keep-open
  expect_args "--enqueue-own --keep-open asks to keep this workspace open" "merge-queue -own -keep-open"
}

test_enqueue_own_in_place_runs_in_this_workspace() {
  mkfixture
  invoke --enqueue-own --in-place
  expect_args "--enqueue-own --in-place asks for the merge to run in this workspace" "merge-queue -own -in-place"
}

test_enqueue_own_refuses_an_unknown_option() {
  mkfixture
  invoke --enqueue-own --bogus
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ]; then
    pass "--enqueue-own refuses an option other than --in-place or --keep-open"
  else
    fail "--enqueue-own refuses an option other than --in-place or --keep-open" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enqueue_own_refuses_uncommitted_work() {
  mkfixture
  DIRTY=" M file.el" invoke --enqueue-own
  if [ "$RUN_RC" -eq 6 ] && [ -z "$(daemon_args)" ]; then
    pass "--enqueue-own refuses uncommitted work and requests nothing"
  else
    fail "--enqueue-own refuses uncommitted work and requests nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enqueue_own_refuses_the_main_worktree() {
  mkfixture
  TOP="$FX/main" invoke --enqueue-own
  if [ "$RUN_RC" -eq 2 ] && grep_in "$RUN_OUT" -q "main worktree"; then
    pass "--enqueue-own refuses the repository's main worktree"
  else
    fail "--enqueue-own refuses the repository's main worktree" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_announce_one_shot_requests_the_one_shot_merge() {
  mkfixture
  invoke --announce-one-shot
  expect_args "--announce-one-shot announces this workspace's own branch a one-shot" "merge-queue -own -one-shot"
}

test_announce_one_shot_front_goes_to_the_front() {
  mkfixture
  invoke --announce-one-shot --front
  expect_args "--announce-one-shot --front announces the one-shot at the front of the queue" "merge-queue -own -one-shot -front"
}

test_announce_one_shot_allows_uncommitted_work() {
  mkfixture
  DIRTY=" M file.el" invoke --announce-one-shot
  expect_args "--announce-one-shot announces with work still uncommitted" "merge-queue -own -one-shot"
}

test_announce_one_shot_refuses_an_argument() {
  mkfixture
  invoke --announce-one-shot --keep-open
  if [ "$RUN_RC" -eq 2 ] && [ -z "$(daemon_args)" ]; then
    pass "--announce-one-shot refuses an argument and requests nothing"
  else
    fail "--announce-one-shot refuses an argument and requests nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_announce_one_shot_refusal_is_exit_5() {
  mkfixture
  DAEMON_EXIT=6 invoke --announce-one-shot
  if [ "$RUN_RC" -eq 5 ]; then
    pass "a refused one-shot announcement exits 5"
  else
    fail "a refused one-shot announcement exits 5" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_workspace_requests_without_waiting() {
  mkfixture
  invoke --land-workspace "$FX/other"
  expect_args "--land-workspace requests another workspace's branch without waiting" "merge-queue -dir $FX/other"
}

test_land_workspace_refuses_its_own() {
  mkfixture
  invoke --land-workspace "$FX/ws"
  if [ "$RUN_RC" -eq 7 ] && [ -z "$(daemon_args)" ]; then
    pass "--land-workspace refuses this shell's own workspace"
  else
    fail "--land-workspace refuses this shell's own workspace" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_branch_requests_the_branch() {
  mkfixture
  invoke --land-branch feat/x
  expect_args "--land-branch requests the branch without waiting" "merge-queue -branch feat/x"
}

test_pr_merged_requests_the_upstream_update() {
  mkfixture
  invoke --pr-merged
  expect_args "--pr-merged requests the merged-upstream update" "merge-queue -pr-merged"
}

test_remove_branch_removes_its_worktree_and_branch() {
  mkfixture
  BRANCH_TREE="$FX/sub-tree" invoke --remove-branch feat/x
  if [ "$RUN_RC" -eq 0 ] && grep -qx -- "-C $FX/main worktree remove $FX/sub-tree" "$FX/git-log" &&
    grep -qx -- "-C $FX/main branch -d feat/x" "$FX/git-log"; then
    pass "--remove-branch removes the branch's worktree, then the branch"
  else
    fail "--remove-branch removes the branch's worktree, then the branch" "exit=$RUN_RC" "$RUN_OUT" "$(cat "$FX/git-log")"
  fi
}

test_remove_branch_without_a_worktree_deletes_the_branch() {
  mkfixture
  invoke --remove-branch feat/x
  if [ "$RUN_RC" -eq 0 ] && ! grep -q "worktree remove" "$FX/git-log" &&
    grep -qx -- "-C $FX/main branch -d feat/x" "$FX/git-log"; then
    pass "--remove-branch with no worktree only deletes the branch"
  else
    fail "--remove-branch with no worktree only deletes the branch" "exit=$RUN_RC" "$RUN_OUT" "$(cat "$FX/git-log")"
  fi
}

test_remove_branch_refuses_an_unlanded_branch() {
  mkfixture
  BRANCH_D_EXIT=1 invoke --remove-branch feat/x
  if [ "$RUN_RC" -eq 2 ] && grep_in "$RUN_OUT" -q "may not have landed"; then
    pass "--remove-branch fails on a branch master lacks"
  else
    fail "--remove-branch fails on a branch master lacks" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_each_outcome_maps_to_its_exit() {
  local verb_exit want
  for pair in "7:8" "6:5" "5:2" "2:2" "0:0"; do
    verb_exit="${pair%%:*}"
    want="${pair##*:}"
    mkfixture
    DAEMON_EXIT="$verb_exit" invoke --land-branch feat/x
    if [ "$RUN_RC" -eq "$want" ]; then
      pass "the verb's exit $verb_exit is the skill's exit $want"
    else
      fail "the verb's exit $verb_exit is the skill's exit $want" "exit=$RUN_RC" "$RUN_OUT"
    fi
  done
}

test_passes_the_verbs_output_through() {
  mkfixture
  invoke --enqueue-own
  if grep_in "$RUN_OUT" -q "merge-queue: ws asked to merge x"; then
    pass "the verb's own lines reach the caller"
  else
    fail "the verb's own lines reach the caller" "$RUN_OUT"
  fi
}

test_missing_daemon_binary_is_an_error() {
  mkfixture
  rm "$FX/daemon"
  invoke --enqueue-own
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

# git_logged LINE — the fake git saw exactly LINE.
git_logged() {
  grep -qxF -- "$1" "$FX/git-log" 2>/dev/null
}

# built_nothing — no worktree was added and the daemon was never asked.
built_nothing() {
  ! grep -q "worktree add" "$FX/git-log" 2>/dev/null && [ -z "$(daemon_args)" ]
}

test_land_commits_builds_the_named_commits_in_branch_order() {
  mkfixture
  invoke --land-commits c3 c1
  if [ "$RUN_RC" -eq 0 ] &&
    git_logged "-C $FX/main worktree add -b feat/big-partial-full-c3 $FX/feat-big-partial-full-c3 master" &&
    git_logged "-C $FX/feat-big-partial-full-c3 cherry-pick full-c1 full-c3" &&
    [ "$(daemon_args)" = "merge-queue -branch feat/big-partial-full-c3" ]; then
    pass "--land-commits cherry-picks the named commits in branch order onto master and requests that branch"
  else
    fail "--land-commits cherry-picks the named commits in branch order onto master and requests that branch" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT" "$(cat "$FX/git-log")"
  fi
}

test_land_commits_needs_a_commit() {
  mkfixture
  invoke --land-commits
  if [ "$RUN_RC" -eq 2 ] && built_nothing; then
    pass "--land-commits with no commit is an error and builds nothing"
  else
    fail "--land-commits with no commit is an error and builds nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_commits_refuses_the_main_worktree() {
  mkfixture
  TOP="$FX/main" invoke --land-commits c1
  if [ "$RUN_RC" -eq 2 ] && built_nothing && grep_in "$RUN_OUT" -q "main worktree"; then
    pass "--land-commits refuses the repository's main worktree"
  else
    fail "--land-commits refuses the repository's main worktree" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_commits_refuses_an_unknown_commit() {
  mkfixture
  UNKNOWN=c9 invoke --land-commits c1 c9
  if [ "$RUN_RC" -eq 2 ] && built_nothing && grep_in "$RUN_OUT" -q "c9 names no commit"; then
    pass "--land-commits refuses a name that is no commit and builds nothing"
  else
    fail "--land-commits refuses a name that is no commit and builds nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_commits_refuses_a_commit_off_the_branch() {
  mkfixture
  OFF_BRANCH=c2 invoke --land-commits c2
  if [ "$RUN_RC" -eq 2 ] && built_nothing && grep_in "$RUN_OUT" -q "not on this workspace's branch"; then
    pass "--land-commits refuses a commit that is not on this workspace's branch"
  else
    fail "--land-commits refuses a commit that is not on this workspace's branch" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_commits_refuses_a_commit_already_on_master() {
  mkfixture
  ON_TARGET=c1 invoke --land-commits c1
  if [ "$RUN_RC" -eq 2 ] && built_nothing && grep_in "$RUN_OUT" -q "already on master"; then
    pass "--land-commits refuses a commit master already has"
  else
    fail "--land-commits refuses a commit master already has" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_land_commits_conflict_leaves_nothing_built() {
  mkfixture
  PICK_EXIT=1 invoke --land-commits c2
  if [ "$RUN_RC" -eq 9 ] && [ -z "$(daemon_args)" ] &&
    git_logged "-C $FX/feat-big-partial-full-c2 cherry-pick --abort" &&
    git_logged "-C $FX/main worktree remove --force $FX/feat-big-partial-full-c2" &&
    git_logged "-C $FX/main branch -D feat/big-partial-full-c2"; then
    pass "--land-commits on a conflict aborts, removes what it built and requests nothing"
  else
    fail "--land-commits on a conflict aborts, removes what it built and requests nothing" "exit=$RUN_RC" "$RUN_OUT" "$(cat "$FX/git-log")"
  fi
}

test_land_commits_refusal_leaves_nothing_built() {
  mkfixture
  DAEMON_EXIT=6 invoke --land-commits c2
  if [ "$RUN_RC" -eq 5 ] &&
    git_logged "-C $FX/main worktree remove --force $FX/feat-big-partial-full-c2" &&
    git_logged "-C $FX/main branch -D feat/big-partial-full-c2"; then
    pass "--land-commits refused by the queue removes the branch it built"
  else
    fail "--land-commits refused by the queue removes the branch it built" "exit=$RUN_RC" "$RUN_OUT" "$(cat "$FX/git-log")"
  fi
}

test_land_commits_flattens_a_prefixed_branch_into_one_directory() {
  mkfixture
  OWN_BRANCH=JB/big invoke --land-commits c1
  if [ "$RUN_RC" -eq 0 ] &&
    git_logged "-C $FX/main worktree add -b JB/big-partial-full-c1 $FX/JB-big-partial-full-c1 master"; then
    pass "--land-commits names one directory for a prefixed branch"
  else
    fail "--land-commits names one directory for a prefixed branch" "exit=$RUN_RC" "$RUN_OUT" "$(cat "$FX/git-log")"
  fi
}

test_workspace_verbs_share_one_main_worktree_refusal() {
  local own commits oneshot
  mkfixture
  TOP="$FX/main" invoke --enqueue-own
  own="$RUN_RC|$RUN_OUT"
  TOP="$FX/main" invoke --land-commits c1
  commits="$RUN_RC|$RUN_OUT"
  TOP="$FX/main" invoke --announce-one-shot
  oneshot="$RUN_RC|$RUN_OUT"
  # shellcheck disable=SC2016 # the literal call-site text is what is counted
  if [ "$own" = "$commits" ] && [ "$own" = "$oneshot" ] && [ "${own%%|*}" -eq 2 ] && [ "$(grep -c 'top="$(workspace_top)"' "$RUN")" -eq 3 ]; then
    pass "--enqueue-own, --land-commits and --announce-one-shot refuse the main worktree through the one shared check"
  else
    fail "--enqueue-own, --land-commits and --announce-one-shot refuse the main worktree through the one shared check" "own=$own" "commits=$commits" "one-shot=$oneshot"
  fi
}

test_front_rides_the_own_branch_request() {
  mkfixture
  invoke --enqueue-own --front
  expect_args "--enqueue-own --front asks for the front of the queue" "merge-queue -own -front"
}

test_front_rides_an_in_place_request_wherever_it_is_given() {
  mkfixture
  invoke --front --enqueue-own --in-place
  expect_args "--front before the verb still rides --enqueue-own --in-place" "merge-queue -own -in-place -front"
}

test_front_rides_a_branch_request() {
  mkfixture
  invoke --land-branch feat/x --front
  expect_args "--land-branch --front asks for the front of the queue" "merge-queue -branch feat/x -front"
}

test_front_rides_a_workspace_request() {
  mkfixture
  invoke --land-workspace "$FX/other" --front
  expect_args "--land-workspace --front asks for the front of the queue" "merge-queue -dir $FX/other -front"
}

test_front_rides_the_upstream_update() {
  mkfixture
  invoke --pr-merged --front
  expect_args "--pr-merged --front asks for the front of the queue" "merge-queue -pr-merged -front"
}

test_front_rides_a_commits_request() {
  mkfixture
  invoke --land-commits c1 --front
  if [ "$RUN_RC" -eq 0 ] && [ "$(daemon_args)" = "merge-queue -branch feat/big-partial-full-c1 -front" ] &&
    git_logged "-C $FX/feat-big-partial-full-c1 cherry-pick full-c1"; then
    pass "--land-commits --front builds only the named commits and asks for the front of the queue"
  else
    fail "--land-commits --front builds only the named commits and asks for the front of the queue" "exit=$RUN_RC args=$(daemon_args)" "$RUN_OUT"
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

test_front_is_refused_on_remove_branch() {
  mkfixture
  invoke --remove-branch feat/x --front
  if [ "$RUN_RC" -eq 2 ] && [ ! -s "$FX/git-log" ]; then
    pass "--front is refused on --remove-branch before anything is removed"
  else
    fail "--front is refused on --remove-branch before anything is removed" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enqueue_own_requests_the_own_branch
test_enqueue_own_keep_open_keeps_the_workspace
test_enqueue_own_in_place_runs_in_this_workspace
test_enqueue_own_refuses_an_unknown_option
test_enqueue_own_refuses_uncommitted_work
test_enqueue_own_refuses_the_main_worktree
test_announce_one_shot_requests_the_one_shot_merge
test_announce_one_shot_front_goes_to_the_front
test_announce_one_shot_allows_uncommitted_work
test_announce_one_shot_refuses_an_argument
test_announce_one_shot_refusal_is_exit_5
test_land_workspace_requests_without_waiting
test_land_workspace_refuses_its_own
test_land_branch_requests_the_branch
test_pr_merged_requests_the_upstream_update
test_remove_branch_removes_its_worktree_and_branch
test_remove_branch_without_a_worktree_deletes_the_branch
test_remove_branch_refuses_an_unlanded_branch
test_each_outcome_maps_to_its_exit
test_passes_the_verbs_output_through
test_missing_daemon_binary_is_an_error
test_unknown_verb_prints_usage
test_dequeue_own_evicts_the_own_merge
test_dequeue_own_refuses_an_argument
test_dequeue_workspace_names_the_evicted_worktree
test_dequeue_workspace_needs_a_directory
test_pause_queue_pauses_every_repository
test_pause_queue_names_one_repository
test_resume_queue_resumes_every_repository
test_resume_queue_names_one_repository
test_land_commits_builds_the_named_commits_in_branch_order
test_land_commits_needs_a_commit
test_land_commits_refuses_the_main_worktree
test_land_commits_refuses_an_unknown_commit
test_land_commits_refuses_a_commit_off_the_branch
test_land_commits_refuses_a_commit_already_on_master
test_land_commits_conflict_leaves_nothing_built
test_land_commits_refusal_leaves_nothing_built
test_land_commits_flattens_a_prefixed_branch_into_one_directory
test_workspace_verbs_share_one_main_worktree_refusal
test_front_rides_the_own_branch_request
test_front_rides_an_in_place_request_wherever_it_is_given
test_front_rides_a_branch_request
test_front_rides_a_workspace_request
test_front_rides_the_upstream_update
test_front_rides_a_commits_request
test_front_is_refused_on_a_queue_control
test_front_is_refused_on_remove_branch

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
