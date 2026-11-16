#!/usr/bin/env bash
# Hermetic tests for bin/linux-vm.sh, the Linux VM's create and smoke run.
# limactl, ssh and git are stubs that record their argv; limactl keeps the
# VM's status in a file, so a case reads whether the smoke left it stopped.

# shellcheck disable=SC2016,SC2329
# SC2016: the stub bodies are single-quoted on purpose, so they expand when the
# stub runs. SC2329: the tests are found and run by name (declare -F).

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"

COMMIT=0123456789abcdef0123456789abcdef01234567

write_stub() {
  printf '#!/usr/bin/env bash\nprintf "%%s %%s\\n" "%s" "$*" >>"$STUB_DIR/../transcript"\n%s\n' "$1" "$2" >"$CASE/stubs/$1"
  chmod +x "$CASE/stubs/$1"
}

# new_case: fresh stubs for a VM that exists, is stopped, and shares nothing.
new_case() {
  CASE="$TMP/case$RANDOM$RANDOM"
  mkdir -p "$CASE/stubs"
  : >"$CASE/transcript"
  echo Stopped >"$CASE/status"
  echo '[]' >"$CASE/mounts"
  echo 1 >"$CASE/exists"
  write_stub limactl '
state="$STUB_DIR/.."
case "$1" in
  list)
    if [ "$2" = --quiet ]; then [ "$(cat "$state/exists")" = 1 ] && echo agent-repl-linux; exit 0; fi
    printf "{\"status\":\"%s\",\"config\":{\"mounts\":%s}}\n" "$(cat "$state/status")" "$(cat "$state/mounts")" ;;
  start) echo Running >"$state/status" ;;
  stop) echo Stopped >"$state/status" ;;
esac'
  write_stub ssh '
eval "cmd=\${$#}"
case "$cmd" in
  *"rev-parse --git-dir"*) exit "${STUB_NO_REPO:-0}" ;;
  *timeout*) exit "${STUB_INSTALL_EXIT:-0}" ;;
esac'
  write_stub git '
case " $* " in
  *" rev-parse "*) [ -n "${STUB_BAD_REV:-}" ] && exit 128; echo '"$COMMIT"' ;;
  *" push "*) [ -n "${GIT_SSH_COMMAND:-}" ] || exit 9 ;;
esac'
}

# run ARGS...: linux-vm.sh with only the stubs and the system on PATH.
run() {
  RC=0
  OUT="$(env -i HOME="$CASE" PATH="$CASE/stubs:/usr/bin:/bin:$(dirname "$(command -v jq)")" STUB_DIR="$CASE/stubs" \
    ${EXTRA_ENV:-} bash "$THIS_DIR/linux-vm.sh" "$@" 2>&1)" || RC=$?
}

ran() { grep -qF -- "$1" "$CASE/transcript"; }
status() { cat "$CASE/status"; }

test_a_smoke_pushes_the_commit_installs_and_stops_the_vm() {
  new_case
  run smoke
  if [ "$RC" -eq 0 ] && ran "git -C" && ran "$COMMIT:refs/heads/smoke" \
    && ran "checkout --quiet --force --detach $COMMIT" && ran "timeout --kill-after=10 1200" \
    && [ "$(status)" = Stopped ] && grep -q "step=install outcome=ok commit=$COMMIT" <<<"$OUT"; then
    pass "a smoke pushes the commit, checks it out, installs under its bound, and stops the VM"
  else fail "smoke" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_vm_without_a_checkout_gets_an_empty_repository() {
  new_case
  EXTRA_ENV=STUB_NO_REPO=1 run smoke
  if [ "$RC" -eq 0 ] && ran "git init -q .config/doom"; then
    pass "a VM with no checkout gets a repository to push into"
  else fail "git init" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_vm_with_a_checkout_is_never_reinitialized() {
  new_case
  run smoke
  if [ "$RC" -eq 0 ] && ! ran "git init"; then
    pass "a VM that has its checkout is pushed into as it is"
  else fail "no git init" "rc=$RC" "$(cat "$CASE/transcript")"; fi
}

test_a_vm_that_shares_a_host_folder_is_never_started() {
  new_case
  echo '[{"location":"~"}]' >"$CASE/mounts"
  run smoke
  if [ "$RC" -ne 0 ] && ! ran "limactl start" && grep -q 'cause="the VM shares host folders" mounts="~ "' <<<"$OUT"; then
    pass "a VM sharing a host folder is refused before it starts"
  else fail "mounts" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_failed_install_fails_the_smoke_and_still_stops_the_vm() {
  new_case
  EXTRA_ENV=STUB_INSTALL_EXIT=1 run smoke
  if [ "$RC" -ne 0 ] && [ "$(status)" = Stopped ] && grep -q "outcome=failed step=install exit=1" <<<"$OUT"; then
    pass "a failed install fails the smoke, logged, and the VM is stopped"
  else fail "failed install" "rc=$RC" "$OUT" "$(status)"; fi
}

test_an_install_past_its_bound_is_named_a_timeout() {
  new_case
  EXTRA_ENV=STUB_INSTALL_EXIT=124 run smoke
  if [ "$RC" -ne 0 ] && [ "$(status)" = Stopped ] && grep -q 'step=install cause="timed out after 1200s"' <<<"$OUT"; then
    pass "an install killed by its bound is logged as a timeout, and the VM is stopped"
  else fail "timeout" "rc=$RC" "$OUT" "$(status)"; fi
}

test_a_rev_that_is_no_commit_never_starts_the_vm() {
  new_case
  EXTRA_ENV=STUB_BAD_REV=1 run smoke nope
  if [ "$RC" -ne 0 ] && ! ran "limactl start" && grep -q 'step=resolve rev=nope cause="not a commit"' <<<"$OUT"; then
    pass "a rev that names no commit fails before the VM starts"
  else fail "bad rev" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_a_smoke_of_a_missing_vm_says_to_create_it() {
  new_case
  echo 0 >"$CASE/exists"
  run smoke
  if [ "$RC" -ne 0 ] && ! ran "limactl start" && grep -q "no such VM; run linux-vm.sh create" <<<"$OUT"; then
    pass "a smoke of a VM that does not exist says to create it"
  else fail "missing vm" "rc=$RC" "$OUT"; fi
}

test_create_makes_a_vm_with_no_mounts() {
  new_case
  echo 0 >"$CASE/exists"
  run create
  if [ "$RC" -eq 0 ] && ran "limactl create --name=agent-repl-linux" && ran "--mount-none"; then
    pass "create makes the VM with every mount removed"
  else fail "create" "rc=$RC" "$OUT" "$(cat "$CASE/transcript")"; fi
}

test_create_refuses_a_vm_that_exists() {
  new_case
  run create
  if [ "$RC" -ne 0 ] && ! ran "limactl create" && grep -q 'cause="the VM already exists"' <<<"$OUT"; then
    pass "create refuses to replace a VM that exists"
  else fail "create existing" "rc=$RC" "$OUT"; fi
}

test_stop_of_a_stopped_vm_stops_nothing() {
  new_case
  run stop
  if [ "$RC" -eq 0 ] && ! ran "limactl stop" && grep -q "outcome=already-stopped" <<<"$OUT"; then
    pass "stop of a stopped VM runs no limactl stop"
  else fail "stop stopped" "rc=$RC" "$OUT"; fi
}

test_an_unknown_command_is_a_usage_error() {
  new_case
  run reboot
  if [ "$RC" -eq 2 ] && grep -q "linux-vm.sh smoke" <<<"$OUT"; then
    pass "an unknown command prints the usage and exits 2"
  else fail "usage" "rc=$RC" "$OUT"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do "$t"; done
harness_finish
