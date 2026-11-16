#!/usr/bin/env bash
# Hermetic tests for bin/lib-service.sh, the one driver of agent-repl's
# services. launchctl and systemctl are stubs that record their argv: no test
# reaches the host's service manager.

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

# Each stub appends "<tool> <argv>" to the transcript. STUB_HELD makes the
# held question answer yes, STUB_PID names the running pid, and STUB_FAIL
# names one verb that fails.
for tool in launchctl systemctl; do
  cat >"$TMP/$tool" <<EOF
#!/usr/bin/env bash
printf '$tool %s\n' "\$*" >>"$TMP/transcript"
verb="\$1"; [ "\$verb" = --user ] && verb="\$2"
[ "\$verb" = "\${STUB_FAIL:-}" ] && exit 1
case "\$verb" in
  print) [ -n "\${STUB_HELD:-}" ] || exit 113; printf 'gui/501/x = {\n\tstate = running\n\tpid = %s\n}\n' "\${STUB_PID:-}" ;;
  is-active) [ -n "\${STUB_HELD:-}" ] || exit 3 ;;
  show) printf '%s\n' "\${STUB_PID:-0}" ;;
esac
exit 0
EOF
  chmod +x "$TMP/$tool"
done
export AGENT_REPL_LAUNCHCTL="$TMP/launchctl" AGENT_REPL_SYSTEMCTL="$TMP/systemctl"
export AGENT_REPL_LAUNCH_AGENTS_DIR="$TMP/agents" AGENT_REPL_SYSTEMD_USER_DIR="$TMP/units"

# shellcheck source=lib-service.sh
. "$THIS_DIR/lib-service.sh"
# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"

uid="$(id -u)"

# reset PLATFORM: a fresh transcript and no stub answers set.
reset() { SERVICE_PLATFORM="$1"; : >"$TMP/transcript"; unset STUB_HELD STUB_PID STUB_FAIL; SERVICE_FAILED_STEP=""; }
transcript() { cat "$TMP/transcript"; }

test_the_definition_is_a_plist_on_macos_and_a_unit_on_linux() {
  local mac linux
  reset darwin; mac="$(service_definition_path com.x.svc)"
  reset linux; linux="$(service_definition_path com.x.svc)"
  if [ "$mac" = "$TMP/agents/com.x.svc.plist" ] && [ "$linux" = "$TMP/units/com.x.svc.service" ]; then
    pass "the definition is a launchd plist on macOS and a systemd user unit on Linux"
  else fail "definition paths" "$mac" "$linux"; fi
}

test_held_asks_launchd_or_systemd() {
  local bad=""
  reset darwin; service_held com.x.svc && bad="$bad darwin-unheld"
  STUB_HELD=1 service_held com.x.svc || bad="$bad darwin-held"
  [ "$(sed -n 1p "$TMP/transcript")" = "launchctl print gui/$uid/com.x.svc" ] || bad="$bad darwin-call"
  reset linux; service_held com.x.svc && bad="$bad linux-unheld"
  STUB_HELD=1 service_held com.x.svc || bad="$bad linux-held"
  [ "$(sed -n 1p "$TMP/transcript")" = "systemctl --user is-active --quiet com.x.svc.service" ] || bad="$bad linux-call"
  if [ -z "$bad" ]; then pass "service_held asks launchd's domain on macOS and systemd's activity on Linux"; else fail "service_held:$bad" "$(transcript)"; fi
}

test_pid_is_the_running_pid_or_nothing() {
  local bad=""
  reset darwin
  [ "$(STUB_HELD=1 STUB_PID=4242 service_pid com.x.svc)" = 4242 ] || bad="$bad darwin-running"
  [ -z "$(service_pid com.x.svc)" ] || bad="$bad darwin-unknown"
  reset linux
  [ "$(STUB_PID=5151 service_pid com.x.svc)" = 5151 ] || bad="$bad linux-running"
  [ -z "$(service_pid com.x.svc)" ] || bad="$bad linux-mainpid-0"
  if [ -z "$bad" ]; then pass "service_pid is the running pid, and nothing for a service that runs none"; else fail "service_pid:$bad"; fi
}

test_stop_and_start_take_the_service_out_and_back() {
  local mac linux
  reset darwin; service_stop com.x.svc; service_start com.x.svc; mac="$(transcript)"
  reset linux; service_stop com.x.svc; service_start com.x.svc; linux="$(transcript)"
  if [ "$mac" = "launchctl bootout gui/$uid/com.x.svc"$'\n'"launchctl bootstrap gui/$uid $TMP/agents/com.x.svc.plist" ] &&
     [ "$linux" = "systemctl --user stop com.x.svc.service"$'\n'"systemctl --user start com.x.svc.service" ]; then
    pass "a stop boots the launchd service out or stops the unit, and a start brings it back from its definition"
  else fail "stop and start" "$mac" "$linux"; fi
}

test_load_reads_the_new_definition_and_runs_it() {
  local mac held linux
  reset darwin; service_load com.x.svc; mac="$(transcript)"
  reset darwin; STUB_HELD=1 service_load com.x.svc; held="$(transcript)"
  reset linux; service_load com.x.svc; linux="$(transcript)"
  if [ "$mac" = "launchctl print gui/$uid/com.x.svc"$'\n'"launchctl bootstrap gui/$uid $TMP/agents/com.x.svc.plist"$'\n'"launchctl kickstart -k gui/$uid/com.x.svc" ] &&
     [ "$(sed -n 2p <<<"$held")" = "launchctl bootout gui/$uid $TMP/agents/com.x.svc.plist" ] &&
     [ "$linux" = "systemctl --user daemon-reload"$'\n'"systemctl --user enable com.x.svc.service"$'\n'"systemctl --user restart com.x.svc.service" ]; then
    pass "a load bootstraps and kickstarts (booting a held service out first), or reloads, enables and restarts the unit"
  else fail "load" "$mac" "$held" "$linux"; fi
}

test_a_failed_step_is_named_and_stops_the_load() {
  local verb bad=""
  for verb in daemon-reload enable restart; do
    reset linux
    if STUB_FAIL="$verb" service_load com.x.svc; then bad="$bad $verb-succeeded"; continue; fi
    case "$verb" in
      daemon-reload) [ "$SERVICE_FAILED_STEP" = "systemctl --user daemon-reload" ] || bad="$bad $verb-named" ;;
      *) [ "$SERVICE_FAILED_STEP" = "systemctl --user $verb com.x.svc.service" ] || bad="$bad $verb-named" ;;
    esac
    [ "$(transcript | tail -1)" = "systemctl --user $verb$([ "$verb" = daemon-reload ] || echo " com.x.svc.service")" ] || bad="$bad $verb-went-on"
  done
  reset darwin
  STUB_FAIL=bootstrap service_load com.x.svc && bad="$bad bootstrap-succeeded"
  [ "$SERVICE_FAILED_STEP" = "launchctl bootstrap gui/$uid $TMP/agents/com.x.svc.plist" ] || bad="$bad bootstrap-named"
  if [ -z "$bad" ]; then pass "a failed step fails the load, names itself in SERVICE_FAILED_STEP, and nothing runs after it"; else fail "failed steps:$bad" "$SERVICE_FAILED_STEP"; fi
}

test_unload_and_forget() {
  local mac_unheld mac_held linux
  reset darwin; service_unload com.x.svc; service_forget; mac_unheld="$(transcript)"
  reset darwin; STUB_HELD=1 service_unload com.x.svc; mac_held="$(transcript)"
  reset linux; service_unload com.x.svc; service_forget; linux="$(transcript)"
  if [ "$mac_unheld" = "launchctl print gui/$uid/com.x.svc" ] &&
     [ "$(sed -n 2p <<<"$mac_held")" = "launchctl bootout gui/$uid $TMP/agents/com.x.svc.plist" ] &&
     [ "$linux" = "systemctl --user disable --now com.x.svc.service"$'\n'"systemctl --user daemon-reload" ]; then
    pass "an unload boots a held launchd service out, or disables and stops the unit, and a forget reloads systemd alone"
  else fail "unload and forget" "$mac_unheld" "$mac_held" "$linux"; fi
}

test_an_unset_platform_is_refused() {
  local out rc=0
  SERVICE_PLATFORM=""; : >"$TMP/transcript"
  out="$(service_held com.x.svc 2>&1)" || rc=$?
  if [ "$rc" -eq 2 ] && [[ $out == *"SERVICE_PLATFORM is ''"* ]] && [ ! -s "$TMP/transcript" ]; then
    pass "a call before SERVICE_PLATFORM is set is refused and runs no tool"
  else fail "unset platform" "rc=$rc" "$out" "$(transcript)"; fi
}

test_the_host_platform_takes_the_harness_override() {
  local got
  got="$(AGENT_REPL_SERVICE_PLATFORM=linux service_host_platform)"
  if [ "$got" = linux ]; then pass "service_host_platform answers AGENT_REPL_SERVICE_PLATFORM when set"; else fail "override" "$got"; fi
}

# The call sites share the driver: no script but the library runs launchctl or
# systemctl itself, as a command (first word, after `$(`, or after a list
# operator or keyword); a mention in prose or usage text is no call.
test_no_script_drives_the_service_manager_itself() {
  local f rel real=""
  while IFS= read -r f; do
    rel="${f#"$MODULE_DIR"/}"
    case "$rel" in bin/lib-service.sh|*/test-*.sh) continue ;; esac
    if grep -qE '(^[[:space:]]*|\$\([[:space:]]*|(;|&&|\|\||\||then|do|else)[[:space:]]+)"?\$?\{?(launchctl|systemctl|LAUNCHCTL|SYSTEMCTL)\}?"?[[:space:]]' <<<"$(grep -vE '^[[:space:]]*#' "$f")"; then
      real="$real $rel"
    fi
  done < <(find "$MODULE_DIR/bin" "$MODULE_DIR/scripts" -name '*.sh' -type f)
  if [ -z "$real" ]; then pass "no script drives launchctl or systemctl outside lib-service.sh"; else fail "these scripts drive the service manager themselves; source lib-service.sh instead:$real"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

harness_finish
