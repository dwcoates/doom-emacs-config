#!/usr/bin/env bash
# Hermetic tests for skills/arrange-sidebar/run.sh, the /arrange-sidebar
# skill's driver. Every case points it at a fake runtime door that records
# its argv and answers as the case says, so no daemon is ever asked.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
RUN="$THIS_DIR/../skills/arrange-sidebar/run.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0
pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; shift; [ $# -eq 0 ] || printf '%s\n' "$@" | sed 's/^/        /' >&2; FAIL=$((FAIL + 1)); }

# fake_runtime NAME STATUS ANSWER: a runtime door that records its argv in
# $TMP/NAME.argv, prints ANSWER and exits STATUS.
fake_runtime() {
  local door="$TMP/$1"
  printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$@" > %q\nprintf "%%s\\n" %q\nexit %d\n' "$door.argv" "$3" "$2" > "$door"
  chmod +x "$door"
  printf '%s' "$door"
}

test_read_asks_for_the_arrangement_and_prints_it() {
  local door out rc=0
  door="$(fake_runtime read 0 '{"repositories":[]}')"
  out="$(ARRANGE_SIDEBAR_RUNTIME="$door" bash "$RUN" --read)" || rc=$?
  if [ "$rc" -eq 0 ] && [ "$out" = '{"repositories":[]}' ] && [ "$(cat "$TMP/read.argv")" = $'call\nGetSidebarArrangement\n{}' ]; then
    pass "--read asks for the arrangement and prints it"
  else
    fail "--read" "rc=$rc" "$out" "$(cat "$TMP/read.argv" 2>/dev/null)"
  fi
}

test_read_fails_hard_when_the_daemon_cannot_be_asked() {
  local door out rc=0
  door="$(fake_runtime read-down 1 'no daemon is serving')"
  out="$(ARRANGE_SIDEBAR_RUNTIME="$door" bash "$RUN" --read 2>&1)" || rc=$?
  if [ "$rc" -eq 2 ] && grep -qF "no daemon is serving" <<<"$out"; then
    pass "--read fails hard, with the cause, when the daemon cannot be asked"
  else
    fail "--read down" "rc=$rc" "$out"
  fi
}

test_arrange_sends_the_change_and_prints_the_answer() {
  local door out rc=0
  door="$(fake_runtime ok 0 '{"success":{"rosterRevision":"3"}}')"
  out="$(ARRANGE_SIDEBAR_RUNTIME="$door" bash "$RUN" --arrange '{"deleteSubsection": {"subsection": {"id": "s1"}}}')" || rc=$?
  if [ "$rc" -eq 0 ] && [ "$out" = '{"success":{"rosterRevision":"3"}}' ] \
     && [ "$(cat "$TMP/ok.argv")" = $'call\nArrangeSidebar\n{"deleteSubsection":{"subsection":{"id":"s1"}}}' ]; then
    pass "--arrange sends the change and prints the answer"
  else
    fail "--arrange" "rc=$rc" "$out" "$(cat "$TMP/ok.argv" 2>/dev/null)"
  fi
}

test_arrange_answers_a_refusal_with_exit_1() {
  local door out rc=0
  door="$(fake_runtime refused 1 '{"error":{"blankName":{}}}')"
  out="$(ARRANGE_SIDEBAR_RUNTIME="$door" bash "$RUN" --arrange '{"renameSubsection":{"subsection":{"id":"s1"},"name":" "}}')" || rc=$?
  if [ "$rc" -eq 1 ] && [ "$out" = '{"error":{"blankName":{}}}' ]; then
    pass "--arrange answers a refusal with exit 1 and prints it"
  else
    fail "--arrange refused" "rc=$rc" "$out"
  fi
}

test_arrange_fails_hard_when_the_call_fails() {
  local door out rc=0
  door="$(fake_runtime broken 1 'connection refused')"
  out="$(ARRANGE_SIDEBAR_RUNTIME="$door" bash "$RUN" --arrange '{"moveRepository":{}}' 2>&1)" || rc=$?
  if [ "$rc" -eq 2 ] && grep -qF "connection refused" <<<"$out"; then
    pass "--arrange fails hard, with the cause, when the call fails"
  else
    fail "--arrange broken" "rc=$rc" "$out"
  fi
}

test_arrange_refuses_a_change_that_is_not_json() {
  local door out rc=0
  door="$(fake_runtime unsent 0 '{}')"
  out="$(ARRANGE_SIDEBAR_RUNTIME="$door" bash "$RUN" --arrange 'not json' 2>&1)" || rc=$?
  if [ "$rc" -eq 2 ] && grep -qF "is not JSON" <<<"$out" && [ ! -e "$TMP/unsent.argv" ]; then
    pass "--arrange refuses a change that is not JSON and asks nothing"
  else
    fail "--arrange not json" "rc=$rc" "$out"
  fi
}

test_an_unknown_verb_prints_the_usage() {
  local out rc=0
  out="$(bash "$RUN" --nope 2>&1)" || rc=$?
  if [ "$rc" -eq 1 ] && grep -qF "usage:" <<<"$out"; then pass "an unknown verb prints the usage"; else fail "unknown verb" "rc=$rc" "$out"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
