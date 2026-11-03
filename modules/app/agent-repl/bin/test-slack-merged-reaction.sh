#!/usr/bin/env bash
# test-slack-merged-reaction.sh — hermetic tests for
# bin/hooks/slack-merged-reaction.sh, the built-in post_deploy hook of an
# emoji-ack workspace.
#
# NOTHING REAL IS CALLED. `agent-repld`, `gh` and `gns` are fakes on PATH: the
# daemon's answer is a fixture file, GitHub answers each pull request's state
# from a fixture directory, and every `gns` call is recorded and answered from
# a fixture.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-grep-in.sh"

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$THIS_DIR/hooks/slack-merged-reaction.sh"
# shellcheck source=lib-harness-report.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/lib-harness-report.sh"

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-merged-reaction-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

FAKE_BIN="$TMP/bin"
mkdir -p "$FAKE_BIN"
# agent-repld: answers `workspace status` with $TMP/status.json, or fails when
# $TMP/status.fail exists.
cat >"$FAKE_BIN/agent-repld" <<FAKE
#!/usr/bin/env bash
echo "\$*" >>"$TMP/agent-repld.calls"
[ ! -e "$TMP/status.fail" ] || { echo "the daemon is down" >&2; exit 1; }
cat "$TMP/status.json"
FAKE
# gh: answers a pull request's state from $TMP/pr/<number>.
cat >"$FAKE_BIN/gh" <<FAKE
#!/usr/bin/env bash
echo "\$*" >>"$TMP/gh.calls"
number="\${3##*/}"
[ -e "$TMP/pr/\$number" ] || { echo "no pull request \$3" >&2; exit 1; }
cat "$TMP/pr/\$number"
FAKE
# gns: records each call; answers with $TMP/gns.out and exits $TMP/gns.rc.
cat >"$FAKE_BIN/gns" <<FAKE
#!/usr/bin/env bash
echo "\$*" >>"$TMP/gns.calls"
[ ! -e "$TMP/gns.out" ] || cat "$TMP/gns.out"
exit "\$(cat "$TMP/gns.rc" 2>/dev/null || echo 0)"
FAKE
chmod +x "$FAKE_BIN"/*
export PATH="$FAKE_BIN:$PATH"

readonly PR1="https://github.com/acme/app/pull/101"
readonly PR2="https://github.com/acme/app/pull/102"
readonly POST1="https://acme.slack.com/archives/C0123/p1730000000123456?thread_ts=1729999999.000100&cid=C0123"
readonly POST2="https://acme.slack.com/archives/C0123/p1730000100654321"

# reset — a fresh fixture: no calls, every gns call succeeds, both pull
# requests merged.
reset() {
  rm -rf "$TMP/pr" "$TMP"/*.calls "$TMP/status.fail" "$TMP/gns.out" "$TMP/gns.rc"
  mkdir -p "$TMP/pr"
  echo MERGED >"$TMP/pr/101"
  echo MERGED >"$TMP/pr/102"
}

# links JSON... — the daemon's status answer carrying these links.
links() {
  local IFS=,
  printf '{"success":{"status":{"links":{"links":[%s]}}}}\n' "$*" >"$TMP/status.json"
}
announcing() { printf '{"url":"%s","label":"PR announced in Slack","ownership":{"owned":{"announces":{"url":"%s"}}}}' "$1" "$2"; }
owned() { printf '{"url":"%s","label":"The pull request","ownership":{"owned":{}}}' "$1"; }
reference() { printf '{"url":"%s","label":"Originating Slack thread","ownership":{"reference":{}}}' "$1"; }

# react — run the hook as the daemon does. RUN_RC and RUN_OUT hold its outcome.
react() {
  set +e
  RUN_OUT="$(AGENT_REPL_CALLING_WORKSPACE=/work/ws "$SCRIPT" 2>&1)"
  RUN_RC=$?
  set -e
}
gns_calls() { cat "$TMP/gns.calls" 2>/dev/null || true; }

test_a_merged_announcement_gets_the_reaction() {
  reset
  links "$(owned "$PR1")" "$(announcing "$POST1" "$PR1")" "$(reference "https://acme.slack.com/archives/C0123/p1729999999000100")"
  react
  if [ "$RUN_RC" -eq 0 ] && [ "$(gns_calls)" = "slack react --channel C0123 --ts 1730000000.123456 --emoji merged" ] &&
    grep_in "$(cat "$TMP/agent-repld.calls")" -qxF "workspace status /work/ws"; then
    pass "the Slack message announcing a merged pull request gets :merged:, found through the workspace's own links"
  else
    fail "the Slack message announcing a merged pull request gets :merged:, found through the workspace's own links" "exit=$RUN_RC" "$RUN_OUT" "gns: $(gns_calls)"
  fi
}

test_every_announcement_is_reacted_on() {
  reset
  links "$(announcing "$POST1" "$PR1")" "$(announcing "$POST2" "$PR2")"
  react
  local want
  want="$(printf 'slack react --channel C0123 --ts 1730000000.123456 --emoji merged\nslack react --channel C0123 --ts 1730000100.654321 --emoji merged')"
  if [ "$RUN_RC" -eq 0 ] && [ "$(gns_calls)" = "$want" ]; then
    pass "every announcing message is reacted on"
  else
    fail "every announcing message is reacted on" "exit=$RUN_RC" "$RUN_OUT" "gns: $(gns_calls)"
  fi
}

test_an_unmerged_pull_request_is_left_unreacted() {
  reset
  echo OPEN >"$TMP/pr/101"
  links "$(announcing "$POST1" "$PR1")"
  react
  if [ "$RUN_RC" -eq 0 ] && [ -z "$(gns_calls)" ] && grep_in "$RUN_OUT" -qF "$PR1 is OPEN, not merged"; then
    pass "a pull request GitHub does not report merged is left unreacted"
  else
    fail "a pull request GitHub does not report merged is left unreacted" "exit=$RUN_RC" "$RUN_OUT" "gns: $(gns_calls)"
  fi
}

test_no_announcement_reacts_on_nothing() {
  reset
  links "$(owned "$PR1")" "$(reference "https://acme.slack.com/archives/C0123/p1729999999000100")"
  react
  if [ "$RUN_RC" -eq 0 ] && [ -z "$(gns_calls)" ] && [ ! -e "$TMP/gh.calls" ] &&
    grep_in "$RUN_OUT" -qF "no stored Slack message of /work/ws announces a pull request"; then
    pass "a workspace with no announcing link reacts on nothing and never searches a thread"
  else
    fail "a workspace with no announcing link reacts on nothing and never searches a thread" "exit=$RUN_RC" "$RUN_OUT" "gns: $(gns_calls)"
  fi
}

test_an_existing_reaction_is_no_failure() {
  reset
  echo "error: already_reacted" >"$TMP/gns.out"
  echo 1 >"$TMP/gns.rc"
  links "$(announcing "$POST1" "$PR1")"
  react
  if [ "$RUN_RC" -eq 0 ] && grep_in "$RUN_OUT" -qF "already carries :merged:"; then
    pass "a message already carrying :merged: is no failure, so the hook bears running again"
  else
    fail "a message already carrying :merged: is no failure, so the hook bears running again" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_a_refused_reaction_fails_the_hook() {
  reset
  echo "error: channel_not_found" >"$TMP/gns.out"
  echo 1 >"$TMP/gns.rc"
  links "$(announcing "$POST1" "$PR1")"
  react
  if [ "$RUN_RC" -eq 1 ] && grep_in "$RUN_OUT" -qF "Slack refused the :merged: reaction on $POST1: error: channel_not_found"; then
    pass "a reaction Slack refuses fails the hook, naming the message and why"
  else
    fail "a reaction Slack refuses fails the hook, naming the message and why" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_an_unreadable_pull_request_fails_the_hook() {
  reset
  rm "$TMP/pr/101"
  links "$(announcing "$POST1" "$PR1")"
  react
  if [ "$RUN_RC" -eq 1 ] && [ -z "$(gns_calls)" ] && grep_in "$RUN_OUT" -qF "could not read whether $PR1 merged"; then
    pass "a pull request GitHub cannot answer for fails the hook"
  else
    fail "a pull request GitHub cannot answer for fails the hook" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_an_unreadable_status_fails_the_hook() {
  reset
  touch "$TMP/status.fail"
  react
  if [ "$RUN_RC" -eq 1 ] && grep_in "$RUN_OUT" -qF "could not read the links of /work/ws"; then
    pass "links the daemon cannot answer for fail the hook"
  else
    fail "links the daemon cannot answer for fail the hook" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_an_announcement_that_is_no_slack_message_fails_the_hook() {
  reset
  links "$(announcing "https://example.com/post/1" "$PR1")"
  react
  if [ "$RUN_RC" -eq 1 ] && [ -z "$(gns_calls)" ] && grep_in "$RUN_OUT" -qF "https://example.com/post/1 announces $PR1 but is no Slack message link"; then
    pass "an announcing link that is no Slack message fails the hook"
  else
    fail "an announcing link that is no Slack message fails the hook" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_a_hook_with_no_workspace_is_refused() {
  reset
  set +e
  RUN_OUT="$(env -u AGENT_REPL_CALLING_WORKSPACE "$SCRIPT" 2>&1)"
  RUN_RC=$?
  set -e
  if [ "$RUN_RC" -eq 1 ] && grep_in "$RUN_OUT" -qF "AGENT_REPL_CALLING_WORKSPACE is unset"; then
    pass "a run with no calling workspace is refused"
  else
    fail "a run with no calling workspace is refused" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

echo "test-slack-merged-reaction.sh"
for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done
harness_finish
