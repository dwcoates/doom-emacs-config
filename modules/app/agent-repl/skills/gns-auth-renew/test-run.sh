#!/usr/bin/env bash
# Hermetic tests for skills/gns-auth-renew/run.sh: gns and open are stubs
# that record their argv and answer from the case's settings; the work
# account's config and Chrome's Local State are fixtures. Nothing reaches a
# real GNS, browser or account.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F).

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
RUN="$THIS_DIR/run.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

PASS=0
FAIL=0
pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; shift; [ $# -eq 0 ] || printf '%s\n' "$@" | sed 's/^/        /' >&2; FAIL=$((FAIL + 1)); }

# new_case: stubs and fixtures for a work account signed in as
# work@example.com, whose Chrome profile is "Profile 3". The gns stub's
# answers are files under $CASE: device-out and device-rc for --device, one
# line per poll in polls (the exit code of each), status-out for status.
CASE=""
new_case() {
  CASE="$(mktemp -d "$TMP/case.XXXXXX")"
  mkdir -p "$CASE/bin"
  printf '{"oauthAccount":{"emailAddress":"work@example.com"}}\n' >"$CASE/work.json"
  printf '{"profile":{"info_cache":{"Default":{"user_name":"me@example.org"},"Profile 3":{"user_name":"work@example.com"}}}}\n' >"$CASE/local-state"
  printf 'Open https://auth.example/device?code=ABCD and enter ABCD\n' >"$CASE/device-out"
  printf '0\n' >"$CASE/device-rc"
  printf '0\n' >"$CASE/polls"
  printf 'production: authenticated as work@example.com\n' >"$CASE/status-out"
  cat >"$CASE/bin/gns" <<'STUB'
#!/usr/bin/env bash
printf 'gns %s\n' "$*" >>"$CASE_DIR/transcript"
case " $* " in
  *" --device-poll "*)
    rc="$(head -n 1 "$CASE_DIR/polls")"
    [ "$(wc -l <"$CASE_DIR/polls")" -gt 1 ] && sed -i '' 1d "$CASE_DIR/polls"
    exit "$rc" ;;
  *" --device "*) cat "$CASE_DIR/device-out"; exit "$(cat "$CASE_DIR/device-rc")" ;;
  *" status "*) cat "$CASE_DIR/status-out" ;;
esac
STUB
  cat >"$CASE/bin/open" <<'STUB'
#!/usr/bin/env bash
printf 'open %s\n' "$*" >>"$CASE_DIR/transcript"
exit "${STUB_OPEN_RC:-0}"
STUB
  chmod +x "$CASE/bin/gns" "$CASE/bin/open"
  : >"$CASE/transcript"
}

# run [ARGS...]: run.sh against the case, capturing RC and OUT.
RC=0
OUT=""
run() {
  set +e
  OUT="$(CASE_DIR="$CASE" PATH="$CASE/bin:/usr/bin:/bin:/opt/homebrew/bin:/usr/local/bin" \
    GNS_AUTH_RENEW_WORK_CONFIG="$CASE/work.json" GNS_AUTH_RENEW_CHROME_STATE="$CASE/local-state" \
    GNS_AUTH_RENEW_POLL_SECONDS=0 GNS_AUTH_RENEW_POLL_BOUND_SECONDS="${BOUND:-60}" \
    bash "$RUN" "$@" 2>&1)"
  RC=$?
  set -e
}

test_a_completed_sign_in_renews_and_exits_zero() {
  new_case; run --renew
  if [ "$RC" -eq 0 ] && grep -qF 'renewed as work@example.com' <<<"$OUT"; then pass "a completed sign-in renews the token"; else fail "completed sign-in" "rc=$RC" "$OUT"; fi
}

test_the_sign_in_page_opens_in_the_work_accounts_profile() {
  new_case; run --renew
  if grep -qF 'open -na Google Chrome --args --profile-directory=Profile 3 https://auth.example/device?code=ABCD' "$CASE/transcript"; then
    pass "the sign-in page opens in the work account's Chrome profile"
  else
    fail "sign-in page profile" "$(cat "$CASE/transcript")"
  fi
}

test_the_sign_in_always_uses_the_device_flow_for_the_named_environment() {
  new_case; run --renew local
  if grep -qF 'gns auth login --env local --device' "$CASE/transcript" && ! grep -E '^gns auth login --env local$' "$CASE/transcript" >/dev/null; then
    pass "the sign-in uses the device flow for the named environment"
  else
    fail "device flow" "$(cat "$CASE/transcript")"
  fi
}

test_a_pending_poll_is_polled_again() {
  new_case; printf '1\n1\n0\n' >"$CASE/polls"; run --renew
  local polls; polls="$(grep -c -- '--device-poll' "$CASE/transcript")"
  if [ "$RC" -eq 0 ] && [ "$polls" -eq 3 ]; then pass "a pending sign-in is polled until it completes"; else fail "pending poll" "rc=$RC polls=$polls" "$OUT"; fi
}

test_no_work_account_exits_three() {
  new_case; rm "$CASE/work.json"; run --renew
  if [ "$RC" -eq 3 ] && grep -qF 'no work account' <<<"$OUT" && ! grep -q '^gns' "$CASE/transcript"; then pass "no work account exits 3 before any sign-in"; else fail "no work account" "rc=$RC" "$OUT"; fi
}

test_no_profile_for_the_work_account_exits_three() {
  new_case; printf '{"profile":{"info_cache":{"Default":{"user_name":"me@example.org"}}}}\n' >"$CASE/local-state"; run --renew
  if [ "$RC" -eq 3 ] && grep -qF 'no Chrome profile is signed in as work@example.com' <<<"$OUT"; then pass "no Chrome profile for the work account exits 3"; else fail "no profile" "rc=$RC" "$OUT"; fi
}

test_a_sign_in_that_cannot_start_exits_two() {
  new_case; printf '5\n' >"$CASE/device-rc"; run --renew
  if [ "$RC" -eq 2 ] && grep -qF 'could not start' <<<"$OUT"; then pass "a sign-in that cannot start exits 2"; else fail "start failure" "rc=$RC" "$OUT"; fi
}

test_a_sign_in_with_no_page_exits_two() {
  new_case; printf 'nothing to open\n' >"$CASE/device-out"; run --renew
  if [ "$RC" -eq 2 ] && grep -qF 'printed no sign-in page' <<<"$OUT"; then pass "a sign-in that prints no page exits 2"; else fail "no page" "rc=$RC" "$OUT"; fi
}

test_a_page_that_cannot_open_exits_two() {
  new_case; STUB_OPEN_RC=1 run --renew
  if [ "$RC" -eq 2 ] && grep -qF 'could not be opened' <<<"$OUT"; then pass "a page that cannot open exits 2"; else fail "open failure" "rc=$RC" "$OUT"; fi
}

test_a_refused_sign_in_exits_four() {
  new_case; printf '2\n' >"$CASE/polls"; run --renew
  if [ "$RC" -eq 4 ] && grep -qF 'refused' <<<"$OUT"; then pass "a refused sign-in exits 4"; else fail "refused" "rc=$RC" "$OUT"; fi
}

test_a_sign_in_past_its_bound_exits_four() {
  new_case; printf '1\n' >"$CASE/polls"; BOUND=0 run --renew
  if [ "$RC" -eq 4 ] && grep -qF 'did not complete within 0s' <<<"$OUT"; then pass "a sign-in past its bound exits 4"; else fail "bound" "rc=$RC" "$OUT"; fi
}

test_a_token_that_reads_as_another_account_exits_four() {
  new_case; printf 'production: authenticated as me@example.org\n' >"$CASE/status-out"; run --renew
  if [ "$RC" -eq 4 ] && grep -qF 'does not read as work@example.com' <<<"$OUT"; then pass "a token reading as another account exits 4"; else fail "wrong account" "rc=$RC" "$OUT"; fi
}

test_usage_exits_one() {
  new_case; run
  if [ "$RC" -eq 1 ] && grep -qF 'usage:' <<<"$OUT"; then pass "no verb exits 1 with the usage"; else fail "usage" "rc=$RC" "$OUT"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
