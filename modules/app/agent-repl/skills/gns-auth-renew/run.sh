#!/usr/bin/env bash
# gns-auth-renew: renew an expired or missing GNS token through the work
# account's own Chrome profile.
#
# Verbs:
#   --renew [ENV]   Renew the token for ENV (default: production): start the
#                   device-code sign-in, open its page in the work account's
#                   Chrome profile, wait for it to complete, and confirm the
#                   environment reads authenticated as the work account.
#
# Exit codes:
#   0  The token is renewed and reads as the work account.
#   1  Usage: no verb, or an unknown one.
#   2  A tool failed (the sign-in could not start, or its page could not open).
#   3  The work account, or its Chrome profile, could not be found.
#   4  The sign-in did not complete: it was refused, it expired, or the
#      environment does not read as the work account afterwards.

set -uo pipefail

WORK_CONFIG="${GNS_AUTH_RENEW_WORK_CONFIG:-$HOME/.claude-chesscom/.claude.json}"
CHROME_STATE="${GNS_AUTH_RENEW_CHROME_STATE:-$HOME/Library/Application Support/Google/Chrome/Local State}"
POLL_SECONDS="${GNS_AUTH_RENEW_POLL_SECONDS:-3}"
# A device code lasts minutes, not hours; the poll gives up at this bound
# rather than waiting on a sign-in nobody is completing.
POLL_BOUND_SECONDS="${GNS_AUTH_RENEW_POLL_BOUND_SECONDS:-900}"

die() { printf 'gns-auth-renew: %s\n' "$1" >&2; exit "${2:-2}"; }
log() { printf '▶ %s\n' "$1" >&2; }

work_email() {
  [ -f "$WORK_CONFIG" ] || return 1
  jq -r '.oauthAccount.emailAddress // empty' "$WORK_CONFIG"
}

work_profile() {
  [ -f "$CHROME_STATE" ] || return 1
  jq -r --arg e "$1" '.profile.info_cache // {} | to_entries[] | select(.value.user_name == $e) | .key' "$CHROME_STATE" | sed -n 1p
}

renew() {
  local env="${1:-production}" email profile started url waited=0 rc status
  email="$(work_email)" || true
  [ -n "${email:-}" ] || die "no work account is signed in under $WORK_CONFIG; see the Setup section of the agent-repl user guide" 3
  profile="$(work_profile "$email")" || true
  [ -n "${profile:-}" ] || die "no Chrome profile is signed in as $email in $CHROME_STATE; see the Setup section of the agent-repl user guide" 3

  started="$(gns auth login --env "$env" --device 2>&1)" || die "the device-code sign-in could not start: $started"
  printf '%s\n' "$started"
  url="$(grep -Eo 'https://[^[:space:]]+' <<<"$started" | sed -n 1p)"
  [ -n "$url" ] || die "the device-code sign-in printed no sign-in page: $started"
  open -na "Google Chrome" --args --profile-directory="$profile" "$url" || die "the sign-in page could not be opened in Chrome profile $profile"
  log "opened the sign-in page in $email's Chrome profile ($profile); waiting for it to complete"

  while :; do
    gns auth login --env "$env" --device-poll >/dev/null 2>&1
    rc=$?
    case "$rc" in
      0) break ;;
      1) ;;
      *) die "the sign-in was refused (device-poll exit $rc)" 4 ;;
    esac
    [ "$waited" -lt "$POLL_BOUND_SECONDS" ] || die "the sign-in did not complete within ${POLL_BOUND_SECONDS}s" 4
    sleep "$POLL_SECONDS"
    waited=$((waited + POLL_SECONDS))
  done

  status="$(gns auth status 2>&1)" || die "gns auth status failed: $status" 4
  grep -qiF "$email" <<<"$status" || die "the $env environment does not read as $email: $status" 4
  log "the $env token is renewed as $email"
}

case "${1:-}" in
  --renew) shift; renew "$@" ;;
  *) printf 'usage: %s --renew [ENV]\n' "$0" >&2; exit 1 ;;
esac
