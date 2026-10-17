#!/usr/bin/env bash
# run.sh — driver for the agent-repl-install skill.
#
# Usage:
#   run.sh --check     report every requirement's status; changes nothing
#   run.sh --install   install every requirement that is missing or outdated
#   run.sh --verify    run the requirements test suites (after a manifest or runner fix)
#   run.sh --finish    refresh Doom (doom env, doom sync), then run doom doctor
#
# Exit codes:
#   --check, --install: 0 every required requirement is met
#                       1 at least one required requirement is unmet (the report says which and why)
#                       2 the requirements could not be read (the report says why)
#   --verify:           0 every suite passed, 1 a suite failed
#   --finish:           0 Doom refreshed and doctor found nothing, 1 doctor reported findings,
#                       2 doom env or doom sync failed
#   any verb:           2 usage error

set -uo pipefail

SKILL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="$(cd "$SKILL_DIR/../.." && pwd -P)"
REPO_DIR="$(cd "$MODULE_DIR/../../.." && pwd -P)"
RUNNER="$MODULE_DIR/bin/requirements.sh"

die() { echo "▶ ERROR: $*" >&2; exit 2; }
log() { echo "▶ $*"; }

usage() { sed -n '4,8p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//' >&2; }

case "${1:-}" in
  --check)
    log "checking every requirement"
    "$RUNNER" check
    ;;
  --install)
    log "installing every missing or outdated requirement"
    "$RUNNER" install
    ;;
  --verify)
    log "running the requirements test suites"
    status=0
    bash "$MODULE_DIR/bin/test-requirements.sh" || status=1
    bash "$REPO_DIR/.claude/test-install.sh" || status=1
    exit "$status"
    ;;
  --finish)
    command -v doom >/dev/null 2>&1 || die "doom is not on PATH; install Doom Emacs first"
    log "refreshing Doom's environment"
    doom env || die "doom env failed"
    log "syncing Doom's packages"
    doom sync || die "doom sync failed"
    log "running doom doctor"
    out="$(doom doctor 2>&1)"
    printf '%s\n' "$out"
    if grep -q 'agent-repl:' <<<"$out"; then exit 1; fi
    ;;
  *)
    usage
    exit 2
    ;;
esac
