#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# realtest.sh — drive the OWNER'S ACTUAL EDITOR.
#
# A realtest is not a test of the module against a fixture. It is the real
# Emacs.app process on the owner's Mac, the real ~/.config/doom on master, the
# real ~/.claude-emacs state, the real store, sidecar, shim and daemon deployed
# from master, and the owner's real ~/.claude transcripts. There is NO sandbox
# and NO image, and no picture is taken at this stage, so Emacs is never brought
# frontmost. The ONE substitution is the vendor: AGENT_REPL_FORBID_VENDOR_CALLS
# is set on the Emacs process and inherited by everything it spawns, so no real
# Claude call can occur.
#
# modules/app/agent-repl/docs/REALTEST-PLAN.md is the CONTRACT — which realtests
# exist, what each measures, and the remediation loop they feed. e2e/REALTEST-SPEC.md
# documents these mechanics. This script is the only supported entry point,
# because the three refusals below are not optional.
#
#   bin/realtest.sh                              every realtest
#   bin/realtest.sh -run TestRealtestStartTheEditor    one, by name
#
#   exit 0   the realtests ran
#   exit 77  DECLINED, and the message says why
#   other    a realtest failed
#
# 77 is the autotools "skipped" convention, used rather than 0 so a run can
# never report a green realtest that did not execute — the same convention every
# other declining entry point in this module uses.
#
# THREE REFUSALS, and each one is here because the alternative is worse than not
# running:
#
#   1. NOT DEPLOYED. Every system must be at the checkout's revision, judged by
#      bin/readiness-report.sh. A realtest against a stale daemon measures a
#      build nobody has, and its findings send the owner after defects that were
#      fixed days ago.
#
#   2. A HUMAN IS USING EMACS. A cold start has to quit the standing editor, and
#      that is the owner's editor with the owner's unsaved work in it.
#      AGENT_REPL_REALTEST_TAKEOVER=1 is the owner saying to go ahead.
#
#   3. THE VENDOR GUARD CANNOT BE HELD. A daemon already running without
#      AGENT_REPL_FORBID_VENDOR_CALLS in its environment would be adopted by the
#      new Emacs and would spawn shims without it, so a prompt could reach the
#      real SDK. The daemon is left alone either way — Emacs adopts, never kills
#      — so this declines and says which process to stop.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="$(cd "$THIS_DIR/.." && pwd)"

# shellcheck source=lib-realtest-backup.sh
. "$THIS_DIR/lib-realtest-backup.sh"

readonly EXIT_DECLINED=77

readonly EMACS_APP=/Applications/Emacs.app

# The emacsclient inside the bundle, explicitly rather than whatever is on PATH:
# a Homebrew emacsclient of a different version can fail to speak to this
# server, and the failure reads as "Emacs is not answering".
#
# AGENT_REPL_REALTEST_EMACSCLIENT overrides it, and the Go side honours the same
# variable so the two can never disagree about which client they are using. Its
# reason for existing is bin/test-realtest.sh, which has to exercise this
# script's refusals without a real editor on the other end of them.
EMACSCLIENT="${AGENT_REPL_REALTEST_EMACSCLIENT:-$EMACS_APP/Contents/MacOS/bin/emacsclient}"
readonly VENDOR_GUARD_ENV=AGENT_REPL_FORBID_VENDOR_CALLS

# HOW LONG A HUMAN'S IDLE COUNTS AS PRESENT. Reported rather than acted on: the
# refusal below does not depend on it, because a cold start quits the editor no
# matter how idle it is and only the owner can consent to that. It is printed so
# the operator asking for a takeover knows whether they are interrupting
# someone.
readonly HUMAN_IDLE_SECONDS=300

decline() {
    printf '[realtest] DECLINED: %s\n' "$1" >&2
    exit "$EXIT_DECLINED"
}

note() { printf '[realtest] %s\n' "$1"; }

# ---- preflight: the tools -------------------------------------------------

[ -d "$EMACS_APP" ] || decline "there is no Emacs.app at $EMACS_APP; a realtest drives the real editor and has nothing to drive"
[ -x "$EMACSCLIENT" ] || decline "$EMACSCLIENT is missing or not executable; the run cannot read Emacs's state"
command -v sqlite3 >/dev/null 2>&1 || decline "sqlite3 is not on PATH; the run cannot read which workspaces the state database holds"
command -v osascript >/dev/null 2>&1 || decline "osascript is not on PATH; the run cannot tell whether a launch moved the owner's focus"
command -v swiftc >/dev/null 2>&1 || decline "swiftc is not on PATH; the real-key-event helper cannot be compiled, and a realtest does not fall back to elisp"
command -v go >/dev/null 2>&1 || decline "go is not on PATH"

# ---- refusal 1: everything deployed ---------------------------------------
#
# Read from the readiness report rather than re-derived: it is the judge, it is
# the same `.source-tree` stamp comparison bin/build-frontend.sh rebuilds on,
# and a second spelling here could disagree with the build.

note "checking that every deployed system is at this checkout's revision"
if ! READINESS="$("$THIS_DIR/readiness-report.sh")"; then
    decline "bin/readiness-report.sh could not produce a report, so whether the deployed stack matches this checkout is unknown"
fi

NOT_READY="$(printf '%s' "$READINESS" | python3 -c '
import json, sys

report = json.load(sys.stdin)
for system in report["systems"]:
    if system.get("ready"):
        continue
    print("{name}: {error}".format(
        name=system["name"],
        error=system.get("error") or "not ready"))
')"
if [ -n "$NOT_READY" ]; then
    printf '[realtest] DECLINED: a realtest measures the DEPLOYED stack, and these systems are not at this checkout:\n' >&2
    printf '%s\n' "$NOT_READY" >&2
    printf '[realtest] run bin/deploy-all.sh, then try again.\n' >&2
    exit "$EXIT_DECLINED"
fi
note "every deployed system is at this checkout's revision"

# Record the stamps this run exercised. They are what a report says the run was
# MEASURING; without them a finding cannot be tied to a build.
RUN_STAMP="$(realtest_backup_stamp)"
RUN_DIR="${AGENT_REPL_REALTEST_OUT:-$HOME/.claude-emacs/realtest/realtest-$RUN_STAMP}"
mkdir -p "$RUN_DIR"
printf '%s\n' "$READINESS" > "$RUN_DIR/readiness.json"
note "run directory: $RUN_DIR"
note "deployed revisions recorded in $RUN_DIR/readiness.json"

# ---- the Emacs server socket ----------------------------------------------
#
# Derived from `TMPDIR` the way Emacs derives `server-socket-dir`, rather than
# hardcoded: the /var/folders path is per-user and per-boot.

# `TMPDIR` carries a trailing slash on macOS and a bare `/tmp` does not, so the
# separator is normalized rather than assumed; getting it wrong produces a
# socket path that exists nowhere and reads as "Emacs is not running".
EMACS_TMPDIR="${TMPDIR:-/tmp}"
EMACS_SOCKET="${AGENT_REPL_REALTEST_EMACS_SOCKET:-${EMACS_TMPDIR%/}/emacs$(id -u)/server}"
note "emacs server socket: $EMACS_SOCKET"

emacs_answering() {
    "$EMACSCLIENT" --socket-name "$EMACS_SOCKET" --eval '(emacs-pid)' >/dev/null 2>&1
}

# ---- refusal 3, checked before 2: the vendor guard ------------------------
#
# Before the backups and before the takeover, because a stack that cannot hold
# the guard must not have the owner's editor quit for it.

DAEMON_PID="$(pgrep -f "$MODULE_ROOT/daemon/bin/claude-repld" 2>/dev/null | head -n1 || true)"
if [ -n "$DAEMON_PID" ]; then
    note "a daemon is running as pid $DAEMON_PID; checking its environment for the vendor guard"
    if ! ps -Eww -o command= -p "$DAEMON_PID" 2>/dev/null | tr ' ' '\n' | grep -q "^$VENDOR_GUARD_ENV="; then
        printf '[realtest] DECLINED: the running daemon (pid %s) does not carry %s.\n' "$DAEMON_PID" "$VENDOR_GUARD_ENV" >&2
        printf '[realtest] Emacs ADOPTS an answering daemon and never kills one, so the new Emacs would inherit\n' >&2
        printf '[realtest] this one and it would spawn shims with the real SDK reachable. Stop it (SPC o C-d from\n' >&2
        printf '[realtest] the editor, or kill %s) so the realtest'"'"'s Emacs spawns a guarded one, then try again.\n' "$DAEMON_PID" >&2
        exit "$EXIT_DECLINED"
    fi
    note "the running daemon carries $VENDOR_GUARD_ENV"
else
    note "no daemon is running; the realtest's Emacs will spawn one under the guard"
fi

# ---- the backups ----------------------------------------------------------
#
# Before the takeover, unconditionally, and before anything is launched. See
# lib-realtest-backup.sh for why an existing backup is never overwritten.

WSM_DB="$HOME/.claude-emacs/wsm.db"
EVENTS_DB="$HOME/.cache/agent-repl/store/events.db"

note "backing up the owner's live state, stamp $RUN_STAMP"
BACKUPS=""
if ! BACKUPS="$(realtest_backup_database "$WSM_DB" "$RUN_STAMP")"; then
    decline "the workspace state database could not be backed up; nothing is run against state that has no copy"
fi
if ! EVENT_BACKUPS="$(realtest_backup_database "$EVENTS_DB" "$RUN_STAMP")"; then
    decline "the store's events database could not be backed up; nothing is run against state that has no copy"
fi
BACKUPS="$BACKUPS
$EVENT_BACKUPS"

printf '[realtest] backups taken:\n'
printf '%s\n' "$BACKUPS" | while IFS= read -r path; do
    [ -n "$path" ] && printf '  %s\n' "$path"
done
printf '%s\n' "$BACKUPS" > "$RUN_DIR/backups.txt"

# ---- refusal 2: a human is using Emacs ------------------------------------

if emacs_answering; then
    IDLE="$("$EMACSCLIENT" --socket-name "$EMACS_SOCKET" --eval \
        '(let ((idle (current-idle-time))) (if idle (float-time idle) 0.0))' 2>/dev/null || printf 'unknown')"
    note "an Emacs is answering; it has been idle for ${IDLE}s (a human counts as present under ${HUMAN_IDLE_SECONDS}s)"
    if [ "${AGENT_REPL_REALTEST_TAKEOVER:-}" != "1" ]; then
        printf '[realtest] DECLINED: an Emacs is running and a cold start has to quit it.\n' >&2
        printf '[realtest] That is the owner'"'"'s editor, with the owner'"'"'s unsaved work in it, and this script\n' >&2
        printf '[realtest] does not decide to close it. Set AGENT_REPL_REALTEST_TAKEOVER=1 to say go ahead.\n' >&2
        printf '[realtest] The backups above were taken first and are already on disk.\n' >&2
        exit "$EXIT_DECLINED"
    fi
    note "AGENT_REPL_REALTEST_TAKEOVER=1: quitting the running Emacs"
    # `kill-emacs`, not `save-buffers-kill-emacs`: the second one PROMPTS, and a
    # prompt on a headless takeover hangs the run holding the owner's editor open
    # on a modal question nobody will answer. It does not save; the refusal above
    # is what protects unsaved work.
    "$EMACSCLIENT" --socket-name "$EMACS_SOCKET" --eval '(kill-emacs)' >/dev/null 2>&1 || true
    for _ in $(seq 1 60); do
        emacs_answering || break
        sleep 1
    done
    if emacs_answering; then
        decline "the running Emacs is still answering $EMACS_SOCKET a minute after (kill-emacs); the run stops rather than launching a second Emacs onto the same socket"
    fi
    note "the running Emacs has exited"
else
    note "no Emacs is answering; nothing to take over"
fi

# ---- the run --------------------------------------------------------------
#
# THE SUITE SLOT. Every suite here is sized to fill the machine, and a realtest
# is worse than most: it measures a startup, so another suite's load turns a
# healthy phase into a reported breach. bin/suite-slot.sh nests, so wrapping
# here is safe even under an outer holder.

note "the vendor is forbidden for this run: $VENDOR_GUARD_ENV=1 on Emacs and everything it spawns"
note "no picture is taken, and Emacs is never brought frontmost"

set +e
AGENT_REPL_REALTEST=1 \
AGENT_REPL_REALTEST_OUT="$RUN_DIR" \
AGENT_REPL_REALTEST_EMACS_SOCKET="$EMACS_SOCKET" \
AGENT_REPL_REALTEST_EMACSCLIENT="$EMACSCLIENT" \
"$THIS_DIR/suite-slot.sh" \
    go -C "$MODULE_ROOT/e2e" test -tags realtest ./realtest/ \
    -run 'TestRealtest' -count=1 -v -timeout 60m "$@"
status=$?
set -e

printf '\n[realtest] the run directory is %s\n' "$RUN_DIR"
if [ -f "$RUN_DIR/MANIFEST.md" ]; then
    printf '[realtest] read %s: it carries every phase measurement and every warning,\n' "$RUN_DIR/MANIFEST.md"
    printf '[realtest] error, malformed record and stray stderr line inside the run window, verbatim.\n'
    printf '[realtest] Nothing was fixed. The owner rules on each finding (docs/REALTEST-PLAN.md).\n'
else
    printf '[realtest] no MANIFEST.md was written: the run did not reach its harvest.\n'
fi
printf '[realtest] the owner'"'"'s editor is left running.\n'

exit "$status"
