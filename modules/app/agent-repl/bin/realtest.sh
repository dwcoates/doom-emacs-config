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
#   bin/realtest.sh 2 3 4                        a sweep, by number, in that order
#
#   exit 0   every realtest that was asked for ran and passed
#   exit 77  DECLINED, and the message says why; NOTHING ran
#   exit 78  INCOMPLETE: what ran passed, but at least one realtest was SKIPPED
#            because the world its precondition demands could not be established
#   other    a realtest failed
#
# 77 is the autotools "skipped" convention, used rather than 0 so a run can
# never report a green realtest that did not execute — the same convention every
# other declining entry point in this module uses. 78 exists for the same
# reason one layer in: a sweep that could not give one of its tests the world
# that test demands must not report the sweep as green.
#
# A SWEEP IS SEQUENCED, ONE `go test` INVOCATION PER REALTEST, because the
# realtests do not share a world. Realtests 1 and 5 through 8 each perform
# their own cold start and REFUSE if an Emacs is already answering; realtest 2
# needs a daemon already serving and quits the standing editor itself; realtest
# 3 needs no daemon running at all. Running them in one invocation left the
# later ones facing an editor the earlier ones started, which is why every
# sweep past realtest 2 used to fail in a tenth of a second having done
# nothing. The preconditions are right; the runner is what establishes the
# world for each of them, from the table in "the world each realtest demands"
# below. Nothing here relaxes a test's own refusal: each one still checks, and
# a world the runner cannot establish makes that realtest a reported SKIP
# rather than a run into a guaranteed failure.
#
# THE BACKUPS AND THE PRUNE HAPPEN ONCE PER RUN, not once per realtest. They
# capture the state as it was BEFORE the run began, and re-taking them between
# tests would overwrite that with state the run itself produced (and cost
# another clone of a multi-gigabyte database each time).
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
#      ONE CONSENT COVERS THE WHOLE RUN, INCLUDING EVERY QUIT INSIDE A SWEEP.
#      A sweep of the cold-start realtests quits the editor once per test, and
#      the refusal below says how many quits the plan holds before any of them
#      happens, so the consent is never wider than what the operator was told.
#      The variable answers "may this run close my editor", and a run that may
#      close it once may close it again while it is running: the second and
#      later editors are ones the run itself started, and the owner's own
#      session ended at the first quit. What the consent does NOT cover is the
#      DAEMON — see below.
#
#   2b. THE DAEMON IS A SEPARATE CONSENT. Realtest 3 measures a startup with
#      the daemon DOWN, and stopping the daemon ends every live session it
#      holds — shims, in-flight turns and all — which the editor takeover says
#      nothing about. AGENT_REPL_REALTEST_STOP_DAEMON=1 is the owner saying the
#      run may stop it. Without that variable realtest 3 is SKIPPED with the
#      reason (and the run exits 78) rather than run into the refusal it would
#      certainly hit. The runner sends SIGTERM only; a daemon that does not go
#      makes realtest 3 a skip too, because escalating to SIGKILL on the
#      owner's daemon is not a decision this script makes.
#
#   3. THE VENDOR GUARD CANNOT BE HELD. A daemon already running without
#      AGENT_REPL_FORBID_VENDOR_CALLS in its environment would be adopted by the
#      new Emacs and would spawn shims without it, so a prompt could reach the
#      real SDK. The SHIMS are checked in their own right for the same reason:
#      a shim already listening on a workspace socket is ADOPTED by the daemon
#      rather than respawned, so a shim that predates the guard keeps its old
#      environment — which is exactly what happened in realtest 1's first run,
#      where a day-old shim submitted a keepalive prompt to the real vendor
#      every four minutes. Nothing is killed either way; this declines and names
#      every process to stop.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="$(cd "$THIS_DIR/.." && pwd)"

# shellcheck source=lib-realtest-backup.sh
. "$THIS_DIR/lib-realtest-backup.sh"

readonly EXIT_DECLINED=77
readonly EXIT_INCOMPLETE=78

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

# ---- the world each realtest demands --------------------------------------
#
# One row per realtest: NUMBER|GO TEST NAME|EMACS|DAEMON. This is the RUNNER'S
# knowledge — what each test's own precondition requires of the world before it
# starts — and bin/test-realtest.sh asserts that every `func TestRealtest*` in
# e2e/realtest/ has a row here, so a new realtest cannot be added without the
# runner being told what world it needs.
#
# EMACS:
#   absent   — the test performs its own cold start and REFUSES if an Emacs is
#              answering (e2e/realtest/realtest_1_..., `coldStart`). The runner
#              quits the standing editor first.
#   selfquit — the test quits the standing editor ITSELF, inside its own run
#              window, because the quit is part of what it measures (realtest
#              2's restart). The runner leaves the editor alone: quitting it
#              here would turn the restart into a plain cold start. It still
#              costs a quit, so it still needs the takeover consent.
#   keep     — the test adopts whatever is standing (realtest 4). The runner
#              touches nothing.
#
# DAEMON:
#   present — a daemon must already be serving (realtest 2 measures an
#             ADOPTION). The runner cannot start one, so it skips the test with
#             the reason when none is running.
#   absent  — no daemon may be running (realtest 3). The runner stops it only
#             under AGENT_REPL_REALTEST_STOP_DAEMON=1, and skips otherwise.
#   any     — the test works either way.
REALTEST_WORLDS='1|TestRealtestStartTheEditor|absent|any
2|TestRealtestRestartWithTheDaemonUp|selfquit|present
3|TestRealtestStartWithTheDaemonDown|absent|absent
4|TestRealtestSwitchBetweenWorkspaces|keep|any
5|TestRealtestCreateWorkDeleteAWorkspace|absent|any
6|TestRealtestRegisterAndReopen|absent|any
7|TestRealtestForkAWorkspace|absent|any
8|TestRealtestPriorityCloseReopenKill|absent|any'

row_field() { printf '%s' "$1" | cut -d'|' -f"$2"; }

known_selectors() {
    printf '%s\n' "$REALTEST_WORLDS" | while IFS= read -r row; do
        printf '  %s  %s\n' "$(row_field "$row" 1)" "$(row_field "$row" 2)"
    done
}

# ---- the selection, parsed before anything is touched ---------------------
#
# A bad selector must cost nothing: it is caught here, before the readiness
# report, before the backups and before a single process is looked at.
#
# Anything that is not a selector and starts with `-` is passed through to
# every `go test` invocation, as `"$@"` always was. Give such a flag its value
# in `-flag=value` form: a detached value would be read as a selector, and
# rather than guess, an unrecognized selector declines and lists the ones that
# exist.

SELECTORS=()
GO_ARGS=()
RUN_REGEX=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        -run)
            [ "$#" -ge 2 ] || decline "-run was given with no test pattern after it"
            RUN_REGEX="$2"
            shift 2
            ;;
        -run=*)
            RUN_REGEX="${1#-run=}"
            shift
            ;;
        -*)
            GO_ARGS+=("$1")
            shift
            ;;
        *)
            SELECTORS+=("$1")
            shift
            ;;
    esac
done

if [ -n "$RUN_REGEX" ] && [ "${#SELECTORS[@]}" -gt 0 ]; then
    decline "-run and a realtest selector were both given; use one or the other"
fi

PLAN=""
if [ -n "$RUN_REGEX" ]; then
    while IFS= read -r row; do
        if printf '%s' "$(row_field "$row" 2)" | grep -Eq -- "$RUN_REGEX"; then
            PLAN="$PLAN$row
"
        fi
    done <<EOF
$REALTEST_WORLDS
EOF
    if [ -z "$PLAN" ]; then
        printf '[realtest] DECLINED: -run %s matches no realtest. These exist:\n' "$RUN_REGEX" >&2
        known_selectors >&2
        exit "$EXIT_DECLINED"
    fi
elif [ "${#SELECTORS[@]}" -gt 0 ]; then
    for selector in "${SELECTORS[@]}"; do
        MATCHED=""
        while IFS= read -r row; do
            if [ "$selector" = "$(row_field "$row" 1)" ] || [ "$selector" = "$(row_field "$row" 2)" ]; then
                MATCHED="$row"
            fi
        done <<EOF
$REALTEST_WORLDS
EOF
        if [ -z "$MATCHED" ]; then
            printf '[realtest] DECLINED: %s is not a realtest. These exist:\n' "$selector" >&2
            known_selectors >&2
            exit "$EXIT_DECLINED"
        fi
        # Asked for twice is run once, in the position it was first asked for:
        # a realtest is a measurement of the owner's real editor and repeating
        # one by accident costs the owner another startup.
        case "
$PLAN" in
            *"
$MATCHED
"*) note "realtest $(row_field "$MATCHED" 1) was asked for more than once; it runs once" ;;
            *) PLAN="$PLAN$MATCHED
" ;;
        esac
    done
else
    PLAN="$REALTEST_WORLDS
"
fi

PLAN_COUNT="$(printf '%s' "$PLAN" | grep -c '|')"

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

# process_carries_guard PID — does the KERNEL's copy of this process's
# environment carry the guard? Not the launcher's intention, not this shell's
# exported environment: the copy `ps -Eww` prints beside the command line.
process_carries_guard() {
    ps -Eww -o command= -p "$1" 2>/dev/null | tr ' ' '\n' | grep -q "^$VENDOR_GUARD_ENV="
}

# daemon_pids — every resident daemon of THIS checkout, one pid per line, and
# nothing on stdout when there is none. One spelling, because the vendor-guard
# refusal below and the world realtest 3 demands must not disagree about what
# counts as a running daemon.
daemon_pids() {
    pgrep -f "$MODULE_ROOT/daemon/bin/claude-repld" 2>/dev/null || true
}

DAEMON_PID="$(daemon_pids | head -n1)"
if [ -n "$DAEMON_PID" ]; then
    note "a daemon is running as pid $DAEMON_PID; checking its environment for the vendor guard"
    if ! process_carries_guard "$DAEMON_PID"; then
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

# ---- refusal 3, second half: the shims a dead daemon left behind ----------
#
# CHECKING THE DAEMON IS NOT ENOUGH, and realtest 1's first run is the proof. A
# shim spawned the day before by an UNGUARDED daemon was still listening on its
# socket; the guarded daemon this run's Emacs brought up ADOPTED it rather than
# spawning a fresh one, and it submitted a keepalive prompt to the real vendor
# every four minutes for the whole run. The guard the daemon carried never
# reached that process, because that process predates it.
#
# So every listening shim under the state directory's sock/ is enumerated and
# checked in its own right, and so is every shim-lock — the helper the shim
# spawns to hold its lock, which inherits the shim's environment and therefore
# tells the same story about it.

STATE_DIR="${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}"
SOCK_DIR="${STATE_DIR%/}/sock"

UNGUARDED=""
note "checking every listening shim under $SOCK_DIR for the vendor guard"
for pid in $(pgrep -f 'shim/dist/main\.js' 2>/dev/null || true); do
    COMMAND_LINE="$(ps -Eww -o command= -p "$pid" 2>/dev/null || true)"
    [ -n "$COMMAND_LINE" ] || continue
    # Only a shim listening under THIS state directory: another checkout's
    # shim is not a process this run would adopt, and declining on it would
    # send the operator after the wrong thing.
    case "$COMMAND_LINE" in
        *"--listen $SOCK_DIR/"*) ;;
        *) continue ;;
    esac
    SOCKET="$(printf '%s' "$COMMAND_LINE" | tr ' ' '\n' | grep "^$SOCK_DIR/" | head -n1)"
    if ! process_carries_guard "$pid"; then
        UNGUARDED="$UNGUARDED
  shim pid $pid listening on ${SOCKET:-(no socket named on its command line)}"
    fi
done

for pid in $(pgrep -f 'agent-repl/bin/shim-lock' 2>/dev/null || true); do
    ps -Eww -o command= -p "$pid" >/dev/null 2>&1 || continue
    if ! process_carries_guard "$pid"; then
        UNGUARDED="$UNGUARDED
  shim-lock pid $pid"
    fi
done

if [ -n "$UNGUARDED" ]; then
    printf '[realtest] DECLINED: these shim processes do not carry %s:\n' "$VENDOR_GUARD_ENV" >&2
    printf '%s\n' "$UNGUARDED" >&2
    printf '[realtest] A daemon ADOPTS a shim that is already listening on a workspace'"'"'s socket rather than\n' >&2
    printf '[realtest] spawning a fresh one, so the guard this run puts on the daemon would never reach these.\n' >&2
    printf '[realtest] One of them kept a keepalive prompt going to the real vendor through realtest 1'"'"'s first\n' >&2
    printf '[realtest] run. Stop them (kill the pids above) so the run'"'"'s daemon spawns guarded shims, then\n' >&2
    printf '[realtest] try again.\n' >&2
    exit "$EXIT_DECLINED"
fi
note "every listening shim carries $VENDOR_GUARD_ENV"


# ---- the backups ----------------------------------------------------------
#
# Before the takeover, unconditionally, and before anything is launched. See
# lib-realtest-backup.sh for why an existing backup is never overwritten, why
# the copy is a clone rather than a full copy, and why it is pruned after.

# THE WORKSPACE STATE IS COPIED; THE STORE IS NOT. The store is a cache and
# during development it needs no retention (owner ruling 2026-09-13): every
# record in events.db is re-derivable, which is why bin/store-reset.sh answers
# a store that has outgrown its host by deleting the file. Its per-run copies
# were not a copy of anything irreplaceable and were not read by the harvest --
# they were 4.6GB of disk across three runs, because a clone diverges as the
# live database is written and starts costing real blocks. The workspace state
# has no such property: wsm.db is the owner's workspaces, branches, selections
# and held prompts, and nothing re-derives it.
WSM_DB="$HOME/.claude-emacs/wsm.db"

# A clone still needs a floor of real free space for its own metadata and for
# the -wal/-shm plain-copy fallback; this is a cheap sanity floor, not the
# thing that makes backups affordable (the clone is).
readonly BACKUP_FREE_FLOOR_KIB=$((2 * 1024 * 1024))
BACKUP_FREE_KIB="$(realtest_free_kib "$HOME")"
if [ -n "$BACKUP_FREE_KIB" ] && [ "$BACKUP_FREE_KIB" -lt "$BACKUP_FREE_FLOOR_KIB" ]; then
    decline "only ${BACKUP_FREE_KIB}KiB free on the volume backing \$HOME; a backup needs headroom even as a clone, and this run stops before touching anything with less than 2GiB free"
fi

note "backing up the owner's workspace state, stamp $RUN_STAMP"
BACKUPS=""
if ! BACKUPS="$(realtest_backup_database "$WSM_DB" "$RUN_STAMP")"; then
    decline "the workspace state database could not be backed up; nothing is run against state that has no copy"
fi

printf '[realtest] backups taken:\n'
printf '%s\n' "$BACKUPS" | while IFS= read -r path; do
    [ -n "$path" ] && printf '  %s\n' "$path"
done
printf '%s\n' "$BACKUPS" > "$RUN_DIR/backups.txt"

# Prune AFTER a successful backup, never before: a run that is about to
# decline over a failed backup must not first destroy an older one that a
# human might still need to fall back to.
BACKUP_KEEP="${AGENT_REPL_REALTEST_BACKUP_KEEP:-3}"
note "pruning backups, keeping the $BACKUP_KEEP most recent sets"
PRUNED="$(realtest_prune_backups "$WSM_DB" "$BACKUP_KEEP")"
if [ -n "$(printf '%s' "$PRUNED" | tr -d '[:space:]')" ]; then
    printf '[realtest] backups pruned:\n'
    # `[ -n "$path" ] && printf` leaves the loop's exit status at 1 whenever the
    # last line read is empty, which under `set -e` would abort the whole run
    # right here. `|| true` keeps a cosmetic print from ending the run.
    printf '%s\n' "$PRUNED" | while IFS= read -r path; do
        [ -n "$path" ] && printf '  %s\n' "$path"
    done || true
else
    note "nothing to prune"
fi

# ---- refusal 2: a human is using Emacs ------------------------------------
#
# CHECKED FOR THE WHOLE PLAN, BEFORE THE FIRST REALTEST RUNS. A sweep quits the
# editor once per cold-start realtest, and the operator is told the count here
# rather than discovering it a quit at a time. The check is after the backups
# on purpose: an operator who then sets the flag is running against state that
# already has a copy (the refusal says so).

# RUN_OWNS_EDITOR — is the editor that is answering one this RUN started?
#
# It is 0 while the editor standing on the socket is the one that was there
# before the run, and 1 from the moment that editor is gone: every editor after
# that was launched by a realtest in this run. The takeover consent is about
# the OWNER'S editor and the owner's unsaved work, so it is required for the
# first quit and not for the run's own restarts — which is what lets a sweep
# quit between its cold-start realtests without asking again.
RUN_OWNS_EDITOR=0
if ! emacs_answering; then
    RUN_OWNS_EDITOR=1
fi

# quit_standing_emacs — quit the editor answering the socket and wait for it to
# go. Non-zero if there is still one answering afterwards, or if the consent
# was not given.
#
# The consent is re-read here rather than assumed from the plan's check: a
# function that closes the owner's editor must not depend on its caller having
# asked first.
quit_standing_emacs() {
    if ! emacs_answering; then
        note "no Emacs is answering; nothing to take over"
        return 0
    fi
    IDLE="$("$EMACSCLIENT" --socket-name "$EMACS_SOCKET" --eval \
        '(let ((idle (current-idle-time))) (if idle (float-time idle) 0.0))' 2>/dev/null || printf 'unknown')"
    note "an Emacs is answering; it has been idle for ${IDLE}s (a human counts as present under ${HUMAN_IDLE_SECONDS}s)"
    if [ "$RUN_OWNS_EDITOR" = "1" ]; then
        note "this editor was started by this run; quitting it for the next realtest's cold start"
    elif [ "${AGENT_REPL_REALTEST_TAKEOVER:-}" != "1" ]; then
        printf '[realtest] an Emacs is running and a cold start has to quit it.\n' >&2
        printf '[realtest] That is the owner'"'"'s editor, with the owner'"'"'s unsaved work in it, and this script\n' >&2
        printf '[realtest] does not decide to close it. Set AGENT_REPL_REALTEST_TAKEOVER=1 to say go ahead.\n' >&2
        return 1
    else
        note "AGENT_REPL_REALTEST_TAKEOVER=1: quitting the running Emacs"
    fi
    # `kill-emacs`, not `save-buffers-kill-emacs`: the second one PROMPTS, and a
    # prompt on a headless takeover hangs the run holding the owner's editor open
    # on a modal question nobody will answer. It does not save; the refusal above
    # is what protects unsaved work.
    "$EMACSCLIENT" --socket-name "$EMACS_SOCKET" --eval '(kill-emacs)' >/dev/null 2>&1 || true
    for _ in $(seq 1 60); do
        if ! emacs_answering; then
            break
        fi
        sleep 1
    done
    if emacs_answering; then
        printf '[realtest] the running Emacs is still answering %s a minute after (kill-emacs); nothing is\n' "$EMACS_SOCKET" >&2
        printf '[realtest] launched onto the same socket.\n' >&2
        return 1
    fi
    note "the running Emacs has exited"
    RUN_OWNS_EDITOR=1
    return 0
}

# WHAT THE PLAN COSTS THE EDITOR, worked out before the first realtest runs.
#
# Every realtest leaves an editor running, so after the first one there is
# always something standing for the next cold-start realtest to quit. Two
# things are counted separately:
#
#   PLANNED_QUITS  — every quit, so the operator is told the whole number.
#   NEEDS_CONSENT  — whether any of them needs the owner's answer. A quit of
#                    an editor THIS RUN started does not; the first quit of an
#                    editor that was already there does. A `selfquit` realtest
#                    always does, because the test's own guard
#                    (startup_shared_test.go, quitStandingEmacs) asks for the
#                    flag whenever an editor is answering, and a run that
#                    reached it without the flag would fail there instead.
PLANNED_QUITS=0
NEEDS_CONSENT=0
STANDING=0
OWNED="$RUN_OWNS_EDITOR"
if emacs_answering; then
    STANDING=1
fi
while IFS= read -r row; do
    [ -n "$row" ] || continue
    EMACS_WORLD="$(row_field "$row" 3)"
    case "$EMACS_WORLD" in
        absent|selfquit)
            if [ "$STANDING" = "1" ]; then
                PLANNED_QUITS=$((PLANNED_QUITS + 1))
                if [ "$EMACS_WORLD" = "selfquit" ] || [ "$OWNED" != "1" ]; then
                    NEEDS_CONSENT=1
                fi
                OWNED=1
            fi
            ;;
    esac
    STANDING=1
done <<EOF
$PLAN
EOF

if [ "$NEEDS_CONSENT" = "1" ] && [ "${AGENT_REPL_REALTEST_TAKEOVER:-}" != "1" ]; then
    printf '[realtest] DECLINED: this run quits Emacs %d time(s), and a running editor is the owner'"'"'s,\n' "$PLANNED_QUITS" >&2
    printf '[realtest] with the owner'"'"'s unsaved work in it. This script does not decide to close it.\n' >&2
    printf '[realtest] Set AGENT_REPL_REALTEST_TAKEOVER=1 to say go ahead; that one answer covers every\n' >&2
    printf '[realtest] quit in this run, and the count above is the whole of what it authorizes.\n' >&2
    printf '[realtest] The backups above were taken first and are already on disk.\n' >&2
    exit "$EXIT_DECLINED"
fi
if [ "$PLANNED_QUITS" -gt 0 ]; then
    note "this run quits Emacs $PLANNED_QUITS time(s)"
fi

# ---- realtest 3's world: the daemon stopped, under its own consent --------
#
# SEPARATE FROM THE TAKEOVER, because it costs something the takeover says
# nothing about: every live session the daemon holds. Without the consent this
# is a SKIP with the reason, never a run into realtest 3's own refusal.

# How long the daemon is given to exit after SIGTERM before realtest 3 is
# skipped. A daemon drains its shims on the way out; this is a small multiple
# of that, not a guess at a machine's speed.
#
# AGENT_REPL_REALTEST_DAEMON_STOP_SECONDS shortens it for bin/test-realtest.sh,
# which asserts what happens when a daemon does NOT go and must not wait out a
# real drain to do it.
readonly DAEMON_STOP_SECONDS="${AGENT_REPL_REALTEST_DAEMON_STOP_SECONDS:-30}"

# The kill used to stop it. A variable so bin/test-realtest.sh can watch what
# would be signalled without a process to signal, the same reason
# AGENT_REPL_REALTEST_EMACSCLIENT exists.
REALTEST_KILL="${AGENT_REPL_REALTEST_KILL:-/bin/kill}"

DAEMON_SKIP_REASON=""
establish_no_daemon() {
    DAEMON_SKIP_REASON=""
    local -a pids
    pids=()
    while IFS= read -r pid; do
        [ -n "$pid" ] || continue
        pids+=("$pid")
    done < <(daemon_pids)
    if [ "${#pids[@]}" -eq 0 ]; then
        note "no daemon is running; this realtest's cold start has to bring one up"
        return 0
    fi
    if [ "${AGENT_REPL_REALTEST_STOP_DAEMON:-}" != "1" ]; then
        DAEMON_SKIP_REASON="a daemon is running (pid(s) ${pids[*]}) and this realtest measures a startup with the daemon DOWN. Stopping it ends every live session it holds, which the editor takeover does not cover: set AGENT_REPL_REALTEST_STOP_DAEMON=1 to let this run stop it, or stop it deliberately (SPC o C-d from the editor, or kill ${pids[*]}) and run this realtest on its own."
        return 1
    fi
    note "AGENT_REPL_REALTEST_STOP_DAEMON=1: stopping the daemon (pid(s) ${pids[*]}) with SIGTERM"
    "$REALTEST_KILL" -TERM "${pids[@]}" >/dev/null 2>&1 || true
    local _i
    for _i in $(seq 1 "$DAEMON_STOP_SECONDS"); do
        if [ -z "$(daemon_pids)" ]; then
            break
        fi
        sleep 1
    done
    local left
    left="$(daemon_pids | tr '\n' ' ')"
    if [ -n "${left// /}" ]; then
        # NO SIGKILL. Escalating on the owner's daemon is not a decision this
        # script makes, and a daemon that ignored SIGTERM is a finding in its
        # own right rather than something to force past.
        DAEMON_SKIP_REASON="the daemon (pid(s) ${left% }) was still running ${DAEMON_STOP_SECONDS}s after SIGTERM. This run does not escalate to SIGKILL on the owner's daemon, so this realtest is skipped rather than run with a daemon up."
        return 1
    fi
    note "the daemon has stopped"
    return 0
}

# ---- the run --------------------------------------------------------------
#
# THE SUITE SLOT. Every suite here is sized to fill the machine, and a realtest
# is worse than most: it measures a startup, so another suite's load turns a
# healthy phase into a reported breach. bin/suite-slot.sh nests, so wrapping
# here is safe even under an outer holder.

note "the vendor is forbidden for this run: $VENDOR_GUARD_ENV=1 on Emacs and everything it spawns"
note "no picture is taken, and Emacs is never brought frontmost"
note "this run holds $PLAN_COUNT realtest(s), each in its own go test invocation"

# run_one NAME — one realtest, in its own `go test`, under its own suite slot.
# Prints nothing itself; the status is the realtest's.
run_one() {
    local name="$1"
    set +e
    AGENT_REPL_REALTEST=1 \
    AGENT_REPL_REALTEST_OUT="$RUN_DIR" \
    AGENT_REPL_REALTEST_EMACS_SOCKET="$EMACS_SOCKET" \
    AGENT_REPL_REALTEST_EMACSCLIENT="$EMACSCLIENT" \
    "$THIS_DIR/suite-slot.sh" \
        go -C "$MODULE_ROOT/e2e" test -tags realtest ./realtest/ \
        -run "^${name}\$" -count=1 -v -timeout 60m ${GO_ARGS[@]+"${GO_ARGS[@]}"}
    local status=$?
    set -e
    return "$status"
}

RAN=0
SKIPPED=0
FAILED=0
FIRST_FAILURE=0
OUTCOMES=""

while IFS= read -r row; do
    [ -n "$row" ] || continue
    NUMBER="$(row_field "$row" 1)"
    NAME="$(row_field "$row" 2)"
    EMACS_WORLD="$(row_field "$row" 3)"
    DAEMON_WORLD="$(row_field "$row" 4)"

    printf '\n'
    note "realtest $NUMBER — $NAME (world: emacs=$EMACS_WORLD daemon=$DAEMON_WORLD)"

    SKIP_REASON=""

    # The editor half of the world.
    case "$EMACS_WORLD" in
        absent)
            if ! quit_standing_emacs; then
                SKIP_REASON="an Emacs is answering $EMACS_SOCKET and this realtest performs its own cold start, which refuses against a standing editor. The quit above says why it did not happen."
            fi
            ;;
        selfquit)
            note "this realtest quits the standing editor itself, inside its own run window; the runner leaves it alone"
            ;;
        keep)
            note "this realtest adopts whatever editor is standing; the runner leaves it alone"
            ;;
        *)
            SKIP_REASON="the world table gives realtest $NUMBER an emacs world of '$EMACS_WORLD', which this runner does not know how to establish"
            ;;
    esac

    # The daemon half.
    if [ -z "$SKIP_REASON" ]; then
        case "$DAEMON_WORLD" in
            present)
                if [ -z "$(daemon_pids)" ]; then
                    SKIP_REASON="no daemon is running and this realtest measures an ADOPTION, so there is nothing for the new Emacs to adopt. This runner does not start a daemon: run realtest 1 first (it leaves one behind), or start Emacs by hand."
                else
                    note "a daemon is serving, which is the world this realtest measures an adoption against"
                fi
                ;;
            absent)
                if ! establish_no_daemon; then
                    SKIP_REASON="$DAEMON_SKIP_REASON"
                fi
                ;;
            any) ;;
            *)
                SKIP_REASON="the world table gives realtest $NUMBER a daemon world of '$DAEMON_WORLD', which this runner does not know how to establish"
                ;;
        esac
    fi

    if [ -n "$SKIP_REASON" ]; then
        SKIPPED=$((SKIPPED + 1))
        printf '[realtest] SKIPPED realtest %s (%s): %s\n' "$NUMBER" "$NAME" "$SKIP_REASON" >&2
        OUTCOMES="$OUTCOMES  realtest $NUMBER  SKIPPED  $SKIP_REASON
"
        continue
    fi

    RAN=$((RAN + 1))
    unit_status=0
    run_one "$NAME" || unit_status=$?
    if [ "$unit_status" -eq 0 ]; then
        OUTCOMES="$OUTCOMES  realtest $NUMBER  passed
"
    else
        FAILED=$((FAILED + 1))
        if [ "$FIRST_FAILURE" -eq 0 ]; then
            FIRST_FAILURE="$unit_status"
        fi
        OUTCOMES="$OUTCOMES  realtest $NUMBER  FAILED (exit $unit_status)
"
        note "realtest $NUMBER failed; the remaining realtests still run, so one run gathers every finding"
    fi
done <<EOF
$PLAN
EOF

printf '\n[realtest] the run directory is %s\n' "$RUN_DIR"
printf '[realtest] what this run did:\n'
printf '%s' "$OUTCOMES"

MANIFESTS="$(find "$RUN_DIR" -name MANIFEST.md 2>/dev/null | sort || true)"
if [ -n "$MANIFESTS" ]; then
    printf '[realtest] read these: they carry every phase measurement and every warning,\n'
    printf '[realtest] error, malformed record and stray stderr line inside each run window, verbatim.\n'
    printf '%s\n' "$MANIFESTS" | while IFS= read -r manifest; do
        [ -n "$manifest" ] && printf '  %s\n' "$manifest"
    done || true
    printf '[realtest] Nothing was fixed. The owner rules on each finding (docs/REALTEST-PLAN.md).\n'
else
    printf '[realtest] no MANIFEST.md was written: no realtest reached its harvest.\n'
fi
printf '[realtest] the owner'"'"'s editor is left running.\n'

# THE VERDICT. A failure outranks a skip (something is broken), and a run where
# nothing ran at all is a DECLINE rather than an incomplete run — the same
# reason 77 exists: no exit status this script hands back may read as a green
# realtest that did not execute.
if [ "$FAILED" -gt 0 ]; then
    exit "$FIRST_FAILURE"
fi
if [ "$RAN" -eq 0 ]; then
    printf '[realtest] DECLINED: not one of the %s realtest(s) asked for could be given the world it needs.\n' \
        "$PLAN_COUNT" >&2
    exit "$EXIT_DECLINED"
fi
if [ "$SKIPPED" -gt 0 ]; then
    printf '[realtest] INCOMPLETE: %d realtest(s) ran and passed, %d never ran (reasons above).\n' \
        "$RAN" "$SKIPPED" >&2
    exit "$EXIT_INCOMPLETE"
fi

exit 0
