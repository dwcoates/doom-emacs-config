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
# and NO image, and no picture is taken at this stage. The ONE substitution is
# the vendor: AGENT_REPL_FORBID_VENDOR_CALLS is set on the Emacs process and
# inherited by everything it spawns, so no real Claude call can occur.
#
# FOCUS IS STOLEN ONCE AND HANDED BACK ONCE (owner ruling, 2026-09-13). Emacs is
# brought frontmost before the first realtest and stays there for the whole
# sweep, so the owner can watch the run; the application that was frontmost
# before gets focus back from the EXIT trap, so a failure, a panic and an
# interrupt all return the desktop, and the desktop coming back is how the owner
# knows the sweep is over. `-run` of a single realtest behaves the same way.
#
# modules/app/agent-repl/docs/REALTEST-PLAN.md is the CONTRACT — which realtests
# exist, what each measures, and the remediation loop they feed. e2e/REALTEST-SPEC.md
# documents these mechanics. This script is the only supported entry point,
# because the four refusals below are not optional.
#
#   bin/realtest.sh                              every realtest
#   bin/realtest.sh -run TestRealtestStartTheEditor    one, by name
#   bin/realtest.sh 2 3 4                        a sweep, by number, in that order
#   bin/realtest.sh --clean-leftovers            remove the workspace registry
#                                                rows an earlier sweep left
#                                                behind, and run nothing
#
#   exit 0   every realtest that was asked for ran and passed, the run left no
#            registry row behind, and nothing was written between the sweeps
#   exit 77  DECLINED, and the message says why; NOTHING ran
#   exit 78  INCOMPLETE: what ran passed, but at least one realtest was SKIPPED
#            because the world its precondition demands could not be established
#   other    a realtest failed, or the sweep left a registry row standing, or
#            the gap since the previous sweep held warnings or errors
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
# FOUR REFUSALS, and each one is here because the alternative is worse than not
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
#      real SDK.
#
#      THE DAEMON HALF RESOLVES ITSELF UNDER THE CONSENT IT ALREADY HAS. Every
#      sweep ends by handing the owner a guard-free editor and a guard-free
#      daemon, so every sweep OPENS against an unguarded daemon; refusing it and
#      naming a kill to run by hand made the second and every later sweep
#      decline (owner complaint, 2026-09-13). With
#      AGENT_REPL_REALTEST_STOP_DAEMON=1 the run quits the standing editor first
#      (under the takeover, so it cannot bring an unguarded daemon straight back
#      up) and then stops the daemon the same orderly way realtest 3's world
#      does — SIGTERM, one bound, never SIGKILL — and says so. Without the
#      consent the refusal stands and names setting the variable as the remedy.
#
#      The SHIMS are checked in their own right for the same reason:
#      a shim already listening on a workspace socket is ADOPTED by the daemon
#      rather than respawned, so a shim that predates the guard keeps its old
#      environment — which is exactly what happened in realtest 1's first run,
#      where a day-old shim submitted a keepalive prompt to the real vendor
#      every four minutes. Nothing is killed either way; this declines and names
#      every process to stop.
#
#   4. A PREVIOUS SWEEP'S REGISTRY ROWS ARE STILL STANDING. A realtest that
#      registers or creates a workspace puts a row in the owner's registry
#      naming a directory under the run directory; the directory goes away and
#      the row does not unless something forgets it, and the owner's editor
#      then reports a stale registry row for as long as it stands (owner
#      complaint, 2026-09-13). A sweep that added its own rows on top of one
#      would bury the evidence of which run made it, so this declines and names
#      the remedy: bin/realtest.sh --clean-leftovers.

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-grep-in.sh"

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

# THE LAUNCHER FOR THE EDITOR THE OWNER GETS BACK. `open -gj -a Emacs` starts a
# normal editor without bringing it to the front and without making it the
# active application, so the handback restores the owner's editor without
# taking the desktop back off them a second time.
#
# AGENT_REPL_REALTEST_OPEN overrides it for bin/test-realtest.sh, the same
# reason AGENT_REPL_REALTEST_EMACSCLIENT exists: the thing being asserted is
# which editor the owner is left with, and launching a real one to find out
# would be the failure.
REALTEST_OPEN="${AGENT_REPL_REALTEST_OPEN:-/usr/bin/open}"

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

# note_err — the same line, on stderr. For a function whose STDOUT IS ITS
# ANSWER (`stop_daemons_orderly` prints the pids still standing), where a note
# on stdout would be read back as part of that answer. It is not a warning and
# is not formatted as one; the two streams are interleaved in a terminal and
# both are captured by bin/test-realtest.sh.
note_err() { printf '[realtest] %s\n' "$1" >&2; }

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
8|TestRealtestPriorityCloseReopenKill|absent|any
9|TestRealtestSendAPrompt|absent|any'

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
CLEAN_LEFTOVERS=0
while [ "$#" -gt 0 ]; do
    case "$1" in
        --clean-leftovers)
            CLEAN_LEFTOVERS=1
            shift
            ;;
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

# ---- the leftover registry rows a previous sweep left behind --------------
#
# A realtest that registers or creates a workspace puts a row in the OWNER'S
# registry naming a directory under the run directory. The run directory goes
# away; the row does not, unless something removes it. What the owner then sees
# is their editor reporting a stale registry row for as long as it stands:
#
#   workspace "workspace-c22fed997b234b27" cannot host a durable log sink
#   (registered-dir=... [MISSING]); its records are written centrally
#
# That is the module telling the truth about a mess a realtest made (owner
# complaint, 2026-09-13). So the sweep now owns three moments:
#
#   START — decline if any row names a directory under the realtest root.
#           A leftover must never go unnoticed, and a sweep that piled its own
#           rows on top of one would bury the evidence of which run made it.
#   END   — close and forget every row under THIS run's directory, through the
#           daemon, however the sweep ended; then fail the sweep for any that
#           survived.
#   --clean-leftovers — the operator's own way to clear what an older sweep
#           left, without running a realtest at all.
#
# Every one of them goes through the SAME implementation
# (e2e/realtest/leftovers.go, driven by TestCleanRealtestLeftovers), because a
# second spelling of "which rows are a run's" in bash is how the two would come
# to disagree about the owner's registry.

# THE OWNER'S STATE ROOT, spelled once. A realtest drives the owner's real
# Emacs.app, launched through LaunchServices with launchd's environment rather
# than this shell's, so its state lives at ~/.claude-emacs — the same root the
# Go harness measures (e2e/realtest, RealEnv). It deliberately does NOT honor
# $AGENT_REPL_STATE_DIR: in the caller's shell that variable describes the
# daemon that spawned the CALLER (an agent session inherits it), not the editor
# this run launches, and honoring it once pointed the unguarded-shim scan at a
# different directory than every other check here looked at.
readonly OWNER_STATE_DIR="$HOME/.claude-emacs"

readonly REALTEST_ROOT="$OWNER_STATE_DIR/realtest"

# run_harness_check NAME ENV... — one non-realtest check in the realtest
# package.
#
# NOT UNDER bin/suite-slot.sh, and that is deliberate rather than an omission.
# The slot exists because every SUITE here is sized to fill the machine; these
# are single-process reads — a snapshot of wsm.db, a walk of the log files, two
# read-only emacsclient probes — that start no editor, spawn no daemon and run
# nothing in parallel. Holding a machine-wide slot for one would make a sweep
# wait on somebody else's vitest run to find out whether the owner's registry
# is clean.
run_harness_check() {
    local name="$1"
    shift
    env "$@" go -C "$MODULE_ROOT/e2e" test -tags realtest ./realtest/ \
        -run "^${name}\$" -count=1 -v -timeout 10m
}

if [ "$CLEAN_LEFTOVERS" = "1" ]; then
    if [ -n "$RUN_REGEX" ] || [ "${#SELECTORS[@]}" -gt 0 ]; then
        decline "--clean-leftovers removes registry rows and runs no realtest; asking for one as well is two different requests"
    fi
    command -v go >/dev/null 2>&1 || decline "go is not on PATH, and the leftover clean is a Go check in e2e/realtest"
    command -v sqlite3 >/dev/null 2>&1 || decline "sqlite3 is not on PATH; the clean cannot read which rows the state database holds"
    note "clearing workspace registry rows that name a directory under $REALTEST_ROOT"
    note "each is closed and then forgotten through the daemon's command-file ingress; a daemon must be serving"
    CLEAN_STATUS=0
    run_harness_check TestCleanRealtestLeftovers \
        AGENT_REPL_REALTEST_LEFTOVERS=clean \
        AGENT_REPL_REALTEST_LEFTOVER_PREFIX="$REALTEST_ROOT" || CLEAN_STATUS=$?
    if [ "$CLEAN_STATUS" -ne 0 ]; then
        printf '[realtest] rows are still standing (output above). If no daemon is serving, start Emacs and run this again.\n' >&2
        exit "$CLEAN_STATUS"
    fi
    note "the registry holds no row under $REALTEST_ROOT"
    exit 0
fi

PLAN=""
if [ -n "$RUN_REGEX" ]; then
    while IFS= read -r row; do
        if grep_in "$(row_field "$row" 2)" -Eq -- "$RUN_REGEX"; then
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

PLAN_COUNT="$(grep_in "$PLAN" -c '|')"

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
    printf '[realtest] run daemon/bin/claude-repld deploy, then try again.\n' >&2
    exit "$EXIT_DECLINED"
fi
note "every deployed system is at this checkout's revision"

# ---- refusal 4: a previous sweep's registry rows --------------------------
#
# Before the run directory is even created, so a decline here leaves nothing
# behind of its own. The rows it finds belong to an EARLIER sweep by
# construction: this one has registered nothing yet.
note "looking for workspace rows a previous sweep left in the registry"
LEFTOVER_STATUS=0
run_harness_check TestCleanRealtestLeftovers \
    AGENT_REPL_REALTEST_LEFTOVERS=report \
    AGENT_REPL_REALTEST_LEFTOVER_PREFIX="$REALTEST_ROOT" || LEFTOVER_STATUS=$?
if [ "$LEFTOVER_STATUS" -ne 0 ]; then
    printf '[realtest] DECLINED: a previous sweep left workspace rows in the owner'"'"'s registry (listed above).\n' >&2
    printf '[realtest] Each one makes the editor report a durable log sink it cannot host, naming a MISSING directory.\n' >&2
    printf '[realtest] Remedy: bin/realtest.sh --clean-leftovers\n' >&2
    exit "$EXIT_DECLINED"
fi
note "no workspace row from a previous sweep is standing under $REALTEST_ROOT"

# Record the stamps this run exercised. They are what a report says the run was
# MEASURING; without them a finding cannot be tied to a build.
RUN_STAMP="$(realtest_backup_stamp)"
RUN_DIR="${AGENT_REPL_REALTEST_OUT:-$REALTEST_ROOT/realtest-$RUN_STAMP}"
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

# ---- what a refusal is allowed to do: quit the editor, stop the daemon ----
#
# BOTH ARE DEFINED HERE, ahead of the vendor-guard refusal, because that
# refusal now RESOLVES itself under the consents rather than only naming what
# to stop by hand (owner complaint, 2026-09-13: every sweep ends by handing
# back a guard-free editor AND daemon, so every next sweep opened against an
# unguarded daemon and declined).

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

# RUN_QUIT_EDITOR — has THIS RUN quit an editor that was already standing?
#
# Separate from RUN_OWNS_EDITOR, which only says whose the next editor is. This
# one is the debt: a run that closed the owner's editor owes them one back at
# the end, and it owes it whether the run finished a sweep, failed a realtest
# or DECLINED in the preflight two lines after the quit (owner complaint,
# 2026-09-13: a sweep quit the editor, stopped the daemon, then declined over
# the shims and left the desktop with nothing on it).
RUN_QUIT_EDITOR=0

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
    RUN_QUIT_EDITOR=1
    return 0
}

REALTEST_MAIN_PID="${BASHPID:-$$}"

# launch_guard_free_editor — cold-start the editor the owner is owed, with the
# guard stripped from the environment by construction (`env -u`) rather than by
# trusting that no line above exported it. ONE SPELLING, because the two places
# that owe the owner an editor — the preflight handback below and the sweep's
# handback at the end — must not disagree about how it is launched.
# shellcheck disable=SC2329
# Invoked from restore_editor_this_run_quit and restore_owner_editor, both of
# which the EXIT traps reach indirectly.
launch_guard_free_editor() {
    if env -u "$VENDOR_GUARD_ENV" "$REALTEST_OPEN" -gj -a Emacs >/dev/null 2>&1; then
        return 0
    fi
    printf '[realtest] THE GUARD-FREE EMACS COULD NOT BE LAUNCHED (open -gj -a Emacs failed), so the owner has\n' >&2
    printf '[realtest] no editor standing. Start one when you next want it.\n' >&2
    return 1
}

# ---- the editor a preflight refusal owes back -----------------------------
#
# THE PREFLIGHT CAN QUIT THE EDITOR AND THEN DECLINE. The vendor-guard section
# below quits the standing Emacs before it stops an unguarded daemon, and every
# refusal after that point exits with the desktop already empty. The sweep's
# handback would cover it, but the sweep's EXIT trap is not installed yet and
# its rule is "nothing is answering, so there is nothing of this run's to hand
# back" — true of a run that never quit anything, false of this one.
#
# So this trap stands from here until `trap sweep_end EXIT` replaces it, and
# sweep_end calls the same body when it is reached before a sweep began.
# shellcheck disable=SC2329
# Invoked from the EXIT trap installed below it, and from sweep_end.
restore_editor_this_run_quit() {
    [ "${BASHPID:-$$}" = "$REALTEST_MAIN_PID" ] || return 0
    [ "$RUN_QUIT_EDITOR" = "1" ] || return 0
    if emacs_answering; then
        return 0
    fi
    note "this run quit the owner's editor and ends with nothing answering; a guard-free Emacs is launched in its place"
    if launch_guard_free_editor; then
        note "the owner's editor was restored: a guard-free Emacs launched"
    fi
    return 0
}
# shellcheck disable=SC2329
preflight_editor_handback() {
    local status=$?
    restore_editor_this_run_quit
    exit "$status"
}
trap preflight_editor_handback EXIT

# How long the daemon is given to exit after it has been asked to stop, before
# realtest 3 is skipped. A daemon stands its sessions down on the way out; this
# is a small multiple of that, not a guess at a machine's speed.
#
# AGENT_REPL_REALTEST_DAEMON_STOP_SECONDS shortens it for bin/test-realtest.sh,
# which asserts what happens when a daemon does NOT go and must not wait out a
# real stand-down to do it.
readonly DAEMON_STOP_SECONDS="${AGENT_REPL_REALTEST_DAEMON_STOP_SECONDS:-30}"

# The kill used for the FALLBACK stop only. A variable so bin/test-realtest.sh
# can watch what would be signalled without a process to signal, the same
# reason AGENT_REPL_REALTEST_EMACSCLIENT exists.
REALTEST_KILL="${AGENT_REPL_REALTEST_KILL:-/bin/kill}"


# stop_daemons_orderly PID... — stop the owner's daemon THROUGH ITS OWN DOOR,
# and wait for it to be gone. Prints the pids still standing afterwards (empty
# when it went). ONE SPELLING of the stop, because the three places that stop a
# daemon — this preflight's vendor-guard swap, realtest 3's world, and the
# sweep-end handback — must not disagree about how a daemon is asked to go or
# how long it is given.
#
# A BARE SIGTERM WAS THE WRONG DOOR, and the daemon said so four times in the
# harvest of 2026-09-13:
#
#   WARN daemon.rollout.reconcile "sessions survived a bounce that wrote no
#        intent manifest; each one is unaccounted for"
#   WARN daemon.rollout.reconcile "a session's bounce disposition needs a human"
#
# SIGTERM cancels the daemon's serving context and does nothing else: no
# session is stood down on the way out, so every shim it was holding survives
# it, and the next daemon adopts processes whose bounce nobody stated an intent
# for. `UpdateShutdownSchedule{now}` — what the editor's own
# `agent-repl-frontend-daemon-stop` sends — stops intake, stands
# every session down, flushes the in-flight writes and exits. Its successor
# then adopts nothing and logs an ordinary boot.
#
# NO EMACS IS INVOLVED, deliberately: two of the three call sites have just
# quit the owner's editor, so an editor-mediated stop is unavailable at exactly
# the moments the stop is needed. The rpc is spoken by
# `TestOrderlyDaemonStop` (e2e/realtest/daemonstop.go) with the generated
# client, run the way every other harness check here is run.
#
# THE FALLBACK IS SIGTERM, AND IT IS ALWAYS STATED. A daemon that does not
# answer its own door cannot be left standing — the run's whole point is a
# stack whose daemon carries the guard — so the signal is still sent, with the
# reason it came to that said out loud rather than discovered in a log.
#
# NO SIGKILL, anywhere. Escalating on the owner's daemon is not a decision this
# script makes; a daemon that ignored both is a finding in its own right.
stop_daemons_orderly() {
    if run_harness_check TestOrderlyDaemonStop \
        AGENT_REPL_REALTEST_DAEMON_STOP=1 >&2; then
        note_err "the daemon was stopped through its own door (UpdateShutdownSchedule{now}); no signal was sent, and it stands its sessions down on the way out"
    else
        note_err "the daemon did not accept an orderly stop (output above), so this falls back to SIGTERM"
        note_err "A DAEMON STOPPED BY SIGNAL STANDS NO SESSION DOWN: its shims survive it, and the next daemon reports each one as an unaccounted-for bounce (daemon.rollout.reconcile)."
        "$REALTEST_KILL" -TERM "$@" >/dev/null 2>&1 || true
    fi
    local _i
    for _i in $(seq 1 "$DAEMON_STOP_SECONDS"); do
        if [ -z "$(daemon_pids)" ]; then
            break
        fi
        sleep 1
    done
    local left
    left="$(daemon_pids | tr '\n' ' ')"
    printf '%s' "${left% }"
}

# ---- refusal 3, checked before 2: the vendor guard ------------------------
#
# Before the backups and before the takeover, because a stack that cannot hold
# the guard must not have the owner's editor quit for it.

# process_carries_guard PID — does the KERNEL's copy of this process's
# environment carry the guard? Not the launcher's intention, not this shell's
# exported environment: the copy `ps -Eww` prints beside the command line.
process_carries_guard() {
    grep_in "$(ps -Eww -o command= -p "$1" 2>/dev/null | tr ' ' '\n')" -q "^$VENDOR_GUARD_ENV="
}

# daemon_pids — every resident daemon of THIS checkout, one pid per line, and
# nothing on stdout when there is none. One spelling, because the vendor-guard
# refusal below and the world realtest 3 demands must not disagree about what
# counts as a running daemon.
daemon_pids() {
    pgrep -f "$MODULE_ROOT/daemon/bin/claude-repld" 2>/dev/null || true
}

# standing_emacs_pid — the pid of the editor answering the socket, and nothing
# on stdout when there is none or when it will not say. The pid is what makes
# the guard question answerable: `process_carries_guard` reads the KERNEL's
# copy of a process's environment, and a socket is not a process.
# shellcheck disable=SC2329
# Invoked from restore_owner_editor, which the EXIT trap reaches indirectly.
standing_emacs_pid() {
    local pid
    pid="$("$EMACSCLIENT" --socket-name "$EMACS_SOCKET" --eval '(emacs-pid)' 2>/dev/null | tr -d '"'"'"'[:space:]')"
    case "$pid" in
        ''|*[!0-9]*) return 0 ;;
    esac
    printf '%s' "$pid"
}

# THE UNGUARDED DAEMON IS THE NORMAL WAY A SWEEP STARTS, not an anomaly. The
# handback at the end of every sweep leaves the owner a guard-free editor and a
# guard-free daemon on purpose, so the next sweep opens against exactly the
# daemon this section refuses. Refusing it and telling the operator to kill it
# by hand made every sweep after the first one decline (owner complaint,
# 2026-09-13).
#
# So under AGENT_REPL_REALTEST_STOP_DAEMON=1 — the same consent realtest 3's
# world uses, and the same consent the handback uses — it is STOPPED, through
# the same orderly path: SIGTERM, and the one bound. Without that consent the
# refusal stands, and now names setting the variable as the remedy.
#
# THE EDITOR IS QUIT FIRST. A standing Emacs that finds its daemon gone brings
# one up again, so stopping the daemon out from under a live editor can hand
# this run a fresh unguarded daemon between the stop and the check. The quit
# costs a takeover, which is the consent the operator has already given for
# every cold start in the plan; without it this declines rather than stopping a
# daemon an editor would immediately replace.
DAEMON_PID="$(daemon_pids | sed -n 1p)"
if [ -n "$DAEMON_PID" ]; then
    note "a daemon is running as pid $DAEMON_PID; checking its environment for the vendor guard"
    if ! process_carries_guard "$DAEMON_PID"; then
        note "the running daemon (pid $DAEMON_PID) does not carry $VENDOR_GUARD_ENV"
        if [ "${AGENT_REPL_REALTEST_STOP_DAEMON:-}" != "1" ]; then
            printf '[realtest] DECLINED: the running daemon (pid %s) does not carry %s.\n' "$DAEMON_PID" "$VENDOR_GUARD_ENV" >&2
            printf '[realtest] Emacs ADOPTS an answering daemon and never kills one, so the new Emacs would inherit\n' >&2
            printf '[realtest] this one and it would spawn shims with the real SDK reachable.\n' >&2
            printf '[realtest] THE REMEDY IS AGENT_REPL_REALTEST_STOP_DAEMON=1: that consent lets this run stop the\n' >&2
            printf '[realtest] daemon itself, orderly, so the realtest'"'"'s Emacs spawns a guarded one. Stopping it by\n' >&2
            printf '[realtest] hand (SPC o C-d from the editor, or kill %s) works too.\n' "$DAEMON_PID" >&2
            exit "$EXIT_DECLINED"
        fi
        # THE EDITOR, FIRST. Ordered, not incidental: the quit has to complete
        # before the SIGTERM so the editor cannot respawn an unguarded daemon
        # in between.
        if emacs_answering; then
            note "quitting the standing Emacs BEFORE the daemon, so it cannot bring an unguarded daemon back up"
            if ! quit_standing_emacs; then
                printf '[realtest] DECLINED: the standing Emacs could not be quit (above), and stopping the daemon\n' >&2
                printf '[realtest] under a live editor would only have it brought back up unguarded.\n' >&2
                exit "$EXIT_DECLINED"
            fi
        fi
        UNGUARDED_DAEMONS=()
        while IFS= read -r pid; do
            [ -n "$pid" ] || continue
            UNGUARDED_DAEMONS+=("$pid")
        done < <(daemon_pids)
        if [ "${#UNGUARDED_DAEMONS[@]}" -gt 0 ]; then
            note "AGENT_REPL_REALTEST_STOP_DAEMON=1: stopping the unguarded daemon (pid(s) ${UNGUARDED_DAEMONS[*]}) through its own door"
            DAEMON_LEFT="$(stop_daemons_orderly "${UNGUARDED_DAEMONS[@]}")"
            if [ -n "$DAEMON_LEFT" ]; then
                printf '[realtest] DECLINED: the unguarded daemon (pid(s) %s) was still running %ss after it was asked to stop.\n' "$DAEMON_LEFT" "$DAEMON_STOP_SECONDS" >&2
                printf '[realtest] This run does not escalate to SIGKILL on the owner'"'"'s daemon, so nothing is run\n' >&2
                printf '[realtest] against a stack whose daemon could reach the real vendor. Stop it by hand.\n' >&2
                exit "$EXIT_DECLINED"
            fi
            note "the owner's unguarded daemon pid ${UNGUARDED_DAEMONS[*]} was stopped under AGENT_REPL_REALTEST_STOP_DAEMON so the realtest's Emacs spawns a guarded one"
        fi
    else
        note "the running daemon carries $VENDOR_GUARD_ENV"
    fi
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

SOCK_DIR="$OWNER_STATE_DIR/sock"

# scan_unguarded_shims — fill UNGUARDED (the lines a refusal prints),
# UNGUARDED_SHIM_PIDS and UNGUARDED_SHIM_SOCKETS from the process table. A
# function rather than a straight-line loop because the stop below has to ask
# the same question a second time, afterwards, and the two answers must come
# from one spelling.
UNGUARDED=""
UNGUARDED_SHIM_PIDS=()
UNGUARDED_SHIM_SOCKETS=()
scan_unguarded_shims() {
    UNGUARDED=""
    UNGUARDED_SHIM_PIDS=()
    UNGUARDED_SHIM_SOCKETS=()
    local pid command_line socket
    for pid in $(pgrep -f 'shim/dist/main\.js' 2>/dev/null || true); do
        command_line="$(ps -Eww -o command= -p "$pid" 2>/dev/null || true)"
        [ -n "$command_line" ] || continue
        # Only a shim listening under THIS state directory: another checkout's
        # shim is not a process this run would adopt, and declining on it would
        # send the operator after the wrong thing.
        case "$command_line" in
            *"--listen $SOCK_DIR/"*) ;;
            *) continue ;;
        esac
        socket="$(printf '%s' "$command_line" | tr ' ' '\n' | grep "^$SOCK_DIR/" | sed -n 1p)"
        if ! process_carries_guard "$pid"; then
            UNGUARDED="$UNGUARDED
  shim pid $pid listening on ${socket:-(no socket named on its command line)}"
            UNGUARDED_SHIM_PIDS+=("$pid")
            UNGUARDED_SHIM_SOCKETS+=("${socket:-}")
        fi
    done

    for pid in $(pgrep -f 'agent-repl/bin/shim-lock' 2>/dev/null || true); do
        ps -Eww -o command= -p "$pid" >/dev/null 2>&1 || continue
        if ! process_carries_guard "$pid"; then
            UNGUARDED="$UNGUARDED
  shim-lock pid $pid"
            UNGUARDED_SHIM_PIDS+=("$pid")
            UNGUARDED_SHIM_SOCKETS+=("")
        fi
    done
}

# shim_pid_alive PID — is this process still in the kernel's table? The stop
# below waits on this rather than on `kill -0`, so it reads the same table the
# guard question is answered from.
shim_pid_alive() {
    ps -Eww -o command= -p "$1" >/dev/null 2>&1
}

note "checking every listening shim under $SOCK_DIR for the vendor guard"
scan_unguarded_shims

# THE SHIMS OUTLIVE THE DAEMON THAT SPAWNED THEM, BY DESIGN. Stopping the
# unguarded daemon above does not take them with it: they keep listening on
# their workspace sockets, and the guarded daemon this run's Emacs brings up
# ADOPTS them rather than spawning fresh ones. So a sweep that had just stopped
# the daemon under consent declined here anyway, having already quit the
# owner's editor (owner complaint, 2026-09-13 15:2x).
#
# The consent that covers stopping the daemon covers these for the same reason
# and in the same breath: they ARE the daemon's live sessions, and the sentence
# AGENT_REPL_REALTEST_STOP_DAEMON=1 says is "end them". THE ORDER IS ORDERLY:
# the daemon is already gone by the time this runs, so nothing respawns a shim
# behind the stop, and each shim gets SIGTERM — never SIGKILL, the same rule
# the daemon stop holds — and the same bound to go in.
if [ -n "$UNGUARDED" ]; then
    if [ "${AGENT_REPL_REALTEST_STOP_DAEMON:-}" != "1" ]; then
        printf '[realtest] DECLINED: these shim processes do not carry %s:\n' "$VENDOR_GUARD_ENV" >&2
        printf '%s\n' "$UNGUARDED" >&2
        printf '[realtest] A daemon ADOPTS a shim that is already listening on a workspace'"'"'s socket rather than\n' >&2
        printf '[realtest] spawning a fresh one, so the guard this run puts on the daemon would never reach these.\n' >&2
        printf '[realtest] One of them kept a keepalive prompt going to the real vendor through realtest 1'"'"'s first\n' >&2
        printf '[realtest] run. They outlive the daemon that spawned them, so stopping the daemon does not take\n' >&2
        printf '[realtest] them with it.\n' >&2
        printf '[realtest] THE REMEDY IS AGENT_REPL_REALTEST_STOP_DAEMON=1: that consent lets this run stand these\n' >&2
        printf '[realtest] sessions down itself, orderly, after the daemon is gone. Stopping them by hand (kill the\n' >&2
        printf '[realtest] pids above) works too.\n' >&2
        exit "$EXIT_DECLINED"
    fi
    note "AGENT_REPL_REALTEST_STOP_DAEMON=1: standing down the unguarded shim processes the stopped daemon left listening (pid(s) ${UNGUARDED_SHIM_PIDS[*]}) with SIGTERM"
    "$REALTEST_KILL" -TERM "${UNGUARDED_SHIM_PIDS[@]}" >/dev/null 2>&1 || true
    for _i in $(seq 1 "$DAEMON_STOP_SECONDS"); do
        SHIMS_LEFT=""
        for _idx in "${!UNGUARDED_SHIM_PIDS[@]}"; do
            if shim_pid_alive "${UNGUARDED_SHIM_PIDS[$_idx]}"; then
                SHIMS_LEFT="$SHIMS_LEFT ${UNGUARDED_SHIM_PIDS[$_idx]}"
                continue
            fi
            # The socket has to go too: a shim that has exited but whose socket
            # file is still there is a socket the run's daemon would connect to
            # and find nothing behind, which is not a stack to measure a
            # startup against.
            if [ -n "${UNGUARDED_SHIM_SOCKETS[$_idx]}" ] && [ -e "${UNGUARDED_SHIM_SOCKETS[$_idx]}" ]; then
                SHIMS_LEFT="$SHIMS_LEFT ${UNGUARDED_SHIM_PIDS[$_idx]}"
            fi
        done
        [ -n "$SHIMS_LEFT" ] || break
        sleep 1
    done
    if [ -n "${SHIMS_LEFT# }" ]; then
        printf '[realtest] DECLINED: these shim processes were still there %ss after SIGTERM:%s\n' \
            "$DAEMON_STOP_SECONDS" "$SHIMS_LEFT" >&2
        printf '[realtest] This run does not escalate to SIGKILL on the owner'"'"'s processes, so nothing is run\n' >&2
        printf '[realtest] against a stack whose shims could reach the real vendor. Stop them by hand.\n' >&2
        exit "$EXIT_DECLINED"
    fi
    # EACH ONE IS STATED, not just the count: the operator is being told which
    # of their live sessions this run ended, and a summed number is not that.
    for _idx in "${!UNGUARDED_SHIM_PIDS[@]}"; do
        if [ -n "${UNGUARDED_SHIM_SOCKETS[$_idx]}" ]; then
            note "the owner's unguarded shim pid ${UNGUARDED_SHIM_PIDS[$_idx]} listening on ${UNGUARDED_SHIM_SOCKETS[$_idx]} was stopped under AGENT_REPL_REALTEST_STOP_DAEMON so the realtest's daemon spawns a guarded one"
        else
            note "the owner's unguarded shim-lock pid ${UNGUARDED_SHIM_PIDS[$_idx]} was stopped under AGENT_REPL_REALTEST_STOP_DAEMON so the realtest's daemon spawns a guarded one"
        fi
    done
    # ASKED AGAIN, FROM THE KERNEL. The stop is only worth what a fresh scan
    # says about it, and a shim that respawned behind the stop is exactly the
    # thing this refusal exists for.
    scan_unguarded_shims
    if [ -n "$UNGUARDED" ]; then
        printf '[realtest] DECLINED: unguarded shim processes are listening again after the stop:\n' >&2
        printf '%s\n' "$UNGUARDED" >&2
        printf '[realtest] Something is respawning them; this run does not fight it. Stop them by hand.\n' >&2
        exit "$EXIT_DECLINED"
    fi
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
WSM_DB="$OWNER_STATE_DIR/wsm.db"

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

# WHICH EDITOR THE OWNER GETS BACK, said before the run rather than discovered
# after it. Every editor a realtest launches carries the vendor guard, so the
# one left standing at the end is never the one the owner should keep; the
# handback in `sweep_end` quits it and cold-starts a normal Emacs in its place.
note "when this run ends the owner gets a GUARD-FREE editor back: a standing Emacs that carries $VENDOR_GUARD_ENV is quit and a normal one is launched with 'open -gj -a Emacs'"

# ---- realtest 3's world: the daemon stopped, under its own consent --------
#
# SEPARATE FROM THE TAKEOVER, because it costs something the takeover says
# nothing about: every live session the daemon holds. Without the consent this
# is a SKIP with the reason, never a run into realtest 3's own refusal.

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
    note "AGENT_REPL_REALTEST_STOP_DAEMON=1: stopping the daemon (pid(s) ${pids[*]}) through its own door"
    local left
    left="$(stop_daemons_orderly "${pids[@]}")"
    if [ -n "$left" ]; then
        DAEMON_SKIP_REASON="the daemon (pid(s) $left) was still running ${DAEMON_STOP_SECONDS}s after it was asked to stop (the door it was asked through is stated above). This run does not escalate to SIGKILL on the owner's daemon, so this realtest is skipped rather than run with a daemon up."
        return 1
    fi
    note "the daemon has stopped"
    return 0
}

# ---- the pre-sweep gap scan -----------------------------------------------
#
# THE HOURS BETWEEN SWEEPS ARE WHERE THE OWNER LIVES, and until now nothing
# read them. Each realtest harvests its own window, so a warning written while
# no realtest was running — a deploy restart, a boot catch-up, the owner's own
# use of the editor — was never read by anything, and every sweep went on
# reporting a clean harvest over it (owner complaint, 2026-09-13).
#
# So the sweep opens by reading from the previous sweep's end to now, across
# every source the in-window harvest knows plus the live editor's *Messages*
# and *Warnings* buffers. Findings go verbatim into the run's own
# `between-sweeps/MANIFEST.md` under "## Between sweeps" and make the sweep
# exit non-zero, exactly like an in-window finding. There is no allowlist.
#
# IT RUNS BEFORE ANY EDITOR IS QUIT. The buffers it reads belong to the editor
# the owner has been using, and the first cold-start realtest kills it.
GAP_SCAN_RAN=0
GAP_SCAN_STATUS=0
note "reading the gap since the previous sweep ended (nothing else covers it)"
run_harness_check TestBetweenSweepsGapScan \
    AGENT_REPL_REALTEST_GAP_SCAN=1 \
    AGENT_REPL_REALTEST_OUT="$RUN_DIR" \
    AGENT_REPL_REALTEST_EMACS_SOCKET="$EMACS_SOCKET" \
    AGENT_REPL_REALTEST_EMACSCLIENT="$EMACSCLIENT" || GAP_SCAN_STATUS=$?
GAP_SCAN_RAN=1
if [ "$GAP_SCAN_STATUS" -ne 0 ]; then
    printf '[realtest] BETWEEN-SWEEPS FINDINGS: the module wrote warnings or errors while no realtest was\n' >&2
    printf '[realtest] running (listed above, verbatim in %s/between-sweeps/MANIFEST.md). The sweep still runs;\n' "$RUN_DIR" >&2
    printf '[realtest] its exit status carries this.\n' >&2
else
    note "nothing was written between the sweeps that the harvest bar rejects"
fi

# ---- the sweep takes focus, ONCE ------------------------------------------
#
# OWNER RULING, 2026-09-13. Every press used to activate Emacs, post, and hand
# focus back, so a sweep flickered the owner's desktop once per keystroke and
# nothing on the screen said whether the run was still going. The sweep now
# STEALS FOCUS ONCE here and HANDS IT BACK ONCE from the EXIT trap below, so the
# owner can watch the run and the desktop coming back is how they know it ended.
#
# THIS IS NOT A REFUSAL POINT. A declined activation, a locked screen and an
# editor that does not exist yet are all readings the take reports and the sweep
# carries on from: a run that refused to collect its findings because the window
# server would not cooperate would throw away the whole point of the run. What
# DOES stop the take is this harness failing its own part — a helper that will
# not compile, a token that could not be written — and then
# AGENT_REPL_REALTEST_FOCUS_HELD stays unset, every press restores focus the old
# way, and no handback is owed.
#
# The editor a cold-start sweep will use does not exist yet, and that is the
# ordinary case: the take then records only where focus STARTED, and the first
# press against the new Emacs takes focus for it and says so in its receipt.
FOCUS_TAKEN=0
FOCUS_STATUS=0
note "taking focus for the whole sweep; it goes back to where it started when the sweep ends"
run_harness_check TestSweepFocusTake \
    AGENT_REPL_REALTEST_FOCUS=take \
    AGENT_REPL_REALTEST_OUT="$RUN_DIR" \
    AGENT_REPL_REALTEST_EMACS_SOCKET="$EMACS_SOCKET" \
    AGENT_REPL_REALTEST_EMACSCLIENT="$EMACSCLIENT" || FOCUS_STATUS=$?
if [ "$FOCUS_STATUS" -eq 0 ]; then
    FOCUS_TAKEN=1
else
    printf '[realtest] THE SWEEP COULD NOT TAKE FOCUS (output above), so every press hands focus back the\n' >&2
    printf '[realtest] old way, one keystroke at a time, and no handback is owed at the end. The run continues.\n' >&2
fi

# ---- the end of the sweep, however it ends --------------------------------
#
# A TRAP, because "the run left the owner's registry as it found it" must hold
# when a realtest FAILED, when one panicked, and when the operator interrupted
# the sweep — not only on the happy path. The alternative, a block after the
# loop, is exactly the code that does not run on the paths where the cleanup
# matters most.
# ---- the editor the owner gets back ---------------------------------------
#
# A RUN LEAVES THE OWNER'S STATE EXACTLY AS IT FOUND IT, and the editor was the
# one exception until the owner's live logs caught it (2026-09-13 14:19). Every
# Emacs a realtest launches carries AGENT_REPL_FORBID_VENDOR_CALLS, and the
# sweep left the last one standing, so from the end of a sweep the owner's
# day-to-day editor WAS the guarded one: the daemon it spawned, and every
# daemon a deploy restarted through it, inherited the guard, and the
# owner's real workspaces talked to the FAKE vendor —
# "shim.fake.query: fake vendor session STARTED" against a workspace the owner
# does real work in.
#
# So the editor is handed back the way focus is: whatever is standing at the
# end is READ FROM THE KERNEL, and a guarded editor is quit and replaced with a
# cold, guard-free one. A guard-free editor is left exactly alone — there is
# nothing to restore, and quitting the owner's own editor to launch an
# identical one would be a disturbance of its own.
#
# THE DAEMON IS PART OF THE HANDBACK, under the consent that already covers
# stopping it. A guard-free Emacs ADOPTS an answering daemon rather than
# spawning one, so a guarded daemon left running makes the new editor a fake
# vendor's editor exactly as before. Without AGENT_REPL_REALTEST_STOP_DAEMON=1
# it is left standing and that is said LOUDLY, because the whole defect this
# closes is a fake vendor nobody was told about.
#
# NONE OF THIS CHANGES THE SWEEP'S VERDICT, for the same reason the focus
# handback does not: it is the owner's desktop, not a finding about the module,
# and letting it overwrite a realtest's exit status would lose the finding the
# sweep exists for.
# shellcheck disable=SC2329
# Invoked indirectly, from the EXIT trap installed below.
restore_owner_editor() {
    local pid quit_pid="" launched=0
    local -a stopped=()
    local daemon_left=""

    if ! emacs_answering; then
        # NOTHING ANSWERING IS NOT ALWAYS NOTHING OWED. It means "no editor of
        # this run's to hand back" only when this run did not quit one; a run
        # that quit the owner's editor and then failed or declined owes them a
        # cold, guard-free one however it ended.
        if [ "$RUN_QUIT_EDITOR" = "1" ]; then
            restore_editor_this_run_quit
            return 0
        fi
        note "no Emacs is answering as this run ends; there is no editor of this run's to hand back"
        return 0
    fi
    pid="$(standing_emacs_pid)"
    if [ -z "$pid" ]; then
        printf '[realtest] THE STANDING EMACS WOULD NOT SAY ITS PID, so this run cannot tell whether the editor\n' >&2
        printf '[realtest] it leaves behind carries %s. Check it by hand: a guarded editor and every daemon\n' "$VENDOR_GUARD_ENV" >&2
        printf '[realtest] and shim under it answer from the FAKE vendor.\n' >&2
        return 0
    fi
    if ! process_carries_guard "$pid"; then
        note "the Emacs left standing (pid $pid) does not carry $VENDOR_GUARD_ENV; the owner keeps it, untouched"
        return 0
    fi

    note "the Emacs left standing (pid $pid) carries $VENDOR_GUARD_ENV; it is quit and a guard-free one takes its place"
    if ! quit_standing_emacs; then
        printf '[realtest] THE GUARDED EMACS (pid %s) IS STILL RUNNING (above), so the editor the owner is left\n' "$pid" >&2
        printf '[realtest] with forbids the vendor and everything it spawns answers from the FAKE vendor. Quit it\n' >&2
        printf '[realtest] by hand and start a normal one: open -gj -a Emacs\n' >&2
        return 0
    fi
    quit_pid="$pid"

    # THE DAEMON, SECOND AND BEFORE THE LAUNCH: a daemon stopped after the new
    # editor came up would already have been adopted by it.
    local dpid
    local -a guarded=()
    while IFS= read -r dpid; do
        [ -n "$dpid" ] || continue
        if process_carries_guard "$dpid"; then
            guarded+=("$dpid")
        fi
    done < <(daemon_pids)
    if [ "${#guarded[@]}" -gt 0 ]; then
        if [ "${AGENT_REPL_REALTEST_STOP_DAEMON:-}" != "1" ]; then
            daemon_left="${guarded[*]}"
            printf '[realtest] THE DAEMON LEFT RUNNING (pid(s) %s) CARRIES %s, and the guard-free Emacs this\n' "${guarded[*]}" "$VENDOR_GUARD_ENV" >&2
            printf '[realtest] run is about to launch will ADOPT it rather than spawn one, so the owner'"'"'s workspaces\n' >&2
            printf '[realtest] keep answering from the FAKE vendor. Stopping it ends every live session it holds,\n' >&2
            printf '[realtest] which this run was not given permission for: set AGENT_REPL_REALTEST_STOP_DAEMON=1\n' >&2
            printf '[realtest] to let a run stop it, or stop it by hand now (kill %s).\n' "${guarded[*]}" >&2
        else
            note "AGENT_REPL_REALTEST_STOP_DAEMON=1: stopping the guarded daemon (pid(s) ${guarded[*]}) through its own door so the new editor spawns a real one"
            local left
            left="$(stop_daemons_orderly "${guarded[@]}")"
            if [ -n "$left" ]; then
                daemon_left="$left"
                printf '[realtest] THE GUARDED DAEMON (pid(s) %s) IS STILL RUNNING %ss after it was asked to stop. This run does\n' "$left" "$DAEMON_STOP_SECONDS" >&2
                printf '[realtest] not escalate to SIGKILL on the owner'"'"'s daemon, so the editor it launches will adopt\n' >&2
                printf '[realtest] a FAKE-vendor daemon. Stop it by hand.\n' >&2
            else
                stopped=("${guarded[@]}")
            fi
        fi
    fi

    if launch_guard_free_editor; then
        launched=1
    fi

    local summary="the owner's editor was restored: guarded Emacs pid $quit_pid quit"
    if [ "${#stopped[@]}" -gt 0 ]; then
        summary="$summary, guarded daemon pid ${stopped[*]} stopped"
    elif [ -n "$daemon_left" ]; then
        summary="$summary, guarded daemon pid $daemon_left LEFT RUNNING (stated above)"
    else
        summary="$summary, no guarded daemon was running"
    fi
    if [ "$launched" = "1" ]; then
        summary="$summary, a guard-free Emacs launched"
    else
        summary="$summary, NO editor could be launched (stated above)"
    fi
    note "$summary"
    return 0
}

SWEEP_STARTED=0
SWEEP_ENDED=0

# shellcheck disable=SC2329
# Invoked indirectly, by the EXIT trap installed below it.
sweep_end() {
    local status=$?
    # Bash runs an EXIT trap in some subshells; a cleanup that ran there would
    # clean the registry from inside a command substitution and report into a
    # pipe nobody reads.
    [ "${BASHPID:-$$}" = "$REALTEST_MAIN_PID" ] || return 0
    # A run that ended before the sweep began still owes the owner the editor
    # it quit in the preflight; the trap this one replaced is what would
    # otherwise have paid it.
    if [ "$SWEEP_STARTED" != "1" ]; then
        restore_editor_this_run_quit
        return 0
    fi
    [ "$SWEEP_ENDED" = "0" ] || return 0
    SWEEP_ENDED=1
    trap - EXIT

    printf '\n[realtest] leaving the owner'"'"'s state as this run found it\n'
    local leftover_status=0
    run_harness_check TestCleanRealtestLeftovers \
        AGENT_REPL_REALTEST_LEFTOVERS=clean \
        AGENT_REPL_REALTEST_LEFTOVER_PREFIX="$RUN_DIR" || leftover_status=$?
    if [ "$leftover_status" -ne 0 ]; then
        printf '[realtest] REALTEST LEFTOVER WORKSPACES: rows this run created are still in the registry\n' >&2
        printf '[realtest] (listed above). Each one makes the editor report a durable log sink it cannot host,\n' >&2
        printf '[realtest] naming a MISSING directory, until it is removed.\n' >&2
        printf '[realtest] Remedy: bin/realtest.sh --clean-leftovers\n' >&2
        [ "$status" -eq 0 ] && status="$leftover_status"
    fi

    # THE MARK IS ONLY MOVED BY A SWEEP THAT READ THE GAP. Advancing it after a
    # run that never scanned would discard the window nobody looked at, which
    # is the whole defect the scan exists for.
    if [ "$GAP_SCAN_RAN" = "1" ]; then
        local mark_status=0
        run_harness_check TestBetweenSweepsMarkTheSweepEnd \
            AGENT_REPL_REALTEST_SWEEP_MARK=1 \
            AGENT_REPL_REALTEST_OUT="$RUN_DIR" \
            AGENT_REPL_REALTEST_EMACS_SOCKET="$EMACS_SOCKET" \
            AGENT_REPL_REALTEST_EMACSCLIENT="$EMACSCLIENT" || mark_status=$?
        if [ "$mark_status" -ne 0 ]; then
            printf '[realtest] the sweep mark could not be written; the next sweep falls back to the newest\n' >&2
            printf '[realtest] MANIFEST.md and reads a wider window rather than a wrong one.\n' >&2
        fi
    fi

    if [ "$GAP_SCAN_STATUS" -ne 0 ] && [ "$status" -eq 0 ]; then
        printf '[realtest] the sweep exits non-zero for its between-sweeps findings alone.\n' >&2
        status="$GAP_SCAN_STATUS"
    fi

    # THE HANDBACK IS LAST, AND IT IS INSIDE THE TRAP FOR THAT REASON. A
    # realtest that failed, a `go test` that panicked and an operator's
    # interrupt all reach it, so the owner's desktop comes back however the
    # sweep ends — which is also how they can tell it HAS ended. It runs after
    # the leftover clean and the mark so nothing that still needs the editor is
    # done behind a desktop this has already handed away.
    #
    # A handback that could not land does NOT change the sweep's verdict. It is
    # a disturbed desktop, printed here, not a finding about the module — and
    # letting it overwrite a realtest's exit status would lose the finding the
    # sweep exists for.
    if [ "$FOCUS_TAKEN" = "1" ]; then
        local focus_back=0
        run_harness_check TestSweepFocusGiveBack \
            AGENT_REPL_REALTEST_FOCUS=give-back \
            AGENT_REPL_REALTEST_OUT="$RUN_DIR" || focus_back=$?
        if [ "$focus_back" -ne 0 ]; then
            printf '[realtest] FOCUS WAS NOT HANDED BACK (output above): the owner'"'"'s desktop is not as this\n' >&2
            printf '[realtest] run found it. This does not change the sweep'"'"'s verdict.\n' >&2
        fi
    fi

    # THE EDITOR HANDBACK IS AFTER THE FOCUS HANDBACK, and last of all. Every
    # step above still needs the editor the run has been driving, and the
    # replacement is launched with `-gj` so it does not take the desktop back
    # off the application the focus handback just returned it to.
    restore_owner_editor
    exit "$status"
}
trap sweep_end EXIT

# ---- the run --------------------------------------------------------------
#
# THE SUITE SLOT. Every suite here is sized to fill the machine, and a realtest
# is worse than most: it measures a startup, so another suite's load turns a
# healthy phase into a reported breach. bin/suite-slot.sh nests, so wrapping
# here is safe even under an outer holder.

note "the vendor is forbidden for this run: $VENDOR_GUARD_ENV=1 on Emacs and everything it spawns"
note "no picture is taken; Emacs is brought frontmost ONCE for the whole sweep and focus goes back at the end"
note "this run holds $PLAN_COUNT realtest(s), each in its own go test invocation"

# run_one NAME — one realtest, in its own `go test`, under its own suite slot.
# Prints nothing itself; the status is the realtest's.
run_one() {
    local name="$1"
    local held=""
    # ONLY A SWEEP THAT ACTUALLY TOOK FOCUS TELLS THE PRESSES TO HOLD IT. A
    # press that kept focus with nobody holding the handback would leave the
    # owner's desktop parked on Emacs after the run.
    # `if`, not `&&`: this runs before `set +e` below, and an AND-list whose
    # left side is false is a failing command that `set -e` would exit the
    # whole sweep on.
    if [ "$FOCUS_TAKEN" = "1" ]; then held=1; fi
    set +e
    AGENT_REPL_REALTEST=1 \
    AGENT_REPL_REALTEST_FOCUS_HELD="$held" \
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
# From here on the sweep owns the registry rows its realtests create, and
# `sweep_end` runs however this ends.
SWEEP_STARTED=1

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
printf '[realtest] which editor the owner is left with is settled at the end of this run, below.\n'

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
