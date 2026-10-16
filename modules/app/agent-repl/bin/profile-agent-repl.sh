#!/usr/bin/env bash
# profile-agent-repl.sh -- system-wide runtime profiling of a live agent-repl.
#
# Usage:
#   bin/profile-agent-repl.sh start [--interval SECONDS] [--log-level LEVEL]
#   bin/profile-agent-repl.sh stop
#   bin/profile-agent-repl.sh status
#   bin/profile-agent-repl.sh report [SESSION] [--startup RFC3339] [--level LEVEL]
#   bin/profile-agent-repl.sh switch-bench [--slow N:SECONDS] [--fast N:SECONDS]
#
# start opens a session under the profiles root (AGENT_REPL_PROFILE_ROOT, else
# ~/.cache/agent-repl/profiles): it names the session in <root>/active, sends
# SIGWINCH to every runtime holding an armed marker (daemon, store, sidecar,
# every shim), asks Emacs through emacsclient, and starts a sampler that records
# CPU and RSS of every agent-repl process (and the WebKit processes) into
# <session>/samples.jsonl. stop closes the session, which makes every runtime
# write its CPU and heap profiles into the session directory, and stops the
# sampler. report summarizes a session (the newest by default): per-process
# CPU and memory, the hottest frames per runtime, and the user-visible
# latencies from the session's log edges, ranked against their budgets. It
# reads the session's logs from --level up (info by default) into
# <session>/logs.jsonl; --startup adds the startup assessment from the
# editor's launch instant.
#
# --log-level (debug, warn or error) raises every runtime -- daemon, store,
# sidecar, every shim, Emacs, the webhost and every page -- to that level for
# the session, as a log level window of at most five minutes that the stop
# closes (proto/vocab/log-level-window.json); the window ends by itself either
# way.
#
# switch-bench measures workspace switching end to end, in one run: it opens a
# session at debug, drives a SLOW switch tour (default 8 switches each way,
# 1.5s apart) and then a FAST one (default 40 each way, 0.25s apart) through
# Emacs's own key loop (agent-repl-profiling-switch-tour), closes the session,
# and writes the report, read from the session's debug-level logs, with a
# section per tour, into the session's summary.md beside those logs
# (logs.jsonl).
# It takes over the live editor while it runs, so it is run with the owner's
# agreement. A rollout during the run (the daemon or Emacs replaced) makes the
# run invalid: it says so and exits non-zero. Exit 0 is a complete run.
#
# The contract is proto/vocab/profile-session.json. Every runtime stops by
# itself after 30 minutes, so a forgotten session cannot run on, but stop it
# when you are done: a profile costs CPU on a battery.
#
# Environment: AGENT_REPL_PROFILE_ROOT (root), AGENT_REPL_EMACS_SOCKET (an
# emacsclient -s socket; default is Emacs's own default server),
# AGENT_REPL_EMACSCLIENT (the emacsclient to use; default the one on PATH, else
# the one inside /Applications/Emacs.app), AGENT_REPL_PROFILE_LOGS_SH (the log
# reader; default bin/logs.sh, a seam for the harness),
# AGENT_REPL_PROFILE_TOUR_MARGIN (seconds a tour may overrun twice its planned
# length before it is cancelled; default 30) and AGENT_REPL_PROFILE_SETTLE
# (seconds a bench waits after its last tour before closing; default 3), both
# seams for the harness.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
ROOT="${AGENT_REPL_PROFILE_ROOT:-$HOME/.cache/agent-repl/profiles}"
ACTIVE="$ROOT/active"
ARMED="$ROOT/armed"

die() {
    printf 'profile-agent-repl: %s\n' "$*" >&2
    exit 1
}

usage() {
    sed -n '3,10p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

# expected_comm RUNTIME -- the process name a runtime's marker must belong to.
expected_comm() {
    case $1 in
        daemon) printf 'agent-repld' ;;
        store) printf 'shim-store' ;;
        sidecar) printf 'shim-claude-sidecar' ;;
        shim) printf 'node' ;;
        *) return 1 ;;
    esac
}

# marker_state MARKER -- what MARKER's process is now: "live", "gone" (no
# process has its pid), "recycled COMM" (its pid now names another program,
# so its own process is gone too), or "unknown" (a runtime this collector
# does not know). Every reader of the armed markers goes through it.
marker_state() {
    local name="${1##*/}" want comm
    want="$(expected_comm "${name%-*}")" || { printf 'unknown'; return 0; }
    comm="$(ps -o comm= -p "${name##*-}" 2>/dev/null || true)"
    if [ -z "$comm" ]; then
        printf 'gone'
    elif [ "${comm##*/}" != "$want" ]; then
        printf 'recycled %s' "${comm##*/}"
    else
        printf 'live'
    fi
}

# signal_armed -- send SIGWINCH to every live runtime holding a marker, and
# remove the markers whose process is gone or whose pid another program now
# has. Prints one line per marker.
signal_armed() {
    [ -d "$ARMED" ] || return 0
    local marker name pid state
    for marker in "$ARMED"/*; do
        [ -e "$marker" ] || continue
        name="${marker##*/}"
        pid="${name##*-}"
        state="$(marker_state "$marker")"
        case $state in
            unknown)
                printf 'skipped %s: unknown runtime\n' "$name"
                continue ;;
            gone)
                rm -f "$marker"
                printf 'removed %s: the process is gone\n' "$name"
                continue ;;
            recycled\ *)
                rm -f "$marker"
                printf 'removed %s: the process is gone; pid %s is now %s\n' "$name" "$pid" "${state#recycled }"
                continue ;;
        esac
        if env kill -WINCH "$pid"; then
            printf 'signaled %s\n' "$name"
        else
            printf 'failed %s: kill -WINCH %s\n' "$name" "$pid"
        fi
    done
}

# emacsclient_path -- the emacsclient to use, or nothing when there is none.
emacsclient_path() {
    if [ -n "${AGENT_REPL_EMACSCLIENT:-}" ]; then
        printf '%s' "$AGENT_REPL_EMACSCLIENT"
    elif command -v emacsclient >/dev/null 2>&1; then
        command -v emacsclient
    elif [ -x /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ]; then
        printf '/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    fi
}

# eval_emacs FORM -- evaluate FORM in the running Emacs and print its value.
eval_emacs() {
    local client args=()
    client="$(emacsclient_path)"
    [ -n "$client" ] || {
        printf 'no emacsclient: not on PATH, not in /Applications/Emacs.app, and AGENT_REPL_EMACSCLIENT unset\n' >&2
        return 1
    }
    [ -z "${AGENT_REPL_EMACS_SOCKET:-}" ] || args=(-s "$AGENT_REPL_EMACS_SOCKET")
    "$client" "${args[@]}" --eval "$1"
}

# call_emacs -- ask Emacs to reconcile; prints its answer or why it could not.
call_emacs() {
    local answer
    if answer="$(eval_emacs '(if (fboundp (quote agent-repl-profiling-reconcile)) (agent-repl-profiling-reconcile) "failed: this Emacs has no agent-repl profiling")' 2>&1)"; then
        printf 'emacs: %s\n' "$answer"
    else
        printf 'emacs: unreachable: %s\n' "$answer"
    fi
}

# sampler SESSION_DIR INTERVAL -- append one JSON line per agent-repl process
# per tick until killed.
sampler() {
    local dir="$1" interval="$2"
    while :; do
        # THE EXECUTABLE IS ARGS' FIRST WORD: macOS cuts the comm column to 16
        # characters when columns follow it, so a daemon, Emacs or WebKit
        # process read by comm matched nothing, and only the shims (whose comm
        # is a short "node") were ever sampled (2026-10-16).
        ps -A -o pid=,rss=,time=,args= | awk -v t="$(date +%s)" '
            function seconds(s,   n, p) {
                n = split(s, p, /[:-]/)
                if (n == 2) return p[1] * 60 + p[2]
                if (n == 3) return p[1] * 3600 + p[2] * 60 + p[3]
                return p[1] * 86400 + p[2] * 3600 + p[3] * 60 + p[4]
            }
            {
                comm = $4; sub(/.*\//, "", comm)
                args = ""; for (i = 4; i <= NF; i++) args = args " " $i
                name = ""
                if (comm == "agent-repld") name = "daemon"
                else if (comm == "shim-store") name = "store"
                else if (comm == "shim-claude-sidecar") name = "sidecar"
                else if (comm == "node" && args ~ /main\.js --listen/) name = "shim"
                else if (comm == "Emacs" || comm == "Emacs-arm64-11") name = "emacs"
                else if (comm ~ /^com\.apple\.WebKit\./) name = "webkit:" substr(comm, length("com.apple.WebKit.") + 1)
                if (name != "") printf "{\"t\":%d,\"pid\":%d,\"cpu_s\":%.2f,\"rss_kb\":%d,\"name\":\"%s\"}\n", t, $1, seconds($3), $2, name
            }' >>"$dir/samples.jsonl"
        sleep "$interval"
    done
}

# newest_session -- the most recent session directory's id.
newest_session() {
    local d newest=""
    for d in "$ROOT"/*/; do
        d="${d%/}"
        [ -d "$d" ] && [ "${d##*/}" != armed ] || continue
        if [ -z "$newest" ] || [ "$d" -nt "$newest" ]; then
            newest="$d"
        fi
    done
    [ -n "$newest" ] || die "no session under $ROOT"
    printf '%s' "${newest##*/}"
}

# report_helper -- build the report helper once per source fingerprint.
report_helper() {
    local source="$THIS_DIR/profile-report.go" fingerprint checksum remainder size build_root binary tmp
    command -v go >/dev/null 2>&1 || die 'go is required to build the report helper'
    fingerprint="$(cksum "$source")" || die "could not fingerprint $source"
    checksum="${fingerprint%% *}"
    remainder="${fingerprint#* }"
    size="${remainder%% *}"
    build_root="${AGENT_REPL_PROFILE_BUILD_DIR:-${TMPDIR:-/tmp}/agent-repl-profile-$(id -u)}"
    binary="$build_root/profile-report-$checksum-$size"
    if [ ! -x "$binary" ]; then
        mkdir -p "$build_root" || die "could not create $build_root"
        tmp="$binary.tmp.$$"
        go build -trimpath -o "$tmp" "$source" || {
            rm -f "$tmp"
            die 'could not build the report helper'
        }
        mv -f "$tmp" "$binary" || die "could not install $binary"
    fi
    printf '%s' "$binary"
}

cmd_start() {
    local interval=2 level=""
    while [ $# -gt 0 ]; do
        case $1 in
            --interval) interval="${2:?--interval needs seconds}"; shift 2 ;;
            --log-level) level="${2:?--log-level needs a level}"; shift 2 ;;
            *) die "start: unknown argument $1" ;;
        esac
    done
    [[ $interval =~ ^[0-9]+$ ]] && [ "$interval" -ge 1 ] || die "start: --interval must be a whole number of seconds"
    case $level in
        "" | debug | warn | error) ;;
        *) die "start: --log-level must be debug, warn or error, not $level" ;;
    esac
    [ ! -e "$ACTIVE" ] || die "a session is already open: $(cat "$ACTIVE"); stop it first"
    local id dir
    id="$(date -u +%Y%m%dT%H%M%SZ)"
    dir="$ROOT/$id"
    mkdir -p "$dir" || die "could not create $dir"
    printf '{"started_at":"%s","interval_s":%d}\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$interval" >"$dir/meta.json"
    # THE LEVEL IS WRITTEN BEFORE THE SESSION OPENS, so every runtime that
    # reconciles into it reads it (proto/vocab/profile-session.json).
    [ -z "$level" ] || printf '%s\n' "$level" >"$dir/log-level"
    printf '%s\n' "$id" >"$ACTIVE"
    { signal_armed; call_emacs; } | tee -a "$dir/control.log"
    sampler "$dir" "$interval" </dev/null >/dev/null 2>"$dir/sampler.err" &
    printf '%s\n' "$!" >"$dir/sampler.pid"
    disown
    printf 'session %s open in %s\n' "$id" "$dir"
}

cmd_stop() {
    [ -e "$ACTIVE" ] || die "no session is open"
    local id dir pid
    id="$(cat "$ACTIVE")"
    dir="$ROOT/$id"
    rm -f "$ACTIVE"
    { signal_armed; call_emacs; } | tee -a "$dir/control.log"
    if [ -f "$dir/sampler.pid" ]; then
        pid="$(cat "$dir/sampler.pid")"
        kill "$pid" 2>/dev/null || printf 'the sampler (pid %s) had already stopped\n' "$pid"
        rm -f "$dir/sampler.pid"
    fi
    printf '{"stopped_at":"%s"}\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >"$dir/stopped.json"
    printf 'session %s closed; profiles in %s\n' "$id" "$dir"
}

cmd_status() {
    if [ -e "$ACTIVE" ]; then
        printf 'open session: %s\n' "$(cat "$ACTIVE")"
    else
        printf 'no session is open\n'
    fi
    local marker name state
    for marker in "$ARMED"/*; do
        [ -e "$marker" ] || continue
        name="${marker##*/}"
        state="$(marker_state "$marker")"
        case $state in
            live) printf 'armed: %s (%s)\n' "$name" "$(expected_comm "${name%-*}")" ;;
            gone) printf 'stale: %s (the process is gone)\n' "$name" ;;
            recycled\ *) printf 'stale: %s (the process is gone; its pid is now %s)\n' "$name" "${state#recycled }" ;;
            *) printf 'unknown: %s\n' "$name" ;;
        esac
    done
}

cmd_report() {
    local id="" startup="" level=info
    while [ $# -gt 0 ]; do
        case $1 in
            --startup) startup="${2:?--startup needs an RFC3339 instant}"; shift 2 ;;
            --level)
                level="${2:?--level needs debug, info, warn or error}"; shift 2
                case $level in debug|info|warn|error) ;; *) die "report: --level must be debug, info, warn or error, not $level" ;; esac ;;
            -*) die "report: unknown argument $1" ;;
            *) id="$1"; shift ;;
        esac
    done
    [ -n "$id" ] || id="$(newest_session)"
    local dir="$ROOT/$id" helper since until logs
    [ -d "$dir" ] || die "no session $id under $ROOT"
    helper="$(report_helper)"
    since="$(sed -n 's/.*"started_at":"\([^"]*\)".*/\1/p' "$dir/meta.json")"
    [ -n "$since" ] || die "session $id has no start instant"
    [ -z "$startup" ] || since="$startup"
    until=""
    [ ! -f "$dir/stopped.json" ] || until="$(sed -n 's/.*"stopped_at":"\([^"]*\)".*/\1/p' "$dir/stopped.json")"
    logs="$dir/logs.jsonl"
    local args=(--all --json --level "$level" --since "$since")
    [ -z "$until" ] || args+=(--until "$until")
    "${AGENT_REPL_PROFILE_LOGS_SH:-$THIS_DIR/logs.sh}" "${args[@]}" >"$logs" 2>"$dir/logs.err" || die "bin/logs.sh failed; see $dir/logs.err"
    local report_args=(--session "$dir" --logs "$logs")
    [ -z "$startup" ] || report_args+=(--startup "$startup")
    "$helper" "${report_args[@]}" | tee "$dir/summary.md"
}

# parse_leg SPEC -- "N:SECONDS" as "N SECONDS", or a refusal.
parse_leg() {
    local n="${1%%:*}" seconds="${1#*:}"
    [[ $1 == *:* && $n =~ ^[0-9]+$ && $n -ge 1 && $seconds =~ ^[0-9]+(\.[0-9]+)?$ ]] ||
        die "switch-bench: a tour is N:SECONDS (switches each way, seconds between keys), not $1"
    awk -v s="$seconds" 'BEGIN { exit !(s > 0) }' || die "switch-bench: a tour's pace must be more than no seconds, not $1"
    printf '%s %s' "$n" "$seconds"
}

# runtime_pids -- "daemon=PID[,PID] emacs=PID", the processes a rollout
# replaces: every daemon whose marker's process is live (marker_state), and
# Emacs.
runtime_pids() {
    local daemons="" marker
    for marker in "$ARMED"/daemon-*; do
        [ -e "$marker" ] && [ "$(marker_state "$marker")" = live ] && daemons="$daemons${daemons:+,}${marker##*-}"
    done
    printf 'daemon=%s emacs=%s' "${daemons:-none}" "$(eval_emacs '(emacs-pid)' 2>/dev/null || printf none)"
}

# run_tour LABEL N SECONDS -- drive one switch tour in Emacs and wait for it to
# end, by its own state, within twice its planned length plus a margin.
run_tour() {
    local label="$1" n="$2" seconds="$3" answer deadline state
    answer="$(eval_emacs "(agent-repl-profiling-switch-tour $n $seconds \"$label\")" 2>&1)" ||
        { printf 'tour %s: Emacs refused it: %s\n' "$label" "$answer"; return 1; }
    printf 'tour %s: %s\n' "$label" "$answer"
    deadline=$(($(date +%s) + $(awk -v n="$n" -v s="$seconds" -v m="${AGENT_REPL_PROFILE_TOUR_MARGIN:-30}" 'BEGIN { printf "%d", 2 * (2 * n * s) + m }')))
    while :; do
        state="$(eval_emacs '(agent-repl-profiling-switch-tour-state)' 2>&1)" ||
            { printf 'tour %s: Emacs stopped answering: %s\n' "$label" "$state"; return 1; }
        [ "$state" != '"idle"' ] || break
        if [ "$(date +%s)" -ge "$deadline" ]; then
            eval_emacs '(agent-repl-profiling-switch-tour-cancel)' >/dev/null 2>&1 || true
            printf 'tour %s: still %s past its deadline; cancelled\n' "$label" "$state"
            return 1
        fi
        sleep 0.5
    done
    printf 'tour %s: done\n' "$label"
}

cmd_switch_bench() {
    local slow="8:1.5" fast="40:0.25"
    while [ $# -gt 0 ]; do
        case $1 in
            --slow) slow="${2:?--slow needs N:SECONDS}"; shift 2 ;;
            --fast) fast="${2:?--fast needs N:SECONDS}"; shift 2 ;;
            *) die "switch-bench: unknown argument $1" ;;
        esac
    done
    local slow_n slow_s fast_n fast_s planned
    read -r slow_n slow_s <<<"$(parse_leg "$slow")"
    read -r fast_n fast_s <<<"$(parse_leg "$fast")"
    # THE WHOLE RUN FITS ITS LOG LEVEL WINDOW, which ends at five minutes.
    planned="$(awk -v a="$slow_n" -v b="$slow_s" -v c="$fast_n" -v d="$fast_s" 'BEGIN { printf "%d", 2 * a * b + 2 * c * d + 10 }')"
    [ "$planned" -le 240 ] || die "switch-bench: the tours plan ${planned}s; they must fit in 240s, inside the 300s log level window"
    [ ! -e "$ACTIVE" ] || die "a session is already open: $(cat "$ACTIVE"); stop it first"
    local ready
    ready="$(eval_emacs '(fboundp (quote agent-repl-profiling-switch-tour-state))' 2>&1)" ||
        die "switch-bench: Emacs is unreachable: $ready"
    [ "$ready" = "t" ] || die "switch-bench: this Emacs has no switch tour (agent-repl-profiling-switch-tour-state); load lisp/profiling.el"
    local before after id dir ok=1
    cmd_start --log-level debug
    # READ AFTER THE START, whose sweep removed every marker of a process that
    # is gone, and of live markers only: a dead daemon's marker is no runtime.
    before="$(runtime_pids)"
    id="$(cat "$ACTIVE")"
    dir="$ROOT/$id"
    printf '{"slow":"%s","fast":"%s","before":"%s"}\n' "$slow" "$fast" "$before" >"$dir/bench.json"
    if ! run_tour slow "$slow_n" "$slow_s" | tee -a "$dir/control.log"; then ok=0; fi
    if [ "$ok" -eq 1 ] && ! run_tour fast "$fast_n" "$fast_s" | tee -a "$dir/control.log"; then ok=0; fi
    # LATE FRAMES AND RECORDS LAND before the session closes on them.
    sleep "${AGENT_REPL_PROFILE_SETTLE:-3}"
    cmd_stop
    after="$(runtime_pids)"
    if [ "$before" != "$after" ]; then
        printf 'switch-bench: INVALID RUN: a rollout replaced a runtime during it (before %s, after %s)\n' "$before" "$after" | tee -a "$dir/control.log" >&2
        ok=0
    fi
    cmd_report "$id" --level debug >/dev/null
    printf 'switch-bench: report %s/summary.md, debug logs %s/logs.jsonl\n' "$dir" "$dir"
    [ "$ok" -eq 1 ] || die "switch-bench: the run did not complete; see $dir/control.log"
}

case "${1:-}" in
    start) shift; cmd_start "$@" ;;
    stop) shift; cmd_stop "$@" ;;
    status) shift; cmd_status "$@" ;;
    report) shift; cmd_report "$@" ;;
    switch-bench) shift; cmd_switch_bench "$@" ;;
    -h|--help) usage ;;
    *) usage >&2; exit 2 ;;
esac
