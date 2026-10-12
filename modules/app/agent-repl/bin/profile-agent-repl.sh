#!/usr/bin/env bash
# profile-agent-repl.sh -- system-wide runtime profiling of a live agent-repl.
#
# Usage:
#   bin/profile-agent-repl.sh start [--interval SECONDS]
#   bin/profile-agent-repl.sh stop
#   bin/profile-agent-repl.sh status
#   bin/profile-agent-repl.sh report [SESSION] [--startup RFC3339]
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
# latencies from the session's INFO log edges, ranked against their budgets.
# --startup adds the startup assessment from the editor's launch instant.
#
# The contract is proto/vocab/profile-session.json. Every runtime stops by
# itself after 30 minutes, so a forgotten session cannot run on, but stop it
# when you are done: a profile costs CPU on a battery.
#
# Environment: AGENT_REPL_PROFILE_ROOT (root), AGENT_REPL_EMACS_SOCKET (an
# emacsclient -s socket; default is Emacs's own default server),
# AGENT_REPL_PROFILE_LOGS_SH (the log reader; default bin/logs.sh, a seam for
# the harness).

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
    sed -n '3,9p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
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

# signal_armed -- send SIGWINCH to every live runtime holding a marker, and
# remove the markers of processes that are gone. Prints one line per marker.
signal_armed() {
    [ -d "$ARMED" ] || return 0
    local marker name runtime pid want comm
    for marker in "$ARMED"/*; do
        [ -e "$marker" ] || continue
        name="${marker##*/}"
        runtime="${name%-*}"
        pid="${name##*-}"
        want="$(expected_comm "$runtime")" || {
            printf 'skipped %s: unknown runtime\n' "$name"
            continue
        }
        comm="$(ps -o comm= -p "$pid" 2>/dev/null || true)"
        if [ -z "$comm" ]; then
            rm -f "$marker"
            printf 'removed %s: the process is gone\n' "$name"
            continue
        fi
        if [ "${comm##*/}" != "$want" ]; then
            printf 'skipped %s: pid %s is %s, not %s\n' "$name" "$pid" "${comm##*/}" "$want"
            continue
        fi
        if env kill -WINCH "$pid"; then
            printf 'signaled %s\n' "$name"
        else
            printf 'failed %s: kill -WINCH %s\n' "$name" "$pid"
        fi
    done
}

# call_emacs -- ask Emacs to reconcile; prints its answer or why it could not.
call_emacs() {
    local args=()
    [ -z "${AGENT_REPL_EMACS_SOCKET:-}" ] || args=(-s "$AGENT_REPL_EMACS_SOCKET")
    if ! command -v emacsclient >/dev/null 2>&1; then
        printf 'emacs: emacsclient is not on PATH\n'
        return 0
    fi
    local answer
    if answer="$(emacsclient "${args[@]}" --eval '(if (fboundp (quote agent-repl-profiling-reconcile)) (agent-repl-profiling-reconcile) "failed: this Emacs has no agent-repl profiling")' 2>&1)"; then
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
        ps -A -o pid=,rss=,time=,comm=,args= | awk -v t="$(date +%s)" '
            function seconds(s,   n, p) {
                n = split(s, p, /[:-]/)
                if (n == 2) return p[1] * 60 + p[2]
                if (n == 3) return p[1] * 3600 + p[2] * 60 + p[3]
                return p[1] * 86400 + p[2] * 3600 + p[3] * 60 + p[4]
            }
            {
                comm = $4; sub(/.*\//, "", comm)
                args = ""; for (i = 5; i <= NF; i++) args = args " " $i
                name = ""
                if (comm == "agent-repld") name = "daemon"
                else if (comm == "shim-store") name = "store"
                else if (comm == "shim-claude-sidecar") name = "sidecar"
                else if (comm == "node" && args ~ /main\.js --listen/) name = "shim"
                else if (comm == "Emacs" || comm == "Emacs-arm64-11") name = "emacs"
                else if (comm ~ /^com\.apple\.WebKit\./) name = "webkit:" substr(comm, 16)
                if (name != "") printf "{\"t\":%d,\"pid\":%d,\"cpu_s\":%.2f,\"rss_kb\":%d,\"name\":\"%s\"}\n", t, $1, seconds($3), $2, name
            }' >>"$dir/samples.jsonl"
        sleep "$interval"
    done
}

# newest_session -- the most recent session directory's id.
newest_session() {
    local d
    d="$(ls -1t "$ROOT" 2>/dev/null | grep -vx -e active -e armed | sed -n 1p || true)"
    [ -n "$d" ] || die "no session under $ROOT"
    printf '%s' "$d"
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
    local interval=2
    while [ $# -gt 0 ]; do
        case $1 in
            --interval) interval="${2:?--interval needs seconds}"; shift 2 ;;
            *) die "start: unknown argument $1" ;;
        esac
    done
    [[ $interval =~ ^[0-9]+$ ]] && [ "$interval" -ge 1 ] || die "start: --interval must be a whole number of seconds"
    [ ! -e "$ACTIVE" ] || die "a session is already open: $(cat "$ACTIVE"); stop it first"
    local id dir
    id="$(date -u +%Y%m%dT%H%M%SZ)"
    dir="$ROOT/$id"
    mkdir -p "$dir" || die "could not create $dir"
    printf '{"started_at":"%s","interval_s":%d}\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$interval" >"$dir/meta.json"
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
    local marker name runtime pid comm
    for marker in "$ARMED"/*; do
        [ -e "$marker" ] || continue
        name="${marker##*/}"
        runtime="${name%-*}"
        pid="${name##*-}"
        comm="$(ps -o comm= -p "$pid" 2>/dev/null || true)"
        if [ -n "$comm" ]; then
            printf 'armed: %s (%s)\n' "$name" "${comm##*/}"
        else
            printf 'stale: %s (the process is gone)\n' "$name"
        fi
    done
}

cmd_report() {
    local id="" startup=""
    while [ $# -gt 0 ]; do
        case $1 in
            --startup) startup="${2:?--startup needs an RFC3339 instant}"; shift 2 ;;
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
    local args=(--all --json --level info --since "$since")
    [ -z "$until" ] || args+=(--until "$until")
    "${AGENT_REPL_PROFILE_LOGS_SH:-$THIS_DIR/logs.sh}" "${args[@]}" >"$logs" 2>"$dir/logs.err" || die "bin/logs.sh failed; see $dir/logs.err"
    local report_args=(--session "$dir" --logs "$logs")
    [ -z "$startup" ] || report_args+=(--startup "$startup")
    "$helper" "${report_args[@]}" | tee "$dir/summary.md"
}

case "${1:-}" in
    start) shift; cmd_start "$@" ;;
    stop) shift; cmd_stop "$@" ;;
    status) shift; cmd_status "$@" ;;
    report) shift; cmd_report "$@" ;;
    -h|--help) usage ;;
    *) usage >&2; exit 2 ;;
esac
