#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# readiness-report.sh — print ONE JSON document saying, per agent-repl system,
# whether what is deployed is what master says it should be.
#
# The problem it answers is the one AGENTS.md opens with: every component here
# is a built artifact and every running process keeps serving the image it
# started with, so a merged commit deploys nothing. A merged-but-undeployed fix
# looks exactly like a fix that does not work. This report makes that state
# visible instead of leaving it to be rediscovered during a debugging session.
#
# Systems reported: daemon, shim, webapp, shim-store, shim-claude-sidecar.
#
# ELISP IS DELIBERATELY NOT REPORTED. Every other system's deployed revision is
# recoverable from disk, but elisp is deployed by LOADING it into a live Emacs,
# and only that Emacs knows which definitions are currently in its obarray — a
# file's mtime, its git sha, and even its presence say nothing about whether
# the running image has it. Any answer this script could give would be a guess,
# so it gives none. (Emacs itself is the only honest reporter there.)
#
# Three questions per system:
#
#   1. DEPLOYED revision — read from the `.built-sha` stamp written beside the
#      artifact by build-frontend.sh / deploy-all.sh. A missing stamp reports
#      "unknown"; it is never inferred from mtimes or from repo HEAD, because a
#      guess is indistinguishable from a fact once it is in the JSON.
#
#   2. SOURCE revision — the newest commit touching that system's pathspec.
#      proto/ is in every system's pathspec: regeneration flows from it, so a
#      proto edit genuinely stales every Go and TypeScript artifact.
#
#   3. RUNNING staleness — whether a live process is serving an older image
#      than the one installed on disk. Only systems with a single long-lived
#      process have this: the daemon, and the two launchd services. The shim is
#      spawned per session and the webapp is loaded into webviews, and BOTH
#      reach the user through a daemon bounce rather than through a process of
#      their own, so they report "running": null rather than a fabricated one.
#
# Distance is measured with the system's pathspec applied
# (`git rev-list --count <deployed>..<source> -- <paths>`), not repo-wide. A
# deployed artifact is built at repo HEAD, which is almost never the same
# commit as the newest commit touching that one system, so the unfiltered count
# would report every unrelated commit in between as a deploy gap. With the
# pathspec applied the count is exactly "changes to THIS system that the
# deployed build does not have", and it is zero precisely when the build is
# current.
#
# Output is ALWAYS a single valid JSON document. A system that cannot be
# assessed carries an "error" string and nulls rather than aborting the run;
# exit stays 0. Exit is non-zero only when the report itself cannot be produced
# at all (no git checkout).
#
# Shape:
#   { "generated_at": "<iso8601>",
#     "repo_head": { "sha": "<sha>" },
#     "systems": [ { "name": …, "deployed_sha": …|null, "deployed_dirty": bool,
#                    "source_sha": …|null, "commits_behind": int|null,
#                    "minutes_behind": int|null,
#                    "running": { "pid": int, "started_at": …|null,
#                                 "stale_binary": bool } | null,
#                    "ready": bool, "error": … (only when something failed) } ] }
#
# `ready` is true when the deployed build carries every committed change to the
# system AND no running process is serving a stale binary.
#
# Cost: git plumbing and pgrep only, never a build. It is meant to be polled
# (Emacs runs it every ~15s), so nothing here may block on the network, take a
# lock, or touch launchd.
#
# `--require-ready SYSTEM` turns the report into a structural deployment gate
# for one named system.  It still writes the complete JSON document; its
# top-level `gate` object repeats the selected system's source and deployed
# revisions, so a failing invocation is actionable from its output alone.
#
# Exit codes:
#   0  a report was produced; the requested gate, when present, passed
#   1  no report could be produced (not a git checkout)
#   2  bad usage
#   3  a report was produced but the requested gate failed

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"

# shellcheck source=lib-deploy-stamp.sh
. "$THIS_DIR/lib-deploy-stamp.sh"

usage() {
    echo "usage: readiness-report.sh [--require-ready SYSTEM]" >&2
    echo "  prints one JSON deploy-readiness document for agent-repl" >&2
}

REQUIRED_SYSTEM=""
case "$#" in
    0) ;;
    1)
        case "$1" in
            -h|--help) usage; exit 0 ;;
            *) echo "readiness-report.sh: unknown argument: $1" >&2; usage; exit 2 ;;
        esac
        ;;
    2)
        if [ "$1" != "--require-ready" ]; then
            echo "readiness-report.sh: unknown argument: $1" >&2
            usage
            exit 2
        fi
        REQUIRED_SYSTEM="$2"
        ;;
    *) echo "readiness-report.sh: expected no arguments or --require-ready SYSTEM" >&2; usage; exit 2 ;;
esac

command -v git >/dev/null 2>&1 || {
    echo "readiness-report.sh: git is not on PATH; no report can be produced" >&2
    exit 1
}

REPO_ROOT="$(git -C "$ROOT" rev-parse --show-toplevel 2>/dev/null)" || {
    echo "readiness-report.sh: $ROOT is not inside a git checkout; no report can be produced" >&2
    exit 1
}

REPO_HEAD="$(git -C "$REPO_ROOT" rev-parse HEAD 2>/dev/null)" || {
    echo "readiness-report.sh: $REPO_ROOT has no HEAD commit; no report can be produced" >&2
    exit 1
}

# Where the module sits inside the checkout, derived rather than hardcoded as
# "modules/app/agent-repl" for the same reason build-frontend.sh derives it:
# the layout is the repo's to change, and a stale hardcoded prefix would make
# every pathspec silently match nothing — which reads as "fully deployed".
REL_ROOT="${ROOT#"$REPO_ROOT"/}"
if [ "$REL_ROOT" = "$ROOT" ]; then
    REL_ROOT=""
fi
prefix() { if [ -n "$REL_ROOT" ]; then printf '%s/%s' "$REL_ROOT" "$1"; else printf '%s' "$1"; fi; }

CACHE_BIN="$HOME/.cache/agent-repl/bin"

SYSTEMS=(daemon shim webapp shim-store shim-claude-sidecar)

system_known() {
    local system
    for system in "${SYSTEMS[@]}"; do
        [ "$system" = "$1" ] && return 0
    done
    return 1
}

if [ -n "$REQUIRED_SYSTEM" ] && ! system_known "$REQUIRED_SYSTEM"; then
    echo "readiness-report.sh: unknown system for --require-ready: $REQUIRED_SYSTEM" >&2
    usage
    exit 2
fi

GATE_READY=""
GATE_DEPLOYED_SHA=""
GATE_SOURCE_SHA=""
GATE_ERROR=""

# system_stamp NAME — path of the `.built-sha` stamp for NAME's artifact.
system_stamp() {
    case "$1" in
        daemon)              printf '%s' "$ROOT/daemon/bin/.built-sha" ;;
        shim)                printf '%s' "$ROOT/agent-shim/claude/shim/dist/.built-sha" ;;
        webapp)              printf '%s' "$ROOT/webapp/dist/.built-sha" ;;
        shim-store)          printf '%s' "$CACHE_BIN/.shim-store.built-sha" ;;
        shim-claude-sidecar) printf '%s' "$CACHE_BIN/.shim-claude-sidecar.built-sha" ;;
        *) return 1 ;;
    esac
}

# proto_paths — the proto tree as a BUILD input: the wire schemas minus the
# review artifacts that live beside them. figma-idl-draft/ and the sketch are
# design documents no build reads, so a commit touching only them must not
# make any system read as behind — that state is undeployable by rebuilding,
# because the staleness check (correctly) sees no buildable input change and
# the stamp can never catch up to the gate.
proto_paths() {
    printf '%s %s %s' "$(prefix proto)" \
        ":(exclude)$(prefix proto/figma-idl-draft)" \
        ":(exclude)$(prefix proto/SKETCH-figma-idl.md)"
}

# system_paths NAME — repo-relative pathspec, space separated. No path in this
# repo contains a space, and keeping them in one string is what lets bash 3.2
# (still /bin/bash on macOS, no associative arrays) carry a per-system table.
system_paths() {
    case "$1" in
        daemon)              printf '%s %s %s %s' "$(prefix daemon)" "$(proto_paths)" "$(prefix agent-shim/wire)" "$(prefix agent-shim/logging)" ;;
        shim)                printf '%s %s %s' "$(prefix agent-shim/claude/shim)" "$(proto_paths)" "$(prefix agent-shim/logging)" ;;
        webapp)              printf '%s %s %s' "$(prefix webapp)" "$(proto_paths)" "$(prefix agent-shim/logging)" ;;
        shim-store)          printf '%s %s %s %s' "$(prefix agent-shim/shim-store)" "$(prefix agent-shim/wire)" "$(prefix agent-shim/logging)" "$(proto_paths)" ;;
        shim-claude-sidecar) printf '%s %s %s %s' "$(prefix agent-shim/claude/shim-sidecar)" "$(prefix agent-shim/wire)" "$(prefix agent-shim/logging)" "$(proto_paths)" ;;
        *) return 1 ;;
    esac
}

# system_process_match NAME — the absolute binary path a live process for NAME
# runs, or nothing when NAME has no single long-lived process.
system_process_match() {
    case "$1" in
        daemon)              printf '%s' "$ROOT/daemon/bin/claude-repld" ;;
        shim-store)          printf '%s' "$CACHE_BIN/shim-store" ;;
        shim-claude-sidecar) printf '%s' "$CACHE_BIN/shim-claude-sidecar" ;;
        *) printf '' ;;
    esac
}

# ---- JSON emission ---------------------------------------------------------

json_escape() {
    local s="$1"
    s="${s//\\/\\\\}"
    s="${s//\"/\\\"}"
    s="${s//$'\n'/\\n}"
    s="${s//$'\r'/\\r}"
    s="${s//$'\t'/\\t}"
    printf '%s' "$s"
}

jstr() { printf '"%s"' "$(json_escape "$1")"; }

# jstr_or_null VALUE — a JSON string, or the JSON null literal when empty. The
# distinction is the whole point of the report: null means "not known", and it
# must never render as the empty string, which reads as a known-empty answer.
jstr_or_null() {
    if [ -n "$1" ]; then jstr "$1"; else printf 'null'; fi
}

jnum_or_null() {
    if [ -n "$1" ]; then printf '%s' "$1"; else printf 'null'; fi
}

jbool() { if [ "$1" -eq 1 ]; then printf 'true'; else printf 'false'; fi; }

iso8601() { # EPOCH -> UTC ISO8601
    date -u -r "$1" +%Y-%m-%dT%H:%M:%SZ 2>/dev/null ||
        date -u -d "@$1" +%Y-%m-%dT%H:%M:%SZ 2>/dev/null ||
        printf ''
}

file_mtime() {
    if [ -e "$1" ]; then
        stat -f %m "$1" 2>/dev/null || stat -c %Y "$1"
    else
        printf '0'
    fi
}

# proc_start_epoch PID — when the process started, derived from `ps -o etime`
# ([[dd-]hh:]mm:ss) rather than parsing `ps -o lstart`, whose human date format
# needs a different `date` invocation on BSD and GNU and would silently produce
# a wrong answer on the wrong one. Prints nothing when PID is gone.
proc_start_epoch() {
    local et days=0 hours=0 mins secs rest
    et="$(ps -o etime= -p "$1" 2>/dev/null | tr -d '[:space:]')" || return 0
    [ -n "$et" ] || return 0
    rest="$et"
    case "$rest" in *-*) days="${rest%%-*}"; rest="${rest#*-}" ;; esac
    secs="${rest##*:}"
    rest="${rest%:*}"
    mins="${rest##*:}"
    case "$rest" in *:*) hours="${rest%%:*}" ;; esac
    # 10#: etime zero-pads, and bash reads a leading-zero literal as octal, so
    # "08" would be a syntax error rather than eight.
    printf '%s' "$(( $(date +%s) - (10#$days * 86400 + 10#$hours * 3600 + 10#$mins * 60 + 10#$secs) ))"
}

# ---- per-system assessment -------------------------------------------------

emit_system() { # NAME
    local name="$1"
    local stamp paths stamp_value deployed_sha deployed_commit deployed_dirty=0
    local source_sha="" source_ct="" deployed_ct="" commits_behind="" minutes_behind=""
    local error="" ready=0
    local match pid="" started_epoch="" started_at="" stale=0 running_json="null"

    stamp="$(system_stamp "$name")"
    paths="$(system_paths "$name")"

    # 1. deployed revision, strictly from the stamp.
    deployed_sha=""
    deployed_commit=""
    if stamp_value="$(read_built_sha "$stamp")"; then
        deployed_sha="$stamp_value"
        deployed_commit="$(built_sha_commit "$stamp_value")"
        if built_sha_is_dirty "$stamp_value"; then deployed_dirty=1; fi
    fi

    # 2. newest commit touching the system, with its commit timestamp.
    local log_line
    # shellcheck disable=SC2086
    if log_line="$(git -C "$REPO_ROOT" log -1 --format='%H %ct' -- $paths 2>/dev/null)" \
       && [ -n "$log_line" ]; then
        source_sha="${log_line%% *}"
        source_ct="${log_line##* }"
    else
        error="no commit touches this system's pathspec: $paths"
    fi

    # 3. distances, only when both ends are known commits in THIS checkout.
    if [ -n "$deployed_commit" ] && [ -n "$source_sha" ]; then
        if ! deployed_ct="$(git -C "$REPO_ROOT" show -s --format=%ct "$deployed_commit" 2>/dev/null)"; then
            deployed_ct=""
            error="deployed revision $deployed_commit is not present in this checkout"
        else
            # shellcheck disable=SC2086
            if ! commits_behind="$(git -C "$REPO_ROOT" rev-list --count "$deployed_commit..$source_sha" -- $paths 2>/dev/null)"; then
                commits_behind=""
                error="could not count commits between $deployed_commit and $source_sha"
            elif [ "$commits_behind" -eq 0 ]; then
                # The deployed build already carries every committed change to
                # this system, even when it was built at an older repo HEAD.
                minutes_behind=0
            else
                minutes_behind=$(( (source_ct - deployed_ct) / 60 ))
                [ "$minutes_behind" -lt 0 ] && minutes_behind=0
            fi
        fi
    fi

    # 4. running process, where one exists.
    match="$(system_process_match "$name")"
    if [ -n "$match" ]; then
        pid="$(pgrep -f "$match" 2>/dev/null | head -n1 || true)"
        if [ -n "$pid" ]; then
            started_epoch="$(proc_start_epoch "$pid")"
            if [ -n "$started_epoch" ]; then started_at="$(iso8601 "$started_epoch")"; fi
            case "$name" in
                daemon)
                    # No kickstart stamp exists for the daemon (Emacs owns its
                    # lifecycle), so staleness is the direct question: was the
                    # binary written after this process began executing it?
                    if [ -n "$started_epoch" ] \
                       && [ "$(file_mtime "$match")" -gt "$started_epoch" ]; then
                        stale=1
                    fi
                    ;;
                *)
                    # The launchd services DO have a kickstart stamp, and it is
                    # the authority deploy-all.sh already bounces on. Reusing
                    # service_needs_bounce keeps the report and the deploy from
                    # ever disagreeing about what "already deployed" means.
                    if service_needs_bounce "$CACHE_BIN" "$name"; then stale=1; fi
                    ;;
            esac
            running_json="{\"pid\": $pid, \"started_at\": $(jstr_or_null "$started_at"), \"stale_binary\": $(jbool "$stale")}"
        fi
    fi

    # 5. verdict.
    if [ -n "$commits_behind" ] && [ "$commits_behind" -eq 0 ] && [ "$stale" -eq 0 ]; then
        ready=1
    fi
    if [ -z "$deployed_sha" ] && [ -z "$error" ]; then
        error="no .built-sha stamp at $stamp; this artifact has not been built by a stamping build"
    fi

    if [ "$name" = "$REQUIRED_SYSTEM" ]; then
        GATE_READY="$ready"
        GATE_DEPLOYED_SHA="$deployed_sha"
        GATE_SOURCE_SHA="$source_sha"
        GATE_ERROR="$error"
    fi

    printf '    {"name": %s, "deployed_sha": %s, "deployed_dirty": %s, "source_sha": %s, "commits_behind": %s, "minutes_behind": %s, "running": %s, "ready": %s' \
        "$(jstr "$name")" \
        "$(jstr_or_null "$deployed_sha")" \
        "$(jbool "$deployed_dirty")" \
        "$(jstr_or_null "$source_sha")" \
        "$(jnum_or_null "$commits_behind")" \
        "$(jnum_or_null "$minutes_behind")" \
        "$running_json" \
        "$(jbool "$ready")"
    if [ -n "$error" ]; then
        printf ', "error": %s' "$(jstr "$error")"
    fi
    printf '}'
}

# ---- document --------------------------------------------------------------

printf '{\n'
printf '  "generated_at": %s,\n' "$(jstr "$(iso8601 "$(date +%s)")")"
printf '  "repo_head": {"sha": %s},\n' "$(jstr "$REPO_HEAD")"
printf '  "systems": [\n'
FIRST=1
for sys in "${SYSTEMS[@]}"; do
    if [ "$FIRST" -eq 0 ]; then printf ',\n'; fi
    FIRST=0
    emit_system "$sys"
done
printf '\n  ]'
if [ -n "$REQUIRED_SYSTEM" ]; then
    if [ "$GATE_READY" -eq 1 ]; then
        GATE_ERROR=""
    elif [ -z "$GATE_ERROR" ]; then
        GATE_ERROR="required system is not ready"
    fi
    printf ',\n  "gate": {"system": %s, "ready": %s, "deployed_sha": %s, "source_sha": %s' \
        "$(jstr "$REQUIRED_SYSTEM")" \
        "$(jbool "$GATE_READY")" \
        "$(jstr_or_null "$GATE_DEPLOYED_SHA")" \
        "$(jstr_or_null "$GATE_SOURCE_SHA")"
    if [ -n "$GATE_ERROR" ]; then
        printf ', "error": %s' "$(jstr "$GATE_ERROR")"
    fi
    printf '}'
fi
printf '\n}\n'

if [ -n "$REQUIRED_SYSTEM" ] && [ "$GATE_READY" -ne 1 ]; then
    exit 3
fi
