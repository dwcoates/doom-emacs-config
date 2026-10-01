#!/usr/bin/env bash
# logs.sh -- the canonical reader for agent-repl structured logs.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
HELPER_SOURCE="$THIS_DIR/logs-reader.go"

usage() {
    cat <<'EOF'
Usage:
  bin/logs.sh --workspace <id|dir|name> [filters]
  bin/logs.sh --central [filters]
  bin/logs.sh --all [filters]
  bin/logs.sh --harvest <from> <to>

Selectors:
  --workspace VALUE  read one workspace, resolved by directory, daemon ID, or daemon name
  --central          read genuinely workspace-free Emacs, daemon, store, and sidecar logs
  --all              read every daemon-known workspace plus the central logs

Filters:
  --since VALUE      inclusive RFC3339 instant or lookback duration such as 15m
  --until RFC3339    inclusive RFC3339 instant
  --level LEVEL      minimum debug, info, warn, or error level
  --runtime A,B      comma-separated runtimes
  --follow           keep reading current generations after the merged history (not with --tally/--sample)
  --json             emit the original JSONL instead of the compact human format; the escape hatch, not the default
  --messages FILE    include a captured Emacs *Messages* text snapshot, scraped for warn/error-shaped lines

Compact query modes (--sample composes with --tally; the rest are exclusive of each other and of --json):
  --tally            print one count/level/runtime/operation line per group, sorted by count descending
  --sample N         print up to N representative records per group, one per line; with --tally, follows the table
  --fields a,b,c     project only the named top-level or context fields, one line per record, nothing else
  --timeline         print one compact time/level/operation/message line per record, in time order
  --width N          truncate the message shown by --sample/--timeline (default 120)

Harvest:
  --harvest FROM TO  count every warn/error and sink finding across --all in the inclusive RFC3339 window

Every selected log includes its current file and rotation generations .1 through .5.
Malformed JSONL is an error naming the file and line; it is never skipped.
Absent or unreadable sinks and workspace sinks that are not symlinks are findings.
Harvest prints attributed finding rows; other modes summarize findings on stderr.
Sink findings do not fail a read unless none of the selected sinks can be read.
A service's `.err.log` (raw stderr) and a --messages snapshot (raw *Messages*
text) are not JSONL; every compact mode and --json synthesizes an operation
("stderr" or "messages"), a runtime, and an inferred level for their lines so
they appear in the same tally, sample, timeline, or field projection as
structured records. See AGENTS.md "Logs" for the synthetic field names.
EOF
}

fail() {
    printf 'logs: %s\n' "$*" >&2
    exit 2
}

require_values() {
    local count="$1" option="$2"
    shift 2
    [ "$#" -ge "$count" ] || fail "$option requires $count value(s)"
}

scope=""
workspace_selector=""
since=""
until=""
level="debug"
level_set=0
runtimes=""
follow=0
format=human
harvest=0
harvest_from=""
harvest_to=""
tally=0
sample_set=0
sample_n=0
fields=""
timeline=0
width=120
width_set=0
messages_path=""

while [ "$#" -gt 0 ]; do
    case "$1" in
        --workspace)
            require_values 1 "$1" "$@"
            [ -z "$scope" ] || fail 'choose exactly one of --workspace, --central, and --all'
            scope=workspace
            workspace_selector="$2"
            shift 2
            ;;
        --central)
            [ -z "$scope" ] || fail 'choose exactly one of --workspace, --central, and --all'
            scope=central
            shift
            ;;
        --all)
            [ -z "$scope" ] || fail 'choose exactly one of --workspace, --central, and --all'
            scope=all
            shift
            ;;
        --since)
            require_values 1 "$1" "$@"
            [ -z "$since" ] || fail '--since may be specified once'
            since="$2"
            shift 2
            ;;
        --until)
            require_values 1 "$1" "$@"
            [ -z "$until" ] || fail '--until may be specified once'
            until="$2"
            shift 2
            ;;
        --level)
            require_values 1 "$1" "$@"
            [ "$level_set" -eq 0 ] || fail '--level may be specified once'
            level="$2"
            level_set=1
            shift 2
            ;;
        --runtime)
            require_values 1 "$1" "$@"
            [ -z "$runtimes" ] || fail '--runtime may be specified once'
            runtimes="$2"
            shift 2
            ;;
        --follow)
            [ "$follow" -eq 0 ] || fail '--follow may be specified once'
            follow=1
            shift
            ;;
        --json)
            [ "$format" = human ] || fail '--json may be specified once'
            format=json
            shift
            ;;
        --harvest)
            require_values 2 "$1" "$@"
            [ "$harvest" -eq 0 ] || fail '--harvest may be specified once'
            harvest=1
            harvest_from="$2"
            harvest_to="$3"
            shift 3
            ;;
        --tally)
            [ "$tally" -eq 0 ] || fail '--tally may be specified once'
            tally=1
            shift
            ;;
        --sample)
            require_values 1 "$1" "$@"
            [ "$sample_set" -eq 0 ] || fail '--sample may be specified once'
            case "$2" in
                *[!0-9]*|'') fail '--sample requires a positive integer' ;;
            esac
            [ "$2" -gt 0 ] || fail '--sample requires a positive integer'
            sample_n="$2"
            sample_set=1
            shift 2
            ;;
        --fields)
            require_values 1 "$1" "$@"
            [ -z "$fields" ] || fail '--fields may be specified once'
            fields="$2"
            shift 2
            ;;
        --timeline)
            [ "$timeline" -eq 0 ] || fail '--timeline may be specified once'
            timeline=1
            shift
            ;;
        --width)
            require_values 1 "$1" "$@"
            [ "$width_set" -eq 0 ] || fail '--width may be specified once'
            case "$2" in
                *[!0-9]*|'') fail '--width requires a positive integer' ;;
            esac
            [ "$2" -gt 0 ] || fail '--width requires a positive integer'
            width="$2"
            width_set=1
            shift 2
            ;;
        --messages)
            require_values 1 "$1" "$@"
            [ -z "$messages_path" ] || fail '--messages may be specified once'
            messages_path="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *) fail "unknown argument: $1" ;;
    esac
done

if [ "$tally" -eq 1 ] && [ "$timeline" -eq 1 ]; then fail '--tally and --timeline may not be combined'; fi
if [ "$tally" -eq 1 ] && [ -n "$fields" ]; then fail '--tally and --fields may not be combined'; fi
if [ "$timeline" -eq 1 ] && [ -n "$fields" ]; then fail '--timeline and --fields may not be combined'; fi
if [ "$sample_set" -eq 1 ] && [ "$timeline" -eq 1 ]; then fail '--sample and --timeline may not be combined'; fi
if [ "$sample_set" -eq 1 ] && [ -n "$fields" ]; then fail '--sample and --fields may not be combined'; fi
if [ "$format" = json ] && { [ "$tally" -eq 1 ] || [ "$sample_set" -eq 1 ] || [ -n "$fields" ] || [ "$timeline" -eq 1 ]; }; then
    fail '--json may not be combined with --tally, --sample, --fields, or --timeline'
fi

if [ "$harvest" -eq 1 ]; then
    [ -z "$scope" ] || fail '--harvest selects all workspace and central logs itself'
    [ -z "$since" ] || fail '--harvest cannot be combined with --since'
    [ -z "$until" ] || fail '--harvest cannot be combined with --until'
    [ "$level_set" -eq 0 ] || fail '--harvest cannot be combined with --level'
    [ -z "$runtimes" ] || fail '--harvest cannot be combined with --runtime'
    [ "$follow" -eq 0 ] || fail '--harvest cannot be combined with --follow'
    [ "$format" = human ] || fail '--harvest cannot be combined with --json'
    [ "$tally" -eq 0 ] || fail '--harvest cannot be combined with --tally'
    [ "$sample_set" -eq 0 ] || fail '--harvest cannot be combined with --sample'
    [ -z "$fields" ] || fail '--harvest cannot be combined with --fields'
    [ "$timeline" -eq 0 ] || fail '--harvest cannot be combined with --timeline'
    [ -z "$messages_path" ] || fail '--harvest cannot be combined with --messages'
    scope=all
    format=harvest
else
    [ -n "$scope" ] || [ -n "$messages_path" ] || fail 'choose --workspace, --central, --all, or --harvest'
fi

if [ "$format" = human ]; then
    if [ "$tally" -eq 1 ]; then
        format=tally
    elif [ "$sample_set" -eq 1 ]; then
        format=sample
    elif [ "$timeline" -eq 1 ]; then
        format=timeline
    elif [ -n "$fields" ]; then
        format=fields
    fi
fi

state_root="${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}"
cache_root="${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl"
emacs_global_log="${AGENT_REPL_EMACS_GLOBAL_LOG:-$state_root/logs/emacs.central.log}"

requested_runtimes=()
if [ -n "$runtimes" ]; then
    IFS=, read -r -a requested_runtimes <<<"$runtimes"
    for runtime in "${requested_runtimes[@]}"; do
        [ -n "$runtime" ] || fail '--runtime contains an empty value'
        case "$runtime" in
            emacs|daemon|shim|webapp|sidecar|store) ;;
            *) fail "unknown runtime: $runtime" ;;
        esac
    done
fi

runtime_requested() {
    local candidate="$1" runtime
    [ "${#requested_runtimes[@]}" -eq 0 ] && return 0
    for runtime in "${requested_runtimes[@]}"; do
        [ "$runtime" = "$candidate" ] && return 0
    done
    return 1
}

workspace_runtime_allowed() {
    case "$1" in emacs|daemon|shim|webapp|sidecar) return 0 ;; esac
    return 1
}

central_runtime_allowed() {
    case "$1" in emacs|daemon|store|sidecar) return 0 ;; esac
    return 1
}

if [ "$scope" = workspace ]; then
    for runtime in "${requested_runtimes[@]}"; do
        workspace_runtime_allowed "$runtime" || fail "runtime $runtime has no workspace log"
    done
elif [ "$scope" = central ]; then
    for runtime in "${requested_runtimes[@]}"; do
        central_runtime_allowed "$runtime" || fail "runtime $runtime has no central log"
    done
fi

read_workspace_rows() {
    local database="$state_root/wsm.db"
    [ -f "$database" ] || fail "daemon workspace state does not exist: $database"
    command -v sqlite3 >/dev/null 2>&1 || fail 'sqlite3 is required to resolve daemon workspace state'
    sqlite3 -readonly -separator $'\t' "$database" \
        'SELECT id, dir, name FROM workspaces ORDER BY id;' ||
        fail "could not read daemon workspace state: $database"
}

resolve_workspace() {
    local selector="$1" rows id dir name id_match="" id_match_dir=""
    local name_match="" name_match_id="" name_count=0
    resolved_workspace_id=""
    resolved_workspace_dir=""
    if [ -d "$selector" ]; then
        resolved_workspace_dir="$(cd "$selector" && pwd -P)"
        return 0
    fi
    rows="$(read_workspace_rows)"
    while IFS=$'\t' read -r id dir name; do
        [ -n "$id" ] || continue
        if [ "$id" = "$selector" ]; then
            id_match="$id"
            id_match_dir="$dir"
        fi
        if [ "$name" = "$selector" ]; then
            name_match="$dir"
            name_match_id="$id"
            name_count=$((name_count + 1))
        fi
    done <<<"$rows"
    if [ -n "$id_match" ]; then
        resolved_workspace_id="$id_match"
        resolved_workspace_dir="$id_match_dir"
    elif [ "$name_count" -eq 1 ]; then
        resolved_workspace_id="$name_match_id"
        resolved_workspace_dir="$name_match"
    elif [ "$name_count" -gt 1 ]; then
        fail "workspace name is ambiguous in daemon state: $selector"
    else
        fail "workspace is not a directory, ID, or name in daemon state: $selector"
    fi
}

workspace_dirs=()
workspace_ids=()
if [ "$scope" = workspace ]; then
    resolve_workspace "$workspace_selector"
    workspace_dirs+=("$resolved_workspace_dir")
    workspace_ids+=("$resolved_workspace_id")
elif [ "$scope" = all ]; then
    rows="$(read_workspace_rows)"
    while IFS=$'\t' read -r id dir name; do
        [ -n "$id" ] || continue
        [ -n "$dir" ] || fail "daemon workspace $id has an empty directory"
        workspace_dirs+=("$dir")
        workspace_ids+=("$id")
    done <<<"$rows"
fi

bases=()
base_runtimes=()
base_workspace_ids=()
base_workspace_dirs=()
base_kinds=()
base_formats=()

add_base() {
    bases+=("$1")
    base_runtimes+=("$2")
    base_workspace_ids+=("$3")
    base_workspace_dirs+=("$4")
    base_kinds+=("$5")
    base_formats+=("$6")
}

if [ "$scope" = central ] || [ "$scope" = all ]; then
    runtime_requested emacs && add_base "$emacs_global_log" emacs "" "" file jsonl
    runtime_requested daemon && add_base "$state_root/logs/daemon.run.log" daemon "" "" file jsonl
    runtime_requested store && add_base "$cache_root/log/shim-store.log" store "" "" file jsonl
    runtime_requested sidecar && add_base "$cache_root/log/shim-claude-sidecar.log" sidecar "" "" file jsonl
    # The two launchd services' emergency stderr sinks: unstructured text the
    # contract permits only when the canonical JSONL sink could not record
    # the process's own failure. Harvest keeps its own established scope and
    # does not gain these; every other mode does.
    if [ "$harvest" -eq 0 ]; then
        runtime_requested store && add_base "$cache_root/log/shim-store.err.log" store "" "" file stderr
        runtime_requested sidecar && add_base "$cache_root/log/shim-claude-sidecar.err.log" sidecar "" "" file stderr
    fi
fi
if [ "$scope" = workspace ] || [ "$scope" = all ]; then
    for workspace_index in "${!workspace_dirs[@]}"; do
        workspace_dir="${workspace_dirs[$workspace_index]}"
        workspace_id="${workspace_ids[$workspace_index]}"
        runtime_requested emacs && add_base "$workspace_dir/.claude/emacs/emacs.log" emacs "$workspace_id" "$workspace_dir" symlink jsonl
        runtime_requested daemon && add_base "$workspace_dir/.claude/emacs/daemon.log" daemon "$workspace_id" "$workspace_dir" symlink jsonl
        runtime_requested shim && add_base "$workspace_dir/.claude/emacs/shim.log" shim "$workspace_id" "$workspace_dir" symlink jsonl
        runtime_requested webapp && add_base "$workspace_dir/.claude/emacs/webapp.log" webapp "$workspace_id" "$workspace_dir" symlink jsonl
        runtime_requested sidecar && add_base "$workspace_dir/.claude/emacs/sidecar.log" sidecar "$workspace_id" "$workspace_dir" symlink jsonl
    done
fi
if [ "$harvest" -eq 0 ] && [ -n "$messages_path" ]; then
    # A captured Emacs *Messages* text snapshot: also not JSONL, scraped for
    # the same warn/error-shaped lines e2e/realtest/messages.go looks for.
    runtime_requested emacs && add_base "$messages_path" emacs "" "" file messages
fi

command -v go >/dev/null 2>&1 || fail 'go is required to build the structured-log reader'
[ -f "$HELPER_SOURCE" ] || fail "reader source is missing: $HELPER_SOURCE"
fingerprint="$(cksum "$HELPER_SOURCE")" || fail "could not fingerprint reader source: $HELPER_SOURCE"
checksum="${fingerprint%% *}"
remainder="${fingerprint#* }"
size="${remainder%% *}"
build_root="${AGENT_REPL_LOGS_BUILD_DIR:-${TMPDIR:-/tmp}/agent-repl-logs-$(id -u)}"
binary="$build_root/logs-reader-$checksum-$size"
if [ ! -x "$binary" ]; then
    mkdir -p "$build_root" || fail "could not create reader build directory: $build_root"
    build_tmp="$binary.tmp.$$"
    if ! go build -trimpath -o "$build_tmp" "$HELPER_SOURCE"; then
        rm -f "$build_tmp"
        fail 'could not build the structured-log reader'
    fi
    if ! chmod 0755 "$build_tmp"; then
        rm -f "$build_tmp"
        fail "could not make the structured-log reader executable: $build_tmp"
    fi
    if ! mv -f "$build_tmp" "$binary"; then
        rm -f "$build_tmp"
        fail "could not install the structured-log reader: $binary"
    fi
fi

helper_args=(--mode "$format" --level "$level" --width "$width" --sample-n "$sample_n")
[ -z "$since" ] || helper_args+=(--since "$since")
[ -z "$until" ] || helper_args+=(--until "$until")
[ -z "$runtimes" ] || helper_args+=(--runtime "$runtimes")
[ "$follow" -eq 0 ] || helper_args+=(--follow)
[ -z "$fields" ] || helper_args+=(--fields "$fields")
if [ "$harvest" -eq 1 ]; then
    helper_args+=(--harvest-from "$harvest_from" --harvest-to "$harvest_to")
fi
# EVERY WORKSPACE IS SHOWN BY ITS NAME: the reader is handed the daemon's
# ID-to-name table and stamps a synthetic workspace_name on each record.
# Workspace scopes have already required the table; a central-only read of a
# state root the daemon never wrote has no workspace to name.
if [ "$scope" != central ] || [ -f "$state_root/wsm.db" ]; then
    while IFS=$'\t' read -r id dir name; do
        [ -n "$id" ] || continue
        [ -n "$name" ] || fail "daemon workspace $id has an empty name"
        helper_args+=(--workspace-name "$id=$name")
    done <<<"$(read_workspace_rows)"
fi
for base_index in "${!bases[@]}"; do
    helper_args+=(
        --base "${bases[$base_index]}"
        --base-runtime "${base_runtimes[$base_index]}"
        --base-workspace-id "${base_workspace_ids[$base_index]}"
        --base-workspace-dir "${base_workspace_dirs[$base_index]}"
        --base-kind "${base_kinds[$base_index]}"
        --base-format "${base_formats[$base_index]}"
    )
done

exec "$binary" "${helper_args[@]}"
