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
  --follow           keep reading current generations after the merged history
  --json             emit the original JSONL instead of the compact human format

Harvest:
  --harvest FROM TO  count every warn/error across --all in the inclusive RFC3339 window

Every selected log includes its current file and rotation generations .1 through .5.
Malformed JSONL is an error naming the file and line; it is never skipped.
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
        -h|--help)
            usage
            exit 0
            ;;
        *) fail "unknown argument: $1" ;;
    esac
done

if [ "$harvest" -eq 1 ]; then
    [ -z "$scope" ] || fail '--harvest selects all workspace and central logs itself'
    [ -z "$since" ] || fail '--harvest cannot be combined with --since'
    [ -z "$until" ] || fail '--harvest cannot be combined with --until'
    [ "$level_set" -eq 0 ] || fail '--harvest cannot be combined with --level'
    [ -z "$runtimes" ] || fail '--harvest cannot be combined with --runtime'
    [ "$follow" -eq 0 ] || fail '--harvest cannot be combined with --follow'
    [ "$format" = human ] || fail '--harvest cannot be combined with --json'
    scope=all
    format=harvest
else
    [ -n "$scope" ] || fail 'choose --workspace, --central, --all, or --harvest'
fi

state_root="${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}"
cache_root="${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl"
emacs_global_log="${AGENT_REPL_EMACS_GLOBAL_LOG:-${TMPDIR:-/tmp}/doom-agent-repl-$(id -u)/doom-agent-repl.log}"

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
    local selector="$1" rows id dir name id_match="" name_match="" name_count=0
    if [ -d "$selector" ]; then
        (cd "$selector" && pwd -P)
        return 0
    fi
    rows="$(read_workspace_rows)"
    while IFS=$'\t' read -r id dir name; do
        [ -n "$id" ] || continue
        if [ "$id" = "$selector" ]; then
            id_match="$dir"
        fi
        if [ "$name" = "$selector" ]; then
            name_match="$dir"
            name_count=$((name_count + 1))
        fi
    done <<<"$rows"
    if [ -n "$id_match" ]; then
        printf '%s\n' "$id_match"
    elif [ "$name_count" -eq 1 ]; then
        printf '%s\n' "$name_match"
    elif [ "$name_count" -gt 1 ]; then
        fail "workspace name is ambiguous in daemon state: $selector"
    else
        fail "workspace is not a directory, ID, or name in daemon state: $selector"
    fi
}

workspace_dirs=()
if [ "$scope" = workspace ]; then
    workspace_dirs+=("$(resolve_workspace "$workspace_selector")")
elif [ "$scope" = all ]; then
    rows="$(read_workspace_rows)"
    while IFS=$'\t' read -r id dir name; do
        [ -n "$id" ] || continue
        [ -n "$dir" ] || fail "daemon workspace $id has an empty directory"
        workspace_dirs+=("$dir")
    done <<<"$rows"
fi

bases=()
if [ "$scope" = central ] || [ "$scope" = all ]; then
    runtime_requested emacs && bases+=("$emacs_global_log")
    runtime_requested daemon && bases+=("$state_root/logs/daemon.run.log")
    runtime_requested store && bases+=("$cache_root/log/shim-store.log")
    runtime_requested sidecar && bases+=("$cache_root/log/shim-claude-sidecar.log")
fi
if [ "$scope" = workspace ] || [ "$scope" = all ]; then
    for workspace_dir in "${workspace_dirs[@]}"; do
        runtime_requested emacs && bases+=("$workspace_dir/.claude/emacs/emacs.log")
        runtime_requested daemon && bases+=("$workspace_dir/.claude/emacs/daemon.log")
        runtime_requested shim && bases+=("$workspace_dir/.claude/emacs/shim.log")
        runtime_requested webapp && bases+=("$workspace_dir/.claude/emacs/webapp.log")
        runtime_requested sidecar && bases+=("$workspace_dir/.claude/emacs/sidecar.log")
    done
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

helper_args=(--mode "$format" --level "$level")
[ -z "$since" ] || helper_args+=(--since "$since")
[ -z "$until" ] || helper_args+=(--until "$until")
[ -z "$runtimes" ] || helper_args+=(--runtime "$runtimes")
[ "$follow" -eq 0 ] || helper_args+=(--follow)
if [ "$harvest" -eq 1 ]; then
    helper_args+=(--harvest-from "$harvest_from" --harvest-to "$harvest_to")
fi
for base in "${bases[@]}"; do
    helper_args+=(--base "$base")
done

exec "$binary" "${helper_args[@]}"
