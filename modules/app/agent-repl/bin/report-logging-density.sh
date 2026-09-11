#!/usr/bin/env bash
#
# report-logging-density.sh — count authored source lines and canonical
# logging call sites for every agent-repl system.
#
# This is a deliberately rough syntactic density report. It does not prove
# semantic logging coverage and cannot determine whether critical branches or
# error paths carry enough context. Reviewers still audit those paths directly.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"

# component;language;directory;canonical;debug;info;warn;error;verbose;zero policy
#
# The patterns are the canonical APIs documented by each system's AGENTS.md.
# Verbose is a class within debug, so it is reported separately but remains in
# the debug count. @remainder assigns normal calls with no explicit level to
# info, which is the documented default for the store and sidecar APIs.
COMPONENT_SPECS=(
    'daemon;go;daemon;\.(Debug|Info|Warn|Error)\(;\.Debug\(;\.Info\(;\.Warn\(;\.Error\(;@zero;required'
    'sidecar;go;agent-shim/claude/shim-sidecar;\.(Log|LogVerbose)\(;\.LogVerbose\(;@remainder;Level:[[:space:]]*"warn";Level:[[:space:]]*"error";\.LogVerbose\(;required'
    'store;go;agent-shim/shim-store;\.(Log|LogVerbose)\(;\.LogVerbose\(;@remainder;Level:[[:space:]]*"warn";Level:[[:space:]]*"error";\.LogVerbose\(;required'
    'logging;go;agent-shim/logging/go;\.(Log|LogVerbose)\(;\.LogVerbose\(;@remainder;Level:[[:space:]]*"warn";Level:[[:space:]]*"error";\.LogVerbose\(;allowed-no-own-calls'
    'shim;typescript;agent-shim/claude/shim/src;\.(debug|info|warn|error|logVerbose)\(;\.(debug|logVerbose)\(;\.info\(;\.warn\(;\.error\(;\.logVerbose\(;required'
    'webapp;typescript;webapp/src;(^|[^.[:alnum:]_])log\.(debug|info|warn|error)\(;(^|[^.[:alnum:]_])log\.debug\(;(^|[^.[:alnum:]_])log\.info\(;(^|[^.[:alnum:]_])log\.warn\(;(^|[^.[:alnum:]_])log\.error\(;verbosity:[[:space:]]*"verbose";required'
    'emacs;elisp;lisp;\(agent-repl--(log|log-verbose|info|warn|warn-once|error|fatal)[[:space:]];\(agent-repl--(log|log-verbose)[[:space:]];\(agent-repl--info[[:space:]];\(agent-repl--(warn|warn-once)[[:space:]];\(agent-repl--(error|fatal)[[:space:]];\(agent-repl--log-verbose[[:space:]];required'
)

COMPONENTS=()

die() {
    printf '[agent-repl-logging-density] ERROR: %s\n' "$*" >&2
    exit 1
}

component_spec() {
    local wanted="$1" row
    for row in "${COMPONENT_SPECS[@]}"; do
        IFS=';' read -r COMPONENT COMPONENT_LANGUAGE COMPONENT_RELATIVE_DIR \
            LOG_PATTERN DEBUG_PATTERN INFO_PATTERN WARN_PATTERN ERROR_PATTERN \
            VERBOSE_PATTERN ZERO_CALL_POLICY <<<"$row"
        if [ "$COMPONENT" = "$wanted" ]; then
            COMPONENT_DIR="$ROOT/$COMPONENT_RELATIVE_DIR"
            return 0
        fi
    done
    return 1
}

if [ "$#" -eq 0 ]; then
    for row in "${COMPONENT_SPECS[@]}"; do
        COMPONENTS+=("${row%%;*}")
    done
else
    for component in "$@"; do
        component_spec "$component" || die "unknown component '$component'"
        COMPONENTS+=("$component")
    done
fi

source_files() {
    case "$COMPONENT_LANGUAGE" in
        go)
            rg --files "$COMPONENT_DIR" \
                -g '*.go' \
                -g '!*_test.go' \
                -g '!vendor/**'
            ;;
        typescript)
            rg --files "$COMPONENT_DIR" \
                -g '*.ts' \
                -g '!*.d.ts' \
                -g '!*.test.ts' \
                -g '!node_modules/**' \
                -g '!coverage/**' \
                -g '!dist/**'
            ;;
        elisp)
            rg --files "$COMPONENT_DIR" \
                -g '*.el' \
                -g '!test-*.el'
            ;;
        *) die "$COMPONENT has unsupported language '$COMPONENT_LANGUAGE'" ;;
    esac
}

count_matches() {
    local label="$1" pattern="$2" matches match_status
    if [ "$pattern" = '@zero' ]; then
        printf '0\n'
        return
    fi
    set +e
    matches="$(rg --text -o --no-filename --no-line-number "$pattern" "${files[@]}")"
    match_status=$?
    set -e
    case "$match_status" in
        0) printf '%s\n' "$matches" | awk 'NF { count++ } END { print count + 0 }' ;;
        1) printf '0\n' ;;
        *) die "$COMPONENT $label search failed with exit code $match_status" ;;
    esac
}

printf 'component,language,source_files,source_lines,canonical_log_calls,debug,info,warn,error,verbose,calls_per_kloc,canonical_pattern,zero_call_policy\n'
for component in "${COMPONENTS[@]}"; do
    component_spec "$component" || die "unknown component '$component'"
    [ -d "$COMPONENT_DIR" ] ||
        die "$component source directory is missing: $COMPONENT_DIR"

    files=()
    while IFS= read -r file; do
        [ -n "$file" ] && files+=("$file")
    done < <(source_files)
    [ "${#files[@]}" -gt 0 ] ||
        die "$component has no authored $COMPONENT_LANGUAGE source files"

    source_lines=0
    for file in "${files[@]}"; do
        lines="$(wc -l <"$file" | tr -d ' ')"
        source_lines=$((source_lines + lines))
    done

    log_calls="$(count_matches canonical "$LOG_PATTERN")"
    debug_calls="$(count_matches debug "$DEBUG_PATTERN")"
    warn_calls="$(count_matches warn "$WARN_PATTERN")"
    error_calls="$(count_matches error "$ERROR_PATTERN")"
    verbose_calls="$(count_matches verbose "$VERBOSE_PATTERN")"
    if [ "$INFO_PATTERN" = '@remainder' ]; then
        info_calls=$((log_calls - debug_calls - warn_calls - error_calls))
        [ "$info_calls" -ge 0 ] ||
            die "$component level counts exceed its canonical call count"
    else
        info_calls="$(count_matches info "$INFO_PATTERN")"
    fi
    [ $((debug_calls + info_calls + warn_calls + error_calls)) -eq "$log_calls" ] ||
        die "$component level counts do not equal its canonical call count"
    [ "$verbose_calls" -le "$debug_calls" ] ||
        die "$component verbose count exceeds its debug count"

    calls_per_kloc="$(awk -v calls="$log_calls" -v lines="$source_lines" \
        'BEGIN { printf "%.2f", (calls * 1000) / lines }')"
    printf '%s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%s,%s,%s\n' \
        "$component" "$COMPONENT_LANGUAGE" "${#files[@]}" "$source_lines" \
        "$log_calls" "$debug_calls" "$info_calls" "$warn_calls" \
        "$error_calls" "$verbose_calls" "$calls_per_kloc" "$LOG_PATTERN" \
        "$ZERO_CALL_POLICY"
done
