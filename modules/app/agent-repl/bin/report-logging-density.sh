#!/usr/bin/env bash
#
# report-logging-density.sh — count authored source lines and canonical
# logging call sites for each non-Lisp agent-repl system.
#
# This is a deliberately rough syntactic density report. It does not prove
# semantic logging coverage and cannot determine whether critical branches or
# error paths carry enough context. Reviewers still audit those paths directly.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"
ALL_COMPONENTS=(daemon sidecar store wire shim webapp)
COMPONENTS=()

die() {
    printf '[agent-repl-logging-density] ERROR: %s\n' "$*" >&2
    exit 1
}

known_component_p() {
    local wanted="$1" component
    for component in "${ALL_COMPONENTS[@]}"; do
        [ "$component" = "$wanted" ] && return 0
    done
    return 1
}

if [ "$#" -eq 0 ]; then
    COMPONENTS=("${ALL_COMPONENTS[@]}")
else
    for component in "$@"; do
        known_component_p "$component" ||
            die "unknown component '$component'"
        COMPONENTS+=("$component")
    done
fi

component_spec() {
    case "$1" in
        daemon)
            COMPONENT_DIR="$ROOT/daemon"
            COMPONENT_LANGUAGE=go
            LOG_PATTERN='(\.logf|[^[:alnum:]_]logf|\.Log|\.LogVerbose|\.EmitNormal|\.EmitVerbose|\.EmitWorkspaceNormal|\.EmitWorkspaceVerbose|\.PersistForwarded|dlog\.Call)\('
            ;;
        sidecar)
            COMPONENT_DIR="$ROOT/agent-shim/claude/shim-sidecar"
            COMPONENT_LANGUAGE=go
            LOG_PATTERN='\.(Log|LogVerbose)\('
            ;;
        store)
            COMPONENT_DIR="$ROOT/agent-shim/shim-store"
            COMPONENT_LANGUAGE=go
            LOG_PATTERN='\.(Log|LogVerbose)\('
            ;;
        wire)
            COMPONENT_DIR="$ROOT/agent-shim/wire"
            COMPONENT_LANGUAGE=go
            LOG_PATTERN='\.(Log|LogVerbose)\('
            ;;
        shim)
            COMPONENT_DIR="$ROOT/agent-shim/claude/shim/src"
            COMPONENT_LANGUAGE=typescript
            LOG_PATTERN='\.(log|logVerbose)\('
            ;;
        webapp)
            COMPONENT_DIR="$ROOT/webapp/src"
            COMPONENT_LANGUAGE=typescript
            LOG_PATTERN='(^|[^.[:alnum:]_])(log|logVerbose)\('
            ;;
    esac
}

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
    esac
}

printf 'component,language,source_files,source_lines,canonical_log_calls,calls_per_kloc\n'
for component in "${COMPONENTS[@]}"; do
    component_spec "$component"
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

    set +e
    matches="$(rg -n --no-heading "$LOG_PATTERN" "${files[@]}")"
    match_status=$?
    set -e
    case "$match_status" in
        0)
            log_calls="$(
                printf '%s\n' "$matches" |
                    awk '!/:[0-9]+:[[:space:]]*(export[[:space:]]+)?function[[:space:]]+(log|logVerbose)\(/' |
                    wc -l |
                    tr -d ' '
            )"
            ;;
        1) log_calls=0 ;;
        *) die "$component logging-call search failed with exit code $match_status" ;;
    esac

    calls_per_kloc="$(awk -v calls="$log_calls" -v lines="$source_lines" \
        'BEGIN { printf "%.2f", (calls * 1000) / lines }')"
    printf '%s,%s,%d,%d,%d,%s\n' \
        "$component" "$COMPONENT_LANGUAGE" "${#files[@]}" "$source_lines" \
        "$log_calls" "$calls_per_kloc"
done
