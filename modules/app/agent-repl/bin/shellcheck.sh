#!/usr/bin/env bash
# The shell scripts' lint gate: shellcheck, at warning severity, over every
# shell script of the agent-repl module and of the repository's hooks,
# .claude scripts and bin/. Exits 1 when shellcheck reports anything, so a
# new warning fails the `shellcheck` suite instead of landing.
#
#   bin/shellcheck.sh           check every script
#   bin/shellcheck.sh --roots   print the roots it reads, spelled as the
#                               roster spells paths (a leading "/" is the
#                               repository root); the roster's Reuse for the
#                               suite declares exactly these
#   bin/shellcheck.sh --files   print the scripts it checks, one per line,
#                               relative to the repository root
#
# A script is a file named *.sh, or a file in a bin/ or a hooks directory
# whose first line is a sh or bash shebang. node_modules trees are not the
# module's own, so the walk skips them, and it never descends into another
# checkout: .claude/worktrees, where Claude Code keeps whole copies of the
# repository, or any directory holding a .git of its own.
set -euo pipefail

MODULE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REPO_DIR="$(cd "$MODULE_DIR/../../.." && pwd)"

ROOTS=(
    bin/
    scripts/
    hooks/
    proto/scripts/
    skills/
    tree-sitter-tldr/
    wifi-helper/
    agent-shim/claude/shim/test/integration-support/
    /.githooks/
    /.claude/
    /bin/
)

# resolve ROOT: the root's directory on disk.
resolve() {
    case "$1" in
        /*) printf '%s/%s\n' "$REPO_DIR" "${1#/}" ;;
        *) printf '%s/%s\n' "$MODULE_DIR" "$1" ;;
    esac
}

# is_script FILE: whether FILE is a shell script this gate checks.
is_script() {
    case "$1" in
        *.sh) return 0 ;;
    esac
    case "$1" in
        */bin/* | */.githooks/*) sed -n 1p "$1" | grep -E '^#!.*[/ ](ba)?sh( |$)' >/dev/null ;;
        *) return 1 ;;
    esac
}

files() {
    local root dir file
    for root in "${ROOTS[@]}"; do
        dir="$(resolve "$root")"
        [ -d "$dir" ] || {
            printf 'shellcheck.sh: the root %s (%s) is not a directory\n' "$root" "$dir" >&2
            return 1
        }
        while IFS= read -r file; do
            if is_script "$file"; then
                printf '%s\n' "${file#"$REPO_DIR"/}"
            fi
        done < <(find "$dir" \( -name node_modules -o -path "$REPO_DIR/.claude/worktrees" \) -prune \
            -o -type d ! -path "$dir" -exec test -e '{}/.git' \; -prune \
            -o -type f -print)
    done | sort -u
}

case "${1:-}" in
    --roots)
        printf '%s\n' "${ROOTS[@]}"
        exit 0
        ;;
    --files)
        files
        exit 0
        ;;
    "") ;;
    *)
        printf 'usage: %s [--roots|--files]\n' "$0" >&2
        exit 2
        ;;
esac

command -v shellcheck >/dev/null || {
    printf 'shellcheck.sh: shellcheck is not installed; the agent-repl-install skill installs it\n' >&2
    exit 1
}

list="$(files)"
mapfile -t scripts <<<"$list"
if [ "${#scripts[@]}" -eq 0 ] || [ -z "${scripts[0]}" ]; then
    printf 'shellcheck.sh: found no script under %s\n' "${ROOTS[*]}" >&2
    exit 1
fi
cd "$REPO_DIR"
# -x follows each `source`, from the sourcing script's own directory.
if shellcheck -x -P SCRIPTDIR -S warning "${scripts[@]}"; then
    printf 'shellcheck.sh: %d scripts are clean\n' "${#scripts[@]}"
else
    printf 'shellcheck.sh: shellcheck reported the warnings above; fix each script\n' >&2
    exit 1
fi
