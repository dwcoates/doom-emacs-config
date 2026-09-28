#!/usr/bin/env bash
# check-go-deps.sh -- fail when two of our Go modules pin one third-party
# dependency at different versions.
#
# WHY. The store, daemon and e2e modules each pinned modernc.org/sqlite on
# their own. The same driver bug (a statement leaked on context cancel, pinning
# the SQLite WAL) sat in two systems at once, and an upgrade landed in one
# module could silently leave the other behind. This check makes that drift
# loud: every go.mod under the module root is read, and a third-party module
# path required at more than one version is a failure naming the dependency,
# each version, and the go.mod files pinning it.
#
# WHAT IS READ. Every `require` entry, in both the block form and the
# single-line `require path version` form, direct and `// indirect` alike: an
# indirect pin is still the version the module builds against.
#
# WHAT IS EXEMPT. Our own local modules (agentrepl/proto, agentrepl/logging,
# ...) carry placeholder or pseudo-versions that legitimately differ, because
# what a module builds is decided by its `replace` to a directory, not by the
# version. A requirement is exempt when THE SAME go.mod replaces that path with
# a local directory (`=> ./...`, `=> ../...`, or an absolute path). Nothing is
# hardcoded: a replace to another module (`=> other/module v1.2.3`) exempts
# nothing.
#
# WHAT IS SKIPPED. go.mod files under node_modules, testdata, fixture and
# fixtures directories: those are inputs to tests or vendored packages, not
# modules we build.
#
# Usage:
#   bin/check-go-deps.sh [ROOT]
#
#   ROOT  directory to search for go.mod files (default: the agent-repl module
#         root, the parent of this script's directory)
#
# Exit status: 0 when every shared third-party dependency is pinned at one
# version, 1 on any conflict or when no go.mod is found.

set -euo pipefail

log() { echo "[check-go-deps] $*"; }
die() { echo "[check-go-deps] $*" >&2; exit 1; }

usage() {
    sed -n '/^# Usage:/,/^# Exit status/p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

case "${1:-}" in
    -h|--help) usage; exit 0 ;;
esac
[ $# -le 1 ] || die "at most one argument (ROOT) is accepted, got $#"

if [ $# -eq 1 ]; then
    ROOT="$1"
else
    ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
fi
[ -d "$ROOT" ] || die "root '$ROOT' is not a directory"
ROOT="$(cd "$ROOT" && pwd -P)"

MODS=()
while IFS= read -r mod; do
    MODS+=("$mod")
done < <(find "$ROOT" \
    \( -type d \( -name node_modules -o -name testdata -o -name fixture -o -name fixtures \) -prune \) \
    -o \( -type f -name go.mod -print \) | LC_ALL=C sort)

[ "${#MODS[@]}" -gt 0 ] || die "no go.mod found under '$ROOT'"

for mod in "${MODS[@]}"; do
    [ -r "$mod" ] || die "cannot read '$mod'"
done

# Pass 1 emits one "REQ<TAB>go.mod<TAB>path<TAB>version" line per
# non-exempt requirement. Pass 2 groups them by path and reports every path
# seen at more than one version.
REQS="$(awk -v root="$ROOT/" '
    function flush(   k, parts) {
        for (k in req) {
            split(k, parts, SUBSEP)
            if (!(parts[1] in localrep))
                printf "%s\t%s\t%s\n", rel, parts[1], parts[2]
        }
        delete req
        delete localrep
    }
    function islocal(target) {
        return target ~ /^\.\.?\// || target ~ /^\//
    }
    # A replace line, with the leading "replace" keyword already removed:
    #   path [version] => target [version]
    function onreplace(   i) {
        for (i = 1; i <= NF; i++) {
            if ($i == "=>") {
                if (i + 1 <= NF && islocal($(i + 1)))
                    localrep[$1] = 1
                return
            }
        }
    }
    FNR == 1 {
        if (NR != 1) flush()
        rel = FILENAME
        if (index(rel, root) == 1) rel = substr(rel, length(root) + 1)
        block = ""
    }
    {
        sub(/\/\/.*/, "")
        gsub(/\r/, "")
        if (NF == 0) next
    }
    block != "" && $1 == ")" { block = ""; next }
    block == "require" { if (NF >= 2) req[$1, $2] = 1; next }
    block == "replace" { onreplace(); next }
    ($1 == "require" || $1 == "replace") && $2 == "(" { block = $1; next }
    $1 == "require" && NF >= 3 { req[$2, $3] = 1; next }
    $1 == "replace" { $1 = ""; $0 = $0; onreplace(); next }
    END { if (NR > 0) flush() }
' "${MODS[@]}")"

CONFLICTS="$(printf '%s\n' "$REQS" | awk -F '\t' '
    NF == 3 {
        key = $2 SUBSEP $3
        if (!(key in files)) {
            files[key] = $1
            nver[$2]++
            vers[$2] = (vers[$2] == "" ? $3 : vers[$2] " " $3)
        } else {
            files[key] = files[key] ", " $1
        }
    }
    END {
        for (p in nver) {
            if (nver[p] < 2) continue
            n = split(vers[p], v, " ")
            line = "conflict: " p " is required at " n " versions:"
            for (i = 1; i <= n; i++)
                line = line " " v[i] " (" files[p SUBSEP v[i]] ")" (i < n ? ";" : "")
            print line
        }
    }
' | LC_ALL=C sort)"

if [ -n "$CONFLICTS" ]; then
    printf '%s\n' "$CONFLICTS" | sed 's/^/[check-go-deps] /' >&2
    die "$(printf '%s\n' "$CONFLICTS" | wc -l | tr -d ' ') third-party dependencies pinned at more than one version across ${#MODS[@]} go.mod files; align each to one version in every module that requires it"
fi

log "${#MODS[@]} go.mod files, every shared third-party dependency pinned at one version"
