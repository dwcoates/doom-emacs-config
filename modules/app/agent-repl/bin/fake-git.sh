#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# fake-git.sh — the `git` the bin/ harnesses put on PATH. TEST SUPPORT ONLY.
#
# NO TEST RUNS REAL GIT (owner rule). The scripts these harnesses cover are git
# plumbing — staleness is "which source revision is this artifact built from",
# readiness is "how far behind the newest commit touching the system is it" —
# so a fixture still needs history, a working tree that can be dirty, and
# pathspecs that select part of it. This models exactly that, as files under
# <top>/.fakegit, and nothing else:
#
#   fixture construction: init, add -A | add PATH..., commit -m, revert HEAD
#   what the scripts ask: rev-parse HEAD | --show-toplevel |
#                         --is-inside-work-tree | --git-dir |
#                         --git-common-dir, diff --cached --name-only,
#                         config KEY VALUE, config --type=bool --get KEY,
#                         status --porcelain,
#                         ls-files -s, log -1 --format, show -s --format=%ct,
#                         rev-list --count A..B, worktree list --porcelain
#
# History is linear (every commit has at most one parent), which is the only
# shape the fixtures build. Anything not modelled exits 2 naming the call, so a
# new git call in a script under test fails its harness loudly instead of
# passing on a guess. Every invocation's argv is appended to $FAKE_GIT_LOG when
# that is set.
#
# Ignore rules come from <top>/.gitignore, one pattern per line: `name/`
# ignores a directory of that name at any depth, and any other pattern (globs
# allowed) ignores a path any of whose components matches it.

set -euo pipefail

[ -z "${FAKE_GIT_LOG:-}" ] || printf 'git %s\n' "$*" >> "$FAKE_GIT_LOG"

die() { printf 'fake git: %s\n' "$*" >&2; exit "${EXIT:-128}"; }

CWD="$PWD"
while [ $# -gt 0 ]; do
    case "$1" in
        -C)
            # An absolute directory is taken as given (no process); anything
            # else is resolved the way `cd` would.
            case "$2" in
                /*) [ -d "$2" ] || die "cannot change to '$2'"; CWD="${2%/}"; [ -n "$CWD" ] || CWD=/ ;;
                *) CWD="$(cd "$2" 2>/dev/null && pwd)" || die "cannot change to '$2'" ;;
            esac
            shift 2
            ;;
        -c) shift 2 ;;
        *) break ;;
    esac
done
[ $# -gt 0 ] || EXIT=2 die "no subcommand"
CMD="$1"; shift

# ---- repository discovery -------------------------------------------------

find_top() {
    local dir="$CWD"
    while :; do
        if [ -d "$dir/.fakegit" ]; then printf '%s' "$dir"; return 0; fi
        [ -n "${GIT_CEILING_DIRECTORIES:-}" ] && [ "$dir" = "$GIT_CEILING_DIRECTORIES" ] && return 1
        [ "$dir" = "/" ] && return 1
        dir="${dir%/*}"; [ -n "$dir" ] || dir=/
    done
}

if [ "$CMD" = init ]; then
    mkdir -p "$CWD/.fakegit/commits" "$CWD/.fakegit/objects"
    : > "$CWD/.fakegit/index"
    exit 0
fi

TOP="$(find_top)" || die "not a git repository (or any of the parent directories)"
G="$TOP/.fakegit"
# Where the -C directory sits in the tree: relative pathspecs resolve from it.
PREFIX="${CWD#"$TOP"}"; PREFIX="${PREFIX#/}"

# ---- helpers ----------------------------------------------------------------

IGNORES=()
if [ -f "$TOP/.gitignore" ]; then
    while IFS= read -r line; do
        [ -n "$line" ] && IGNORES+=("$line")
    done < "$TOP/.gitignore"
fi

# PRUNE — the ignore rules as find(1) arguments. `find -name` matches any one
# path component with the same glob rules as the patterns, at any depth, which
# is exactly the rule above; pruning also keeps the walk out of ignored trees
# (node_modules, the fixture's dep store) altogether.
PRUNE=(-path ./.fakegit)
for pat in ${IGNORES[@]+"${IGNORES[@]}"}; do
    case "$pat" in
        */) PRUNE+=(-o '(' -type d -name "${pat%/}" ')') ;;
        *) PRUNE+=(-o -name "$pat") ;;
    esac
done

# worktree_snapshot — "<blob>\t<path>" for every file not ignored, sorted by
# path.
#
# The paths are hashed in ONE shasum call rather than one per file: the
# harnesses ask for a snapshot on nearly every git call, and a process per file
# made the suites several times slower than the scripts they test.
worktree_snapshot() {
    (
        cd "$TOP"
        find . '(' "${PRUNE[@]}" ')' -prune -o -type f -print0 |
            xargs -0 shasum -a 1 -- |
            awk '{ path = substr($0, 43); sub(/^\.\//, "", path); if (path != "-") print $1 "\t" path }'
    ) | LC_ALL=C sort -t "$(printf '\t')" -k2
}

# SPECS — the pathspec after `--`, normalized to top-relative form. Empty means
# the whole tree.
SPECS=()
set_specs() {
    local s neg
    SPECS=()
    for s in "$@"; do
        neg=""
        case "$s" in ":(exclude)"*) neg=":(exclude)"; s="${s#:(exclude)}" ;; esac
        case "$s" in
            "$TOP") s="." ;;
            "$TOP"/*) s="${s#"$TOP"/}" ;;
            /*) EXIT=128 die "'$s' is outside repository at '$TOP'" ;;
            *) [ -n "$PREFIX" ] && s="$PREFIX/$s" ;;
        esac
        s="${s%/}"
        SPECS+=("$neg$s")
    done
}

# path_matches PATH — true when PATH is selected by SPECS.
path_matches() {
    local path="$1" s included=0 any_include=0
    [ "${#SPECS[@]}" -eq 0 ] && return 0
    for s in "${SPECS[@]}"; do
        case "$s" in ":(exclude)"*) continue ;; esac
        any_include=1
        if [ "$s" = "." ] || [ "$path" = "$s" ] || [ "${path#"$s"/}" != "$path" ]; then
            included=1
        fi
    done
    [ "$any_include" -eq 0 ] && included=1
    [ "$included" -eq 1 ] || return 1
    for s in "${SPECS[@]}"; do
        case "$s" in ":(exclude)"*) s="${s#:(exclude)}" ;; *) continue ;; esac
        if [ "$path" = "$s" ] || [ "${path#"$s"/}" != "$path" ]; then return 1; fi
    done
    return 0
}

# filter_snapshot [TAG] — keep the snapshot lines on stdin whose path SPECS
# selects, each prefixed with "TAG\t" when a TAG is given.
filter_snapshot() {
    local tab=$'\t' blob path tag
    tag="${1:+$1$tab}"
    while IFS=$'\t' read -r blob path; do
        path_matches "$path" && printf '%s%s\t%s\n' "$tag" "$blob" "$path"
    done
    return 0
}

# changed_paths A B — the paths whose entry differs between two snapshots.
changed_paths() {
    awk -F'\t' '
        NR == FNR { blob[$2] = $1; next }
        { if (!($2 in blob) || blob[$2] != $1) print $2; delete blob[$2] }
        END { for (p in blob) print p }' "$1" "$2" | LC_ALL=C sort
}

# HEAD_SHA / HEAD_TREE — the current commit and its tree file, read once
# without a process ("" and /dev/null before the first commit).
HEAD_SHA=""
HEAD_TREE=/dev/null
load_head() {
    HEAD_SHA=""
    HEAD_TREE=/dev/null
    if [ -f "$G/HEAD" ]; then
        IFS= read -r HEAD_SHA < "$G/HEAD" || true
        HEAD_TREE="$G/commits/$HEAD_SHA/tree"
    fi
}
load_head

head_sha() {
    [ -n "$HEAD_SHA" ] || return 1
    printf '%s\n' "$HEAD_SHA"
}

head_tree() { cat "$HEAD_TREE"; }

# stage — record the worktree's entries for SPECS in the index, and their
# content in the object store (revert restores from it).
stage() {
    local tmp="$G/.stage.$$" blob path
    # Entries outside the spec stay as they are.
    while IFS=$'\t' read -r blob path; do
        path_matches "$path" || printf '%s\t%s\n' "$blob" "$path"
    done < "$G/index" > "$tmp"
    worktree_snapshot | filter_snapshot | while IFS=$'\t' read -r blob path; do
        [ -f "$G/objects/$blob" ] || cp "$TOP/$path" "$G/objects/$blob"
        printf '%s\t%s\n' "$blob" "$path"
    done >> "$tmp"
    LC_ALL=C sort -t $'\t' -k2 "$tmp" > "$G/index"
    rm -f "$tmp"
}

# commit_index MSG — snapshot the index as a new commit on HEAD.
commit_index() {
    local msg="$1" ct sha dir changed="$G/.changed.$$"
    changed_paths "$HEAD_TREE" "$G/index" > "$changed"
    if [ ! -s "$changed" ]; then
        rm -f "$changed"
        EXIT=1 die "nothing to commit, working tree clean"
    fi
    case "${GIT_COMMITTER_DATE:-}" in
        @*) ct="${GIT_COMMITTER_DATE#@}"; ct="${ct%% *}" ;;
        *) ct="$(date +%s)" ;;
    esac
    read -r sha _ < <(
        { printf '%s\n%s\n%s\n' "$HEAD_SHA" "$ct" "$msg"; cat "$G/index"; } | shasum -a 1)
    dir="$G/commits/$sha"
    mkdir -p "$dir"
    cp "$G/index" "$dir/tree"
    mv "$changed" "$dir/changed"
    printf '%s\n' "$HEAD_SHA" > "$dir/parent"
    printf '%s\n' "$ct" > "$dir/ct"
    printf '%s\n' "$msg" > "$dir/msg"
    printf '%s\n' "$sha" > "$G/HEAD"
    load_head
}

# commit_touches SHA — true when SHA changed a path SPECS selects.
commit_touches() {
    local path
    while IFS= read -r path; do
        path_matches "$path" && return 0
    done < "$G/commits/$1/changed"
    return 1
}

# first_line FILE — FILE's first line, without a process (history walks call
# this once per commit).
first_line() {
    local line=""
    IFS= read -r line < "$1" || true
    printf '%s\n' "$line"
}

parent_of() { first_line "$G/commits/$1/parent"; }

resolve() {
    local rev="$1"
    [ "$rev" = HEAD ] && { head_sha || return 1; return 0; }
    [ -d "$G/commits/$rev" ] || return 1
    printf '%s\n' "$rev"
}

# split_dashdash ARGS... — OPTS before `--`, SPECS after it.
OPTS=()
split_dashdash() {
    OPTS=()
    while [ $# -gt 0 ]; do
        if [ "$1" = "--" ]; then shift; set_specs "$@"; return 0; fi
        OPTS+=("$1"); shift
    done
    set_specs
}

format_commit() { # FORMAT SHA
    local out="$1"
    out="${out//%H/$2}"
    out="${out//%ct/$(first_line "$G/commits/$2/ct")}"
    printf '%s\n' "$out"
}

# ---- subcommands ------------------------------------------------------------

split_dashdash "$@"
case "$CMD $*" in
    "rev-parse HEAD")
        head_sha || die "ambiguous argument 'HEAD': unknown revision"
        ;;
    "rev-parse --show-toplevel") printf '%s\n' "$TOP" ;;
    "rev-parse --is-inside-work-tree") printf 'true\n' ;;
    # The repository's own directory is .fakegit, and there is one worktree,
    # so the common dir is the same directory.
    "rev-parse --git-dir" | "rev-parse --git-common-dir") printf '%s\n' "$G" ;;
    # Repository config is <top>/.fakegit/config, one KEY=VALUE per line, the
    # last line for a key winning, as a later `git config` replaces it.
    "config --type=bool --get "*)
        [ "${#OPTS[@]}" -eq 3 ] || EXIT=2 die "unmodelled config form: $*"
        key="${OPTS[2]}"
        value=""
        found=0
        if [ -f "$G/config" ]; then
            while IFS= read -r line; do
                if [ "${line%%=*}" = "$key" ]; then value="${line#*=}"; found=1; fi
            done < "$G/config"
        fi
        [ "$found" -eq 1 ] || exit 1
        # git's own boolean spellings, case-insensitive; anything else is the
        # fatal error real git answers with status 128.
        case "$(printf '%s' "$value" | tr '[:upper:]' '[:lower:]')" in
            true | yes | on | 1) printf 'true\n' ;;
            false | no | off | 0 | "") printf 'false\n' ;;
            *) die "bad boolean config value '$value' for '$key'" ;;
        esac
        ;;
    "config "*)
        [ "${#OPTS[@]}" -eq 2 ] || EXIT=2 die "unmodelled config form: $*"
        case "${OPTS[0]}" in -*) EXIT=2 die "unmodelled config form: $*" ;; esac
        printf '%s=%s\n' "${OPTS[0]}" "${OPTS[1]}" >> "$G/config"
        ;;
    "diff --cached --name-only"*)
        [ "${OPTS[*]}" = "--cached --name-only" ] || EXIT=2 die "unmodelled diff form: $*"
        changed_paths "$HEAD_TREE" "$G/index" | while IFS= read -r path; do
            if path_matches "$path"; then printf '%s\n' "$path"; fi
        done
        ;;
    "worktree list --porcelain") printf 'worktree %s\n\n' "$TOP" ;;
    "add -A")
        set_specs
        stage
        ;;
    "add "*)
        set_specs "$@"
        stage
        ;;
    "commit "*)
        msg=""
        for i in "${!OPTS[@]}"; do
            case "${OPTS[$i]}" in -qm|-m) msg="${OPTS[$((i + 1))]}" ;; esac
        done
        [ -n "$msg" ] || EXIT=2 die "unmodelled commit form: $*"
        commit_index "$msg"
        ;;
    "revert --no-edit HEAD")
        h="$(head_sha)" || die "no HEAD to revert"
        p="$(parent_of "$h")"
        [ -n "$p" ] || EXIT=2 die "reverting a root commit is not modelled"
        while IFS= read -r path; do
            line="$(awk -F'\t' -v p="$path" '$2 == p { print $1 }' "$G/commits/$p/tree")"
            if [ -n "$line" ]; then
                mkdir -p "$(dirname "$TOP/$path")"
                cp "$G/objects/$line" "$TOP/$path"
            else
                rm -f "$TOP/$path"
            fi
        done < "$G/commits/$h/changed"
        reverted=()
        while IFS= read -r path; do reverted+=("$path"); done < "$G/commits/$h/changed"
        set_specs "${reverted[@]}"
        stage
        commit_index "Revert \"$(cat "$G/commits/$h/msg")\""
        ;;
    "status --porcelain"*)
        [ "${OPTS[*]}" = "--porcelain" ] || EXIT=2 die "unmodelled status form: $*"
        # One pass over three tagged snapshots: a path is changed when the
        # working tree, the index and HEAD do not all name the same blob.
        {
            worktree_snapshot | filter_snapshot W
            filter_snapshot I < "$G/index"
            head_tree | filter_snapshot H
        } | awk -F'\t' '
            { blob[$1, $3] = $2; seen[$3] = 1 }
            END {
                for (p in seen)
                    if (blob["W", p] != blob["I", p] || blob["I", p] != blob["H", p])
                        print " M " p
            }' | LC_ALL=C sort
        ;;
    "ls-files -s"*)
        [ "${OPTS[*]}" = "-s" ] || EXIT=2 die "unmodelled ls-files form: $*"
        while IFS=$'\t' read -r blob path; do
            if path_matches "$path"; then printf '100644 %s 0\t%s\n' "$blob" "$path"; fi
        done < "$G/index"
        ;;
    "log -1 --format="*)
        [ "${#OPTS[@]}" -eq 2 ] || EXIT=2 die "unmodelled log form: $*"
        fmt="${OPTS[1]#--format=}"
        sha="$(head_sha || true)"
        while [ -n "$sha" ]; do
            if [ "${#SPECS[@]}" -eq 0 ] || commit_touches "$sha"; then
                format_commit "$fmt" "$sha"
                break
            fi
            IFS= read -r sha < "$G/commits/$sha/parent" || sha=""
        done
        ;;
    "show -s --format=%ct "*)
        [ "${#OPTS[@]}" -eq 3 ] || EXIT=2 die "unmodelled show form: $*"
        sha="$(resolve "${OPTS[2]}")" || die "bad object ${OPTS[2]}"
        first_line "$G/commits/$sha/ct"
        ;;
    "rev-list --count "*)
        [ "${#OPTS[@]}" -eq 2 ] || EXIT=2 die "unmodelled rev-list form: $*"
        range="${OPTS[1]}"
        from="$(resolve "${range%%..*}")" || die "bad revision '${range%%..*}'"
        sha="$(resolve "${range#*..}")" || die "bad revision '${range#*..}'"
        count=0
        while [ -n "$sha" ] && [ "$sha" != "$from" ]; do
            if [ "${#SPECS[@]}" -eq 0 ] || commit_touches "$sha"; then count=$((count + 1)); fi
            IFS= read -r sha < "$G/commits/$sha/parent" || sha=""
        done
        printf '%s\n' "$count"
        ;;
    *)
        EXIT=2 die "unmodelled invocation: $CMD $*"
        ;;
esac
