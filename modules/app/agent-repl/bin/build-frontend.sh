#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# The four checks above are OPT-IN (`shellcheck -o all`) and are declined
# deliberately, not overlooked. The file is clean at shellcheck's real bar
# (default plus `-S style`) with no suppressions at all.
#   SC2250 (${x} braces) / SC2292 (prefer [[ ]]) — cosmetic house-style
#     opinions. Applying them would rewrite ~170 sites across code this change
#     never touches, for no defect reduction.
#   SC2312 (command substitution masks a return value) — fires on idioms like
#     "$(basename "$entry")" where the substitution genuinely cannot fail in a
#     way this script should abort on.
#   SC2310 (a function in an `if` disables set -e) — that is the POINT of
#     is_stale: it is a predicate whose false answer means "fresh", not an
#     error. Making it abort the script would invert the staleness logic.
# build-frontend.sh — build the claude-repl frontend artifacts, but only
# when they are out of date ("build-if-stale").
#
# Five artifacts are managed, each independently:
#   1. shim    — TypeScript, built with `npm run build` ->
#               agent-shim/claude/shim/dist/main.js
#   2. webapp  — TypeScript + Vite, built with `npm run build` -> webapp/dist/index.html
#   3. daemon  — Go, built with `go build` -> daemon/bin/claude-repld
#   4. store   — Go, built with `go build` -> ~/.cache/agent-repl/bin/shim-store
#   5. sidecar — Go, built with `go build` ->
#               ~/.cache/agent-repl/bin/shim-claude-sidecar
#
# Staleness rule (per artifact): rebuild iff the artifact is missing, or any
# source file under its source set is newer (mtime) than the artifact. This is
# the same prerequisite-newer-than-target rule `make` uses, done by hand so no
# Makefile is required and so Emacs can invoke a single entrypoint.
#
# Every successful build also writes a `.built-sha` stamp beside its artifact
# (dist/.built-sha, daemon/bin/.built-sha, ~/.cache/agent-repl/bin/.<name>.built-sha)
# recording the source revision it was compiled from, which is what
# readiness-report.sh reads to say how far behind master a deployed artifact
# is. A skipped ("fresh") build leaves the stamp untouched. See
# lib-deploy-stamp.sh — and note these are NOT the `.<name>.deployed`
# fingerprint stamps deploy-all.sh uses for bounce detection.
#
# Exit codes:
#   0  every artifact is fresh or was rebuilt successfully
#   1  a build step failed (message on stderr)
#   2  a required toolchain binary is missing (message on stderr)
#
# Node dependencies live in a SHARED store outside the checkout
# ($AGENT_REPL_NODE_STORE, default ~/.cache/agent-repl/node-store), keyed by the
# hash of the project's lockfile, and each checkout's node_modules is a symlink
# into it. Every worktree therefore has deps the moment it is created (no
# per-worktree `npm install`, no per-worktree 100MB tree), and a lockfile change
# transparently keys a fresh store entry.
#
# STORE COLLECTION. Because a lockfile change keys a NEW entry and nothing ever
# removed the old one, the store grew without bound: entries for lockfiles no
# checkout references any more sat there forever (a webapp entry is ~60-90MB).
# `gc` sweeps them. An entry is COLLECTABLE only when all three hold:
#
#   1. No worktree's CURRENT lockfile hashes to it. Every worktree is
#      enumerated (`git worktree list`), not just this one — the store is
#      shared, and collecting on behalf of one checkout would strand 300 others.
#   2. No worktree's node_modules SYMLINK resolves to it. This is the
#      safety-critical half of the rule and does not follow from (1): a
#      worktree whose lockfile changed but which has not rebuilt yet is still
#      POINTING at the old entry, and deleting it would break that worktree's
#      deps without warning.
#   3. It is older than the grace window ($AGENT_REPL_NODE_STORE_GRACE_MINS,
#      default 60). This covers an entry being populated RIGHT NOW by a
#      concurrent build whose worktree we failed to enumerate for any reason.
#
# The sweep takes an exclusive lock (an atomic mkdir, since flock is not
# portable to macOS) and SKIPS rather than waits when another sweep holds it:
# collection is opportunistic and must never delay a build.
#
# Usage:
#   build-frontend.sh [--force] [--dry-run] [-v]
#                     [shim|webapp|daemon|store|sidecar|deps|gc ...]
#     --force            rebuild the selected artifacts unconditionally
#     --dry-run          gc only: report what WOULD be collected, delete nothing
#     -v, --verbose      gc only: also report each entry KEPT and why
#     deps               only link node_modules at the shared store (no build)
#     gc                 sweep unreferenced store entries and exit
#     positional targets restrict the run to the named artifacts (default: all)
#
# A sweep also runs automatically at the end of a successful run that MINTED a
# new store entry — the only moment the store can have just grown, and so the
# natural trigger. Runs that mint nothing never pay the enumeration cost.
#
# Env:
#   AGENT_REPL_NODE_STORE             store root (default ~/.cache/agent-repl/node-store)
#   AGENT_REPL_NODE_STORE_GRACE_MINS  min age before an entry may be collected (default 60)
#   AGENT_REPL_WORKTREE_ROOTS         newline-separated worktree roots, overriding
#                                     `git worktree list`. For tests and for CI
#                                     checkouts that are not git worktrees.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"

# shellcheck source=lib-deploy-stamp.sh
. "$THIS_DIR/lib-deploy-stamp.sh"

SHIM_DIR="$ROOT/agent-shim/claude/shim"
WEBAPP_DIR="$ROOT/webapp"
DAEMON_DIR="$ROOT/daemon"
STORE_DIR="$ROOT/agent-shim/shim-store"
SIDECAR_DIR="$ROOT/agent-shim/claude/shim-sidecar"
WIRE_DIR="$ROOT/agent-shim/wire"
SHARED_LOGGING_DIR="$ROOT/agent-shim/logging/go"
PROTO_GO_DIR="$ROOT/proto/gen/go"

# Where ROOT sits inside its checkout, so the same relative paths can be
# applied to OTHER worktrees when building the gc protection set. Derived
# rather than hardcoded as "modules/app/agent-repl": the layout is the repo's
# to change, and a stale hardcoded path would silently protect nothing.
if command -v git >/dev/null 2>&1 &&
       WORKTREE_ROOT="$(git -C "$ROOT" rev-parse --show-toplevel 2>/dev/null)"; then
    REL_ROOT="${ROOT#"$WORKTREE_ROOT"/}"
    [ "$REL_ROOT" = "$ROOT" ] && REL_ROOT=""
else
    WORKTREE_ROOT="$ROOT"
    REL_ROOT=""
fi
REL_SHIM="${REL_ROOT:+$REL_ROOT/}agent-shim/claude/shim"
REL_WEBAPP="${REL_ROOT:+$REL_ROOT/}webapp"

NODE_STORE="${AGENT_REPL_NODE_STORE:-${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl/node-store}"

# The shim artifact is the esbuild SINGLE-FILE bundle (`npm run build` ->
# build.mjs). It stays at dist/main.js — the exact entry the daemon (daemon.el)
# and the e2e harness spawn — so the bundle IS the spawned shim without a path
# change on the (frozen) daemon side. The bundle inlines @bufbuild/protobuf,
# which the committed out-of-package proto stubs cannot resolve at runtime; a
# plain tsc emit both breaks that resolution and lands under a deep rootDir path.
SHIM_ARTIFACT="$SHIM_DIR/dist/main.js"
WEBAPP_ARTIFACT="$WEBAPP_DIR/dist/index.html"
DAEMON_ARTIFACT="$DAEMON_DIR/bin/claude-repld"
CACHE_BIN="$HOME/.cache/agent-repl/bin"
STORE_ARTIFACT="$CACHE_BIN/shim-store"
SIDECAR_ARTIFACT="$CACHE_BIN/shim-claude-sidecar"

# Built-sha stamps, written beside each artifact after a SUCCESSFUL build so
# readiness-report.sh can say which source revision the deployed artifact is
# compiled from. A skipped ("fresh") build leaves the existing stamp alone —
# the artifact did not change, so neither did the revision it came from.
#
# The two service stamps sit next to the pre-existing `.<name>.deployed`
# fingerprint stamps but are a different family entirely; see
# lib-deploy-stamp.sh. Never write one from the other's code path.
SHIM_SHA_STAMP="$SHIM_DIR/dist/.built-sha"
WEBAPP_SHA_STAMP="$WEBAPP_DIR/dist/.built-sha"
DAEMON_SHA_STAMP="$DAEMON_DIR/bin/.built-sha"
STORE_SHA_STAMP="$CACHE_BIN/.shim-store.built-sha"
SIDECAR_SHA_STAMP="$CACHE_BIN/.shim-claude-sidecar.built-sha"

GRACE_MINS="${AGENT_REPL_NODE_STORE_GRACE_MINS:-60}"

FORCE=0
DRY_RUN=0
VERBOSE=0
# Set by link_node_modules when it populates an entry: the store just grew, so
# a sweep is warranted. A run that mints nothing skips the sweep entirely.
MINTED_ENTRY=0
TARGETS=()

while [ $# -gt 0 ]; do
    case "$1" in
        --force) FORCE=1 ;;
        --dry-run) DRY_RUN=1 ;;
        -v|--verbose) VERBOSE=1 ;;
        shim|webapp|daemon|store|sidecar|deps|gc) TARGETS+=("$1") ;;
        -h|--help)
            sed -n '2,60p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "build-frontend.sh: unknown argument: $1" >&2
            exit 1
            ;;
    esac
    shift
done

# Default to all three, in dependency-agnostic order. `gc` is never implicit:
# it is either asked for, or triggered by a run that minted an entry.
if [ "${#TARGETS[@]}" -eq 0 ]; then
    TARGETS=(shim webapp daemon)
fi

require_bin() {
    # require_bin BIN HUMAN-HINT
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "build-frontend.sh: required binary '$1' not found on PATH ($2)" >&2
        exit 2
    fi
}

# newest_mtime DIR [FILE...] — echo the largest mtime (epoch seconds) among the
# given files plus every regular file under DIR/src, skipping node_modules.
# Emits 0 when nothing matches.
newest_mtime() {
    local newest=0 f mt
    for f in "$@"; do
        [ -e "$f" ] || continue
        mt="$(stat -f %m "$f" 2>/dev/null || stat -c %Y "$f")"
        [ "$mt" -gt "$newest" ] && newest="$mt"
    done
    echo "$newest"
}

# artifact_mtime FILE — echo the artifact's mtime, or 0 if it is missing (which
# forces a rebuild since any source mtime is >= 0).
artifact_mtime() {
    if [ -e "$1" ]; then
        stat -f %m "$1" 2>/dev/null || stat -c %Y "$1"
    else
        echo 0
    fi
}

# is_stale ARTIFACT SOURCE... — return 0 (stale, needs build) when ARTIFACT is
# missing or older than the newest SOURCE; return 1 (fresh) otherwise.
#
# Callers pass the source list as ${SOURCES[@]+"${SOURCES[@]}"}. The `+` guard
# is not decoration: bash 3.2 -- still /bin/bash on macOS, and this script runs
# under `set -u` -- aborts on "${arr[@]}" when arr is EMPTY, which is exactly
# the case for a project with no src/ and no manifests.
is_stale() {
    local artifact="$1"; shift
    [ "$FORCE" -eq 1 ] && return 0
    local a s
    a="$(artifact_mtime "$artifact")"
    s="$(newest_mtime "$@")"
    [ "$s" -ge "$a" ] || [ "$a" -eq 0 ]
}

# emit_sources DIR [EXTRA_FIND_ARGS...] — print DIR's complete source set,
# NUL-delimited: every regular file under DIR/src, the package/build manifests
# that also invalidate the artifact, and (when EXTRA_FIND_ARGS are given)
# everything `find DIR EXTRA_FIND_ARGS` matches.
#
# NUL rather than newline because this list is read back into an array and a
# path is the one thing that may contain any byte except NUL. The predecessor
# printed newline-separated paths that the caller then passed UNQUOTED, so the
# shell split them on IFS and glob-expanded the pieces: a source file named
# `my component.ts` reached the mtime scan as two nonexistent paths, both
# silently skipped, and edits to it never triggered a rebuild.
emit_sources() {
    local dir="$1"; shift
    if [ -d "$dir/src" ]; then
        find "$dir/src" -type f -print0
    fi
    local manifest
    for manifest in package.json tsconfig.json vite.config.ts go.mod go.sum build.mjs; do
        if [ -f "$dir/$manifest" ]; then
            printf '%s\0' "$dir/$manifest"
        fi
    done
    if [ "$#" -gt 0 ]; then
        find "$dir" "$@" -print0
    fi
    return 0
}

# SOURCES — the array load_sources fills, consumed immediately by its caller.
# A global because bash 3.2 (still /bin/bash on macOS) has no namerefs.
SOURCES=()

# load_sources DIR [EXTRA_FIND_ARGS...] — populate SOURCES from emit_sources,
# one array element per path however awkward the filename.
load_sources() {
    SOURCES=()
    local f
    while IFS= read -r -d '' f; do
        SOURCES+=("$f")
    done < <(emit_sources "$@")
}

# store_key DIR — echo a short content hash of DIR's dependency manifest, so a
# lockfile (or, absent one, package.json) change keys a different store entry.
store_key() {
    local manifest="$1/package-lock.json"
    [ -f "$manifest" ] || manifest="$1/package.json"
    { shasum -a 256 "$manifest" 2>/dev/null || sha256sum "$manifest"; } | cut -c1-16
}

# link_node_modules DIR NAME — point DIR/node_modules at the shared store entry
# for NAME, installing that entry once (for every worktree, forever) when it is
# not there yet. The store holds only the manifests plus node_modules, so the
# install is a pure dependency fetch, independent of the checkout it serves.
# Deps that already resolve (a real directory, or a symlink into a populated
# store) are left exactly as they are — including a deliberate local install.
link_node_modules() {
    local dir="$1" name="$2" entry
    [ -d "$dir/node_modules" ] && return 0
    entry="$NODE_STORE/$name-$(store_key "$dir")"
    if [ ! -d "$entry/node_modules" ]; then
        require_bin npm "install Node.js"
        echo "[build-frontend] $name: populating shared dep store $entry"
        # The store just grew: this run is the one that should sweep.
        MINTED_ENTRY=1
        mkdir -p "$entry"
        cp "$dir/package.json" "$entry/package.json"
        if [ -f "$dir/package-lock.json" ]; then
            cp "$dir/package-lock.json" "$entry/package-lock.json"
            ( cd "$entry" && npm ci )
        else
            ( cd "$entry" && npm install )
        fi
    fi
    ln -sfn "$entry/node_modules" "$dir/node_modules"
    echo "[build-frontend] $name: node_modules -> $entry/node_modules"
}

# ---------------------------------------------------------------------------
# Store collection
# ---------------------------------------------------------------------------

# entry_size_kb DIR — echo DIR's apparent size in KB (0 when it is gone).
entry_size_kb() {
    [ -d "$1" ] || { echo 0; return; }
    du -sk "$1" 2>/dev/null | awk '{print $1}'
}

# human_kb KB — render a KB count as a rounded MB figure for the log.
human_kb() {
    awk -v kb="$1" 'BEGIN { printf "%.1fMB", kb / 1024 }'
}

# worktree_roots — print every checkout that shares this store, one per line.
#
# `git worktree list` is the authority: this script serves hundreds of
# worktrees off one store, and a protection set built from only the invoking
# checkout would collect entries every other worktree still depends on. The
# AGENT_REPL_WORKTREE_ROOTS override exists for tests and for CI checkouts that
# are not worktrees at all; when neither is available we fall back to THIS
# checkout alone, which is the conservative direction (it protects less, so the
# grace window and symlink rules below carry the safety).
worktree_roots() {
    if [ -n "${AGENT_REPL_WORKTREE_ROOTS:-}" ]; then
        printf '%s\n' "$AGENT_REPL_WORKTREE_ROOTS"
        return 0
    fi
    if command -v git >/dev/null 2>&1 &&
           git -C "$ROOT" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        git -C "$ROOT" worktree list --porcelain 2>/dev/null |
            awk '/^worktree /{ $1=""; sub(/^ /,""); print }'
        return 0
    fi
    printf '%s\n' "$WORKTREE_ROOT"
}

# protected_keys — print every store entry name that must NOT be collected.
#
# Two independent sources, and the second is the one that keeps this safe:
#   - the entry each worktree's CURRENT lockfile hashes to, and
#   - the entry each worktree's node_modules symlink actually RESOLVES to.
# They differ exactly when a worktree's lockfile changed and it has not rebuilt
# since; that worktree is still using the old entry, so hashing alone would
# happily delete the deps out from under it.
protected_keys() {
    local wt dir name manifest target
    while read -r wt; do
        [ -n "$wt" ] || continue
        for rel in "$REL_SHIM:shim" "$REL_WEBAPP:webapp"; do
            dir="$wt/${rel%%:*}"
            name="${rel##*:}"
            [ -d "$dir" ] || continue
            manifest="$dir/package-lock.json"
            [ -f "$manifest" ] || manifest="$dir/package.json"
            [ -f "$manifest" ] && echo "$name-$(store_key "$dir")"
            # The symlink's real target, whatever the lockfile now says.
            if [ -L "$dir/node_modules" ]; then
                target="$(readlink "$dir/node_modules" 2>/dev/null || true)"
                # <store>/<entry>/node_modules -> <entry>
                [ -n "$target" ] && basename "$(dirname "$target")"
            fi
        done
    done < <(worktree_roots)
}

# gc_store — sweep unreferenced entries. Never fails the build: a store that
# cannot be swept is a housekeeping problem, not a build problem.
gc_store() {
    [ -d "$NODE_STORE" ] || { echo "[build-frontend] gc: no store at $NODE_STORE"; return 0; }

    # Exclusive, non-blocking, and portable: `mkdir` is atomic everywhere,
    # whereas flock(1) does not exist on macOS. Skipping (not waiting) is
    # deliberate — a sweep is opportunistic and must never delay a build.
    local lock="$NODE_STORE/.gc.lock"
    if ! mkdir "$lock" 2>/dev/null; then
        # Break a lock left behind by a killed sweep, but only once it is far
        # older than any sweep could legitimately run for.
        if [ -n "$(find "$lock" -maxdepth 0 -mmin +60 2>/dev/null)" ]; then
            echo "[build-frontend] gc: breaking stale lock $lock"
            rm -rf "$lock"
            mkdir "$lock" 2>/dev/null || { echo "[build-frontend] gc: lock held, skipping sweep"; return 0; }
        else
            echo "[build-frontend] gc: another sweep holds the lock, skipping"
            return 0
        fi
    fi
    echo "$$" > "$lock/pid" 2>/dev/null || true
    trap 'rm -rf "$lock"' RETURN

    local keep_file; keep_file="$(mktemp)"
    protected_keys | sort -u > "$keep_file"
    local kept_count; kept_count="$(grep -c . "$keep_file" || true)"
    echo "[build-frontend] gc: $kept_count referenced entr$([ "$kept_count" = 1 ] && echo y || echo ies) protected (grace ${GRACE_MINS}m, dry-run=$DRY_RUN)"

    local entry base freed_kb=0 removed=0 size
    for entry in "$NODE_STORE"/*; do
        [ -d "$entry" ] || continue
        base="$(basename "$entry")"
        [ "$base" = ".gc.lock" ] && continue

        if grep -qxF "$base" "$keep_file"; then
            [ "$VERBOSE" -eq 1 ] && echo "[build-frontend] gc: keep $base (referenced)"
            continue
        fi
        # Young entries are presumed in-flight: a concurrent build may be
        # populating this very directory right now.
        if [ -z "$(find "$entry" -maxdepth 0 -mmin +"$GRACE_MINS" 2>/dev/null)" ]; then
            [ "$VERBOSE" -eq 1 ] && echo "[build-frontend] gc: keep $base (younger than ${GRACE_MINS}m grace)"
            continue
        fi

        size="$(entry_size_kb "$entry")"
        if [ "$DRY_RUN" -eq 1 ]; then
            echo "[build-frontend] gc: WOULD collect $base ($(human_kb "$size"))"
        else
            echo "[build-frontend] gc: collecting $base ($(human_kb "$size"))"
            rm -rf "$entry"
        fi
        freed_kb=$((freed_kb + size))
        removed=$((removed + 1))
    done

    rm -f "$keep_file"
    if [ "$removed" -eq 0 ]; then
        echo "[build-frontend] gc: nothing to collect"
    elif [ "$DRY_RUN" -eq 1 ]; then
        echo "[build-frontend] gc: would free $(human_kb "$freed_kb") across $removed entr$([ "$removed" = 1 ] && echo y || echo ies)"
    else
        echo "[build-frontend] gc: freed $(human_kb "$freed_kb") across $removed entr$([ "$removed" = 1 ] && echo y || echo ies)"
    fi
    return 0
}

build_deps() {
    link_node_modules "$SHIM_DIR" shim
    link_node_modules "$WEBAPP_DIR" webapp
}

build_shim() {
    link_node_modules "$SHIM_DIR" shim
    load_sources "$SHIM_DIR"
    if ! is_stale "$SHIM_ARTIFACT" ${SOURCES[@]+"${SOURCES[@]}"}; then
        echo "[build-frontend] shim: fresh, skipping"
        return 0
    fi
    require_bin npm "install Node.js"
    echo "[build-frontend] shim: building..."
    # ONE revision, baked into the bundle AND written to the stamp.
    #
    # The daemon bounces a surviving shim whose reported build identity differs
    # from this stamp, so the two must be the same value rather than two
    # computations that usually agree: `source_revision` reports `-dirty` off a
    # live working tree, which a build can enter and leave between the bundle
    # and the stamp. Computing it once removes the question.
    local shim_sha=""
    shim_sha="$(source_revision "$ROOT" || true)"
    ( cd "$SHIM_DIR" && SHIM_BUILD_SHA="$shim_sha" npm run build )
    write_built_sha_value "$SHIM_SHA_STAMP" "$shim_sha"
    echo "[build-frontend] shim: done"
}

build_webapp() {
    link_node_modules "$WEBAPP_DIR" webapp
    load_sources "$WEBAPP_DIR"
    if ! is_stale "$WEBAPP_ARTIFACT" ${SOURCES[@]+"${SOURCES[@]}"}; then
        echo "[build-frontend] webapp: fresh, skipping"
        return 0
    fi
    require_bin npm "install Node.js"
    echo "[build-frontend] webapp: building..."
    ( cd "$WEBAPP_DIR" && npm run build )
    write_built_sha "$WEBAPP_SHA_STAMP" "$ROOT"
    echo "[build-frontend] webapp: done"
}

build_daemon() {
    # The daemon's set is its manifests plus every .go file in the tree.
    load_sources "$DAEMON_DIR" -name '*.go'
    if ! is_stale "$DAEMON_ARTIFACT" ${SOURCES[@]+"${SOURCES[@]}"}; then
        echo "[build-frontend] daemon: fresh, skipping"
        return 0
    fi
    require_bin go "install the Go toolchain"
    echo "[build-frontend] daemon: building..."
    mkdir -p "$DAEMON_DIR/bin"
    ( cd "$DAEMON_DIR" && go build -o "$DAEMON_ARTIFACT" ./cmd/claude-repld )
    write_built_sha "$DAEMON_SHA_STAMP" "$ROOT"
    echo "[build-frontend] daemon: done"
}

load_service_sources() {
    # Both services compile against repo-local proto + wire + logging modules.
    # Their generated/source files are real prerequisites even though they live
    # outside the service module, so a proto, wire or logging edit must stale
    # the installed launchd binary.
    local dir="$1" source_dir f
    SOURCES=()
    for source_dir in "$dir" "$WIRE_DIR" "$SHARED_LOGGING_DIR" "$PROTO_GO_DIR"; do
        if [ ! -d "$source_dir" ]; then
            echo "build-frontend.sh: required service source directory missing: $source_dir" >&2
            exit 1
        fi
    done
    while IFS= read -r -d '' f; do
        SOURCES+=("$f")
    done < <(find "$dir" "$WIRE_DIR" "$SHARED_LOGGING_DIR" "$PROTO_GO_DIR" -type f \
             \( -name '*.go' -o -name 'go.mod' -o -name 'go.sum' \) -print0)
}

build_service() {
    # NAME MODULE-DIR ARTIFACT — shared build-if-stale path for the two Go
    # launchd services.  Keeping one helper prevents their source sets and
    # install semantics from drifting.
    local name="$1" dir="$2" artifact="$3" sha_stamp="$4"
    load_service_sources "$dir"
    if ! is_stale "$artifact" ${SOURCES[@]+"${SOURCES[@]}"}; then
        echo "[build-frontend] $name: fresh, skipping"
        return 0
    fi
    require_bin go "install the Go toolchain"
    echo "[build-frontend] $name: building..."
    mkdir -p "$CACHE_BIN"
    ( cd "$dir" && go build -o "$artifact" . )
    write_built_sha "$sha_stamp" "$ROOT"
    echo "[build-frontend] $name: done"
}

EXPLICIT_GC=0
for target in "${TARGETS[@]}"; do
    case "$target" in
        deps)   build_deps ;;
        shim)   build_shim ;;
        webapp) build_webapp ;;
        daemon) build_daemon ;;
        store)  build_service shim-store "$STORE_DIR" "$STORE_ARTIFACT" "$STORE_SHA_STAMP" ;;
        sidecar) build_service shim-claude-sidecar "$SIDECAR_DIR" "$SIDECAR_ARTIFACT" "$SIDECAR_SHA_STAMP" ;;
        gc)     EXPLICIT_GC=1 ;;
        # Unreachable: the argument parser allowlists these same names. It is
        # here so that adding a target THERE and forgetting it here fails
        # loudly instead of silently doing nothing.
        *)
            echo "build-frontend.sh: internal error: unhandled target '$target'" >&2
            exit 1
            ;;
    esac
done

# Sweep on the natural trigger: an explicit `gc`, or a successful run that just
# minted an entry (the only moment the store can have grown). Reaching here at
# all means every requested target succeeded — `set -e` would have aborted
# otherwise — so a sweep never runs on the back of a failed build.
if [ "$EXPLICIT_GC" -eq 1 ] || [ "$MINTED_ENTRY" -eq 1 ]; then
    gc_store
fi
