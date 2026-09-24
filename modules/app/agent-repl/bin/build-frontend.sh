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
#     source_is_stale: it is a predicate whose false answer means "fresh", not
#     an error. Making it abort the script would invert the staleness logic.
# build-frontend.sh — build the claude-repl frontend artifacts, but only
# when they are out of date ("build-if-stale").
#
# WHY THIS SCRIPT IS KEPT. The daemon owns deploys now (daemon/internal/deploy),
# but this stays the ONE build step, with two callers:
#   - Emacs's cold start runs it IN PLACE, because it must build the daemon
#     before any daemon exists to build anything;
#   - the daemon's deploy runs it in STAGING mode (`--out DIR`, below), after
#     `make -C proto all`, then hashes the staged artifacts and installs them
#     into their live locations only when every build succeeded.
# One script for both means the cold start and the deploy can never build an
# artifact two different ways.
#
# Five artifacts are managed, each independently:
#   1. shim    — TypeScript, built with `npm run build` ->
#               agent-shim/claude/shim/dist/main.js
#   2. webapp  — TypeScript + Vite, built with `npm run build` -> webapp/dist/index.html
#   3. daemon  — Go, built with `go build` -> daemon/bin/claude-repld
#   4. store   — Go, built with `go build` -> ~/.cache/agent-repl/bin/shim-store
#   5. sidecar — Go, built with `go build` ->
#               ~/.cache/agent-repl/bin/shim-claude-sidecar
#   6. lock    — Go, built with `go build` ->
#               ~/.cache/agent-repl/bin/shim-lock. The shim spawns it to hold
#               each kernel claim, because Node cannot take a flock, so a shim
#               without it refuses every session — which is why `lock` is in
#               the DEFAULT target set and store/sidecar are not.
#
# Staleness rule (per artifact): rebuild iff the artifact is missing, or the
# SOURCE REVISION it was built from is not the source revision standing here
# now. "Source revision" is the hash of the git index entries for that
# system's pathspec — the same pathspec readiness-report.sh attributes to the
# system, taken from the one table in lib-deploy-stamp.sh — and a working tree
# that is dirty anywhere under it always reads as stale.
#
# It is NOT an mtime comparison any more. That rule shipped two ways to call a
# stale artifact fresh and on 2026-09-09 a real deploy hit both: the hand-listed
# source set was narrower than the readiness report's, so merges landing under
# `shim/test/` and `agent-shim/logging/` were invisible to it, and an mtime is
# wall-clock metadata a checkout or a copy can set to anything. The full
# reasoning is at "family 3" in lib-deploy-stamp.sh.
#
# Every successful build also writes a `.built-sha` stamp beside its artifact
# (dist/.built-sha, daemon/bin/.built-sha, ~/.cache/agent-repl/bin/.<name>.built-sha)
# recording the source revision it was compiled from, which is what
# readiness-report.sh reads to say how far behind master a deployed artifact
# is. A skipped ("fresh") build leaves the stamp untouched. See
# lib-deploy-stamp.sh — and note that bounce detection is a different question
# entirely, answered by each launchd service's own build report (family 2
# there), never by these stamps.
#
# STAGING MODE (`--out DIR`). THIS LAYOUT IS A CONTRACT the daemon's deploy
# (daemon/internal/deploy) depends on; change it only together with that code.
# Every selected target is BUILT unconditionally (the staging dir is fresh, so
# the staleness stamps are not consulted), and nothing in the live artifact
# locations is written. node_modules linking into the shared node store and
# the store gc behave exactly as in place. DIR must be absolute; it is created
# if absent. Layout, per target:
#   shim    -> DIR/agent-shim/claude/shim/dist/main.js
#              (+ .built-sha, .source-tree in that dist dir)
#   webapp  -> DIR/webapp/dist/  (the whole Vite dist)
#              (+ .built-sha, .source-tree, .build-id in it)
#   daemon  -> DIR/daemon/bin/claude-repld  (+ .built-sha, .source-tree beside it)
#   store   -> DIR/cache-bin/shim-store
#   sidecar -> DIR/cache-bin/shim-claude-sidecar
#   lock    -> DIR/cache-bin/shim-lock
#              (each + .<name>.built-sha, .<name>.source-tree in DIR/cache-bin/)
#
# Every `go build` passes -buildvcs=false; see GO_BUILD_FLAGS below.
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
# THE STORE HEALS ITSELF. An entry is judged by whether it satisfies its own
# lockfile, never by whether it exists: an entry that exists but is broken
# (emptied through a link, left partial) is repaired in place under a per-entry
# lock and swapped in atomically. lib-node-store.sh holds that one repair path,
# shared with ensure-deps.sh; see link_node_modules below.
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
#   build-frontend.sh [--force] [--dry-run] [-v] [--out DIR]
#                     [shim|webapp|daemon|store|sidecar|lock|deps|gc ...]
#     --force            rebuild the selected artifacts unconditionally
#     --out DIR          staging mode: build every selected target into the
#                        layout above under the ABSOLUTE DIR, never in place
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
# shellcheck source=lib-node-store.sh
. "$THIS_DIR/lib-node-store.sh"

SHIM_DIR="$ROOT/agent-shim/claude/shim"
WEBAPP_DIR="$ROOT/webapp"
DAEMON_DIR="$ROOT/daemon"
STORE_DIR="$ROOT/agent-shim/shim-store"
SIDECAR_DIR="$ROOT/agent-shim/claude/shim-sidecar"
LOCK_DIR="$ROOT/agent-shim/shim-lock"
SHARED_LOGGING_DIR="$ROOT/agent-shim/logging/go"
PROTO_GO_DIR="$ROOT/proto/gen/go"

# Where ROOT sits inside its checkout, so the same relative paths can be
# applied to OTHER worktrees when building the gc protection set. Derived
# rather than hardcoded as "modules/app/agent-repl": the layout is the repo's
# to change, and a stale hardcoded path would silently protect nothing.
if command -v git >/dev/null 2>&1 &&
       WORKTREE_ROOT="$(git -C "$ROOT" rev-parse --show-toplevel 2>/dev/null)"; then
    REL_ROOT="$(deploy_stamp_rel_root "$WORKTREE_ROOT" "$ROOT")"
else
    WORKTREE_ROOT="$ROOT"
    REL_ROOT=""
fi
REL_SHIM="${REL_ROOT:+$REL_ROOT/}agent-shim/claude/shim"
REL_WEBAPP="${REL_ROOT:+$REL_ROOT/}webapp"

NODE_STORE="$(node_store_root)"

GRACE_MINS="${AGENT_REPL_NODE_STORE_GRACE_MINS:-60}"

FORCE=0
DRY_RUN=0
VERBOSE=0
# Staging mode's root; empty means build in place. See STAGING MODE above.
OUT_DIR=""
# Set by link_node_modules when it populates an entry: the store just grew, so
# a sweep is warranted. A run that mints nothing skips the sweep entirely.
MINTED_ENTRY=0
TARGETS=()

while [ $# -gt 0 ]; do
    case "$1" in
        --force) FORCE=1 ;;
        --dry-run) DRY_RUN=1 ;;
        -v|--verbose) VERBOSE=1 ;;
        --out)
            if [ $# -lt 2 ] || [ -z "$2" ]; then
                echo "build-frontend.sh: --out needs a directory" >&2
                exit 1
            fi
            OUT_DIR="$2"
            shift
            ;;
        shim|webapp|daemon|store|sidecar|lock|deps|gc) TARGETS+=("$1") ;;
        -h|--help)
            # The whole header comment, however long it grows.
            sed -n '2,/^set -euo pipefail/p' "${BASH_SOURCE[0]}" | sed '$d' | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "build-frontend.sh: unknown argument: $1" >&2
            exit 1
            ;;
    esac
    shift
done

# A relative staging dir would resolve against whatever cwd the caller had,
# and the daemon's deploy then reads the artifacts from a path it did not
# mean. Refuse it rather than guess.
if [ -n "$OUT_DIR" ]; then
    case "$OUT_DIR" in
        /*) ;;
        *)
            echo "build-frontend.sh: --out must be an absolute directory, got: $OUT_DIR" >&2
            exit 1
            ;;
    esac
    mkdir -p "$OUT_DIR"
fi

# The shim artifact is the esbuild SINGLE-FILE bundle (`npm run build` ->
# build.mjs). It stays at dist/main.js — the exact entry the daemon and the e2e
# harness spawn — so the bundle IS the spawned shim without a path change on
# the daemon side. The bundle inlines @bufbuild/protobuf,
# which the committed out-of-package proto stubs cannot resolve at runtime; a
# plain tsc emit both breaks that resolution and lands under a deep rootDir path.
#
# Where each artifact lands: its live location in place, or its slot in the
# staging layout under --out (the contract spelled out in the header).
if [ -n "$OUT_DIR" ]; then
    SHIM_DIST="$OUT_DIR/agent-shim/claude/shim/dist"
    WEBAPP_DIST="$OUT_DIR/webapp/dist"
    DAEMON_BIN="$OUT_DIR/daemon/bin"
    CACHE_BIN="$OUT_DIR/cache-bin"
else
    SHIM_DIST="$SHIM_DIR/dist"
    WEBAPP_DIST="$WEBAPP_DIR/dist"
    DAEMON_BIN="$DAEMON_DIR/bin"
    CACHE_BIN="$HOME/.cache/agent-repl/bin"
fi
SHIM_ARTIFACT="$SHIM_DIST/main.js"
WEBAPP_ARTIFACT="$WEBAPP_DIST/index.html"
DAEMON_ARTIFACT="$DAEMON_BIN/claude-repld"
STORE_ARTIFACT="$CACHE_BIN/shim-store"
SIDECAR_ARTIFACT="$CACHE_BIN/shim-claude-sidecar"
LOCK_ARTIFACT="$CACHE_BIN/shim-lock"

# Built-sha stamps, written beside each artifact after a SUCCESSFUL build so
# readiness-report.sh can say which source revision the deployed artifact is
# compiled from. A skipped ("fresh") build leaves the existing stamp alone —
# the artifact did not change, so neither did the revision it came from.
#
# Whether a launchd service RUNS the installed binary is a different question,
# answered by the service's own build report; see family 2 in
# lib-deploy-stamp.sh. These stamps never stand in for it.
SHIM_SHA_STAMP="$SHIM_DIST/.built-sha"
WEBAPP_SHA_STAMP="$WEBAPP_DIST/.built-sha"
# The webapp ARTIFACT's own identity, which is a different question from the
# source revision beside it: two builds of a dirty tree share one revision, and
# the cache key that identifies a build must not.
WEBAPP_BUILD_ID_STAMP="$WEBAPP_DIST/.build-id"
DAEMON_SHA_STAMP="$DAEMON_BIN/.built-sha"
STORE_SHA_STAMP="$CACHE_BIN/.shim-store.built-sha"
SIDECAR_SHA_STAMP="$CACHE_BIN/.shim-claude-sidecar.built-sha"
LOCK_SHA_STAMP="$CACHE_BIN/.shim-lock.built-sha"

# Source-tree stamps, beside each artifact and shaped exactly like the built-sha
# stamps. These are the staleness authority: what the artifact standing here was
# built from, compared against what the checkout says now. readiness-report.sh
# reads these same files, so the build and the gate cannot reach opposite
# answers about the same artifact.
SHIM_TREE_STAMP="$SHIM_DIST/.source-tree"
WEBAPP_TREE_STAMP="$WEBAPP_DIST/.source-tree"
DAEMON_TREE_STAMP="$DAEMON_BIN/.source-tree"
STORE_TREE_STAMP="$CACHE_BIN/.shim-store.source-tree"
SIDECAR_TREE_STAMP="$CACHE_BIN/.shim-claude-sidecar.source-tree"
LOCK_TREE_STAMP="$CACHE_BIN/.shim-lock.source-tree"

# Flags every `go build` here passes, in place and staged alike.
#
# -buildvcs=false: the deploy decides staleness by the CONTENT HASH of each
# binary, and Go's default VCS stamping embeds the commit and the dirty flag
# into the binary, so every commit would change every binary's hash and bounce
# every service for nothing. Nothing in this repo reads the VCS build info.
GO_BUILD_FLAGS=(-buildvcs=false)

# Default set, in dependency-agnostic order. `gc` is never implicit: it is
# either asked for, or triggered by a run that minted an entry.
#
# `lock` is in here and `store`/`sidecar` are not, because the three are not
# alike: the store and the sidecar are launchd SERVICES, deployed and bounced
# on their own cadence, while shim-lock is a dependency of the SHIM ITSELF —
# the shim spawns it for every session claim and refuses to start a session
# without it. Building the shim without it would deploy a bundle that cannot
# take a lock.
if [ "${#TARGETS[@]}" -eq 0 ]; then
    TARGETS=(shim webapp daemon lock)
fi

require_bin() {
    # require_bin BIN HUMAN-HINT
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "build-frontend.sh: required binary '$1' not found on PATH ($2)" >&2
        exit 2
    fi
}

# system_source_id NAME — echo the source-tree id of NAME's pathspec, or return
# 1 when it cannot be determined (no git, no checkout, a pathspec matching
# nothing). The pathspec table is shared with readiness-report.sh; see
# lib-deploy-stamp.sh.
system_source_id() {
    local paths
    paths="$(deploy_stamp_system_paths "$1" "$REL_ROOT")" || return 1
    # Deliberately unquoted: the table carries a space-separated pathspec list
    # and each element must reach git as its own argument.
    # shellcheck disable=SC2086
    source_tree_id "$WORKTREE_ROOT" $paths
}

# source_is_stale NAME ARTIFACT STAMP — return 0 (stale, needs build) and 1
# (fresh, skip).
#
# Stale when any of these holds, and every one of them is a case where calling
# the artifact fresh would be a guess:
#   - --force was asked for;
#   - this is a staging run (--out): the staging dir is fresh, so every
#     selected target is built and no stamp is consulted;
#   - the artifact is missing;
#   - the source revision cannot be determined at all;
#   - the working tree is dirty under the system's pathspec;
#   - no stamp records what the artifact was built from; or
#   - the recorded revision is not the one standing here now.
source_is_stale() {
    local name="$1" artifact="$2" stamp="$3" current recorded
    [ "$FORCE" -eq 1 ] && return 0
    [ -n "$OUT_DIR" ] && return 0
    [ -e "$artifact" ] || return 0
    current="$(system_source_id "$name")" || return 0
    if source_tree_is_dirty "$current"; then return 0; fi
    recorded="$(read_source_tree "$stamp")" || return 0
    [ "$recorded" = "$current" ] && return 1
    return 0
}

# stamp_source_tree NAME STAMP — record what a just-built artifact was built
# from. An id that cannot be determined removes the stamp rather than leaving a
# stale one, which keeps the next run honest: no stamp means stale.
stamp_source_tree() {
    write_source_tree "$2" "$(system_source_id "$1" || true)"
}

# store_key DIR — echo a short content hash of DIR's dependency manifest, so a
# lockfile (or, absent one, package.json) change keys a different store entry.
# The one derivation lives in lib-node-store.sh, shared with ensure-deps.sh.
store_key() {
    node_store_key "$1"
}

# link_node_modules DIR NAME — point DIR/node_modules at the shared store entry
# for NAME, making sure the entry it points at SATISFIES ITS LOCKFILE.
#
#   - A real directory is a deliberate local install and is left exactly as it
#     is.
#   - A link into the store has THAT entry checked (`npm ls` inside it) and,
#     when it exists but is broken — emptied or partial — repaired in place
#     under the entry's lock (lib-node-store.sh). The link itself is kept.
#   - Anything else (absent, dangling) is linked to the entry the current
#     lockfile keys, installing that entry once, for every worktree, when it
#     is not there yet.
#
# A repair that fails fails the build: a checkout linked to a tree that does
# not satisfy its lockfile would build against missing dependencies.
link_node_modules() {
    local dir="$1" name="$2" entry linked
    if [ -d "$dir/node_modules" ] && [ ! -L "$dir/node_modules" ]; then
        return 0
    fi
    if [ -d "$dir/node_modules" ] && linked="$(node_store_entry_of_link "$dir/node_modules")"; then
        require_bin npm "install Node.js"
        if ! node_store_repair "$linked" "$dir"; then
            echo "[build-frontend] $name: the store entry $linked is broken and could not be repaired" >&2
            return 1
        fi
        return 0
    fi
    [ -d "$dir/node_modules" ] && return 0
    entry="$NODE_STORE/$name-$(store_key "$dir")"
    require_bin npm "install Node.js"
    if [ ! -e "$entry/node_modules" ]; then
        echo "[build-frontend] $name: populating shared dep store $entry"
        # The store just grew: this run is the one that should sweep.
        MINTED_ENTRY=1
    fi
    if ! node_store_repair "$entry" "$dir"; then
        echo "[build-frontend] $name: the store entry $entry could not be installed" >&2
        return 1
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
    if ! source_is_stale shim "$SHIM_ARTIFACT" "$SHIM_TREE_STAMP"; then
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
    # SHIM_BUILD_OUTFILE is build.mjs's own output override: dist/main.js in
    # place, the staging slot under --out.
    ( cd "$SHIM_DIR" && SHIM_BUILD_SHA="$shim_sha" SHIM_BUILD_OUTFILE="$SHIM_ARTIFACT" npm run build )
    write_built_sha_value "$SHIM_SHA_STAMP" "$shim_sha"
    stamp_source_tree shim "$SHIM_TREE_STAMP"
    echo "[build-frontend] shim: done"
}

# write_webapp_build_id — stamp the built webapp with the identity of the
# artifact itself, taken from the content hash Vite already fingerprints its
# entry bundle with.
#
# THE WEBVIEW'S URL CARRIES THIS, because index.html is served from a fixed
# path and is the only thing naming which bundle to run. A stable URL is a
# stable cache key, so a client can go on answering out of its own cache with a
# bundle from an earlier build — including one whose file has since been
# deleted. Folding the artifact's identity into the URL makes each build a
# different address, which a cache cannot satisfy from an older one.
#
# It is taken from the artifact rather than from git on purpose: two builds of
# a dirty tree report one revision, and this must differ whenever the output
# does.
write_webapp_build_id() {
    local index="$WEBAPP_ARTIFACT" entry
    entry="$(sed -n 's|.*src="/assets/index-\([A-Za-z0-9_-]*\)\.js".*|\1|p' "$index" | head -1)"
    if [ -z "$entry" ]; then
        echo "[build-frontend] webapp: FAILED to read the entry bundle hash from $index" >&2
        return 1
    fi
    write_built_sha_value "$WEBAPP_BUILD_ID_STAMP" "$entry"
}

build_webapp() {
    link_node_modules "$WEBAPP_DIR" webapp
    if ! source_is_stale webapp "$WEBAPP_ARTIFACT" "$WEBAPP_TREE_STAMP"; then
        # The build-id describes the ARTIFACT, so a skipped build still owes it:
        # the artifact standing here is the one the webview must address, and a
        # stamp missing beside it would leave that address unbuildable.
        write_webapp_build_id
        echo "[build-frontend] webapp: fresh, skipping"
        return 0
    fi
    require_bin npm "install Node.js"
    echo "[build-frontend] webapp: building..."
    if [ -n "$OUT_DIR" ]; then
        # The whole dist goes to the staging slot. --emptyOutDir because Vite
        # refuses to empty an outDir outside the project root without it.
        ( cd "$WEBAPP_DIR" && npm run build -- --outDir "$WEBAPP_DIST" --emptyOutDir )
    else
        ( cd "$WEBAPP_DIR" && npm run build )
    fi
    write_built_sha "$WEBAPP_SHA_STAMP" "$ROOT"
    stamp_source_tree webapp "$WEBAPP_TREE_STAMP"
    write_webapp_build_id
    echo "[build-frontend] webapp: done"
}

build_daemon() {
    require_go_source_dirs "$DAEMON_DIR"
    if ! source_is_stale daemon "$DAEMON_ARTIFACT" "$DAEMON_TREE_STAMP"; then
        echo "[build-frontend] daemon: fresh, skipping"
        return 0
    fi
    require_bin go "install the Go toolchain"
    echo "[build-frontend] daemon: building..."
    mkdir -p "$DAEMON_BIN"
    ( cd "$DAEMON_DIR" && go build "${GO_BUILD_FLAGS[@]}" -o "$DAEMON_ARTIFACT" ./cmd/claude-repld )
    write_built_sha "$DAEMON_SHA_STAMP" "$ROOT"
    stamp_source_tree daemon "$DAEMON_TREE_STAMP"
    echo "[build-frontend] daemon: done"
}

# require_go_source_dirs MODULE_DIR — abort unless every directory a Go artifact
# here compiles against is actually present.
#
# The generated proto module and the shared logging module live OUTSIDE the
# module being built, and a checkout missing either produces a build failure
# that reads as a compiler problem. Failing here names the missing directory
# instead. It is a pre-flight assertion, not the staleness rule: staleness is
# the source-tree id, which covers these same trees through the shared pathspec.
require_go_source_dirs() {
    local dir="$1" source_dir
    for source_dir in "$dir" "$SHARED_LOGGING_DIR" "$PROTO_GO_DIR"; do
        if [ ! -d "$source_dir" ]; then
            echo "build-frontend.sh: required Go source directory missing: $source_dir" >&2
            exit 1
        fi
    done
}

build_service() {
    # NAME MODULE-DIR ARTIFACT SHA-STAMP TREE-STAMP — shared build-if-stale path
    # for the Go binaries that install into the shared cache bin. Keeping one
    # helper prevents their source sets and install semantics from drifting.
    local name="$1" dir="$2" artifact="$3" sha_stamp="$4" tree_stamp="$5"
    require_go_source_dirs "$dir"
    if ! source_is_stale "$name" "$artifact" "$tree_stamp"; then
        echo "[build-frontend] $name: fresh, skipping"
        return 0
    fi
    require_bin go "install the Go toolchain"
    echo "[build-frontend] $name: building..."
    mkdir -p "$CACHE_BIN"
    ( cd "$dir" && go build "${GO_BUILD_FLAGS[@]}" -o "$artifact" . )
    write_built_sha "$sha_stamp" "$ROOT"
    stamp_source_tree "$name" "$tree_stamp"
    echo "[build-frontend] $name: done"
}

EXPLICIT_GC=0
for target in "${TARGETS[@]}"; do
    case "$target" in
        deps)   build_deps ;;
        shim)   build_shim ;;
        webapp) build_webapp ;;
        daemon) build_daemon ;;
        store)  build_service shim-store "$STORE_DIR" "$STORE_ARTIFACT" "$STORE_SHA_STAMP" "$STORE_TREE_STAMP" ;;
        sidecar) build_service shim-claude-sidecar "$SIDECAR_DIR" "$SIDECAR_ARTIFACT" "$SIDECAR_SHA_STAMP" "$SIDECAR_TREE_STAMP" ;;
        lock)   build_service shim-lock "$LOCK_DIR" "$LOCK_ARTIFACT" "$LOCK_SHA_STAMP" "$LOCK_TREE_STAMP" ;;
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
