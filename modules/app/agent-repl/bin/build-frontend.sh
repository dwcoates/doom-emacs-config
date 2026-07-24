#!/usr/bin/env bash
# build-frontend.sh — build the claude-repl frontend artifacts, but only
# when they are out of date ("build-if-stale").
#
# Three artifacts are managed, each independently:
#   1. shim    — TypeScript, built with `npm run build` ->
#               agent-shim/claude/shim/dist/main.js
#   2. webapp  — TypeScript + Vite, built with `npm run build` -> webapp/dist/index.html
#   3. daemon  — Go, built with `go build` -> daemon/bin/claude-repld
#
# Staleness rule (per artifact): rebuild iff the artifact is missing, or any
# source file under its source set is newer (mtime) than the artifact. This is
# the same prerequisite-newer-than-target rule `make` uses, done by hand so no
# Makefile is required and so Emacs can invoke a single entrypoint.
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
# Usage:
#   build-frontend.sh [--force] [shim|webapp|daemon|deps ...]
#     --force            rebuild the selected artifacts unconditionally
#     deps               only link node_modules at the shared store (no build)
#     positional targets restrict the run to the named artifacts (default: all)

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"

SHIM_DIR="$ROOT/agent-shim/claude/shim"
WEBAPP_DIR="$ROOT/webapp"
DAEMON_DIR="$ROOT/daemon"

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

FORCE=0
TARGETS=()

while [ $# -gt 0 ]; do
    case "$1" in
        --force) FORCE=1 ;;
        shim|webapp|daemon|deps) TARGETS+=("$1") ;;
        -h|--help)
            sed -n '2,32p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "build-frontend.sh: unknown argument: $1" >&2
            exit 1
            ;;
    esac
    shift
done

# Default to all three, in dependency-agnostic order.
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
is_stale() {
    local artifact="$1"; shift
    [ "$FORCE" -eq 1 ] && return 0
    local a s
    a="$(artifact_mtime "$artifact")"
    s="$(newest_mtime "$@")"
    [ "$s" -ge "$a" ] || [ "$a" -eq 0 ]
}

# collect_sources DIR — print every regular source file under DIR/src plus the
# package/build manifests that also invalidate the artifact when they change.
collect_sources() {
    local dir="$1"
    if [ -d "$dir/src" ]; then
        find "$dir/src" -type f
    fi
    for manifest in package.json tsconfig.json vite.config.ts go.mod go.sum; do
        [ -f "$dir/$manifest" ] && echo "$dir/$manifest"
    done
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

build_deps() {
    link_node_modules "$SHIM_DIR" shim
    link_node_modules "$WEBAPP_DIR" webapp
}

build_shim() {
    link_node_modules "$SHIM_DIR" shim
    if ! is_stale "$SHIM_ARTIFACT" $(collect_sources "$SHIM_DIR"); then
        echo "[build-frontend] shim: fresh, skipping"
        return 0
    fi
    require_bin npm "install Node.js"
    echo "[build-frontend] shim: building..."
    ( cd "$SHIM_DIR" && npm run build )
    echo "[build-frontend] shim: done"
}

build_webapp() {
    link_node_modules "$WEBAPP_DIR" webapp
    if ! is_stale "$WEBAPP_ARTIFACT" $(collect_sources "$WEBAPP_DIR"); then
        echo "[build-frontend] webapp: fresh, skipping"
        return 0
    fi
    require_bin npm "install Node.js"
    echo "[build-frontend] webapp: building..."
    ( cd "$WEBAPP_DIR" && npm run build )
    echo "[build-frontend] webapp: done"
}

build_daemon() {
    if ! is_stale "$DAEMON_ARTIFACT" $(collect_sources "$DAEMON_DIR") $(find "$DAEMON_DIR" -name '*.go'); then
        echo "[build-frontend] daemon: fresh, skipping"
        return 0
    fi
    require_bin go "install the Go toolchain"
    echo "[build-frontend] daemon: building..."
    mkdir -p "$DAEMON_DIR/bin"
    ( cd "$DAEMON_DIR" && go build -o "$DAEMON_ARTIFACT" ./cmd/claude-repld )
    echo "[build-frontend] daemon: done"
}

for target in "${TARGETS[@]}"; do
    case "$target" in
        deps)   build_deps ;;
        shim)   build_shim ;;
        webapp) build_webapp ;;
        daemon) build_daemon ;;
    esac
done
