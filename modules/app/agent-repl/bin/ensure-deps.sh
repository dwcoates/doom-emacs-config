#!/usr/bin/env bash
#
# ensure-deps.sh — make one npm package's node_modules satisfy its lockfile,
# NEVER by reinstalling through a shared dependency-store symlink.
#
# Usage: ensure-deps.sh [PACKAGE_DIR]   (default: the current directory)
#
# The webapp's and the shim's `ensure-deps` npm scripts call this (their
# `pre*` hooks run it before every test, typecheck and coverage run).
#
# WHY IT EXISTS. bin/build-frontend.sh links each checkout's node_modules into
# ONE shared store entry per lockfile hash (~/.cache/agent-repl/node-store), so
# every worktree with that lockfile shares one tree. The old script was
# `npm ls --depth=0 || npm ci`, and `npm ci` deletes node_modules before it
# installs -- THROUGH the symlink, so one worktree's failed `npm ls` emptied
# the store entry every other worktree was running on (2026-09-23: every
# worktree lost its webapp deps mid-suite, `vitest: not found`).
#
# SO A SYMLINK IS NEVER INSTALLED THROUGH. Deps that already resolve are left
# exactly as they are. When they do not and node_modules is a symlink, only the
# LINK is removed (the store entry is not touched) and the package gets a real,
# private node_modules of its own; that is said loudly, because it means the
# store entry is stale or broken and every other worktree on it is too.
set -euo pipefail

dir="${1:-$PWD}"
cd "$dir"

if npm ls --depth=0 >/dev/null 2>&1; then
    exit 0
fi

if [ -L node_modules ]; then
    target="$(readlink node_modules)"
    printf 'ensure-deps: %s/node_modules is a link into the shared store (%s), and that tree does not satisfy this package.\n' "$PWD" "$target" >&2
    printf 'ensure-deps: removing the LINK only and installing a private node_modules here; the store entry is left untouched.\n' >&2
    printf 'ensure-deps: the store entry is stale or broken for every worktree that links it; repopulate it with bin/build-frontend.sh deps once it is removed.\n' >&2
    # A path with no trailing slash: rm removes the link, never what it names.
    rm node_modules
fi

exec npm ci --no-audit --no-fund
