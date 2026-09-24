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
# exactly as they are. When they do not and node_modules is a link into the
# store:
#
#   1. A BROKEN ENTRY (one that does not satisfy its own lockfile: emptied,
#      partial) is REPAIRED IN PLACE, keeping the link, through the store's one
#      repair path (lib-node-store.sh): under the entry's lock, into a fresh
#      tree, swapped in atomically. Every other worktree on that entry is healed
#      by the same repair.
#   2. Only when that repair FAILS, or the entry is healthy but keyed by a
#      different lockfile than this package's, is the LINK removed (the entry is
#      not touched) and a private node_modules installed here, loudly.
set -euo pipefail

# shellcheck source=lib-node-store.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib-node-store.sh"

dir="${1:-$PWD}"
cd "$dir"

if npm ls --depth=0 >/dev/null 2>&1; then
    exit 0
fi

if [ -L node_modules ]; then
    target="$(readlink node_modules)"
    printf 'ensure-deps: %s/node_modules is a link into the shared store (%s), and that tree does not satisfy this package.\n' "$PWD" "$target" >&2
    if entry="$(node_store_entry_of_link node_modules)"; then
        if node_store_entry_healthy "$entry"; then
            printf 'ensure-deps: the store entry %s satisfies its own lockfile, so this package is on a different one.\n' "$entry" >&2
        elif node_store_repair "$entry" "$PWD"; then
            if npm ls --depth=0 >/dev/null 2>&1; then
                printf 'ensure-deps: repaired the store entry %s in place; the link is kept.\n' "$entry" >&2
                exit 0
            fi
            printf 'ensure-deps: the repaired store entry %s still does not satisfy this package.\n' "$entry" >&2
        else
            printf 'ensure-deps: REPAIRING the store entry %s FAILED; falling back to a private install.\n' "$entry" >&2
        fi
    else
        printf 'ensure-deps: the link does not name an entry of the store at %s, so it is not repaired.\n' "$(node_store_root)" >&2
    fi
    printf 'ensure-deps: removing the LINK only and installing a private node_modules here; the store entry is left untouched.\n' >&2
    # A path with no trailing slash: rm removes the link, never what it names.
    rm node_modules
fi

exec npm ci --no-audit --no-fund
