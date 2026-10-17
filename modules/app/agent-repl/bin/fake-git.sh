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
#   linked worktrees:     worktree add [-q|--quiet] (-b BRANCH | --detach) PATH,
#                         which checks HEAD's tree out into PATH
#   hooks:                rev-parse --git-path hooks (core.hooksPath, else
#                         <repository>/hooks)
#   named refs:           rev-parse --abbrev-ref HEAD |
#                         --path-format=absolute --git-common-dir,
#                         tag NAME REV, tag -d NAME, update-ref REF REV,
#                         update-ref -d REF; each is one ref transaction that
#                         runs the reference-transaction hook as git does
#                         (prepared, where nonzero aborts; then committed)
#                         tag --points-at REV, rev-parse --verify --quiet REF
#                         (the checked-out branch as refs/heads/NAME, or any
#                         named ref)
#   remotes:              push --atomic REMOTE SRC:DST..., which runs the
#                         pre-push hook as git does (a nonzero exit pushes
#                         nothing), and ls-remote REMOTE
#
# A LINKED WORKTREE is a directory whose .fakegit is a FILE naming the main
# worktree ("main <path>"), as a real linked worktree's .git file names its
# repository. Only where a linked worktree belongs is modelled -- rev-parse
# --git-dir / --git-common-dir / --show-toplevel and worktree list -- and not
# its own index or branch: any other call inside one exits 2. A linked
# worktree inside the main one is left out of the main one's snapshots, as
# real git leaves a nested worktree out of its parent's status.
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
#
# THE MODEL IS COMPILED (bin/fake-git.go). This script builds it once per
# source revision -- keyed by the source's checksum, into a per-user build
# directory, installed by atomic rename, the pattern bin/logs.sh uses for its
# reader -- and execs it. A shell implementation spawned bash and five helper
# processes per call, and under a full run's load those spawns set the
# heaviest harnesses' wall (build-frontend 219s for 48 CPU-seconds).
#
#   fake-git.sh ARGS...           run one git call against the model
#   fake-git.sh --install DEST    build the model and copy it to DEST, so a
#                                 harness puts the binary itself on PATH and
#                                 pays no shell per call

set -euo pipefail

# The script's own directory, through any symlink a harness put on PATH as
# `git` (test-lib-worktrees.sh links it): the source sits beside the script.
self="${BASH_SOURCE[0]}"
while [ -L "$self" ]; do
    target="$(readlink "$self")"
    case "$target" in
        /*) self="$target" ;;
        *) self="$(dirname "$self")/$target" ;;
    esac
done
THIS_DIR="$(cd "$(dirname "$self")" && pwd)"
SOURCE="$THIS_DIR/fake-git.go"

fail() { printf 'fake git: %s\n' "$*" >&2; exit 2; }

command -v go >/dev/null 2>&1 || fail 'go is required to build the fake git'
[ -f "$SOURCE" ] || fail "the fake git's source is missing: $SOURCE"
fingerprint="$(cksum "$SOURCE")" || fail "could not fingerprint the fake git's source: $SOURCE"
checksum="${fingerprint%% *}"
remainder="${fingerprint#* }"
size="${remainder%% *}"
build_root="${AGENT_REPL_FAKE_GIT_BUILD_DIR:-${TMPDIR:-/tmp}/agent-repl-fake-git-$(id -u)}"
binary="$build_root/fake-git-$checksum-$size"
if [ ! -x "$binary" ]; then
    mkdir -p "$build_root" || fail "could not create the fake git's build directory: $build_root"
    build_tmp="$binary.tmp.$$"
    # The build itself must never see this fake as `git`, nor stamp VCS state.
    if ! GOFLAGS=-buildvcs=false go build -trimpath -o "$build_tmp" "$SOURCE"; then
        rm -f "$build_tmp"
        fail 'could not build the fake git'
    fi
    if ! mv -f "$build_tmp" "$binary"; then
        rm -f "$build_tmp"
        fail "could not install the fake git: $binary"
    fi
fi

if [ "${1:-}" = --install ]; then
    [ $# -eq 2 ] || fail 'usage: fake-git.sh --install DEST'
    cp "$binary" "$2" || fail "could not copy the fake git to $2"
    chmod +x "$2" || fail "could not make $2 executable"
    exit 0
fi

exec "$binary" "$@"
