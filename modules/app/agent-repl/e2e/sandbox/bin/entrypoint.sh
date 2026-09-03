#!/usr/bin/env bash
# Container entrypoint for the agent-repl e2e sandbox.
#
# Turns the READ-ONLY repo mount at /repo-src into a WRITABLE working copy at
# /work/repo, wires the Doom profile's module path at the checked-out source,
# installs the node dependencies OFFLINE from the image's primed npm cache,
# and then execs whatever command the caller passed.
#
# Nothing here can write outside the container: /repo-src is mounted ro, and
# HOME is /sandbox/home, which is image-local.
set -euo pipefail

log() { printf 'sandbox: %s\n' "$*" >&2; }
die() { log "$*"; exit 1; }

REPO_SRC=${REPO_SRC:-/repo-src}
REPO=${REPO:-/work/repo}
MODULE_REL=modules/app/agent-repl

[[ -d $REPO_SRC/$MODULE_REL ]] || die "no agent-repl module at $REPO_SRC/$MODULE_REL (is the repo mounted at $REPO_SRC?)"

if [[ -w $REPO_SRC ]]; then
  die "$REPO_SRC is WRITABLE; the sandbox must mount the repo read-only (:ro)"
fi

log "materializing a writable working copy: $REPO_SRC -> $REPO"
mkdir -p "$REPO"
# --exclude node_modules: the image's own offline install below is the one
# that counts, and copying a host-built tree in would defeat it.
#
# --exclude '.git' has NO trailing slash on purpose, so it matches a .git
# FILE as well as a .git directory. In a git WORKTREE checkout .git is a file
# holding an absolute host path ("gitdir: /Users/.../.git/worktrees/..."),
# which does not exist in the container: copying it in made every git command
# run from the working copy die with "fatal: not a git repository". The suite
# gets its build identity from .sandbox-sha below, not from git.
# --no-owner --no-group --chmod=u+rwX, NOT a plain `-a`: `-a` preserves the
# HOST's uid/gid and mode bits, and the container runs as an unprivileged uid
# that owns none of them. That made rsync fail on every directory at once —
# `chgrp "..." failed: Operation not permitted` followed by `mkdir "..."
# failed: Permission denied` — leaving a half-copied working tree. Ownership
# and group of a throwaway working copy carry no meaning inside the sandbox;
# what matters is that the sandbox uid can read and traverse all of it and
# write where it needs to, which is exactly what u+rwX grants.
rsync -rlptD --delete \
  --no-owner --no-group \
  --chmod=u+rwX \
  --exclude '.git' \
  --exclude 'node_modules/' \
  "$REPO_SRC"/ "$REPO"/

# The suite resolves the shim build identity from git; the working copy has
# no .git, so hand it the SHA the run script read on the host.
if [[ -n ${AGENT_REPL_SANDBOX_SHA:-} ]]; then
  printf '%s\n' "$AGENT_REPL_SANDBOX_SHA" > "$REPO/.sandbox-sha"
  log "build identity: $AGENT_REPL_SANDBOX_SHA (from the host checkout)"
fi

# Point the Doom profile's `:app agent-repl` at the checked-out source,
# replacing the build-time loader-file snapshot.
mod_dir=$DOOMDIR/modules/app/agent-repl
rm -rf "$mod_dir"
mkdir -p "$(dirname "$mod_dir")"
ln -s "$REPO/$MODULE_REL" "$mod_dir"
log "doom module :app agent-repl -> $REPO/$MODULE_REL"

# HOME is a fresh tmpfs, so re-create the parts of it the image could not
# bake: XDG dirs, and a WRITABLE copy of the primed npm cache. The Go module
# cache stays where the image put it (/sandbox/cache/go/pkg/mod, read-only) —
# a complete module cache is read-only-safe — but npm writes to its cache
# even on an offline install, so that one is copied in.
mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" "$GOCACHE"
[[ -s ${GIT_CONFIG_GLOBAL:-} ]] || die "no git identity at ${GIT_CONFIG_GLOBAL:-<unset>}; the image did not bake one"

# Node dependencies, offline, from the cache baked into the image.
if [[ ${SANDBOX_SKIP_NPM:-0} != 1 ]]; then
  primed=${SANDBOX_CACHE:?SANDBOX_CACHE unset}/npm
  [[ -d $primed ]] || die "no primed npm cache at $primed"
  log "copying the primed npm cache into the writable HOME"
  mkdir -p "$HOME/.npm"
  cp -a "$primed/." "$HOME/.npm/"
  export NPM_CONFIG_CACHE="$HOME/.npm" npm_config_cache="$HOME/.npm"
  for rel in "$MODULE_REL/agent-shim/claude/shim" "$MODULE_REL/webapp"; do
    dir=$REPO/$rel
    [[ -f $dir/package-lock.json ]] || { log "no lockfile in $rel; skipping"; continue; }
    log "npm ci --offline in $rel"
    (cd "$dir" && npm ci --offline --no-audit --no-fund)
  done
fi

if [[ $# -eq 0 ]]; then
  die "no command given; e.g. e2e-sandbox.sh go test ./..."
fi

log "exec: $*"
cd "$REPO/$MODULE_REL"
exec "$@"
