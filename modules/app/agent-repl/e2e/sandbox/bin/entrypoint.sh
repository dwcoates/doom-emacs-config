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
rsync -a --delete \
  --exclude '.git/' \
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

# Node dependencies, offline, from the cache baked into the image.
if [[ ${SANDBOX_SKIP_NPM:-0} != 1 ]]; then
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
