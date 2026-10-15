#!/usr/bin/env bash
# Container entrypoint for the agent-repl e2e sandbox.
#
# Turns the working copy e2e-sandbox.sh STREAMS onto stdin (a tar of the
# module's staged allowlist) into a WRITABLE tree at /work/repo, wires the Doom
# profile's module path at it, installs the node dependencies OFFLINE from the
# image's primed npm cache, and then execs whatever command the caller passed.
#
# NO HOST PATH IS MOUNTED. A bind mount of the checkout made Docker Desktop's
# file sharing open every staged file on the host and never release them, so
# each run leaked thousands of host descriptors until the host's file table
# was full. The stream is the only way the checkout reaches this container.
#
# HOME is /sandbox/home, which is image-local.
set -euo pipefail

log() { printf 'sandbox: %s\n' "$*" >&2; }
die() { log "$*"; exit 1; }

REPO=${REPO:-/work/repo}
MODULE_REL=modules/app/agent-repl

# A MOUNTED CHECKOUT IS REFUSED, NOT USED: it is the leak this stream replaced.
if [[ -d /repo-src ]] && [[ -n $(ls -A /repo-src 2>/dev/null) ]]; then
  die "/repo-src holds a mounted checkout; the sandbox takes its working copy on stdin only (e2e-sandbox.sh run)"
fi
if [[ -t 0 ]]; then
  die "no working copy on stdin; run the sandbox through e2e-sandbox.sh run, which streams it"
fi

log "materializing a writable working copy from the stream on stdin -> $REPO/$MODULE_REL"
mkdir -p "$REPO/$MODULE_REL"
# --no-same-owner and the umask: the stream carries the HOST's uid/gid and
# modes, which mean nothing here; the sandbox uid must own and traverse all of
# it. Mtimes survive, which is what lets the harness re-check the webapp
# dist's staleness with the same rule the host used.
tar -x -f - --no-same-owner -C "$REPO/$MODULE_REL" \
  || die "extracting the streamed working copy FAILED; the stream was cut or is not a tar"
chmod -R u+rwX "$REPO/$MODULE_REL"

log "staged $(find "$REPO/$MODULE_REL" -type f | wc -l | tr -d ' ') files, $(du -sh "$REPO" | cut -f1) total"

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
mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME"

# THE GO BUILD CACHE, seeded from the image rather than compiled from cold.
#
# GOCACHE lives under HOME and HOME is a fresh tmpfs, so every run used to
# start with an empty build cache and recompile the whole dependency graph --
# measured at ~18s before the first test ran. The image primes one (see the
# Dockerfile's "prime the Go BUILD cache"), and it is COPIED rather than
# linked because `go` writes to its cache on every invocation, trimming and
# adding entries, and the image layer is read-only.
#
# Seeding it can never be WRONG, only useless: Go's build cache is
# content-addressed, so an entry compiled from different source bytes is not
# found and the package is compiled as it would have been anyway.
primed_gocache=${SANDBOX_CACHE:?SANDBOX_CACHE unset}/go-build
if [[ -d $primed_gocache ]]; then
  log "seeding the Go build cache from the image ($(du -sh "$primed_gocache" | cut -f1))"
  mkdir -p "$(dirname "$GOCACHE")"
  cp -a "$primed_gocache" "$GOCACHE"
else
  log "WARNING: no primed Go build cache at $primed_gocache; every package will compile from cold"
  mkdir -p "$GOCACHE"
fi
[[ -s ${GIT_CONFIG_GLOBAL:-} ]] || die "no git identity at ${GIT_CONFIG_GLOBAL:-<unset>}; the image did not bake one"

# --- node dependencies: LINKED from the image, never installed per run -----
#
# The image bakes a complete `node_modules` for the shim and the webapp (see
# the Dockerfile's "BAKE the node dependency trees" step) at
# /sandbox/deps/<name>/node_modules, together with the sha256 of the
# package-lock.json it installed from.
#
# Linking rather than installing is worth two things at once. It removes
# about seven seconds of `npm ci` from every run, and -- the reason it exists
# -- it removes about 730 MiB from every container's MEMORY budget, because
# the working copy is a tmpfs and an installed `node_modules` is therefore
# resident RAM, while an image layer is shared and paid for once.
#
# IT IS NOT A BLIND SHORTCUT. The link is taken only when the checkout's
# lockfile hashes to exactly what the image installed from. When it does not,
# the mismatch is announced with both digests and the run falls back to the
# real `npm ci --offline` out of the npm cache the same bake primed, so a
# checkout that has moved its dependencies on can never be silently tested
# against the image's older tree.
#
# THE LINKED TREE IS READ-ONLY, and that is a constraint on the suites rather
# than a caveat on this comment: nothing a run does may write inside
# `node_modules`. One thing did. Vite's default cache directory is
# `node_modules/.vite`, so every `TestWebappLayer*` area failed in here -- and
# only in here -- until the webapp's configs moved their cache into the package
# directory (see webapp/vite-cache.ts). Copying the tree instead would have
# bought that write back at exactly the price this bake exists to avoid.
#
# SANDBOX_NODE_MODULES=copy still forces a writable per-run copy, for a caller
# that has some other reason to write in there and accepts the ~108 MiB
# (webapp) or ~621 MiB (shim) of tmpfs it costs; `install` forces the `npm ci`
# path outright. e2e-sandbox.sh forwards both from the caller's environment.
sandbox_npm_cache_ready=0
prepare_npm_cache() {
  (( sandbox_npm_cache_ready )) && return 0
  local primed=${SANDBOX_CACHE:?SANDBOX_CACHE unset}/npm
  [[ -d $primed ]] || die "no primed npm cache at $primed"
  log "copying the primed npm cache into the writable HOME"
  mkdir -p "$HOME/.npm"
  cp -a "$primed/." "$HOME/.npm/"
  export NPM_CONFIG_CACHE="$HOME/.npm" npm_config_cache="$HOME/.npm"
  sandbox_npm_cache_ready=1
}

install_node_deps() {
  local dir=$1 rel=$2
  prepare_npm_cache
  log "npm ci --offline in $rel"
  (cd "$dir" && npm ci --offline --no-audit --no-fund)
}

stage_node_deps() {
  local name=$1 rel=$2
  local dir=$REPO/$rel
  local baked=/sandbox/deps/$name
  [[ -f $dir/package-lock.json ]] || { log "no lockfile in $rel; skipping"; return 0; }

  local mode=${SANDBOX_NODE_MODULES:-link}
  if [[ $mode == install ]]; then
    install_node_deps "$dir" "$rel"
    return 0
  fi

  if [[ ! -d $baked/node_modules || ! -s $baked/.lock-sha256 ]]; then
    log "WARNING: the image bakes no node_modules for $name at $baked; installing instead"
    install_node_deps "$dir" "$rel"
    return 0
  fi

  local want have
  want=$(cut -d" " -f1 < "$baked/.lock-sha256")
  have=$(sha256sum "$dir/package-lock.json" | cut -d" " -f1)
  if [[ $want != "$have" ]]; then
    log "WARNING: $rel/package-lock.json has moved since this image was built"
    log "  image installed from sha256:$want"
    log "  checkout carries      sha256:$have"
    log "  the baked node_modules is therefore STALE and is NOT used; installing from the primed cache instead."
    log "  rebuild the image (e2e-sandbox.sh build) to get the fast path back."
    install_node_deps "$dir" "$rel"
    return 0
  fi

  rm -rf "$dir/node_modules"
  if [[ $mode == copy ]]; then
    log "copying the image's baked node_modules into $rel (SANDBOX_NODE_MODULES=copy)"
    cp -a "$baked/node_modules" "$dir/node_modules"
    return 0
  fi
  ln -s "$baked/node_modules" "$dir/node_modules"
  log "$rel/node_modules -> $baked/node_modules (image-baked, lockfile sha256:$want)"
}

if [[ ${SANDBOX_SKIP_NPM:-0} != 1 ]]; then
  stage_node_deps shim   "$MODULE_REL/agent-shim/claude/shim"
  stage_node_deps webapp "$MODULE_REL/webapp"
fi

# Optional `--dir <relpath>`, consumed before the command: <relpath> is
# resolved relative to the MODULE root, not the repo root, because every
# command this sandbox documents is written relative to the module (per
# AGENTS.md). This exists because the e2e suite is its OWN Go module
# (e2e/go.mod) with no go.mod at the module root, so a bare `go test` run
# from the module root dies with "go.mod file not found in the current
# directory or any parent directory" — `--dir e2e` is how a caller reaches
# it. Any other submodule with its own go.mod (e.g. `daemon`) works the same
# way. Omitted, the command runs from the module root as before.
run_rel=$MODULE_REL
if [[ ${1:-} == --dir ]]; then
  shift
  [[ $# -gt 0 ]] || die "--dir requires an argument"
  run_rel=$MODULE_REL/$1
  shift
fi
[[ -d $REPO/$run_rel ]] || die "--dir target does not exist: $REPO/$run_rel"

if [[ $# -eq 0 ]]; then
  die "no command given; e.g. e2e-sandbox.sh run go test ./... (or: run --dir e2e go test . -run X)"
fi

log "exec: $* (in $run_rel)"
cd "$REPO/$run_rel"
# THROUGH bin/background.sh, like every test run on the host: inside this
# Linux container that is `nice -n 19`, and the marker it exports is what
# lets the ERT harness, the vitest configs and the Go integration harness
# start at all (docker run passes the host's environment in empty).
exec "$REPO/$MODULE_REL/bin/background.sh" "$@"
