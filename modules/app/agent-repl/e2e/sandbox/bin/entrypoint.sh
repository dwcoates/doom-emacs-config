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

log "materializing a writable working copy: $REPO_SRC/$MODULE_REL -> $REPO/$MODULE_REL"

# STAGING IS AN ALLOWLIST, NOT A DENYLIST.
#
# The previous revision rsynced the WHOLE checkout. On a developer's live
# checkout that is 19 GB, 18 of which is `.claude/worktrees` (a hundred-plus
# stale agent worktrees, each a full checkout). The copy exhausted the host's
# file-descriptor table outright —
#   rsync: [sender] send_files failed to open ".../sessioncommand.go":
#          Too many open files in system (23)
# — and it walked paths that have nothing to do with any suite (a literal
# `~/.claude-chesscom` tree inside the repo, `.agents-sandbox`, `.git`).
#
# Nothing any suite touches lives outside `modules/app/agent-repl`: there is
# no repo-root `go.work`, no `.nvmrc`, no shared config. Every `replace`
# directive in every go.mod resolves within the module, every TypeScript
# `../../../proto/...` import resolves within it, and the one test that
# reaches for the checkout root (shim's metaprompt test, seven levels up)
# only reads `<root>/modules/app/agent-repl/metaprompt.md` back out of it.
# So the module tree IS the dependency set, and it is staged at its real
# relative path so those root-relative resolutions still land.
#
# The set is enumerated rather than globbed so a MISSING path is a loud
# refusal instead of a suite that fails later for an unrelated-looking
# reason, and so an unexpected NEW top-level directory cannot silently join
# the copy.
STAGE_ENTRIES=(
  # Go modules (daemon, e2e, shim-store, shim-sidecar, logging, fakedaemon,
  # generated protos) and their go.mod/go.sum manifests.
  daemon
  e2e
  agent-shim
  proto
  # Elisp sources and suites; also carries lisp/testsupport/fakedaemon.
  lisp
  # The three files Doom's module loader resolves by exact path.
  config.el
  packages.el
  doctor.el
  # The webapp module (TypeScript/Vitest) and its lockfile.
  webapp
  # Build and helper scripts the suites shell out to (bin/build-frontend.sh).
  bin
  scripts
  hooks
  # Data and prompt/skill/doc trees the suites and the running system read.
  testdata
  prompts
  skills
  projects
  plans
  docs
  images
  # Read by the shim's committed-metaprompt test and by AGENTS.md assertions.
  metaprompt.md
  AGENTS.md
)

module_src=$REPO_SRC/$MODULE_REL
missing=()
for entry in "${STAGE_ENTRIES[@]}"; do
  [[ -e $module_src/$entry ]] || missing+=("$MODULE_REL/$entry")
done
if (( ${#missing[@]} )); then
  log "refusing to stage: these required paths are absent from $REPO_SRC:"
  printf '  - %s\n' "${missing[@]}" >&2
  exit 1
fi

mkdir -p "$REPO/$MODULE_REL"

# Belt and braces on top of the allowlist: even inside the staged set, none
# of these may ever be dragged in. `.git` has NO trailing slash on purpose,
# so it matches a .git FILE too — in a git WORKTREE checkout .git is a file
# holding an absolute HOST path, which does not exist in the container, and
# copying it in made every git command from the working copy die with "fatal:
# not a git repository". The suite gets its build identity from .sandbox-sha
# below, not from git.
#
# --no-owner --no-group --chmod=u+rwX, NOT a plain `-a`: `-a` preserves the
# HOST's uid/gid and mode bits, and the container runs as an unprivileged uid
# that owns none of them. That made rsync fail on every directory at once —
# `chgrp "..." failed: Operation not permitted` followed by `mkdir "..."
# failed: Permission denied` — leaving a half-copied working tree. Ownership
# of a throwaway working copy carries no meaning inside the sandbox; what
# matters is that the sandbox uid can read and traverse all of it and write
# where it needs to, which is exactly what u+rwX grants.
#
# No --delete: /work is a fresh tmpfs on every run, so there is nothing to
# delete, and the flag only added a way to remove something unexpectedly.
stage_excludes=(
  --exclude '.git'
  --exclude '.claude/'
  --exclude '.agents-sandbox/'
  --exclude 'node_modules/'
  --exclude 'dist/'
  --exclude '.worktree'
  --exclude 'worktrees/'
  --exclude '.venv/'
  --exclude '__pycache__/'
  --exclude '*.test'
  --exclude '.sandbox-sha'
)

# A copy taken from a LIVE checkout always races: other processes write the
# source while rsync reads it, which produced dozens of
#   file has vanished: "/repo-src/modules/app/agent-repl/..."
# lines and a nonzero exit. The mount deliberately stays the caller's own
# checkout (that is what makes the sandbox test the working tree), so the
# race is TOLERATED EXPLICITLY rather than hidden: rsync's own exit code 24
# ("some files vanished before they could be transferred") is accepted, the
# vanished paths are summarized loudly, and EVERY other nonzero code is
# still fatal.
stage_log=$(mktemp)
rc=0
rsync -rlptD \
  --no-owner --no-group \
  --chmod=u+rwX \
  "${stage_excludes[@]}" \
  "${STAGE_ENTRIES[@]/#/$module_src/}" \
  "$REPO/$MODULE_REL"/ >"$stage_log" 2>&1 || rc=$?

vanished=$(grep -c 'file has vanished' "$stage_log" || true)
if (( rc == 24 )); then
  log "WARNING: $vanished source file(s) vanished mid-copy (the mount is a LIVE checkout); continuing"
  grep 'file has vanished' "$stage_log" >&2 || true
elif (( rc != 0 )); then
  cat "$stage_log" >&2
  rm -f "$stage_log"
  die "staging the working copy FAILED (rsync exit $rc)"
elif (( vanished > 0 )); then
  log "WARNING: $vanished source file(s) vanished mid-copy (the mount is a LIVE checkout)"
fi
rm -f "$stage_log"

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
exec "$@"
