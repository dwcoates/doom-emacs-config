#!/usr/bin/env bash
# Build and run the agent-repl cross-system e2e sandbox.
#
#   e2e-sandbox.sh build [--allow-unpinned] [--no-cache]
#   e2e-sandbox.sh run   [--] <command> [args...]
#   e2e-sandbox.sh shell
#   e2e-sandbox.sh preflight
#
# HOST-STATE SAFETY, structurally:
#   * the repo is bind-mounted READ-ONLY at /repo-src, and the entrypoint
#     refuses to proceed if that mount turns out to be writable;
#   * HOME inside the container is /sandbox/home, an image-local path that no
#     mount ever covers, so ~/.claude, ~/.emacs.d, the Doom cache and the git
#     config a run touches are all container-local;
#   * NO host path other than /repo-src (ro) and the artifacts dir (rw, and
#     only when the caller names one) is mounted at all;
#   * the container runs as a non-root uid, so even a bind mount it should
#     not have could not be written as root;
#   * --network none: every dependency is baked into the image at build time.
set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
sandbox_dir=$(cd -- "$here/.." && pwd)
repo_root=$(cd -- "$sandbox_dir/../../../../.." && pwd)

IMAGE=${AGENT_REPL_SANDBOX_IMAGE:-agent-repl-e2e-sandbox:latest}
MODULE_REL=modules/app/agent-repl

log() { printf 'e2e-sandbox: %s\n' "$*" >&2; }
die() { log "$*"; exit 1; }

runtime() {
  if [[ -n ${AGENT_REPL_SANDBOX_RUNTIME:-} ]]; then
    printf '%s\n' "$AGENT_REPL_SANDBOX_RUNTIME"
    return 0
  fi
  local candidate
  for candidate in docker podman; do
    if command -v "$candidate" >/dev/null 2>&1; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done
  return 1
}

preflight() {
  "$here/preflight.sh"
}

# --- build -----------------------------------------------------------------

stage_context() {
  # Assembles the build context in a scratch dir: the sandbox's own files
  # plus the exact loader files, lockfiles and go.mod/go.sum manifests the
  # image needs at BUILD time. Nothing is duplicated in git — every staged
  # file is copied from the live checkout on each build.
  local ctx=$1
  cp "$sandbox_dir/Dockerfile" "$ctx/"
  mkdir -p "$ctx/bin" "$ctx/doom"
  cp "$here/entrypoint.sh" "$ctx/bin/"
  cp "$sandbox_dir/doom/"*.el "$ctx/doom/"

  # The three files Doom's module loader resolves by exact path.
  mkdir -p "$ctx/module-loader-files"
  local f
  for f in config.el packages.el doctor.el; do
    cp "$repo_root/$MODULE_REL/$f" "$ctx/module-loader-files/"
  done
  # config.el `load!`s lisp/*, and doctor.el loads three of them; a bare
  # `doom sync` only reads packages.el, but `doom doctor` and any elisp load
  # need the sources, so stage them too.
  mkdir -p "$ctx/module-loader-files/lisp"
  cp "$repo_root/$MODULE_REL/lisp/"*.el "$ctx/module-loader-files/lisp/"

  # npm lockfiles, for the offline cache prime.
  mkdir -p "$ctx/npm-lockfiles/shim" "$ctx/npm-lockfiles/webapp"
  cp "$repo_root/$MODULE_REL/agent-shim/claude/shim/package.json" \
     "$repo_root/$MODULE_REL/agent-shim/claude/shim/package-lock.json" \
     "$ctx/npm-lockfiles/shim/"
  cp "$repo_root/$MODULE_REL/webapp/package.json" \
     "$repo_root/$MODULE_REL/webapp/package-lock.json" \
     "$ctx/npm-lockfiles/webapp/"

  # Every go.mod/go.sum under the module, at its own relative path, so the
  # replace directives between them still resolve during the cache prime.
  local mod rel
  while IFS= read -r mod; do
    rel=${mod#"$repo_root/$MODULE_REL/"}
    mkdir -p "$ctx/go-manifests/$(dirname "$rel")"
    cp "$mod" "$ctx/go-manifests/$rel"
    if [[ -f ${mod%.mod}.sum ]]; then
      cp "${mod%.mod}.sum" "$ctx/go-manifests/${rel%.mod}.sum"
    fi
  done < <(find "$repo_root/$MODULE_REL" -name go.mod -not -path '*/node_modules/*')
}

do_build() {
  local allow_unpinned=0 extra=()
  while (( $# )); do
    case $1 in
      --allow-unpinned) allow_unpinned=1 ;;
      --no-cache) extra+=(--no-cache) ;;
      *) die "build: unknown flag $1" ;;
    esac
    shift
  done

  local rt
  rt=$(runtime) || die "no container runtime on PATH; run 'e2e-sandbox.sh preflight' for details"
  "$rt" info >/dev/null 2>&1 || die "'$rt' is not usable; run 'e2e-sandbox.sh preflight' for details"

  # The identifiers this checkout cannot pin on its own. See the Dockerfile's
  # pinning comment for why each one needs a registry or a download.
  local unpinned=()
  [[ -n ${SANDBOX_BASE_IMAGE:-} ]] || unpinned+=("SANDBOX_BASE_IMAGE (base image digest)")
  [[ -n ${SANDBOX_NODE_SHA256:-} ]] || unpinned+=("SANDBOX_NODE_SHA256")
  [[ -n ${SANDBOX_GO_SHA256:-} ]] || unpinned+=("SANDBOX_GO_SHA256")
  [[ -n ${SANDBOX_DOOM_REF:-} ]] || unpinned+=("SANDBOX_DOOM_REF (doom commit sha)")
  if (( ${#unpinned[@]} )) && (( allow_unpinned == 0 )); then
    log "refusing to build: these identifiers are not pinned:"
    printf '  - %s\n' "${unpinned[@]}" >&2
    log "set them, or pass --allow-unpinned to build against moving targets deliberately."
    exit 2
  fi
  (( ${#unpinned[@]} )) && log "WARNING: building with UNPINNED: ${unpinned[*]}"

  local ctx
  ctx=$(mktemp -d)
  trap 'rm -rf "$ctx"' EXIT
  stage_context "$ctx"

  local args=(build -t "$IMAGE" -f "$ctx/Dockerfile")
  [[ -n ${SANDBOX_BASE_IMAGE:-} ]] && args+=(--build-arg "BASE_IMAGE=$SANDBOX_BASE_IMAGE")
  [[ -n ${SANDBOX_SNAPSHOT_STAMP:-} ]] && args+=(--build-arg "SNAPSHOT_STAMP=$SANDBOX_SNAPSHOT_STAMP")
  [[ -n ${SANDBOX_NODE_VERSION:-} ]] && args+=(--build-arg "NODE_VERSION=$SANDBOX_NODE_VERSION")
  [[ -n ${SANDBOX_NODE_SHA256:-} ]] && args+=(--build-arg "NODE_SHA256=$SANDBOX_NODE_SHA256")
  [[ -n ${SANDBOX_GO_VERSION:-} ]] && args+=(--build-arg "GO_VERSION=$SANDBOX_GO_VERSION")
  [[ -n ${SANDBOX_GO_SHA256:-} ]] && args+=(--build-arg "GO_SHA256=$SANDBOX_GO_SHA256")
  # DOOM_REF has no default in the Dockerfile: an empty one fails the build
  # loudly rather than silently tracking Doom's master.
  args+=(--build-arg "DOOM_REF=${SANDBOX_DOOM_REF:-}")
  args+=("${extra[@]}" "$ctx")

  log "building $IMAGE with $rt"
  "$rt" "${args[@]}"
  log "built $IMAGE"
}

# --- run -------------------------------------------------------------------

do_run() {
  (( $# )) || die "run: no command given"
  local rt
  rt=$(runtime) || die "no container runtime on PATH; run 'e2e-sandbox.sh preflight' for details"
  preflight >&2 || die "preflight failed; see the message above"

  local sha
  sha=$(git -C "$repo_root" rev-parse HEAD 2>/dev/null || echo unknown)

  local args=(
    run --rm --init
    --network none
    --user 1000:1000
    --read-only
    --tmpfs "/tmp:rw,exec,size=2g"
    --tmpfs "/work:rw,exec,size=8g"
    --tmpfs "/sandbox/home/.cache:rw,exec,size=4g"
    --mount "type=bind,source=$repo_root,target=/repo-src,readonly"
    --env "AGENT_REPL_SANDBOX_SHA=$sha"
    --workdir "/work/repo/$MODULE_REL"
  )
  # The image's own HOME, Doom install and caches must stay writable even
  # under --read-only, so they get their own volumes rather than a host path.
  args+=(--tmpfs "/sandbox/home:rw,exec,size=4g")
  args+=(--tmpfs "/sandbox/doom/modules:rw,size=64m")

  if [[ -n ${AGENT_REPL_E2E_ARTIFACTS:-} ]]; then
    mkdir -p "$AGENT_REPL_E2E_ARTIFACTS"
    args+=(--mount "type=bind,source=$AGENT_REPL_E2E_ARTIFACTS,target=/artifacts")
    args+=(--env "AGENT_REPL_E2E_ARTIFACTS=/artifacts")
    log "artifacts -> $AGENT_REPL_E2E_ARTIFACTS"
  fi

  if [[ -t 1 ]]; then args+=(-t); fi

  args+=("$IMAGE" "$@")
  log "running: $*"
  # Logs stream straight through: no capture, no buffering, stderr stays
  # stderr so a failing test's output is the caller's output.
  "$rt" "${args[@]}"
}

# --- dispatch --------------------------------------------------------------

cmd=${1:-}
[[ $# -gt 0 ]] && shift || true
case $cmd in
  build) do_build "$@" ;;
  run)
    [[ ${1:-} == -- ]] && shift
    do_run "$@"
    ;;
  shell) do_run bash -l ;;
  preflight) preflight ;;
  ""|-h|--help)
    sed -n '2,20p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
    ;;
  *) die "unknown subcommand '$cmd'; one of build|run|shell|preflight" ;;
esac
