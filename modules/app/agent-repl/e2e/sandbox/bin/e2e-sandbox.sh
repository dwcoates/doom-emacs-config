#!/usr/bin/env bash
# Build and run the agent-repl cross-system e2e sandbox.
#
#   e2e-sandbox.sh build [--allow-unpinned] [--no-cache] [--force]
#   e2e-sandbox.sh run   [--] [--dir <module-relative-path>] <command> [args...]
#   e2e-sandbox.sh shell
#   e2e-sandbox.sh preflight
#
# HOST-STATE SAFETY, structurally:
#   * NO HOST PATH IS EVER MOUNTED. The working copy is STREAMED in: the host
#     tars the module's staged allowlist onto `docker run -i`'s stdin and the
#     entrypoint extracts it, so the container's VM never opens a host file.
#     Failure artifacts come back the same way, out of a VM-side volume
#     through `docker cp`. (A bind mount of the checkout leaked ~4,500 host
#     descriptors per run into Docker's file sharing, which never released
#     them, until the host's file table was full; see stage_stream.)
#   * HOME inside the container is /sandbox/home, an image-local path that no
#     mount ever covers, so ~/.claude, ~/.emacs.d, the Doom cache and the git
#     config a run touches are all container-local;
#   * the container runs as a non-root uid;
#   * --network none: every dependency is baked into the image at build time.
set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
# sandbox_dir, repo_root, MODULE_REL, SANDBOX_STAMP_LABEL, sandbox_stamp and
# sandbox_image: which image this checkout's sandbox is.
# shellcheck source=sandbox-image.sh
. "$here/sandbox-image.sh"

IMAGE=$(sandbox_image)

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

  # THE GO SOURCES, for the build-cache prime.
  #
  # The Go BUILD cache (as opposed to the module cache) is empty in every
  # container, because it lives under HOME and HOME is a fresh tmpfs -- so
  # every run recompiled the whole dependency graph from scratch, measured at
  # ~18s before the first test ran. Compiling once at build time and shipping
  # the cache removes that.
  #
  # It is SAFE in a way the other staged artifacts are not, and that is worth
  # stating: Go's build cache is content-addressed, so an entry produced from
  # different source bytes simply is not found. A stale primed cache cannot
  # produce a stale binary; the worst it can do is miss and compile.
  #
  # `.go` files, plus the embedded testdata and the vocab JSON the Go side
  # embeds, at their real relative paths so every `replace` and every
  # `//go:embed` resolves the same way it will at run time.
  #
  # ONE tar pipeline per tree, never a mkdir and a cp per file: the module
  # holds well over a thousand .go files, and two forks each made staging alone
  # take tens of seconds.
  mkdir -p "$ctx/go-sources" "$ctx/go-manifests"
  (cd "$repo_root/$MODULE_REL" &&
    find . -name '*.go' -not -path '*/node_modules/*' -not -path '*/dist/*' -print0 |
      tar --null -T - -cf -) | tar -xf - -C "$ctx/go-sources"

  # Every go.mod (with the go.sum beside it, when there is one) under the
  # module, at its own relative path, so the replace directives between them
  # still resolve during the cache prime. The list is built with shell
  # builtins only, for the same reason.
  (cd "$repo_root/$MODULE_REL" &&
    find . -name go.mod -not -path '*/node_modules/*' -print0 |
      while IFS= read -r -d '' mod; do
        printf '%s\0' "$mod"
        if [[ -f ${mod%.mod}.sum ]]; then printf '%s\0' "${mod%.mod}.sum"; fi
      done | tar --null -T - -cf -) | tar -xf - -C "$ctx/go-manifests"
}

# --- recorded pins ---------------------------------------------------------
#
# pins.env holds the identifiers the Dockerfile cannot resolve from a
# checkout, so a normal build needs no `--allow-unpinned`. An environment
# variable of the same name always WINS over the recorded value, which is
# what keeps an override deliberate; the gate below is untouched, so a pin
# that is missing from BOTH is still a refusal to build.
load_pins() {
  local rt=$1 pins=$sandbox_dir/pins.env
  [[ -f $pins ]] || { log "no $pins; every identifier must come from the environment"; return 0; }

  local key value
  # shellcheck disable=SC2034  # $value is read by the eval below
  while IFS='=' read -r key value; do
    [[ $key == SANDBOX_* || $key == NODE_SHA256_* || $key == GO_SHA256_* ]] || continue
    # Already set in the environment: the caller's value wins.
    [[ -n ${!key:-} ]] && continue
    eval "$key=\"\$value\""
    export "${key?}"
  done < <(sed -e 's/[[:space:]]*#.*$//' -e '/^[[:space:]]*$/d' "$pins")

  # The tarball checksums are per architecture, so pick the pair matching the
  # architecture this build targets.
  local arch
  arch=$("$rt" version --format '{{.Server.Arch}}' 2>/dev/null || true)
  [[ -n $arch ]] || arch=$(uname -m)
  case $arch in
    x86_64|amd64) arch=amd64 ;;
    aarch64|arm64) arch=arm64 ;;
    *) die "unsupported build architecture '$arch'; no recorded node/go checksum for it" ;;
  esac
  local nkey=NODE_SHA256_$arch gkey=GO_SHA256_$arch
  [[ -n ${SANDBOX_NODE_SHA256:-} ]] || SANDBOX_NODE_SHA256=${!nkey:-}
  [[ -n ${SANDBOX_GO_SHA256:-} ]] || SANDBOX_GO_SHA256=${!gkey:-}
  log "pins: arch=$arch from $pins"
}

# --- the source stamp and the build lock ----------------------------------
#
# WHY. Five `build` runs at once thrashed one buildkit cache, and a build from
# a STALE checkout re-tagged a shared `:latest` over a good image, so a suite
# then ran against sandbox sources nobody was looking at. Each image is now
# tagged by its own sandbox stamp (sandbox-image.sh), and two structural fixes
# remain, neither advisory:
#
#   * BUILD IS EXCLUSIVE, host-wide. The lock is a directory claimed with
#     `mkdir` -- the one filesystem operation that is atomic and fails if the
#     name exists -- with the holder's pid recorded inside, exactly the
#     technique the run gate above uses. A second build
#     WAITS out loud; a lock whose holder is gone is taken over.
#
#   * THE IMAGE CARRIES ITS SOURCE. The git tree hash of e2e/sandbox is
#     written onto the image as an OCI label, so the image can always say
#     which sandbox sources produced it. A build whose stamp already matches
#     the tagged image does nothing at all, and `run` REFUSES an image whose
#     stamp is not this checkout's rather than silently using it.

# image_stamp RUNTIME — the stamp recorded on the tagged image, empty when the
# image does not exist or carries no stamp (an image built before this label
# existed, which is precisely the stale image this gate is here to catch).
image_stamp() {
  local rt=$1 out
  out=$("$rt" image inspect --format "{{ index .Config.Labels \"$SANDBOX_STAMP_LABEL\" }}" "$IMAGE" 2>/dev/null || true)
  out=${out//<no value>/}
  printf '%s\n' "${out//$'\n'/}"
}

# BUILD_LOCK_DIR is a fixed host path, NOT under $TMPDIR: on macOS $TMPDIR is
# per-process, which would hand every caller a private lock and therefore no
# exclusion at all. Builds from DIFFERENT worktrees of this repo share one
# buildkit cache, so they must share one lock.
BUILD_LOCK_DIR=${AGENT_REPL_SANDBOX_BUILD_LOCK_DIR:-/tmp/agent-repl-sandbox-build.lock}

# BUILD_LOCK_HELD is the lock this process holds, released by the EXIT trap.
# Not `local`: the trap runs after the acquiring function's frame is gone.
BUILD_LOCK_HELD=""

release_build_lock() {
  [[ -n ${BUILD_LOCK_HELD:-} ]] || return 0
  rm -rf "$BUILD_LOCK_HELD"
  BUILD_LOCK_HELD=""
}

# acquire_build_lock — block until this host's sandbox build lock is ours.
#
# The order is what makes the takeover safe: the pid is read from a lock that
# EXISTS, so nobody can be claiming it at that moment (a claim is `mkdir`,
# which fails on an existing name); only then is the directory moved aside. A
# lock therefore becomes claimable only by ceasing to exist, never by being
# emptied under a live holder.
acquire_build_lock() {
  local waited=0 announced=0 pid
  mkdir -p "$(dirname "$BUILD_LOCK_DIR")"
  while true; do
    if mkdir "$BUILD_LOCK_DIR" 2>/dev/null; then
      BUILD_LOCK_HELD=$BUILD_LOCK_DIR
      printf '%s\n' "$$" > "$BUILD_LOCK_DIR/pid"
      trap release_build_lock EXIT INT TERM
      (( waited > 0 )) && log "build lock acquired after ${waited}s"
      return 0
    fi
    pid=$(cat "$BUILD_LOCK_DIR/pid" 2>/dev/null || echo "")
    if [[ -n $pid ]] && ! kill -0 "$pid" 2>/dev/null; then
      log "reclaiming the build lock from dead pid $pid"
      mv "$BUILD_LOCK_DIR" "$BUILD_LOCK_DIR.dead.$$" 2>/dev/null && rm -rf "$BUILD_LOCK_DIR.dead.$$"
      continue
    fi
    if (( announced == 0 )); then
      log "WAITING: another sandbox build holds the host build lock${pid:+ (pid $pid)}."
      log "  This is the build gate, not a hang: $BUILD_LOCK_DIR is the lock."
      log "  Concurrent builds thrash one buildkit cache."
      announced=1
    elif (( waited % 30 == 0 )); then
      log "still waiting for the sandbox build lock (${waited}s)"
    fi
    sleep 2
    waited=$(( waited + 2 ))
  done
}

# require_fresh_image RUNTIME — refuse to RUN an image that is not this
# checkout's sandbox.
#
# A silently-used stale image is the failure this whole section exists to end:
# it produces test results about sources nobody is looking at, and it looks
# exactly like a real run while doing it. So the refusal is loud, it names
# both stamps, and it says what to do.
require_fresh_image() {
  local rt=$1 want have
  want=$(sandbox_stamp)
  have=$(image_stamp "$rt")
  [[ $have == "$want" ]] && return 0

  if [[ ${AGENT_REPL_SANDBOX_ALLOW_STALE:-0} == 1 ]]; then
    log "AGENT_REPL_SANDBOX_ALLOW_STALE=1: running a sandbox image whose sources are NOT this checkout's"
    log "  image sandbox stamp: ${have:-<none>}"
    log "  checkout sandbox stamp: $want"
    return 0
  fi

  log "REFUSING TO RUN: '$IMAGE' was not built from this checkout's e2e/sandbox."
  log "  image sandbox stamp: ${have:-<none: built before the stamp existed, or no such image>}"
  log "  checkout sandbox stamp: $want"
  log "  Rebuild it:  e2e/sandbox/bin/e2e-sandbox.sh build"
  log "  Or set AGENT_REPL_SANDBOX_ALLOW_STALE=1 to use it anyway, deliberately."
  exit 3
}

do_build() {
  local allow_unpinned=0 force=0 extra=()
  while (( $# )); do
    case $1 in
      --allow-unpinned) allow_unpinned=1 ;;
      --no-cache) extra+=(--no-cache); force=1 ;;
      --force) force=1 ;;
      *) die "build: unknown flag $1" ;;
    esac
    shift
  done

  local rt
  rt=$(runtime) || die "no container runtime on PATH; run 'e2e-sandbox.sh preflight' for details"
  "$rt" info >/dev/null 2>&1 || die "'$rt' is not usable; run 'e2e-sandbox.sh preflight' for details"

  # EXCLUSIVE, HOST-WIDE, AND BEFORE ANYTHING IS STAGED. Everything below this
  # line writes the shared buildkit cache.
  acquire_build_lock

  # The wait above is usually a wait for the very image this run wanted. Once
  # it is ours, ask what is on the tag: if it already carries this checkout's
  # sandbox stamp, there is nothing to build and saying so is the whole job.
  local stamp
  stamp=$(sandbox_stamp)
  if (( force == 0 )) && [[ $(image_stamp "$rt") == "$stamp" ]]; then
    log "'$IMAGE' already carries this checkout's sandbox stamp ($stamp); nothing to build"
    log "  pass --force (or --no-cache) to rebuild it anyway."
    return 0
  fi

  load_pins "$rt"

  # The identifiers this checkout cannot pin on its own. See the Dockerfile's
  # pinning comment for why each one needs a registry or a download.
  local unpinned=()
  [[ -n ${SANDBOX_BASE_IMAGE:-} ]] || unpinned+=("SANDBOX_BASE_IMAGE (base image digest)")
  [[ -n ${SANDBOX_NODE_SHA256:-} ]] || unpinned+=("SANDBOX_NODE_SHA256")
  [[ -n ${SANDBOX_GO_SHA256:-} ]] || unpinned+=("SANDBOX_GO_SHA256")
  [[ -n ${SANDBOX_EMACS_REF:-} ]] || unpinned+=("SANDBOX_EMACS_REF (upstream emacs commit sha)")
  [[ -n ${SANDBOX_DOOM_REF:-} ]] || unpinned+=("SANDBOX_DOOM_REF (doom commit sha)")
  if (( ${#unpinned[@]} )) && (( allow_unpinned == 0 )); then
    log "refusing to build: these identifiers are not pinned:"
    printf '  - %s\n' "${unpinned[@]}" >&2
    log "set them, or pass --allow-unpinned to build against moving targets deliberately."
    exit 2
  fi
  (( ${#unpinned[@]} )) && log "WARNING: building with UNPINNED: ${unpinned[*]}"

  # NOT `local`: the EXIT trap below runs after this function's frame is
  # gone, so a function-local would be UNSET by then — and under `set -u`
  # that made the trap body itself die with "ctx: unbound variable",
  # replacing the real failure with a shell error. A file-scope name plus a
  # `:-` guard in the trap makes the cleanup correct on every path.
  CTX=$(mktemp -d)
  # BOTH cleanups in one trap: `trap ... EXIT` REPLACES the handler, so a bare
  # context-cleanup trap here would silently discard the build lock's
  # release_build_lock trap and leak the lock to every later build on this host.
  trap 'rm -rf "${CTX:-}"; release_build_lock' EXIT INT TERM
  local ctx=$CTX
  stage_context "$ctx"

  local args=(build -t "$IMAGE" -f "$ctx/Dockerfile")
  # The image's own account of which sandbox sources made it. `run` reads this
  # back, so an image can never be stale without saying so.
  args+=(--label "$SANDBOX_STAMP_LABEL=$stamp")
  [[ -n ${SANDBOX_BASE_IMAGE:-} ]] && args+=(--build-arg "BASE_IMAGE=$SANDBOX_BASE_IMAGE")
  [[ -n ${SANDBOX_SNAPSHOT_STAMP:-} ]] && args+=(--build-arg "SNAPSHOT_STAMP=$SANDBOX_SNAPSHOT_STAMP")
  [[ -n ${SANDBOX_NODE_VERSION:-} ]] && args+=(--build-arg "NODE_VERSION=$SANDBOX_NODE_VERSION")
  [[ -n ${SANDBOX_NODE_SHA256:-} ]] && args+=(--build-arg "NODE_SHA256=$SANDBOX_NODE_SHA256")
  [[ -n ${SANDBOX_GO_VERSION:-} ]] && args+=(--build-arg "GO_VERSION=$SANDBOX_GO_VERSION")
  [[ -n ${SANDBOX_GO_SHA256:-} ]] && args+=(--build-arg "GO_SHA256=$SANDBOX_GO_SHA256")
  # Neither EMACS_REF nor DOOM_REF has a default in the Dockerfile: an empty
  # one fails the build loudly rather than silently tracking upstream master.
  args+=(--build-arg "EMACS_REF=${SANDBOX_EMACS_REF:-}")
  args+=(--build-arg "DOOM_REF=${SANDBOX_DOOM_REF:-}")
  args+=(${extra[@]+"${extra[@]}"} "$ctx")

  log "building $IMAGE with $rt"
  # No pipeline, no `|| true`, no subshell: the runtime's own status is this
  # function's status, and `set -e` propagates it. The explicit `||` exists
  # only to name the failure; it re-raises the same code.
  local rc=0
  "$rt" "${args[@]}" || rc=$?
  if (( rc != 0 )); then
    die "$rt build FAILED (exit $rc); no image was produced"
  fi
  verify_image "$rt"
  log "built $IMAGE (sandbox stamp $stamp)"
}

# --- post-build verification ----------------------------------------------
#
# A build script that reports success without an image is worse than no build
# script, so success is not the runtime's word for it: the image must exist
# AND carry every binary the suite depends on, or this fails loudly.
verify_image() {
  local rt=$1
  "$rt" image inspect "$IMAGE" >/dev/null 2>&1 \
    || die "build reported success but image '$IMAGE' does not exist"

  # Run the probe with the same isolation the suite uses, so a missing binary
  # is caught under the real conditions and not a friendlier ad-hoc setup.
  local probe
  probe=$(cat <<'PROBE'
set -eu
fail=0
for b in emacs node npm go git rsync script doom; do
  command -v "$b" >/dev/null 2>&1 || { echo "MISSING BINARY: $b" >&2; fail=1; }
done
# EMACS MUST BE THE HOST'S OWN BUILD, WITH XWIDGETS AND NATIVE COMP.
#
#   * the VERSION, because the pinned Doom refuses to start an interactive
#     session below 29.1 (a `-nw` frame IS interactive), and because the
#     sandbox is only worth trusting if it runs the Emacs our users run;
#   * XWIDGETS, because the webapp panel this module renders IS an
#     `xwidget-webkit` webview — no distro Emacs is built with it, and
#     without it the panel does not work at all;
#   * NATIVE COMPILATION, because it is how the host's Emacs is built and
#     because `native-comp-available-p` is the only check that proves
#     libgccjit and gcc are actually usable at RUN time, not merely that
#     `configure` said yes at build time.
#
# All three are asserted in the Dockerfile too. They are re-asserted HERE so
# the property belongs to the IMAGE — an image built by some other path, or
# an older one still lying around under the same tag, cannot pass this gate.
emacs_version=$(emacs -Q --batch --eval '(princ emacs-version)' 2>/dev/null || echo unknown)
case "$emacs_version" in
  30.2*) ;;
  *) echo "WRONG EMACS: '$emacs_version' (expected 30.2, the host build)" >&2; fail=1 ;;
esac
emacs -Q --batch --eval '(unless (featurep (quote xwidget-internal)) (kill-emacs 1))' 2>/dev/null \
  || { echo "EMACS HAS NO XWIDGETS: the webapp panel's webview cannot exist" >&2; fail=1; }
emacs -Q --batch --eval '(unless (native-comp-available-p) (kill-emacs 1))' 2>/dev/null \
  || { echo "EMACS HAS NO NATIVE COMPILATION available at run time" >&2; fail=1; }
command -v Xvfb >/dev/null 2>&1 \
  || { echo "MISSING BINARY: Xvfb (an xwidget frame needs a display)" >&2; fail=1; }
# `doom sync` must have been baked at build time: the profile's .local is
# what proves it, and without it every Emacs test would pay the sync.
if [ ! -d "$EMACSDIR/.local" ]; then
  echo "MISSING: $EMACSDIR/.local (doom sync was not baked into the image)" >&2
  fail=1
fi
# The node dependency trees must be baked too, WITH the lockfile digest the
# entrypoint checks the checkout against. Without them every run reinstalls
# them onto a tmpfs -- seven seconds and ~730 MiB of the container's memory
# budget, which is what made two concurrent sandboxes OOM the VM.
for d in shim webapp; do
  if [ ! -d "/sandbox/deps/$d/node_modules" ]; then
    echo "MISSING: /sandbox/deps/$d/node_modules (the node deps were not baked into the image)" >&2
    fail=1
  fi
  if [ ! -s "/sandbox/deps/$d/.lock-sha256" ]; then
    echo "MISSING: /sandbox/deps/$d/.lock-sha256 (no lockfile digest, so staleness could not be checked)" >&2
    fail=1
  fi
done
exit "$fail"
PROBE
)
  # `-lc`, not `-c`: this deliberately probes the LOGIN environment, which is
  # what `e2e-sandbox.sh shell` and the image's default CMD get, so a PATH
  # that only works for non-login shells is caught here.
  "$rt" run --rm --network none --entrypoint /bin/bash "$IMAGE" -lc "$probe" \
    || die "image '$IMAGE' is missing required contents (see above)"
  log "verified: image exists, carries emacs 30.2 (xwidgets + native-comp), Xvfb, node/go/script/doom, a baked doom sync and baked node deps"
}

# --- the concurrency gate -------------------------------------------------
#
# WHY A GATE AT ALL. One sandbox is a real Emacs with a GUI frame, an Xvfb, a
# daemon, a Node shim, a store and a sidecar, three of those at a time inside
# the container. Measured, that container peaks at 1.58 GiB. The Docker VM it
# runs in has 5.79 GiB total and is SHARED -- on the machine this was measured
# on, unrelated containers were already holding 1.82 GiB of it. Two sandboxes
# started at once did not run slowly, they OOM-killed each other: `npm ci`
# exited 137 and Emacs was SIGKILLed mid-scenario, which then read as a test
# failure with nothing wrong in it.
#
# So a second `run` WAITS, out loud, instead of overcommitting. The default is
# ONE container; more are allowed only when the VM's free memory actually
# covers them, computed from `docker info` and what the running containers are
# using rather than assumed.
#
# The slots are directories, claimed with `mkdir` -- the one filesystem
# operation that is atomic and fails if the name exists, so two runs cannot
# both believe they hold the same slot. A slot whose holder died is reclaimed:
# the reaper reads the recorded pid, checks it is gone, and `mv`s the
# directory away, which means a slot only ever becomes claimable by
# disappearing. It is never simply deleted out from under a live holder.

# sandboxSlotDir is where the slots live: a fixed host path, so runs from
# DIFFERENT worktrees of this repo gate against each other. It is not under
# $TMPDIR, which on macOS is per-process and would give every caller a private
# set of slots and no gate at all.
SLOT_DIR=${AGENT_REPL_SANDBOX_SLOT_DIR:-/tmp/agent-repl-e2e-sandbox.slots}

# SANDBOX_MEM_BUDGET_MB is what one container is assumed to need.
#
# MEASURED: the full Emacs layer at its own parallelism bound peaks at
# 1.58 GiB of container memory; the whole Go e2e suite is smaller. 2048 MiB is
# that peak plus a quarter, because a budget that is merely equal to the
# observed peak has no room for the run that is slightly worse than the one
# that was measured -- and the failure mode of getting this wrong is an
# OOM-kill, not a slowdown.
SANDBOX_MEM_BUDGET_MB=${AGENT_REPL_SANDBOX_MEM_BUDGET_MB:-2048}

# SANDBOX_MEM_HEADROOM_MB is what is left to the VM itself and to whatever
# starts while a run is in flight. MEASURED only in the sense that 1 GiB is
# what the VM was observed to hold in page cache, daemons and slack with no
# sandbox running at all.
SANDBOX_MEM_HEADROOM_MB=${AGENT_REPL_SANDBOX_MEM_HEADROOM_MB:-1024}

# SANDBOX_MAX_SLOTS caps the computed answer regardless of memory: past a
# handful of containers the CPUs they share are the binding constraint, not
# the RAM.
SANDBOX_MAX_SLOTS=${AGENT_REPL_SANDBOX_MAX_SLOTS:-4}

# SANDBOX_CPUS is how many CPUs ONE container may use: `docker run --cpus`,
# and GOMAXPROCS for every Go process inside (the image's Go predates
# cgroup-aware GOMAXPROCS, so without it each one sizes itself to the whole
# VM and is throttled against the quota). The VM itself has as many CPUs as
# the host gives Docker, which is not a number any bound here was measured
# on; four is the VM the Emacs layer's parallelism bound of two Emacsen was
# measured on (e2e/EMACS-LAYER-SPEC.md, "The parallelism bound, measured").
#
# Under testrun the container is a unit holding AGENT_REPL_UNIT_SLOTS core
# slots, and bin/test-e2e-emacs.sh passes that width here, so the cores the
# container uses are the slots the scheduler reserved for it.
SANDBOX_CPUS=${AGENT_REPL_SANDBOX_CPUS:-4}

# slot_budget RUNTIME — how many containers this VM can afford right now.
#
# Answers 1 whenever it cannot tell, which is the safe direction: the gate's
# purpose is to stop overcommitment, and a gate that opens wide because it
# could not read `docker info` would be worse than no gate, since it would
# LOOK like it was protecting something.
slot_budget() {
  local rt=$1 total_bytes total_mb used_mb avail_mb n

  total_bytes=$("$rt" info --format '{{.MemTotal}}' 2>/dev/null || echo 0)
  [[ $total_bytes =~ ^[0-9]+$ ]] && (( total_bytes > 0 )) || { printf '1\n'; return 0; }
  total_mb=$(( total_bytes / 1024 / 1024 ))

  # What the currently running containers hold. `docker stats` reports
  # human-readable sizes ("583.6MiB / 5.787GiB"), so only the first field of
  # each line is read and its unit converted. A container that vanishes
  # mid-read simply contributes nothing.
  used_mb=$("$rt" stats --no-stream --format '{{.MemUsage}}' 2>/dev/null \
    | awk -F' */ *' '{print $1}' \
    | awk '
        /GiB/ { gsub(/GiB/,""); s += $1 * 1024; next }
        /MiB/ { gsub(/MiB/,""); s += $1; next }
        /KiB/ { gsub(/KiB/,""); s += $1 / 1024; next }
        /B/   { gsub(/B/,"");   s += $1 / 1024 / 1024; next }
        END { printf "%d\n", s }' || echo 0)
  [[ $used_mb =~ ^[0-9]+$ ]] || used_mb=0

  avail_mb=$(( total_mb - used_mb - SANDBOX_MEM_HEADROOM_MB ))
  n=$(( avail_mb / SANDBOX_MEM_BUDGET_MB ))
  (( n < 1 )) && n=1
  (( n > SANDBOX_MAX_SLOTS )) && n=SANDBOX_MAX_SLOTS
  log "memory budget: VM ${total_mb}MiB, containers using ${used_mb}MiB, headroom ${SANDBOX_MEM_HEADROOM_MB}MiB, ${SANDBOX_MEM_BUDGET_MB}MiB per sandbox => ${n} concurrent sandbox(es)"
  printf '%s\n' "$n"
}

# reap_dead_slots frees slots whose holder is gone.
#
# The order is what makes it safe: the pid is read from a slot that EXISTS, so
# no one else can be claiming it at that moment (a claim is `mkdir`, which
# fails on an existing name); only then is the directory moved aside. A slot
# therefore becomes claimable only by ceasing to exist, never by being emptied
# while someone holds it.
reap_dead_slots() {
  local slot pid
  for slot in "$SLOT_DIR"/slot-*; do
    [[ -d $slot ]] || continue
    pid=$(cat "$slot/pid" 2>/dev/null || echo "")
    if [[ -z $pid ]]; then
      # A slot mid-claim (created, pid not yet written). Leave it: the
      # claimer writes the pid immediately, and reclaiming it here would
      # race a live run.
      continue
    fi
    if kill -0 "$pid" 2>/dev/null; then
      continue
    fi
    log "reclaiming slot $(basename "$slot") from dead pid $pid"
    mv "$slot" "$slot.dead.$$" 2>/dev/null && rm -rf "$slot.dead.$$"
  done
}

# SANDBOX_SLOT is the slot this process holds, released by the EXIT trap. Not
# `local`: the trap runs after the acquiring function's frame is gone.
SANDBOX_SLOT=""

release_slot() {
  [[ -n ${SANDBOX_SLOT:-} ]] || return 0
  rm -rf "$SANDBOX_SLOT"
  SANDBOX_SLOT=""
}

# acquire_slot RUNTIME — block until one of the VM's sandbox slots is free.
acquire_slot() {
  local rt=$1
  if [[ ${AGENT_REPL_SANDBOX_NO_GATE:-0} == 1 ]]; then
    log "AGENT_REPL_SANDBOX_NO_GATE=1: running WITHOUT the concurrency gate; concurrent sandboxes can OOM-kill each other"
    return 0
  fi

  mkdir -p "$SLOT_DIR"
  local slots i waited=0 announced=0
  slots=$(slot_budget "$rt")

  while true; do
    reap_dead_slots
    for (( i = 1; i <= slots; i++ )); do
      if mkdir "$SLOT_DIR/slot-$i" 2>/dev/null; then
        SANDBOX_SLOT="$SLOT_DIR/slot-$i"
        printf '%s\n' "$$" > "$SANDBOX_SLOT/pid"
        trap release_slot EXIT
        (( waited > 0 )) && log "slot $i acquired after ${waited}s"
        return 0
      fi
    done
    if (( announced == 0 )); then
      log "WAITING: all $slots sandbox slot(s) are in use by another run."
      log "  This is the memory gate, not a hang: $SLOT_DIR holds a directory per running sandbox."
      log "  Concurrent sandboxes OOM-kill each other on this VM; set AGENT_REPL_SANDBOX_NO_GATE=1 to override deliberately."
      announced=1
    elif (( waited % 30 == 0 )); then
      log "still waiting for a sandbox slot (${waited}s)"
    fi
    sleep 2
    waited=$(( waited + 2 ))
  done
}

# --- host-staged build artifacts ------------------------------------------

# stage_webapp_dist builds the webapp dist ON THE HOST when it is stale, so
# the container never has to.
#
# The Emacs client layer serves the REAL webapp to a real webview, so it
# needs a real dist. Building it inside the container -- which is what used
# to happen, once per run -- meant `tsc --noEmit && vite build` running on a
# tmpfs inside a 5.8 GiB VM: about six seconds and roughly a gigabyte of
# resident memory for tsc alone, immediately before a real Emacs started.
# That was the single biggest reason two sandboxes could not run at once.
#
# The staleness rule is bin/webapp-dist.sh's, and it is ONE rule: this runs
# `ensure` (check, and build through bin/build-frontend.sh when stale), and
# the harness inside the container runs `check` from the SAME script before
# it hands the dist to the daemon. A stale dist can therefore never be
# silently served -- it is rebuilt here, or refused there.
#
# AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1 skips the host build. It does NOT skip
# the container's check, so a stale dist still fails loudly. testrun sets it:
# a test run builds the dist once before any suite starts (testrun's
# PrepareWebappDist), so no suite rewrites the checkout under another
# suite's watch-mode driver mid-run.
stage_webapp_dist() {
  if [[ ${AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD:-0} == 1 ]]; then
    log "AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1: not building the webapp dist; the container will refuse a stale one"
    return 0
  fi
  "$here/webapp-dist.sh" ensure
}

# --- the container's CPU cap --------------------------------------------------

# container_cpu_args CPUS -- the `docker run` arguments that cap a container to
# CPUS CPUs, space separated; anything but a positive whole number is refused
# (the refusal is printed, for the caller to die with).
container_cpu_args() {
  local cpus=$1
  if [[ ! $cpus =~ ^[1-9][0-9]*$ ]]; then
    printf "AGENT_REPL_SANDBOX_CPUS must be a positive whole number of CPUs, got '%s'\n" "$cpus"
    return 1
  fi
  printf -- '--cpus %s --env GOMAXPROCS=%s\n' "$cpus" "$cpus"
}

# --- the streamed working copy ------------------------------------------------

# STAGE_ENTRIES is what of the module a sandbox run gets: an ALLOWLIST, not a
# denylist.
#
# An earlier revision copied the WHOLE checkout. On a developer's live
# checkout that is 19 GB, 18 of which is `.claude/worktrees` (a hundred-plus
# stale agent worktrees, each a full checkout), and the copy exhausted the
# host's file table outright.
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
  # generated protos, and the test runner whose testenv package the e2e and
  # daemon harnesses import) and their go.mod/go.sum manifests.
  daemon
  e2e
  agent-shim
  proto
  testrun
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
  plans
  docs
  images
  # Read by the shim's committed-metaprompt test and by AGENTS.md assertions.
  metaprompt.md
  AGENTS.md
)

# stage_find lists, NUL-separated and relative to the module root, every
# directory, file and symlink of the staged set. DIRECTORIES ARE LISTED TOO,
# for their mtimes: tar archives each entry without recursion (-n) and restores
# a listed directory's mtime, while a directory it only creates on the way to a
# file gets the extraction time, and the webapp dist's staleness rule
# (webapp-dist.sh: `find src -newer dist/index.html`) counts directories, so
# every freshly built dist read as stale. Inside the allowlist none of these is
# ever taken:
#   * `.git`, file or directory: in a git WORKTREE checkout .git is a FILE
#     holding an absolute host path that does not exist in the container, and
#     the suite gets its build identity from .sandbox-sha instead;
#   * node_modules, every `dist/` but the webapp's own, the host's vite cache
#     (keyed to the host's absolute paths), worktrees, virtualenvs, Python
#     caches, compiled `*.test` binaries and a stale `.sandbox-sha`. Each
#     excluded directory is excluded as a SYMLINK too: a checkout's
#     webapp/node_modules is a link into the host's node store
#     (lib-node-store.sh), and a `-type d` test alone streamed that link, a
#     host path, into the container (2026-10-15).
# THE WEBAPP DIST IS STAGED, and it is the one `dist/` that is: the Emacs
# layer's panel is a real webview on the daemon's own origin, so it needs the
# REAL built webapp, which the host builds (stage_webapp_dist) before the run.
stage_find() {
  local module_src=$1
  ( cd -- "$module_src" && find "${STAGE_ENTRIES[@]}" \
      \( -name .git \
         -o \( \( -type d -o -type l \) \( -name .claude -o -name .agents-sandbox -o -name node_modules \
                         -o -name .vite-cache -o -name worktrees -o -name .venv -o -name __pycache__ \) \) \
         -o \( \( -type d -o -type l \) -name dist ! -path webapp/dist \) \
         -o -name .worktree -o -name '*.test' -o -name .sandbox-sha \) -prune \
      -o -print0 )
}

# stage_stream MODULE_SRC STATUS_FILE writes the staged working copy to stdout
# as one tar stream, for `docker run -i` to carry into the container.
#
# THIS IS THE ONLY WAY THE CHECKOUT REACHES THE CONTAINER. A bind mount made
# Docker Desktop's file sharing open every staged file on the host, and it
# never released them: each run leaked ~4,500 descriptors into the VM's
# process (measured 2026-10-15: 51,851 of the host's 65,536 held by it), until
# the table was full and every e2e-emacs gate on the host failed staging with
# "Too many open files in system". A stream is read by THIS process, closed as
# it goes, and handed over through the runtime's API; the VM opens nothing.
#
# A LIVE CHECKOUT RACES: a file listed and then deleted before tar reads it is
# reported, tolerated, and summarized, exactly as the old rsync's exit 24 was.
# Any OTHER tar failure is recorded in STATUS_FILE, which do_run checks after
# the container exits, so a broken stream can never pass as a green run.
stage_stream() {
  local module_src=$1 status=$2 errs rc=0
  errs=$(mktemp)
  stage_find "$module_src" \
    | COPYFILE_DISABLE=1 tar -c -f - --null -n -T - \
        --no-xattrs --no-acls --no-mac-metadata -C "$module_src" 2>"$errs" || rc=$?
  if (( rc != 0 )) && ! grep -v -E 'No such file or directory|Error exit delayed from previous errors' "$errs" >/dev/null; then
    log "WARNING: $(grep -c 'No such file or directory' "$errs") staged file(s) vanished mid-stream (a LIVE checkout); continuing"
    rc=0
  fi
  if (( rc != 0 )); then
    cat "$errs" >&2
  fi
  printf '%s\n' "$rc" > "$status"
  rm -f "$errs"
}

# stage_check refuses a run whose allowlist names a path the checkout lacks.
stage_check() {
  local module_src=$1 entry missing=()
  for entry in "${STAGE_ENTRIES[@]}"; do
    [[ -e $module_src/$entry ]] || missing+=("$MODULE_REL/$entry")
  done
  if (( ${#missing[@]} )); then
    log "refusing to stage: these required paths are absent from the checkout:"
    printf '  - %s\n' "${missing[@]}" >&2
    exit 1
  fi
}

# SANDBOX_CONTAINER names this run's container, so it can be removed however
# the run ends. Not `local`: the EXIT trap runs after do_run's frame is gone.
SANDBOX_CONTAINER=""
SANDBOX_RT=""

remove_container() {
  [[ -n ${SANDBOX_CONTAINER:-} ]] || return 0
  "$SANDBOX_RT" rm -f -v "$SANDBOX_CONTAINER" >/dev/null 2>&1 || true
  SANDBOX_CONTAINER=""
}

# --- run -------------------------------------------------------------------

do_run() {
  # --shell: hold the container open on the staged copy and hand the user an
  # interactive login shell in it (see the end of this function).
  local shell=0
  if [[ ${1:-} == --shell ]]; then
    shell=1
    shift
  fi
  (( $# )) || die "run: no command given"
  local rt
  rt=$(runtime) || die "no container runtime on PATH; run 'e2e-sandbox.sh preflight' for details"

  # BEFORE the preflight, the gate and the webapp build: an image that is not
  # this checkout's sandbox is refused loudly, never silently used.
  require_fresh_image "$rt"

  preflight >&2 || die "preflight failed; see the message above"

  # BEFORE anything is built or started. The gate is what keeps two runs from
  # OOM-killing each other on a 5.8 GiB VM.
  acquire_slot "$rt"

  local sha
  sha=$(git -C "$repo_root" rev-parse HEAD 2>/dev/null || echo unknown)

  stage_webapp_dist

  local cpu_out cpu_args
  cpu_out=$(container_cpu_args "$SANDBOX_CPUS") || die "$cpu_out"
  read -ra cpu_args <<<"$cpu_out"
  log "capping the container to $SANDBOX_CPUS CPUs"

  # NO `--rm`: the container outlives its exit so the failure artifacts can
  # be copied out of it, and the EXIT trap removes it (see below).
  local args=(
    run --init
    "${cpu_args[@]}"
    --network none
    --user 1000:1000
    --read-only
    # A WEDGED EMACS IS DIAGNOSED FROM OUTSIDE OR NOT AT ALL. When Emacs
    # loops in C it answers no server socket, no nested eval, no SIGUSR2 and
    # no SIGINT, so the only remaining account of the stall is a native
    # backtrace, and taking one means `gdb` attaching with ptrace. The
    # harness's gdb is a SIBLING of Emacs (both are children of `go test`),
    # not an ancestor, so under the host's Yama `ptrace_scope=1` the attach
    # is refused without CAP_SYS_PTRACE. The capability is scoped to this
    # container, whose whole process tree this run already owns; it grants
    # nothing over the host, which `--network none`, `--read-only`, the
    # non-root uid and the read-only source bind all still hold.
    --cap-add SYS_PTRACE
    # EVERY writable tmpfs carries uid/gid=1000 EXPLICITLY. Docker mounts a
    # tmpfs root:root 0755, so without this the container's non-root uid
    # cannot create anything in /work at all and the entrypoint's extraction dies
    # with "mkdir ... Permission denied" on the first directory. /tmp keeps
    # the conventional 1777 instead, since anything may write there.
    --tmpfs "/tmp:rw,exec,size=2g,mode=1777"
    --tmpfs "/work:rw,exec,size=8g,uid=1000,gid=1000,mode=0755"
    # THE WORKING COPY ARRIVES ON STDIN (stage_stream), never through a mount.
    --interactive
    --env "AGENT_REPL_SANDBOX_SHA=$sha"
    # --workdir IS DELIBERATELY THE TMPFS ROOT, not the module directory.
    # Docker CREATES a missing workdir itself, as ROOT, before the entrypoint
    # runs — so naming /work/repo/<module> here pre-created that whole chain
    # root-owned inside the tmpfs, and the entrypoint's extraction then failed
    # "mkdir ... Permission denied" on every directory under it. /work is the
    # tmpfs mountpoint, already owned by the sandbox uid, so nothing is
    # created; the entrypoint cd's to the module directory before exec.
    --workdir "/work"
  )
  # The image's own HOME, Doom install and caches must stay writable even
  # under --read-only, so they get their own volumes rather than a host path.
  # HOME must be a tmpfs (that is what keeps a run off the host's ~), which
  # means it HIDES anything the image put under /sandbox/home. The image
  # therefore keeps its primed caches at /sandbox/cache and its git identity
  # at /sandbox/gitconfig, neither of which any mount covers.
  args+=(--tmpfs "/sandbox/home:rw,exec,size=8g,uid=1000,gid=1000,mode=0755")
  args+=(--tmpfs "/sandbox/doom/modules:rw,size=64m,uid=1000,gid=1000,mode=0755")

  # THE ENTRYPOINT'S OWN KNOBS, FORWARDED WHEN THE CALLER SET THEM.
  #
  # `docker run` starts a container with an EMPTY environment but for what the
  # image and these flags put there, so a caller who exported
  # SANDBOX_NODE_MODULES or SANDBOX_SKIP_NPM saw it silently ignored -- a
  # documented escape hatch that could not actually be reached. Only the names
  # listed here are forwarded: a blanket passthrough would leak the host's
  # whole environment into a sandbox whose entire point is that it carries
  # nothing of the host in with it. Unset stays unset, so the entrypoint's
  # defaults are untouched.
  local knob
  for knob in SANDBOX_NODE_MODULES SANDBOX_SKIP_NPM; do
    if [[ -n ${!knob:-} ]]; then
      args+=(--env "$knob=${!knob}")
      log "forwarding $knob=${!knob} to the container"
    fi
  done

  # THE CONTAINER IS NAMED AND REMOVED HERE, never `--rm`: the failure
  # artifacts are copied out of it after it exits, and the EXIT trap removes it
  # (with its volume) however the run ends.
  SANDBOX_RT=$rt
  SANDBOX_CONTAINER="agent-repl-e2e-$$-$RANDOM"
  trap 'release_slot; remove_container' EXIT
  args+=(--name "$SANDBOX_CONTAINER")

  # FAILURE ARTIFACTS LEAVE THROUGH THE API TOO. They are written to a volume
  # that lives in the VM, and `docker cp` streams them out once the container
  # has exited; a bind mount would have the VM open host files to write them.
  # The image owns /artifacts as the sandbox uid, which a fresh volume copies.
  if [[ -n ${AGENT_REPL_E2E_ARTIFACTS:-} ]]; then
    mkdir -p "$AGENT_REPL_E2E_ARTIFACTS"
    args+=(--mount "type=volume,target=/artifacts")
    args+=(--env "AGENT_REPL_E2E_ARTIFACTS=/artifacts")
    log "artifacts -> $AGENT_REPL_E2E_ARTIFACTS (copied out after the run)"
  fi

  local module_src=$repo_root/$MODULE_REL status rc=0
  stage_check "$module_src"
  status=$(mktemp)
  args+=("$IMAGE" "$@")
  log "running: $*"
  if (( shell )); then
    shell_session "$rt" "$module_src" "$status" "${args[@]}"
    return 0
  fi
  # Logs stream straight through: no capture, no buffering, stderr stays
  # stderr so a failing test's output is the caller's output. Stdin is the
  # staged working copy, so no tty is allocated.
  set +e
  stage_stream "$module_src" "$status" | "$rt" "${args[@]}"
  rc=${PIPESTATUS[1]}
  set -e

  if [[ -n ${AGENT_REPL_E2E_ARTIFACTS:-} ]]; then
    "$rt" cp "$SANDBOX_CONTAINER:/artifacts/." "$AGENT_REPL_E2E_ARTIFACTS/" \
      || log "WARNING: copying the failure artifacts out of $SANDBOX_CONTAINER failed"
  fi

  local staged
  staged=$(cat "$status" 2>/dev/null || echo missing)
  rm -f "$status"
  if [[ $staged != 0 ]]; then
    die "streaming the working copy into the container FAILED (tar status: $staged); the run's result is not trusted"
  fi
  return "$rc"
}

# shell_session RT MODULE_SRC STATUS ARGS... runs the container (ARGS end with
# the image and a command that holds it open) on the streamed working copy,
# waits until the entrypoint says the copy is staged, and then gives the user
# an interactive login shell in it through `exec`. Stdin carries the tar, so
# the shell cannot be the container's own command.
#
# THE WAIT IS A LATCH, NOT A DELAY: the entrypoint writes its "staged" line to
# stderr once the copy is complete, and this blocks reading the container's
# stderr until that line arrives (or the container exits first).
shell_session() {
  local rt=$1 module_src=$2 status=$3 fifo line ready=0
  shift 3
  fifo=$(mktemp -u)
  mkfifo "$fifo"
  stage_stream "$module_src" "$status" | "$rt" "$@" 2>"$fifo" &
  exec 3<"$fifo"
  rm -f "$fifo"
  while IFS= read -r line <&3; do
    printf '%s\n' "$line" >&2
    if [[ $line == "sandbox: staged "* ]]; then
      ready=1
      break
    fi
  done
  (( ready )) || die "the container exited before its working copy was staged"
  # Keep draining the container's stderr, so it never blocks on a full pipe.
  cat <&3 >&2 &
  "$rt" exec -it --workdir "/work/repo/$MODULE_REL" "$SANDBOX_CONTAINER" bash -l || true
}

# --- dispatch --------------------------------------------------------------

main() {
  local cmd=${1:-}
  [[ $# -gt 0 ]] && shift || true
  case $cmd in
    build) do_build "$@" ;;
    run)
      [[ ${1:-} == -- ]] && shift
      do_run "$@"
      ;;
    shell) do_run --shell sleep infinity ;;
    preflight) preflight ;;
    ""|-h|--help)
      sed -n '2,20p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
      ;;
    *) die "unknown subcommand '$cmd'; one of build|run|shell|preflight" ;;
  esac
}

# SOURCED, THE SCRIPT DEFINES ITS FUNCTIONS AND RUNS NOTHING. The build-lock
# tests (e2e/sandboxscripts) source it to replace stage_context, whose tar of
# every .go file in the module is not what those tests assert.
if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
  main "$@"
fi
