# sandbox-image.sh -- WHICH image a checkout's sandbox runs in. Sourced, never
# run: e2e-sandbox.sh and preflight.sh both source it, so the two can never
# name different images for one checkout.
#
# ONE IMAGE PER SANDBOX SOURCE. The image's tag IS the stamp of the
# inputs it was built from (sandbox_stamp) (agent-repl-e2e-sandbox:<stamp>). One
# shared `:latest` tag made two checkouts whose sandbox sources differ (master
# and a branch cut before master's last sandbox change) mutually exclusive:
# building one evicted the other, and the refused checkout's e2e-emacs gate
# failed whatever its own code did. With a tag per stamp, every checkout finds
# or builds its own image, and no build can evict another checkout's.
#
# The sourcing script sets nothing first: this file resolves its own paths.

sandbox_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
repo_root=$(cd -- "$sandbox_dir/../../../../.." && pwd)
MODULE_REL=modules/app/agent-repl

# SANDBOX_IMAGE_REPO is the repository every sandbox image is tagged under.
SANDBOX_IMAGE_REPO=agent-repl-e2e-sandbox

SANDBOX_STAMP_LABEL=org.agent-repl.sandbox-tree

# sandbox_stamp prints the identity of everything THIS checkout's image is
# built from that the image cannot survive going stale: a content digest, by
# path and by bytes, over sandbox_stamp_inputs. Two checkouts whose inputs
# agree share an image; any input that differs names a different one.
#
# IT IS MORE THAN e2e/sandbox. The image bakes offline caches primed from
# files outside that directory -- the npm cache from the shim's and the
# webapp's package.json and package-lock.json, the Go module cache from every
# go.mod and go.sum, Doom's packages from packages.el -- and a run installs
# from those caches with the network off. A stamp of e2e/sandbox alone kept
# an image current across a lockfile change (2026-10-15: Stryker's
# dependencies, d170021689), and every run then failed `npm ci --offline` on
# a package the cache never held.
#
# Inputs whose staleness is harmless stay out, so they rebuild nothing: the
# Go sources (the build cache is content-addressed, so a stale entry only
# misses) and the module's elisp (the entrypoint replaces the baked module
# directory with the checkout's own at run time).
sandbox_stamp() {
  local digest
  digest=$( { cd -- "$repo_root/$MODULE_REL" && sandbox_stamp_inputs \
      | xargs -0 shasum -a 256; } | shasum -a 256 | awk '{print $1}')
  printf 'inputs-%s\n' "$digest"
}

# sandbox_stamp_inputs prints, NUL-separated, sorted and relative to the
# module, every file sandbox_stamp covers. A named input that is missing is a
# refusal: a stamp that silently skipped one would name an image it does not
# describe.
sandbox_stamp_inputs() {
  local f
  for f in agent-shim/claude/shim/package.json agent-shim/claude/shim/package-lock.json \
           webapp/package.json webapp/package-lock.json packages.el; do
    [[ -f $f ]] || { printf 'sandbox-image: the stamp input %s is missing\n' "$f" >&2; return 1; }
  done
  {
    sandbox_walk e2e/sandbox | sandbox_select_named_files sandbox
    printf '%s\0' agent-shim/claude/shim/package.json agent-shim/claude/shim/package-lock.json \
      webapp/package.json webapp/package-lock.json packages.el
    sandbox_walk . | sandbox_select_named_files go
  } | LC_ALL=C sort -z
}

# sandbox_walk prints, NUL-separated, every regular file under ROOT outside
# node_modules, without following symlinks. It walks with shell globs rather
# than find: the stamp runs on the LIVE checkout while other suites of the
# same run install packages into it, and find fails the whole walk when an
# entry it listed (npm's .node_modules.lock, 2026-10-16) vanishes before it
# stats it. A glob never stats what it lists, and an entry gone before its
# own test is simply not a file or a directory, so a concurrent writer cannot
# fail the stamp.
sandbox_walk() (
  shopt -s dotglob nullglob
  local -a dirs=("$1")
  local d e
  while ((${#dirs[@]})); do
    d=${dirs[${#dirs[@]}-1]}
    unset 'dirs[${#dirs[@]}-1]'
    for e in "$d"/*; do
      if [[ -L $e ]]; then
        continue
      elif [[ -d $e ]]; then
        [[ ${e##*/} == node_modules ]] || dirs+=("$e")
      elif [[ -f $e ]]; then
        printf '%s\0' "$e"
      fi
    done
  done
)

# sandbox_select_named_files filters sandbox_walk's NUL-separated paths by
# the stamp's two naming rules: `sandbox` keeps every file whose name does
# not start with a dot, and `go` keeps go.mod and go.sum.
sandbox_select_named_files() {
  local f name
  while IFS= read -r -d '' f; do
    name=${f##*/}
    case $1 in
      sandbox) [[ $name == .* ]] || printf '%s\0' "$f" ;;
      go) [[ $name == go.mod || $name == go.sum ]] && printf '%s\0' "$f" ;;
      *) printf 'sandbox-image: no naming rule %s\n' "$1" >&2; return 1 ;;
    esac
  done
  return 0
}

# sandbox_go_module_files prints, NUL-separated and relative to the module,
# every go.mod and go.sum outside node_modules.
sandbox_go_module_files() {
  sandbox_walk . | sandbox_select_named_files go
}

# sandbox_image prints the image THIS checkout's sandbox runs in: the one
# AGENT_REPL_SANDBOX_IMAGE names when it is set, deliberately, and otherwise
# the image tagged with this checkout's sandbox stamp.
sandbox_image() {
  if [[ -n ${AGENT_REPL_SANDBOX_IMAGE:-} ]]; then
    printf '%s\n' "$AGENT_REPL_SANDBOX_IMAGE"
    return 0
  fi
  printf '%s:%s\n' "$SANDBOX_IMAGE_REPO" "$(sandbox_stamp)"
}
