# sandbox-image.sh -- WHICH image a checkout's sandbox runs in. Sourced, never
# run: e2e-sandbox.sh and preflight.sh both source it, so the two can never
# name different images for one checkout.
#
# ONE IMAGE PER SANDBOX SOURCE. The image's tag IS the stamp of the
# e2e/sandbox sources it was built from (agent-repl-e2e-sandbox:<stamp>). One
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

# sandbox_stamp prints the identity of THIS checkout's e2e/sandbox sources.
#
# The git tree hash is the honest answer when the sandbox directory is clean:
# it is exactly "these bytes, in this shape". When it is dirty, or there is no
# git at all, the tree hash would name sources that are NOT what would be
# built, so a content digest of the directory is computed instead and marked
# `dirty-`; two dirty checkouts with identical sandbox bytes still agree, and
# a dirty one never claims to be the committed tree.
sandbox_stamp() {
  local rel=$MODULE_REL/e2e/sandbox tree dirty

  if git -C "$repo_root" rev-parse --git-dir >/dev/null 2>&1; then
    dirty=$(git -C "$repo_root" status --porcelain -- "$rel" 2>/dev/null || echo dirty)
    if [[ -z $dirty ]]; then
      tree=$(git -C "$repo_root" rev-parse "HEAD:$rel" 2>/dev/null || true)
      if [[ -n $tree ]]; then
        printf '%s\n' "$tree"
        return 0
      fi
    fi
  fi

  # Content digest fallback: every file under the sandbox dir, by path and by
  # bytes, in a stable order.
  local digest
  digest=$( { cd -- "$sandbox_dir" && find . -type f -not -name '.*' -print0 \
      | LC_ALL=C sort -z \
      | xargs -0 shasum -a 256 2>/dev/null; } | shasum -a 256 | awk '{print $1}')
  printf 'dirty-%s\n' "$digest"
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
