#!/usr/bin/env bash
# land-direct.sh — land branches straight on a repository's main branch, for a
# workspace holding the repository's queue bypass mode
# (agentrepl.v1.UpdateQueueBypassMode). The queue is already hard-stopped by
# the mode; this script only lands.
#
# Usage: land-direct.sh REPO PART LINE BRANCH...
#   Merge each BRANCH, in order, into REPO's main branch in its main worktree
#   (one --no-ff merge commit each), append LINE to the repository's
#   remediation changelog (committed on main), tag main with the next version
#   (PART is minor or patch; LINE is the tag's message) and print the new head
#   and its version. PART is `none` only for a repository whose deploy config
#   versions nothing.
#
# Exit codes:
#   0  landed
#   1  a branch did not merge cleanly; the merge was aborted and main is left
#      at its prior head (printed)
#   3  PART does not match whether the repository is versioned
#   2  usage or operational error
set -uo pipefail

die() { echo "land-direct: $*" >&2; exit 2; }
log() { echo "[land-direct] $*" >&2; }

repo="${1:-}"; part="${2:-}"; line="${3:-}"
[[ -n "$repo" && -d "$repo" ]] || die "usage: land-direct.sh REPO PART LINE BRANCH..."
git -C "$repo" rev-parse --git-dir >/dev/null 2>&1 || die "$repo is not a git repository"
shift 3 2>/dev/null || die "usage: land-direct.sh REPO PART LINE BRANCH..."
(( $# > 0 )) || die "name at least one branch to land"
[[ -n "$line" ]] || die "name the changelog line"

main=$(git -C "$repo" worktree list --porcelain | awk '/^worktree /{print substr($0, 10); exit}')
[[ -n "$main" ]] || die "could not find the main worktree of $repo"
versioned=false
grep -q 'version_tag:' "$main/.agent-repl/deploy.yaml" 2>/dev/null && versioned=true
case "$part" in
  minor|patch) $versioned || { echo "the deploy config of $main versions nothing; land with PART none"; exit 3; } ;;
  none) $versioned && { echo "the deploy config of $main versions every landing on main; land with PART minor or patch"; exit 3; } ;;
  *) die "PART is minor, patch or none, not '$part'" ;;
esac

target=$(git -C "$main" symbolic-ref --short HEAD) || die "the main worktree $main has no branch checked out"
before=$(git -C "$main" rev-parse --short HEAD)
for branch in "$@"; do
  git -C "$main" rev-parse --verify --quiet "$branch^{commit}" >/dev/null || die "no such branch: $branch"
  if ! git -C "$main" merge --no-ff -q -m "merge($target): $branch" "$branch"; then
    git -C "$main" merge --abort 2>/dev/null
    git -C "$main" reset -q --keep "$before" || die "could not restore $target to $before"
    echo "$branch did not merge cleanly into $target; $target is back at $before"
    exit 1
  fi
  log "merged $branch into $target"
done

# THE CHANGELOG LINE: the repository's remediation changelog, when it keeps one.
changelog=$(git -C "$main" ls-files '*REMEDIATION-CHANGELOG.md' | head -1)
if [[ -n "$changelog" ]]; then
  printf -- '- %s %s (landed in queue bypass mode): %s\n' "$(date +%Y-%m-%d)" "$(IFS=,; echo "$*")" "$line" >>"$main/$changelog"
  git -C "$main" add -- "$changelog" || die "could not stage $changelog"
  git -C "$main" commit -q -m "docs: changelog line for the queue-bypass landing of $*" -- "$changelog" \
    || die "could not commit the changelog line to $changelog"
  log "appended the changelog line to $changelog"
else
  log "the repository keeps no remediation changelog; the line rides the version tag only"
fi

version=""
if [[ "$part" != none ]]; then
  previous=$(git -C "$main" tag --merged HEAD | grep -E '^[0-9]+\.[0-9]+\.[0-9]+$' | sort -t. -k1,1n -k2,2n -k3,3n | tail -1)
  [[ -n "$previous" ]] || die "no version tag is reachable from $target; tag it by hand"
  IFS=. read -r major minor patch <<<"$previous"
  if [[ "$part" == minor ]]; then version="$major.$((minor + 1)).0"; else version="$major.$minor.$((patch + 1))"; fi
  git -C "$main" tag -a "$version" -m "$line" HEAD || die "could not tag $target with $version"
  log "tagged $target $version (from $previous)"
fi
echo "$(git -C "$main" rev-parse --short HEAD) ${version:-unversioned}"
