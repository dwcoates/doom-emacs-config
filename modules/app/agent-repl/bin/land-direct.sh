#!/usr/bin/env bash
# land-direct.sh — land branches straight on a repository's main branch, for a
# workspace holding the repository's queue bypass mode
# (agentrepl.v1.UpdateQueueBypassMode), and for the deploy mediator. The
# workspace's agent has paused the queue before landing
# (metaprompt-queue-bypass.md); this script gates and lands.
#
# Usage: land-direct.sh REPO PART LINE BRANCH
#   Gate BRANCH (lib-land-gate.sh: checked out and clean in a worktree,
#   rebased onto REPO's main branch, the full suite passing on it), then merge
#   it into REPO's main branch in its main worktree (one --no-ff merge
#   commit), append LINE to the repository's remediation changelog (committed
#   on main), tag main with the next version (PART is minor or patch; LINE is
#   the tag's message) and print the new head and its version. PART is `none`
#   only for a repository whose lifecycle config versions nothing.
#
#   ONE BRANCH A LANDING: the suite passed on BRANCH's own tree, which is
#   what its merge lands only while it is the one branch merged.
#
# Exit codes:
#   0  landed
#   1  the branch did not merge cleanly; the merge was aborted and main is left
#      at its prior head (printed)
#   3  PART does not match whether the repository is versioned
#   4  the gate refused the branch (why is printed); nothing was merged
#   2  usage or operational error
set -uo pipefail

# shellcheck source=lib-land-gate.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib-land-gate.sh"

die() { echo "land-direct: $*" >&2; exit 2; }
log() { echo "[land-direct] $*" >&2; }

repo="${1:-}"; part="${2:-}"; line="${3:-}"
[[ -n "$repo" && -d "$repo" ]] || die "usage: land-direct.sh REPO PART LINE BRANCH"
git -C "$repo" rev-parse --git-dir >/dev/null 2>&1 || die "$repo is not a git repository"
shift 3 2>/dev/null || die "usage: land-direct.sh REPO PART LINE BRANCH"
(( $# == 1 )) || die "name exactly one branch to land, not $#: the suite passes one branch's tree"
branch="$1"
[[ -n "$line" ]] || die "name the changelog line"

main=$(main_worktree "$repo")
[[ -n "$main" ]] || die "could not find the main worktree of $repo"
versioned=false
grep -q 'version_tag:' "$main/.agent-repl/lifecycle.yaml" 2>/dev/null && versioned=true
case "$part" in
  minor|patch) $versioned || { echo "the lifecycle config of $main versions nothing; land with PART none"; exit 3; } ;;
  none) $versioned && { echo "the lifecycle config of $main versions every landing on main; land with PART minor or patch"; exit 3; } ;;
  *) die "PART is minor, patch or none, not '$part'" ;;
esac

target=$(git -C "$main" rev-parse --abbrev-ref HEAD) || die "could not read the branch of the main worktree $main"
[[ "$target" != HEAD ]] || die "the main worktree $main has no branch checked out"
git -C "$main" rev-parse --verify --quiet "refs/heads/$branch" >/dev/null || die "no such branch: $branch"
gate=0
refusal=$(land_gate "$main" "$branch") || gate=$?
case "$gate" in
  0) ;;
  "$LAND_GATE_REFUSED") echo "$refusal"; exit 4 ;;
  *) die "the gate could not judge $branch" ;;
esac
before=$(git -C "$main" rev-parse --short HEAD)
if ! git -C "$main" merge --no-ff -q -m "merge($target): $branch" "$branch"; then
  git -C "$main" merge --abort 2>/dev/null
  git -C "$main" reset -q --keep "$before" || die "could not restore $target to $before"
  echo "$branch did not merge cleanly into $target; $target is back at $before"
  exit 1
fi
log "merged $branch into $target"

# THE CHANGELOG LINE: the repository's remediation changelog, when it keeps one.
changelogs=$(git -C "$main" ls-files '*REMEDIATION-CHANGELOG.md') || die "could not list the changelogs of $main"
changelog=$(sed -n 1p <<<"$changelogs")
if [[ -n "$changelog" ]]; then
  printf -- '- %s %s (landed in queue bypass mode): %s\n' "$(date +%Y-%m-%d)" "$branch" "$line" >>"$main/$changelog"
  git -C "$main" add -- "$changelog" || die "could not stage $changelog"
  git -C "$main" commit -q -m "docs: changelog line for the queue-bypass landing of $branch" -- "$changelog" \
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
