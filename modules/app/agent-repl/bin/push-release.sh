#!/usr/bin/env bash
# push-release.sh — push master to origin as a release.
#
# Every push of master publishes the version the merge queue landed there, and
# origin records it as the lightweight tag release-<version> on the pushed
# commit. This tags master's tip release-<its version label> (the label is the
# queue's annotated version tag, e.g. 0.97.5) and pushes master and that tag in
# ONE atomic push, so origin takes both or neither.
#
# The pre-push hook (.githooks/pre-push) refuses any master push that lacks its
# release tag, and supplies version_label, the one label lookup both share.
#
#   push-release.sh     run from any worktree of the repository
#
# Exit status: 0 pushed; 1 refused (master's tip has no single version label,
# release-<version> already names another commit, or the push failed).
set -euo pipefail

HOOK="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)/.githooks/pre-push"
# shellcheck source=/dev/null
. "$HOOK"

readonly REMOTE="origin"

die() {
  echo "[push-release] $*" >&2
  exit 1
}

sha="$(git rev-parse --verify --quiet refs/heads/master)" || die "this repository has no master branch"
version="$(version_label "$sha")" || die "master's tip $sha has no single version label; land it through the merge queue"
tag="release-$version"

if existing="$(git rev-parse --verify --quiet "refs/tags/$tag")"; then
  [ "$existing" = "$sha" ] || die "$tag already names $existing, not master's tip $sha"
else
  git tag "$tag" "$sha"
fi

git push --atomic "$REMOTE" "refs/heads/master:refs/heads/master" "refs/tags/$tag:refs/tags/$tag" ||
  die "pushing master and $tag to $REMOTE failed; $REMOTE took neither"
echo "[push-release] pushed master at $version ($sha) to $REMOTE as $tag"
