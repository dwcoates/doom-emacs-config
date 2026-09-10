#!/usr/bin/env bash
# The webapp dist the Emacs client layer serves, and the ONE staleness rule
# behind it.
#
#   webapp-dist.sh check    # exit 0 when dist/ is fresh, 1 when it is not
#   webapp-dist.sh ensure   # check, and BUILD on the host when it is not
#   webapp-dist.sh path     # print the dist directory
#
# WHY THIS EXISTS. The Emacs layer's panel is an `xwidget-webkit` webview
# pointed at the daemon's own origin, so the bytes it renders are the bytes
# the webapp build produces -- a stub dist would mean every webview assertion
# inspected a placeholder while reporting on the product. The layer therefore
# needs the REAL dist.
#
# It used to build that dist INSIDE the container, on every run:
# `npm run build` is `tsc --noEmit && vite build`, which took ~6s on an idle
# host and about a gigabyte of resident memory for tsc alone -- inside a
# 5.8 GiB Docker VM, on a tmpfs, while a real Emacs was about to start. That
# is the single largest reason two concurrent sandboxes could not coexist.
#
# So the dist is a HOST-STAGED artifact now, exactly like the shim bundle's
# build identity: the host builds it when it is stale, the entrypoint stages
# it into the working copy, and the container only ever READS it.
#
# STALENESS IS HONEST, AND IT IS ONE RULE. `check` is the rule; the host runs
# it before launching a container (and builds when it fails), and the harness
# inside the container runs THE SAME SCRIPT before handing the dist to the
# daemon. A stale dist is never silently served: on the host it rebuilds, in
# the container it fails loudly and names the command to run. The rule
# here is the older prerequisite-newer-than-target one -- artifact older than
# any source means stale. bin/build-frontend.sh no longer decides staleness
# that way (it compares source REVISIONS, because an mtime is wall-clock
# metadata a checkout can set to anything); this script keeps the timestamp
# rule because the container it also runs in has no checkout to compare
# against. It covers a superset of that script's source set:
# the generated protobuf TypeScript and the vocab JSON that `webapp/src`
# imports from `../../proto/` are sources here, because an edit to them
# changes the bundle and must therefore stale it.
set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
module_root=$(cd -- "$here/../../.." && pwd)
webapp=$module_root/webapp
dist=$webapp/dist
artifact=$dist/index.html

log() { printf 'webapp-dist: %s\n' "$*" >&2; }
die() { log "$*"; exit 2; }

[[ -d $webapp ]] || die "no webapp at $webapp"

# source_roots — every path whose change must invalidate the dist. Files and
# directories both, handed to one `find` as its start points.
#
# The set covers a superset of bin/build-frontend.sh's: `webapp/src` and the
# manifests it names, plus index.html and package-lock.json, plus the
# generated protobuf TypeScript and vocab JSON under `../../proto/` that
# `webapp/src` imports directly. An edit to any of them changes the bundle,
# so an edit to any of them must stale it.
source_roots() {
  local p
  printf '%s\0' "$webapp/src"
  for p in package.json package-lock.json tsconfig.json vite.config.ts \
           protobuf-runtime-aliases.ts index.html; do
    [[ -f $webapp/$p ]] && printf '%s\0' "$webapp/$p"
  done
  for p in "$module_root/proto/gen/ts" "$module_root/proto/vocab"; do
    [[ -d $p ]] && printf '%s\0' "$p"
  done
  return 0
}

# mtime FILE — the file's modification time in epoch seconds.
#
# THE ORDER OF THE TWO SPELLINGS MATTERS, and it is the opposite of the one
# bin/build-frontend.sh uses. That script is macOS-only, so it may lead with
# BSD's `stat -f %m`; this one runs on the developer's macOS host AND inside
# the Debian container, and on GNU coreutils `stat -f` means "stat the FILE
# SYSTEM", which SUCCEEDS -- it prints a filesystem report, the `||` never
# fires, and an arithmetic context then evaluates the word `File` as a
# variable and dies "File: unbound variable" under `set -u`. So GNU's
# spelling is tried first and the answer is checked to be a number, which
# makes a third stat dialect a loud failure instead of a silent zero.
mtime() {
  local t
  t=$(stat -c %Y "$1" 2>/dev/null) || t=$(stat -f %m "$1" 2>/dev/null) || t=
  [[ $t =~ ^[0-9]+$ ]] || die "cannot read the modification time of $1 (stat produced ${t:-nothing})"
  printf '%s\n' "$t"
}

# touch_at EPOCH FILE — stamp FILE with the given epoch-second mtime.
#
# Two dialects again, and the result is READ BACK rather than trusted: this
# stamp is the whole reference the staleness comparison is made against, so a
# `touch` that quietly did nothing would turn every check into "fresh".
touch_at() {
  local when=$1 file=$2 got
  : > "$file"
  touch -d "@$when" "$file" 2>/dev/null \
    || touch -t "$(date -r "$when" +%Y%m%d%H%M.%S 2>/dev/null)" "$file" 2>/dev/null \
    || die "cannot set a modification time on $file with this touch/date"
  got=$(mtime "$file")
  [[ $got == "$when" ]] || die "touch set $file to $got, not $when; the staleness reference is unusable"
}

# do_check — exit 0 fresh, 1 stale. The reason is always printed, because a
# reader who has just been told to rebuild needs to know what moved.
#
# ONE `find`, not a `stat` per file. The source set is around 1,100 files and
# forking `stat` over all of them cost 3.9s on the host -- a price every
# sandbox run would have paid before the container even started. `find
# -newer` asks the same question in one process, and `-quit` stops it at the
# first answer.
#
# THE REFERENCE IS THE ARTIFACT'S MTIME MINUS ONE SECOND, held in a scratch
# file rather than passed to `find -newermt`: BSD find (the developer's host)
# and GNU find (the container) do not agree on how to parse a bare `@epoch`,
# and the version that does not simply FAILS -- which, swallowed, reads as
# "nothing is newer, the dist is fresh". A reference FILE is the one spelling
# both accept. The minus one second is deliberate: `-newer` is
# strictly-greater, these timestamps are second-granularity, and a source
# written in the same second as the artifact must count as newer. That
# reproduces bin/build-frontend.sh's `>=` rule instead of loosening it.
do_check() {
  if [[ ! -f $artifact ]]; then
    log "STALE: no built webapp at $artifact"
    return 1
  fi
  local a roots=() r first ref
  a=$(mtime "$artifact")
  while IFS= read -r -d "" r; do roots+=("$r"); done < <(source_roots)
  ref=$(mktemp)
  # No `|| true` anywhere on this path: a find that fails must fail the
  # check, never report freshness by silence.
  touch_at "$((a - 1))" "$ref"
  first=$(find "${roots[@]}" -newer "$ref" -print -quit) || {
    rm -f "$ref"
    die "the staleness scan failed; refusing to guess whether $artifact is current"
  }
  rm -f "$ref"
  if [[ -n $first ]]; then
    log "STALE: $first is newer than $artifact"
    return 1
  fi
  return 0
}

# do_build delegates to the module's OWN build script rather than spelling
# `npm run build` a second time. bin/build-frontend.sh is what a developer and
# the deploy path both run: it links `node_modules` at the shared dependency
# store (a fresh worktree has none of its own, so a bare `npm run build` here
# would simply fail), writes the `.built-sha` and webapp build-id stamps
# beside the artifact, and builds through the same `npm run build` this file
# would otherwise duplicate. `--force` because the caller has ALREADY decided
# the artifact is stale, by the stricter rule above; leaving that decision to
# the script's own, looser rule would let it answer "fresh, skipping" to a
# question it was not asked.
do_build() {
  local builder=$module_root/bin/build-frontend.sh
  [[ -x $builder ]] || die "no build script at $builder"
  log "building the webapp dist via $builder --force webapp"
  "$builder" --force webapp
  [[ -f $artifact ]] || die "the webapp build produced no entry point at $artifact"
  # The build must actually have cleared the rule it was run for. A build
  # that leaves the artifact still older than a source is a broken rule, not
  # a fresh dist, and saying so here beats failing inside a container later.
  do_check || die "the webapp build finished but $artifact is still stale; the staleness rule and the build disagree"
  log "built $dist"
}

case ${1:-} in
  check) do_check ;;
  ensure)
    if do_check; then
      log "fresh: $dist"
    else
      do_build
    fi
    ;;
  path) printf '%s\n' "$dist" ;;
  *) die "usage: webapp-dist.sh check|ensure|path" ;;
esac
