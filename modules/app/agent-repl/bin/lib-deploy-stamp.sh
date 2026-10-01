#!/usr/bin/env bash

# shellcheck shell=bash
# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# lib-deploy-stamp.sh — the stamp vocabulary shared by build-frontend.sh and
# readiness-report.sh. Sourced, never executed.
#
# There are THREE INDEPENDENT stamp families here and conflating them is the
# whole reason this file exists:
#
#   1. BUILT-SHA stamps (`.built-sha` beside each artifact) answer "which
#      SOURCE REVISION is this artifact compiled from". Written by whoever
#      builds the artifact. Read by readiness-report.sh to compute how far
#      behind master a deployed artifact is. They say nothing about whether
#      anything is running that image.
#
#   2. SERVICE BUILD REPORTS (`<run dir>/<name>.build.json`) answer "is the
#      RUNNING launchd service executing the installed binary". Written by the
#      service ITSELF at boot (agent-shim/logging/go/buildreport): its pid and
#      the sha256 of the binary it was exec'd from. They are the
#      bounce-detection signal, and nothing here ever writes one — a report
#      written by anyone but the running process could say it runs a build it
#      does not.
#
#   3. SOURCE-TREE stamps (`.source-tree` beside each artifact) answer "which
#      SOURCE CONTENT is this artifact built from". They are what decides
#      whether a build is stale, and they are read by BOTH build-frontend.sh
#      (to decide whether to rebuild) and readiness-report.sh (to decide
#      whether the artifact is ready), off ONE pathspec table that also lives
#      here. That sharing is the point: a staleness rule and a readiness gate
#      that computed their own source sets could — and did — disagree, leaving
#      the gate reporting a system behind while the build insisted it was fresh
#      and refused to rebuild it.
#
# Family 2's file is owned by the Go package above, which the daemon's deploy
# (daemon/internal/deploy) reads too; the shape read here is that package's.
# All three live here so the scripts cannot drift apart on any.

# ---- family 1: built-sha stamps -------------------------------------------

# source_revision DIR — echo the revision DIR's checkout is on, as
# "<sha>" or "<sha>-dirty" when DIR's subtree has uncommitted changes.
#
# Returns 1 (printing nothing) when DIR is not a git checkout or git is
# unavailable. Callers MUST treat that as "no stamp", never as a default:
# a guessed revision is worse than an absent one, because a report cannot
# tell a guess from a fact.
#
# Dirtiness is scoped to DIR's own subtree, not the whole repository: an
# unrelated edit elsewhere in a large checkout does not make THIS artifact a
# dirty build, and the repo-wide check would also cost a full-tree status on
# every build.
source_revision() {
    local dir="$1" sha
    command -v git >/dev/null 2>&1 || return 1
    sha="$(git -C "$dir" rev-parse HEAD 2>/dev/null)" || return 1
    [ -n "$sha" ] || return 1
    if [ -n "$(git -C "$dir" status --porcelain -- "$dir" 2>/dev/null)" ]; then
        printf '%s-dirty\n' "$sha"
    else
        printf '%s\n' "$sha"
    fi
}

# write_built_sha STAMP_FILE SOURCE_DIR — record SOURCE_DIR's revision beside a
# freshly built artifact.
#
# When the revision cannot be determined, any EXISTING stamp is removed rather
# than left in place: the artifact was just rebuilt, so an old stamp now
# describes a revision the artifact is not built from, which is exactly the
# guess the report must never see.
write_built_sha() {
    local stamp="$1" dir="$2" value
    if ! value="$(source_revision "$dir")"; then
        rm -f "$stamp"
        return 0
    fi
    mkdir -p "$(dirname "$stamp")"
    printf '%s\n' "$value" > "$stamp"
}

# write_built_sha_value STAMP_FILE VALUE — record an ALREADY-COMPUTED revision.
#
# This exists so a build that BAKES its revision into the artifact can stamp the
# very same value it baked, instead of computing the revision a second time and
# hoping the two agree. They can genuinely disagree: `source_revision` reports
# `-dirty` off a live working tree, which a build can enter and leave.
#
# An empty VALUE means "no revision", and any existing stamp is removed for the
# same reason write_built_sha removes one — an old stamp now describes a
# revision the artifact is not built from.
write_built_sha_value() {
    local stamp="$1" value="$2"
    if [ -z "$value" ]; then
        rm -f "$stamp"
        return 0
    fi
    mkdir -p "$(dirname "$stamp")"
    printf '%s\n' "$value" > "$stamp"
}

# read_built_sha STAMP_FILE — echo the recorded revision, or return 1 when the
# stamp is missing or empty.
read_built_sha() {
    local value
    [ -f "$1" ] || return 1
    IFS= read -r value < "$1" || true
    [ -n "$value" ] || return 1
    printf '%s\n' "$value"
}

# built_sha_is_dirty VALUE — return 0 when a stamp value carries the dirty marker.
built_sha_is_dirty() {
    case "$1" in *-dirty) return 0 ;; *) return 1 ;; esac
}

# built_sha_commit VALUE — echo the bare 40-hex commit, dirty marker stripped.
built_sha_commit() { printf '%s\n' "${1%-dirty}"; }

# ---- family 2: service build reports --------------------------------------

binary_fingerprint() { shasum -a 256 "$1" | cut -d' ' -f1; }

# service_report_dir — the run directory the services write their reports in:
# $AGENT_REPL_LOCK_DIR when set, else ~/.cache/agent-repl/run. The same rule as
# buildreport.ResolveDir, so a harness that isolates the kernel locks isolates
# the reports with them.
service_report_dir() {
    if [ -n "${AGENT_REPL_LOCK_DIR:-}" ]; then
        printf '%s\n' "$AGENT_REPL_LOCK_DIR"
    else
        printf '%s\n' "$HOME/.cache/agent-repl/run"
    fi
}

# read_service_report FILE — echo "<pid> <build>" from a report, or return 1
# when it does not parse.
#
# The report is written by Go's encoding/json from a two-field struct, so its
# shape is exact and compact: {"pid":<n>,"build":"<hex>"}. A sed over that
# shape is the whole parser; anything else — a truncated file, a hand edit, a
# future field — fails to match and is reported as unparseable, never read as
# a partial answer.
read_service_report() {
    local parsed
    parsed="$(sed -n 's/^{"pid":\([1-9][0-9]*\),"build":"\([0-9a-f]\{64\}\)"}$/\1 \2/p' "$1" 2>/dev/null)"
    [ -n "$parsed" ] || return 1
    printf '%s\n' "$parsed"
}

# service_needs_bounce CACHE_BIN NAME — return 0 when the RUNNING service is
# not executing the installed binary, 1 when it is.
#
# The authority is the service's own report, not "did this build change the
# file": an install that restarted nothing leaves a new binary on disk while
# the live process still serves the old image, and only the process can say
# which one it runs. Needs a bounce when ANY of these holds:
#   - the installed binary CACHE_BIN/NAME is missing: there is nothing to be in
#     sync with, so say bounce and let the caller decide;
#   - no report exists: the service never booted, or predates reporting;
#   - the report does not parse (also warned on stderr — a report that exists
#     and cannot be read is a fault, never silently fresh);
#   - the reporting pid is not alive: nothing is running that build;
#   - the reported build is not the sha256 of the installed binary.
service_needs_bounce() {
    local cache_bin="$1" name="$2"
    local report parsed pid build
    [ -f "$cache_bin/$name" ] || return 0
    report="$(service_report_dir)/$name.build.json"
    [ -f "$report" ] || return 0
    if ! parsed="$(read_service_report "$report")"; then
        echo "lib-deploy-stamp: WARNING: unparseable service build report $report; treating $name as needing a bounce" >&2
        return 0
    fi
    pid="${parsed%% *}"
    build="${parsed#* }"
    kill -0 "$pid" 2>/dev/null || return 0
    [ "$(binary_fingerprint "$cache_bin/$name")" = "$build" ] && return 1
    return 0
}

# ---- family 3: source-tree stamps ------------------------------------------
#
# WHY NOT MTIMES. Staleness used to be `make`'s rule done by hand: rebuild when
# any file under a hand-listed source directory is newer than the artifact.
# That rule has two independent ways to answer "fresh" about a stale artifact,
# and on 2026-09-09 both fired at once on a real deploy:
#
#   - The hand-listed set was NARROWER than the set the readiness report
#     attributes to the same system. The shim scanned only `shim/src` plus a
#     few manifests, while the report counted all of `shim/`, `proto/` and
#     `agent-shim/logging/`. Twelve merges landed changes under `shim/test/`
#     and `agent-shim/logging/go/`; the build saw nothing newer in its narrow
#     set and skipped, and the gate then reported the shim three commits
#     behind — a state no un-forced rebuild could ever clear.
#   - An mtime is wall-clock metadata, not content. A checkout, a rebase, a
#     `git restore`, or a copy can hand a changed file an OLD mtime, and a
#     staging step can hand an unchanged artifact a NEW one.
#
# So staleness is decided by SOURCE REVISION instead: the hash of the git index
# entries for the system's pathspec — the very pathspec the readiness report
# attributes to it — with a dirty working tree always reading as stale.

# deploy_stamp_rel_root REPO_ROOT MODULE_ROOT — where the module sits inside the
# checkout, as a repo-relative prefix ("" when the module IS the top level).
#
# Derived rather than hardcoded as "modules/app/agent-repl": the layout is the
# repo's to change, and a stale hardcoded prefix would make every pathspec
# silently match nothing — which reads as "fully deployed".
deploy_stamp_rel_root() {
    local repo="$1" mod="$2" rel
    rel="${mod#"$repo"/}"
    if [ "$rel" = "$mod" ]; then rel=""; fi
    printf '%s' "$rel"
}

# deploy_stamp_prefix PATH REL_ROOT — a module-relative path as a repo-relative
# pathspec.
deploy_stamp_prefix() {
    if [ -n "$2" ]; then printf '%s/%s' "$2" "$1"; else printf '%s' "$1"; fi
}

# deploy_stamp_proto_paths REL_ROOT — the proto tree as a BUILD input: the
# schemas minus the review artifacts that live beside them. figma-idl-draft/
# and the sketch are design documents no build reads, so a commit touching only
# them must not make any system read as behind — that state is undeployable by
# rebuilding, because the staleness check (correctly) sees no buildable input
# change and the stamp can never catch up to the gate.
deploy_stamp_proto_paths() {
    printf '%s %s %s' "$(deploy_stamp_prefix proto "$1")" \
        ":(exclude)$(deploy_stamp_prefix proto/figma-idl-draft "$1")" \
        ":(exclude)$(deploy_stamp_prefix proto/SKETCH-figma-idl.md "$1")"
}

# deploy_stamp_system_paths NAME REL_ROOT — the repo-relative pathspec that IS
# the system's source set, space separated. No path in this repo contains a
# space, and keeping them in one string is what lets bash 3.2 (still /bin/bash
# on macOS, no associative arrays) carry a per-system table.
#
# THIS TABLE IS THE SINGLE DEFINITION. build-frontend.sh decides staleness from
# it and readiness-report.sh decides readiness from it; a second copy anywhere
# reopens the drift this file exists to close.
deploy_stamp_system_paths() {
    local rel="$2"
    case "$1" in
        # The daemon compiles the test runner's roster in (the merge gate's
        # suite selection), so the roster package and its go.mod are inputs.
        daemon)              printf '%s %s %s %s %s' "$(deploy_stamp_prefix daemon "$rel")" "$(deploy_stamp_proto_paths "$rel")" "$(deploy_stamp_prefix agent-shim/logging "$rel")" "$(deploy_stamp_prefix testrun/roster "$rel")" "$(deploy_stamp_prefix testrun/go.mod "$rel")" ;;
        shim)                printf '%s %s %s' "$(deploy_stamp_prefix agent-shim/claude/shim "$rel")" "$(deploy_stamp_proto_paths "$rel")" "$(deploy_stamp_prefix agent-shim/logging "$rel")" ;;
        webapp)              printf '%s %s %s' "$(deploy_stamp_prefix webapp "$rel")" "$(deploy_stamp_proto_paths "$rel")" "$(deploy_stamp_prefix agent-shim/logging "$rel")" ;;
        shim-store)          printf '%s %s %s' "$(deploy_stamp_prefix agent-shim/shim-store "$rel")" "$(deploy_stamp_prefix agent-shim/logging "$rel")" "$(deploy_stamp_proto_paths "$rel")" ;;
        shim-claude-sidecar) printf '%s %s %s' "$(deploy_stamp_prefix agent-shim/claude/shim-sidecar "$rel")" "$(deploy_stamp_prefix agent-shim/logging "$rel")" "$(deploy_stamp_proto_paths "$rel")" ;;
        # No proto: shim-lock speaks no wire at all. Its whole contract is argv,
        # one stdout line, stdin's EOF and an exit code.
        shim-lock)           printf '%s %s' "$(deploy_stamp_prefix agent-shim/shim-lock "$rel")" "$(deploy_stamp_prefix agent-shim/logging "$rel")" ;;
        *) return 1 ;;
    esac
}

# source_tree_id REPO_ROOT PATHSPEC... — echo a stable id for the CONTENT of
# the given pathspec: a hash of the index entries (mode, blob sha, path), with
# "-dirty" appended when the working tree disagrees with the index anywhere
# under it.
#
# Returns 1 (printing nothing) when git is unavailable, REPO_ROOT is not a
# checkout, or the pathspec matches NO tracked file. Every one of those means
# "the source set could not be determined", and callers MUST treat it as stale
# rather than as fresh: a pathspec that silently matches nothing is precisely
# how a build talks itself out of work it owes.
source_tree_id() {
    local repo="$1"; shift
    local listing hash
    command -v git >/dev/null 2>&1 || return 1
    listing="$(git -C "$repo" ls-files -s -- "$@" 2>/dev/null)" || return 1
    [ -n "$listing" ] || return 1
    hash="$(printf '%s\n' "$listing" | { shasum -a 256 2>/dev/null || sha256sum; } | cut -c1-40)"
    [ -n "$hash" ] || return 1
    if [ -n "$(git -C "$repo" status --porcelain -- "$@" 2>/dev/null)" ]; then
        printf '%s-dirty\n' "$hash"
    else
        printf '%s\n' "$hash"
    fi
}

# source_tree_is_dirty VALUE — return 0 when a source-tree id carries the dirty
# marker. A dirty id never matches a stamp, so a dirty tree always rebuilds.
source_tree_is_dirty() {
    case "$1" in *-dirty) return 0 ;; *) return 1 ;; esac
}

# source_tree_base VALUE — echo a source-tree id with the dirty marker stripped,
# i.e. the COMMITTED content the id is anchored to.
#
# Comparing bases is what keeps a dirty tree deployable. A build off a dirty
# tree is stale by definition (the id cannot describe uncommitted content, so
# build-frontend.sh rebuilds every time), but a readiness gate that refused to
# pass while a tree was dirty would refuse every deploy a developer makes from a
# working checkout, and no rebuild could ever clear it. So staleness compares
# full ids and readiness compares bases, and the uncertainty is REPORTED (as the
# dirty flag) rather than turned into a block.
source_tree_base() { printf '%s\n' "${1%-dirty}"; }

# write_source_tree STAMP_FILE VALUE — record the source-tree id an artifact was
# just built from. An empty VALUE removes any existing stamp, for the same
# reason write_built_sha does: a stamp that no longer describes the artifact
# beside it is worse than no stamp, because nothing downstream can tell the two
# apart.
write_source_tree() {
    local stamp="$1" value="$2"
    if [ -z "$value" ]; then
        rm -f "$stamp"
        return 0
    fi
    mkdir -p "$(dirname "$stamp")"
    printf '%s\n' "$value" > "$stamp"
}

# read_source_tree STAMP_FILE — echo the recorded id, or return 1 when the stamp
# is missing or empty.
read_source_tree() {
    local value
    [ -f "$1" ] || return 1
    IFS= read -r value < "$1" || true
    [ -n "$value" ] || return 1
    printf '%s\n' "$value"
}
