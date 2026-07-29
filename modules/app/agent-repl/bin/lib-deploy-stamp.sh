#!/usr/bin/env bash

# shellcheck shell=bash
# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# lib-deploy-stamp.sh — the stamp vocabulary shared by build-frontend.sh,
# deploy-all.sh, and readiness-report.sh. Sourced, never executed.
#
# There are TWO INDEPENDENT stamp families here and conflating them is the
# whole reason this file exists:
#
#   1. BUILT-SHA stamps (`.built-sha` beside each artifact) answer "which
#      SOURCE REVISION is this artifact compiled from". Written by whoever
#      builds the artifact. Read by readiness-report.sh to compute how far
#      behind master a deployed artifact is. They say nothing about whether
#      anything is running that image.
#
#   2. DEPLOYED-FINGERPRINT stamps (`.<name>.deployed`) answer "is the RUNNING
#      launchd service executing the installed binary". Written by deploy-all.sh
#      at kickstart time, and they are the bounce-detection signal — never
#      rewrite one from a build step, or a `--no-bounce` install would look
#      deployed while the live process still serves the old image.
#
# Family 1 is additive; family 2 is pre-existing and must keep its exact
# semantics. Both live here so the two scripts cannot drift apart on either.

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

# ---- family 2: deployed-fingerprint stamps --------------------------------

binary_fingerprint() { shasum -a 256 "$1" | cut -d' ' -f1; }

# service_needs_bounce CACHE_BIN NAME — return 0 when the RUNNING service is
# not executing the installed binary.
#
# A stamp written at kickstart time, NOT "did this build change the file".
# Those are different questions, and conflating them is a real failure mode: a
# `--no-bounce` run installs a new binary without restarting anything, so the
# next run's build is "unchanged" while the live process is still on the old
# image — and the deploy silently skips the bounce the user asked for.
#
# A MISSING installed binary is not "in sync": there is nothing to be in sync
# with, so say bounce and let the caller decide.
service_needs_bounce() {
    local cache_bin="$1" name="$2"
    local stamp="$cache_bin/.$name.deployed"
    [ -f "$cache_bin/$name" ] || return 0
    [ -f "$stamp" ] || return 0
    [ "$(binary_fingerprint "$cache_bin/$name")" = "$(cat "$stamp")" ] && return 1
    return 0
}

record_service_deployed() {
    binary_fingerprint "$1/$2" > "$1/.$2.deployed"
}
