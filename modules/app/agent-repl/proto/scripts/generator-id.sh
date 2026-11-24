#!/usr/bin/env bash
#
# generator-id.sh ES_LOCK SWIFT_PLUGIN — print the identity of every generator
# `make all` runs, one line each, so a stamp of these lines changes whenever
# any generator does (bin/build-frontend.sh build_proto).
#
# WHAT EACH LINE NAMES, and why that is the generator's whole identity:
#   protoc                 its --version and the bytes of the executable on
#                          PATH: it links its runtime library, whose version
#                          is the one --version reports;
#   protoc-gen-go          the bytes of the executable on PATH (a static Go
#   protoc-gen-connect-go  binary: its bytes are everything it does);
#   protoc-gen-es          node's version and the bytes of ES_LOCK, the
#                          lockfile npm writes describing every package it
#                          installed (protoc-gen-es and what it runs);
#   protoc-gen-swift       the bytes of SWIFT_PLUGIN, the plugin the Makefile
#                          built from the launcher's pinned swift-protobuf.
#
# The Makefile's `generator-id` target is the one caller: it names ES_LOCK and
# SWIFT_PLUGIN by the same variables its generation targets use, and brings
# both up to date first through the same rules.
#
# A generator that cannot be found or read fails the script, naming it: an
# identity that leaves a generator out would let its change go unseen.
set -euo pipefail

if [ "$#" -ne 2 ]; then
    printf 'generator-id: usage: generator-id.sh ES_LOCK SWIFT_PLUGIN\n' >&2
    exit 2
fi
es_lock="$1" swift_plugin="$2"

# sha256 FILE — the hex sha256 of FILE's bytes.
sha256() {
    local sum
    sum="$({ shasum -a 256 2>/dev/null || sha256sum; } < "$1")" || return 1
    printf '%s\n' "${sum%% *}"
}

# file_id NAME FILE — NAME and the hash of FILE, or fail naming NAME.
file_id() {
    local name="$1" file="$2" sum
    if [ ! -f "$file" ] || ! sum="$(sha256 "$file")" || [ -z "$sum" ]; then
        printf 'generator-id: cannot read %s at %s\n' "$name" "$file" >&2
        return 1
    fi
    printf '%s %s\n' "$name" "$sum"
}

# on_path NAME — the executable NAME resolves to on PATH, or fail naming it.
on_path() {
    local path
    if ! path="$(command -v "$1")"; then
        printf 'generator-id: %s is not on PATH\n' "$1" >&2
        return 1
    fi
    printf '%s\n' "$path"
}

protoc_path="$(on_path protoc)"
protoc_version="$(protoc --version)" || {
    printf 'generator-id: protoc --version failed\n' >&2
    exit 1
}
printf 'protoc-version %s\n' "$protoc_version"
file_id protoc "$protoc_path"
go_plugin="$(on_path protoc-gen-go)"
file_id protoc-gen-go "$go_plugin"
connect_plugin="$(on_path protoc-gen-connect-go)"
file_id protoc-gen-connect-go "$connect_plugin"
node_version="$(node --version)" || {
    printf 'generator-id: node --version failed\n' >&2
    exit 1
}
printf 'node %s\n' "$node_version"
file_id protoc-gen-es "$es_lock"
file_id protoc-gen-swift "$swift_plugin"
