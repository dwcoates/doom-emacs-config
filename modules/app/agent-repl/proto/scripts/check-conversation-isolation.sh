#!/usr/bin/env bash
#
# INVARIANT I7 — CONVERSATION-INTERNAL ISOLATION, AS A BUILD GATE.
#
# A stored conversation record has two halves. shim.v1's ExternalEntry is the
# half that may cross the shim→daemon wire; store.v1 is the half that may
# not — which observation plane produced the record, the key the store deduped it
# on, and anything the producer could not convert.
#
# store.v1 IS ONE OF THE FIVE SURFACES, not a sub-namespace of a shared
# umbrella. It is named for what it holds — shim-side internals — which is why it
# cannot also be the prefix conversation, protocol, frontend and state hang under.
#
# WHY THIS EXISTS. The daemon used to read the observation plane in order to
# decide whether a turn boundary was authoritative. That is an abstraction leak
# with a shape: "is this boundary authoritative" is a property of the BOUNDARY,
# and answering it from a sibling field meant a shim implementation detail was
# load-bearing three runtimes away. The fix was to stop publishing the detail —
# but a fact that is merely absent from a struct comes back the moment someone
# needs it, so the boundary is enforced rather than documented.
#
# WHY A SEPARATE PACKAGE RATHER THAN A SEPARATE FILE. Every file in one proto
# package generates into ONE Go package. entry.proto and external.proto sitting
# side by side in one package would both land in one Go namespace, and a daemon
# importing the external half would get Plane for free — the file split would
# enforce nothing at all in Go. TypeScript would have honored it (one module per
# file); Go would not. So the shim-side half is its own proto package, hence its
# own Go import path and its own TS module, and this gate checks the one thing
# left: that nobody imports it from a runtime that has no business with it.
#
# WHY THE PACKAGE IS NOT CALLED `internal`. It was, briefly, and that was a bug
# this gate could not catch. Go's `internal/` path element means "importable
# only from code rooted at the parent directory" — which for a generated proto
# tree is the tree itself, so the shim, the sidecar AND the store were locked
# out of the type they exist to write. The keyword enforced the opposite of the
# intent, and enforced it so completely that nothing compiled far enough for
# this gate to ever fire.
#
# WHAT IT REFUSES: any file under a FORBIDDEN ROOT that imports the internal
# package, in .proto, .go or .ts form. The forbidden roots are the consumer-side
# runtimes — the daemon and the webapp. The shim, the sidecar and the store are
# the producers and the store: they are exactly who this package is FOR.
#
# WHAT IT DELIBERATELY ALLOWS. Prose is unconstrained. Comments are stripped
# before anything is matched, because the way an invariant like this is taught
# is by NAMING the forbidden package in the comments of the code that must not
# use it, and a gate that punished the documentation would be deleted in a week.
#
# Usage: check-conversation-isolation.sh [repo-root]
# Defaults to the agent-repl module containing this script.
#
# Every violation in the tree is reported before the script exits nonzero: a
# gate that stops at the first one turns a cleanup into N build runs.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="${1:-$(cd "$THIS_DIR/../.." && pwd)}"

# The proto package, and the Go import path its bindings generate into. Both
# forms are checked because a Go file names the import path, not the package.
PROTO_PKG="store/v1"
GO_PKG="agentrepl/proto/store/v1"
# The store's write-batch validation (proto/helpers/go/storewrite) imports
# store.v1, so a daemon importing it would reach the storage half and the write
# batch without ever naming store/v1. It is the producers' and the store's,
# never the daemon's: the daemon's relationship with the store is read-only.
WRITE_PKG="agentrepl/protohelpers/storewrite"

if [ ! -d "$ROOT" ]; then
    printf 'conversation-isolation: root %s does not exist\n' "$ROOT" >&2
    exit 2
fi

# The runtimes that consume the external half and must never see the internal
# one. Producers (shim, sidecar) and the store itself are deliberately absent.
FORBIDDEN_ROOTS=(daemon webapp)

present=0
for rel in "${FORBIDDEN_ROOTS[@]}"; do
    [ -d "$ROOT/$rel" ] && present=1
done
if [ "$present" -eq 0 ]; then
    printf 'conversation-isolation: none of the forbidden roots (%s) exist under %s — the gate would pass vacuously\n' "${FORBIDDEN_ROOTS[*]}" "$ROOT" >&2
    exit 2
fi

sources=()
for rel in "${FORBIDDEN_ROOTS[@]}"; do
    [ -d "$ROOT/$rel" ] || continue
    while IFS= read -r -d '' f; do
        sources+=("$f")
    done < <(find "$ROOT/$rel" \
        \( -name node_modules -o -name gen -o -name dist -o -name .git \) -prune -o \
        \( -name '*.go' -o -name '*.ts' -o -name '*.proto' \) -type f -print0 | sort -z)
done

if [ "${#sources[@]}" -eq 0 ]; then
    printf 'conversation-isolation: no sources found under %s — the gate would pass vacuously\n' "${FORBIDDEN_ROOTS[*]}" >&2
    exit 2
fi

# One awk pass. Comments are stripped with the same carry-across-lines logic the
# durable-isolation gate uses, so "mentioned in prose" and "imported in code"
# cannot disagree about what counts as code.
violations="$(
    awk -v proto_pkg="$PROTO_PKG" -v go_pkg="$GO_PKG" -v write_pkg="$WRITE_PKG" -v root="$ROOT/" '
    function strip(line,   out, p, b, l) {
        out = ""
        while (length(line) > 0) {
            if (inblock) {
                p = index(line, "*/")
                if (p == 0) { return out }
                inblock = 0
                line = substr(line, p + 2)
                continue
            }
            b = index(line, "/*")
            l = index(line, "//")
            if (l > 0 && (b == 0 || l < b)) { return out substr(line, 1, l - 1) }
            if (b > 0) {
                out = out substr(line, 1, b - 1)
                line = substr(line, b + 2)
                inblock = 1
                continue
            }
            return out line
        }
        return out
    }
    FNR == 1 { inblock = 0 }
    {
        code = strip($0)
        if (code == "") { next }
        if (index(code, go_pkg) > 0 || index(code, proto_pkg) > 0 || index(code, write_pkg) > 0) {
            name = FILENAME
            sub("^" root, "", name)
            printf "%s:%d: IMPORTS the conversation-shimside package\n", name, FNR
        }
    }
    END { exit 0 }
    ' "${sources[@]}"
)"

# INVARIANT I8 — THE DAEMON'S VIEW OF THE STORE IS READ-ONLY (owner ruling,
# 2026-10-27). storeread.v1 is the one package the daemon reads the store
# through, and it stays read-only by holding nothing but the conversation
# vocabulary: a storeread/v1 file may import conversation/v1 and its own
# package, and nothing else. A store.v1 import would hand the daemon the
# storage half and the write batch through the back door; any other import is
# a new surface this package has no business carrying. The store serves it from
# a handler holding only its read-only pool, so no verb declared here can
# write; this half keeps the types that could describe a write out of reach.
STOREREAD_DIR="$ROOT/proto/src/storeread"
if [ ! -d "$STOREREAD_DIR" ]; then
    printf 'conversation-isolation: %s does not exist — the read-only gate would pass vacuously\n' "$STOREREAD_DIR" >&2
    exit 2
fi
storeread_sources=()
while IFS= read -r -d '' f; do
    storeread_sources+=("$f")
done < <(find "$STOREREAD_DIR" -name '*.proto' -type f -print0 | sort -z)
if [ "${#storeread_sources[@]}" -eq 0 ]; then
    printf 'conversation-isolation: no .proto files under %s — the read-only gate would pass vacuously\n' "$STOREREAD_DIR" >&2
    exit 2
fi
storeread_violations="$(
    awk -v root="$ROOT/" '
    /^[[:space:]]*import[[:space:]]/ {
        line = $0
        sub("//.*$", "", line)
        if (match(line, /"[^"]*"/) == 0) { next }
        path = substr(line, RSTART + 1, RLENGTH - 2)
        if (index(path, "conversation/v1/") == 1 || index(path, "storeread/v1/") == 1) { next }
        name = FILENAME
        sub("^" root, "", name)
        printf "%s:%d: imports %s\n", name, FNR, path
    }
    ' "${storeread_sources[@]}"
)"

status=0
if [ -n "$violations" ]; then
    printf 'conversation-isolation: INVARIANT I7 VIOLATED — %s is the half of a stored record that never crosses the shim wire, and no daemon or webapp source may import it.\n' "$PROTO_PKG" >&2
    printf '%s\n' "$violations" >&2
    printf 'conversation-isolation: a consumer reading the observation plane or the store dedup key is deciding from a shim implementation detail rather than from the record itself, which is the leak this split removed. Read the fact off the record it belongs to, or have the producer state it in an arm.\n' >&2
    status=1
fi
if [ -n "$storeread_violations" ]; then
    printf 'conversation-isolation: INVARIANT I8 VIOLATED — storeread.v1 is the daemon’s read-only view of the store, and its files may import only conversation/v1 and storeread/v1.\n' >&2
    printf '%s\n' "$storeread_violations" >&2
    printf 'conversation-isolation: the daemon never writes the store and never sees its storage types; a fact it needs is served in the conversation vocabulary, and a write belongs to a producer through store.v1.WriteBatch.\n' >&2
    status=1
fi
if [ "$status" -ne 0 ]; then
    exit "$status"
fi

printf 'conversation-isolation: %s is not imported by any daemon or webapp source, and storeread.v1 imports only the conversation vocabulary\n' "$PROTO_PKG"
