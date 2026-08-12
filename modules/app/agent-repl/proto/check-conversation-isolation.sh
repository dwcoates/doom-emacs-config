#!/usr/bin/env bash
#
# INVARIANT I7 — CONVERSATION-INTERNAL ISOLATION, AS A BUILD GATE.
#
# A stored conversation record has two halves. agentshim.conversation.v1 is the
# half that may cross the shim→daemon wire; agentshim.conversation.internal.v1
# is the half that may not — which observation plane produced the record, the
# key the store deduped it on, and anything the producer could not convert.
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
# side by side in agentshim.conversation.v1 would both land in `conversationv1`,
# and a daemon importing the external half would get Plane and dedup_key in the
# same namespace for free — the file split would enforce nothing at all in Go.
# TypeScript would have honored it (one module per file); Go would not. So the
# internal half is its own proto package, hence its own Go import path and its
# own TS module, and this gate checks the one thing left: that nobody imports it
# from a runtime that has no business with it.
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
ROOT="${1:-$(cd "$THIS_DIR/.." && pwd)}"

# The proto package, and the Go import path its bindings generate into. Both
# forms are checked because a Go file names the import path, not the package.
PROTO_PKG="agentshim/conversation/internal/v1"
GO_PKG="agentrepl/proto/agentshim/conversation/internal/v1"

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
    awk -v proto_pkg="$PROTO_PKG" -v go_pkg="$GO_PKG" -v root="$ROOT/" '
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
        if (index(code, go_pkg) > 0 || index(code, proto_pkg) > 0) {
            name = FILENAME
            sub("^" root, "", name)
            printf "%s:%d: IMPORTS the conversation-internal package\n", name, FNR
        }
    }
    END { exit 0 }
    ' "${sources[@]}"
)"

if [ -n "$violations" ]; then
    printf 'conversation-isolation: INVARIANT I7 VIOLATED — %s is the half of a stored record that never crosses the shim wire, and no daemon or webapp source may import it.\n' "$PROTO_PKG" >&2
    printf '%s\n' "$violations" >&2
    printf 'conversation-isolation: a consumer reading the observation plane or the store dedup key is deciding from a shim implementation detail rather than from the record itself, which is the leak this split removed. Read the fact off the record it belongs to, or have the producer state it in an arm.\n' >&2
    exit 1
fi

printf 'conversation-isolation: %s is not imported by any daemon or webapp source\n' "$PROTO_PKG"
