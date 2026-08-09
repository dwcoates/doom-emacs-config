#!/usr/bin/env bash
#
# INVARIANT I6 — DURABLE ISOLATION, AS A BUILD GATE.
#
# durable.proto is the persistence evidence layer. Its own header states the
# invariant that makes it real: "no message on the push surface names a type
# declared in this file". That sentence was, until this script existed, a
# comment — enforced by whoever happened to read it. The moment one component
# message names a durable type, the frontend becomes the thing that decides
# what a vendor-faithful audit record MEANS, and the second frontend to do so
# decides differently. Worse, the durable shapes are frozen for byte-identical
# replay, so a renderer that starts depending on one freezes the RENDERER too.
#
# So the coupling is refused at CODEGEN time rather than at review time: this
# check is the `codegen-gate` target, which `make go`, `make ts`, and `make
# lint` all require, so generated bindings cannot be produced from a schema that
# has already drifted by ANY Makefile route — not merely by the linting one.
#
# WHAT IT REFUSES, for every frontend/v1 file except durable.proto itself:
#
#   - importing durable.proto, and
#   - naming any message DECLARED in durable.proto.
#
# The second is the one that matters: proto has no visibility modifier, so a
# file that imports something else which imports durable.proto could still
# name a durable type without an import line of its own.
#
# WHAT IT DELIBERATELY ALLOWS. durable.proto's own imports are unconstrained —
# the isolation is ONE-DIRECTIONAL, and durable.proto importing tokens.proto is
# exactly the derivation direction the layering wants. Prose is unconstrained
# too: comments are stripped before anything is matched, because the way this
# invariant is taught is by NAMING the durable types in the comments of the
# files that must not use them, and a gate that punished the documentation
# would be deleted within a week.
#
# Usage: check-durable-isolation.sh [component-dir] [durable-basename]
# Defaults to this repository's agentshim/frontend/v1 and durable.proto.
#
# Every violation in the tree is reported before the script exits nonzero: a
# gate that stops at the first one turns a schema cleanup into N build runs.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DIR="${1:-$THIS_DIR/agentshim/frontend/v1}"
DURABLE_BASE="${2:-durable.proto}"
DURABLE="$DIR/$DURABLE_BASE"

if [ ! -d "$DIR" ]; then
    printf 'durable-isolation: component directory %s does not exist\n' "$DIR" >&2
    exit 2
fi
if [ ! -f "$DURABLE" ]; then
    printf 'durable-isolation: durable schema %s does not exist\n' "$DURABLE" >&2
    exit 2
fi

# The siblings are collected NUL-safely rather than through word splitting: the
# self-test drives this script against a fixture under TMPDIR, and a temporary
# directory with a space in it would otherwise silently scan nothing and pass.
siblings=()
while IFS= read -r -d '' proto; do
    siblings+=("$proto")
done < <(find "$DIR" -maxdepth 1 -name '*.proto' ! -name "$DURABLE_BASE" -print0 | sort -z)

if [ "${#siblings[@]}" -eq 0 ]; then
    printf 'durable-isolation: no sibling component schemas found beside %s in %s — the gate would pass vacuously\n' "$DURABLE_BASE" "$DIR" >&2
    exit 2
fi

# The scan is one awk pass over the durable file (to learn the declared names)
# followed by one pass per sibling. Comment stripping is shared by both, which
# is what keeps "declared here" and "referenced there" from disagreeing about
# what is code.
violations="$(
    awk -v durable="$DURABLE" -v durable_base="$DURABLE_BASE" '
    # strip removes block and line comments from one physical line, carrying
    # block state across lines in inblock.
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
    # Pass one: the durable file names everything the siblings may not name.
    FILENAME == durable {
        code = strip($0)
        if (match(code, /^[ \t]*message[ \t]+[A-Za-z_][A-Za-z0-9_]*/)) {
            n = code
            sub(/^[ \t]*message[ \t]+/, "", n)
            sub(/[^A-Za-z0-9_].*$/, "", n)
            declared[n] = 1
        }
        next
    }
    # Pass two: every other component file.
    {
        code = strip($0)
        if (code ~ /^[ \t]*import[ \t]/) {
            path = code
            sub(/^[^"]*"/, "", path)
            sub(/".*$/, "", path)
            base = path
            sub(/^.*\//, "", base)
            if (base == durable_base) {
                printf "%s:%d: IMPORTS the durable schema (%s)\n", FILENAME, FNR, path
                bad = 1
            }
        }
        # Tokenize what is left and test each dotted identifier. A qualified
        # name counts only when the qualifier is this same package: a
        # like-named message in another package is a different type, and this
        # gate does not police it.
        rest = code
        while (match(rest, /[A-Za-z_][A-Za-z0-9_.]*/)) {
            tok = substr(rest, RSTART, RLENGTH)
            rest = substr(rest, RSTART + RLENGTH)
            base = tok
            qual = ""
            if (index(tok, ".") > 0) {
                base = tok
                sub(/^.*\./, "", base)
                qual = substr(tok, 1, length(tok) - length(base) - 1)
            }
            if (!(base in declared)) { continue }
            if (qual != "" && qual != "agentshim.frontend.v1") { continue }
            printf "%s:%d: NAMES the durable type %s\n", FILENAME, FNR, tok
            bad = 1
        }
    }
    END { exit 0 }
    ' "$DURABLE" "${siblings[@]}"
)"

if [ -n "$violations" ]; then
    printf 'durable-isolation: INVARIANT I6 VIOLATED — %s is the persistence evidence layer and no other component schema may import it or name a type it declares.\n' "$DURABLE_BASE" >&2
    printf '%s\n' "$violations" >&2
    printf 'durable-isolation: a frontend that names a durable type becomes the thing that decides what an audit record means, and it freezes itself to a shape kept frozen for byte-identical replay. Route the evidence through a resolved component view instead.\n' >&2
    exit 1
fi

printf 'durable-isolation: %s is not imported or named by any sibling component schema\n' "$DURABLE_BASE"
