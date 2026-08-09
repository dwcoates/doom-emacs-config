#!/usr/bin/env bash
#
# THE GATE SELF-TEST for check-durable-isolation.sh (invariant I6).
#
# A build gate nobody has watched fail is not a gate: the failure mode of a
# grep-shaped check is that it silently matches nothing, and a check that
# matches nothing passes every run and reports the tree as clean forever.
# So this drives the real script against FIXTURE trees whose answers are known,
# in both directions — the clean tree must pass, and each spelling of the
# violation must fail.
#
# It follows test-check-generated.sh: a scratch fixture, the real script under
# test, and no dependence on the repository's own schemas, so tightening or
# relaxing the actual protos can never quietly change what this asserts.
#
# EVERY ROW RUNS. Failures are collected and reported together rather than
# aborting on the first, because the rows are independent spellings of one rule
# and fixing them one build at a time is the slow way to learn all three.
set -uo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECK="$THIS_DIR/check-durable-isolation.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-durable-isolation-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

failures=0

# newFixture materializes a fresh component directory holding the durable
# schema and one clean sibling, and echoes its path.
newFixture() {
    local dir="$TMP/$1"
    mkdir -p "$dir"
    cat >"$dir/durable.proto" <<'EOF'
syntax = "proto3";
package agentshim.frontend.v1;
message DurableRecord {
  int64 input_tokens = 1;
}
message DurableDetail {
  int64 thinking_tokens = 1;
}
EOF
    # The clean sibling NAMES both durable types in prose, in a line comment and
    # in a block comment, and refers to a same-named message from a foreign
    # package. All three are legal, and a gate that flagged them would punish
    # exactly the documentation that teaches the invariant.
    cat >"$dir/clean.proto" <<'EOF'
syntax = "proto3";
package agentshim.frontend.v1;
// Never carries a DurableRecord: what a renderer needs from that evidence
// arrives already resolved.
/* DurableDetail is likewise persistence-only. */
message CleanView {
  int64 total = 1;
  other.pkg.v1.DurableRecord unrelated_foreign_type = 2;
}
EOF
    printf '%s\n' "$dir"
}

# expect runs the check against a fixture and compares its exit status with the
# required one, reporting the gate's own output when the answer is wrong.
expect() {
    local want="$1" name="$2" dir="$3"
    local out rc
    out="$("$CHECK" "$dir" durable.proto 2>&1)"
    rc=$?
    if [ "$rc" -ne "$want" ]; then
        printf 'FAIL %s: check-durable-isolation exited %d, want %d. Output:\n%s\n' "$name" "$rc" "$want" "$out" >&2
        failures=$((failures + 1))
        return
    fi
    printf 'ok %s\n' "$name"
}

# --- the clean tree passes -----------------------------------------------------
clean="$(newFixture clean-tree)"
expect 0 "a component schema that only mentions the durable types in prose is accepted" "$clean"

# --- an import is refused ------------------------------------------------------
importer="$(newFixture importing-tree)"
cat >"$importer/importer.proto" <<'EOF'
syntax = "proto3";
package agentshim.frontend.v1;
import "agentshim/frontend/v1/durable.proto";
message ImportingView {
  int64 total = 1;
}
EOF
expect 1 "a component schema that imports durable.proto is refused" "$importer"

# --- a bare type reference is refused ------------------------------------------
namer="$(newFixture naming-tree)"
cat >"$namer/namer.proto" <<'EOF'
syntax = "proto3";
package agentshim.frontend.v1;
message NamingView {
  DurableRecord record = 1;
}
EOF
expect 1 "a component schema that names a durable type is refused" "$namer"

# --- a package-qualified reference is refused ----------------------------------
#
# This is the row that makes the gate more than an import check: proto has no
# visibility modifier, so a file reaching a durable type through another file's
# import carries no import line of its own to catch.
qualified="$(newFixture qualified-tree)"
cat >"$qualified/qualified.proto" <<'EOF'
syntax = "proto3";
package agentshim.frontend.v1;
message QualifiedView {
  agentshim.frontend.v1.DurableDetail detail = 1;
}
EOF
expect 1 "a component schema that names a durable type by its full package path is refused" "$qualified"

# --- a violation hidden behind a comment on the same line is refused -----------
#
# Comment stripping is the one place a gate like this can be talked out of its
# own answer, so it is asserted from both sides: prose alone passes (above), and
# prose does not launder the code beside it.
trailing="$(newFixture trailing-comment-tree)"
cat >"$trailing/trailing.proto" <<'EOF'
syntax = "proto3";
package agentshim.frontend.v1;
message TrailingView {
  DurableRecord record = 1; // resolved elsewhere, honestly
}
EOF
expect 1 "a durable type reference followed by a comment is still refused" "$trailing"

# --- a missing durable schema is a setup failure, not a pass -------------------
#
# The gate's worst outcome is passing because it looked at nothing. A component
# directory without the durable schema means the caller is wrong about the tree,
# and reporting that as clean would make every later run meaningless.
missing="$TMP/missing-tree"
mkdir -p "$missing"
printf 'syntax = "proto3";\n' >"$missing/only.proto"
expect 2 "a component directory with no durable schema is a setup failure rather than a pass" "$missing"

# --- the gate is wired to CODEGEN, not only to lint --------------------------
#
# A gate whose header promises "generated bindings cannot be produced from a
# schema that has already drifted" is only telling the truth if the targets that
# EMIT bindings depend on it. Hanging it off `lint` alone leaves `make go`,
# `make ts`, and `make all` free to emit from a coupled schema, and those are
# the invocations a developer actually types. So the dependency edge itself is
# asserted here, by driving the real Makefile against a drifted fixture through
# COMPONENT_DIR — the same targets, not a stand-in for them.

# drifted materializes a component tree whose sibling names a durable type.
driftedTree="$(newFixture drifted-codegen-tree)"
cat >"$driftedTree/drifted.proto" <<'EOF'
syntax = "proto3";
package agentshim.frontend.v1;
message DriftedView {
  DurableRecord record = 1;
}
EOF

# expectMakeRefuses runs one real Makefile target against the drifted fixture
# and requires that it fail AS THE GATE. A nonzero status alone would also be
# produced by a missing protoc, so the gate's own refusal must appear in the
# output, and protoc must never have been reached.
expectMakeRefuses() {
    local target="$1"
    local out rc
    out="$(make -C "$THIS_DIR" "$target" COMPONENT_DIR="$driftedTree" DURABLE_SCHEMA=durable.proto 2>&1)"
    rc=$?
    if [ "$rc" -eq 0 ]; then
        printf 'FAIL make %s emitted from a drifted schema (exited 0). Output:\n%s\n' "$target" "$out" >&2
        failures=$((failures + 1))
        return
    fi
    case "$out" in
        *'INVARIANT I6 VIOLATED'*) ;;
        *)
            printf 'FAIL make %s failed for some reason other than the durable-isolation gate. Output:\n%s\n' "$target" "$out" >&2
            failures=$((failures + 1))
            return
            ;;
    esac
    case "$out" in
        *'--go_out'*|*'--es_out'*|*'--descriptor_set_out'*)
            printf 'FAIL make %s ran protoc despite the drifted schema, so the gate is not a PREREQUISITE of codegen. Output:\n%s\n' "$target" "$out" >&2
            failures=$((failures + 1))
            return
            ;;
    esac
    printf 'ok make %s refuses a drifted schema before invoking protoc\n' "$target"
}

expectMakeRefuses go
expectMakeRefuses ts
expectMakeRefuses all
expectMakeRefuses lint

# The rows above would all pass vacuously if the gate refused every tree, so the
# shared gate target is also asserted to ACCEPT the clean fixture. It is driven
# here rather than through `make go`, which would then run protoc over this
# repository's own schemas and make the self-test depend on them.
gateOut="$(make -C "$THIS_DIR" codegen-gate COMPONENT_DIR="$clean" DURABLE_SCHEMA=durable.proto 2>&1)"
if [ $? -ne 0 ]; then
    printf 'FAIL make codegen-gate rejected a clean component tree. Output:\n%s\n' "$gateOut" >&2
    failures=$((failures + 1))
else
    printf 'ok make codegen-gate accepts a clean component tree\n'
fi

if [ "$failures" -ne 0 ]; then
    printf '%d durable-isolation gate self-test row(s) failed\n' "$failures" >&2
    exit 1
fi

printf 'durable-isolation gate accepts a clean component tree, refuses every spelling of the coupling, and blocks every Makefile codegen path\n'
