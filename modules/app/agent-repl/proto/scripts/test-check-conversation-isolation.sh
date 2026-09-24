#!/usr/bin/env bash
#
# THE GATE SELF-TEST for check-conversation-isolation.sh (invariant I7).
#
# A build gate nobody has watched fail is not a gate: the failure mode of a
# grep-shaped check is that it silently matches nothing, and a check that
# matches nothing passes every run and reports the tree as clean forever. So
# this drives the real script against FIXTURE trees whose answers are known, in
# both directions — the clean tree must pass, and each spelling of the violation
# must fail.
#
# The shape is a scratch fixture, the real script under test, and no dependence
# on the repository's own sources, so a real import landing or being removed can
# never quietly change what this asserts.
#
# EVERY ROW RUNS. Failures are collected and reported together rather than
# aborting on the first, because the rows are independent spellings of one rule.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -uo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECK="$THIS_DIR/check-conversation-isolation.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-conversation-isolation-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

failures=0

# newFixture materializes a fresh root holding a clean daemon, a clean webapp,
# and a producer (the sidecar) that legitimately DOES import the internal
# package, and echoes its path.
newFixture() {
    local root="$TMP/$1"
    mkdir -p "$root/daemon/internal/frontend" "$root/webapp/src" "$root/agent-shim/claude/shim-sidecar"
    cat >"$root/daemon/internal/frontend/translate.go" <<'EOF'
package frontend

import shimv1 "agentrepl/proto/shim/v1"

func Render(e *shimv1.ExternalEntry) string { return e.GetSessionId() }
EOF
    cat >"$root/webapp/src/store.ts" <<'EOF'
import type { ExternalEntry } from "../proto/shim/v1/external_pb.js";
export const sid = (e: ExternalEntry): string => e.sessionId;
EOF
    # The producer's import is legitimate and must NEVER be reported.
    cat >"$root/agent-shim/claude/shim-sidecar/write.go" <<'EOF'
package sidecar

import internalv1 "agentrepl/proto/store/v1"

func New() *internalv1.Entry { return &internalv1.Entry{} }
EOF
    printf '%s' "$root"
}

# expect runs the gate against a fixture and asserts the exit status.
expect() {
    local want="$1" desc="$2" root="$3"
    local out status
    out="$("$CHECK" "$root" 2>&1)"
    status=$?
    if [ "$status" -eq "$want" ]; then
        printf 'ok %s\n' "$desc"
    else
        printf 'FAIL %s (want exit %d, got %d)\n%s\n' "$desc" "$want" "$status" "$out" >&2
        failures=$((failures + 1))
    fi
}

root="$(newFixture clean)"
expect 0 "a tree where only the producer imports the shim-side package is accepted" "$root"

root="$(newFixture go-import)"
cat >>"$root/daemon/internal/frontend/plane.go" <<'EOF'
package frontend

import internalv1 "agentrepl/proto/store/v1"

func Plane(e *internalv1.Entry) any { return e.GetInternal().GetPlane() }
EOF
expect 1 "a daemon Go file importing the shim-side package is refused" "$root"

root="$(newFixture ts-import)"
cat >>"$root/webapp/src/plane.ts" <<'EOF'
import type { Entry } from "../proto/store/v1/entry_pb.js";
export const plane = (e: Entry): unknown => e.internal?.plane;
EOF
expect 1 "a webapp TypeScript file importing the shim-side package is refused" "$root"

root="$(newFixture proto-import)"
cat >>"$root/daemon/leak.proto" <<'EOF'
syntax = "proto3";
package frontend.v1;
import "store/v1/entry.proto";
EOF
expect 1 "a daemon-side proto importing the shim-side package is refused" "$root"

# THE ROW THAT MATTERS MOST. The invariant is taught by naming the forbidden
# package in the comments of the code that must not use it, so a gate that
# punished the documentation would be deleted within a week.
root="$(newFixture prose-only)"
cat >>"$root/daemon/internal/frontend/note.go" <<'EOF'
package frontend

// The observation plane lives in agentrepl/proto/store/v1
// and is deliberately unreachable from here: read the fact off the record it
// belongs to instead. See store/v1/entry.proto.
func Note() {}
EOF
expect 0 "a daemon file that only mentions the shim-side package in prose is accepted" "$root"

root="$(newFixture block-comment)"
cat >>"$root/daemon/internal/frontend/block.go" <<'EOF'
package frontend

/*
Historical note: this used to import
agentrepl/proto/store/v1
before the split.
*/
func Block() {}
EOF
expect 0 "a mention inside a block comment is accepted" "$root"

# A gate that scans nothing passes forever, so an empty tree is a SETUP failure
# rather than a pass.
empty="$TMP/empty"
mkdir -p "$empty/daemon"
expect 2 "a tree with the roots present but no sources is a setup failure rather than a pass" "$empty"

if [ "$failures" -ne 0 ]; then
    printf '\ntest-check-conversation-isolation: %d row(s) failed\n' "$failures" >&2
    exit 1
fi
printf '\ntest-check-conversation-isolation: all rows passed\n'
