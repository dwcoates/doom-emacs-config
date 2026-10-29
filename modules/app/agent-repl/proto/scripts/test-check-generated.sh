#!/usr/bin/env bash

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-proto-check-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

FIXTURE="$TMP/fixture"
STUBS="$TMP/stubs"
mkdir -p "$FIXTURE/gen/go" "$FIXTURE/gen/ts" "$FIXTURE/component" "$FIXTURE/src" "$FIXTURE/scripts" "$STUBS"
cp "$THIS_DIR/../Makefile" "$FIXTURE/Makefile"
cp "$THIS_DIR/../package.json" "$THIS_DIR/../package-lock.json" "$FIXTURE/"
printf 'syntax = "proto3";\n' >"$FIXTURE/src/fixture.proto"

# The fixture carries the structural gate too, because `go` and `ts` require it:
# codegen is gated, so a fixture that omitted the gate would be testing a
# Makefile the repository does not have. I7's gate is pointed at the fixture
# root, which has no daemon or webapp tree — hence the stub below, which keeps it
# from reporting a setup failure while leaving its real behavior to its own
# self-test.
cp "$THIS_DIR/check-conversation-isolation.sh" "$FIXTURE/scripts/check-conversation-isolation.sh"
mkdir -p "$FIXTURE/daemon"
printf 'package daemon\n' >"$FIXTURE/daemon/stub.go"
# The gate also requires the store's read-only view package (I8) to exist and
# import only the conversation vocabulary, so the fixture carries a clean one.
mkdir -p "$FIXTURE/proto/src/storeread/v1"
cat >"$FIXTURE/proto/src/storeread/v1/stub.proto" <<'EOF'
syntax = "proto3";
package storeread.v1;
import "conversation/v1/history.proto";
EOF
cat >"$FIXTURE/component/clean.proto" <<'EOF'
syntax = "proto3";
package frontend.v1;
message CleanView {
  int64 total = 1;
}
EOF
printf 'stable\n' >"$FIXTURE/gen/go/artifact"
printf 'stable\n' >"$FIXTURE/gen/ts/artifact"

cat >"$STUBS/protoc" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
for arg in "$@"; do
    case "$arg" in
        --go_out=*) out="${arg#--go_out=}" ;;
        --es_out=*) out="${arg#--es_out=}" ;;
        *) continue ;;
    esac
    printf '%s\n' "$out" >>"$PROTO_TEST_WRITES"
    printf '%s\n' "${PROTO_TEST_OUTPUT:-stable}" >"$out/artifact"
done
EOF

cat >"$STUBS/npm" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
[ "$1" = ci ]
mkdir -p node_modules/.bin
cp "$PROTO_TEST_PLUGIN" node_modules/.bin/protoc-gen-es
chmod +x node_modules/.bin/protoc-gen-es
printf '{}\n' >node_modules/.package-lock.json
EOF

cat >"$STUBS/protoc-gen-es" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
cat >"$STUBS/node" <<'EOF'
#!/usr/bin/env bash
printf '2.14.1\n'
EOF
chmod +x "$STUBS/protoc" "$STUBS/npm" "$STUBS/protoc-gen-es" "$STUBS/node"

# The stub protoc records every output directory it writes, so the matching run
# can be held to never writing the checked tree: check-generated compares
# against a scratch regeneration, and a write into gen/ fails here instead of
# passing silently.
export PROTO_TEST_WRITES="$TMP/writes"
PATH="$STUBS:/usr/bin:/bin" \
    PROTO_TEST_PLUGIN="$STUBS/protoc-gen-es" \
    make -C "$FIXTURE" check-generated PROTOS=fixture.proto COMPONENT_DIR=component ISOLATION_ROOT=. >/dev/null
if [ "$(wc -l <"$PROTO_TEST_WRITES")" -ne 2 ]; then
    printf 'check-generated did not regenerate both bindings:\n%s\n' "$(cat "$PROTO_TEST_WRITES")" >&2
    exit 1
fi
if grep -qxE 'gen/(go|ts)' "$PROTO_TEST_WRITES"; then
    printf 'check-generated wrote the checked gen/ tree:\n%s\n' "$(cat "$PROTO_TEST_WRITES")" >&2
    exit 1
fi

set +e
PATH="$STUBS:/usr/bin:/bin" \
    PROTO_TEST_PLUGIN="$STUBS/protoc-gen-es" \
    PROTO_TEST_OUTPUT=changed \
    make -C "$FIXTURE" check-generated PROTOS=fixture.proto COMPONENT_DIR=component ISOLATION_ROOT=. >/dev/null 2>&1
rc=$?
set -e

if [ "$rc" -eq 0 ]; then
    printf 'check-generated accepted stale generated artifacts\n' >&2
    exit 1
fi

printf 'check-generated accepts matching dirty artifacts without writing them and rejects stale output\n'
