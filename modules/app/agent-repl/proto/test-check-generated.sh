#!/usr/bin/env bash
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-proto-check-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

FIXTURE="$TMP/fixture"
STUBS="$TMP/stubs"
mkdir -p "$FIXTURE/gen/go" "$FIXTURE/gen/ts" "$STUBS"
cp "$THIS_DIR/Makefile" "$FIXTURE/Makefile"
printf 'syntax = "proto3";\n' >"$FIXTURE/fixture.proto"
printf 'stable\n' >"$FIXTURE/gen/go/artifact"
printf 'stable\n' >"$FIXTURE/gen/ts/artifact"

cat >"$STUBS/protoc" <<'EOF'
#!/usr/bin/env bash
for arg in "$@"; do
    case "$arg" in
        --go_out=*) printf '%s\n' "${PROTO_TEST_OUTPUT:-stable}" >gen/go/artifact ;;
        --es_out=*) printf '%s\n' "${PROTO_TEST_OUTPUT:-stable}" >gen/ts/artifact ;;
    esac
done
EOF

cat >"$STUBS/npx" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    *"which protoc-gen-es"*) printf '%s\n' "$PROTO_TEST_PLUGIN" ;;
esac
EOF

cat >"$STUBS/protoc-gen-es" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
chmod +x "$STUBS/protoc" "$STUBS/npx" "$STUBS/protoc-gen-es"

PATH="$STUBS:/usr/bin:/bin" \
    PROTO_TEST_PLUGIN="$STUBS/protoc-gen-es" \
    make -C "$FIXTURE" check-generated PROTOS=fixture.proto >/dev/null

set +e
PATH="$STUBS:/usr/bin:/bin" \
    PROTO_TEST_PLUGIN="$STUBS/protoc-gen-es" \
    PROTO_TEST_OUTPUT=changed \
    make -C "$FIXTURE" check-generated PROTOS=fixture.proto >/dev/null 2>&1
rc=$?
set -e

if [ "$rc" -eq 0 ]; then
    printf 'check-generated accepted stale generated artifacts\n' >&2
    exit 1
fi

printf 'check-generated accepts matching dirty artifacts and rejects stale output\n'
