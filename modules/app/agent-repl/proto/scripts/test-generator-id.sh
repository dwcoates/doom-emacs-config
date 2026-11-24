#!/usr/bin/env bash
#
# THE SELF-TEST for generator-id.sh: the identity bin/build-frontend.sh stamps
# beside the bindings, so that a changed generator regenerates them.
#
# A stamp that leaves a generator out lets that generator change unseen, and
# the bindings go stale with nothing to say so. So this drives the real script
# against stub generators on PATH and asserts, for EVERY generator, that a
# change to it changes the identity, and that a missing one fails the script
# naming it.
#
# EVERY ROW RUNS. Failures are collected and reported together.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -uo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT="$THIS_DIR/generator-id.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-generator-id-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

failures=0

ok() { printf 'ok %s\n' "$1"; }
not_ok() {
    printf 'FAIL %s\n%s\n' "$1" "$2" >&2
    failures=$((failures + 1))
}

# newFixture NAME — a directory holding a stub of every generator: protoc and
# node answer their --version from PROTOC_VERSION and NODE_VERSION, the
# plugins are files whose bytes are their identity. Echoes its path.
newFixture() {
    local root="$TMP/$1"
    mkdir -p "$root/bin"
    cat >"$root/bin/protoc" <<'EOF'
#!/usr/bin/env bash
[ "${1:-}" = --version ] && { echo "libprotoc ${PROTOC_VERSION:-29.3}"; exit 0; }
exit 1
EOF
    cat >"$root/bin/node" <<'EOF'
#!/usr/bin/env bash
[ "${1:-}" = --version ] && { echo "${NODE_VERSION:-v24.2.0}"; exit 0; }
exit 1
EOF
    printf 'go plugin\n' >"$root/bin/protoc-gen-go"
    printf 'connect plugin\n' >"$root/bin/protoc-gen-connect-go"
    chmod +x "$root/bin/protoc" "$root/bin/node" "$root/bin/protoc-gen-go" "$root/bin/protoc-gen-connect-go"
    printf '{"lockfileVersion": 3}\n' >"$root/package-lock.json"
    printf 'swift plugin\n' >"$root/protoc-gen-swift"
    printf '%s' "$root"
}

# identity ROOT — run the script against ROOT's stubs, with nothing else of
# PATH but what a shell needs, so no host generator can answer for a stub.
identity() {
    PATH="$1/bin:/usr/bin:/bin" "$SCRIPT" "$1/package-lock.json" "$1/protoc-gen-swift"
}

root="$(newFixture lines)"
out="$(identity "$root" 2>&1)"
status=$?
names="$(printf '%s\n' "$out" | awk '{print $1}' | tr '\n' ' ')"
want="protoc-version protoc protoc-gen-go protoc-gen-connect-go node protoc-gen-es protoc-gen-swift "
if [ "$status" -eq 0 ] && [ "$names" = "$want" ] &&
       grep -qx 'protoc-version libprotoc 29.3' <<<"$out" && grep -qx 'node v24.2.0' <<<"$out"; then
    ok "every generator is named by its own line, the versions as they report them"
else
    not_ok "every generator is named by its own line, the versions as they report them" "exit $status; names [$names]; output:
$out"
fi

if [ "$(identity "$root")" = "$(identity "$root")" ]; then
    ok "unchanged generators answer the same identity"
else
    not_ok "unchanged generators answer the same identity" "$(identity "$root")"
fi

# changed DESC MUTATION — a change the identity must see: MUTATION is run with
# the fixture root as $root, between the two askings.
changed() {
    local desc="$1" mutation="$2" root before after
    root="$(newFixture "changed-$RANDOM$RANDOM")"
    before="$(identity "$root")"
    eval "$mutation"
    after="$(identity "$root")"
    if [ -n "$before" ] && [ -n "$after" ] && [ "$before" != "$after" ]; then
        ok "$desc"
    else
        not_ok "$desc" "before:
$before
after:
$after"
    fi
}

changed "a new protoc version changes the identity" 'export PROTOC_VERSION=30.0'
unset PROTOC_VERSION
changed "a rebuilt protoc of the same version changes the identity" 'printf "# rebuilt\n" >>"$root/bin/protoc"'
changed "a changed protoc-gen-go changes the identity" 'printf "v2\n" >>"$root/bin/protoc-gen-go"'
changed "a changed protoc-gen-connect-go changes the identity" 'printf "v2\n" >>"$root/bin/protoc-gen-connect-go"'
changed "a new node version changes the identity" 'export NODE_VERSION=v25.0.0'
unset NODE_VERSION
changed "a changed protoc-gen-es install changes the identity" 'printf "{\"lockfileVersion\": 4}\n" >"$root/package-lock.json"'
changed "a rebuilt protoc-gen-swift changes the identity" 'printf "v2\n" >>"$root/protoc-gen-swift"'

# missing DESC NAME MUTATION — a generator taken away fails the script, and
# its stderr names NAME.
missing() {
    local desc="$1" name="$2" mutation="$3" root out status
    root="$(newFixture "missing-$RANDOM$RANDOM")"
    eval "$mutation"
    out="$(identity "$root" 2>&1 >/dev/null)"
    status=$?
    if [ "$status" -ne 0 ] && grep -q "generator-id: .*$name" <<<"$out"; then
        ok "$desc"
    else
        not_ok "$desc" "exit $status; stderr: $out"
    fi
}

missing "a protoc missing from PATH fails, naming it" protoc 'rm "$root/bin/protoc"'
missing "a protoc whose --version fails fails, naming it" protoc 'printf "#!/usr/bin/env bash\nexit 3\n" >"$root/bin/protoc"'
missing "a protoc-gen-go missing from PATH fails, naming it" protoc-gen-go 'rm "$root/bin/protoc-gen-go"'
missing "a protoc-gen-connect-go missing from PATH fails, naming it" protoc-gen-connect-go 'rm "$root/bin/protoc-gen-connect-go"'
missing "a node whose --version fails fails, naming it" node 'printf "#!/usr/bin/env bash\nexit 3\n" >"$root/bin/node"'
missing "a missing protoc-gen-es install fails, naming it" protoc-gen-es 'rm "$root/package-lock.json"'
missing "a missing protoc-gen-swift fails, naming it" protoc-gen-swift 'rm "$root/protoc-gen-swift"'

out="$("$SCRIPT" only-one 2>&1)"
status=$?
if [ "$status" -eq 2 ] && grep -q 'usage' <<<"$out"; then
    ok "a call without both paths is refused with its usage"
else
    not_ok "a call without both paths is refused with its usage" "exit $status; output: $out"
fi

# THE MAKEFILE'S generator-id TARGET, the one caller: driven against a
# fixture holding the real Makefile and script, an installed protoc-gen-es
# lockfile newer than the manifests (so no npm ci runs) and a stub swift
# plugin named through PROTOC_GEN_SWIFT (so no swift build runs).
MAKEFILE="$THIS_DIR/../Makefile"
root="$(newFixture make-target)"
mkdir -p "$root/proto/scripts" "$root/proto/node_modules"
cp "$MAKEFILE" "$root/proto/Makefile"
cp "$SCRIPT" "$root/proto/scripts/generator-id.sh"
printf '{"devDependencies": {"@bufbuild/protoc-gen-es": "0.0.0"}}\n' >"$root/proto/package.json"
cp "$root/package-lock.json" "$root/proto/package-lock.json"
cp "$root/package-lock.json" "$root/proto/node_modules/.package-lock.json"
touch -t 200001010000 "$root/proto/package.json" "$root/proto/package-lock.json"
want="$(PATH="$root/bin:/usr/bin:/bin" "$SCRIPT" "$root/proto/node_modules/.package-lock.json" "$root/protoc-gen-swift")"
out_file="$root/identity"
make_out="$(PATH="$root/bin:$PATH" make -s -C "$root/proto" generator-id PROTOC_GEN_SWIFT="$root/protoc-gen-swift" GENERATOR_ID_OUT="$out_file" 2>&1)"
status=$?
if [ "$status" -eq 0 ] && [ -n "$want" ] && [ "$(cat "$out_file" 2>/dev/null)" = "$want" ]; then
    ok "make generator-id writes the script's identity of the Makefile's own generators to GENERATOR_ID_OUT"
else
    not_ok "make generator-id writes the script's identity of the Makefile's own generators to GENERATOR_ID_OUT" "exit $status; make: $make_out; wrote: $(cat "$out_file" 2>&1); want: $want"
fi

make_out="$(PATH="$root/bin:$PATH" make -s -C "$root/proto" generator-id PROTOC_GEN_SWIFT="$root/protoc-gen-swift" 2>&1)"
status=$?
if [ "$status" -ne 0 ] && grep -q 'GENERATOR_ID_OUT names no file' <<<"$make_out"; then
    ok "make generator-id without GENERATOR_ID_OUT is refused"
else
    not_ok "make generator-id without GENERATOR_ID_OUT is refused" "exit $status; make: $make_out"
fi

if [ "$failures" -ne 0 ]; then
    printf '\ntest-generator-id: %d row(s) failed\n' "$failures" >&2
    exit 1
fi
printf '\ntest-generator-id: all rows passed\n'
