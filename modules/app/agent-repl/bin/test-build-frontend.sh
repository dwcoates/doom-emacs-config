#!/usr/bin/env bash
# test-build-frontend.sh — hermetic tests for build-frontend.sh staleness logic.
#
# Builds a throwaway project tree around a copy of build-frontend.sh and stubs
# `npm` and `go` on PATH so no real toolchain runs: each stub just `touch`es the
# artifact it would have produced and records that it fired. Tests then assert
# WHICH artifacts the script decided to (re)build under each staleness scenario.
#
# Run with:   bash bin/test-build-frontend.sh

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/build-frontend.sh"

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; }

# --- build a fake project tree in a temp dir -------------------------------
# Layout mirrors what build-frontend.sh expects relative to its parent dir.
make_tree() {
    local root="$1"
    mkdir -p "$root/bin" "$root/shim/src" "$root/shim/dist" \
             "$root/webapp/src" "$root/webapp/dist" \
             "$root/daemon/cmd/claude-repld" "$root/daemon/bin"
    cp "$SCRIPT_UNDER_TEST" "$root/bin/build-frontend.sh"

    # Sources.
    echo "export const x = 1" > "$root/shim/src/main.ts"
    echo "export const y = 1" > "$root/webapp/src/main.ts"
    echo "package main" > "$root/daemon/cmd/claude-repld/main.go"
    echo '{"scripts":{"build":"true"}}' > "$root/shim/package.json"
    echo '{"scripts":{"build":"true"}}' > "$root/webapp/package.json"
    echo "module x" > "$root/daemon/go.mod"

    # node_modules present so the stubs are never asked to `npm install`.
    mkdir -p "$root/shim/node_modules" "$root/webapp/node_modules"
}

# Fresh artifacts: newer than every source so nothing is stale.
make_fresh_artifacts() {
    local root="$1"
    echo built > "$root/shim/dist/main.js"
    echo built > "$root/webapp/dist/index.html"
    echo built > "$root/daemon/bin/claude-repld"
    # Bump artifact mtimes strictly past the sources.
    sleep 1
    touch "$root/shim/dist/main.js" "$root/webapp/dist/index.html" \
          "$root/daemon/bin/claude-repld"
}

# PATH stubs for npm/go that log to $STUB_LOG and touch their artifact.
make_stubs() {
    local bindir="$1"
    mkdir -p "$bindir"
    cat > "$bindir/npm" <<'EOF'
#!/usr/bin/env bash
echo "npm $*" >> "$STUB_LOG"
# Emulate `npm run build` producing dist output for whichever project we're in.
if [ -d src ] && [ -f package.json ]; then
    if [ -d dist ] || mkdir -p dist; then :; fi
    if grep -q webapp <<<"$PWD" 2>/dev/null || [ -f webapp.marker ]; then :; fi
fi
# Touch the conventional artifact for the cwd project.
case "$PWD" in
    *shim*)   mkdir -p dist; echo built > dist/main.js ;;
    *webapp*) mkdir -p dist; echo built > dist/index.html ;;
esac
exit 0
EOF
    cat > "$bindir/go" <<'EOF'
#!/usr/bin/env bash
echo "go $*" >> "$STUB_LOG"
# `go build -o <out> ./cmd/...` — grab the -o target and touch it.
out=""
while [ $# -gt 0 ]; do
    [ "$1" = "-o" ] && { out="$2"; shift 2; continue; }
    shift
done
[ -n "$out" ] && { mkdir -p "$(dirname "$out")"; echo built > "$out"; }
exit 0
EOF
    chmod +x "$bindir/npm" "$bindir/go"
}

run_script() {
    # run_script ROOT [args...] — invoke the copied script with stubs on PATH.
    local root="$1"; shift
    STUB_LOG="$root/stub.log" \
        PATH="$root/stubs:$PATH" \
        bash "$root/bin/build-frontend.sh" "$@"
}

# --- Test 1: all fresh -> nothing rebuilt ----------------------------------
t_all_fresh() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    run_script "$root" >/dev/null
    if [ ! -s "$root/stub.log" ]; then
        pass "all-fresh: no build tool invoked"
    else
        fail "all-fresh: no build tool invoked" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# --- Test 2: one stale source -> only that artifact rebuilt -----------------
t_one_stale() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    sleep 1
    touch "$root/shim/src/main.ts"   # shim now stale, others fresh
    run_script "$root" >/dev/null
    local log; log="$(cat "$root/stub.log")"
    if grep -q "npm" <<<"$log" && ! grep -q "^go " <<<"$log"; then
        pass "one-stale: only shim (npm) rebuilt, daemon (go) skipped"
    else
        fail "one-stale: only shim rebuilt" "stub.log: $log"
    fi
    rm -rf "$root"
}

# --- Test 3: missing artifact -> rebuilt ------------------------------------
t_missing_artifact() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    rm -f "$root/daemon/bin/claude-repld"   # daemon artifact gone
    run_script "$root" daemon >/dev/null
    if grep -q "^go build" "$root/stub.log"; then
        pass "missing-artifact: absent daemon binary triggers go build"
    else
        fail "missing-artifact: daemon rebuilt" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# --- Test 4: --force -> rebuild despite fresh -------------------------------
t_force() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    run_script "$root" --force daemon >/dev/null
    if grep -q "^go build" "$root/stub.log"; then
        pass "force: fresh daemon still rebuilt under --force"
    else
        fail "force: fresh daemon rebuilt" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_all_fresh
t_one_stale
t_missing_artifact
t_force

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
