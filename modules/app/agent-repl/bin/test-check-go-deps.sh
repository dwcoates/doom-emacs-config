#!/usr/bin/env bash
# Hermetic fixture tests for bin/check-go-deps.sh.
#
# Every case builds its own tree of fixture go.mod files in a mktemp directory
# and points the check at it through its ROOT argument. No go toolchain and no
# git is run. The last case runs the check against the REAL module tree and
# requires it to pass: that case is the gate that keeps our Go modules pinned
# to one version of every shared third-party dependency.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
CHECK="$THIS_DIR/check-go-deps.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-check-go-deps-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0

pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; FAIL=$((FAIL + 1)); }

# gomod writes one fixture go.mod. Its body is read from stdin.
gomod() { # CASE-NAME REL-DIR
    local dir="$TMP/$1/$2"
    mkdir -p "$dir"
    cat >"$dir/go.mod"
}

# act runs the check against one case root, capturing its exit status and
# both output streams.
act() { # CASE-NAME
    set +e
    "$CHECK" "$TMP/$1" >"$TMP/$1.stdout" 2>"$TMP/$1.stderr"
    RUN_RC=$?
    set -e
}

report() { # DESCRIPTION CASE-NAME OK
    if [ "$3" = 1 ]; then
        pass "$1"
    else
        fail "$1"
        printf '  exit: %s\n  stdout:\n%s\n  stderr:\n%s\n' "$RUN_RC" \
            "$(cat "$TMP/$2.stdout")" "$(cat "$TMP/$2.stderr")" >&2
    fi
}

test_agreeing_versions_pass() {
    local c=agree ok=0
    gomod $c a <<'EOF'
module example/a

go 1.24.0

require (
	golang.org/x/sys v0.37.0
	google.golang.org/protobuf v1.36.11
)
EOF
    gomod $c b <<'EOF'
module example/b

go 1.24.0

require golang.org/x/sys v0.37.0
EOF
    act $c
    [ "$RUN_RC" -eq 0 ] && grep -q '2 go.mod files' "$TMP/$c.stdout" && ok=1
    report "modules agreeing on every shared dependency pass" $c $ok
}

test_conflicting_direct_dep_fails_naming_both() {
    local c=direct ok=0
    gomod $c a <<'EOF'
module example/a

require (
	golang.org/x/sys v0.35.0
)
EOF
    gomod $c b <<'EOF'
module example/b

require (
	golang.org/x/sys v0.37.0
)
EOF
    act $c
    [ "$RUN_RC" -ne 0 ] &&
        grep -q 'conflict: golang.org/x/sys is required at 2 versions: v0.35.0 (a/go.mod); v0.37.0 (b/go.mod)' \
            "$TMP/$c.stderr" && ok=1
    report "a direct dependency at two versions fails naming each version and go.mod" $c $ok
}

test_conflicting_indirect_dep_fails() {
    local c=indirect ok=0
    gomod $c a <<'EOF'
module example/a

require (
	modernc.org/sqlite v1.46.1 // indirect
)
EOF
    gomod $c b <<'EOF'
module example/b

require (
	modernc.org/sqlite v1.34.4 // indirect
)
EOF
    act $c
    [ "$RUN_RC" -ne 0 ] &&
        grep -q 'conflict: modernc.org/sqlite is required at 2 versions' "$TMP/$c.stderr" && ok=1
    report "an indirect dependency at two versions fails" $c $ok
}

test_single_line_require_is_parsed() {
    local c=single-line ok=0
    gomod $c a <<'EOF'
module example/a

require connectrpc.com/connect v1.17.0
EOF
    gomod $c b <<'EOF'
module example/b

require (
	connectrpc.com/connect v1.18.1
)
EOF
    act $c
    [ "$RUN_RC" -ne 0 ] &&
        grep -q 'conflict: connectrpc.com/connect is required at 2 versions: v1.17.0 (a/go.mod); v1.18.1 (b/go.mod)' \
            "$TMP/$c.stderr" && ok=1
    report "the single-line require form is parsed" $c $ok
}

test_local_replace_is_exempt() {
    local c=local-replace ok=0
    gomod $c a <<'EOF'
module example/a

require (
	agentrepl/proto v0.0.0
	agentrepl/logging v0.0.0-00010101000000-000000000000
)

replace agentrepl/proto => ../proto

replace (
	agentrepl/logging => ./logging
)
EOF
    gomod $c b <<'EOF'
module example/b

require (
	agentrepl/proto v0.0.0-00010101000000-000000000000
	agentrepl/logging v0.0.0
)

replace agentrepl/proto => ../proto

replace agentrepl/logging v0.0.0 => ../logging
EOF
    act $c
    [ "$RUN_RC" -eq 0 ] && ok=1
    report "modules replaced by a local path are exempt from version agreement" $c $ok
}

test_non_local_replace_does_not_exempt() {
    local c=remote-replace ok=0
    gomod $c a <<'EOF'
module example/a

require github.com/foo/bar v1.0.0

replace github.com/foo/bar => github.com/fork/bar v1.0.1
EOF
    gomod $c b <<'EOF'
module example/b

require github.com/foo/bar v1.2.0
EOF
    act $c
    [ "$RUN_RC" -ne 0 ] &&
        grep -q 'conflict: github.com/foo/bar is required at 2 versions' "$TMP/$c.stderr" && ok=1
    report "a replace to another module does not exempt the dependency" $c $ok
}

test_nested_node_modules_and_testdata_are_ignored() {
    local c=skipped ok=0
    gomod $c a <<'EOF'
module example/a

require golang.org/x/sys v0.37.0
EOF
    gomod $c a/node_modules/pkg <<'EOF'
module example/nm

require golang.org/x/sys v0.1.0
EOF
    gomod $c a/testdata/mod <<'EOF'
module example/td

require golang.org/x/sys v0.2.0
EOF
    act $c
    [ "$RUN_RC" -eq 0 ] && grep -q '1 go.mod files' "$TMP/$c.stdout" && ok=1
    report "go.mod files under node_modules and testdata are ignored" $c $ok
}

test_no_go_mod_is_an_error() {
    local c=empty ok=0
    mkdir -p "$TMP/$c/nothing-here"
    act $c
    [ "$RUN_RC" -ne 0 ] && grep -q 'no go.mod found' "$TMP/$c.stderr" && ok=1
    report "a root with no go.mod is an error" $c $ok
}

# THE GATE. The real module tree must pin every shared third-party dependency
# at one version.
test_real_module_tree_passes() {
    local out rc
    set +e
    out="$("$CHECK" 2>&1)"
    rc=$?
    set -e
    if [ "$rc" -eq 0 ]; then
        pass "the real agent-repl module tree pins every shared dependency at one version"
    else
        fail "the real agent-repl module tree pins every shared dependency at one version"
        printf '%s\n' "$out" >&2
    fi
}

test_agreeing_versions_pass
test_conflicting_direct_dep_fails_naming_both
test_conflicting_indirect_dep_fails
test_single_line_require_is_parsed
test_local_replace_is_exempt
test_non_local_replace_does_not_exempt
test_nested_node_modules_and_testdata_are_ignored
test_no_go_mod_is_an_error
test_real_module_tree_passes

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
