#!/usr/bin/env bash
# Hermetic fixture tests for bin/check-personal-values.mjs.
#
# Every case builds its own tree in a mktemp directory, runs the check against
# it with HOME pointed at a fake home of its own (so the "personal values" are
# the fixture's, never the developer's), and asserts the exit status and the
# file:line it names. No git is run. The last case runs the check against the
# REAL module tree with the real HOME and requires it to pass: that case is the
# gate that keeps every personal value out of agent-repl.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
CHECK="$THIS_DIR/check-personal-values.mjs"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-check-personal-values-test.XXXXXX")"
TMP="$(cd "$TMP" && pwd -P)"
trap 'rm -rf "$TMP"' EXIT
FAKE_HOME="$TMP/home-ann"
mkdir -p "$FAKE_HOME"
printf '{"oauthAccount":{"emailAddress":"ann@host.test"}}' >"$FAKE_HOME/.claude.json"
PASS=0
FAIL=0

pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; FAIL=$((FAIL + 1)); }

# file writes one fixture file under a case's tree. Its body is the third
# argument.
file() { # CASE-NAME REL-PATH BODY
    local p="$TMP/$1/$2"
    mkdir -p "$(dirname "$p")"
    printf '%s\n' "$3" >"$p"
}

# act runs the check against one case's tree under the fake home, with any
# extra environment given, capturing its exit status and both output streams.
act() { # CASE-NAME [VAR=VALUE...]
    local c=$1
    shift
    set +e
    env -u CAPTURE_PERSONAL_NAMES -u CAPTURE_PERSONAL_EMAILS HOME="$FAKE_HOME" "$@" \
        node "$CHECK" "$TMP/$c" >"$TMP/$c.stdout" 2>"$TMP/$c.stderr"
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

test_clean_tree_passes() {
    local c=clean ok=0
    file $c src/a.go 'package a // no one is named here'
    act $c
    [ "$RUN_RC" -eq 0 ] && grep -q 'files clean' "$TMP/$c.stdout" && ok=1
    report "a tree naming no one passes" $c $ok
}

test_home_path_fails_naming_the_line() {
    local c=home ok=0
    file $c src/a.go "package a
const p = \"$FAKE_HOME/.config\""
    act $c
    [ "$RUN_RC" -eq 1 ] && grep -q 'src/a.go:2' "$TMP/$c.stderr" && ok=1
    report "the home directory fails, naming file:line" $c $ok
}

test_home_slug_in_a_path_fails() {
    local c=slug ok=0 slug
    slug="$(printf '%s' "$FAKE_HOME" | sed 's/[^A-Za-z0-9]/-/g')"
    file $c "projects/$slug-proj/s.jsonl" '{}'
    act $c
    [ "$RUN_RC" -eq 1 ] && grep -q 'the path names a personal value' "$TMP/$c.stderr" && ok=1
    report "a path holding the home's slug fails" $c $ok
}

test_account_email_fails() {
    local c=email ok=0
    file $c a.md 'mail ANN@host.test about it'
    act $c
    [ "$RUN_RC" -eq 1 ] && grep -q 'a.md:1' "$TMP/$c.stderr" && ok=1
    report "an account email signed in under the home fails, in any case" $c $ok
}

test_names_are_opt_in() {
    local c=names ok=0 unset_rc
    file $c a.md 'Annabel asked Ann'
    act $c
    unset_rc=$RUN_RC
    act $c CAPTURE_PERSONAL_NAMES=Ann
    [ "$unset_rc" -eq 0 ] && [ "$RUN_RC" -eq 1 ] && grep -q 'a.md:1' "$TMP/$c.stderr" && ok=1
    report "a name fails only once \$CAPTURE_PERSONAL_NAMES lists it" $c $ok
}

test_docs_reports_are_skipped_but_the_user_guide_is_not() {
    local c=docs ok=0 report_rc
    file $c docs/reports/old.md "$FAKE_HOME"
    act $c
    report_rc=$RUN_RC
    file $c docs/USER-GUIDE.md "$FAKE_HOME"
    act $c
    [ "$report_rc" -eq 0 ] && [ "$RUN_RC" -eq 1 ] && grep -q 'docs/USER-GUIDE.md:1' "$TMP/$c.stderr" && ok=1
    report "docs/ reports are skipped and docs/USER-GUIDE.md is checked" $c $ok
}

test_lineage_root_line_is_skipped() {
    local c=lineage ok=0
    file $c skills/x/SKILL.md '---
lineage_root: user.ann.skills.x
---'
    act $c CAPTURE_PERSONAL_NAMES=ann
    [ "$RUN_RC" -eq 0 ] && ok=1
    report "a skill's lineage_root line is skipped" $c $ok
}

test_real_module_is_clean() {
    local out
    set +e
    out="$(node "$CHECK" 2>&1)"
    RUN_RC=$?
    set -e
    if [ "$RUN_RC" -eq 0 ]; then
        pass "the real agent-repl module carries no personal value of whoever runs this"
    else
        fail "the real agent-repl module carries no personal value of whoever runs this"
        printf '%s\n' "$out" >&2
    fi
}

test_clean_tree_passes
test_home_path_fails_naming_the_line
test_home_slug_in_a_path_fails
test_account_email_fails
test_names_are_opt_in
test_docs_reports_are_skipped_but_the_user_guide_is_not
test_lineage_root_line_is_skipped
test_real_module_is_clean

printf '%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
