#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
# shellcheck disable=SC2016
# SC2016 is declined because this harness quotes shell and Make source TEXT on
# purpose: the prologue, `$(BACKGROUND)` and `"$@"` are the literal strings the
# scan matches, and child `bash -c` bodies must expand in the child, not here.
#
# test-background.sh -- tests for bin/background.sh, the helper every test run
# goes through so it runs at background priority, and the SOURCE SCAN that
# fails when any test entry point does not route through it.
#
# Three parts:
#   1. The helper, HERMETICALLY: uname, perl and nice are stubs, so every
#      platform branch (wrap, pass-through, unreadable niceness, refusal) is
#      exercised on any host.
#   2. The helper on THIS host, for real: a wrapped command's grandchild reads
#      niceness 19, and a nested wrap does not push it further.
#   3. The source scan over the real repository, and over fixture trees that
#      each carry exactly one violation, so the scan is shown to catch it.
#
# Run with:   bash bin/test-background.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-grep-in.sh"

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
HELPER="$THIS_DIR/background.sh"
MODULE_DIR="$(cd "$THIS_DIR/.." && pwd -P)"
REPO_ROOT="$(cd "$MODULE_DIR/../../.." && pwd -P)"

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; }

TMP="$(mktemp -d "${TMPDIR:-/tmp}/bg.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# ============================================================================
# 1. The helper, hermetically.
# ============================================================================

STUBS="$TMP/stubs"
mkdir -p "$STUBS"

cat >"$STUBS/uname" <<'STUB'
#!/bin/bash
printf '%s\n' "${STUB_UNAME:?}"
STUB
cat >"$STUBS/perl" <<'STUB'
#!/bin/bash
[ "${STUB_NICE_FAIL:-0}" = 1 ] && exit 1
printf '%s' "${STUB_NICENESS:?}"
STUB
cat >"$STUBS/nice" <<'STUB'
#!/bin/bash
if [ "$#" -eq 0 ]; then
    [ "${STUB_NICE_FAIL:-0}" = 1 ] && exit 1
    printf '%s\n' "${STUB_NICENESS:?}"
    exit 0
fi
printf 'nice %s\n' "$*" >>"$STUB_LOG"
shift 2
exec "$@"
STUB
cat >"$STUBS/taskpolicy" <<'STUB'
#!/bin/bash
printf 'taskpolicy %s\n' "$*" >>"$STUB_LOG"
shift 2
exec "$@"
STUB
chmod +x "$STUBS/uname" "$STUBS/perl" "$STUBS/nice" "$STUBS/taskpolicy"

# run_helper DIR [ENV=VALUE...] -- <command...>
# Runs the helper under the stubs with a CLEAN marker, the way a fresh entry
# point would. Sets RC; the subject's stdout/stderr land in DIR.
run_helper() {
    local dir="$1"; shift
    local env_args=()
    while [ "$1" != "--" ]; do env_args+=("$1"); shift; done
    shift
    mkdir -p "$dir"
    : >"$dir/log"
    set +e
    env -u AGENT_REPL_BACKGROUND_PRIORITY \
        PATH="$STUBS:/usr/bin:/bin" \
        STUB_LOG="$dir/log" \
        "${env_args[@]}" \
        bash "$HELPER" "$@" >"$dir/out" 2>"$dir/err"
    RC=$?
    set -e
}

# The subject every wrap case runs: it reports the marker it was handed.
MARKER_CMD=(bash -c 'printf "marker=%s\n" "${AGENT_REPL_BACKGROUND_PRIORITY:-}"')

# Both supported platforms demote the same way; only how the niceness is read
# differs (perl's getpriority on macOS, nice(1) on Linux), and the stubs
# answer both from STUB_NICENESS.
for platform in Darwin Linux; do
    # --- 1a. a normal-priority run: wrapped in nice -n 19 with the marker ------
    d="$TMP/h1a-$platform"
    run_helper "$d" STUB_UNAME="$platform" STUB_NICENESS=0 -- "${MARKER_CMD[@]}"
    if [ "$RC" -eq 0 ] && grep -q '^nice -n 19 bash -c' "$d/log" && [ "$(cat "$d/out")" = "marker=nice-19" ]; then
        pass "$platform: a normal-priority run is executed under nice -n 19 with the nice-19 marker"
    else
        fail "$platform: a normal-priority run is executed under nice -n 19 with the nice-19 marker" "rc=$RC log: $(cat "$d/log") out: $(cat "$d/out") err: $(cat "$d/err")"
    fi
    # --- 1a'. disk I/O: throttled on macOS, untouched on Linux ----------------
    if [ "$platform" = Darwin ]; then
        if grep -q '^taskpolicy -d throttle nice -n 19 bash -c' "$d/log"; then
            pass "Darwin: a normal-priority run's disk I/O is throttled with taskpolicy -d throttle"
        else
            fail "Darwin: a normal-priority run's disk I/O is throttled with taskpolicy -d throttle" "log: $(cat "$d/log")"
        fi
        if grep -q '^taskpolicy .*-b' "$d/log"; then
            fail "Darwin: the run is never put in the background band that pins to efficiency cores" "log: $(cat "$d/log")"
        else
            pass "Darwin: the run is never put in the background band that pins to efficiency cores"
        fi
    else
        if grep -q '^taskpolicy' "$d/log"; then
            fail "Linux: no taskpolicy is invoked" "log: $(cat "$d/log")"
        else
            pass "Linux: no taskpolicy is invoked"
        fi
    fi

    # --- 1b. already at niceness 19: passes through --------------------------
    d="$TMP/h1b-$platform"
    run_helper "$d" STUB_UNAME="$platform" STUB_NICENESS=19 -- "${MARKER_CMD[@]}"
    if [ "$RC" -eq 0 ] && [ ! -s "$d/log" ] && [ "$(cat "$d/out")" = "marker=nice-19" ]; then
        pass "$platform: a run already at niceness 19 passes through without re-wrapping"
    else
        fail "$platform: a run already at niceness 19 passes through without re-wrapping" "rc=$RC log: $(cat "$d/log") out: $(cat "$d/out")"
    fi

    # --- 1c. a niceness above 19 (macOS reaches 20): passes through ----------
    d="$TMP/h1c-$platform"
    run_helper "$d" STUB_UNAME="$platform" STUB_NICENESS=20 -- "${MARKER_CMD[@]}"
    if [ "$RC" -eq 0 ] && [ ! -s "$d/log" ]; then
        pass "$platform: a run already above niceness 19 passes through without re-wrapping"
    else
        fail "$platform: a run already above niceness 19 passes through without re-wrapping" "rc=$RC log: $(cat "$d/log")"
    fi

    # --- 1d. a niceness below 19 but not 0: still demoted --------------------
    d="$TMP/h1d-$platform"
    run_helper "$d" STUB_UNAME="$platform" STUB_NICENESS=10 -- "${MARKER_CMD[@]}"
    if [ "$RC" -eq 0 ] && grep -q '^nice -n 19 bash -c' "$d/log"; then
        pass "$platform: a run at niceness 10 is still demoted"
    else
        fail "$platform: a run at niceness 10 is still demoted" "rc=$RC log: $(cat "$d/log")"
    fi

    # --- 1e. the niceness cannot be read: refused, command not run -----------
    d="$TMP/h1e-$platform"
    run_helper "$d" STUB_UNAME="$platform" STUB_NICE_FAIL=1 STUB_NICENESS=0 -- \
        bash -c 'echo ran >"$0"' "$d/ran"
    if [ "$RC" -eq 78 ] && [ ! -e "$d/ran" ] && grep -q 'REFUSING TO RUN' "$d/err"; then
        pass "$platform: an unreadable niceness refuses the run loudly (exit 78) and never runs it"
    else
        fail "$platform: an unreadable niceness refuses the run loudly (exit 78) and never runs it" "rc=$RC err: $(cat "$d/err")"
    fi

    # --- 1f. the niceness is not a number: refused ---------------------------
    d="$TMP/h1f-$platform"
    run_helper "$d" STUB_UNAME="$platform" STUB_NICENESS=high -- bash -c 'echo ran >"$0"' "$d/ran"
    if [ "$RC" -eq 78 ] && [ ! -e "$d/ran" ] && grep -q "not a number: 'high'" "$d/err"; then
        pass "$platform: a niceness that is not a number refuses the run loudly"
    else
        fail "$platform: a niceness that is not a number refuses the run loudly" "rc=$RC err: $(cat "$d/err")"
    fi
done

# --- 1g. any other platform: refused, never run at normal priority -----------
d="$TMP/h1g"
run_helper "$d" STUB_UNAME=FreeBSD -- bash -c 'echo ran >"$0"' "$d/ran"
if [ "$RC" -eq 78 ] && [ ! -e "$d/ran" ] && grep -q "platform 'FreeBSD'" "$d/err"; then
    pass "an unknown platform refuses the run loudly, naming the platform"
else
    fail "an unknown platform refuses the run loudly, naming the platform" "rc=$RC err: $(cat "$d/err")"
fi

# --- 1h. no command: refused -------------------------------------------------
d="$TMP/h1h"
run_helper "$d" STUB_UNAME=Darwin STUB_NICENESS=0 --
if [ "$RC" -eq 78 ] && grep -q 'no command given' "$d/err"; then
    pass "no command is a loud refusal"
else
    fail "no command is a loud refusal" "rc=$RC err: $(cat "$d/err")"
fi

# --- 1i. the command's exit status is the helper's ---------------------------
d="$TMP/h1i"
run_helper "$d" STUB_UNAME=Darwin STUB_NICENESS=0 -- bash -c 'exit 7'
if [ "$RC" -eq 7 ]; then
    pass "a failing command's exit status passes through the helper"
else
    fail "a failing command's exit status passes through the helper" "rc=$RC"
fi

# --- 1j. the prologue re-execs an unmarked script through the helper ---------
d="$TMP/h1j"
mkdir -p "$d/bin"
cp "$HELPER" "$d/bin/background.sh"
cat >"$d/bin/test-subject.sh" <<'SUBJECT'
#!/usr/bin/env bash
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"
printf 'marker=%s args=%s\n' "$AGENT_REPL_BACKGROUND_PRIORITY" "$*"
SUBJECT
: >"$d/log"
set +e
env -u AGENT_REPL_BACKGROUND_PRIORITY PATH="$STUBS:/usr/bin:/bin" STUB_LOG="$d/log" \
    STUB_UNAME=Darwin STUB_NICENESS=0 \
    bash "$d/bin/test-subject.sh" one two >"$d/out" 2>"$d/err"
RC=$?
set -e
if [ "$RC" -eq 0 ] && [ "$(cat "$d/out")" = "marker=nice-19 args=one two" ] &&
    [ "$(grep -c '^nice -n 19 bash ' "$d/log")" -eq 1 ]; then
    pass "the prologue re-execs an unmarked script through the helper exactly once, arguments intact"
else
    fail "the prologue re-execs an unmarked script through the helper exactly once, arguments intact" "rc=$RC out: $(cat "$d/out") log: $(cat "$d/log") err: $(cat "$d/err")"
fi

# --- 1k. the prologue leaves an already-marked script alone ------------------
d="$TMP/h1k"
mkdir -p "$d"
: >"$d/log"
set +e
env PATH="$STUBS:/usr/bin:/bin" STUB_LOG="$d/log" AGENT_REPL_BACKGROUND_PRIORITY=nice-19 \
    bash "$TMP/h1j/bin/test-subject.sh" >"$d/out" 2>"$d/err"
RC=$?
set -e
if [ "$RC" -eq 0 ] && [ ! -s "$d/log" ] && [ "$(cat "$d/out")" = "marker=nice-19 args=" ]; then
    pass "the prologue runs an already-marked script directly"
else
    fail "the prologue runs an already-marked script directly" "rc=$RC out: $(cat "$d/out") log: $(cat "$d/log")"
fi

# ============================================================================
# 2. The helper on this host, for real.
# ============================================================================
#
# This harness itself runs at niceness 19 (its prologue saw to that), and an
# unprivileged process can never lower its niceness again, so the wrap from
# normal priority is pinned hermetically above. Here the real kernel answers
# what inheritance and idempotency depend on.

HOST_OS="$(uname -s)"
# The niceness query every case reads back, the same one the helper uses.
case $HOST_OS in
    Darwin) QUERY='perl -e "print getpriority(0, 0)"' ;;
    Linux) QUERY='nice' ;;
    *) QUERY='' ;;
esac

if [ -n "$QUERY" ]; then
    # --- 2a. this harness runs at niceness 19 ---------------------------------
    out="$(bash -c "$QUERY" 2>&1)" || true
    if [ "$out" = 19 ]; then
        pass "$HOST_OS (real): a test entry point runs at niceness 19"
    else
        fail "$HOST_OS (real): a test entry point runs at niceness 19" "niceness=$out"
    fi

    # --- 2b. a wrapped command's grandchild inherits niceness 19 --------------
    out="$(env -u AGENT_REPL_BACKGROUND_PRIORITY bash "$HELPER" bash -c "bash -c '$QUERY'" 2>&1)" || true
    if [ "$out" = 19 ]; then
        pass "$HOST_OS (real): a grandchild of a wrapped command runs at niceness 19"
    else
        fail "$HOST_OS (real): a grandchild of a wrapped command runs at niceness 19" "niceness=$out"
    fi

    # --- 2c. a nested wrap does not demote further ----------------------------
    # macOS lets niceness climb to 20, so a second `nice -n 19` would show here.
    out="$(env -u AGENT_REPL_BACKGROUND_PRIORITY bash "$HELPER" \
        env -u AGENT_REPL_BACKGROUND_PRIORITY bash "$HELPER" bash -c "$QUERY" 2>&1)" || true
    if [ "$out" = 19 ]; then
        pass "$HOST_OS (real): a helper nested in a helper leaves the niceness at 19"
    else
        fail "$HOST_OS (real): a helper nested in a helper leaves the niceness at 19" "niceness=$out"
    fi

    # --- 2d. the wrap EXECs: the caller's pid is the command's ----------------
    out="$(env -u AGENT_REPL_BACKGROUND_PRIORITY \
        bash -c 'echo "$$"; exec bash "$0" bash -c "echo \$\$"' "$HELPER" 2>&1)" || true
    if [ "$(printf '%s\n' "$out" | wc -l | tr -d ' ')" = 2 ] &&
        [ "$(printf '%s\n' "$out" | sort -u | wc -l | tr -d ' ')" = 1 ]; then
        pass "$HOST_OS (real): the wrap execs, so a caller's pid (and process group) is the command's"
    else
        fail "$HOST_OS (real): the wrap execs, so a caller's pid (and process group) is the command's" "pids: $out"
    fi
else
    fail "this platform ($HOST_OS) has no background-priority mechanism; the helper should have refused this harness"
fi

# ============================================================================
# 3. The source scan: every test entry point routes through the helper.
# ============================================================================

# The test-runner invocations a non-test-named file may not carry unwrapped.
RUNNER_RE='(go test|go -C [^ ]+ test|vitest|ert-run-tests|npm (run )?test|bin/test-all\.sh|report-nonlisp-coverage\.sh|e2e-coverage\.sh|scripts/test-)'
PROLOGUE_RE='^\[\[ -n \$\{AGENT_REPL_BACKGROUND_PRIORITY:-\} \]\] \|\| exec "\$\(dirname "\$\{BASH_SOURCE\[0\]\}"\)/([^"]+)" bash "\$\{BASH_SOURCE\[0\]\}" "\$@"$'

# resolve PATH -- the physical path of PATH's directory, plus its basename.
resolve() {
    local dir
    dir="$(cd "$(dirname "$1")" 2>/dev/null && pwd -P)" || return 1
    printf '%s/%s\n' "$dir" "$(basename "$1")"
}

# first_code_line FILE -- the first line after the shebang that is neither
# blank nor a comment.
first_code_line() {
    awk 'NR == 1 { next } /^[[:space:]]*(#|$)/ { next } { print; exit }' "$1"
}

# check_prologue FILE HELPER -- FILE's first executable line is the prologue,
# and the helper it names is HELPER.
check_prologue() {
    local file="$1" helper="$2" line rel target
    line="$(first_code_line "$file")"
    if [[ ! $line =~ $PROLOGUE_RE ]]; then
        printf '%s: first executable line is not the background prologue\n' "$file"
        return
    fi
    rel="${BASH_REMATCH[1]}"
    target="$(resolve "$(dirname "$file")/$rel")" || target=""
    [ "$target" = "$helper" ] ||
        printf '%s: prologue names %s, which is not %s\n' "$file" "$rel" "$helper"
}

# scan ROOT -- print one line per test entry point under the repository at
# ROOT that does not route through ROOT's bin/background.sh.
scan() {
    local root="$1"
    local module="$root/modules/app/agent-repl"
    local helper guard file
    helper="$(resolve "$module/bin/background.sh")" || { echo "$module/bin/background.sh: missing"; return; }
    guard="$(resolve "$module/bin/require-background.mjs")" || { echo "$module/bin/require-background.mjs: missing"; return; }

    # (a) Every bash test script, and the non-test-named suite runners, carry
    # the prologue as their first executable line.
    {
        find "$root/bin" "$root/.githooks" "$root/.claude" -maxdepth 1 -name 'test-*.sh' 2>/dev/null
        find "$module" \( -name node_modules -o -name testdata \) -prune -o -name 'test-*.sh' -print
        local named
        for named in bin/report-nonlisp-coverage.sh bin/e2e-coverage.sh bin/e2e-repeat.sh bin/suite-slot.sh; do
            [ -e "$module/$named" ] && echo "$module/$named"
        done
        for named in .claude/safe-test-run.sh .claude/run-subproject-tests.sh; do
            [ -e "$root/$named" ] && echo "$root/$named"
        done
    } | sort -u | while IFS= read -r file; do
        check_prologue "$file" "$helper"
    done

    # (b) A hook or agent hook that runs a suite carries the prologue too. The
    # pre-commit hook runs only the grep boundary lint and so is not demoted;
    # the day it runs a suite again, this is what makes it route.
    for file in "$root"/.githooks/* "$root"/.claude/*.sh; do
        [ -f "$file" ] || continue
        case "$(basename "$file")" in test-*) continue ;; esac
        if grep_in "$(grep -v '^[[:space:]]*#' "$file")" -Eq "$RUNNER_RE"; then
            [[ "$(first_code_line "$file")" =~ $PROLOGUE_RE ]] ||
                printf '%s: runs a test suite without the background prologue\n' "$file"
        fi
    done

    # (c) Every Makefile recipe that runs a suite goes through $(BACKGROUND),
    # and $(BACKGROUND) is this module's helper.
    find "$module" -name node_modules -prune -o -name Makefile -print | sort | while IFS= read -r file; do
        local def rel target
        # Logical recipe lines: continuations joined, comments dropped.
        awk '
            { line = (cont ? acc " " : "") $0 }
            /\\$/ { acc = substr(line, 1, length(line) - 1); cont = 1; next }
            { cont = 0; acc = ""; if (line ~ /^\t/) print line }
        ' "$file" | while IFS= read -r recipe; do
            if grep_in "$recipe" -Eq "$RUNNER_RE" &&
                ! grep_in "$recipe" -Fq '$(BACKGROUND)'; then
                printf '%s: recipe runs a suite without $(BACKGROUND): %s\n' "$file" "$(printf '%s' "$recipe" | tr -s '\t ' ' ' | sed 's/^ //')"
            fi
        done
        if grep -Fq '$(BACKGROUND)' "$file"; then
            def="$(grep -E '^BACKGROUND := ' "$file" || true)"
            if [[ $def =~ ^BACKGROUND\ :=\ \$\(abspath\ \$\(dir\ \$\(lastword\ \$\(MAKEFILE_LIST\)\)\)([^\)]+)\)$ ]]; then
                rel="${BASH_REMATCH[1]}"
                target="$(resolve "$(dirname "$file")/$rel")" || target=""
                [ "$target" = "$helper" ] ||
                    printf '%s: BACKGROUND names %s, which is not %s\n' "$file" "$rel" "$helper"
            else
                printf '%s: uses $(BACKGROUND) without the standard definition\n' "$file"
            fi
        fi
    done

    # (d) Every package script that runs tests starts with the helper, and a
    # compound one is wrapped whole (`<helper> sh -c '...'`). Build and dev
    # scripts are what a deploy runs, and must NOT be demoted.
    find "$module" -name node_modules -prune -o -name package.json -print | sort | while IFS= read -r file; do
        node -e '
            const s = require(process.argv[1]).scripts || {};
            for (const [k, v] of Object.entries(s)) console.log(k + "\t" + v);
        ' "$file" | while IFS=$'\t' read -r name value; do
            local pkgdir rel rest target
            pkgdir="$(dirname "$file")"
            if [[ $name =~ ^(pre)?(build|dev)(:.*)?$ ]]; then
                case "$value" in
                    *background.sh*) printf '%s: build script "%s" is demoted; the deploy build must stay at normal priority\n' "$file" "$name" ;;
                esac
                continue
            fi
            if [[ ! $name =~ ^(pre)?(test|coverage|smoke)(:.*)?$ ]] && [[ ! $value =~ vitest ]]; then
                continue
            fi
            rel="${value%% *}"
            rest="${value#* }"
            target="$(resolve "$pkgdir/$rel")" || target=""
            if [ "$target" != "$helper" ]; then
                printf '%s: script "%s" does not start with the background helper: %s\n' "$file" "$name" "$value"
                continue
            fi
            case "$rest" in
                *'&&'*|*'||'*|*';'*|*'|'*)
                    [[ $rest =~ ^sh\ -c\ \'[^\']*\'$ ]] ||
                        printf '%s: script "%s" chains commands outside the helper: %s\n' "$file" "$name" "$value"
                    ;;
            esac
        done
    done

    # (e) Every vitest config imports the guard that refuses an unwrapped run.
    find "$module" -name node_modules -prune -o -name 'vitest*.config.ts' -print | sort | while IFS= read -r file; do
        local rel target
        rel="$(sed -nE 's/^import "([^"]*require-background\.mjs)";$/\1/p' "$file" | sed -n 1p)"
        target=""
        [ -n "$rel" ] && target="$(resolve "$(dirname "$file")/$rel")"
        [ "$target" = "$guard" ] ||
            printf '%s: does not import %s\n' "$file" "$guard"
    done

    # (f) The live runtime and the deploy's builds are never demoted: the
    # daemon's deploy (daemon/internal/deploy) runs bin/build-frontend.sh.
    for file in "$module"/daemon/internal/deploy/*.go "$module/bin/build-frontend.sh" \
        "$module"/launchd/* "$module"/lisp/*.el; do
        [ -f "$file" ] || continue
        case "$(basename "$file")" in test-*.el | *_test.go) continue ;; esac
        if grep -Eq 'background\.sh|nice -n|"nice"|taskpolicy|require-background' "$file"; then
            printf '%s: live runtime or deploy path references the test background helper\n' "$file"
        fi
    done

    # (g) The e2e sandbox runs every command through the helper.
    file="$module/e2e/sandbox/bin/entrypoint.sh"
    if [ -f "$file" ]; then
        grep -Fqx 'exec "$REPO/$MODULE_REL/bin/background.sh" "$@"' "$file" ||
            printf '%s: does not exec its command through bin/background.sh\n' "$file"
        ! grep -Eqx 'exec "\$@"' "$file" ||
            printf '%s: execs a command without bin/background.sh\n' "$file"
    fi

    # (h) Every ERT suite loads the gated batch harness. test-helpers.el makes
    # the gate call at top level; every other lisp/test-*.el loads it, either
    # directly or through test-integration-helpers.el (which loads it).
    file="$module/lisp/test-helpers.el"
    if [ -f "$file" ]; then
        grep -Eq '^\(agent-repl-test--require-background-priority$' "$file" ||
            printf '%s: does not call agent-repl-test--require-background-priority at load\n' "$file"
        for file in "$module"/lisp/test-*.el; do
            [ "$(basename "$file")" = test-helpers.el ] && continue
            grep -Eq '\(load \(expand-file-name "test-(integration-)?helpers\.el"' "$file" ||
                printf '%s: does not load the gated batch harness test-helpers.el\n' "$file"
        done
    fi
}

# --- 3a. the real repository is clean ----------------------------------------
violations="$(scan "$REPO_ROOT")"
if [ -z "$violations" ]; then
    pass "scan: every test entry point in this repository routes through bin/background.sh"
else
    fail "scan: every test entry point in this repository routes through bin/background.sh" "$(printf '\n%s' "$violations")"
fi

# make_fixture DIR -- a minimal CLEAN tree with one of each entry-point kind.
make_fixture() {
    local root="$1" m="$1/modules/app/agent-repl"
    mkdir -p "$m/bin" "$m/pkg" "$m/svc" "$m/e2e/sandbox/bin" "$m/lisp" "$root/.githooks" "$root/.claude"
    cp "$HELPER" "$m/bin/background.sh"
    printf '// guard\n' >"$m/bin/require-background.mjs"
    cat >"$m/bin/test-thing.sh" <<'EOF'
#!/usr/bin/env bash
# a harness
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"
set -euo pipefail
EOF
    cat >"$m/svc/Makefile" <<'EOF'
BACKGROUND := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))../bin/background.sh)
test:
	TMPDIR=/tmp $(BACKGROUND) go test ./... \
		-count=1
build:
	go build ./...
EOF
    cat >"$m/pkg/package.json" <<'EOF'
{ "scripts": {
  "build": "node build.mjs",
  "pretest": "../bin/background.sh npm run ensure-deps",
  "test": "../bin/background.sh vitest run",
  "pretest:integration": "../bin/background.sh sh -c 'npm run build && npm run lock'"
} }
EOF
    cat >"$m/pkg/vitest.config.ts" <<'EOF'
import "../bin/require-background.mjs";
import { defineConfig } from "vitest/config";
EOF
    cat >"$m/e2e/sandbox/bin/entrypoint.sh" <<'EOF'
#!/usr/bin/env bash
exec "$REPO/$MODULE_REL/bin/background.sh" "$@"
EOF
    cat >"$root/.githooks/pre-commit" <<'EOF'
#!/usr/bin/env bash
./lint.sh
EOF
    mkdir -p "$m/daemon/internal/deploy"
    printf 'package deploy\n' >"$m/daemon/internal/deploy/builder.go"
    printf ';;; daemon.el\n' >"$m/lisp/daemon.el"
    printf '(defun agent-repl-test--require-background-priority (m b))\n(agent-repl-test--require-background-priority\n (getenv "X") noninteractive)\n' >"$m/lisp/test-helpers.el"
    printf '(load (expand-file-name "test-helpers.el" dir) nil t)\n' >"$m/lisp/test-daemon.el"
}

# expect_violation NAME DIR PATTERN -- the scan of DIR reports PATTERN.
expect_violation() {
    local name="$1" dir="$2" pattern="$3" out
    out="$(scan "$dir")"
    if grep_in "$out" -Fq -- "$pattern"; then
        pass "scan catches: $name"
    else
        fail "scan catches: $name" "scan output: $out"
    fi
}

# --- 3b. the clean fixture is clean -------------------------------------------
d="$TMP/s-clean"; make_fixture "$d"
out="$(scan "$d")"
if [ -z "$out" ]; then
    pass "scan: the clean fixture tree reports nothing"
else
    fail "scan: the clean fixture tree reports nothing" "$out"
fi

# --- 3c. a test script without the prologue -----------------------------------
d="$TMP/s-noprologue"; make_fixture "$d"
printf '#!/usr/bin/env bash\nset -euo pipefail\n' >"$d/modules/app/agent-repl/bin/test-thing.sh"
expect_violation "a test script without the prologue" "$d" "test-thing.sh: first executable line is not the background prologue"

# --- 3d. a test script whose prologue names another helper --------------------
d="$TMP/s-wrongpath"; make_fixture "$d"
sed -i.bak 's|/background.sh" bash|/other.sh" bash|' "$d/modules/app/agent-repl/bin/test-thing.sh"
expect_violation "a prologue naming a path that is not the helper" "$d" "prologue names other.sh"

# --- 3e. a prologue that is not the first executable line ---------------------
d="$TMP/s-late"; make_fixture "$d"
{ printf '#!/usr/bin/env bash\nset -euo pipefail\n'; sed -n 3p "$d/modules/app/agent-repl/bin/test-thing.sh"; } >"$d/t" &&
    mv "$d/t" "$d/modules/app/agent-repl/bin/test-thing.sh"
expect_violation "a prologue that runs after other code" "$d" "test-thing.sh: first executable line is not the background prologue"

# --- 3f. a Makefile recipe running go test unwrapped --------------------------
d="$TMP/s-make"; make_fixture "$d"
printf 'integration:\n\tgo test -tags integration ./...\n' >>"$d/modules/app/agent-repl/svc/Makefile"
expect_violation "a Makefile recipe running go test without \$(BACKGROUND)" "$d" 'recipe runs a suite without $(BACKGROUND): go test -tags integration'

# --- 3g. a Makefile whose BACKGROUND is not the helper ------------------------
d="$TMP/s-makedef"; make_fixture "$d"
sed -i.bak 's|../bin/background.sh)|../bin/nothing.sh)|' "$d/modules/app/agent-repl/svc/Makefile"
expect_violation "a Makefile whose BACKGROUND names another path" "$d" "BACKGROUND names ../bin/nothing.sh"

# --- 3h. a package test script without the helper ------------------------------
d="$TMP/s-npm"; make_fixture "$d"
sed -i.bak 's|"test": "../bin/background.sh vitest run"|"test": "vitest run"|' "$d/modules/app/agent-repl/pkg/package.json"
expect_violation "a package test script that does not start with the helper" "$d" 'script "test" does not start with the background helper'

# --- 3i. a compound pre-hook whose second half runs unwrapped -------------------
d="$TMP/s-npmchain"; make_fixture "$d"
sed -i.bak "s|\"../bin/background.sh sh -c 'npm run build \&\& npm run lock'\"|\"../bin/background.sh npm run build \&\& npm run lock\"|" "$d/modules/app/agent-repl/pkg/package.json"
expect_violation "a compound package script chaining outside the helper" "$d" 'script "pretest:integration" chains commands outside the helper'

# --- 3j. a vitest-running script with a non-test name ----------------------------
d="$TMP/s-npmother"; make_fixture "$d"
sed -i.bak 's|"build": "node build.mjs",|"build": "node build.mjs", "check": "vitest run",|' "$d/modules/app/agent-repl/pkg/package.json"
expect_violation "a vitest-running package script under a non-test name" "$d" 'script "check" does not start with the background helper'

# --- 3k. a build script that was demoted -----------------------------------------
d="$TMP/s-npmbuild"; make_fixture "$d"
sed -i.bak 's|"build": "node build.mjs"|"build": "../bin/background.sh node build.mjs"|' "$d/modules/app/agent-repl/pkg/package.json"
expect_violation "a demoted build script" "$d" 'build script "build" is demoted'

# --- 3l. a vitest config without the guard ---------------------------------------
d="$TMP/s-vitest"; make_fixture "$d"
printf 'import { defineConfig } from "vitest/config";\n' >"$d/modules/app/agent-repl/pkg/vitest.config.ts"
expect_violation "a vitest config that does not import the guard" "$d" "vitest.config.ts: does not import"

# --- 3m. a hook that runs a suite without the prologue ---------------------------
d="$TMP/s-hook"; make_fixture "$d"
printf '#!/usr/bin/env bash\nmodules/app/agent-repl/bin/test-all.sh\n' >"$d/.githooks/pre-commit"
expect_violation "a git hook that runs a suite without the prologue" "$d" "pre-commit: runs a test suite without the background prologue"

# --- 3n. the deploy path referencing the helper ----------------------------------
d="$TMP/s-deploy"; make_fixture "$d"
printf 'package deploy\n\nvar argv = []string{"bin/background.sh", "bash", "bin/build-frontend.sh"}\n' >"$d/modules/app/agent-repl/daemon/internal/deploy/builder.go"
expect_violation "the deploy path demoting its build" "$d" "builder.go: live runtime or deploy path references the test background helper"

# --- 3o. the live runtime's spawn demoting with nice ----------------------------
d="$TMP/s-runtime"; make_fixture "$d"
printf '(list "nice" "-n" "19" daemon)\n' >"$d/modules/app/agent-repl/lisp/daemon.el"
expect_violation "a live-runtime spawn demoting the daemon" "$d" "daemon.el: live runtime or deploy path references the test background helper"

# --- 3p. the sandbox entrypoint exec'ing its command bare ------------------------
d="$TMP/s-sandbox"; make_fixture "$d"
printf '#!/usr/bin/env bash\nexec "$@"\n' >"$d/modules/app/agent-repl/e2e/sandbox/bin/entrypoint.sh"
expect_violation "a sandbox entrypoint that execs its command bare" "$d" "entrypoint.sh: does not exec its command through bin/background.sh"

# --- 3q. an ERT suite that never loads the gated harness ------------------------
d="$TMP/s-ert"; make_fixture "$d"
printf '(require (quote ert))\n' >"$d/modules/app/agent-repl/lisp/test-daemon.el"
expect_violation "an ERT suite that does not load test-helpers.el" "$d" "test-daemon.el: does not load the gated batch harness"

# --- 3r. a batch harness whose gate call was dropped -----------------------------
d="$TMP/s-ertgate"; make_fixture "$d"
printf '(defun agent-repl-test--require-background-priority (m b))\n' >"$d/modules/app/agent-repl/lisp/test-helpers.el"
expect_violation "a batch harness that no longer calls its gate" "$d" "test-helpers.el: does not call agent-repl-test--require-background-priority"

echo
echo "test-background.sh: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
