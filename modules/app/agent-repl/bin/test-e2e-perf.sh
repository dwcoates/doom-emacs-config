#!/usr/bin/env bash
#
# Hermetic tests for e2e-perf.sh: each mode's go test command line and
# environment, against a stub go, git and ensure-e2e-deps.sh.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"

TMP="$(mktemp -d "${TMPDIR:-/tmp}/e2e-perf-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# make_tree TREE -- the real script beside a stub ensure-e2e-deps.sh, with
# stub go and git on PATH; each stub logs to $TREE/log.
make_tree() {
    local tree="$1"
    mkdir -p "$tree/bin" "$tree/e2e" "$tree/stubs"
    cp "$THIS_DIR/e2e-perf.sh" "$tree/bin/e2e-perf.sh"
    cat >"$tree/bin/ensure-e2e-deps.sh" <<'STUB'
#!/usr/bin/env bash
printf 'deps\n' >>"$STUB_LOG"
exit "${STUB_DEPS_EXIT:-0}"
STUB
    cat >"$tree/stubs/go" <<'STUB'
#!/usr/bin/env bash
printf 'go %s (in %s) perf=%s baseline=%s tip=%s\n' "$*" "$PWD" "${AGENT_REPL_E2E_PERF:-}" "${AGENT_REPL_E2E_PERF_BASELINE:-}" "${AGENT_REPL_E2E_PERF_TIP:-}" >>"$STUB_LOG"
STUB
    cat >"$tree/stubs/git" <<'STUB'
#!/usr/bin/env bash
printf 'git %s\n' "$*" >>"$STUB_LOG"
[ "${STUB_GIT_FAIL:-0}" = 1 ] && exit 1
echo abc1234
STUB
    chmod +x "$tree/bin/e2e-perf.sh" "$tree/bin/ensure-e2e-deps.sh" "$tree/stubs/go" "$tree/stubs/git"
}

# run_perf TREE ARGS... -- RC and the log; the run is already marked, so the
# script runs straight through.
run_perf() {
    local tree="$1"
    shift
    : >"$tree/log"
    set +e
    PATH="$tree/stubs:/usr/bin:/bin" STUB_LOG="$tree/log" AGENT_REPL_BACKGROUND_PRIORITY=nice-19 \
        bash "$tree/bin/e2e-perf.sh" "$@" >"$tree/out" 2>"$tree/err"
    RC=$?
    set -e
}

for case_ in \
    "report|-count=1|perf=1 baseline= tip=" \
    "enforce|-count=1|perf=1 baseline=enforce tip=" \
    "baseline|-count=5|perf=1 baseline=write tip=abc1234"; do
    IFS='|' read -r mode count env <<<"$case_"
    tree="$TMP/$mode"
    make_tree "$tree"
    run_perf "$tree" "$mode"
    e2e="$(cd "$tree/e2e" && pwd)"
    want="go test ./... -tags perf -run TestPerf $count -timeout 45m -parallel 1 -v (in $e2e) $env"
    if [ "$RC" -eq 0 ] && [ "$(head -1 "$tree/log")" = "deps" ] && grep -qxF "$want" "$tree/log"; then
        pass "$mode: provisions the e2e deps, then runs the perf tests serially with its own environment"
    else
        fail "$mode: provisions the e2e deps, then runs the perf tests serially with its own environment" "rc=$RC want: $want" "log: $(cat "$tree/log")" "err: $(cat "$tree/err")"
    fi
done

tree="$TMP/timeout"
make_tree "$tree"
: >"$tree/log"
set +e
PATH="$tree/stubs:/usr/bin:/bin" STUB_LOG="$tree/log" AGENT_REPL_BACKGROUND_PRIORITY=nice-19 E2E_PERF_TIMEOUT=90m \
    bash "$tree/bin/e2e-perf.sh" report >/dev/null 2>&1
RC=$?
set -e
if [ "$RC" -eq 0 ] && grep -q -- '-timeout 90m ' "$tree/log"; then
    pass "E2E_PERF_TIMEOUT overrides the go test timeout"
else
    fail "E2E_PERF_TIMEOUT overrides the go test timeout" "log: $(cat "$tree/log")"
fi

for refused in "" "bogus" "report extra"; do
    tree="$TMP/refused-${refused// /-}"
    make_tree "$tree"
    # shellcheck disable=SC2086 # the case's words are the arguments
    run_perf "$tree" $refused
    if [ "$RC" -eq 2 ] && [ ! -s "$tree/log" ] && grep -q 'usage: e2e-perf.sh report|baseline|enforce' "$tree/err"; then
        pass "arguments '$refused' are refused with the usage and nothing run"
    else
        fail "arguments '$refused' are refused with the usage and nothing run" "rc=$RC log: $(cat "$tree/log") err: $(cat "$tree/err")"
    fi
done

tree="$TMP/deps"
make_tree "$tree"
STUB_DEPS_EXIT=3 run_perf "$tree" report
if [ "$RC" -eq 2 ] && ! grep -q '^go ' "$tree/log" && grep -q "npm deps could not be ensured" "$tree/err"; then
    pass "a failed deps provision stops the run before any measurement"
else
    fail "a failed deps provision stops the run before any measurement" "rc=$RC log: $(cat "$tree/log")"
fi

tree="$TMP/tip"
make_tree "$tree"
STUB_GIT_FAIL=1 run_perf "$tree" baseline
if [ "$RC" -eq 2 ] && ! grep -q '^go ' "$tree/log" && grep -q "the tip the baselines are measured at could not be read" "$tree/err"; then
    pass "a baseline whose tip cannot be read writes nothing"
else
    fail "a baseline whose tip cannot be read writes nothing" "rc=$RC log: $(cat "$tree/log")"
fi

harness_finish
