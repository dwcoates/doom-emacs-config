#!/usr/bin/env bash
# test-e2e-repeat.sh — hermetic tests for e2e-repeat.sh's evidence rules.
#
# The script's whole reason to exist is that a red run's artifacts survive, so
# that is what is asserted here: one directory per run, a transcript inside it,
# and NOTHING removed when a run goes red. No container and no Go toolchain is
# involved — a throwaway module root is built around a copy of the script with
# stub `suite-slot.sh` and `e2e-sandbox.sh` on the paths the script resolves.
#
# Run with:   bash bin/test-e2e-repeat.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -uo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/e2e-repeat.sh"

FAILURES=0
pass() { printf 'ok   - %s\n' "$1"; }
fail() { printf 'FAIL - %s\n     %s\n' "$1" "$2"; FAILURES=$((FAILURES + 1)); }

# make_root <exit-status-script> — a throwaway module root whose sandbox stub
# exits with the status the given snippet prints for the run index in $RUN_INDEX.
make_root() {
    local root
    root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
    mkdir -p "$root/bin" "$root/e2e/sandbox/bin"
    cp "$SCRIPT_UNDER_TEST" "$root/bin/e2e-repeat.sh"
    cat > "$root/bin/suite-slot.sh" <<'STUB'
#!/usr/bin/env bash
exec "$@"
STUB
    cat > "$root/e2e/sandbox/bin/e2e-sandbox.sh" <<STUB
#!/usr/bin/env bash
echo "sandbox stub argv: \$*"
count_file="\$AGENT_REPL_E2E_ARTIFACTS/../run-count"
n=\$(cat "\$count_file" 2>/dev/null || echo 0)
n=\$((n + 1))
echo "\$n" > "\$count_file"
$1
STUB
    chmod +x "$root/bin/suite-slot.sh" "$root/e2e/sandbox/bin/e2e-sandbox.sh"
    printf '%s\n' "$root"
}

# --- a passing sweep gives every run its own directory ----------------------
root=$(make_root 'exit 0')
out_root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
"$root/bin/e2e-repeat.sh" --runs 3 --root "$out_root" >/dev/null 2>&1
status=$?
if (( status != 0 )); then
    fail "an all-green sweep exits 0" "exit status was $status"
else
    pass "an all-green sweep exits 0"
fi
dirs=$(find "$out_root" -maxdepth 1 -type d -name 'run-*' | wc -l | tr -d ' ')
if [[ $dirs == 3 ]]; then
    pass "each run gets its own artifacts directory"
else
    fail "each run gets its own artifacts directory" "found $dirs, wanted 3"
fi
if [[ -s "$out_root/run-001/go-test.log" ]]; then
    pass "a run's transcript lands beside its artifacts"
else
    fail "a run's transcript lands beside its artifacts" "run-001/go-test.log is missing or empty"
fi
rm -rf "$root" "$out_root"

# --- a red run's evidence survives the rest of the sweep --------------------
root=$(make_root 'echo "    --- FAIL: TestSomething (0.10s)"; [[ $n == 2 ]] && exit 1; exit 0')
out_root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
"$root/bin/e2e-repeat.sh" --runs 3 --root "$out_root" >/dev/null 2>&1
status=$?
if (( status == 1 )); then
    pass "a sweep with a red run exits non-zero"
else
    fail "a sweep with a red run exits non-zero" "exit status was $status"
fi
if [[ -s "$out_root/run-002/go-test.log" && $(cat "$out_root/run-002/exit-status") == 1 ]]; then
    pass "the red run's transcript and status are kept"
else
    fail "the red run's transcript and status are kept" "run-002 lost its evidence"
fi
dirs=$(find "$out_root" -maxdepth 1 -type d -name 'run-*' | wc -l | tr -d ' ')
if [[ $dirs == 3 ]]; then
    pass "a red run does not stop the sweep and nothing is removed"
else
    fail "a red run does not stop the sweep and nothing is removed" "found $dirs run dirs, wanted 3"
fi
rm -rf "$root" "$out_root"

# --- --stop-on-fail halts at the red run, keeping it ------------------------
root=$(make_root 'echo "    --- FAIL: TestSomething (0.10s)"; [[ $n == 1 ]] && exit 1; exit 0')
out_root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
"$root/bin/e2e-repeat.sh" --runs 4 --stop-on-fail --root "$out_root" >/dev/null 2>&1
dirs=$(find "$out_root" -maxdepth 1 -type d -name 'run-*' | wc -l | tr -d ' ')
if [[ $dirs == 1 && -s "$out_root/run-001/go-test.log" ]]; then
    pass "--stop-on-fail halts at the first red and keeps it"
else
    fail "--stop-on-fail halts at the first red and keeps it" "found $dirs run dirs"
fi
rm -rf "$root" "$out_root"

# --- the load gate waits for a quiet box, and records what it started at ----
root=$(make_root 'exit 0')
out_root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
# A stub `uptime` first on PATH: two loaded readings, then a quiet one, so the
# gate has to actually WAIT rather than take the first value it sees.
stub_dir=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
cat > "$stub_dir/uptime" <<'STUB'
#!/usr/bin/env bash
n=$(cat "$AGENT_REPL_TEST_UPTIME_COUNT" 2>/dev/null || echo 0)
n=$((n + 1)); echo "$n" > "$AGENT_REPL_TEST_UPTIME_COUNT"
if (( n < 3 )); then
    echo "12:00  up 1:00, 1 user, load averages: 42.00 40.00 39.00"
else
    echo "12:00  up 1:00, 1 user, load averages: 1.50 2.00 3.00"
fi
STUB
chmod +x "$stub_dir/uptime"
export AGENT_REPL_TEST_UPTIME_COUNT="$stub_dir/count"
PATH="$stub_dir:$PATH" "$root/bin/e2e-repeat.sh" --runs 1 --root "$out_root" >/dev/null 2>&1
if [[ $(cat "$out_root/run-001/start-load" 2>/dev/null) == "1.50" ]]; then
    pass "a run waits for a quiet box and records the load it started at"
else
    fail "a run waits for a quiet box and records the load it started at"          "start-load was '$(cat "$out_root/run-001/start-load" 2>/dev/null)', wanted 1.50"
fi
rm -rf "$root" "$out_root" "$stub_dir"

# --- --no-load-gate starts under load, and still records it -----------------
root=$(make_root 'exit 0')
out_root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
stub_dir=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
cat > "$stub_dir/uptime" <<'STUB'
#!/usr/bin/env bash
echo "12:00  up 1:00, 1 user, load averages: 42.00 40.00 39.00"
STUB
chmod +x "$stub_dir/uptime"
PATH="$stub_dir:$PATH" "$root/bin/e2e-repeat.sh" --runs 1 --no-load-gate --root "$out_root" >/dev/null 2>&1
if [[ $(cat "$out_root/run-001/start-load" 2>/dev/null) == "42.00" ]]; then
    pass "--no-load-gate runs under load and still records it"
else
    fail "--no-load-gate runs under load and still records it"          "start-load was '$(cat "$out_root/run-001/start-load" 2>/dev/null)'"
fi
rm -rf "$root" "$out_root" "$stub_dir"

# --- a host with no load average is said, never assumed quiet ---------------
root=$(make_root 'exit 0')
out_root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
stub_dir=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
printf '#!/usr/bin/env bash
echo "no load line here"
' > "$stub_dir/uptime"
chmod +x "$stub_dir/uptime"
PATH="$stub_dir:$PATH" "$root/bin/e2e-repeat.sh" --runs 1 --root "$out_root" >"$out_root/driver.log" 2>&1
if [[ $(cat "$out_root/run-001/start-load" 2>/dev/null) == "unavailable" ]] &&
   grep -q "reports no load average" "$out_root/driver.log"; then
    pass "a host with no load average is said out loud, not assumed quiet"
else
    fail "a host with no load average is said out loud, not assumed quiet"          "start-load '$(cat "$out_root/run-001/start-load" 2>/dev/null)'"
fi
rm -rf "$root" "$out_root" "$stub_dir"

# --- a malformed load bound is refused, never defaulted ---------------------
root=$(make_root 'exit 0')
out_root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
"$root/bin/e2e-repeat.sh" --runs 1 --load-max zero --root "$out_root" >/dev/null 2>&1
status=$?
if (( status == 2 )); then
    pass "a non-numeric --load-max is refused"
else
    fail "a non-numeric --load-max is refused" "exit status was $status"
fi
rm -rf "$root" "$out_root"

# --- a malformed run count is refused, never defaulted ----------------------
root=$(make_root 'exit 0')
out_root=$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")
"$root/bin/e2e-repeat.sh" --runs zero --root "$out_root" >/dev/null 2>&1
status=$?
if (( status == 2 )); then
    pass "a non-numeric --runs is refused"
else
    fail "a non-numeric --runs is refused" "exit status was $status"
fi
rm -rf "$root" "$out_root"

printf '\n'
if (( FAILURES == 0 )); then
    printf 'test-e2e-repeat.sh: all assertions passed\n'
    exit 0
fi
printf 'test-e2e-repeat.sh: %d assertion(s) failed\n' "$FAILURES"
exit 1
