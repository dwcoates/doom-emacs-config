#!/usr/bin/env bash
# Hermetic fixture tests for bin/logs.sh.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
LOGS="$THIS_DIR/logs.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0

pass() {
    printf '  PASS: %s\n' "$1"
    PASS=$((PASS + 1))
}

fail() {
    printf '  FAIL: %s\n' "$1" >&2
    FAIL=$((FAIL + 1))
}

bin="$TMP/bin"
home="$TMP/home"
state="$TMP/state"
cache="$TMP/cache"
runtime_tmp="$TMP/runtime"
targets="$TMP/targets"
workspace_a="$TMP/workspaces/alpha-dir"
workspace_b="$TMP/workspaces/beta-dir"
rows="$TMP/workspaces.tsv"
mkdir -p "$bin" "$home" "$state/logs" "$cache/agent-repl/log" "$runtime_tmp" \
    "$targets" "$workspace_a/.claude/emacs" "$workspace_b/.claude/emacs"
: >"$state/wsm.db"

cat >"$bin/sqlite3" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
[ -f "${AGENT_REPL_LOGS_TEST_ROWS:?workspace rows fixture required}" ]
cat "$AGENT_REPL_LOGS_TEST_ROWS"
EOF
chmod +x "$bin/sqlite3"

printf 'ws-a\t%s\talpha\nws-b\t%s\tbeta\n' "$workspace_a" "$workspace_b" >"$rows"

cat >"$targets/alpha-daemon.log.1" <<EOF
{"timestamp":"2010-01-01T00:00:00.000000Z","runtime":"daemon","pid":10,"level":"debug","verbosity":"normal","operation":"daemon.old","message":"outside duration","context":{},"workspace_dir":"$workspace_a","workspace_id":"ws-a"}
{"timestamp":"2026-09-10T10:00:00.000000Z","runtime":"daemon","pid":10,"level":"debug","verbosity":"normal","operation":"daemon.rotated","message":"oldest selected record","context":{"generation":1},"workspace_dir":"$workspace_a","workspace_id":"ws-a"}
EOF
cat >"$targets/alpha-daemon.log" <<EOF
{"timestamp":"2026-09-10T10:03:00.000000Z","runtime":"daemon","pid":10,"level":"info","verbosity":"normal","operation":"daemon.third","message":"third record","context":{"order":3},"workspace_dir":"$workspace_a","workspace_id":"ws-a"}
{"timestamp":"2026-09-10T10:01:00.000000Z","runtime":"daemon","pid":10,"level":"warn","verbosity":"normal","operation":"daemon.warning","message":"repeated warning","context":{"order":1},"workspace_dir":"$workspace_a","workspace_id":"ws-a"}
{"timestamp":"2026-09-10T10:01:30.000000Z","runtime":"daemon","pid":10,"level":"warn","verbosity":"normal","operation":"daemon.warning","message":"repeated warning","context":{"order":2},"workspace_dir":"$workspace_a","workspace_id":"ws-a"}
EOF
cat >"$targets/alpha-shim.log" <<EOF
{"timestamp":"2026-09-10T10:02:00.000000Z","runtime":"shim","pid":11,"level":"error","verbosity":"normal","operation":"shim.failure","message":"shim failed","context":{"cause":"fixture"},"workspace_dir":"$workspace_a","workspace_id":"ws-a"}
EOF
cat >"$targets/alpha-webapp.log" <<EOF
{"timestamp":"2026-09-10T10:02:30.000000Z","runtime":"webapp","connection_id":"page-1","level":"info","verbosity":"normal","operation":"webapp.ready","message":"page ready","context":{},"workspace_dir":"$workspace_a","workspace_id":"ws-a"}
EOF
ln -s "$targets/alpha-daemon.log" "$workspace_a/.claude/emacs/daemon.log"
ln -s "$targets/alpha-shim.log" "$workspace_a/.claude/emacs/shim.log"
ln -s "$targets/alpha-webapp.log" "$workspace_a/.claude/emacs/webapp.log"

cat >"$targets/beta-daemon.log" <<EOF
{"timestamp":"2026-09-10T10:04:00.000000Z","runtime":"daemon","pid":20,"level":"warn","verbosity":"normal","operation":"daemon.beta","message":"beta warning","context":{},"workspace_dir":"$workspace_b","workspace_id":"ws-b"}
EOF
ln -s "$targets/beta-daemon.log" "$workspace_b/.claude/emacs/daemon.log"

cat >"$state/logs/daemon.run.log" <<'EOF'
{"timestamp":"2026-09-10T10:00:30.000000Z","runtime":"daemon","pid":30,"level":"info","verbosity":"normal","operation":"daemon.boot","message":"daemon central","context":{}}
EOF
cat >"$cache/agent-repl/log/shim-store.log" <<'EOF'
{"timestamp":"2026-09-10T10:05:00.000000Z","runtime":"store","pid":40,"level":"error","verbosity":"normal","operation":"store.failure","message":"store failed","context":{"cause":"fixture"}}
EOF
emacs_global="$runtime_tmp/emacs-global.log"
cat >"$emacs_global" <<'EOF'
{"timestamp":"2026-09-10T10:00:15.000000Z","runtime":"emacs","pid":50,"level":"info","verbosity":"normal","operation":"emacs.ready","message":"emacs central","context":{}}
EOF

run_logs() {
    PATH="$bin:$PATH" \
        HOME="$home" \
        TMPDIR="$runtime_tmp" \
        GOCACHE="$TMP/go-cache" \
        AGENT_REPL_LOGS_BUILD_DIR="$TMP/build" \
        AGENT_REPL_LOGS_TEST_ROWS="$rows" \
        AGENT_REPL_STATE_DIR="$state" \
        XDG_CACHE_HOME="$cache" \
        AGENT_REPL_EMACS_GLOBAL_LOG="$emacs_global" \
        TZ=UTC \
        "$LOGS" "$@"
}

test_workspace_directory_and_default_format() {
    local out first_operation last_operation
    out="$(run_logs --workspace "$workspace_a")"
    first_operation="$(printf '%s\n' "$out" | sed -n '2p' | awk '{print $4}')"
    last_operation="$(printf '%s\n' "$out" | tail -n 1 | awk '{print $4}')"
    if [ "$first_operation" = daemon.rotated ] &&
        [ "$last_operation" = daemon.third ] &&
        printf '%s\n' "$out" | grep -q 'WARN  daemon.*daemon.warning.*workspace_id=ws-a.*context={"order":1}'; then
        pass "--workspace directory emits compact local-time records merged with rotations"
    else
        fail "--workspace directory emits compact local-time records merged with rotations"
    fi
}

test_workspace_id() {
    local out
    out="$(run_logs --workspace ws-a --runtime shim --json)"
    if printf '%s\n' "$out" | grep -q '"operation":"shim.failure"'; then
        pass "--workspace resolves a daemon workspace ID"
    else
        fail "--workspace resolves a daemon workspace ID"
    fi
}

test_workspace_name() {
    local out
    out="$(run_logs --workspace alpha --runtime webapp --json)"
    if printf '%s\n' "$out" | grep -q '"operation":"webapp.ready"'; then
        pass "--workspace resolves a daemon workspace name"
    else
        fail "--workspace resolves a daemon workspace name"
    fi
}

test_central() {
    local out
    out="$(run_logs --central --json)"
    if printf '%s\n' "$out" | grep -q '"operation":"emacs.ready"' &&
        printf '%s\n' "$out" | grep -q '"operation":"daemon.boot"' &&
        printf '%s\n' "$out" | grep -q '"operation":"store.failure"'; then
        pass "--central selects every central sink"
    else
        fail "--central selects every central sink"
    fi
}

test_all() {
    local out
    out="$(run_logs --all --level warn --json)"
    if printf '%s\n' "$out" | grep -q '"workspace_id":"ws-a"' &&
        printf '%s\n' "$out" | grep -q '"workspace_id":"ws-b"' &&
        printf '%s\n' "$out" | grep -q '"runtime":"store"'; then
        pass "--all selects daemon-known workspaces and central sinks"
    else
        fail "--all selects daemon-known workspaces and central sinks"
    fi
}

test_since_rfc3339() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime daemon --since 2026-09-10T10:02:00Z --json)"
    if printf '%s\n' "$out" | grep -q '"operation":"daemon.third"' &&
        ! printf '%s\n' "$out" | grep -q '"operation":"daemon.warning"'; then
        pass "--since accepts an RFC3339 lower bound"
    else
        fail "--since accepts an RFC3339 lower bound"
    fi
}

test_since_duration() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime daemon --since 100000h --json)"
    if printf '%s\n' "$out" | grep -q '"operation":"daemon.rotated"' &&
        ! printf '%s\n' "$out" | grep -q '"operation":"daemon.old"'; then
        pass "--since accepts a lookback duration"
    else
        fail "--since accepts a lookback duration"
    fi
}

test_until() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime daemon --since 2026-01-01T00:00:00Z --until 2026-09-10T10:01:00Z --json)"
    if printf '%s\n' "$out" | grep -q '"timestamp":"2026-09-10T10:01:00.000000Z"' &&
        ! printf '%s\n' "$out" | grep -q '"timestamp":"2026-09-10T10:01:30.000000Z"'; then
        pass "--until applies an inclusive RFC3339 upper bound"
    else
        fail "--until applies an inclusive RFC3339 upper bound"
    fi
}

test_level() {
    local out
    out="$(run_logs --workspace "$workspace_a" --level warn --json)"
    if printf '%s\n' "$out" | grep -q '"level":"warn"' &&
        printf '%s\n' "$out" | grep -q '"level":"error"' &&
        ! printf '%s\n' "$out" | grep -q '"level":"info"'; then
        pass "--level applies the requested minimum severity"
    else
        fail "--level applies the requested minimum severity"
    fi
}

test_runtime_list() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime daemon,shim --json)"
    if printf '%s\n' "$out" | grep -q '"runtime":"daemon"' &&
        printf '%s\n' "$out" | grep -q '"runtime":"shim"' &&
        ! printf '%s\n' "$out" | grep -q '"runtime":"webapp"'; then
        pass "--runtime accepts a comma-separated runtime list"
    else
        fail "--runtime accepts a comma-separated runtime list"
    fi
}

test_json() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime shim --json)"
    if [ "${out#\{}" != "$out" ] && ! printf '%s\n' "$out" | grep -q 'context='; then
        pass "--json emits the original JSONL record"
    else
        fail "--json emits the original JSONL record"
    fi
}

test_harvest() {
    local out
    out="$(run_logs --harvest 2026-09-10T10:00:00Z 2026-09-10T10:10:00Z)"
    if printf '%s\n' "$out" | grep -Eq "ws-a +$workspace_a +warn +daemon +daemon.warning +repeated warning +2" &&
        printf '%s\n' "$out" | grep -Eq 'central +- +error +store +store.failure +store failed +1'; then
        pass "--harvest attributes and counts all warn/error records"
    else
        fail "--harvest attributes and counts all warn/error records"
    fi
}

test_empty_harvest_window() {
    local out rc
    set +e
    out="$(run_logs --harvest 2026-09-11T00:00:00Z 2026-09-11T01:00:00Z)"
    rc=$?
    set -e
    if [ "$rc" -eq 0 ] && [ "$(printf '%s\n' "$out" | wc -l | tr -d ' ')" -eq 1 ] &&
        printf '%s\n' "$out" | grep -q '^WORKSPACE_ID'; then
        pass "an empty harvest window prints its header and exits zero"
    else
        fail "an empty harvest window prints its header and exits zero"
    fi
}

test_malformed_line() {
    local workspace="$TMP/workspaces/malformed" target="$targets/malformed.log" rc
    mkdir -p "$workspace/.claude/emacs"
    printf '%s\n' 'not-json' >"$target"
    ln -s "$target" "$workspace/.claude/emacs/daemon.log"
    set +e
    run_logs --workspace "$workspace" --runtime daemon --json >"$TMP/malformed.out" 2>"$TMP/malformed.err"
    rc=$?
    set -e
    if [ "$rc" -ne 0 ] && grep -q "malformed.log:1: malformed JSONL" "$TMP/malformed.err"; then
        pass "a malformed JSONL line is reported with its source"
    else
        fail "a malformed JSONL line is reported with its source"
    fi
}

test_absent_selected_log() {
    local workspace="$TMP/workspaces/no-logs" rc
    mkdir -p "$workspace"
    set +e
    run_logs --workspace "$workspace" --runtime daemon --json >"$TMP/absent.out" 2>"$TMP/absent.err"
    rc=$?
    set -e
    if [ "$rc" -ne 0 ] && grep -q 'none of the selected log files or rotation generations exists' "$TMP/absent.err"; then
        pass "an absent selected log fails instead of reporting an empty read"
    else
        fail "an absent selected log fails instead of reporting an empty read"
    fi
}

wait_for_pattern() {
    local pattern="$1" path="$2" pid="$3" started=$SECONDS
    while ! grep -q "$pattern" "$path" 2>/dev/null; do
        kill -0 "$pid" 2>/dev/null || return 1
        [ $((SECONDS - started)) -lt 5 ] || return 1
        sleep 0.02
    done
}

test_follow() {
    local target="$targets/follow.log" workspace="$TMP/workspaces/follow" pid rc=0
    mkdir -p "$workspace/.claude/emacs"
    cat >"$target" <<EOF
{"timestamp":"2026-09-10T11:00:00.000000Z","runtime":"daemon","pid":60,"level":"info","verbosity":"normal","operation":"daemon.follow.initial","message":"initial","context":{},"workspace_dir":"$workspace","workspace_id":"ws-follow"}
EOF
    ln -s "$target" "$workspace/.claude/emacs/daemon.log"
    PATH="$bin:$PATH" \
        HOME="$home" \
        TMPDIR="$runtime_tmp" \
        GOCACHE="$TMP/go-cache" \
        AGENT_REPL_LOGS_BUILD_DIR="$TMP/build" \
        AGENT_REPL_LOGS_TEST_ROWS="$rows" \
        AGENT_REPL_STATE_DIR="$state" \
        XDG_CACHE_HOME="$cache" \
        AGENT_REPL_EMACS_GLOBAL_LOG="$emacs_global" \
        TZ=UTC \
        "$LOGS" --workspace "$workspace" --runtime daemon --follow --json \
        >"$TMP/follow.out" 2>"$TMP/follow.err" &
    pid=$!
    if wait_for_pattern 'daemon.follow.initial' "$TMP/follow.out" "$pid"; then
        cat >>"$target" <<EOF
{"timestamp":"2026-09-10T11:01:00.000000Z","runtime":"daemon","pid":60,"level":"info","verbosity":"normal","operation":"daemon.follow.appended","message":"appended","context":{},"workspace_dir":"$workspace","workspace_id":"ws-follow"}
EOF
        wait_for_pattern 'daemon.follow.appended' "$TMP/follow.out" "$pid" || rc=1
    else
        rc=1
    fi
    kill -TERM "$pid" 2>/dev/null || rc=1
    if ! wait "$pid"; then
        rc=1
    fi
    if [ "$rc" -eq 0 ]; then
        pass "--follow emits records appended after startup"
    else
        fail "--follow emits records appended after startup"
        sed -n '1,80p' "$TMP/follow.err" >&2
    fi
}

test_workspace_directory_and_default_format
test_workspace_id
test_workspace_name
test_central
test_all
test_since_rfc3339
test_since_duration
test_until
test_level
test_runtime_list
test_json
test_harvest
test_empty_harvest_window
test_malformed_line
test_absent_selected_log
test_follow

printf '%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
