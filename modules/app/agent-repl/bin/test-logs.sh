#!/usr/bin/env bash
# Hermetic fixture tests for bin/logs.sh.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

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

make_empty_workspace_sink() {
    local workspace="$1" label="$2" runtime="$3" target
    target="$targets/$label-$runtime.log"
    : >"$target"
    ln -s "$target" "$workspace/.claude/emacs/$runtime.log"
}

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
make_empty_workspace_sink "$workspace_a" alpha emacs
make_empty_workspace_sink "$workspace_a" alpha sidecar

cat >"$targets/beta-daemon.log" <<EOF
{"timestamp":"2026-09-10T10:04:00.000000Z","runtime":"daemon","pid":20,"level":"warn","verbosity":"normal","operation":"daemon.beta","message":"beta warning","context":{},"workspace_dir":"$workspace_b","workspace_id":"ws-b"}
EOF
ln -s "$targets/beta-daemon.log" "$workspace_b/.claude/emacs/daemon.log"
make_empty_workspace_sink "$workspace_b" beta emacs
make_empty_workspace_sink "$workspace_b" beta shim
make_empty_workspace_sink "$workspace_b" beta webapp
make_empty_workspace_sink "$workspace_b" beta sidecar

cat >"$state/logs/daemon.run.log" <<'EOF'
{"timestamp":"2026-09-10T10:00:30.000000Z","runtime":"daemon","pid":30,"level":"info","verbosity":"normal","operation":"daemon.boot","message":"daemon central","context":{}}
EOF
cat >"$cache/agent-repl/log/shim-store.log" <<'EOF'
{"timestamp":"2026-09-10T10:05:00.000000Z","runtime":"store","pid":40,"level":"error","verbosity":"normal","operation":"store.failure","message":"store failed","context":{"cause":"fixture"}}
EOF
: >"$cache/agent-repl/log/shim-claude-sidecar.log"
emacs_global="$runtime_tmp/emacs-global.log"
cat >"$emacs_global" <<'EOF'
{"timestamp":"2026-09-10T10:00:15.000000Z","runtime":"emacs","pid":50,"level":"info","verbosity":"normal","operation":"emacs.ready","message":"emacs central","context":{}}
EOF

# The store's emergency stderr sink: unstructured text, not JSONL. One line
# names an error plainly; the other does not, so --level exercises the
# inferred default of warn against the inferred error.
cat >"$cache/agent-repl/log/shim-store.err.log" <<'EOF'
unexpected error: disk write failed
heartbeat skipped this cycle
EOF

# A captured Emacs *Messages* snapshot, read the same way
# e2e/realtest/messages.go scrapes it: only lines matching a known severity
# shape become records, everything else is prose and is skipped.
messages_file="$runtime_tmp/Messages.txt"
cat >"$messages_file" <<'EOF'
Loading personal-bindings...done
WARNING: the module warned about a fixture condition
just an ordinary echo line nobody cares about
Wrong type argument: stringp, nil
EOF

run_logs() {
    PATH="$bin:$PATH" \
        HOME="$home" \
        TMPDIR="$runtime_tmp" \
        GOCACHE="$TMP/go-cache" \
        AGENT_REPL_LOGS_BUILD_DIR="$TMP/build" \
        AGENT_REPL_LOGS_TEST_ROWS="${AGENT_REPL_LOGS_TEST_ROWS_OVERRIDE:-$rows}" \
        AGENT_REPL_STATE_DIR="$state" \
        XDG_CACHE_HOME="$cache" \
        AGENT_REPL_EMACS_GLOBAL_LOG="${AGENT_REPL_EMACS_GLOBAL_LOG_OVERRIDE-$emacs_global}" \
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
        printf '%s\n' "$out" | grep -q 'WARN  daemon.*daemon.warning.*workspace=alpha .*context={"order":1}'; then
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

test_ambiguous_workspace_name() {
    local ambiguous_rows="$TMP/ambiguous-workspaces.tsv" rc
    cp "$rows" "$ambiguous_rows"
    printf 'ws-c\t%s\talpha\n' "$workspace_b" >>"$ambiguous_rows"
    set +e
    AGENT_REPL_LOGS_TEST_ROWS_OVERRIDE="$ambiguous_rows" \
        run_logs --workspace alpha --json >"$TMP/ambiguous.out" 2>"$TMP/ambiguous.err"
    rc=$?
    set -e
    if [ "$rc" -ne 0 ] && grep -q 'workspace name is ambiguous in daemon state: alpha' "$TMP/ambiguous.err"; then
        pass "an ambiguous workspace name is rejected"
    else
        fail "an ambiguous workspace name is rejected"
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

test_central_default_emacs_sink() {
    local out
    mkdir -p "$state/logs"
    printf '%s\n' '{"timestamp":"2026-09-10T10:00:16.000000Z","runtime":"emacs","pid":51,"level":"info","verbosity":"normal","operation":"emacs.durable","message":"emacs durable central","context":{}}' \
        >"$state/logs/emacs.central.log"
    out="$(AGENT_REPL_EMACS_GLOBAL_LOG_OVERRIDE= run_logs --central --runtime emacs --json)"
    rm -f "$state/logs/emacs.central.log"
    if printf '%s\n' "$out" | grep -q '"operation":"emacs.durable"'; then
        pass "--central reads the durable <state>/logs/emacs.central.log by default"
    else
        fail "--central reads the durable <state>/logs/emacs.central.log by default"
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

test_harvest_incomplete_workspace_attribution() {
    local workspace="$TMP/workspaces/incomplete" target="$targets/incomplete.log"
    local incomplete_rows="$TMP/incomplete-workspaces.tsv" rc
    mkdir -p "$workspace/.claude/emacs"
    cat >"$target" <<EOF
{"timestamp":"2026-09-10T10:06:00.000000Z","runtime":"daemon","pid":70,"level":"warn","verbosity":"normal","operation":"daemon.incomplete","message":"missing workspace ID","context":{},"workspace_dir":"$workspace"}
EOF
    ln -s "$target" "$workspace/.claude/emacs/daemon.log"
    cp "$rows" "$incomplete_rows"
    printf 'ws-incomplete\t%s\tincomplete\n' "$workspace" >>"$incomplete_rows"
    set +e
    AGENT_REPL_LOGS_TEST_ROWS_OVERRIDE="$incomplete_rows" \
        run_logs --harvest 2026-09-10T10:00:00Z 2026-09-10T10:10:00Z \
        >"$TMP/incomplete.out" 2>"$TMP/incomplete.err"
    rc=$?
    set -e
    if [ "$rc" -ne 0 ] && grep -q 'incomplete workspace attribution' "$TMP/incomplete.err"; then
        pass "harvest rejects incomplete workspace attribution"
    else
        fail "harvest rejects incomplete workspace attribution"
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

run_sink_finding_case() {
    local label="$1" shape="$2" broken_runtime="$3" mode="$4" expected_rc="$5" description="$6"
    local workspace case_rows canonical target out err
    local runtime current_target rc finding_file continuation_ok=1 attribution_ok=1 failure_ok=1
    local continuation_operation="fixture.$label.readable"
    workspace="$TMP/workspaces/$label"
    case_rows="$TMP/$label-workspaces.tsv"
    target="$targets/$label-$broken_runtime.log"
    out="$TMP/$label.out"
    err="$TMP/$label.err"

    # Arrange
    mkdir -p "$workspace/.claude/emacs"
    workspace="$(cd "$workspace" && pwd -P)"
    canonical="$workspace/.claude/emacs/$broken_runtime.log"
    for runtime in emacs daemon shim webapp sidecar; do
        current_target="$targets/$label-$runtime.log"
        if [ "$runtime" = "$broken_runtime" ]; then
            case "$shape" in
                dangling)
                    ln -s "$current_target.missing" "$workspace/.claude/emacs/$runtime.log"
                    ;;
                unreadable)
                    : >"$current_target"
                    chmod 000 "$current_target"
                    ln -s "$current_target" "$workspace/.claude/emacs/$runtime.log"
                    ;;
                directory)
                    mkdir "$workspace/.claude/emacs/$runtime.log"
                    ;;
                absent) ;;
                *) fail "unknown sink fixture shape: $shape"; return ;;
            esac
        else
            cat >"$current_target" <<EOF
{"timestamp":"2026-09-10T10:07:00.000000Z","runtime":"$runtime","pid":80,"level":"warn","verbosity":"normal","operation":"$continuation_operation","message":"readable companion","context":{},"workspace_dir":"$workspace","workspace_id":"ws-$label"}
EOF
            ln -s "$current_target" "$workspace/.claude/emacs/$runtime.log"
        fi
    done
    printf 'ws-%s\t%s\t%s\n' "$label" "$workspace" "$label" >"$case_rows"

    # Act
    set +e
    if [ "$mode" = harvest ]; then
        AGENT_REPL_LOGS_TEST_ROWS_OVERRIDE="$case_rows" \
            run_logs --harvest 2026-09-10T10:00:00Z 2026-09-10T10:10:00Z >"$out" 2>"$err"
    elif [ "$mode" = zero ]; then
        run_logs --workspace "$workspace" --runtime "$broken_runtime" --json >"$out" 2>"$err"
    else
        run_logs --workspace "$workspace" --runtime "$broken_runtime,shim" --json >"$out" 2>"$err"
    fi
    rc=$?
    set -e
    if [ "$shape" = unreadable ]; then
        chmod 0600 "$target"
    fi

    # Assert
    local finding_pattern
    case "$shape" in
        dangling) finding_pattern="sink absent: $canonical -> $target.missing" ;;
        unreadable) finding_pattern="sink unreadable: $canonical -> $target:" ;;
        directory) finding_pattern="sink not a symlink: $canonical" ;;
        absent) finding_pattern="sink absent: $canonical" ;;
    esac
    finding_file="$err"
    if [ "$mode" = harvest ]; then
        finding_file="$out"
        grep -Eq "ws-$label +$workspace +finding +$broken_runtime +logs.sink" "$out" || attribution_ok=0
    fi
    if [ "$shape" != absent ]; then
        grep -q "$continuation_operation" "$out" || continuation_ok=0
    fi
    if [ "$expected_rc" -ne 0 ]; then
        grep -q 'none of the selected log sinks could be read' "$err" || failure_ok=0
    fi
    if [ "$rc" -eq "$expected_rc" ] && grep -Fq "$finding_pattern" "$finding_file" &&
        [ "$continuation_ok" -eq 1 ] && [ "$attribution_ok" -eq 1 ] && [ "$failure_ok" -eq 1 ]; then
        pass "$description"
    else
        fail "$description"
        sed -n '1,40p' "$out" >&2
        sed -n '1,40p' "$err" >&2
    fi
}

test_sink_findings() {
    local label shape runtime mode expected_rc description
    while IFS='|' read -r label shape runtime mode expected_rc description; do
        run_sink_finding_case "$label" "$shape" "$runtime" "$mode" "$expected_rc" "$description"
    done <<'EOF'
dangling|dangling|sidecar|harvest|0|a dangling symlink is attributed while the rest is harvested
unreadable|unreadable|daemon|json|0|an unreadable target is reported while another sink is read
directory|directory|daemon|json|0|a directory at a canonical sink is reported as not a symlink
zero-readable|absent|daemon|zero|2|zero readable sinks reports the finding and exits non-zero
EOF
}

test_orphan_generations() {
    local workspace="$TMP/workspaces/orphan" targets_dir="$TMP/orphan-targets"
    local canonical_target orphan_first orphan_second rows_orphan out err
    local first_operation second_operation third_operation
    mkdir -p "$workspace/.claude/emacs" "$targets_dir"
    canonical_target="$targets_dir/agent-repl-ws-orphan-daemon-current.log"
    orphan_first="$targets_dir/agent-repl-ws-orphan-daemon-1111111111.log"
    orphan_second="$targets_dir/agent-repl-ws-orphan-daemon-2222222222.log"
    cat >"$canonical_target" <<EOF
{"timestamp":"2026-09-10T12:02:00.000000Z","runtime":"daemon","pid":90,"level":"info","verbosity":"normal","operation":"daemon.orphan.current","message":"current instance","context":{},"workspace_dir":"$workspace","workspace_id":"ws-orphan"}
EOF
    cat >"$orphan_first" <<EOF
{"timestamp":"2026-09-10T12:00:00.000000Z","runtime":"daemon","pid":91,"level":"info","verbosity":"normal","operation":"daemon.orphan.first","message":"first orphaned instance","context":{},"workspace_dir":"$workspace","workspace_id":"ws-orphan"}
EOF
    cat >"$orphan_second" <<EOF
{"timestamp":"2026-09-10T12:01:00.000000Z","runtime":"daemon","pid":92,"level":"info","verbosity":"normal","operation":"daemon.orphan.second","message":"second orphaned instance","context":{},"workspace_dir":"$workspace","workspace_id":"ws-orphan"}
EOF
    ln -s "$canonical_target" "$workspace/.claude/emacs/daemon.log"
    make_empty_workspace_sink "$workspace" orphan emacs
    make_empty_workspace_sink "$workspace" orphan shim
    make_empty_workspace_sink "$workspace" orphan webapp
    make_empty_workspace_sink "$workspace" orphan sidecar
    rows_orphan="$TMP/orphan-workspaces.tsv"
    cp "$rows" "$rows_orphan"
    printf 'ws-orphan\t%s\torphan\n' "$workspace" >>"$rows_orphan"
    out="$TMP/orphan.out"
    err="$TMP/orphan.err"
    AGENT_REPL_LOGS_TEST_ROWS_OVERRIDE="$rows_orphan" \
        run_logs --workspace ws-orphan --runtime daemon >"$out" 2>"$err"
    first_operation="$(sed -n '1p' "$out" | awk '{print $4}')"
    second_operation="$(sed -n '2p' "$out" | awk '{print $4}')"
    third_operation="$(sed -n '3p' "$out" | awk '{print $4}')"
    if [ "$first_operation" = daemon.orphan.first ] &&
        [ "$second_operation" = daemon.orphan.second ] &&
        [ "$third_operation" = daemon.orphan.current ] &&
        grep -q 'included 2 orphan generation(s)' "$err"; then
        pass "sibling unique targets an earlier daemon instance minted are merged into the workspace stream"
    else
        fail "sibling unique targets an earlier daemon instance minted are merged into the workspace stream"
        sed -n '1,40p' "$out" >&2
        sed -n '1,40p' "$err" >&2
    fi
}

test_orphan_generations_absent_when_none_minted() {
    local out err
    out="$TMP/no-orphan.out"
    err="$TMP/no-orphan.err"
    run_logs --workspace ws-a --runtime daemon >"$out" 2>"$err"
    if grep -q 'included 0 orphan generation(s)' "$err"; then
        pass "a workspace with no orphaned generations reports a zero count"
    else
        fail "a workspace with no orphaned generations reports a zero count"
        sed -n '1,40p' "$err" >&2
    fi
}

test_tally_counts_and_ordering() {
    local out
    out="$(run_logs --workspace "$workspace_a" --tally)"
    if printf '%s\n' "$out" | grep -Eq '^2\s+warn\s+daemon\s+daemon.warning$' &&
        printf '%s\n' "$out" | grep -Eq '^1\s+info\s+daemon\s+daemon.third$' &&
        [ "$(printf '%s\n' "$out" | sed -n '2p' | awk '{print $1}')" = 2 ]; then
        pass "--tally counts groups and sorts by count descending"
    else
        fail "--tally counts groups and sorts by count descending"
        printf '%s\n' "$out" >&2
    fi
}

test_sample_respects_n_and_width() {
    local out lines
    out="$(run_logs --workspace "$workspace_a" --runtime daemon --tally --sample 1 --width 10)"
    lines="$(printf '%s\n' "$out" | grep -c 'daemon.warning' || true)"
    if [ "$lines" -eq 2 ] &&
        printf '%s\n' "$out" | grep -Eq 'daemon\.warning re.*\.\.\.$'; then
        pass "--sample N caps representative records per group and --width truncates the message"
    else
        fail "--sample N caps representative records per group and --width truncates the message"
        printf '%s\n' "$out" >&2
    fi
}

test_sample_alone_without_tally() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime daemon --sample 1)"
    if ! printf '%s\n' "$out" | grep -q '^COUNT' &&
        printf '%s\n' "$out" | grep -q 'daemon.warning' &&
        printf '%s\n' "$out" | grep -q 'daemon.third'; then
        pass "--sample alone prints representative lines without a count table"
    else
        fail "--sample alone prints representative lines without a count table"
        printf '%s\n' "$out" >&2
    fi
}

test_fields_projects_only_requested_including_context() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime daemon --level warn --fields operation,order)"
    if printf '%s\n' "$out" | grep -q '^operation=daemon.warning order=1$' &&
        ! printf '%s\n' "$out" | grep -q 'message=' &&
        ! printf '%s\n' "$out" | grep -q 'level='; then
        pass "--fields projects only the named top-level and context fields"
    else
        fail "--fields projects only the named top-level and context fields"
        printf '%s\n' "$out" >&2
    fi
}

test_timeline_time_ordered() {
    local out first second third
    out="$(run_logs --workspace "$workspace_a" --runtime daemon --level info --timeline)"
    first="$(sed -n '1p' <<<"$out" | awk '{print $3}')"
    second="$(sed -n '2p' <<<"$out" | awk '{print $3}')"
    third="$(sed -n '3p' <<<"$out" | awk '{print $3}')"
    if [ "$first" = daemon.warning ] && [ "$second" = daemon.warning ] && [ "$third" = daemon.third ] &&
        ! printf '%s\n' "$out" | grep -q '^COUNT'; then
        pass "--timeline prints one time-ordered line per record"
    else
        fail "--timeline prints one time-ordered line per record"
        printf '%s\n' "$out" >&2
    fi
}

test_compact_modes_compose_with_level_and_runtime() {
    local tally_out timeline_out
    tally_out="$(run_logs --all --level error --runtime daemon,store --tally)"
    timeline_out="$(run_logs --all --level error --runtime daemon,store --timeline)"
    if printf '%s\n' "$tally_out" | grep -q 'error.*store.*store.failure' &&
        ! printf '%s\n' "$tally_out" | grep -q 'daemon.warning' &&
        printf '%s\n' "$timeline_out" | grep -q 'store.failure' &&
        ! printf '%s\n' "$timeline_out" | grep -q 'webapp.ready'; then
        pass "--tally and --timeline compose with --level and --runtime"
    else
        fail "--tally and --timeline compose with --level and --runtime"
        printf '%s\n' "$tally_out" >&2
        printf '%s\n' "$timeline_out" >&2
    fi
}

test_json_still_emits_raw_records() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime daemon --json)"
    if printf '%s\n' "$out" | grep -q '"operation":"daemon.third"' &&
        ! printf '%s\n' "$out" | grep -Eq 'COUNT|context='; then
        pass "--json is unaffected by the compact query modes and still emits raw JSONL"
    else
        fail "--json is unaffected by the compact query modes and still emits raw JSONL"
        printf '%s\n' "$out" >&2
    fi
}

test_stderr_source_in_tally() {
    local out
    out="$(run_logs --central --runtime store --tally)"
    if printf '%s\n' "$out" | grep -Eq '^1\s+error\s+store\s+stderr$' &&
        printf '%s\n' "$out" | grep -Eq '^1\s+warn\s+store\s+stderr$'; then
        pass "a stderr fixture line is inferred error or warn and appears in --tally"
    else
        fail "a stderr fixture line is inferred error or warn and appears in --tally"
        printf '%s\n' "$out" >&2
    fi
}

test_stderr_source_in_timeline() {
    local out
    out="$(run_logs --central --runtime store --timeline)"
    if printf '%s\n' "$out" | grep -q 'stderr unexpected error: disk write failed' &&
        printf '%s\n' "$out" | grep -q 'stderr heartbeat skipped this cycle'; then
        pass "a stderr fixture line appears in --timeline with a synthetic stderr operation"
    else
        fail "a stderr fixture line appears in --timeline with a synthetic stderr operation"
        printf '%s\n' "$out" >&2
    fi
}

test_stderr_source_fields_and_level() {
    local out
    out="$(run_logs --central --runtime store --level error --fields operation,level,message)"
    if printf '%s\n' "$out" | grep -q '^operation=stderr level=error message=unexpected error: disk write failed$' &&
        ! printf '%s\n' "$out" | grep -q 'heartbeat skipped'; then
        pass "--fields and --level honor a stderr source's synthesized fields"
    else
        fail "--fields and --level honor a stderr source's synthesized fields"
        printf '%s\n' "$out" >&2
    fi
}

test_messages_source_in_tally() {
    local out
    out="$(run_logs --central --runtime emacs --messages "$messages_file" --tally)"
    if printf '%s\n' "$out" | grep -Eq '^1\s+warn\s+emacs\s+messages$' &&
        printf '%s\n' "$out" | grep -Eq '^1\s+error\s+emacs\s+messages$'; then
        pass "a Messages fixture line is scraped and appears in --tally"
    else
        fail "a Messages fixture line is scraped and appears in --tally"
        printf '%s\n' "$out" >&2
    fi
}

test_messages_source_in_timeline() {
    local out
    out="$(run_logs --central --runtime emacs --messages "$messages_file" --timeline)"
    if printf '%s\n' "$out" | grep -q 'messages WARNING: the module warned about a fixture condition' &&
        printf '%s\n' "$out" | grep -q "messages Wrong type argument: stringp, nil" &&
        ! printf '%s\n' "$out" | grep -q 'ordinary echo line'; then
        pass "a Messages fixture line appears in --timeline and prose lines are skipped"
    else
        fail "a Messages fixture line appears in --timeline and prose lines are skipped"
        printf '%s\n' "$out" >&2
    fi
}

test_messages_source_fields_and_level() {
    local out
    out="$(run_logs --central --messages "$messages_file" --level error --fields operation,level,message)"
    if printf '%s\n' "$out" | grep -q '^operation=messages level=error message=Wrong type argument: stringp, nil$' &&
        ! printf '%s\n' "$out" | grep -q 'WARNING: the module warned'; then
        pass "--fields and --level honor a Messages source's synthesized fields"
    else
        fail "--fields and --level honor a Messages source's synthesized fields"
        printf '%s\n' "$out" >&2
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
        AGENT_REPL_EMACS_GLOBAL_LOG="${AGENT_REPL_EMACS_GLOBAL_LOG_OVERRIDE-$emacs_global}" \
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


test_human_names_the_workspace_and_hides_its_id() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime daemon)"
    if printf '%s\n' "$out" | grep -q 'workspace=alpha ' &&
        ! printf '%s\n' "$out" | grep -q 'workspace_id=\|workspace_dir='; then
        pass "the compact format names the workspace and hides its ID and directory"
    else
        fail "the compact format names the workspace and hides its ID and directory"
    fi
}

test_json_appends_the_workspace_name_last() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime shim --json)"
    if printf '%s\n' "$out" | grep -q '"workspace_id":"ws-a".*,"workspace_name":"alpha"}$'; then
        pass "--json appends the synthetic workspace_name as the record's last key"
    else
        fail "--json appends the synthetic workspace_name as the record's last key"
    fi
}

test_fields_projects_the_workspace_name() {
    local out
    out="$(run_logs --workspace "$workspace_a" --runtime shim --fields workspace_name,operation)"
    if printf '%s\n' "$out" | grep -q '^workspace_name=alpha operation=shim.failure$'; then
        pass "--fields projects the synthetic workspace_name"
    else
        fail "--fields projects the synthetic workspace_name"
    fi
}

test_an_unknown_workspace_keeps_its_id() {
    local only_b="$TMP/only-beta-workspaces.tsv" out
    printf 'ws-b\t%s\tbeta\n' "$workspace_b" >"$only_b"
    out="$(AGENT_REPL_LOGS_TEST_ROWS_OVERRIDE="$only_b" run_logs --workspace "$workspace_a" --runtime daemon)"
    if printf '%s\n' "$out" | grep -q 'workspace_id=ws-a' && ! printf '%s\n' "$out" | grep -q 'workspace='; then
        pass "a record whose workspace the daemon cannot name keeps its ID"
    else
        fail "a record whose workspace the daemon cannot name keeps its ID"
    fi
}

test_an_unnamed_daemon_workspace_is_refused() {
    local unnamed="$TMP/unnamed-workspaces.tsv" rc=0 err
    printf 'ws-a\t%s\t\nws-b\t%s\tbeta\n' "$workspace_a" "$workspace_b" >"$unnamed"
    err="$(AGENT_REPL_LOGS_TEST_ROWS_OVERRIDE="$unnamed" run_logs --workspace "$workspace_b" 2>&1 >/dev/null)" || rc=$?
    if [ "$rc" -ne 0 ] && printf '%s\n' "$err" | grep -q 'daemon workspace ws-a has an empty name'; then
        pass "a daemon workspace with no name is refused rather than shown by its ID"
    else
        fail "a daemon workspace with no name is refused rather than shown by its ID"
    fi
}


test_reader_refuses_a_malformed_or_repeated_workspace_name() {
    local reader rc1=0 rc2=0 err1 err2
    reader="$(ls "$TMP"/build/logs-reader-* | head -n 1)"
    err1="$("$reader" --workspace-name "ws-a" 2>&1)" || rc1=$?
    err2="$("$reader" --workspace-name "ws-a=alpha" --workspace-name "ws-a=again" 2>&1)" || rc2=$?
    if [ "$rc1" -ne 0 ] && printf '%s\n' "$err1" | grep -q 'is not ID=NAME' &&
        [ "$rc2" -ne 0 ] && printf '%s\n' "$err2" | grep -q 'names workspace "ws-a" twice'; then
        pass "the reader refuses a malformed or repeated --workspace-name"
    else
        fail "the reader refuses a malformed or repeated --workspace-name"
    fi
}

test_workspace_directory_and_default_format
test_workspace_id
test_workspace_name
test_ambiguous_workspace_name
test_central
test_central_default_emacs_sink
test_all
test_since_rfc3339
test_since_duration
test_until
test_level
test_runtime_list
test_json
test_human_names_the_workspace_and_hides_its_id
test_json_appends_the_workspace_name_last
test_fields_projects_the_workspace_name
test_an_unknown_workspace_keeps_its_id
test_an_unnamed_daemon_workspace_is_refused
test_reader_refuses_a_malformed_or_repeated_workspace_name
test_harvest
test_empty_harvest_window
test_harvest_incomplete_workspace_attribution
test_malformed_line
test_sink_findings
test_orphan_generations
test_orphan_generations_absent_when_none_minted
test_tally_counts_and_ordering
test_sample_respects_n_and_width
test_sample_alone_without_tally
test_fields_projects_only_requested_including_context
test_timeline_time_ordered
test_compact_modes_compose_with_level_and_runtime
test_json_still_emits_raw_records
test_stderr_source_in_tally
test_stderr_source_in_timeline
test_stderr_source_fields_and_level
test_messages_source_in_tally
test_messages_source_in_timeline
test_messages_source_fields_and_level
test_follow

printf '%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
