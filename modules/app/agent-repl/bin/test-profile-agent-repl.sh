#!/usr/bin/env bash
# Hermetic tests for bin/profile-agent-repl.sh and bin/profile-report.go.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
SCRIPT="$THIS_DIR/profile-agent-repl.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
TMP="$(cd "$TMP" && pwd -P)"
SAMPLERS=()
cleanup() {
    local pid
    for pid in "${SAMPLERS[@]}"; do kill "$pid" 2>/dev/null || true; done
    rm -rf "$TMP"
}
trap cleanup EXIT
PASS=0
FAIL=0

pass() {
    printf 'ok   - %s\n' "$1"
    PASS=$((PASS + 1))
}

fail() {
    printf 'FAIL - %s\n' "$1" >&2
    [ -z "${2:-}" ] || printf '       %s\n' "$2" >&2
    FAIL=$((FAIL + 1))
}

# --- stubs: ps answers from a table, kill -WINCH is recorded, emacsclient answers
STUBS="$TMP/stubs"
mkdir -p "$STUBS"
cat >"$STUBS/ps" <<'EOF'
#!/usr/bin/env bash
# ps -o comm= -p PID answers from $STUB_PS_TABLE ("PID COMM" lines);
# ps -A ... answers $STUB_PS_ALL.
if [ "$1" = "-A" ]; then cat "$STUB_PS_ALL"; exit 0; fi
pid="${@: -1}"
awk -v p="$pid" '$1 == p { print $2; found = 1 } END { exit !found }' "$STUB_PS_TABLE"
EOF
cat >"$STUBS/kill" <<'EOF'
#!/usr/bin/env bash
if [ "$1" = "-WINCH" ]; then printf 'WINCH %s\n' "$2" >>"$STUB_LOG"; exit 0; fi
exec /bin/kill "$@"
EOF
cat >"$STUBS/emacsclient" <<'EOF'
#!/usr/bin/env bash
printf 'emacsclient %s\n' "$*" >>"$STUB_LOG"
printf '"%s"\n' "${STUB_EMACS_ANSWER:-started}"
EOF
chmod +x "$STUBS"/*

# fresh_world -- a profiles root with three markers: a live daemon, a gone
# store and a pid that is some other program.
fresh_world() {
    W="$TMP/$1"
    mkdir -p "$W/root/armed"
    : >"$W/root/armed/daemon-101"
    : >"$W/root/armed/store-202"
    : >"$W/root/armed/sidecar-303"
    printf '101 /x/agent-repld\n303 /bin/zsh\n' >"$W/ps-table"
    printf '  101  51200     0:01.50 /x/agent-repld /x/agent-repld --serve\n  404  20480     1:02.00 node node /x/dist/main.js --listen /s.sock\n  505  10240     0:00.10 /bin/zsh -zsh\n' >"$W/ps-all"
    : >"$W/log"
}

collector() {
    env PATH="$STUBS:$PATH" AGENT_REPL_PROFILE_ROOT="$W/root" STUB_PS_TABLE="$W/ps-table" \
        STUB_PS_ALL="$W/ps-all" STUB_LOG="$W/log" bash "$SCRIPT" "$@"
}

remember_sampler() {
    local f
    for f in "$W"/root/*/sampler.pid; do
        [ -e "$f" ] && SAMPLERS+=("$(cat "$f")")
    done
}

# --- start ---------------------------------------------------------------------
fresh_world start
set +e
out="$(collector start --interval 1 2>&1)"
rc=$?
set -e
remember_sampler
session="$(cat "$W/root/active" 2>/dev/null || true)"
if [ "$rc" -eq 0 ] && [ -n "$session" ] && [ -f "$W/root/$session/meta.json" ] && grep -q '"interval_s":1' "$W/root/$session/meta.json"; then
    pass "start opens a session: active names it and its directory holds its meta"
else
    fail "start opens a session: active names it and its directory holds its meta" "rc=$rc out=$out"
fi
if grep -qx 'WINCH 101' "$W/log" && ! grep -q 'WINCH 202\|WINCH 303' "$W/log"; then
    pass "start signals exactly the live runtime whose process matches its marker"
else
    fail "start signals exactly the live runtime whose process matches its marker" "$(cat "$W/log")"
fi
if [ ! -e "$W/root/armed/store-202" ]; then
    pass "start removes the marker of a process that is gone"
else
    fail "start removes the marker of a process that is gone"
fi
if grep -q 'skipped sidecar-303: pid 303 is zsh, not shim-claude-sidecar' "$W/root/$session/control.log"; then
    pass "start skips a marker whose pid is another program, and says so"
else
    fail "start skips a marker whose pid is another program, and says so" "$(cat "$W/root/$session/control.log")"
fi
if grep -q 'agent-repl-profiling-reconcile' "$W/log" && grep -q '^emacs: "started"' "$W/root/$session/control.log"; then
    pass "start asks Emacs to reconcile and records its answer"
else
    fail "start asks Emacs to reconcile and records its answer" "$(cat "$W/log")"
fi
for _ in $(seq 1 100); do
    [ -s "$W/root/$session/samples.jsonl" ] && break
    sleep 0.05
done
if grep -q '"pid":101,"cpu_s":1.50,"rss_kb":51200,"name":"daemon"' "$W/root/$session/samples.jsonl" &&
    grep -q '"pid":404,"cpu_s":62.00,"rss_kb":20480,"name":"shim"' "$W/root/$session/samples.jsonl" &&
    ! grep -q '"pid":505' "$W/root/$session/samples.jsonl"; then
    pass "the sampler records every agent-repl process and nothing else"
else
    fail "the sampler records every agent-repl process and nothing else" "$(cat "$W/root/$session/samples.jsonl" 2>&1)"
fi

# --- start refusals ------------------------------------------------------------
set +e
out="$(collector start 2>&1)"
rc=$?
set -e
if [ "$rc" -ne 0 ] && grep -q 'a session is already open' <<<"$out"; then
    pass "start refuses while a session is open"
else
    fail "start refuses while a session is open" "rc=$rc out=$out"
fi

# --- status --------------------------------------------------------------------
out="$(collector status)"
if grep -q "open session: $session" <<<"$out" && grep -q 'armed: daemon-101 (agent-repld)' <<<"$out"; then
    pass "status names the open session and the armed runtimes"
else
    fail "status names the open session and the armed runtimes" "$out"
fi

# --- stop ----------------------------------------------------------------------
: >"$W/log"
sampler_pid="$(cat "$W/root/$session/sampler.pid")"
out="$(collector stop 2>&1)"
if [ ! -e "$W/root/active" ] && grep -qx 'WINCH 101' "$W/log" && [ -f "$W/root/$session/stopped.json" ]; then
    pass "stop closes the session, signals again and records the stop"
else
    fail "stop closes the session, signals again and records the stop" "$out"
fi
for _ in $(seq 1 100); do
    kill -0 "$sampler_pid" 2>/dev/null || break
    sleep 0.05
done
if ! kill -0 "$sampler_pid" 2>/dev/null; then
    pass "stop ends the sampler"
else
    fail "stop ends the sampler" "pid $sampler_pid is still running"
fi

set +e
out="$(collector stop 2>&1)"
rc=$?
set -e
if [ "$rc" -ne 0 ] && grep -q 'no session is open' <<<"$out"; then
    pass "stop with no session open is a loud refusal"
else
    fail "stop with no session open is a loud refusal" "rc=$rc out=$out"
fi

fresh_world bad-interval
set +e
out="$(collector start --interval 0 2>&1)"
rc=$?
set -e
if [ "$rc" -ne 0 ] && [ ! -e "$W/root/active" ]; then
    pass "start refuses an interval that is not a whole positive number of seconds"
else
    fail "start refuses an interval that is not a whole positive number of seconds" "rc=$rc out=$out"
fi

# --- the report ----------------------------------------------------------------
R="$TMP/report"
mkdir -p "$R/sess"
cat >"$R/sess/samples.jsonl" <<'EOF'
{"t":100,"pid":101,"cpu_s":10.00,"rss_kb":102400,"name":"daemon"}
{"t":100,"pid":404,"cpu_s":1.00,"rss_kb":20480,"name":"shim"}
{"t":102,"pid":101,"cpu_s":13.50,"rss_kb":204800,"name":"daemon"}
{"t":102,"pid":404,"cpu_s":1.25,"rss_kb":20480,"name":"shim"}
{"t":102,"pid":606,"cpu_s":0.75,"rss_kb":10240,"name":"shim"}
EOF
cat >"$R/sess/shim-404.cpuprofile" <<'EOF'
{"nodes":[{"callFrame":{"functionName":"hot","url":"file:///x/main.js","lineNumber":9},"hitCount":30},{"callFrame":{"functionName":"cold","url":"file:///x/main.js","lineNumber":19},"hitCount":10}]}
EOF
cat >"$R/sess/shim-404.heapprofile" <<'EOF'
{"head":{"callFrame":{"functionName":"(root)","url":"","lineNumber":-1},"selfSize":0,"children":[{"callFrame":{"functionName":"alloc","url":"file:///x/main.js","lineNumber":4},"selfSize":4096,"children":[]}]}}
EOF
printf 'reversed tree line 1\nline 2\n' >"$R/sess/emacs-9.cpu.txt"
printf '{"session":"s","ws":"a","ok":true,"interval_ms":50,"ticks":10,"wall_ms":5000,"stalled_ms":240,"longest_ms":180,"stalls":[[1000,60],[3000,180]]}' >"$R/sess/webapp-a.lag.json"
cat >"$R/logs.jsonl" <<'EOF'
{"timestamp":"2026-10-12T12:00:00.000-04:00","runtime":"daemon","operation":"daemon.cmd.boot","message":"boot","workspace_id":""}
{"timestamp":"2026-10-12T12:00:01.000-04:00","runtime":"emacs","operation":"x","message":"elisp.roster.tab-open: ws=a id=A dir=/a","workspace_id":"A"}
{"timestamp":"2026-10-12T12:00:01.200-04:00","runtime":"daemon","operation":"daemon.workspace.bring_up","message":"the session is up","workspace_id":"A"}
{"timestamp":"2026-10-12T12:00:02.000-04:00","runtime":"webapp","operation":"main.boot","message":"the webapp booted","workspace_id":"A","context":{"connection_id":"c1"}}
{"timestamp":"2026-10-12T12:00:02.450-04:00","runtime":"webapp","operation":"feed.page-placed","message":"placed","workspace_id":"A","context":{"connection_id":"c1"}}
{"timestamp":"2026-10-12T12:00:10.000-04:00","runtime":"emacs","operation":"x","message":"elisp.host.editor-switch ws=b origin=slot count=1","workspace_id":"B"}
{"timestamp":"2026-10-12T12:00:10.080-04:00","runtime":"emacs","operation":"x","message":"elisp.panels.webview-on-switch: ws=b command=nil from-window=nil window=w windows=1 size=1x1 xwidgets=1 view=first selected=nil","workspace_id":"B"}
{"timestamp":"2026-10-12T12:00:11.300-04:00","runtime":"webapp","operation":"feed.page-placed","message":"placed","workspace_id":"B","context":{"connection_id":"c2"}}
{"timestamp":"2026-10-12T12:00:20.000-04:00","runtime":"emacs","operation":"x","message":"elisp.host.editor-switch ws=a origin=cycle count=2","workspace_id":"A"}
{"timestamp":"2026-10-12T12:00:20.025-04:00","runtime":"emacs","operation":"x","message":"elisp.panels.switch-activation-won ws=a generation=2 waited-ms=25","workspace_id":"A"}
{"timestamp":"2026-10-12T12:00:20.040-04:00","runtime":"emacs","operation":"x","message":"elisp.panels.webview-on-switch: ws=a command=nil from-window=nil window=w windows=1 size=1x1 xwidgets=1 view=same selected=nil","workspace_id":"A"}
{"timestamp":"2026-10-12T12:00:20.070-04:00","runtime":"webapp","operation":"page.switched-to","message":"switched","workspace_id":"A","context":{"connection_id":"c1"}}
{"timestamp":"2026-10-12T12:00:20.130-04:00","runtime":"webapp","operation":"page.shown-frame","message":"frame","workspace_id":"A","context":{"connection_id":"c1"}}
{"timestamp":"2026-10-12T12:00:25.000-04:00","runtime":"emacs","operation":"x","message":"elisp.host.editor-switch ws=a origin=cycle count=3","workspace_id":"A"}
{"timestamp":"2026-10-12T12:00:25.040-04:00","runtime":"emacs","operation":"x","message":"elisp.panels.webview-on-switch: ws=a command=nil from-window=nil window=w windows=1 size=1x1 xwidgets=1 view=same selected=nil","workspace_id":"A"}
{"timestamp":"2026-10-12T12:00:30.000-04:00","runtime":"emacs","operation":"x","message":"elisp.input.submit ws=a origin=nil key=k blocks=1 selection=nil delivery=nil","workspace_id":"A"}
{"timestamp":"2026-10-12T12:00:30.250-04:00","runtime":"webapp","operation":"feed.draw-user-prompt","message":"drew","workspace_id":"A","context":{"connection_id":"c1"}}
EOF
HELPER="$TMP/profile-report"
go build -o "$HELPER" "$THIS_DIR/profile-report.go"
if out="$(cd "$THIS_DIR" && go test profile-report.go profile-report_test.go 2>&1)"; then
    pass "the report's Go unit tests pass"
else
    fail "the report's Go unit tests pass" "$out"
fi
report="$("$HELPER" --session "$R/sess" --logs "$R/logs.jsonl" --startup 2026-10-12T15:59:59Z)"

if grep -q '| daemon | 101 | 3.50 | 150 | 200 | 2 |' <<<"$report"; then
    pass "the report gives a process its CPU-seconds and its average and peak RSS"
else
    fail "the report gives a process its CPU-seconds and its average and peak RSS" "$report"
fi
if grep -q '| shim | 606 | 0.75 |' <<<"$report" && grep -q '| shim | 1.00 |' <<<"$report"; then
    pass "a process born during the session is charged all of its CPU, and a runtime's processes sum"
else
    fail "a process born during the session is charged all of its CPU, and a runtime's processes sum" "$report"
fi
if grep -q '75.0%  30  hot main.js:10' <<<"$report" && grep -q '100.0%  4096  alloc main.js:5' <<<"$report"; then
    pass "the report ranks the shim's CPU and heap frames"
else
    fail "the report ranks the shim's CPU and heap frames" "$report"
fi
if grep -q 'main thread stalled 240ms of 5000ms sampled (2 stalls past one frame), longest 180ms' <<<"$report" &&
    grep -q '+3000ms: stalled 180ms' <<<"$report" && ! grep -q '+1000ms: stalled' <<<"$report"; then
    pass "the report summarizes a page's main-thread stalls, listing those past 100ms"
else
    fail "the report summarizes a page's main-thread stalls, listing those past 100ms" "$report"
fi
if grep -q 'reversed tree line 1' <<<"$report"; then
    pass "the report carries Emacs's reversed call tree"
else
    fail "the report carries Emacs's reversed call tree" "$report"
fi
if grep -q '| first switch to a never-shown workspace (keystroke to page painted) | 100ms | 1 | 1300ms | 1300ms | 1300ms | 13.0x |' <<<"$report"; then
    pass "a first switch is measured from the keystroke to the page painted"
else
    fail "a first switch is measured from the keystroke to the page painted" "$report"
fi
if grep -q "| switch to a shown workspace (keystroke to the page's first shown frame) | 100ms | 1 | 130ms |" <<<"$report"; then
    pass "a repeat switch is measured from the keystroke to the page's first shown frame"
else
    fail "a repeat switch is measured from the keystroke to the page's first shown frame" "$report"
fi
if grep -q '| Emacs, before its switch pass runs (its redisplay and hooks) | 1 | 25ms |' <<<"$report" &&
    grep -q "| Emacs's switch pass, to its view shown | 1 | 15ms |" <<<"$report" &&
    grep -q '| the webhost, to the page told it is shown | 1 | 30ms |' <<<"$report" &&
    grep -q '| the page, to its first shown frame | 1 | 60ms |' <<<"$report"; then
    pass "a repeat switch is split into its Emacs, webhost and page phases"
else
    fail "a repeat switch is split into its Emacs, webhost and page phases" "$report"
fi
if grep -q '1 switches left out: an edge of theirs is missing from the logs.' <<<"$report"; then
    pass "a repeat switch missing an edge is left out of the phases, and counted"
else
    fail "a repeat switch missing an edge is left out of the phases, and counted" "$report"
fi
if grep -q '| page boot to first feed paint | 300ms | 1 | 450ms |' <<<"$report" &&
    grep -q '| workspace open (tab opened to session up) | 300ms | 1 | 200ms |' <<<"$report" &&
    grep -q '| prompt echo (submit to the prompt drawn) | 100ms | 1 | 250ms |' <<<"$report"; then
    pass "page boot, workspace open and prompt echo are each paired off their own edges"
else
    fail "page boot, workspace open and prompt echo are each paired off their own edges" "$report"
fi
first_row="$(grep -n 'first switch to a never-shown' <<<"$report" | sed -n 1p | cut -d: -f1)"
echo_row="$(grep -n '| prompt echo' <<<"$report" | sed -n 1p | cut -d: -f1)"
if [ -n "$first_row" ] && [ -n "$echo_row" ] && [ "$first_row" -lt "$echo_row" ]; then
    pass "the latency table puts the interaction furthest over budget first"
else
    fail "the latency table puts the interaction furthest over budget first" "$report"
fi
if grep -q 'Not exercised in this session: prompt to first response text (vendor-bound).' <<<"$report"; then
    pass "an interaction with no edges in the session is named as not exercised"
else
    fail "an interaction with no edges in the session is named as not exercised" "$report"
fi
if grep -q 'Daemon.s first record: +1000ms' <<<"$report" && grep -q 'Every workspace loaded: +12300ms after launch.' <<<"$report" &&
    grep -q '| first | B | 1300ms |' <<<"$report" && grep -q '| repeat | A | 130ms |' <<<"$report"; then
    pass "the startup assessment runs from launch to every workspace loaded, then every switch"
else
    fail "the startup assessment runs from launch to every workspace loaded, then every switch" "$report"
fi

set +e
bad="$("$HELPER" --session "$R/sess" --logs <(printf 'not json\n') 2>&1)"
rc=$?
set -e
if [ "$rc" -ne 0 ] && grep -q 'logs line 1' <<<"$bad"; then
    pass "a malformed log line fails the report, naming the line"
else
    fail "a malformed log line fails the report, naming the line" "rc=$rc out=$bad"
fi

# --- report through the collector ----------------------------------------------
W="$TMP/collector-report"
mkdir -p "$W/root/20261012T160000Z"
cp "$R/sess/samples.jsonl" "$W/root/20261012T160000Z/"
printf '{"started_at":"2026-10-12T16:00:00Z","interval_s":2}\n' >"$W/root/20261012T160000Z/meta.json"
cat >"$W/logs.sh" <<EOF
#!/usr/bin/env bash
printf '%s\n' "\$*" >"$W/logs-args"
cat "$R/logs.jsonl"
EOF
chmod +x "$W/logs.sh"
out="$(env AGENT_REPL_PROFILE_ROOT="$W/root" AGENT_REPL_PROFILE_LOGS_SH="$W/logs.sh" AGENT_REPL_PROFILE_BUILD_DIR="$TMP/build" bash "$SCRIPT" report)"
if grep -q '# Profile session 20261012T160000Z' <<<"$out" && [ -f "$W/root/20261012T160000Z/summary.md" ] &&
    grep -qx -- '--all --json --level info --since 2026-10-12T16:00:00Z' "$W/logs-args"; then
    pass "report reads the newest session's window at info and keeps the summary beside it"
else
    fail "report reads the newest session's window at info and keeps the summary beside it" "$out $(cat "$W/logs-args" 2>&1)"
fi

printf '\ntest-profile-agent-repl.sh: %d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
