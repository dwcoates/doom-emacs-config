#!/usr/bin/env bash
# Hermetic fixture tests for bin/agent-repl-runtime.
#
# NOTHING REAL RUNS. Every implementation a verb dispatches to, emacsclient and
# `open` are stubs on the AGENT_REPL_RUNTIME_* environment that append what
# they were asked to do to one transcript, so a test reads the exact order of
# the hard bounce's steps. The emacsclient stub keeps "Emacs is running" as a
# file, so a `(kill-emacs)` makes it stop answering exactly as a real quit does.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
RUNTIME="$THIS_DIR/agent-repl-runtime"
# The hard bounce asks the running Emacs to hot-load its stale elisp with this form.
MODULE="$(cd "$THIS_DIR/.." && pwd -P)"
RELOAD="emacsclient (progn (unless (fboundp (quote agent-repl-elisp-reload-if-stale)) (load \"$MODULE/lisp/elisp-build.el\" nil t)) (agent-repl-elisp-reload-if-stale \"$MODULE\"))"
TMP="$(cd "$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")" && pwd -P)"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0

pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; FAIL=$((FAIL + 1)); }

TRANSCRIPT="$TMP/transcript"
STATE="$TMP/state"
mkdir -p "$STATE"

# An implementation stub: records its name and arguments, exits STUB_EXIT_<NAME>.
make_impl() {
    local name="$1"
    cat >"$TMP/$name" <<EOF
#!/usr/bin/env bash
printf '%s\n' "$name\${*:+ \$*}" >>"$TRANSCRIPT"
code_var="STUB_EXIT_${name//-/_}"
exit "\${!code_var:-0}"
EOF
    chmod +x "$TMP/$name"
}
for impl in build byte-compile bounce doctor readiness logs claude-repld open; do
    make_impl "$impl"
done

cat >"$TMP/pgrep" <<EOF
#!/usr/bin/env bash
# pgrep stub: Emacs's process is up exactly when STUB_EMACS_PROCESS is set.
[ -n "\${STUB_EMACS_PROCESS:-}" ]
EOF
chmod +x "$TMP/pgrep"

cat >"$TMP/emacsclient" <<EOF
#!/usr/bin/env bash
# emacsclient stub: answers only while \$STATE/running exists.
[ -f "$STATE/running" ] || exit 1
form="\${2:-}"
case "\$form" in
    t) exit 0 ;;
    "(kill-emacs)")
        printf 'emacsclient kill-emacs\n' >>"$TRANSCRIPT"
        [ -n "\${STUB_NEVER_EXITS:-}" ] || rm -f "$STATE/running"
        exit 0 ;;
    *mapconcat*) printf '"%s"\n' "\${STUB_UNSAVED:-}" ;;
    "(load "*) printf 'emacsclient %s\n' "\$form" >>"$TRANSCRIPT" ;;
    *reload-if-stale*)
        printf 'emacsclient %s\n' "\$form" >>"$TRANSCRIPT"
        printf '"%s"\n' "\${STUB_RELOAD:-current}" ;;
esac
EOF
chmod +x "$TMP/emacsclient"

export AGENT_REPL_RUNTIME_BUILD="$TMP/build"
export AGENT_REPL_RUNTIME_BYTE_COMPILE="$TMP/byte-compile"
export AGENT_REPL_RUNTIME_BOUNCE="$TMP/bounce"
export AGENT_REPL_RUNTIME_DOCTOR="$TMP/doctor"
export AGENT_REPL_RUNTIME_READINESS="$TMP/readiness"
export AGENT_REPL_RUNTIME_LOGS="$TMP/logs"
export AGENT_REPL_RUNTIME_CLAUDE_REPLD="$TMP/claude-repld"
export AGENT_REPL_RUNTIME_EMACSCLIENT="$TMP/emacsclient"
export AGENT_REPL_RUNTIME_OPEN="$TMP/open"
export AGENT_REPL_RUNTIME_EMACS_APP="/Applications/Emacs.app"
export AGENT_REPL_RUNTIME_QUIT_WAIT=2
export AGENT_REPL_RUNTIME_LAUNCH_ATTEMPTS=2
export AGENT_REPL_RUNTIME_PGREP="$TMP/pgrep"

# reset puts the fixture back to "Emacs is running, nothing has run yet".
reset() {
    : >"$TRANSCRIPT"
    touch "$STATE/running"
    unset STUB_RELOAD STUB_UNSAVED STUB_NEVER_EXITS STUB_EXIT_byte_compile STUB_EXIT_bounce STUB_EXIT_open STUB_EMACS_PROCESS
}

# expect_transcript NAME EXPECTED compares the whole transcript.
expect_transcript() {
    local got
    got="$(cat "$TRANSCRIPT")"
    if [ "$got" = "$2" ]; then pass "$1"; else fail "$1 (got: $(printf '%s' "$got" | tr '\n' '|'))"; fi
}

echo "agent-repl-runtime: pass-through verbs"
for pair in "build:build --force daemon" "byte-compile:byte-compile" "doctor:doctor --json" \
            "readiness:readiness" "logs:logs --tally" "call:claude-repld call DaemonHealth"; do
    reset
    verb="${pair%%:*}"
    want="${pair#*:}"
    # shellcheck disable=SC2086
    args=""
    [[ "$want" == *" "* ]] && args="${want#* }"
    [ "$verb" = "call" ] && args="DaemonHealth"
    # shellcheck disable=SC2086
    "$RUNTIME" "$verb" $args >/dev/null 2>&1
    expect_transcript "$verb dispatches to its implementation with its arguments" "$want"
done

echo "agent-repl-runtime: bounce"
reset
"$RUNTIME" bounce >/dev/null 2>&1
expect_transcript "bounce runs the bounce and touches no Emacs" "bounce"

reset
if "$RUNTIME" bounce --soft >/dev/null 2>&1; then fail "bounce refuses an unknown argument"; else pass "bounce refuses an unknown argument"; fi

echo "agent-repl-runtime: bounce --hard"
reset
"$RUNTIME" bounce --hard >/dev/null 2>&1
expect_transcript "a hard bounce byte-compiles, hot-loads stale elisp, bounces, quits Emacs and relaunches it, in order" \
"byte-compile
$RELOAD
bounce
emacsclient kill-emacs
open -a /Applications/Emacs.app"

reset
export STUB_RELOAD=failed
if "$RUNTIME" bounce --hard >/dev/null 2>&1; then fail "a failed elisp hot load fails the hard bounce"; else pass "a failed elisp hot load fails the hard bounce"; fi
expect_transcript "a failed elisp hot load stops before the bounce" "byte-compile
$RELOAD"

reset
export STUB_RELOAD=reloaded
"$RUNTIME" bounce --hard >/dev/null 2>&1
expect_transcript "a hot-loaded Emacs goes on to the bounce" "byte-compile
$RELOAD
bounce
emacsclient kill-emacs
open -a /Applications/Emacs.app"

reset
export STUB_RELOAD=other-root
"$RUNTIME" bounce --hard >/dev/null 2>&1
expect_transcript "an Emacs on another checkout's elisp is bounced as it is" "byte-compile
$RELOAD
bounce
emacsclient kill-emacs
open -a /Applications/Emacs.app"

reset
export STUB_UNSAVED="notes.org"
if "$RUNTIME" bounce --hard >/dev/null 2>&1; then fail "a hard bounce refuses while Emacs holds unsaved files"; else pass "a hard bounce refuses while Emacs holds unsaved files"; fi
expect_transcript "a refused hard bounce runs nothing" ""

reset
export STUB_EXIT_byte_compile=1
if "$RUNTIME" bounce --hard >/dev/null 2>&1; then fail "a byte-compile failure fails the hard bounce"; else pass "a byte-compile failure fails the hard bounce"; fi
expect_transcript "a byte-compile failure stops before the bounce" "byte-compile"

reset
export STUB_EXIT_bounce=1
if "$RUNTIME" bounce --hard >/dev/null 2>&1; then fail "a bounce failure fails the hard bounce"; else pass "a bounce failure fails the hard bounce"; fi
expect_transcript "a bounce failure leaves Emacs running" "byte-compile
$RELOAD
bounce"

reset
rm -f "$STATE/running"
"$RUNTIME" bounce --hard >/dev/null 2>&1
expect_transcript "with no Emacs running, a hard bounce only launches it" "byte-compile
bounce
open -a /Applications/Emacs.app"

reset
export STUB_NEVER_EXITS=1
if "$RUNTIME" bounce --hard >/dev/null 2>&1; then fail "an Emacs that will not exit fails the hard bounce"; else pass "an Emacs that will not exit fails the hard bounce"; fi
expect_transcript "an Emacs that will not exit is not relaunched beside itself" "byte-compile
$RELOAD
bounce
emacsclient kill-emacs"

reset
export STUB_EXIT_open=1 STUB_EMACS_PROCESS=1
if "$RUNTIME" bounce --hard >/dev/null 2>&1; then pass "a launch the launcher misreports, with Emacs up, is a launch"; else fail "a launch the launcher misreports, with Emacs up, is a launch"; fi
expect_transcript "a misreported launch is not retried" "byte-compile
$RELOAD
bounce
emacsclient kill-emacs
open -a /Applications/Emacs.app"

reset
export STUB_EXIT_open=1
if "$RUNTIME" bounce --hard >/dev/null 2>&1; then fail "a launch that never brings Emacs up fails the hard bounce"; else pass "a launch that never brings Emacs up fails the hard bounce"; fi
expect_transcript "a failed launch is retried up to its bound" "byte-compile
$RELOAD
bounce
emacsclient kill-emacs
open -a /Applications/Emacs.app
open -a /Applications/Emacs.app"

echo "agent-repl-runtime: deploy and hot-reload"
reset
"$RUNTIME" deploy -force >/dev/null 2>&1
expect_transcript "deploy dispatches to the daemon's deploy verb" "claude-repld deploy -force"

reset
mkdir -p "$TMP/lisp"
: >"$TMP/lisp/panels.el"
"$RUNTIME" hot-reload "$TMP/lisp/panels.el" >/dev/null 2>&1
expect_transcript "hot-reload loads the file into the running Emacs" "emacsclient (load \"$TMP/lisp/panels.el\" nil t)"

reset
: >"$TMP/lisp/test-panels.el"
if "$RUNTIME" hot-reload "$TMP/lisp/panels.el" "$TMP/lisp/test-panels.el" >/dev/null 2>&1; then
    fail "hot-reload refuses a test file"
else
    pass "hot-reload refuses a test file"
fi
expect_transcript "a refused hot-reload loads nothing, not even the files before the test file" ""

reset
if "$RUNTIME" hot-reload "$TMP/lisp/missing.el" >/dev/null 2>&1; then fail "hot-reload refuses a missing file"; else pass "hot-reload refuses a missing file"; fi

reset
rm -f "$STATE/running"
if "$RUNTIME" hot-reload "$TMP/lisp/panels.el" >/dev/null 2>&1; then fail "hot-reload refuses with no running Emacs"; else pass "hot-reload refuses with no running Emacs"; fi

echo "agent-repl-runtime: refusals"
reset
if "$RUNTIME" no-such-verb >/dev/null 2>&1; then fail "an unknown verb is refused"; else pass "an unknown verb is refused"; fi

reset
if AGENT_REPL_RUNTIME_CLAUDE_REPLD="$TMP/missing" "$RUNTIME" call DaemonHealth >/dev/null 2>&1; then
    fail "call without a daemon binary is refused"
else
    pass "call without a daemon binary is refused"
fi

reset
if "$RUNTIME" help | grep -q 'bounce --hard'; then pass "help lists the verbs"; else fail "help lists the verbs"; fi

echo "agent-repl-runtime: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
