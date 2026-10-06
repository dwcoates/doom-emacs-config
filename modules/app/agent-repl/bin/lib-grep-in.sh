#!/usr/bin/env bash

# grep_in: the ONE way a script here asks grep about text it already holds.
#
# grep_in TEXT GREP-ARGS... greps TEXT fed from a here-string, NEVER a pipe.
# Under pipefail, `printf '%s\n' "$out" | grep -q PATTERN` fails whenever grep
# matches and exits before the writer has written everything: the writer dies
# of SIGPIPE, the pipeline answers 141, and a check whose output was correct
# fails. It needs the writer descheduled mid-write, so it flaked only under
# load (2026-10-06: the logs harness failed 10 of 24 runs six-wide, across
# nine cases). The same holds for any command piped into an early-exiting
# grep, so a command's output is captured first: grep_in "$(cmd)" -q PATTERN.
#
# testrun/internal/run/grep_pipe_scan_test.go fails any scanned script that
# pipes into `grep -q` again.
grep_in() {
    local text="$1"
    shift
    grep "$@" <<<"$text"
}
