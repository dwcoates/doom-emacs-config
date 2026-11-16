#!/usr/bin/env bash

# lib-stat.sh — the ONE reading of a file's stat fields across platforms.
#
# BSD stat (macOS) and GNU stat (Linux) spell their formats differently, and a
# script that tried one spelling and fell back to the other answered garbage on
# Linux: GNU stat reads BSD's `-f' as "the file system", prints that file
# system's record, and only then fails, so both stats' output came back at once
# (the doctor's size gate, 2026-11-14). The spelling is chosen by platform,
# never tried in turn. test-lib-stat.sh fails a script that calls `stat -f' or
# `stat -c' itself.
#
# Each function answers stat's own status: a caller decides whether a failure
# is its own error to report.

# stat_field BSD-FORMAT GNU-FORMAT FILE: one field of FILE's stat, asked in the
# platform's own spelling (AGENT_REPL_STAT_PLATFORM names a platform for a
# hermetic harness).
stat_field() {
    case "${AGENT_REPL_STAT_PLATFORM:-$(uname -s)}" in
        Darwin|darwin) stat -f "$1" "$3" ;;
        *) stat -c "$2" "$3" ;;
    esac
}

# stat_mtime FILE: FILE's modification time in epoch seconds.
stat_mtime() { stat_field %m %Y "$1"; }

# stat_size FILE: FILE's size in bytes.
stat_size() { stat_field %z %s "$1"; }
