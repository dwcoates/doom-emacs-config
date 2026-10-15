#!/bin/sh
# A stand-in for agent-shim/shim-lock, speaking its protocol: it creates the
# lock file the shim names (its directory, as the real holder does), answers
# the one `locked` line, and holds until its stdin closes.
#
# IT TAKES NO KERNEL LOCK. The suite's cases that are about the lock itself,
# its contention and who owns it, run the REAL holder (SHIM_LOCK_BINARY); every
# other case only needs a session to start, and the real holder there would be
# a second agent-repl system inside the shim's integration suite (AGENTS.md,
# "A system's integration tests run that system alone").
set -e
mkdir -p "$(dirname "$1")"
: >>"$1"
echo locked
exec cat >/dev/null
