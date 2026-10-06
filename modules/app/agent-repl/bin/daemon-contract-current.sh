#!/usr/bin/env bash
# daemon-contract-current.sh -- is the RUNNING daemon built from the checkout's
# wire contract?
#
# The hard bounce hot-loads the checkout's elisp into the outgoing Emacs before
# the stand-down (bin/agent-repl-runtime). That is safe only while the daemon
# serving that Emacs speaks the same contract: a decoder newer than the daemon
# refuses the daemon's last pushes as contract breaches (observed 2026-10-06: a
# newly required oneof, RosterMergedSection.fold, refused twice in the stand-down
# window). This answers whether `proto/src` is unchanged between the commit the
# running daemon was built from and HEAD.
#
#   exit 0  current: the running daemon's contract is the checkout's
#   exit 1  changed: proto/src differs between the two
#   exit 2  unknown: no running daemon, a stale running binary, or an
#           unreadable build stamp -- the caller treats it as changed
#
# Honored environment: AGENT_REPL_RUNTIME_READINESS (the readiness report).
set -uo pipefail

MODULE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
READINESS="${AGENT_REPL_RUNTIME_READINESS:-$MODULE_DIR/bin/readiness-report.sh}"

report="$("$READINESS" 2>/dev/null)" || { echo "unknown: the readiness report failed"; exit 2; }
sha="$(printf '%s' "$report" | python3 -c '
import json, sys
try:
    doc = json.load(sys.stdin)
except ValueError:
    sys.exit(0)
items = doc if isinstance(doc, list) else doc.get("components", doc.get("systems", []))
for c in items:
    if c.get("name") != "daemon":
        continue
    running = c.get("running") or {}
    if not running or running.get("stale_binary"):
        break
    print(str(c.get("deployed_sha", "")).removesuffix("-dirty"))
')"
if [[ ! $sha =~ ^[0-9a-f]{40}$ ]]; then
    echo "unknown: no running daemon on a current binary with a readable build stamp"
    exit 2
fi
git -C "$MODULE_DIR" cat-file -e "$sha^{commit}" 2>/dev/null || { echo "unknown: build commit $sha is not in this checkout"; exit 2; }
if git -C "$MODULE_DIR" diff --quiet "$sha" HEAD -- proto/src; then
    echo "current: the running daemon speaks the checkout's contract ($sha)"
    exit 0
fi
echo "changed: proto/src moved since the running daemon's build ($sha)"
exit 1
