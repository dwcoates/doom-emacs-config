#!/usr/bin/env bash
#
# playtest.sh — drive the real application headlessly and photograph it.
#
# Runs the `playtest`-tagged playbooks in `e2e/` inside the sandbox: a real
# Doom, a daemon Emacs spawns through its own launcher, the real shim, store
# and sidecar, the real webapp inside a real WebKit view — and the FAKE SDK
# as the only vendor, so no real Claude call can occur. Each playbook takes a
# picture after every user act and writes a MANIFEST.md saying what each
# picture must show. See e2e/PLAYTEST-SPEC.md.
#
# THE OUTPUT IS FOR A HUMAN, and that is why this is not part of any gate.
# The run is green when every capture exists, is the declared geometry and is
# not blank; whether the pictures show the RIGHT thing is read off the
# pictures against the manifest.
#
#   bin/playtest.sh                       # every playbook
#   bin/playtest.sh -run TestPlaytestCold # one, by name
#   AGENT_REPL_PLAYTEST_OUT=<dir> bin/playtest.sh
#
#   exit 0   the playbooks ran and every capture held
#   exit 77  DECLINED: the sandbox is unavailable, and the preflight's own
#            message says why
#   other    a playbook failed
#
# 77 is the autotools "skipped" convention, used rather than 0 so a run can
# never report a green playtest that did not execute.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="$(cd "$THIS_DIR/.." && pwd)"
SANDBOX="$MODULE_ROOT/e2e/sandbox/bin/e2e-sandbox.sh"

readonly EXIT_DECLINED=77

if [ ! -x "$SANDBOX" ]; then
    printf '[playtest] DECLINED: the sandbox runner is missing or not executable: %s\n' "$SANDBOX" >&2
    exit "$EXIT_DECLINED"
fi

# The preflight's output is the actionable part (start Docker, build the
# image), so it is reproduced VERBATIM rather than paraphrased — the same
# rule every other sandboxed entry point here follows.
if ! preflight_out="$("$SANDBOX" preflight 2>&1)"; then
    printf '[playtest] DECLINED: the e2e sandbox is not usable, and a playtest never runs unsandboxed.\n' >&2
    printf '%s\n' "$preflight_out" >&2
    exit "$EXIT_DECLINED"
fi
printf '%s\n' "$preflight_out"

# WHERE THE PICTURES LAND. The sandbox mounts this one host directory
# read-write into the container and exports AGENT_REPL_E2E_ARTIFACTS inside
# it; the playbooks write under `playtest/<playbook>/` there. It is the ONLY
# host path a run may write, and a playbook whose pictures have nowhere to go
# FAILS rather than passing quietly.
OUT="${AGENT_REPL_PLAYTEST_OUT:-$MODULE_ROOT/e2e/.playtest-out}"
mkdir -p "$OUT"

# THE SWEEP IS EACH PLAYBOOK'S OWN, and deliberately not this script's. A
# stale picture is indistinguishable from a fresh one, which is the one way a
# visual review reaches a confident wrong answer — but a sweep here would
# also delete every playbook a `-run` of ONE playbook does not re-take, and
# re-running one playbook to look again at its pictures is the ordinary way
# this is used. Each playbook removes its own directory as it claims it.

printf '[playtest] pictures will land in %s/playtest\n' "$OUT"
printf '[playtest] the vendor is the FAKE SDK only; no real Claude call can occur\n'

set +e
AGENT_REPL_E2E_ARTIFACTS="$OUT" "$SANDBOX" run --dir e2e \
    go test -tags playtest ./ -run 'TestPlaytest' -count=1 -v "$@"
status=$?
set -e

printf '\n[playtest] pictures and manifests:\n'
if [ -d "$OUT/playtest" ]; then
    for book in "$OUT"/playtest/*/; do
        [ -d "$book" ] || continue
        printf '  %s  (%d captures)\n' "$book" "$(find "$book" -name '*.png' | wc -l | tr -d ' ')"
    done
    printf '\n[playtest] read each MANIFEST.md beside its pictures: it says, per step, what the\n'
    printf '[playtest] image must show. A green run means the captures EXIST and are not blank;\n'
    printf '[playtest] whether they show the right thing is the review.\n'
else
    printf '  none — no playbook reached its first capture\n'
fi

exit "$status"
