#!/usr/bin/env bash
#
# ensure-e2e-deps.sh — make every npm package the unsandboxed e2e suite
# consumes satisfy its lockfile, before the suite runs.
#
# Usage: ensure-e2e-deps.sh
#
# The e2e suite bundles the REAL shim from source (e2e/main_test.go's
# requireShimBundle) and drives the REAL webapp (e2e/webapplayer_e2e_test.go),
# and both fail loudly when their package's node_modules is absent. Those
# trees used to appear only as a side effect of an EARLIER suite: the `shim`
# and `webapp` suites' npm `pre*` hooks run ensure-deps.sh. A run that
# selected `e2e` without them (`test-all.sh --suites daemon,e2e`, a fresh
# merge tree) therefore failed on a missing node_modules. The e2e runners
# (test-e2e.sh, e2e-coverage.sh) call this instead, so the suite provisions
# its own dependencies whatever else runs.
#
# Every install goes through ensure-deps.sh, the one path that never installs
# through a shared-store symlink.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"

# The npm packages the e2e suite consumes, relative to modules/app/agent-repl.
E2E_NPM_PACKAGES=(
    agent-shim/claude/shim
    webapp
)

for package in "${E2E_NPM_PACKAGES[@]}"; do
    status=0
    "$THIS_DIR/ensure-deps.sh" "$ROOT/$package" || status=$?
    if [ "$status" -ne 0 ]; then
        printf '[e2e-deps] ERROR: ensuring the npm deps of %s (%s) failed with exit %d; the e2e suite cannot run without them\n' \
            "$package" "$ROOT/$package" "$status" >&2
        exit "$status"
    fi
done
