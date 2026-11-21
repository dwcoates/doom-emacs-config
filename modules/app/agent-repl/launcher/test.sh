#!/usr/bin/env bash
# The launcher's unit tests (Tests/ZettelLauncherTests), run by the
# `launcher` suite. SwiftPM builds the package (fetching the pinned
# swift-protobuf on a cold cache) and runs every XCTest case; a failing case
# fails the suite with XCTest's own report.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"
# --disable-sandbox: the test runner already runs every unit under a sandbox
# profile of its own, and macOS refuses SwiftPM's nested sandbox-exec for the
# manifest and its plugins ("sandbox_apply: Operation not permitted").
swift build --disable-sandbox --build-tests
swift test --disable-sandbox --skip-build
