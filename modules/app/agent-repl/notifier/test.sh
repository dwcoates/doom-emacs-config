#!/usr/bin/env bash
# The Zettel notifier app's unit tests (Tests/ZettelNotifierTests), run by the
# `notifier` suite. SwiftPM builds the package (fetching the pinned
# swift-protobuf on a cold cache) and runs every XCTest case; a failing case
# fails the suite with XCTest's own report.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"
swift build --build-tests
swift test --skip-build
