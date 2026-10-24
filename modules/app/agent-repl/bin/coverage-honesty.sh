#!/usr/bin/env bash
# coverage-honesty.sh PACKAGE -- one coverage-only suite: prove PACKAGE's
# per-file istanbul numbers are still a measurement (bin/coverage-honesty.mjs,
# through the package's own `npm run coverage:verify`, which names its probes).
#
# It runs only in a coverage run (testrun/roster, Suite.CoverageOnly): the
# check is about the coverage numbers, and it runs the package's coverage
# command three times, which no ordinary run should pay for.
#
# PACKAGE is the npm package's directory, relative to the agent-repl module.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

if [ "$#" -ne 1 ]; then
    printf 'usage: coverage-honesty.sh PACKAGE\n' >&2
    exit 2
fi
module="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
package="$module/$1"
if [ ! -f "$package/package.json" ]; then
    printf 'coverage-honesty: %s holds no package.json\n' "$package" >&2
    exit 2
fi
cd "$package"
exec npm run coverage:verify
