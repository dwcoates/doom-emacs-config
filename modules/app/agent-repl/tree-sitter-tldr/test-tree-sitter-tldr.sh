#!/usr/bin/env bash
# test-tree-sitter-tldr.sh -- the tree-sitter-tldr suite: the grammar's corpus
# (test/corpus/*.txt), and a check that the committed generated parser (src/)
# is the one grammar.js generates, so no consumer compiles a stale parser.
#
# Run with:   bash tree-sitter-tldr/test-tree-sitter-tldr.sh
# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"
[[ -d node_modules ]] || npm ci --silent
tree_sitter=node_modules/.bin/tree-sitter

fresh="$(mktemp -d)"
trap 'rm -rf "$fresh"' EXIT
"$tree_sitter" generate --abi 14 -o "$fresh" >/dev/null
stale=0
for generated in parser.c grammar.json node-types.json; do
  if ! cmp -s "src/$generated" "$fresh/$generated"; then
    echo "not ok - src/$generated is not what grammar.js generates: run \`npm run generate\` and commit src/"
    stale=1
  fi
done
[[ $stale -eq 0 ]] && echo "ok   - src/ is what grammar.js generates"

"$tree_sitter" test
exit "$stale"
