#!/usr/bin/env bash
# test-tree-sitter-tldr.sh -- the tree-sitter-tldr suite, for both grammars
# (tldr, the response tree; tldr_prompt, the prompt entries):
#
#   - each grammar's corpus (<grammar>/test/corpus/*.txt);
#   - each committed generated parser (<grammar>/src/) is the one its
#     grammar.js generates, so no consumer compiles a stale parser;
#   - tldr's committed wasm is the one its src/ builds, so the webapp never
#     ships a stale grammar. Building it runs emscripten in Docker, which the
#     tree-sitter CLI drives; the build is reproducible byte for byte.
#
# The tldr grammar's answers are held to the old webapp parser's by the
# webapp suite (webapp/test/tree-parse.test.ts over tldr/test/golden).
#
# Run with:   bash tree-sitter-tldr/test-tree-sitter-tldr.sh
# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
cd "$root"
[[ -d node_modules ]] || npm ci --silent
tree_sitter="$root/node_modules/.bin/tree-sitter"

scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
failed=0

for grammar in tldr tldr_prompt; do
  cd "$root/$grammar"
  "$tree_sitter" generate --abi 14 -o "$scratch/$grammar" >/dev/null
  stale=0
  for generated in parser.c grammar.json node-types.json; do
    if ! cmp -s "src/$generated" "$scratch/$grammar/$generated"; then
      echo "not ok - $grammar/src/$generated is not what grammar.js generates: run \`npm run generate\` and commit src/"
      stale=1
    fi
  done
  if [[ $stale -eq 0 ]]; then echo "ok   - $grammar/src/ is what grammar.js generates"; else failed=1; fi
  "$tree_sitter" test || failed=1
done

cd "$root/tldr"
"$tree_sitter" build --wasm -o "$scratch/tree-sitter-tldr.wasm" . >/dev/null 2>&1
if cmp -s tree-sitter-tldr.wasm "$scratch/tree-sitter-tldr.wasm"; then
  echo "ok   - tldr/tree-sitter-tldr.wasm is what tldr/src/ builds"
else
  echo "not ok - tldr/tree-sitter-tldr.wasm is not what tldr/src/ builds: run \`npm run generate\` and commit it"
  failed=1
fi

exit "$failed"
