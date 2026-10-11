# tree-sitter-tldr

The one grammar of the metaprompt's TLDR tree, and of the prompt entries that
answer it (`3.1.2 - my answer`). `grammar.js` says what each node means;
`src/scanner.c` holds the three tokens that look past their own end.

Every program that reads a tree is meant to parse it with this grammar, so no
two of them can disagree on what a label, a branch or an entry is:

- the webapp, as wasm, to wrap and link a response's tree;
- the daemon, to bind a prompt's entries to the response they answer;
- Emacs (`treesit`), to highlight entries while the user types.

No program loads it yet. Until one does, `webapp/src/metaprompt-tree.ts` is
still the parser the feed runs, and the merge gate maps this directory to this
suite alone (`daemon/internal/merge/suiteselect.go`).

## Generating

```bash
npm run generate        # tree-sitter generate --abi 14
```

- The generated parser (`src/parser.c`, `src/grammar.json`,
  `src/node-types.json`, `src/tree_sitter/`) is committed, so a consumer
  compiles `src/` without the CLI. Regenerate and commit it with every change
  to `grammar.js`; the suite fails when it is stale.
- ABI 14 is pinned because Emacs 30 loads grammar ABIs 13 to 14 only
  (`treesit-library-abi-version`). The CLI's default, 15, would build a parser
  Emacs refuses.

## Testing

```bash
npm test                # or: bash test-tree-sitter-tldr.sh
```

The `tree-sitter-tldr` roster suite runs the corpus in `test/corpus/` (one
case per edge) and checks that `src/` is what `grammar.js` generates. A corpus
expectation is written by `tree-sitter test --update` and then read by hand:
`--update` records whatever the grammar does, right or wrong.
