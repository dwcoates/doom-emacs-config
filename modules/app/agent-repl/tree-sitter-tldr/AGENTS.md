# tree-sitter-tldr

The tree-sitter grammars of the metaprompt's TLDR tree. Every program that reads
a tree parses it with these, so no two of them can disagree on what a label, a
branch, a fence or an entry is.

| Grammar        | Parses                                                     | Read by |
|----------------|------------------------------------------------------------|---------|
| `tldr/`        | a response's tree: where it starts and ends, each line's class, each branch's indent, prefix, label and body, the fences | the daemon (`daemon/internal/prose`), through cgo (`tldr/src/binding.go`) |
| `tldr_prompt/` | a prompt's entries answering a tree's labels (`3.1.2 - my answer`) | nothing yet |

## `tldr` is the webapp's old parser, exactly

`tldr` is a one-to-one port of the line parsing `webapp/src/metaprompt-tree.ts`
did before it moved here (`findTreeRegion`, `isMetapromptTree`,
`looksLikeIntendedTree`, `splitTreeSegments`, `parseBranch`,
`parseContinuation`). The daemon reads every text it serves through it, repairs
the tree's labels and serves the result as `frontend.v1.ParsedProse`; wrapping
stayed in the webapp, which draws that graph and parses nothing.

- `tldr/src/scanner.c` does every line: it reads a line whole, classifies it
  and splits its body, each function named after the TypeScript it ports.
- `tldr/grammar.js` arranges the classified lines into `before`, `tree` and
  `after`, as `findTreeRegion` did. It is written so that every fork resolves
  by failure: the lines that follow always rule out all but one reading. Never
  resolve a fork with `prec.dynamic` instead: tree-sitter picks between
  readings locally, where they meet, which is not always the longer tree.
- `tldr/test/golden/golden.json` is the old parser's own answers over 3,613
  texts and 5,652 distinct lines, recorded once by `generate.mjs` against the
  commit it names. It is frozen: the old parser is gone, so it cannot be
  regenerated. `daemon/internal/prose/golden_test.go` holds the daemon's
  reading of the parse to every answer in it; any other program that reads
  the parse should hold its own reading to it the same way.

A change to how `tldr` parses is a change to what the daemon serves and so to
what the webapp draws. Make it on purpose, and say in the commit which golden
answers it changes and why.

## Generating

```bash
npm run generate
```

- Each grammar's generated parser (`<grammar>/src/parser.c`,
  `grammar.json`, `node-types.json`, `tree_sitter/`) is committed, so a
  consumer compiles `src/` without the CLI. Regenerate and commit them with
  every change to a `grammar.js` or `scanner.c`; the suite fails when any is
  stale.
- ABI 14 is pinned because Emacs 30 loads grammar ABIs 13 to 14 only
  (`treesit-library-abi-version`). The CLI's default, 15, would build a parser
  Emacs refuses.

## Testing

Tests run through the daemon (`agent-repld test run -suite tree-sitter-tldr`;
the `agent-repl-tests` skill says how). The `tree-sitter-tldr` roster suite runs each grammar's corpus
(`<grammar>/test/corpus/`, one case per edge) and checks every committed
generated file. A corpus expectation is written by `tree-sitter test --update`
and then read by hand: `--update` records whatever the grammar does, right or
wrong. A change here also runs the `daemon` suite, which holds the golden
answers, and the cross-system suite.
