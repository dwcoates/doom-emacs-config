# agent-repl todo

Deficiencies in agent-repl's code or its agent harness that an agent noticed
while working on something else, and that its work did not address. The
metaprompt's "Noticed deficiencies" section says when a row is added. One row
per deficiency; a row is removed when the work that fixes it lands.

| Noticed | Area | Deficiency | Where | Noticed while |
|---|---|---|---|---|
| 2026-10-25 | testrun | zettel is a program a run builds but no coverage system: it has no entry in `testrun/coverage-baseline.json`, so `roster.CoverageSystems` leaves it out and a zettel test removal is held to no baseline | `testrun/roster/roster.go` `CoverageSystems`, `testrun/coverage-baseline.json`, the gomeasure fixture tree | landing zettel chat threads through the merge queue |
| 2026-10-25 | webapp | the monospace face is spelled out at about twenty sites in `styles.css`, and six read a `--mono` token no rule declares, each with its own fallback (`monospace` alone at three, `ui-monospace, monospace` at two, a Menlo stack at one), so they already draw different faces. One root token should name the face, which first needs a ruling on which face the `monospace`-only sites should draw | `webapp/src/styles.css` (`.queued-command`, `.merge-commit`, and the other `var(--mono, ...)` and `ui-monospace` sites) | matching the prompt bubble to the response tree (`--tree-font-family`) |
