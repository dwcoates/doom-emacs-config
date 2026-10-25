# agent-repl todo

Deficiencies in agent-repl's code or its agent harness that an agent noticed
while working on something else, and that its work did not address. The
metaprompt's "Noticed deficiencies" section says when a row is added. One row
per deficiency; a row is removed when the work that fixes it lands.

| Noticed | Area | Deficiency | Where | Noticed while |
|---|---|---|---|---|
| 2026-10-25 | testrun | zettel is a program a run builds but no coverage system: it has no entry in `testrun/coverage-baseline.json`, so `roster.CoverageSystems` leaves it out and a zettel test removal is held to no baseline | `testrun/roster/roster.go` `CoverageSystems`, `testrun/coverage-baseline.json`, the gomeasure fixture tree | landing zettel chat threads through the merge queue |
