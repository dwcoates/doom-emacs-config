# Owner 20 (K61-63, multi-workspace concurrency) -- paused state

Branch `overhaul/int-play-20`, rebased onto `overhaul/integration` at 86f038026.
Tree clean; no process, sandbox container or suite-slot of this owner survives.

## Authored (committed)

- 1eb63c320 fix(e2e/playtest): a tab-bar capture's sentence comes from the TAB BAR's table
  (substrate: `e2e/playtest_scenario_test.go`, `armPaint`/`captureArm`).
- e853bf795 test(e2e/playtest): `e2e/playtest_20_concurrency_test.go` with the three playbooks:
  - `TestPlaytestTwoWorkspacesThinkingAtOnce` (K61)
  - `TestPlaytestAttentionInBackgroundWhileForegroundIdle` (K62)
  - `TestPlaytestMergeBesideRunningTurn` (K63)

## Ran (one run, artifacts under `modules/app/agent-repl/e2e/.playtest-out/playtest/`, gitignored)

- K61 `20-concurrency-two-thinking`: completed; captures `05-both-thinking.png`, `06-both-settled.png`.
- K62 `20-concurrency-background-attention`: completed; captures `05-attention-in-background.png`, `06-ask-shown-after-switch.png`.
- K63 `20-concurrency-merge-beside-turn`: manifest stops after step 03 (child workspace created and settled) while the second workspace's panel was opening; the run was cut short by the stand-down order, so the outcome is unknown (not a confirmed failure). Failure artifacts in `e2e/.playtest-out/TestPlaytestMergeBesideRunningTurn/`.

## Not yet done

- No capture has been inspected against its manifest by the owner (K61/K62 images unread).
- K63 must be run to completion.
- Playbooks green twice consecutively: not yet.
- Gates: `gofmt -l .` / `go vet -tags playtest ./...` in e2e, host e2e package, not yet run on this tip.
- No production defects fixed or filed so far.

## Next

1. `bin/suite-slot.sh bin/playtest.sh -run 'TestPlaytest(TwoWorkspacesThinkingAtOnce|AttentionInBackgroundWhileForegroundIdle|MergeBesideRunningTurn)$'` from the module root.
2. Inspect every PNG with Read against MANIFEST.md; remediate or file.
3. Rerun for the second consecutive green; run the gates; report.
