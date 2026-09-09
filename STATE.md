# Owner 12 (E37-E41, permissions / questions / mode picker) — STATE

Branch: overhaul/int-play-12 at 646e576b7 (cut from overhaul/integration b5d4c0b18).

## Authored (committed)
- modules/app/agent-repl/e2e/playtest_12_asks_test.go (build tag `playtest`):
  - TestPlaytestPermissionAllowFamily (E37 + `!perm-no-standing`), one table loop, one world
  - TestPlaytestPermissionDenyFamily (E38), one table loop, one world
  - TestPlaytestPermissionHoldSurvivesASwitch (E39), two workspaces, three captures
  - TestPlaytestQuestionFamily (E40), one table loop, one world
  - TestPlaytestPermissionModePicker (E41), three captures
  - helpers local to the file: readInPage, clickWithin, permissionRowSel, questionRowSel
- Gates passed so far: `gofmt -l .` empty; `go vet -tags playtest ./...` and `go vet ./...` clean (e2e dir).
- Deviation: the draft and its selector fix-up were authored by the owner directly, because every
  opus dispatch hit the session-wide subagent ceiling. Disclosed for the lead.

## Ran
- Nothing yet. `npm ci` done in agent-shim/claude/shim and webapp. No sandbox run started;
  no container, no suite slot, no process of mine is alive.

## Next
1. Rebase onto overhaul/integration 86f038026.
2. `bin/suite-slot.sh bin/playtest.sh -run 'TestPlaytestPermission|TestPlaytestQuestion'` (image
   agent-repl-e2e-sandbox:latest id 37b7acd5b457 is current per the lead).
3. Remediate blockers, rerun to two consecutive greens, inspect every PNG under
   e2e/.playtest-out/playtest/12-*/ against MANIFEST.md, file mismatches.
4. Ordinary host e2e package green; final gates; report.

## Misrouted item
- The coordinator's `TestBashPartialOutputWithSpill` / `!bash-spill` item belongs to owner 13 (F42),
  not this section; it was not acted on here.
