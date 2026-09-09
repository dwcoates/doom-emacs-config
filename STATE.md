# Owner 14 (F44–45, files + web) — state at stand-down

Branch `overhaul/int-play-14`, rebased onto `overhaul/integration` at 86f038026.

## Authored (committed, 0709d164b)
- `modules/app/agent-repl/e2e/playtest_14_files_web_test.go` — two table-driven playbooks:
  - `TestPlaytestFileTools` (F44): 14 rows — read, read-head, read-range, read-truncated, read-image, write-create, write-update, edit, ide-diagnostics, ide-diagnostics-write, grep-content, grep-files, grep-count, glob. Artifacts `playtest/14-files-web/44-files/`.
  - `TestPlaytestWebTools` (F45): 3 rows — web-fetch, web-fetch-redirect, web-search. Artifacts `playtest/14-files-web/45-web/`.
  - Per row: submit `!scenario` via composer RET; assert the ordinal-th `[data-unit="simpleToolCall"]` row names the tool, is `data-state=returned`, `data-verdict=succeeded`, has the expected `data-output-form`, plus a per-row DOM predicate (omitted text "showing 2 of 4 lines", `[data-diff-line]`, `.tool-diagnostic` rows, link anchors, no output body for read-image); then response bubble settled + roster arm settled; then capture.
- `gofmt -l e2e` empty, `go vet -tags playtest ./...` clean (checked after the rebase).
- `npm ci` done in agent-shim/claude/shim and webapp (before the rebase; lockfiles unchanged by it).

## Ran
- Nothing has executed yet. Run 1 was queued behind the suite-slot gate (another owner's playtest held it) and was killed at the stand-down order. No sandbox container or slot of mine exists.

## Next (on re-wake)
1. `cd modules/app/agent-repl && bin/suite-slot.sh bin/playtest.sh -run 'TestPlaytestFileTools|TestPlaytestWebTools'` (detached; image 37b7acd5b457 is current).
2. Remediate anything red (delegate edits to opus agents per the owner ruling), rerun until green twice consecutively.
3. Inspect every PNG under `e2e/.playtest-out/playtest/14-files-web/` against MANIFEST.md; file/fix mismatches (note: read-image is expected to draw NO image — the `none` form is the contract this wave).
4. Gates: gofmt, go vet -tags playtest, ordinary host e2e package (`TMPDIR=/tmp bin/suite-slot.sh go test ./... -count=1 -parallel 8` in e2e), webapp suites only if the webapp is touched.
5. Report to the lead.
