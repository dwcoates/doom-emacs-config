# Owner 15 (F46–48: skills, hooks, automation) — state at stand-down

Worktree: /Users/dodgecoates/.config/doom-overhaul/integration-agents/play-15, branch overhaul/int-play-15 (cut from b5d4c0b18; NOT yet rebased onto overhaul/integration 86f038026).

## Authored (committed)

- modules/app/agent-repl/e2e/playtest_15_skills_hooks_automation_test.go (tag `playtest`): three table-driven tests, one world each — TestPlaytestSkills (4 rows), TestPlaytestHooks (4 rows), TestPlaytestAutomation (12 rows). Each row: composer RET submit, roster arm settle, in-page DOM predicate scoped to the just-submitted turn's `data-turn` after its `turnEnded` row, then capture. gofmt clean, `go vet -tags playtest` clean.
- npm ci done in agent-shim/claude/shim and webapp.

## Ran (run 1, sandbox image 37b7acd5b457, via bin/suite-slot.sh bin/playtest.sh)

- TestPlaytestSkills PASS (4 captures), TestPlaytestHooks PASS (4 captures), TestPlaytestAutomation FAIL at row `!worktree-keep`.
- Log: /private/tmp/claude-501/-Users-dodgecoates--config-doom/6a1b0e3a-9be5-4fdb-b4f7-d5c97ccdec15/scratchpad/play15-run1.log
- Captures: modules/app/agent-repl/e2e/.playtest-out/playtest/15-{skills,hooks,automation}/

## Inspection so far

- 15-skills: all four captures match their manifest sentences (teal loaded card with `loaded` badge, folded document, allows line; failed card with `Error: no such skill: absent-skill`; memory and skills-injected draw no card). Observation: after `!memory` / `!skills-injected` settle, the footer status reads `loading` with a `memory` / `listing` sub-status while the roster arm is settled — not asserted, may be the FooterStatusLoading arm; check against footer.proto before filing.
- 15-hooks: all four match. Observation to check: in `!hook-blocked` the loud hook card is drawn BELOW the turn's response bubble ("A hook blocked the edit.") rather than beside the gated Edit card — row order follows arrival; decide whether that is contract or a defect.
- 15-automation, `02-plan.png`: DEFECT CANDIDATE — the plan card is drawn TWICE (identical `plan` cards before the prose and after "The plan is ready."), while feed.proto says one FeedPlan bubble coalesces enter and exit onto one FeedId. Daemon or webapp; not yet localized.
- 15-automation, `03-findings.png`: DEFECT CANDIDATE — the capture is pixel-identical to 02-plan (no `!findings` prompt, no findings card visible) although the DOM predicate (findings row, 3 `[data-finding]`) passed. Either the feed did not follow its tail (scroll pinning) or the webview did not repaint. Investigate before the second run; compare with 15-skills/15-hooks where the feed did follow.

## Test defect (mine), fix pending

- worktree rows use `[data-row-kind="separation"][data-arm="worktreeEntered|worktreeLeft"]`; feed-view.ts mirrors the separation arm onto the row chrome as `data-state` only. Change the three occurrences to `[data-state=...]`.

## Next

1. Dispatch opus-medium: fix the worktree selectors; investigate the double plan card and the non-following capture; file or fix.
2. Rerun all three playbooks; inspect every capture (automation rows 04–13 not yet seen).
3. Second consecutive green run; rebase onto overhaul/integration 86f038026; gates (gofmt, go vet -tags playtest, ordinary host e2e package); report.

No process, container or suite-slot of mine is running.
