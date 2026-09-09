# Owner 17 (section H, failure arms) — STATE

Branch overhaul/int-play-17, cut from overhaul/integration at b5d4c0b18. Stood down by the lead (wave two); nothing is running.

## Authored (committed, 796a13cbc)
- modules/app/agent-repl/e2e/playtest_17_failure_arms_test.go — TestPlaytestFailureArms: one world, one table-driven loop over 29 rows (17 `!fail-*`, 12 `!api-*`; `fail-marker` excluded, it is the merge pipeline's `e2e-fail-this-turn` gate, not a typed `!` prompt). Per row: submit, await the newest root-feed `turnEnded` row by `[data-arm]` + exact `.turn-ended-cause` + exact `.turn-ended-vendor` (interrupted arm: text `interrupted`), await the fate of the in-flight work (`Working on it…` survives / none fabricated), await the tab on a settled arm, then capture. Artifacts under playtest/17-failure-arms/.
- gofmt -l . empty; go vet -tags playtest ./... clean.

## Ran
- Ordinary host e2e package (`go test ./e2e`, under suite-slot): green (ok 18.4s).
- The playbook itself has NOT run yet: two attempts never acquired the host suite slot (other owners held it); both were killed. No sandbox container, no gate slot of mine survives.

## Next
1. Rebase onto overhaul/integration 86f038026 (lead's instruction) before final gates.
2. Run: `bin/suite-slot.sh bin/playtest.sh -run TestPlaytestFailureArms` from the module root (image 37b7acd5b457 is current despite "3 hours ago"); expect to queue. Green twice consecutively.
3. If the 2s playtestPageBound is too short for a full turn round trip, measure at the site and add a named bound with the measurement — never widen blind.
4. Inspect all 29 PNGs with Read against MANIFEST.md; file/fix mismatches (a collapsed headline is the defect class).
5. Per the owner ruling relayed by the lead, any further edits go through a dispatched opus-medium/opus-low subagent in this worktree, not by hand.
