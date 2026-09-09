# STATE — playtest owner 9 (D25–28, feed families), paused on owner order

Branch `overhaul/int-play-09`, rebased onto overhaul/integration 86f038026.

## Authored (all committed)
- a1a8a688e / 05f231afd  fake `!md` showcase carries a bare 4-line tree with two branches wider than 105 columns, dotless metaprompt labels; shim tests pin width and shape.
- a3148cd7e  e2e/playtest_09_feed_prose_test.go — one table, one loop, one workspace per row (md, interrupt, query-eof, query-fail, query-eof-mid-ask, rotate); PLAYTEST-PLAN.md row 27 reworded (errored terminal row + footer blocked line, not a failure overlay).
- a11fc7b5c  PRODUCTION FIX webapp/src/metaprompt-tree.ts: a daemon-wrapped continuation line (leading spaces/`│`, text, no connector) stays inside the tree region and counts as tree-shaped; 7 unit tests + 2 proseHtml tests. Was: the tree sheared to a markdown list at the first wrapped branch.
- 8b40b9f2e  webapp-layer integration assertion on `!md`: six `.mp-line`, a `│   │` prefix (ran green on host: TestWebappLayerFeedFamilies).
- 53305ed31  playbook closes each row's workspace after its capture (releases its webview; see defect A).
- 605097b19  roster.layer.test.ts lint fix (preserve-caught-error), pre-existing on the integration tip.

## Runs
- Run 1: row 25 red (tree not drawn) → fixed above.
- Run 2 (after fixes): rows 25–27 green, 5 captures under e2e/.playtest-out/playtest/09-feed-prose; row 28 (rotate, 6th workspace) red: page stalled at boot, rows=0 text="".
- Run 3 with the close-per-row change: NOT YET RUN. Need two consecutive greens.

## Inspection of run-2 captures
- 02 md: heading/fence/list/blockquote/rule fine; tree rails intact through the wrap. Notes: 🌳 draws as tofu (sandbox image lacks an emoji font); daemon 105-col break + bubble's narrower re-flow gives ragged orphan fragments.
- 03 interrupted: matches.
- 04 query-eof: MISMATCH — DOM assertions passed but the painted page is stale (empty feed, prompt still in hold tray, footer idle/ready): a WebKit paint stall. No stall record in webapp logs.
- 05 query-fail, 06 mid-ask: match.

## Defects to file for the lead (not this section's to fix)
A. Cross-webview connection cap: every live page pins one HTTP/1.1 connection (WatchPage mux); all xwidgets share one WebKit network process; WebKit caps a host at six; the 6th live page's boot stalls (adopt ~1s late, no WatchPage at the daemon, no forwarded webapp log). Product-level for users with 6+ open panels.
B. Substrate probe race (playtest_scenario_test.go): `agent-repl-playtest--js` is one global; an in-flight callback from the previous question/page can satisfy the next wait. Fix: tag each question with a nonce and compare.
C. WebKit paint stall (capture 04) — matches the known GUI stall investigation.
D. Sandbox image has no emoji font (tofu in trees).
E. Double wrap (daemon 105 cols vs bubble width) reads ragged; consider joining continuations in the webapp renderer or matching widths.

## Next
1. `bin/suite-slot.sh bin/playtest.sh -run 'TestPlaytestFeedProseFamilies$'` (foreground) twice; inspect 6 captures each time (especially whether 04's stall recurs).
2. Final gates: gofmt/vet in e2e (playtest tag), host `go test ./e2e`, webapp npm test/lint/typecheck, shim tests; rebase onto current overhaul/integration.
3. Final report per scenario row.
