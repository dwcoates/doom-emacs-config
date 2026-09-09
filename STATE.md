# Playtest owner 16 (G49-52: subagents, tasks, send-message) -- STATE

Standing down on the lead's instruction (wave two). Nothing is running; no
sandbox container, gate slot, or background process of mine survives.

## Where things are

- Branch `overhaul/int-play-16`, REBASED onto overhaul/integration 86f038026
  (clean fast-forward; no overlap with playtest_*.go or webapp/src/feed).
- `npm ci` done in agent-shim/claude/shim and webapp.
- NOTHING AUTHORED YET: no playbook file, no webapp change, no commit.
- Two dispatch attempts of the implementer brief hit the 20-subagent limit.

## Reading done (do not redo)

PLAYTEST-PLAN/SPEC, substrate (playtest_scenario_test.go, playtest_capture_test.go),
playtest_00_feed_tail_test.go, playtest_04 (template), subagents_e2e_test.go,
mergequeue TestFanWideCancel, remainder Send/Task tests, webapp bubble.ts,
feed-view.ts, stop.ts, strip.ts, expanded.ts, agent-prompt.ts, subagent.ts,
fake scenarios subagents.ts/tasks.ts.

## Toggle defect -- analysis so far (unproven, needs a real-webview repro)

In bubble.ts `open()`: ensureChild -> applyPage -> openWatch -> applyExpanded(true).
Nested rows present + data-expanded false means applyPage ran and something
between it and applyExpanded(true) threw or hung; `void expand()` swallows it.
The jsdom bubble.test "says it is expanded" passes, so the divergence is in
what the harness fakes (real AppContext / page-streams mux / daemon OpenFeed
answer). Plan: install unhandledrejection/error listeners in-page before the
caret click and read the error text; then fix at source + webapp unit test.

## Next (in order)

1. Dispatch ONE opus-medium implementer with the brief (kept verbatim in the
   lead's transcript of this owner; summary: file
   e2e/playtest_16_subagents_tasks_test.go, tag `playtest`, tests named
   TestPlaytest16*, artifacts playtest/16-subagents-tasks*/, table-driven,
   in-page assertions before every capture; fix the bubble toggle first with a
   webapp unit test; DOM hooks: .bubble[data-expanded]/[data-expand]/
   .bubble-subfeed, .subagent-head[data-state], .prompt-delivery[data-delivery]
   (+ .refused), .footer-chip[data-chip="tasks"|"agents"],
   [data-task-status], .footer-stop-all [data-interrupt],
   .footer-stop-note[data-stop-outcome="interruptedDetached"] "stopped 3 agents").
2. Runs via `bin/suite-slot.sh bin/playtest.sh -run TestPlaytest16`; image
   agent-repl-e2e-sandbox:latest (37b7acd5b457) is current -- do not poll, do
   not rebuild.
3. Inspect every PNG against MANIFEST.md; file mismatches.
4. Gates: gofmt/go vet -tags playtest in e2e; webapp npm test/lint/typecheck;
   playbooks green twice; report.
