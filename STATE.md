# Owner 11 (D33–D36, footer and sidebar) — STATE

Branch `overhaul/int-play-11`, worktree `integration-agents/play-11`. Stood down by the lead (wave two); nothing of mine is running.

## Authored (committed, 837580ec3)

- `modules/app/agent-repl/e2e/playtest_11_footer_sidebar_test.go` — four table-driven playbooks, one loop each, one capture per row:
  - `TestPlaytestFooterRateLimits` (D33, `11-rate-limits`): arranges `!usage-service-unavailable` first so the turn-close reprobe answers UNREAD and the event figure survives the close (otherwise the 82%/91% line retires inside the same turn and no capture can catch it); rows `!rate-limit-five-hour` (session 82% allowed_warning), `!rate-limit-seven-day` (weekly 91%), then `!rate-limit` overage: asserts the `daemon.footer.rate_limit_overage` warn record in the workspace daemon log and exactly two allowance cells.
  - `TestPlaytestFooterUsageOutcomes` (D34, `11-usage-outcomes`): `!usage-available` arranged (negative: no rate line), then service/window/utilization-unavailable and sampling-failure each asserted as `.footer-allowance-unread[data-sample=…]` with exact caveat text BESIDE standing 41%/63%, then `!usage-opus-absent` retires the line.
  - `TestPlaytestFooterContextStatus` (D35, `11-context-status`): `!context-tip`, `!tokens-reminder` (idle, no budget line), `!context-budget-warning` (verbatim text on the idle activity line).
  - `TestPlaytestMcpRows` (D36, `11-mcp`): `!mcp-all` then `/mcp` (five rows, exact healths, `spawn ENOENT` detail), `!mcp-healthy` then `/mcp` (all five rows still stand). Note: the product draws MCP healths as the `/mcp` command panel row on the feed, not in the workspace sidebar; the manifest says so.
- `gofmt -l .` empty, `go vet -tags playtest ./...` clean in e2e. `npm ci` done in shim and webapp.

## Ran

- Nothing completed. One run was started under `bin/suite-slot.sh` and killed on stand-down before it left the gate queue. No captures exist yet under `e2e/.playtest-out/playtest/11-*`.

## Next

1. Rebase onto `overhaul/integration` at 86f038026.
2. `bin/suite-slot.sh bin/playtest.sh -run 'TestPlaytestFooterRateLimits|TestPlaytestFooterUsageOutcomes|TestPlaytestFooterContextStatus|TestPlaytestMcpRows'` (image 37b7acd5b457 is current despite its age).
3. Remediate any blocker via a dispatched opus subagent (ruling: I author nothing further myself), rerun until green twice, inspect every PNG against its manifest, file mismatches.
4. Gates: gofmt/vet, ordinary host e2e green, webapp suites if touched.

## Open questions for the lead

- D33 pictures carry the unread caveat by arrangement (see above). If the lead wants a caveat-free allowance picture, it needs a fake-side lever (a scenario whose close reprobe is newsworthy) — a shim change, not a playbook change.
