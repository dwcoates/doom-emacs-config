# Owner 6 (B17-18: detached indicator, link severed/recovered) — live state

Opus-medium replacement owner. Branch `overhaul/int-play-06`, worktree
`integration-agents/play-06`.

## Done
- Rebased onto `overhaul/integration` by cherry-pick, DROPPING the lead's
  `wip(playtest/06)` 50ms-settle-window commit; integration's reconciled
  `playtest_capture_test.go` wins.
- `go vet -tags playtest ./e2e` clean after the rebase.

## Landed on this branch (inherited, all with tests)
- shim/fake: `AGENT_REPL_FAKE_DETACH_GATE` parks `bash-detach` after its first
  spool line, so "detached work outlives its turn" is arranged rather than raced.
- daemon/workspace: a reaped shim is not the workspace's client (`Fleet.Client`
  reads liveness like `Fleet.Shim`; `Fleet.Start` retires the dead row first),
  with a unit test and a daemon integration test.
- playbook `e2e/playtest_06_tab_arms_link_test.go`.

## In flight
- Run 1 of the section queued on the host suite slot (one image build has held
  it ~50 minutes with ~10 waiters behind it). Log:
  scratchpad/owner-06/run1.log; pictures under scratchpad/owner-06/out-r1.

## Remaining
- Two consecutive green runs with every picture matching its manifest sentence.
- Delete this file in the last commit before merge.
