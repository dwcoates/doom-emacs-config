# STATE — owner 5 (plan B14–B16: failed, hibernated, merging/done/parked)

Branch `overhaul/int-play-05`, rebased onto overhaul/integration at 9b23085c6. Tree clean at pause.

## Authored (all committed)
- `modules/app/agent-repl/e2e/playtest_05_tab_arm_failed_test.go` — three playbooks:
  `TestPlaytestTabArmFailed` (B.14), `TestPlaytestTabArmHibernated` (B.15),
  `TestPlaytestTabArmMergingDoneParked` (B.16). Artifacts under `playtest/05-tab-arms-lifecycle/`.
- Substrate: my `armPaint` fix was dropped at rebase in favor of owner 20's identical fix (a229e634a).

## Production defects fixed (daemon), each with unit + integration tests
- dfb0fae53 fix(daemon/drain): host view republished after the hibernation lease is released
  (Emacs composer gate stayed `:draining` after a park, so a revival prompt was refused).
- 174063281 fix(daemon/workspace): `sidebar.Registry.Sessions` was never populated in production,
  so the resolver's parked branch was unreachable and a parked row resolved `dead`.
- 0291abc84 fix(daemon/drain): roster republished after the hibernation stand-down record.
- 642e0883c, c1eb912b7 integration tests for the above.
- b8218be5c fix(daemon/footer): a parked session is idle on the footer strip and topbar dot
  (was `disconnected · dead`, which also closed the webapp composer); 8f1a0d4a2 its integration test.
  The subagent that landed these was stopped at the pause; its final gate report was NOT received —
  rerun daemon `make test` + `make integration` before trusting b8218be5c/8f1a0d4a2.

## Runs
- Run 4 (before the footer fix): all three playbooks PASS (`play05-run4.log` in the session scratchpad);
  every picture inspected and matches its manifest. Run 1 pictures (B.14, B.16) also matched.
- Needed next: two consecutive green section runs on the current tree
  (`bin/suite-slot.sh bin/playtest.sh -run 'TestPlaytestTabArm(Failed|Hibernated|MergingDoneParked)$'`),
  inspection of B.15's pictures for the footer now reading idle (not `disconnected · dead`),
  ordinary host e2e package green, daemon suites green, `gofmt -l .` / `go vet -tags playtest ./...` in e2e.

## To file / open
- daemon.md ~line 1074 says "a hibernated session REFUSES input", which conflicts with
  "implicit revive on prompt" (~975) and SPAWN ON MOUNT; read as the lease's refusal during the
  stand-down only. Lead to rule.
- Owner 4's `playtest_04_tab_arms_test.go` carries a stale dangling B.14 comment (mine to own).
- Fix agent noted the ACQUIRE side of drain/hibernation leases also does not republish the host view.
