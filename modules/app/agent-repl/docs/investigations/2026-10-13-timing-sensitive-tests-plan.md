# Timing-sensitive tests: plan (2026-10-13)

## The problem

Full runs went red on tests that pass alone. The host's load average sat at
45-57 on 16 cores during those runs (116 at one point on 2026-10-13), so every
process ran 3-7x slower than when idle. Any test that fails on a wall-clock
number fails under that load, whatever the code does.

Two kinds of timing appear in the suites:

- **Kind A, latency assertions.** The test measures how long something took and
  fails above a bound. The bound always stands in for a structural fact
  ("the exit did not wait out a budget", "the revival was taken on the death").
- **Kind B, deadline waits.** The test waits for an event and fails if it has
  not arrived by a deadline. The event is legitimate and arrives late under
  load. The deadline is only there to turn a hang into a failure.

## Kind A: assert the fact, not the clock (done)

Each bound was replaced by an assertion on the record that states the fact.
Where the code rode a bound silently, it was made to say so first.

| Test | Bound | Now asserts | Commit |
| --- | --- | --- | --- |
| `e2e/daemonstop_e2e_test.go` `TestAStopAfterACompletedTurnLeavesOnTheStop` | 250ms | no shim record "did not end within its conclusion budget" | 641374ebd |
| `daemon/cmd/agent-repld/run_test.go` `TestARequestlessConnectionDoesNotHoldTheExit` | 1s | no WARN "the exit's http shutdown grace expired" (new record, the one exit bound that was silent) | 9262de7b0 |
| `daemon/integration/drain_rollout_test.go` stop after a turn | 1s | every exit bound reports at WARN+ (the sweep fails on it); the one INFO bound ride, "the session kill did not answer", is asserted absent | 9717d6b2c |
| `daemon/integration/dead_shim_recovery_test.go` revival | 1s | the revival record "the shim died on its own; its session is brought back now" | 6f3ef4d75 |
| `agent-shim/claude/shim/test/integration/gate.test.ts` forced KillSession during an ask | 250ms | no warn/error record once the process has exited (every teardown budget logs when spent) | 9b8ec1ce9 |

Mutation checks: removing `closeRequestless` fails the serve test, and
skipping the watcher conclusion fails the gate test by naming the budget
record.

Not changed, deterministic already:

- `webapp/test/webapp-layer/drive.test.ts` composer budget: the clock is
  `vi.useFakeTimers`, moved only by the stub's `settle()`.
- The perf gates in `e2e/perf_harness_test.go` and the scheduler smoke in
  `testrun/internal/sched/plan_test.go` are wall-clock by intent.

## Kind B: one real cost fixed

`agent-shim/claude/shim/test/main.test.ts` timed out at 2.5s in a loaded run
(the whole file took 32s). Each `main()` test re-imports `main.js` after a
module reset, and 220-270ms of each re-import was re-evaluating the generated
protobuf modules. The suite now pins them across resets
(`test/stateless-modules.ts`): the file's test time went from 7.9s to 2.65s and
no test exceeds 300ms (27dc1e550). Pinning in the shared setup file was tried
and rejected: every file then paid for the protos, and the suite's CPU went up
(56s to 67s user).

## Kind B: the rest is a design decision

The sites seen failing:

- `webapp/test/webapp-layer/roster.layer.test.ts`, the 900ms vitest timeout
  (`vitest.webapp-layer.config.ts`), 15 failures in one run.
- `daemon/integration/handover_stale_bounce_test.go:110`: the successor's
  handover took 10s against a healthy 1-2s; the record it waited for did
  arrive, late.
- `lisp/test-integration-helpers.el` `agent-repl-itest-default-timeout` (14s).
- `e2e` disk-manager waits (`context deadline exceeded`).
- The harness's own self-checks (`harness/stop_test.go:65`,
  `e2e/processexit_test.go:92`, `harness_cleanup_test.go:192`,
  `e2e/emacs_teardown_test.go:410`) have 5-7x margins and only fire on a hang.

Options:

1. **Progress deadlines (recommended).** A wait fails when the system under test
   has stopped making progress, not when a wall-clock budget runs out. The
   harness's wait primitives (`waitCtx`/`DefaultTimeout`, the ERT
   `--wait-until`, the vitest per-test timeout for layer suites) extend their
   deadline while the processes under test are progressing: new log records,
   or CPU time rising. A deadlock writes nothing and burns nothing, so it
   still fails at the short idle bound. A slow host cannot fail the wait.
   - Makes the load failure impossible, not unlikely.
   - Cost to the tight-timeout rule: the idle bound stays tight (2x measured
     healthy), but a livelock that keeps logging is caught only by the
     suite's outer timeout.
2. **Load-scaled budgets.** The same bounds, but counted in time the host
   could have run the test (wall time x min(1, cores / load)). Still a
   probability: a spike inside one wait can outrun the one-minute load
   average.
3. **Host admission.** The runner holds unit starts while load exceeds cores.
   It cannot stop other sessions' load arriving mid-run, so it reduces the
   failures rather than removing them.
4. **Re-measure under load and size each bound at 2x that.** This is the
   current rule (write-tests pitfall 5) applied to a 50-load host; the bounds
   grow 3-7x and a hang takes that much longer to report.
