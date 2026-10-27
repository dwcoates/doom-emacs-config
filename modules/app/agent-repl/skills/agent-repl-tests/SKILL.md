---
name: agent-repl-tests
description: 'Write or change agent-repl tests (Go unit/integration/e2e, vitest, ERT, shell harnesses) so they are fast and cheap WITHOUT losing any coverage, and so every new or changed test obeys the host CPU governor. Use whenever an agent adds, changes, speeds up, or stabilizes a test anywhere under modules/app/agent-repl, writes a test fixture or harness helper, or runs tests for a change: every run goes through `agent-repld test run`, naming only the tests needed. Holds the measured test-performance pitfalls and their fixes as a living list.'
allowed-tools: Bash(~/.claude/skills/agent-repl-workspaces/agent-repld:*)
---

# Write tests (agent-repl)

## The rule

Test performance work NEVER loses coverage. A faster test that tests less is
a regression, whatever it saves. Every change below keeps the same assertions
over the same code paths; it changes only what the test costs to run.

## Running tests: `agent-repld test run`

Every test run goes through the daemon:
`~/.claude/skills/agent-repl-workspaces/agent-repld test run [-workspace WS] SELECTION...`.
This skill is linked in the repository alone, so it calls the installed
`/agent-repl-workspaces` front door, which runs the main checkout's build.
Never a hand-run `go test`, `npm test`, `emacs -batch`,
`bin/background.sh ...` or `bin/test-all.sh`. The daemon runs the
workspace's own `bin/test-all.sh` with the selection, so the run takes
background priority, the host's one test-CPU budget, and the runner's own
log.

Name only the tests you need: the tests covering what you changed. Never
the full suite: the merge queue runs it as the gate of every landing (the
metaprompt's "When to run which tests" rules how broad a run is).

Selections (each a repeatable flag; paths relative to the working directory
or absolute, inside the workspace's worktree):

```text
-go DIR                        every test of the Go package in DIR
-go DIR=TestA,TestB/case/sub   those tests; TestB/case/sub is one subtest path
-vitest FILE                   every test of the vitest file
-vitest 'FILE=suite test'      one test or describe block by its full name; repeat the flag for more
-ert FILE                      every test of the ERT file
-ert FILE=test-a,test-b        those ert-deftests
-harness SCRIPT                the whole shell harness
-harness SCRIPT=group1,group2  those groups of a harness that lists groups
-harness SCRIPT=TestA,TestB    those top-level Go tests of a sandboxed suite (bin/test-e2e-emacs.sh)
-suite NAME                    one roster suite, whole
-every-suite                   the whole roster (the merge queue's gate)
```

`-every-suite` is the full suite; an agent names it only for a change with
systemic knock-on effects, and says why.

Options:

```text
-coverage                instrument and report coverage; every selection must be a whole suite (-suite or -every-suite); the coverage-only suites (the istanbul honesty probes) run in a coverage run of -every-suite, or when named with -suite
-record                  append the run's timings to test_time.csv; -every-suite alone, on master, only when the user asks
-runs N                  repeat the run N times, each its own run with its own log (every frame names its run); for a flaky test, repeat just that test
-stop-at-first-failure   stop repeating at the first failing run
-cpu-load N -as-user     hold N busy loops for the run's length; ONLY when the user asks for load (owner ruling 2026-10-06); the daemon refuses load from a workspace
```

The command exits 1 when any run of a repeat failed.

Example, run from `modules/app/agent-repl`:

```bash
~/.claude/skills/agent-repl-workspaces/agent-repld test run -go daemon/internal/merge=TestQueue/front -ert lisp/test-status.el=agent-repl-status-test-x
```

Output: one line of protojson per event as it arrives: `planned` (the units
and the run's log path), `unit_ended` per unit (verdict, wall time, per-test
results, and a failed unit's output tail), then `ended` (passed or failed
counts) or `refused` (a selection naming something the roster does not hold,
or a name the runner cannot find). Read the run's log path from `planned`
for full output.

Exit codes: 0 passed, 1 tests failed, 6 refused (bad selection, unknown
workspace, no selection), 2 the run or the call failed (e.g.
`runner_failed`, which carries the script's exit code and last lines).

A Go integration package (`-tags integration`) is selected by its directory
like any other; the runner knows its tags.

### Which tests are redundant: `agent-repld test redundancy`

`agent-repld test redundancy go|vitest|report [-workspace WS] FLAGS` runs
`testrun redundancy` through the daemon, with the tool's own flags (`-out`
absolute or relative to the working directory; every other path relative to
the working directory, inside the worktree). It prints the measurement's
output as it comes and exits 0 when the measurement succeeded.

What the tool measures and every flag's meaning:
`modules/app/agent-repl/AGENTS.md`, "Which tests are redundant".

Example, run from `modules/app/agent-repl`:

```bash
~/.claude/skills/agent-repl-workspaces/agent-repld test redundancy go -module daemon -pkg ./internal/workspace -tests sessions_test.go -out /tmp/redundancy-sessions
~/.claude/skills/agent-repl-workspaces/agent-repld test redundancy vitest -package agent-shim/claude/shim -mutate 'src/**/*.ts,!src/fake/**' -out /tmp/redundancy-shim
```

## Every test obeys the host CPU governor

Owner ruling, 2026-10-26: tests together may only bring the host's TOTAL CPU
use up to 65%. The governor (`testrun/internal/cores`, `cores.Governor`)
only admits units: a unit starts once it holds its core seats, and while the
host is at the cap no unit of any run starts ("waiting for CPU"). A running
unit is never paused. The mechanics are in `modules/app/agent-repl/AGENTS.md`,
"THE HOST'S TOTAL CPU IS CAPPED" and "A UNIT'S WIDTH MUST BE THE CORES IT
USES"; read those for how it works, not restated here.

A new or changed test or suite must do the following so the governor can
hold it:

1. **Run only as a testrun unit.** A new suite gets a roster entry
   (`testrun/roster/roster.go`), never a script, hook or helper that runs
   tests outside `agent-repld test run`. Work outside a unit holds no seat,
   so the governor cannot count or hold it back.
2. **Use exactly the cores its width says.** Pin it (Go: `GOMAXPROCS`, `-p`,
   `-parallel`; vitest `--maxWorkers=1`), or give the roster entry the
   measured width (`Slots`) and cap the unit to it on its own command line,
   as `e2e-emacs` does with `docker --cpus`. The run summary's "it is not
   pinned to its width" line names a unit that uses more; fix that before
   landing. Measured 2026-10-26: `daemon:integration` units holding 2 seats
   used up to 4.5 cores.
3. **Keep all of its work inside its own process tree, and reap it.** A
   daemon, VM or worker a test starts and leaves running, or reparents
   away, uses CPU no seat accounts for.
4. **Runner-side work that is test load must hold a seat too.** This
   includes testrun's own planning, e.g. its `go list -deps ... -test ./...`,
   which measured 109% CPU outside any unit. Adding more such work outside
   units is refused. This is a known gap, recorded in `org/todo.md`.

A test's own timeouts are unaffected by CPU waits, since the wait happens
before the unit starts; a harness bound that spans a whole run must allow
for "waiting for CPU" (the run's events carry `waiting_for_cpu` and
`cpu_wait_ended`).

## The rules this builds on

They are owned elsewhere; read them, do not restate them:

- `modules/app/agent-repl/AGENTS.md`: one test file per source file, one edge
  case per test, every change tested, the test-run cadence, the shared suite
  slot.
- `daemon/AGENTS.md`, `webapp/AGENTS.md`, `lisp/AGENTS.md`,
  `agent-shim/*/AGENTS.md`: each suite's exact test and lint invocations.
- Go: no `time.Sleep` for synchronization (channels, `sync.WaitGroup`);
  table-driven tests in Arrange/Act/Assert.
- No real git in any test (the fake git); mock external dependencies only.
  Owner approval, 2026-10-12: the daemon's TEST logs and TEST state may live
  in memory (the in-memory state seam; the log pipe was tried and rejected,
  pitfall 2). Our own systems
  otherwise run for real.
- Each system's `## Performance lessons (living document)` section in its
  AGENTS.md: that system's own lessons.

## Pitfalls and their fixes (measured)

Each entry: the pitfall, the evidence, the fix, where it landed, and its
coverage impact (always none).

1. **Copying a fixture binary per test.**
   - Evidence: every daemon start copied the 3.3MB fake git into its fakebin: ~3GB written per daemon suite, its largest single writer (2026-10-12).
   - Fix: hard-link the suite's one built binary (`harness.InstallFakeGit`, da3219969). The stray sweep matches by argv, which names the link's path exactly as the copy's.
   - General form: build a fixture ONCE per package (TestMain / `sync.Once`), give each test its own isolated view (a link, a namespace, a key prefix), never a per-test copy.

2. **Funnelling every test daemon's logs through the one test process. REJECTED.**
   - What was tried: the test log pipe (`AGENT_REPL_TEST_LOG_PIPE`, 49cfaafc6), records over a unix socket into the harness's memory, files only for a failed test. It halved the test binary's writes (553MB -> 283MB), about 0.5MB per test.
   - Why it was reverted (2026-10-12, 4e5d4c563): under load the daemon integration suite failed broadly, a different set of waits timing out each run. The daemons' log writes blocked on a collector that had fallen behind.
   - Why making the daemon's writes non-blocking did not save it (branch `logpipe-v2`, measured with three copies of the integration package at once): the daemons no longer stalled, but each copy still failed 5-8 tests against master's 1 failure in all three, with file logs under the same three-copy load. Every daemon's logs still went through ONE collector in ONE test process; starved of CPU under load, its reads, barriers and log awaits fell behind.
   - Lesson: do not centralize per-process I/O into the test process to save bytes. Files let each daemon write straight to the kernel and each reader read independently; on the RAM disk they cost almost nothing. Cut the record VOLUME instead (pitfall 4).

3. **Test SQLite on disk.**
   - Fix: the in-memory state seam (`AGENT_REPL_TEST_STATE_IN_MEMORY`, 510ee7408, 0515d102a, 61621f034). ~570 of ~790 integration daemon starts run on memory.
   - Pitfall inside the fix: a test that starts a SUCCESSOR daemon on the same state root (restart, handover, crash recovery) or reads `wsm.db` must start its first daemon with `harness.Opts{DurableState: true}`. The harness refuses loudly otherwise ("was started on in-memory state, so nothing of it outlived its daemon"); never silence that, set the option (6146f4fce).

4. **Debug-logging volume.**
   - Evidence: the integration suite runs its daemons at debug; the sidebar re-logged every workspace's unchanged row decisions on every render, in each of three views (26% of a test daemon's records).
   - Fix: log a decision when it CHANGES, with one record counting what was skipped (40482a061). Prefer that over lowering a record's level: the suite asserts debug records.

5. **Wait bounds sized on an idle host.**
   - Evidence: wait-bound tests run 5-7x slower inside a full run than alone (ERT integration files 3.4s -> 28s; an e2e successor test 1.7s -> 10.5s, past its 5s bound) (docs/investigations/2026-10-11-test-speed.md).
   - Fix: a bound is a small multiple of the healthy max OBSERVED UNDER REALISTIC CONCURRENCY (a full run), stated at the site with its measurement; a genuinely long wait gets its own explicit bound. Never fix a flaky wait by raising a timeout without that measurement.
   - Owner ruling, 2026-10-13: never make a timeout very long, and think carefully about what a reasonable one is. A long timeout is extremely expensive when it fails, because every failure, and every hang, pays the whole bound, and bounds 10-100x the healthy latency have hung runs and gates. A too-short timeout causes flakiness. Size it just over 2x the healthy latency measured under realistic concurrency, name the operation it bounds at the site, and give every blocking wait a bound (no unbounded channel receives, process waits or polls). The full rule is in AGENTS.md, "Every timeout is sized deliberately".

6. **The macOS RAM disk throttles low-priority I/O.**
   - Evidence: units run at `taskpolicy -d utility`; on the run's RAM disk a 4KB fsync is p50 0.1ms but p90 166ms, p99 330ms during a run, and 86ms per fsync whenever default-tier I/O touches the same volume (the SSD shows no such effect) (test-speed investigation, fourth round).
   - Consequence for test design: every fsync and every small write in a hot test loop can cost tens of milliseconds under load. Write less (pitfalls 2-4), batch, and never fsync in a test unless the fsync is the subject.

7. **Running a whole suite when a targeted run would do.**
   - Evidence: the host has ONE suite slot shared by every session's full runs and every merge gate; gates waited 64-338s for it before any suite started, and a load average of 25-95 turned timing-sensitive tests red across unrelated merges (docs/investigations/2026-10-12-merge-queue-latency.md).
   - Fix: run only the tests you need, through `agent-repld test run` (see "Running tests"); never the full suite, which is the merge queue's gate.

8. **Sequential waits that could overlap.**
   - Fix: `t.Parallel()` with per-test isolation (own temp dirs, own state roots, own sockets); wait-bound packages overlap 8 tests per chunk (df9f409ed; 16 per chunk was slower and failed).

9. **Speculative work that is thrown away.**
   - Evidence: merge-queue gates passed on a tip that then moved, and restart-cut gates re-ran from scratch, including their slot wait (merge-queue latency report).
   - Fix in tests: do not start work whose result the test then discards (a full fixture for a test that asserts a refusal at the first call; a second daemon a test never talks to).

10. **Process spawns and polls in the harness.**
    - Fixes that kept coverage: `isZombie` reads the kernel's state in process instead of spawning `ps` (409602c36); the log reader decodes only what was appended since its last read.

11. **A fixture that costs more than the test it serves, built per test.**
    - Evidence (2026-10-14, solo, steady state): an e2e `World` costs about 2.7s wall to build and tear down, a fresh workspace inside a running one 0.30-0.41s; three tests on three Worlds took 7.80s and 1.33 CPU-s, on one World 2.36s and 0.72 CPU-s. A daemon-integration `newOpened` fixture costs 0.14s, which is not worth sharing (docs/investigations/2026-10-14-test-shared-setup-lzb.md).
    - Fix: share the fixture, never a slice of it: `SharedWorld(t)` (e2e) builds one default World per test process, owned by `harness.Suite()`, and each test takes a `harness.Daemon.View` with its own sweep over the workspaces it registered. The rule is AGENTS.md "A case may share an expensive fixture, never a slice another case reads".
    - Coverage impact: none; every test keeps its own case, workspace and failure.

## Changes that LOOK faster and are refused

- Skipping a test, or moving it behind a flag nobody sets: coverage loss.
- Weakening an assertion (checking less of the result, matching a prefix): coverage loss.
- Sharing state that more than one case READS or CHANGES: hides order-dependent failures. Sharing an expensive fixture in which every case owns its own slice is allowed, under AGENTS.md "A case may share an expensive fixture, never a slice another case reads".
- Raising a timeout instead of fixing the wait: hides the race and makes every failure slower.
- Replacing one of our own systems with a mock outside the owner-approved list above: loses integration coverage.

## Measuring a test change

- Whether a case is REDUNDANT (may be deleted): `agent-repld test redundancy`,
  never judgement. It measures the mutants each case kills and proves which
  cases the rest dominate; see AGENTS.md, "Which tests are redundant".

- Wall and CPU: `/usr/bin/time -p <command>` (user+sys is CPU), before and after, same host load.
- Bytes written: sample each process's `proc_pid_rusage` `ri_diskio_byteswritten` (per process; short-lived processes and root processes are invisible to it), and the SSD's cumulative counter (`ioreg -c IOBlockStorageDriver -r -l`, "Bytes (Write)") with a quiet baseline before and after. Other sessions' runs contaminate the system counter: say so.
- Flakes: repeat with `agent-repld test run ... -runs N` (for a flaky test,
  repeat just that test) and report pass counts; add load only with
  `agent-repld test run ... -cpu-load N -as-user`, and only when the user
  asks for it, never a hand-written busy loop. A flaky test is root-caused
  and fixed, never retried.
- Report numbers (bytes, seconds, counts), never "less" or "faster".

## Living document

This file is updated whenever a session finds an important test-performance
improvement or pitfall. Add or update an entry under "Pitfalls and their
fixes" with:

- the pitfall,
- the evidence, with numbers and the date,
- the fix,
- the commit or investigation file,
- its coverage impact (none, or the change is refused).

A lesson specific to one system also goes into that system's AGENTS.md
`## Performance lessons (living document)` section.
