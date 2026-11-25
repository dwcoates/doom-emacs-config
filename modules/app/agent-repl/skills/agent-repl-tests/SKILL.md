---
name: agent-repl-tests
description: 'Load BEFORE beginning any work on agent-repl tests: writing, changing, reviewing, assessing, auditing, running, speeding up, stabilizing or measuring a test anywhere under modules/app/agent-repl, a fixture or harness helper, a suite or roster entry, the test runner and its CPU governor, or profiling where the CPU of a test run goes (`-profile`). Keeps tests fast and cheap WITHOUT losing coverage, under the host CPU governor and the test performance standards; every run goes through `agent-repld test run`, naming only the tests the change added or changed. Holds the measured test-performance pitfalls and their fixes as a living list.'
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

A repository's main workspace sits on the main checkout, so the branch it
works on lives in a git worktree it cut by hand, which no workspace is
registered at. It runs that worktree's tests with `-worktree DIR`:
`agent-repld test run -worktree DIR SELECTION...`. Selection paths then lie
in DIR. The daemon refuses `-worktree` from any workspace but a main one,
and for a directory that is no worktree of the main workspace's repository.

Name only the tests your own work added or changed. Every other test, and
the full suite, runs only in the merge queue's gate: a workspace never runs
the full suite (the metaprompt's "When to run which tests" rules how broad a
run is).

Selections (each a repeatable flag; paths relative to the working directory
or absolute, inside the workspace's worktree):

```text
-go DIR                        every test of the Go package in DIR
-go DIR=TestA,TestB/case/sub   those tests; TestB/case/sub is one subtest path
-vitest FILE                   every test of the vitest file
-vitest 'FILE=suite test'      one test or describe block by its full name; repeat the flag for more
-ert FILE                      every test of the ERT file
-ert FILE=test-a,test-b        those ert-deftests; a FILE with no directory (test-x.el) is the one in lisp/, from any directory
-harness SCRIPT                the whole shell harness
-harness SCRIPT=group1,group2  those groups of a harness that lists groups
-suite NAME                    one roster suite, whole
-every-suite                   the whole roster (the merge queue's gate)
```

`-every-suite` is the full suite, the merge queue's gate. A workspace never
names it (owner ruling, 2026-11-15); the one exception is a queue bypass
holder's own landing gate, which stands in for the queue's.

`-rerun`, in place of every selection, runs the workspace's recorded cases:
the daemon records every case each run of the workspace names (a Go subtest
path as its top-level test), and every failing case a merge queue gate reads
off its branch, each once, for the workspace's life. A rerun runs each of
them the tree still holds; a case renamed, moved or deleted since is dropped,
named on stderr, and forgotten, and a record with nothing left to run is
refused. It is the branch's regression set, so a workspace may run it at any
time: it runs only recorded cases, never a whole suite unless one is a
recorded case itself (a failing unit whose tests could not be named), and
never the whole roster. The merge queue's fixing brief has every attempt run
it.

Options:

```text
-coverage                instrument and report coverage; every selection must be a whole suite (-suite or -every-suite); -every-suite measures every system over every tier and fails on a tier it should have run, while a selection reports each system only as far as the selected suites reached it, every line provisional as a partial measurement; the coverage-only suites (the istanbul honesty probes) run in a coverage run of -every-suite, or when named with -suite
-record                  append the run's timings to test_time.csv; -every-suite alone, on master, only when the user asks
-runs N                  repeat the run N times, each its own run with its own log (every frame names its run); for a flaky test, repeat just that test
-stop-at-first-failure   stop repeating at the first failing run
-profile                 CPU-profile every unit that runs and the production processes its tests start (see "Profiling a run"); refused with -coverage and -record
-remeasure               bound every selected unit as one never measured, so a unit that legitimately got slower passes and records its new peak; the runner otherwise kills each unit at five times its expected time (`run.BoundFactor`)
-debug-info              link every Go test binary and program with its DWARF debug information, for a debugger; a TEMPORARY debugging measure only, never a standing setting: runs otherwise link without it (-ldflags=-w), which costs about a fifth less link CPU and keeps every stack trace and file:line
-no-reuse -as-user       run every selected unit, reusing no earlier pass; ONLY when the user explicitly asks to measure or debug the tests themselves (owner ruling 2026-11-13); never part of ordinary work, and the daemon refuses it from a workspace
-ungoverned -as-user     run with no CPU governor, on every core; ONLY when the user asks for it (owner ruling 2026-11-13: every run is governed by default, a queue bypass holder's included); the daemon refuses it from a workspace
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
counts; a run that failed with every unit passing, such as a full run's
placement check, names why in `runFailures`, also said on stderr) or `refused` (a selection naming something the roster does not hold,
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

### Profiling a run: `-profile`

`agent-repld test run -profile SELECTION...` shows where a run's CPU goes, in
test code and in the production code its tests run. Off by default.

- What it profiles:
  - every Go test binary, through `-test.cpuprofile`;
  - every node process of a vitest unit, workers included, through `--cpu-prof`;
  - every daemon, shim, store and sidecar a test starts: each profiles its
    whole life and writes the profile when it stops;
  - every ERT chunk, through the elisp CPU profiler, sampling the chunk's
    loads and tests (`testrun/ert/driver.el`).
- Where the profiles land: `<run log dir>/profiles/`, beside the run's log and
  its failure artifacts, pruned with them.
  - One directory per unit, named by its unit id (`:`, `/` and `#` become `_`).
    - `test.cpu.pprof` is a Go unit's own profile.
    - `CPU.*.cpuprofile` are a vitest unit's node processes.
    - `ert.cpu.eprofile` is an ERT chunk's profile, and `ert.cpu.top.tsv` its
      hottest functions by total and by self samples (samples, share, kind,
      function), readable without Emacs.
  - Inside it, one directory per test that started production processes,
    named by the test the same way.
    - `daemon-<pid>.cpu.pprof`, `store-<pid>.cpu.pprof` and
      `sidecar-<pid>.cpu.pprof` are the Go services.
    - `shim-<pid>.cpuprofile` is each shim.
    - `workspaces.tsv` maps each workspace id the test registered to the
      test, one `<workspace id>\t<test>` line each.
  - A shared test daemon (`harness.Suite()`) profiles into the directory
    named `Suite`.
- The run's `ended` event names the directory (`profileDir`), and the command
  says it on stderr.
- A reused unit does not run, so it writes no profile. Profiling a whole suite
  with nothing reused is `-no-reuse -as-user`, the user's alone.
- A merge gate profiles only when its merge asks:
  `agent-repld workspace async-merge ... -profile-gate` or
  `agent-repld workspace finished ... -profile-gate`. Its profiles land beside
  the gate run's log, which the gate's archived output names.
- Reading the profiles:
  - Go: `go tool pprof -top FILE` for the hottest functions, `go tool pprof
    -http=: FILE` to explore. Several files merge into one view: `go tool pprof
    -top FILE1 FILE2...`.
  - The daemon labels its per-workspace work by workspace id
    (`daemon/AGENTS.md`, "Profiler labels"): `go tool pprof -tags FILE` lists
    each workspace's share, and `go tool pprof -tagfocus workspace=<id> -top
    FILE` keeps one; `workspaces.tsv` says which test that workspace was.
  - node: load a `.cpuprofile` into Chrome DevTools (Performance, Load
    profile) for a flame chart.
  - ERT: read `ert.cpu.top.tsv` directly; for the call tree,
    `M-x profiler-find-profile` on `ert.cpu.eprofile`.

## Every test obeys the host CPU governor

Owner ruling, 2026-11-13: the tests together average 35% of the host's
cores, their own share that other load does not eat, and never bring the
whole host above 50% (`cores.BaseLimits`). On AC power in High Power Mode
they average 50% and the host reaches 55% (`cores.PoweredLimits`, owner
rulings 2026-11-17 and 2026-11-20), read at every second, so unplugging returns the base
limits for the next unit admitted. The governor
(`testrun/internal/cores`) measures the tests' CPU apart from the host (every
process carrying `cores.NestedEnv`), averages the last 3 one-second reads, and
admits a unit when tests + its cores fit the target and other + tests + its
cores fit the ceiling. A unit costs its own history (`plan.CoresKey`), else 2
cores per slot. It admits, never pauses. Tests keep normal CPU and disk
priority (a lower one made their time depend on the owner's work and tripped
limits). The mechanics are in `modules/app/agent-repl/AGENTS.md`,
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
   measured width (`Slots`) and cap the unit to it on its own command line.
   The run summary's "it is not
   pinned to its width" line names a unit that uses more; fix that before
   landing. Measured 2026-10-26: `daemon:integration` units holding 2 seats
   used up to 4.5 cores.
3. **Keep all of its work inside its own process tree, and reap it.** A
   daemon, VM or worker a test starts and leaves running, or reparents
   away, uses CPU no seat accounts for.
4. **Runner-side work that is test load must hold a seat too.** testrun's
   planning `go list -deps ... -test ./...` (measured 109% CPU outside any
   unit) holds one core seat per listing (`reuse.HeldGoList`). Adding
   runner-side work that holds no seat is refused.

A test's own timeouts are unaffected by CPU waits, since the wait happens
before the unit starts; a harness bound that spans a whole run must allow
for "waiting for CPU" (the run's events carry `waiting_for_cpu` and
`cpu_wait_ended`).

## A Go module's test binaries are built by one command

- Every unit-test binary of a Go module pass (its ordinary pass, and its
  tagged integration pass) is built by ONE build unit, `<suite>:build` or
  `<suite>:build[<tags>]`: `testrun build-tests` runs one `go test -exec`
  over every package (`testrun/internal/gobuild`), held to its width
  (`suites.BuildSlots`, pinned by `-p` and `GOMAXPROCS`).
- Never a build unit per package, and never a hand-written `go test -c` in a
  unit. Each package's tests still run as their own units against its binary,
  so per-package verdicts, bounds, failure output and reruns by path are
  unchanged; a package that does not compile fails only its own units, with
  its compiler output.
- A step that drops units from a suite (a selection's narrowing, per-package
  reuse) ends with `suites.Units.Finish`, so the build names only the packages
  whose units remain.
- Enforced by `TestEveryRosterGoSuitePlansOneBuildPerPass`
  (`testrun/internal/suites/modulebuild_test.go`), which fails when a roster
  suite plans any other build unit.

## Every suite declares its inputs; reuse is per unit

- Every roster suite (`testrun/roster/roster.go`) declares `Reuse`, what its units read beyond what the runner derives, or `NeverReused`, a written reason it can never be reused.
  - `roster.Check` refuses a suite with neither or with both, at the start of every run and in `TestEverySuiteDeclaresItsInputs`, so a new suite lands with its declaration.
  - `NeverReused` is only for a suite no profile can confine (a container's daemon reads the checkout outside it, a scan walks the whole module) or whose verdict is not a function of its inputs (a latency measurement).
- Reuse is per UNIT: a Go package (unit or integration, a tagged pass named `<package>[<tags>]`), a vitest test file, an ERT test file, a suite-wide check (`typecheck`, the e2e suite's `vet`), or a whole script or e2e suite.
  - A GoModule suite's package owns its own vet verdict: its check unit (`<suite>:<package>:vet`) belongs to the package's unit, and the pass's one vet (`<suite>:vet`) vets only the packages whose units run. A package without test files is a unit of its vet check alone.
  - A unit's inputs are derived by the tools, never kept by hand: a Go package's from `go list -deps -test` (every package its test binary links, their testdata and embedded files, go.mod, go.sum), a vitest file's from its import graph, an ERT file's from what it and the helpers load.
  - A run skips every unit whose inputs an earlier green run on the host passed it on, from any workspace: a unit's key is its inputs' bytes, never a checkout path, branch or workspace.
  - Two inputs are not read by their bytes:
    - What git ignores (session state, build output) is never part of a key.
    - Generated protobuf code (`proto/gen/`) is read by its symbols: a unit's key holds the fingerprint of every message, enum and service its other inputs name, closed over what each holds, so a comment-only `.proto` edit re-runs nothing and a changed symbol re-runs only the units that name it (`testrun/internal/reuse/bindings.go`).
  - A script suite with `Reuse.Scripts` reads its script and, transitively, every script of those directories one it reads names, never a whole directory.
  - A unit that passed is recorded though its suite failed, so the rerun after a fix runs only the failed units and those whose inputs the fix touched.
- Each unit runs confined to exactly the inputs its key covers. A test failing with "Operation not permitted" on a checkout file reads an undeclared input.
  - Declare it for that unit in `Reuse.Units["<unit>"]`, or in `Reuse.Inputs` only when every unit of the suite reads it. Never widen a profile.
  - A refused read does not always say "operation not permitted": a listing of an undeclared directory comes back EMPTY (`filepath.Glob` drops the refusal), so a scan that globs a directory fails on finding nothing. Its declaration is the directory (a `/`-suffixed subtree).
  - A unit whose reads cannot be enumerated goes in `Reuse.Never` with why: it runs unconfined and is never recorded.
  - A targeted run (`-go DIR=Test`, `-vitest FILE=name`) is never confined, so only an untargeted run of the unit proves a declaration; run one after adding a read.
- vitest and ERT files run in BATCHES, one process per batch (`testrun/internal/suites/batch.go`): files of one area (a vitest file's most imported `src/` directory, an ERT file's module family), at most the suite's own `Batch.Max` per batch, areas under its `Batch.Min` pooled (`roster.Suite.Batch`, default 48 and 12). A batch is one process, but reuse is per FILE (`reuse.decideBatch`): a batch runs only its files whose own inputs changed since each last passed, confined to those files' inputs together, and regrouping files or changing a suite's batch size never forces a rerun; so a batch's size trades only process starts.
  - Accepted caveat: a file that passed beside some files is taken to pass beside any others; a cross-file leak that hides is a defect fixed at its source (`modules/app/agent-repl/AGENTS.md`, "Integration tests share one canonical process, each case on its own slice").
  - A file its suite never reuses (`Reuse.Never`) is a batch of its own.
  - A suite's batch size is tuned by bisecting toward the best batch count: change its `Batch`, then compare the suite's average CPU seconds per merge-gate run over about 10 real merges before and after (`agentrepl_test_unit_cpu_seconds` through `/agent-repl-metrics`). A full run's CPU is the starting estimate (with per-file reuse a size trades only process starts); real merges decide.
  - Measured 2026-11-20, before per-file reuse (one full run each, loaded host): webapp 364 CPU-s at 48/12 against 428 at 96/24; at 192/48 one process of 146 files overflowed its stack, from computed-style wrappers stacked file over file (fixed in `test/animations.ts`).
- How keys, profiles and records work is `modules/app/agent-repl/AGENTS.md`, "A run reuses an earlier pass of each unit's exact inputs".

## Test performance standards (owner rulings, 2026-11-12 and 2026-11-13)

- A change that makes a suite slower or costlier is a defect, fixed before it lands. Report it in numbers: the suite's wall time and CPU before and after.
- vitest and ERT stay batched; one process per file came back once and made `webapp` take 15.6 minutes in the gate (pitfall 15).
- `NeverReused` is the owner's to grant: a new never-reused suite, or a suite becoming one, needs their approval. Today's list is `e2e`, `repo-scans`, `proto` and a few whole-module script harnesses.
- No test needs Docker; the Emacs e2e layer was removed for it (2026-11-13, reviewed 2026-11-20, `e2e/AGENTS.md`). An e2e step proves a real seam and names it (`Seam:`); what one system can prove goes to that system's tier.
- Measuring what the suites cost is done only at the user's explicit request: `-no-reuse -as-user`, fresh then rerun, in High Power Mode (Low Power roughly doubles the times), reported per suite. Reference, 2026-11-13, governed: `webapp`+`shim`+`ert` fresh 4.7 min, rerun 10s.

## E2E coverage is a step of a scenario

- A new top-level e2e test is only for an entirely new, large feature, and it is itself a scenario (`scenario(t, steps...)` in `e2e/scenarios_test.go`).
- Any other e2e coverage is a new step inside the existing scenario whose subject it extends: a `testXxx(t *testing.T)` function in the file of its subject, listed in that scenario's `scenario(...)` call.
- A step takes its world per `modules/app/agent-repl/AGENTS.md` "Integration tests share one canonical process, each case on its own slice".
- Why (owner ruling, 2026-10-31): the merge gate runs every e2e test side by side under the host's 35% CPU cap; tests that pass alone failed there waiting on a CPU inside their 5-6s bounds (in the 72h before 2026-10-31, `TestEmacsTabsRehydrateFromTheRosterOnConnect` failed 8 gate runs and `TestEmacsKillCloseLeavesTheTabOrderAlone` 6, across unrelated merges). Scenarios cap the concurrent worlds at the number of scenarios. The owner accepted "god tests" for this.
- Layout and how a step reruns: `e2e/AGENTS.md`, "Every world-building test is a step of a scenario".

## Synchronization: events, never time

A flaky test is a test whose verdict depends on timing. Owner ruling,
2026-11-02: the suites carry none. Every rule below applies to test code and
to every harness, fake and fixture a test uses, in every language.

### No sleep stands in for an event

- A test waits for the EVENT it needs: a channel receive, a `sync.WaitGroup`,
  a mutex or condition variable, a process exit, a line on an inherited pipe,
  a stream frame, a promise or deferred the fake resolves, a log record.
- NEVER `time.Sleep`, `setTimeout(r, ms)`, `sleep-for`/`sit-for`, a timed
  `accept-process-output`, or shell `sleep N` to let something happen first.
  A sleep that passes today fails under the gate's load tomorrow.
- A sleep loop is a sleep: polling a condition every few milliseconds is
  allowed only when no event exists, and then the poll obeys the fault latch
  rule below.
- A fixture that must report something (a pid, its readiness, that it
  trapped a signal) reports it over a pipe the test reads, never through a
  file the test polls for.
- A fake whose call a test awaits signals the call (a channel, a resolved
  deferred), so the test never polls a recorder.
- Time itself is the subject only through a fake clock: Go's injected
  `clock`/`now`, vitest fake timers, ERT's stubbed `current-time`. A
  `setTimeout(r, 0)` flush is a sleep too when the work it waits for is not
  one macrotask; await that work's own promise.
- A negative assertion ("X never happens") is not a window of time: order it
  against a later positive event that the system emits only after X would
  have happened.

### A wait fails at its dependency's failure, not at its bound

- Every process a harness spawns (the daemon, a shim, the store, the sidecar,
  Emacs, a fake daemon, Xvfb) has ONE FAULT LATCH, owned by the harness that
  spawned it. The latch closes, with its cause, the moment the process:
  - exits when nobody asked it to,
  - writes a record that would fail the test anyway (an ERROR, or a WARN the
    test did not declare with `ExpectWarnings`),
  - reports that it could not start (its boot refusal, its readiness error).
- Every wait selects on the latches of the processes it depends on, beside
  its event and its bound. The test then fails at that instant, naming the
  dependency's own record and its stderr or log tail, instead of failing
  seconds later with "never satisfied".
- A test declares what it provokes BEFORE it provokes it, since a record
  written before its declaration closes the latch. In the daemon harness:
  - a boot's warnings go in `harness.Opts.ExpectWarnings`, never in an
    `ExpectWarnings` call after `StartDaemon` returns (the daemon writes them
    while it boots, as does a fixture with a short `IdleCutoffMS`);
  - a mid-life warning is declared right after its daemon is made, before
    the act that writes it;
  - a failing exit that is the subject is declared with
    `Daemon.ExpectFailedExit` (or `Opts.ExpectEarlyExit` for a refused
    boot); `Daemon.AwaitExit` itself is never ended by the exit it awaits;
  - a test whose subject is its own undeclared warnings (a sweep test, a
    golden's regeneration) calls `Daemon.SweepIsTheSubject`, which stands its
    record latch aside while its cleanup sweep still judges every record.
- A dependency that initializes asynchronously reports its failed boot at
  once (its exit, its refusal record), and the harness's readiness wait ends
  on that report. A readiness wait that only times out is a defect.
- A failed call inside a polled form is a failure, not a value: a poll that
  gets an error back from the thing it asks (an elisp signal, a refused rpc,
  a 4xx from a fake) fails with that error unless the test declared the
  error an expected intermediate state.
- The bound stays, as the FALLBACK for a hang that writes nothing and does
  not exit. Its size comes from the metaprompt, "Every timeout is two to three times
  the measured expected time of what it bounds", on the granted clock where the harness has one. A timeout is never the
  main channel a failure has.
- Shared wait primitives own this, so a test cannot forget it: the daemon
  integration harness's `Await*` family, the e2e `AwaitEval`/`AwaitView`
  helpers, ERT's `agent-repl-itest--wait-until`, and the vitest harness waits.
  A new wait goes through one of them, or adds the latch to its own select;
  a wait loop hand-rolled without the latch is refused.

### A resource that is not the subject is built once

- A test builds fresh only what it tests. A dependency that is not the
  subject (the fake git, a compiled fake, Xvfb, a Doom boot, the store and
  sidecar under a daemon test, a copied fixture tree, a module re-import) is
  built ONCE per test process (`TestMain`, `sync.Once`, `testenv.ProgramOr`,
  `harness.Suite()`, a vitest `globalSetup`) and each case takes its own
  slice of it: a hard link, a key prefix, a fresh workspace, a fresh buffer.
- Our own service under test runs as ONE canonical process per test process
  that every integration case shares on its own slice. A case starts its own
  only for the two reasons AGENTS.md "Integration tests share one canonical process, each case on its own slice" names: process-wide modal state (a group
  process shared by every case needing that precondition) and the process's
  own lifecycle (a fresh one). Starting the service per case is the most
  expensive pitfall measured (2026-11-20: per-case daemons were 69% of the
  daemon suite's CPU per merge gate); pitfall 19.
- What a case may share is ruled by AGENTS.md "Integration tests share one canonical process, each case on its own slice".

### Sharing by suite

Each suite's entry names how a new test takes the shared process; its AGENTS.md
section holds the rest.

- **Daemon integration (`daemon/integration`)**: a case takes the canonical daemon with `newCanonical(t)`, `newCanonicalRegistered(t)` or `canonicalView(t)`; its slice is its own repository and workspace, and an id it mints (a turn id, a URL a stub records) carries its workspace id, since a turn id is unique across the daemon.
  - A case that changes daemon-wide state takes its group with `newInGroup(t, group)`; only a case whose subject is the daemon's own lifecycle starts a fresh daemon; a view's idle barrier (`ExpectNoPush`, `ExpectFileUnchanged`) waits only for its own workspaces' work.
  - The rest: `daemon/AGENTS.md`, "The integration suite's canonical daemon".
- **webapp integration** (`webapp/test/integration/`): `const shared = sharedMount(options)` at a file's top, then `harness = await shared()` in a case; a case's slice is its own turn (its rows, removed at its end), its own fake-daemon script and calls, and its handed panels.
  - The mount is per FILE, not per worker, because the setup resets the page before each file; chrome (footer, topbar, rail) is pushed onto it, and what a case opens through a control is undone by the file's `endCase`.
  - The rest: `webapp/AGENTS.md`, "Integration cases share one mounted app".
- **shim** (`agent-shim/claude/shim`): a case takes the file's shim with `canonicalSession()` (`test/integration-support/canonical.ts`); a shim hosts ONE session, so a case's slice is its turns.
  - Mint ids with `uniqueTurn()`, hand every one to `endsTurn`, and read the store, log and transcript through `since(...)`; a shared precondition is a `groupShim`/`groupSession` key.
  - The rest: `agent-shim/claude/shim/AGENTS.md`, "Integration cases share the canonical shim".
- **ERT integration (`lisp/test-integration-helpers.el`)**: a scenario takes the shared fake daemon through `agent-repl-itest--with-fake-daemon`; there is ONE fake daemon per batch Emacs process, started on the first scenario and reaped at exit, and `agent-repl-itest--begin-scenario` resets it on the way IN (two `/_fake/reset`s around the subscriber drain, the state root swept, the address re-published), so a case's slice is the whole fake for its own run, since ERT runs one test at a time.
  - Newcomers get wrong: production's process-global tables (workspaces, host names, prompt queue, selection slot, timers, log targets) are let-bound per scenario inside the macro, so a new process-global table a feature adds must join that binding list or it leaks between scenarios; only a case whose subject is a daemon's own lifecycle (a bounce, a second daemon, a cold start) starts another, through `agent-repl-itest--with-second-daemon` or `agent-repl-itest--with-cold-start`.
  - The rest: `modules/app/agent-repl/AGENTS.md`, "ONE fake daemon serves a whole suite run".
- **e2e** (`e2e/`): a scenario step takes `stack.SharedWorld(t)` and registers its own workspace (`w.NewWorkspace(t)`); a step needing other `WorldOpts` (a daemon option, a sidecar window, a real zettel) takes `stack.GroupWorld(t, "<group>", opts)`, one World per group shared by every step asking for it; a step whose subject is the daemon's lifecycle (deploy, relaunch, stop, cold boot) takes `stack.ScenarioLifecycleWorld(t, daemonOpts)`, a fresh daemon and sidecar on its scenario's shared store; only a step that stops or restarts the store builds `stack.NewWorld`.
  - A glob of the account root narrows to the step's own `harness.ProjectDir(w.DefaultConfigDir, ws.GetDir())`, and a provoked warning is declared with `w.ExpectWarningsFor(ws, ...)`; a targeted run puts each step in its own process, so sharing only shows when a scenario runs whole.
  - The rest: `e2e/AGENTS.md`, "Scenario steps share one World per precondition".

### Finding and fixing a flake

- Gate history is the first evidence: every gate archive is under
  `~/.claude-emacs/merge-logs/`; a test that failed across unrelated
  branches, or failed one round and passed the next, is a flake.
- Reproduce with `agent-repld test run ... -runs N` on just that test; never
  under added load unless the user asks.
- The fix removes the timing dependency at its source: production first when
  the race is in the product, the harness when the test read an intermediate
  state. Raising a bound, adding a retry, or rerunning is refused.

## Log calls are sites, and their templates are kept stable

Every log call in every runtime passes its helper a literal template with
named placeholders (`"timed out connecting to shim {shim} after {elapsed}"`
with the values named), and every record carries the call's `site` key and
`template`. The rule's one home is `logging-contract.md` "Log sites"; what a
test author needs from it:

- A site's key is the first 12 hex of the sha256 of the runtime, the calling
  function (in TypeScript, the logger's static scope) and the template. It is
  the same for every record the call writes, whatever values they carry.
- Change a template only when it is really necessary. Rewording a template,
  or renaming or moving the function it is written in, gives its site a NEW
  key, which detaches the site from its history: the production checks then
  read it as a site that never fired before.
- A test asserting a record matches on its operation and its rendered
  message, as before; it may also assert its `Template` or `Site`.
- Each runtime has a static scan test over every log call in its source. A
  scan failure names the call: a placeholder its named values lack, two calls
  in one function writing one template (give each its own wording), or two
  colliding keys.

## The rules this builds on

They are owned elsewhere; read them, do not restate them:

- `modules/app/agent-repl/AGENTS.md`: one test file per source file, one edge
  case per test, every change tested, the test-run cadence, the shared suite
  slot.
- `daemon/AGENTS.md`, `webapp/AGENTS.md`, `lisp/AGENTS.md`,
  `agent-shim/*/AGENTS.md`: each suite's exact test and lint invocations.
- Go: table-driven tests in Arrange/Act/Assert.
- No real git in any test (the fake git); mock external dependencies only.
  Owner approval, 2026-10-12: the daemon's TEST logs and TEST state may live
  in memory (the in-memory state seam; the log pipe was tried and rejected,
  pitfall 2). Our own systems
  otherwise run for real.
  - A test found running real `git`, whoever wrote it and however long it has
    stood, is fixed in the same work to use the fake git (`bin/fake-git.sh`,
    `daemon/integration/fakegit`); finding it is never a reason to leave it
    or to report it as pre-existing (owner ruling, 2026-11-17).
  - This replaces the before-and-after checkout comparison
    `.claude/safe-test-run.sh` used to make, which could not tell a test's
    change from the agent's own edits; the wrapper now checks only the refs
    the run's own git moves.
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
   - Fix: a bound is two to three times the healthy max OBSERVED UNDER REALISTIC CONCURRENCY (a full run), stated at the site with its measurement; a genuinely long wait gets its own explicit bound. Never fix a flaky wait by raising a timeout without that measurement.
   - Sizing rule: the metaprompt, "Every timeout is two to three times the measured expected time of what it bounds".

6. **The macOS RAM disk throttles low-priority I/O.**
   - Evidence: units run at `taskpolicy -d utility`; on the run's RAM disk a 4KB fsync is p50 0.1ms but p90 166ms, p99 330ms during a run, and 86ms per fsync whenever default-tier I/O touches the same volume (the SSD shows no such effect) (test-speed investigation, fourth round).
   - Consequence for test design: every fsync and every small write in a hot test loop can cost tens of milliseconds under load. Write less (pitfalls 2-4), batch, and never fsync in a test unless the fsync is the subject.

7. **Running tests beyond the ones the change added or changed.**
   - Evidence: the host has ONE suite slot shared by every session's full runs and every merge gate; gates waited 64-338s for it before any suite started, and a load average of 25-95 turned timing-sensitive tests red across unrelated merges (docs/investigations/2026-10-12-merge-queue-latency.md).
   - Fix: run only the tests your own work added or changed, through `agent-repld test run` (see "Running tests"); every other test is the merge queue's gate.

8. **Sequential waits that could overlap.**
   - Fix: `t.Parallel()` with per-test isolation (own temp dirs, own state roots, own sockets); wait-bound packages overlap 8 tests per chunk (df9f409ed; 16 per chunk was slower and failed).

9. **Speculative work that is thrown away.**
   - Evidence: merge-queue gates passed on a tip that then moved, and restart-cut gates re-ran from scratch, including their slot wait (merge-queue latency report).
   - Fix in tests: do not start work whose result the test then discards (a full fixture for a test that asserts a refusal at the first call; a second daemon a test never talks to).

10. **Process spawns and polls in the harness.**
    - Fixes that kept coverage: `isZombie` reads the kernel's state in process instead of spawning `ps` (409602c36); the log reader decodes only what was appended since its last read.

11. **A fixture that costs more than the test it serves, built per test.**
    - Evidence (2026-10-14, solo, steady state): an e2e `World` costs about 2.7s wall to build and tear down, a fresh workspace inside a running one 0.30-0.41s; three tests on three Worlds took 7.80s and 1.33 CPU-s, on one World 2.36s and 0.72 CPU-s. A daemon-integration `newOpened` fixture costs 0.14s, which is not worth sharing (docs/investigations/2026-10-14-test-shared-setup-lzb.md).
    - Fix: share the fixture, never a slice of it: `SharedWorld(t)` (e2e) builds one default World per test process, owned by `harness.Suite()`, and each test takes a `harness.Daemon.View` with its own sweep over the workspaces it registered. The rule is AGENTS.md "Integration tests share one canonical process, each case on its own slice".
    - Coverage impact: none; every test keeps its own case, workspace and failure.

12. **Too many e2e worlds at once in the gate.**
    - Evidence: the 8 and 6 gate failures above, each logging up to about 1s "waiting for a CPU" per wait (2026-10-31, merge-queue-retest-rounds' analysis).
    - Fix: 113 world-building top-level tests became 12 scenarios whose steps run in order (`e2e/scenarios_test.go`); the gate reruns a failed step alone by its path (daemon/internal/merge/failingtests.go `deepest`, testrun gotest.PathsPatterns).
    - Measured (2026-10-31, solo): the Go e2e chunk 21.3s -> 16.3s wall; the Emacs layer's `go test` 64.2s -> 74.3s, its longest scenario (`TestEmacsWorkspaces`) 40.9s.
    - Coverage impact: none; every former test runs whole as a step, its body unchanged but for its `t.Parallel()`.

13. **A wait that learns of its dependency's failure only from its bound.**
    - Evidence: every flake in the merge gates from 2026-10-30 to 2026-11-02 was a wait running out its bound while its dependency had already failed: a page that hung on a Vite chunk cycle and never drew (`TestEmacsKillCloseLeavesTheTabOrderAlone`, 5 branches; `TestEmacsTabsRehydrateFromTheRosterOnConnect`, 4), a registration refused in silence (`TestDoom/*`, 4 steps at 5s each), a successor that closed the gap a test meant to read (`TestHandoverTransfersAFreeWorkspaceThroughTheAdoptionRendezvous`, 2). Each read as "slow under load" (docs/investigations/2026-11-02-flaky-tests.md).
    - Fix: the fault latch (see "Synchronization: events, never time"); the dependency's failure ends the wait with its own record.
    - Coverage impact: none; a wait that fails sooner asserts the same thing.

14. **A build unit per Go package, and a whole suite rerun for one package's change.**
    - Evidence (merge gate, `~/.claude-emacs/merge-logs/abc9582429d943eb-tests-1.log`): 514 units, 960s wall, 1,785 CPU-s; the daemon suite alone planned 340 units, 167 of them `:build` units (230s of unit wall, 281 CPU-s), each a `go test -c` that re-linked what the others had just compiled. A change to any daemon file reran all of it.
    - Fix 1: one build command per module pass ("A Go module's test binaries are built by one command"). Measured (2026-11-11, full `-suite daemon` runs under the CPU governor): the daemon suite plans 175 units; its builds are two units, `daemon:build` 82.7-92.2 CPU-s (49-56s wall at `-p=2`) and `daemon:build[integration]` 4.7 CPU-s.
    - Fix 2: each daemon unit-test package is reused on its own green pass ("Every suite declares its inputs; reuse is per unit"), now every unit of every suite. Measured (2026-11-11): with a one-line change to `internal/runecap` (22 dependent test packages), 114 packages were reused and 61 units ran, 259 CPU-s against 792 CPU-s for the same suite with nothing reused; `daemon:build` built only the remaining packages, 26.4 CPU-s.
    - Coverage impact: none; every package's tests run as their own units against the same binaries, and a reused package's pass is of exactly its inputs.

15. **One process per vitest or ERT file.**
    - Evidence: per-unit reuse (450f3ccd79) ran each file in its own process; a vitest process costs about 12s beyond its tests, so the gate's `webapp` went from 2.3 to 15.6 min and `ert` from 1.1 to 3.8 (2026-11-12).
    - Fix: area batches ("Every suite declares its inputs"), 0.425.8. Governed, High Power, 2026-11-13: `webapp` units sum 2.9 min, longest 0.9.
    - Coverage impact: none; every file still runs, confined to its batch's inputs.
    - 2026-11-20: reuse became per file within a batch (`reuse.decideBatch`), so a batch's size no longer decides what reruns, only how many processes start.

16. **Measuring in Low Power Mode.**
    - Evidence: persistent-wifi mode held the machine in Low Power with the lid open; an ungoverned run's longest units took about twice what the governed High Power run's did (2026-11-13).
    - Fix: check `pmset -g` shows `powermode 2` before any measurement; persistent wifi now changes the energy mode only on lid close and open (0.427.1).
    - Coverage impact: none.

17. **A platform guarantee assumed, not checked.**
    - Evidence: the e2e Emacs teardown's `awaitFindable` (since removed with the Emacs layer) polled `/proc` for a stand-in on the premise "Start returns at the fork". Replaced by a one-shot invariant check, it failed on its first linux run (2026-11-02): linux wakes the vfork parent at `exec_mmap`, before the new environment is published, so `/proc/<pid>/environ` can lag `cmd.Start`.
    - Fix: assert a believed guarantee loudly once, and keep a wait only where it fails; the wait stays a poll (no event exists) that ends at the stand-in's exit.
    - Coverage impact: none.

18. **A fault latch finds the defects a suite was silently carrying.**
    - Evidence: the Emacs e2e layer's elisp record latch (never landed; the layer was removed) failed every scenario on its first run, twice: a sandbox-only bytecode ERROR and a panel-finder record logged with no scope, both written in every scenario for weeks with nobody reading them (2026-11-02).
    - Fix: land a latch only after a run of the whole suite under it, and fix each record it finds at the source; declare one (`ExpectWarnings`) only when the test provokes it on purpose.
    - Coverage impact: none; the latch adds failures, never removes them.

19. **A service process started per integration case.**
    - Evidence (2026-11-20): the merge-queue gate metrics over the 29 merges put the daemon suite at about 920 CPU-s per gate, 637 of it (69%) in daemon:integration units whose ~546 top-level tests nearly each start their own daemon; daemon:integration[integration]#01 used 5.5 CPU-s per wall second.
    - Fix: one canonical process per test process, per AGENTS.md "Integration tests share one canonical process, each case on its own slice"; conversion under way per suite (daemon, webapp, shim).
    - Coverage impact: none; every case keeps its name and assertions, scoped to its own slice.

20. **Build CPU spent on debug information, a re-vetted module, an unpinned program build and a full typecheck.**
    - Evidence (2026-11-20, one run each, warm, at the build unit's width): linking every daemon test binary cost 88 CPU-s with DWARF and 71 without (`-ldflags=-w`), the daemon program 9.6 and 4.8; `go vet` of all 188 daemon packages cost about 16.3 CPU-s, 94 of them 9.5, 47 of them 4.9; a program build held to one slot used 1.17-1.45 cores in the gates; the webapp's full `tsc --noEmit` cost 19-20 CPU-s, the shim's 16.3-16.9.
    - Fix: every run links without DWARF unless `-debug-info`; each package's vet belongs to its own reuse unit, so a gate vets only the packages it runs; a program build holds `BuildSlots`, pinned as the module build is; webapp and shim typecheck incrementally with declaration emit, each worktree seeded from the host's newest passing typecheck (`testrun typecheck`). Seeded from another worktree, the webapp's typecheck cost 5.8 CPU-s with nothing changed and 4.9 after a body edit to a file sixteen files import.
    - Pitfall inside the fix: incremental `tsc` under `--noEmit` keys a file's signature on its text, so a body edit to that file cost 26.1 CPU-s, more than the full check. Declaration emit makes the signature its declarations.
    - Coverage impact: none; every binary, package and file is still built, vetted and typechecked, and tsc compares every source by the hash of its text.

21. **Every vitest process re-transforming the same modules.**
    - Evidence (2026-11-20): on vitest 2.1.9 a 48-file webapp batch cost 25-34 CPU-s, about a third of it transforming modules every earlier process of the run had already transformed; the gate's webapp suite cost 269.3 CPU-s and the shim's 197.8 (gate 06af8b3d47e14195, nothing reused).
    - Fix: vitest 5.1.1 (vite 7.3.8; vite 8 would have replaced the production bundle's Rollup) and its persistent module cache, kept per worktree (`webapp/vite-cache.ts`'s `vitestModuleCacheDir`, the shim's in its own `node_modules`). Each entry is keyed by the module's path and its text's hash, so a confined unit never reads one for a changed source. Measured unit by unit in full `-suite` runs: webapp 176.3 CPU-s, shim 140.3 (cache cold).
    - Pitfalls the upgrade surfaced, each a fix at its source: a second `vi.spyOn` of the same method returns the spy already installed, so a spy per fixture recursed; a getter spy and a setter spy on one accessor restored the mocked getter for every later file; files that left the root element's attributes or the document's own properties behind failed others once vitest 5 reordered the files (`webapp/test/setup-shared-worker.ts` now starts every test on a bare root element and the document as the environment made it); a module reading an import at its top level inside an import cycle read `undefined` (`src/sidebar/section-classes.ts`).
    - Coverage impact: none; every test runs with its assertions unchanged.

22. **One wait bound spent on a sequence of waits.**
    - Evidence (2026-11-24): `TestSubagents/BashDetachedStartAndComplete` took one `w.WaitCtx()` and spent it on six waits in a row; under the gate's load its settle wait ran the 5s bound out (5.001s granted of 6.731s wall, 2.004s waiting for a CPU), though it passes alone and its gate passes took 0.84-2.62s for the whole step. `TestSubagents/DetachedLostWentSilent` and `TestTurns/InterruptAfterTextDelta` chained their waits the same way.
    - Fix: a wait helper bounds its own wait (`w.WaitCtx()`, or `grantedclock.WithBound` for a named window) and takes no context from its caller, so a step cannot stretch one bound over several waits (`e2e/detachedbash_e2e_test.go`'s `dbAwait*`, `e2e/detachedlost_e2e_test.go`'s `dlAwait*`). `harness.DefaultTimeout`'s doc already says it bounds ONE wait; a longer chain gets its own named, measured bound, as `HibernationChainTimeout` does.
    - Coverage impact: none; every wait asserts the same event.

## Changes that LOOK faster and are refused

- Skipping a test, or moving it behind a flag nobody sets: coverage loss.
- Weakening an assertion (checking less of the result, matching a prefix): coverage loss.
- In a UNIT test, sharing state that more than one case reads or changes: hides order-dependent failures. Integration cases share the canonical process and assert only on their own slice, per AGENTS.md "Integration tests share one canonical process, each case on its own slice".
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
