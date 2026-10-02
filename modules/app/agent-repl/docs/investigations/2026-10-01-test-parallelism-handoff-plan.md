# Test parallelism overhaul: handoff plan (2026-10-01)

Branch: `test-perf` (worktree `~/.config/doom-worktrees/test-perf`).
The tip is a `wip(testrun)` commit holding the unvalidated Go splitter.
Every git command, commit included, is allowed. Master moves only through `/merge-queue`.

## Goal (owner's model, settled)

- Slots = `runtime.NumCPU() - 2`, on any machine.
  - `testrun/internal/cli/run.go` `SlotsForHost` already does this.
  - Don't special-case hosts with 2 or fewer cores, and don't ask about them.
- Each unit runs on one core and the scheduler owns all parallelism.
- Work is spread evenly across the slots.
  - A suite may be split into chunks, or several suites may run one after another in a single slot, whichever balances better.
  - The planner (`testrun/internal/sched/plan.go` `PlanRun`) decides using host timing history in `~/.cache/agent-repl/test-history.json`.

## What is already built (do not redo)

- `modules/app/agent-repl/testrun/` is a Go module, `agentrepl/testrun`.
  - `internal/sched` has the DAG queue, longest-chain-first list scheduling, the simulator, and the chunk planner (least work within 2% of the best makespan).
  - `internal/history` stores EWMA timings, flock-guarded.
  - `internal/run` has the runner, the process-group executor, exit 77 handling for declined suites, and bracketed per-unit output.
  - `internal/suites` builds units for each roster kind (Script, ERT, GoModule, Vitest, E2E), defined in `roster/roster.go`.
  - `internal/plan` estimates units, `internal/cli` handles args, the timing CSV and regression report, and `internal/cover` writes the Go coverage report.
- `bin/test-all.sh` is a thin wrapper: `suite-slot.sh` → `.claude/safe-test-run.sh --` → `testrun run`.
- Every unit is pinned with `GOFLAGS=-p=1` and `GOMAXPROCS=2`.
  - Go chunks also pass `-test.parallel=1`, and vitest chunks pass `--maxWorkers=1`.
  - `GOMAXPROCS=1` broke e2e waits, so leave it at 2.
- Binaries are built once and shared through `AGENT_REPL_TEST_PREBUILD` and `AGENT_REPL_TEST_PREBUILT` (`daemon/integration/harness/prebuilt.go`, `e2e/main_test.go`).
- The merge gate (`daemon/internal/merge/testgate.go`, `suiteselect.go`) parses the runner's output and takes its suite list from `roster.Names()`.
- Measured so far: 185s for a full run with `GOMAXPROCS=1` pinning, versus 396–480s for the old serial script.

## Remaining work, in order

### 1. Finish the generic Go package splitter (WIP commit at tip)

- Files: `testrun/internal/suites/gopkg.go`, `gomod.go`, `e2e.go`, `internal/run/runner.go` (the `Display` field), and `internal/plan/plan.go` (unknown items fall back to the group mean, then the suite mean).
- Fix the known bug first:
  - `e2e.go` sets `AGENT_REPL_E2E_PREBUILD` and `AGENT_REPL_E2E_PREBUILT`.
  - The harness reads `AGENT_REPL_TEST_PREBUILD` and `AGENT_REPL_TEST_PREBUILT`.
  - Use one shared constant for these names rather than redeclaring the strings in `testrun`. Neither copy should be able to drift.
- Then run it for real, under `bin/background.sh`:
  ```bash
  bin/test-all.sh --suites testrun,lock,logging,store,sidecar,daemon,e2e
  ```
- Check:
  - Every package's tests run and report item timings.
  - The coverage report from `cover-report` matches the old per-module shape.
  - The vet unit catches what `go test` used to vet.
- Split the WIP commit into proper atomic commits, each with its tests.

### 2. Coverage only on demand (owner decision)

- By default, `test-all` runs no coverage for any suite.
  - This covers vitest (webapp, shim) and Go `-cover`/`cover-report`.
- Add an explicit flag, e.g. `--coverage`, to `testrun/internal/cli/args.go` `ParseArgs`, passed through by `bin/test-all.sh`.
  - Without the flag, vitest chunks run `vitest run` with no coverage, there is no merge unit, and Go builds skip `-cover` with no report unit.
  - With the flag, the run behaves as it does today, including the vitest blob merge and the single `all: true` chunk.
- The merge gate's invocation should not pass `--coverage`, unless a gate-side coverage check depends on it. Check `daemon/internal/merge` for one first.
- Update `modules/app/agent-repl/AGENTS.md`:
  - Coverage runs are expensive and run only on demand.
  - Run them conservatively, only when coverage is the question being asked, never as a routine step.
- Add unit tests for both paths: the unit graph with and without the flag.

### 3. Remove the proto build's network fetch

- `proto/Makefile:89-91` currently fetches `@bufbuild/protoc-gen-es@$(PROTOC_GEN_ES_VERSION)` through `npx --yes`.
  - Under load, this once stalled a run for 247s.
- Make it a pinned local dev dependency installed by `npm ci` (a `proto/package.json` and lockfile, or the existing webapp deps if proto already depends on them).
  - The Makefile should invoke `./node_modules/.bin/protoc-gen-es`.
  - Drop the `npx` and the `|| echo` fallback. A missing binary should fail hard.
- Update `proto/AGENTS.md:126-139` and `proto/scripts/test-check-generated.sh`.
- Generated output must not change. Regenerate and confirm `git diff` is empty.

### 4. Split the slowest bash harnesses

- `build-frontend-harness` takes 150–167s under load but 40s alone. `readiness-harness` takes 118–150s under load but 25s alone.
- Give the bash harnesses a shared split protocol: `--list` prints test names and `--only NAMES` runs a subset, emitting the per-item timing lines a `Split` parses.
  - Put this in one helper library sourced by every harness, not per-harness copies.
- Add a roster kind, or extend Script, so these harnesses become `Split`s.

### 5. Make the full run reliably green

- These failures appear only under full load:
  - e2e `TestSubagentInterleavedResponsesStayOnTheirOwnFeeds`, `TestArtifactPublishAndList` and `TestIdeDiagnosticsAfterEdit`.
  - sidecar `TestMockScenarios/!push-no-transport`.
  - webapp `vite-config.test.ts` hook timeout.
- Find the root cause of each and fix it at the source.
  - Production code first. Change the test only when the failure is a true test artifact.
  - No `time.Sleep`, no retries, and no blanket timeout increases.
- `sidecar:integration` takes 150s under load but only 49s of test time. Its `TestMain` builds binaries, which should move to the shared prebuild.

### 6. Merge-gate gap

- `daemon/internal/merge/testgate.go` doesn't parse `<suite>: DECLINED after …` lines, so a declined suite stays "running" in the tests tab. Parse it and add a test.

### 7. Docs, sweep and land

- Update `modules/app/agent-repl/AGENTS.md` with the testrun model: slots, pinning, history file, `--suites`, `--coverage`.
  - Also update the parallelism section of `e2e/SPEC.md`.
- Sweep for duplicated code across the whole branch diff against master (`git diff master...`).
  - Extract each helper as its own behavior-preserving commit, with tests.
- Run the entire `bin/test-all.sh` until it's green, then land through `/merge-queue`.

## Constraints that apply throughout

- Run tests under `bin/background.sh`.
- No real git in any test.
- Don't put CPU load on the machine except the test runs themselves.
- Every new or changed function gets table-driven Arrange/Act/Assert tests, including its error paths.
- Every tracked test failure is yours to fix. Never call one pre-existing.
- Never swallow an error or add a fallback for an invariant.
- Write memory only after the owner approves the change.
