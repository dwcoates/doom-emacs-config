# e2e/

The end-to-end suite: the one place several real agent-repl systems (daemon, shim, store, sidecar, webapp layer) run together. The module-wide rule "A system's integration tests run that system alone" (`modules/app/agent-repl/AGENTS.md`) is what this suite is exempt from, and nothing else here is.

## Git and the vendor are always mocks

The module-wide rule "Tests NEVER run real git or a real vendor" applies here in full, including to harness bookkeeping such as reading a commit for a perf baseline.

- Git: the fake git executable (`daemon/integration/fakegit`, `bin/fake-git.sh`). No test, helper or harness in this tree runs the host's `git`.
- Vendor: the shim's mocked SDK (`agent-shim/claude/shim/src/fake/`).

## Every world-building test is a step of a scenario

- `scenarios_test.go` lists the suite's scenarios: the only top-level tests that build a world (`NewWorld`, `NewWorldWithSidecarStaleness`, `SharedWorld`, `GroupWorld`, `LifecycleWorld`, `ScenarioLifecycleWorld`).
- A scenario's body is one `scenario(t, steps...)` call (`scenario_test.go`, over `kit/scenario`).
  - The scenario runs in parallel with the other scenarios.
  - Its steps run in order, each a function `testXxx(t *testing.T)` reported as the case `Scenario/Xxx`.
  - A step builds or takes its own world and owns its own workspaces, under `modules/app/agent-repl/AGENTS.md` "Integration tests share one canonical process, each case on its own slice".
- `TestEveryStepRunsInExactlyOneScenario` and `TestEveryWorldBuildingTestIsAScenario` (`kit/scans/inventory_unit_test.go`) fail a step no scenario lists and a world-building test outside a scenario.
- Every scenario and every step names the seam it proves on a line of its own in its doc comment: `Seam: <family>` for a family of `testdata/scenario-goldens` (whose wiring test it is), or `Seam: <producer> -> <consumer>, <what crosses>` (`modules/app/agent-repl/AGENTS.md`, "An e2e test proves a seam between real systems, and nothing else"). `TestEveryScenarioAndStepNamesItsSeam` fails one that names none or a line in neither form; `seamClaims` (`kit/scans/inventory_test.go`) is the one reader of the line, the scenario matrix's included. Exempt, with why, is the one scenario in `seamExempt`: `TestHarness` (the harness's own World machinery).
- A step reruns alone by its path: `-go e2e=TestTurns/ContextUsage`.
- Where new e2e coverage goes is the `agent-repl-tests` skill's rule ("E2E coverage is a step of a scenario").
- A step retired into a system's own tier, whose scenario no other e2e test drives, is pinned in `SCENARIO-MATRIX.md` section (f) with its replacement tests in the runner's selection syntax (`-go DIR=TestName`, `-vitest FILE=suite test`), one in the shim's tier and one in the daemon's. The matrix check (`kit/scans/retired_test.go`) fails a pin whose test does not exist, and counts the scenario covered only while both halves' pins stand. A step whose replacements sit in one system's tier is pinned in section (g) instead, the same way: its pins must exist, and it counts nothing covered.

## Scenario steps share one World per precondition

- A step takes `stack.SharedWorld(t)` and registers its own fresh workspace (`w.NewWorkspace(t)`, `w.NewRepoWorkspace(t)`); its slice is that workspace: its feed, its session, its shim and its per-workspace logs.
  - It asserts only on its slice: a glob of the account root narrows to its own project directory (`harness.ProjectDir(w.DefaultConfigDir, ws.GetDir())`), never every project's.
  - A warning it provokes is declared for its workspace, `w.ExpectWarningsFor(ws, ops...)`, after the workspace exists.
- A step that needs other `WorldOpts` (a daemon option such as a keep-alive interval or an idle cutoff, a sidecar window, a real zettel) takes the group World for them, `stack.GroupWorld(t, "<group>", opts)`: one World per group per test process, shared by every step that asks for the group, built with the same options (an ask under others fails the step). The groups today are `keepalive` (`keepaliveWorld`) and `hibernation`.
- A step whose state is the whole account's or the whole daemon's (the weekly allowance its rate limits move) takes a group World named for that state (`weekly-allowance`), so other steps' cases cannot move it.
- A step whose subject is the DAEMON's own lifecycle (it deploys, relaunches, stops or cold-boots it) takes `stack.ScenarioLifecycleWorld(t, daemonOpts)`: a fresh daemon, account roots and sidecar of its own, with its own options, on its scenario's lifecycle family's one shared STORE (`kit/stack/lifecycle.go`). The store is not its subject, so it is built once per scenario; the steps of a scenario run in order, so a family's store never serves two of its steps at once.
  - Each daemon's state root sits under the family's parent, so its lock dir is the family's: the store reports its build there once, and each step's sidecar reports there as it starts.
  - The sidecar is never shared: it reads the shim identity records under ONE daemon's state root, so a sidecar of another root would book a rotated transcript under a second id.
  - A cold boot or relaunch inside the step starts its successor through `w.SuccessorOpts(t)` as before.
- Only a step that stops or restarts the STORE (its lifecycle is the subject, such as the store's shutdown record or a real store outage), or whose precondition is its own alone (a world option or environment value only it sets), builds its own World with `stack.NewWorld`, and its doc comment says why.
- The shared and group Worlds' store and sidecar are shared: stopping or restarting them from a step fails it.
- The rule is `modules/app/agent-repl/AGENTS.md`, "Integration tests share one canonical process, each case on its own slice".

## The harness lives in kit/, and its own tests run there

- The e2e suite is never reused (`testrun/roster/roster.go`), so a test in it runs in every run that selects it. A test that is not a scenario step does not belong in it.
- `kit/` is its own Go module (`agentrepl/e2e/kit`, replaced into this one) and its own roster suite, `e2e-kit`, reused per package on the inputs each reads:
  - `kit/stack`: the world every scenario builds (`stack.World`, `stack.SharedWorld`, the webapp layer's driver, the suite's layout and programs, `stack.RunSuite`), with its own tests. Its unit takes the run's programs and declares the trees the layout checks.
  - `kit/scenario`: the scenario runner `scenario` calls.
  - `kit/scans`: every scan of this package's sources and of `kit/stack`'s (the scenario inventory and seam lints, the scenario matrix, and the lints pinning each shared helper as the one place its shape is spelled). It runs in this directory and declares `e2e/` as its input.
  - `kit`: harness helpers that build nothing, with their tests.
- A new harness test goes in `kit/`, beside its subject. It stays here only when it needs a world of real systems (the `TestHarness` steps): a world spawns the real shim, whose installed `node_modules` and built bundle the e2e suite's prebuild provides and a confined `e2e-kit` unit cannot read.
- The grep gate (`main_test.go`) and `TestNoE2EWaitIsBoundOnWallTime` hold `kit/`'s code to the same rules as this package's.

## The Emacs e2e layer was removed (2026-11-13)

Owner decision, 2026-11-13, taken as a trial: the owner reviews it on 2026-11-20 and may restore the layer.

- What was removed:
  - the `e2e-emacs` gate suite (`bin/test-e2e-emacs.sh`, `bin/lib-sandbox-suite.sh`) and the runner machinery only it used (`roster.Suite.GoTests` with its `-harness SCRIPT=TestA` selection, `roster.Suite.NeedsWebappDist` with the run's one dist build);
  - every test it ran: the `TestEmacsWorkspaces` and `TestEmacsDaemonLifecycle` scenarios (7 Emacs seam steps), the `TestDoom` tier (6 steps against the fake daemon) and the `TestEmacsTeardown*` tests;
  - the Emacs world in `kit/stack` (`EmacsWorld`, `StartEmacs`, the sandbox and its Xvfb display, the emacsclient helpers, the Doom stamp waits, the trampoline prewarm and scenario-override checks) and the kit tests of only that machinery;
  - the Docker sandbox (`e2e/sandbox/`), its script tests (`e2e/sandboxscripts/`), `EMACS-LAYER-SPEC.md`, the `docker` requirement, and the scenario matrix's Emacs column.
- Why: the layer made agent-repl require Docker, and its container and Emacsen cost the gate CPU for 7 Emacs seam steps, the Doom tier and the teardown tests. Emacs's own behavior is covered by ERT against the fake daemon (`lisp/testsupport/fakedaemon`); this suite keeps the cross-system seams between the daemon, shim, store, sidecar and webapp. Nothing stays behind a flag: a test either runs in the gate or is deleted.
- To restore it, revert the removal commits, newest first: `git revert a0733f5eee 06d7d45312 9eeda8a87b` (the guides, then the e2e layer and the sandbox, then the suite and its runner machinery), and rebuild the sandbox image. Those are the hashes the commits had on the branch `merge-queue-stall-triage`; if a landing rewrote them, find them by their subjects (`testrun: the e2e-emacs suite leaves the roster…`, `e2e: remove the Emacs e2e layer…`, `docs: the Emacs e2e layer…`).
