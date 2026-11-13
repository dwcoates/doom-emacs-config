# e2e/

The end-to-end suite: the one place several real agent-repl systems (daemon, shim, store, sidecar, webapp layer) run together. The module-wide rule "A system's integration tests run that system alone" (`modules/app/agent-repl/AGENTS.md`) is what this suite is exempt from, and nothing else here is.

## Git and the vendor are always mocks

The module-wide rule "Tests NEVER run real git or a real vendor" applies here in full, including to harness bookkeeping such as reading a commit for a perf baseline.

- Git: the fake git executable (`daemon/integration/fakegit`, `bin/fake-git.sh`). No test, helper or harness in this tree runs the host's `git`.
- Vendor: the shim's mocked SDK (`agent-shim/claude/shim/src/fake/`).

## Every world-building test is a step of a scenario

- `scenarios_test.go` lists the suite's scenarios: the only top-level tests that build a world (`NewWorld`, `SharedWorld`, `NewEmacsWorld`, `StartEmacs`, `NewDoomWorld`).
- A scenario's body is one `scenario(t, steps...)` call (`scenario_test.go`, over `kit/scenario`).
  - The scenario runs in parallel with the other scenarios.
  - Its steps run in order, each a function `testXxx(t *testing.T)` reported as the case `Scenario/Xxx`.
  - A step builds or takes its own world and owns its own workspaces, under the shared-fixture rule in `modules/app/agent-repl/AGENTS.md`.
- `TestEveryStepRunsInExactlyOneScenario` and `TestEveryWorldBuildingTestIsAScenario` (`kit/scans/inventory_unit_test.go`) fail a step no scenario lists and a world-building test outside a scenario.
- Every scenario and every step names the seam it proves on a line of its own in its doc comment: `Seam: <family>` for a family of `testdata/scenario-goldens` (whose wiring test it is), or `Seam: <producer> -> <consumer>, <what crosses>` (`modules/app/agent-repl/AGENTS.md`, "An e2e test proves a seam between real systems, and nothing else"). `TestEveryScenarioAndStepNamesItsSeam` fails one that names none or a line in neither form; `seamClaims` (`kit/scans/inventory_test.go`) is the one reader of the line, the scenario matrix's included. Exempt, with why, are the scenarios in `seamExempt`: `TestHarness` (the harness's own World machinery) and `TestDoom` (the Doom tier).
- A step reruns alone by its path: `-go e2e=TestTurns/ContextUsage`, or `-harness bin/test-e2e-emacs.sh=TestEmacsWorkspaces/EmacsProofOfLife` for the Emacs layer and the Doom tier.
- Where new e2e coverage goes is the `agent-repl-tests` skill's rule ("E2E coverage is a step of a scenario").
- A step retired into a system's own tier, whose scenario no other e2e test drives, is pinned in `SCENARIO-MATRIX.md` section (f) with its replacement tests in the runner's selection syntax (`-go DIR=TestName`, `-vitest FILE=suite test`), one in the shim's tier and one in the daemon's. The matrix check (`kit/scans/retired_test.go`) fails a pin whose test does not exist, and counts the scenario covered only while both halves' pins stand.

## The harness lives in kit/, and its own tests run there

- The e2e suite is never reused (`testrun/roster/roster.go`), so a test in it runs in every run that selects it. A test that is not a scenario step does not belong in it.
- `kit/` is its own Go module (`agentrepl/e2e/kit`, replaced into this one) and its own roster suite, `e2e-kit`, reused per package on the inputs each reads:
  - `kit/stack`: the world every scenario builds (`stack.World`, `stack.SharedWorld`, `stack.EmacsWorld`, the Emacs sandbox, the webapp layer's driver, the suite's layout and programs, `stack.RunSuite`), with its own tests. Its unit takes the run's programs and declares the trees the layout checks.
  - `kit/scenario`: the scenario runner `scenario` calls.
  - `kit/scans`: every scan of this package's sources and of `kit/stack`'s (the scenario inventory and seam lints, the scenario matrix, and the lints pinning each shared helper as the one place its shape is spelled). It runs in this directory and declares `e2e/` as its input.
  - `kit`: harness helpers that build nothing, with their tests.
- A new harness test goes in `kit/`, beside its subject. It stays here only when it needs what only this suite's runs give it:
  - a world of real systems (the `TestHarness` steps): a world spawns the real shim, whose installed `node_modules` and built bundle the e2e suite's prebuild provides and a confined `e2e-kit` unit cannot read;
  - the `e2e-emacs` container (`TestEmacsTeardown*`, which read Linux's `/proc`): that suite runs this package's `TestEmacs*` tests in the container.
- The grep gate (`main_test.go`) and `TestNoE2EWaitIsBoundOnWallTime` hold `kit/`'s code to the same rules as this package's.
