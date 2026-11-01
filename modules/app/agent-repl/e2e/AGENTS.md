# e2e/

The end-to-end suite: the one place several real agent-repl systems (daemon, shim, store, sidecar, webapp layer) run together. The module-wide rule "A system's integration tests run that system alone" (`modules/app/agent-repl/AGENTS.md`) is what this suite is exempt from, and nothing else here is.

## Git and the vendor are always mocks

The module-wide rule "Tests NEVER run real git or a real vendor" applies here in full, including to harness bookkeeping such as reading a commit for a perf baseline.

- Git: the fake git executable (`daemon/integration/fakegit`, `bin/fake-git.sh`). No test, helper or harness in this tree runs the host's `git`.
- Vendor: the shim's mocked SDK (`agent-shim/claude/shim/src/fake/`).

## Every world-building test is a step of a scenario

- `scenarios_test.go` lists the suite's scenarios: the only top-level tests that build a world (`NewWorld`, `SharedWorld`, `NewEmacsWorld`, `StartEmacs`, `NewDoomWorld`).
- A scenario's body is one `scenario(t, steps...)` call (`scenario_test.go`).
  - The scenario runs in parallel with the other scenarios.
  - Its steps run in order, each a function `testXxx(t *testing.T)` reported as the case `Scenario/Xxx`.
  - A step builds or takes its own world and owns its own workspaces, under the shared-fixture rule in `modules/app/agent-repl/AGENTS.md`.
- `TestEveryStepRunsInExactlyOneScenario` and `TestEveryWorldBuildingTestIsAScenario` (`scenario_unit_test.go`) fail a step no scenario lists and a world-building test outside a scenario.
- A step reruns alone by its path: `-go e2e=TestTurns/ContextUsage`, or `-harness bin/test-e2e-emacs.sh=TestEmacsLayout/EmacsFullscreenTogglesAndRestores` for the Emacs layer and the Doom tier.
- Where new e2e coverage goes is the `agent-repl-tests` skill's rule ("E2E coverage is a step of a scenario").
