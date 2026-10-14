# e2e/

The end-to-end suite: the one place several real agent-repl systems (daemon, shim, store, sidecar, webapp layer) run together. The module-wide rule "A system's integration tests run that system alone" (`modules/app/agent-repl/AGENTS.md`) is what this suite is exempt from, and nothing else here is.

## Git and the vendor are always mocks

The module-wide rule "Tests NEVER run real git or a real vendor" applies here in full, including to harness bookkeeping such as reading a commit for a perf baseline.

- Git: the fake git executable (`daemon/integration/fakegit`, `bin/fake-git.sh`). No test, helper or harness in this tree runs the host's `git`.
- Vendor: the shim's mocked SDK (`agent-shim/claude/shim/src/fake/`).
