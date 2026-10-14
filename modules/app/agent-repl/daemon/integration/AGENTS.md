# daemon/integration/

The daemon's integration suite. Two module-wide rules govern every test here (`modules/app/agent-repl/AGENTS.md`):

- "Tests NEVER run real git or a real vendor"
- "A system's integration tests run that system alone; every other system is a mock"

## What is real and what is a mock here

- Real: `agent-repld`, the one system under test.
- Git: the fake git executable built from `fakegit/`, first on the daemon's `PATH`.
- Shim: the fake shim built from `fakeshim/`.
- Store and sidecar: never started. `harness.Opts.ServiceBinaries` stays empty, and the harness states the services' build reports itself.
- Vendor: never reached. The fake shim speaks for it.

A test that needs a real shim, store or sidecar belongs in `e2e/`.
