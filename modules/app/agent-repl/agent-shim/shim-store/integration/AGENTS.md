# shim-store/integration/

The store's integration suite. Two module-wide rules govern every test here (`modules/app/agent-repl/AGENTS.md`):

- "Tests NEVER run real git or a real vendor"
- "A system's integration tests run that system alone; every other system is a mock"

## What is real and what is a mock here

- Real: the `shim-store` binary, the one system under test.
- Every client (shim, sidecar, daemon) is the test itself speaking `store.v1` on the socket. No other agent-repl system is started here.
