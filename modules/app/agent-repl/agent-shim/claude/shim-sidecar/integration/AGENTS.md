# shim-sidecar/integration/

The sidecar's integration suite. Two module-wide rules govern every test here (`modules/app/agent-repl/AGENTS.md`):

- "Tests NEVER run real git or a real vendor"
- "A system's integration tests run that system alone; every other system is a mock"

## What is real and what is a mock here

- Real: the sidecar binary, the one system under test.
- Store: the in-process fake store (`startFakeStore` in `helpers_test.go`). The real `shim-store` binary is never started here.
- Vendor: never reached. Vendor files are built from `testdata/` fixtures and grown by the test itself.

A test that needs the real store to say anything useful belongs in `e2e/`.
