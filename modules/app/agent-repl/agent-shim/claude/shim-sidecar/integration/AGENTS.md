# shim-sidecar/integration/

The sidecar's integration suite. Two module-wide rules govern every test here (`modules/app/agent-repl/AGENTS.md`):

- "Tests NEVER run real git or a real vendor"
- "A system's integration tests run that system alone; every other system is a mock"

## What is real and what is a mock here

- Real: the sidecar binary, the one system under test.
- Store: the in-process fake store (`startFakeStore` in `helpers_test.go`), which keeps what the store's contract makes durable with a batch (cursors, shell run claims), validates writes with the store's own `protohelpers/storewrite`, and answers each agent's book as the sidecar wrote it. The real `shim-store` binary is never started here; what the store does with the sidecar's writes (paging, watches, restamps, identity refusals) is tested in `shim-store`'s suite.
- Vendor: never reached. Vendor files are built from `testdata/` fixtures and grown by the test itself, or written by the mocked vendor below.
- The mocked vendor (`mock_*_test.go`): the shim's own fake SDK mode, which is the vendor's mock, run as the shim bundle against the shim package's fake store (`test/fakes/store-server-main.ts`), never against the real store. It is why a shim change selects this suite (`daemon/internal/merge/suiteselect.go`).

A test that needs the real store to say anything useful belongs in `e2e/`.
