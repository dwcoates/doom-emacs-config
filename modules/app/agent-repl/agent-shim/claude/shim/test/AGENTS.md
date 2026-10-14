# shim/test/

The shim's unit (`test/`) and integration (`test/integration/`) suites. Two module-wide rules govern every test here (`modules/app/agent-repl/AGENTS.md`):

- "Tests NEVER run real git or a real vendor"
- "A system's integration tests run that system alone; every other system is a mock"

## What is real and what is a mock here

- Real: the shim, the one system under test.
- Vendor: the mocked SDK (`src/fake/`).
- Store: the in-process fake store (`test/fakes/store-server.ts`).
- Daemon: the test itself spawns the shim bundle the way the daemon does and speaks for the daemon. No daemon is started here.
- Git: no test here runs git. One that ever needs it uses the fake git executable (`bin/fake-git.sh`).
