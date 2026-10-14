# webapp/test/

The webapp's unit (`test/`) and integration (`test/integration/`) suites. Two module-wide rules govern every test here (`modules/app/agent-repl/AGENTS.md`):

- "Tests NEVER run real git or a real vendor"
- "A system's integration tests run that system alone; every other system is a mock"

## What is real and what is a mock here

- Real: the webapp, the one system under test.
- Daemon: the fake daemon (`test/integration/fake-daemon.ts`). No real `agent-repld` is started by these suites.
- `test/webapp-layer/` is the exception by design: it runs only under `e2e/`, against the real daemon the e2e suite starts, and is never part of the webapp's own suite.
