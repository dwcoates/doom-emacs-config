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

## Every bound is granted time

Every failure bound the harness and the suite put on a wait is a `grantedclock.WithBound` context: it is charged only the time this binary's processes were not starved of CPU (on darwin, the process tree's `ri_runnable_time`; on linux, the container's `cpu.pressure`), so a saturated host never reads as the daemon's failure. See `harness/granted.go`. `harness.TestEveryWallBoundIsAllowedByName` fails a hand-rolled wall-clock bound unless it is allowed by name with its reason, as negative probe windows are.
