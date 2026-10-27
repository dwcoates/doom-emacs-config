# daemon/integration/

The daemon's integration suite. Two module-wide rules govern every test here (`modules/app/agent-repl/AGENTS.md`):

- "Tests NEVER run real git or a real vendor"
- "A system's integration tests run that system alone; every other system is a mock"

## What is real and what is a mock here

- Real: `agent-repld`, the one system under test.
- Git: the fake git executable built from `fakegit/`, first on the daemon's `PATH`.
- Shim: the fake shim built from `fakeshim/`.
- Store and sidecar: never started. `harness.Opts.ServiceBinaries` stays empty, and the harness states the services' build reports itself.
- The store's read-only face: `fakestore/`, served by the harness on the read-only socket beside each daemon's store socket (`harness.serveFakeStore`), from the books the fake shims keep beside it. Every fake shim keeps its books there whenever they change and names its book (`main@<workspace>`) on every StartSession answer, so a workspace with no shim -- one at its cold gate -- shows its conversation as it does in production. A successor daemon carries its predecessor's store socket (`SuccessorOpts`), as the real store outlives every daemon.
- Vendor: never reached. The fake shim speaks for it.

A test that needs a real shim, store or sidecar belongs in `e2e/`.

## Every bound is granted time

Every failure bound the harness and the suite put on a wait is a `grantedclock.WithBound` context: it is charged only the time this binary's processes were not starved of CPU (on darwin, the process tree's `ri_runnable_time`; on linux, the container's `cpu.pressure`), so a saturated host never reads as the daemon's failure. See `harness/granted.go`. `harness.TestEveryWallBoundIsAllowedByName` fails a hand-rolled wall-clock bound unless it is allowed by name with its reason, as negative probe windows are.
