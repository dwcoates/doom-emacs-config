# agent-shim/logging/

The parts of the agent-repl logging contract that every runtime must answer
identically, owned once instead of copied per runtime.

The daemon, the store, the sidecar, the shim and the webapp write to separate
sinks, but their records are read together. Anything whose divergence would
make those records incomparable belongs here. Anything a single runtime is free
to decide for itself does not.

## Layout

- `go/` — module `agentrepl/logging`, imported by `daemon/`,
  `agent-shim/shim-store/` and `agent-shim/claude/shim-sidecar/` through a
  `replace` directive, exactly as `agentrepl/wire` is.
- `ts/` — plain sources compiled by both `agent-shim/claude/shim/` and
  `webapp/` through a relative import. There is deliberately no package of its
  own: the two consumers already span `agent-repl/` in their build programs,
  and a third npm install surface would buy nothing.

Emacs is the sixth runtime and cannot import either. Its copy is the single
`agent-repl--log-timestamp-format` defconst in `core.el`.

## The cross-language seam

Three languages cannot compile one source, so `proto/vocab/log-timestamp.json`
is where the three are held to the same answer. `proto/vocab/log-level-window.json`
does the same for the log level window (`go/window.go`, `ts/level-window.ts`
and `agent-repl--log-level-window` in `core.el`): a level other than info holds
only inside an unexpired window of at most five minutes, so a leftover setting
never outlives it. Go asserts from `go/window_test.go`, TypeScript from
`agent-shim/claude/shim/test/log-level-window.test.ts`, and elisp from
`test-core.el`. Go asserts against it from
`go/timestamp_test.go`, TypeScript from
`agent-shim/claude/shim/test/log-timestamp.test.ts`, and elisp from
`test-log-timestamp.el`. This mirrors `proto/vocab/render-colors.json`, which
does the same job for the six-color contract.

`proto/vocab/profile-session.json` does the same for runtime profiling
(`go/profiling`, `ts/profile-session.ts` and `profiling.el`): one profiles
root, the `active` file naming the open session, the `armed/` marker of each
signaled runtime, SIGWINCH, and the 30-minute window after which a runtime
stops by itself. Go asserts from `go/profiling/profiling_test.go`,
TypeScript from `agent-shim/claude/shim/test/profile-session.test.ts`, and
elisp from `test-profiling.el`; `bin/profile-agent-repl.sh` is the collector.

Changing the representation means changing the fixture and all three corners
together. Changing one corner alone fails that corner's test rather than
producing a log nobody can interleave.

## Adding to this module

The bar is that at least two runtimes must answer the question and their
answers must be required to match. A helper only one runtime calls belongs in
that runtime. A convention every runtime happens to follow but whose divergence
is harmless belongs in `logging-contract.md` as prose, not here as code.

New Go packages here are registered as the `logging` component in
`bin/report-nonlisp-coverage.sh`, `bin/report-logging-density.sh` and
`bin/test-all.sh`, and as a service prerequisite in `bin/build-frontend.sh` and
`bin/readiness-report.sh`.

Read the records produced through this shared contract with `../../bin/logs.sh`;
the full path, rotation, attribution, and level-switch table is in
`../../AGENTS.md`.

## Performance lessons (living document)

THIS SECTION IS A LIVING DOCUMENT. Whenever an important performance lesson
about the shared logging libraries is found -- at runtime or in its tests -- add or update an entry
here: what was slow or heavy, the evidence with numbers and the date, the fix
and its commit (or the open item). Test-performance lessons also go into the
`/agent-repl-tests` skill; runtime profiling and the perf-sensitive list live in
`/agent-repl-profile`.

- Every record costs a marshal and a write; per-render or per-event repeats at debug dominate test volume (see the daemon's lesson on log-on-change). Prefer logging a decision when it changes over lowering its level.
