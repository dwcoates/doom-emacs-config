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
is where the three are held to the same answer. Go asserts against it from
`go/timestamp_test.go`, TypeScript from
`agent-shim/claude/shim/test/log-timestamp.test.ts`, and elisp from
`test-log-timestamp.el`. This mirrors `proto/vocab/render-colors.json`, which
does the same job for the six-color contract.

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
