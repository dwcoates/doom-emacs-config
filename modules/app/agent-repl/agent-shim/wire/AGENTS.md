# agent-shim/wire/

The shared Go framing layer for every agent-shim UDS hop, in two layers:

1. **Raw frames** — `WriteFrame` / `ReadFrame`: a 4-byte big-endian length
   prefix followed by that many bytes of opaque payload.
2. **The Any envelope** — `WriteAny` / `ReadAny` (and their halves
   `MarshalAny` / `UnmarshalAny`): the frame payload is a serialized
   `google.protobuf.Any` whose `type_url` is THE message discriminator,
   resolved against the proto global registry. `core.proto` has no top-level
   frame oneof, so the Any *is* the type tag.

One owner so shim-store, shim-claude-sidecar, and the daemon cannot drift on
either layer. Oversized or truncated frames are protocol violations surfaced
as loud errors, never absorbed. `ReadAny` returns `ReadFrame`'s error
**verbatim** — every call site classifies a clean `io.EOF` close apart from a
fault, so wrapping it here would be a behavior change.

`any_test.go` pins the envelope's byte-level behavior against a hand-rolled
reference encoder, because layer 2 is a wire contract with live peers (a
running shim, a running store) and not merely an internal helper.

Dependencies: `google.golang.org/protobuf` (layer 2 only; layer 1 is stdlib).
The TypeScript twin lives in `agent-shim/claude/shim/src/uds/framing.ts`
(necessarily separate language).

## Logging

- `wire` is a pure framing library, not a runtime, and owns no logging sink.
  It returns contextual errors to its caller. The daemon, sidecar, or store
  using it records those errors exactly once through that runtime's canonical
  normal or verbose JSON logging API.
- New or changed framing functions must return enough operation, frame, and
  branch context for the owning runtime to diagnose the failure. Error-path
  tests assert that context and the runtime-level tests assert the canonical
  log record.
- Direct diagnostic output through `fmt`, `log`, `slog`, or an ad hoc logger is
  forbidden.

## Verification and coverage

- Run `make coverage` after every wire Go change. It tests every local package
  with `-coverpkg=./...` and prints per-function coverage plus total statement
  coverage. The command must pass.
- Before handoff, run `modules/app/agent-repl/bin/test-all.sh` from the
  repository root. Every tracked suite must pass.
- Maintain at least 90% statement coverage. Never reduce the measured baseline,
  and add focused tests for every critical branch and every error path changed.
- `modules/app/agent-repl/bin/report-logging-density.sh wire` should report zero
  direct logging calls. Any nonzero result is an abstraction-boundary review
  trigger.
- After a commit lands on `master`, run
  `modules/app/agent-repl/bin/test-all.sh --record`, inspect
  `modules/app/agent-repl/test_time.csv`, and surface every reported timing
  regression.
