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
