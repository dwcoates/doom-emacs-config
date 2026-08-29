# fakedaemon — the mocked NEIGHBOR for Emacs's integration suite

A Go program that speaks `agentrepl.v1` the way the real daemon does, so the
elisp integration suite can send input to a running Emacs-side stack and
assert on the output without any other real system in the picture.

It mocks Emacs's neighbor and **composes nothing**: no shim, no store, no
webapp, no vendor. `AGENT_REPL_FORBID_VENDOR_CALLS=1` is expected in its
environment, and nothing here can reach a vendor even without it.

## Why a real Connect server rather than an elisp stub

Every request the suite sends is unmarshalled into the **generated Go types**
and every scripted answer is marshalled out of them. That round trip is the
check on elisp's encoders and decoders, and it catches both halves of getting
the wire wrong:

- a **misspelled** field — the strict protojson codec (`strictjson.go`)
  refuses unknown fields, restoring the default `connect-go` discards;
- a **forgotten** field — `validate.go` enforces the contract's validation
  invariant (an unset non-optional message field, an unset non-synthetic
  oneof, or an `*_UNSPECIFIED` enum is `invalid_argument`, recursively).

## Process contract

- `AGENT_REPL_STATE_DIR` **must** be set; the fake refuses to start without
  it. There is no fallback state root — divergence is a loud misconfig.
- It binds `127.0.0.1:0`, then writes `127.0.0.1:<port>\n` to
  `$AGENT_REPL_STATE_DIR/daemon.addr` by atomic rename, and **removes** that
  file on orderly exit (before closing the listener, so a client re-reading
  the file never finds an address that is already refusing).
- One mux serves `agentrepl.v1` (JSON and binary codecs, HTTP/1.1 and h2c)
  and the `/_fake/` control plane on the same origin.
- Structured JSON logs, one record per line, to stderr.
- Two instances can run at once, which is how handover scenarios are staged:
  the second binds a fresh port and rewrites `daemon.addr`.
- `SIGTERM` / `SIGINT` also exit orderly.

## What the rpcs do

**Unary** — every method records its request (method plus the request's
protojson) in order, then answers:

1. from the **scripted table** when `/_fake/script` has set an answer for
   that method;
2. otherwise from a **reflective default**: the response's `result` oneof
   resolves to `success`, every non-optional message field reachable from
   there is filled with a legal empty value, and any oneof inside the success
   resolves to its first arm (so `DaemonHealth` answers `healthy` and
   `SubmitPrompt` answers `turn`). A default that left a non-optional field
   unset would make every client raise instead of exercising the path under
   test.

Three methods mint identity the generic rule cannot:

| method | default success |
|---|---|
| `RegisterWorkspace` | `{id: "ws-<sha of cleaned dir>", dir: <cleaned dir>}` — stable per directory, so re-registration is idempotent by dir |
| `CreateWorkspace` | a ref minted from the repository dir |
| `SubmitPrompt` | `turn.turn.value = "turn-<idempotency key>"` |

**Streams** — the three Emacs holds are implemented against a subscriber
registry: `WatchHostWorkspace` (keyed by workspace id), `WatchDaemon` and
`WatchWorkspaceRoster`. A subscription replays any stored snapshot and then
**stands** until the client cancels or the control plane ends it. Every other
`agentrepl.v1` stream belongs to the webapp; the fake answers those
`unimplemented` so a wrong caller fails loudly instead of hanging.

**Acceptance is the header block.** A standing stream is ACCEPTED the moment
its subscription is registered, and the response headers (status 200 and the
streaming content type) are flushed right then — before any snapshot, before
any push, and even when the stream will stay silent indefinitely. A client
treats header arrival as acceptance, and ends a watch only by killing its
transport.

`connect-go` does not do this on its own: it writes headers lazily, on the
handler's first `Send`. `accept.go` supplies the mechanism — a
`ResponseWriter` wrapper the mux installs on every request, which the stream
handlers call `accept` on once the subscriber is registered, and which
swallows `connect-go`'s later `WriteHeader` (logging a warning if that call
ever disagrees about the status). Unary calls are untouched: nothing calls
`accept`, so `connect-go`'s own `WriteHeader` is the first one through, and a
refusal keeps its own status.

Validation runs BEFORE registration, so an illegal stream request is refused
and never accepted.

## Control plane

All under `/_fake/`, on the same address. Control bodies are parsed with
unknown fields **disallowed**, so a typo in a helper is a loud 400 rather
than a silently ignored instruction.

| endpoint | body | effect |
|---|---|---|
| `POST /_fake/script` | `{method, response}` | Canned answer for one unary method. `response` is protojson, validated by unmarshalling into the generated response type. Unknown method → 400; a response the schema rejects → 400. |
| `POST /_fake/push` | `{stream, workspace_id?, message, snapshot?}` | Deliver `message` (protojson of the stream's response type) to every matching open subscriber. `snapshot: true` also stores it, so later subscribers receive it on subscribe. |
| `POST /_fake/end` | `{stream, workspace_id?, error?: {code, message}, abort?}` | End matching streams. With `error`, the end frame carries that Connect error. With `abort`, the TCP connection is dropped and **no end frame is written at all**. |
| `GET /_fake/calls` | — | The recorded requests in order: `[{method, body}]`, `body` being the request's protojson. `[]` when nothing has been called. |
| `GET /_fake/subscribers` | — | Open streams: `[{id, stream, workspace_id?}]`. |
| `POST /_fake/exit` | — | Answer, then exit orderly (removing `daemon.addr`). |

`stream` is one of `host`, `daemon`, `roster`. `workspace_id` is **required**
for `host` (that stream is keyed by workspace) and **refused** for the other
two (they are the workspace-independent channels by ruling).

`error.code` is a Connect code in snake_case (`unavailable`,
`invalid_argument`, `failed_precondition`, …). `abort` cannot carry an
`error`: it writes nothing.

## The scenario contract

A suite drives one instance like this:

1. Start the process with a **private** `AGENT_REPL_STATE_DIR`, then wait for
   `daemon.addr` to appear. Never poll with a sleep — the elisp helper waits
   through `accept-process-output` under a deadline.
2. Read the address from that file, exactly as production does.
3. `/_fake/script` any answer the scenario needs that is not the default (an
   error arm, an unhealthy verdict, a blocked close).
4. Open whatever streams the scenario needs from the Emacs side. The open
   completes on the header block alone, so it never blocks on a first push.
   Then poll `GET /_fake/subscribers` until the expected subscription is
   registered — a push before that would be delivered to nobody.
5. `/_fake/push` the pushes, in order. Ordering within one stream is the only
   ordering the contract provides.
6. Assert on `GET /_fake/calls` (what Emacs sent) and on Emacs's own state
   and production logs (what Emacs did about it).
7. `/_fake/end` when the scenario is about an ending; otherwise cancel from
   the Emacs side, which is the graceful close.
8. `/_fake/exit`, or kill the process, at teardown.

**Handover** scenarios run two instances: the second one's start rewrites
`daemon.addr`, the first announces `shutdown_announced{address}` naming the
second, and per-workspace `transferred` pushes come from the first while the
`AdoptHostWorkspace` calls land on the second — each instance's own
`/_fake/calls` says which daemon Emacs talked to.

## Build and test

Offline, from this directory:

```
GOFLAGS=-mod=mod GOPROXY=off go build ./...
GOFLAGS=-mod=mod GOPROXY=off go test ./...
```

Pins: `connectrpc.com/connect v1.17.0`, `golang.org/x/net v0.43.0`,
`google.golang.org/protobuf v1.36.11`, `go 1.23`, and
`replace agentrepl/proto => ../../../proto/gen/go`. connect v1.20.0 needs
Go 1.25 and does **not** build here.

The built binary is never tracked; `test-integration-helpers.el` builds it
once per run into a temp directory.
