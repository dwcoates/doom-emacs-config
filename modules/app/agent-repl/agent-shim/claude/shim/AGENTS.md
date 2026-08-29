# agent-shim/claude/shim/

The per-session Claude shim: one TypeScript/Node process per workspace session,
spawned by the daemon, driving the vendor's agent binary through the Claude
Agent SDK.

## The six surfaces, and which direction each faces

| Surface | Direction | Where the contract lives |
| --- | --- | --- |
| `shim.v1` | the shim **SERVES** it to the daemon | `proto/src/shim/v1/` |
| `store.v1` | the shim **WRITES** and **READS** it | `proto/src/store/v1/` |
| `conversation.v1` | the shim **PRODUCES** it into the store | `proto/src/conversation/v1/` |
| the Claude Agent SDK | the shim **DRIVES** it | `node_modules/@anthropic-ai/claude-agent-sdk/sdk.d.ts` |
| the vendor's files | the shim's transcript backup **READS** them; `--fake` **WRITES** them | `docs/overhaul/shim.md`, mock section |
| the kernel locks | the shim **HOLDS** them; the daemon **PROBES** them | `~/.cache/agent-repl/run/` (`$AGENT_REPL_LOCK_DIR`) |

Everything the shim says on the wire is one of the first three. Nothing
vendor-shaped leaves this process: the vendor's uuids, message ids and record
shapes stay inside it, and what crosses the boundary is `conversation.v1`.

## What the shim is

- **The vendor adapter.** One shim per session. It converts the SDK's flat
  message log into `conversation.v1` units with identity, upserted whole.
- **Stateless.** It accumulates nothing of variable size. History is served
  FROM THE STORE, never from memory; the joins it keeps are constant-size (the
  pending permission callbacks, the live task table, spawn provenance).
- **Longer-lived than its daemon.** A daemon disconnect does not end a turn, and
  a daemon death does not end the process. That is why the log sink is an
  inherited descriptor rather than a pipe to the daemon's stderr, and why
  SIGTERM exists as a teardown path at all.

## Module map

```
src/
  main.ts              argv, env, the workspace lock, the log fd, signals, --version, wiring
  build-identity.ts    SHIM_BUILD_SHA + sdk/agent-binary versions (SessionRuntime)
  log.ts               THE canonical JSONL logging API
  locks.ts             the two kernel flocks (workspace at startup, session at StartSession)
  vendor-guard.ts      the ONLY dynamic import of the SDK; the FORBID_VENDOR_CALLS gate
  metaprompt.ts        the canonical metaprompt append
  proto.ts             THE single import site: shimv1 / storev1 / conversationv1 namespaces
  sdk/
    types.ts           the SDK boundary, aliased off sdk.d.ts; the upgrade canary's surface
    real-query.ts      the real query() factory (preset prompt, setting sources, pre-mint)
  service/
    server.ts          the UDS listener (h2c + HTTP/1.1 on one socket)
    routes.ts          the shim.v1 implementation: one handler per rpc
    failures.ts        one constructor per failure message + arm
    validate/          fields.ts (per non-primitive field), requests.ts (per request message)
  engine/
    engine.ts          the Engine seam + NotImplementedEngine
    session.ts turn.ts identity.ts cold.ts keepalive.ts compaction.ts
    backup.ts detached.ts pushes.ts permission-gate.ts
  convert/
    fold.ts            the fold seam and FoldOutput
    ids.ts             the four identifier spaces, minted
  store/
    keys.ts            upsert_key + write_id (THE one place)
    client.ts          the store.v1 client over the store UDS
  fake/
    index.ts           createFakeQuery(): the scenario engine behind --fake
test/
  one test file per src module, mirroring its path
  fakes/store-server.ts   an in-process store.v1 server, shared by the store and integration suites
scripts/
  dist-smoke.ts        the built bundle, spawned and dialed for real
  capture/             the capture harness
```

## The spawn contract

```
node dist/main.js --listen <uds> --store-socket <uds> --log-fd 3 [--fake]
node dist/main.js --version
```

Nothing else. An unrecognized flag is a startup **failure**, not a warning: it
means the daemon and this build disagree about the contract.

- **cwd** is the workspace directory, set by the spawner. It is not a flag —
  two sources for one fact can disagree.
- **Session facts travel only in `StartSession`.** The model, the permission
  mode and the vendor binding (fresh with a pre-minted id, or a resume handle)
  are rpc arguments, so `--session-id`, `--model`, `--permission-mode` and
  `--resume` do not exist. Neither does `--claude-bin`: the SDK's own bundled,
  pinned binary is the engine (R12).
- **Environment**, all refusals rather than defaults:
  - `CLAUDE_CONFIG_DIR` (required) — which ACCOUNT the session runs as.
  - `AGENT_REPL_OWNED=1` (required) — the daemon's mark; a shim refuses to run
    unowned.
  - `SHIM_BUILD_SHA` (required) — reported on `SessionStarted`; the daemon
    compares it against the deploy stamp and bounces a stale survivor.
  - `AGENT_REPL_STATE_DIR` (default `~/.claude-emacs`) — the one state root.
  - `AGENT_REPL_STORE_SOCKET` — the store socket when `--store-socket` is
    absent. **The flag beats the env.**
  - `AGENT_REPL_LOCK_DIR` (default `~/.cache/agent-repl/run`) — the kernel-lock
    directory. Both the shim and the daemon's probe read it, which is what makes
    relocation safe.
  - `AGENT_REPL_FORBID_VENDOR_CALLS` — the guard (see below).
  - `AGENT_REPL_FAKE_TURN_GATE`, `AGENT_REPL_FAKE_TURN_GATE_TEXT`,
    `AGENT_REPL_FAKE_SPOOL_ROOT` — `--fake` only.
- **Startup order**: parse argv → resolve env → configure the log on fd 3 →
  take the WORKSPACE lock → bind the UDS → serve. The lock precedes the bind
  because binding first leaves a window in which a duplicate shim is reachable
  and already writing. The SESSION lock is taken inside `StartSession`, keyed by
  the vendor session id, before the SDK is touched.
- **Signals**: SIGTERM is the one authorized shutdown and takes the
  `KillSession{force:true}` path, then exits 0 (nonzero if the stand-down
  failed). SIGINT is REFUSED and logged at error — an attached terminal's Ctrl-C
  must not end a live turn.
- **`--version`** prints `claude-shim <version>` and exits before any socket,
  lock, log fd or SDK import. It is a dependency-free smoke of the bundle.

## Mocked vendor: prompt → scenario table

_The mocked-vendor agent fills this in. It is generated from
`src/fake/registry.ts`, so it cannot drift from the registered scenarios._

## No real SDK calls from tests

`src/vendor-guard.ts` is the ONLY place that may dynamically import
`@anthropic-ai/claude-agent-sdk`; every call site goes through `importRealSDK`.
When `AGENT_REPL_FORBID_VENDOR_CALLS` is set to any non-empty value the guard
throws and the shim exits nonzero — never a silent no-op, never a fake fallback.
`test/setup.ts` sets it for the whole vitest suite, so a test needing offline
behavior must pass `--fake`. Production must never set it.

`test/vendor-guard.test.ts` enforces the chokepoint STRUCTURALLY: it walks
`src/` and fails if any file other than the guard contains a dynamic import of
the SDK. `src/sdk/types.ts` imports SDK types with `import type`, which is erased
at build time and is not a vendor import site.

## Logging

- `src/log.ts` is the one canonical JSON logging API, split between normal and
  verbose emission. New or changed shim code uses that API only.
- The durable sink is the **inherited fd 3**, never a pipe to the daemon's
  stderr: a shim must survive its daemon's death without dying on its own log
  line (EPIPE incident, 2026-08-10). A poisoned sink is surfaced, never
  silently swallowed; the stderr mirror is a convenience that retires itself
  once, durably recorded.
- Every record carries the shim `pid`, the workspace dir and its id, and every
  known agent-repl and Claude session identifier. Before a session exists the
  `agent_repl_session_id` is the process's own `shim-<workspace-key>-<pid>`,
  which correlates a log line with its lock file.
- **Every logical branch logs** — warnings at `warn`, errors at `error`. This
  instrumentation exists for the remediation loop: the integration suite is run
  to read the production code's logs.
- Each error is logged exactly once by its owning layer, with session, store
  key, socket, request, operation, resolved inputs, branch outcome and cause.
- Frequent or hot diagnostics use the verbose helper. Direct `console`,
  `process.stderr` or ad hoc logger aliases are forbidden except the documented
  pre-logger bootstrap failure and logger-sink emergency paths.
- The full contract is `modules/app/agent-repl/logging-contract.md`.

## Validation and errors

- **One base validate function per request message** (`service/validate/
  requests.ts`), **one per non-primitive field** (`service/validate/fields.ts`).
  An unset non-optional field or oneof is answered `InvalidArgument`
  IMMEDIATELY; refusals name the field path from the request root.
- **One constructor per failure message and arm** (`service/failures.ts`), so a
  refusal's `kind`/`cause` oneof can never be left unset.
- Three refusal channels: a TYPED failure in the response's own oneof (a fact
  about the session), a CONNECT ERROR (`InvalidArgument` for a malformed
  request, `NotFound` for a refused stream open, `Unimplemented` for the
  workflow trio), and a SESSION FAULT on `WatchSession` (the shim reporting its
  own degradation).
- **Workflow is kicked** (ruled 2026-08-29): `GetWorkflow`, `WatchWorkflow` and
  `StopWorkflow` answer `Code.Unimplemented` and have no `Engine` method.

## Verification

```bash
npm run typecheck     # tsc over src/, test/, scripts/ and the generated stubs
npm test              # vitest
npm run coverage      # vitest with v8 coverage over authored src/**/*.ts
npm run build         # esbuild -> dist/main.js (the entry the daemon spawns)
npm run smoke         # spawn and dial dist/main.js for real (needs a build first)
```

- `AGENT_REPL_FORBID_VENDOR_CALLS=1` in every shell you run tests in.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs every
  tracked suite across the module.
- Maintain at least 90% statement coverage. Never reduce the measured baseline,
  and add focused tests for every critical branch and every error path changed.
- `modules/app/agent-repl/bin/report-logging-density.sh shim` is a rough review
  aid, not semantic coverage: audit critical branches and errors directly even
  when the ratio rises.
