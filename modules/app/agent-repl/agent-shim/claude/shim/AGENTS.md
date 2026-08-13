# agent-shim/claude/shim/

The per-session Claude shim (TypeScript/Node, one process per session).
Responsibility: drive the Claude Agent SDK (`query()`), CONVERT the SDK stream
into vendor-neutral records at the edge, write durable ones to the shim-store,
forward the store-merged session stream plus live records to its daemon
connection, and execute control messages (prompts, interrupts, `canUseTool`
permission round-trips).

It holds no cross-turn state, serves no frontend, and derives no render-state.
A daemon disconnect does not end the in-flight turn (reattach support).

## The shim converts at the edge, and says less than it used to

Nothing vendor-shaped leaves this process. Each SDK message becomes zero or
more `agentshim.v1.Entry` records whose external half
(`protocol.v1.ExternalEntry`) is already neutral and whose internal half never
crosses the wire.

- **It is authoritative for LIFECYCLE**, and that is what it converts:
  `system:init` -> `SessionBegan`, `result` -> `TurnEnded`,
  `conversation_reset` -> `SessionIdentityChanged`, a stamped `message_start` ->
  `ResponseTiming`, `tool_progress` -> `Heartbeat`. Every one is bookkeeping —
  a fact ABOUT the session, which renders as nothing.
- **It produces NO conversation content, and cannot.** A
  `conversation.v1.MessageEntry` must state its `parent`, and the SDK stream
  carries no parent pointer. The file plane (the sidecar) owns content because
  that is what the vendor itself recorded.
- **The ONE exception is the live typing preview** (`ContentArriving`, in
  `src/proto/delta.ts`), handed straight to the daemon and never written. It is
  refused rather than guessed when the parent is unresolvable — see below.
- **Everything else lands on `InternalEntry.unconverted`**, whole: understood
  vendor material on `VendorSpecificEntry`, an unrecognized discriminator on
  `UnknownEntry`, a read failure on `UnparsedEntry`. A record with no external
  half has no path to the daemon at all, which is what makes eager conversion
  safe rather than lossy.

**Never encode an unresolved parent as a root.** `MessageParent` is a oneof
precisely so "this is a feed row" and "the producer could not resolve a parent"
cannot wear the same value. A producer that cannot resolve one has nothing legal
to emit and must fail loudly.

Dependencies: `@anthropic-ai/claude-agent-sdk`, `proto/gen/ts` (generated TS for
`conversation.v1`, `protocol.v1` and `agentshim.v1`, re-exported through
`src/uds/proto.ts`), the shim-store UDS socket.

## No real SDK calls from tests

`src/vendor-guard.ts` is the ONLY place that may dynamically import
`@anthropic-ai/claude-agent-sdk`; every call site goes through `importRealSDK`.
When `AGENT_REPL_FORBID_VENDOR_CALLS` is set to any non-empty value the guard
throws and the shim exits nonzero — never a silent no-op, never a fake
fallback. `test/setup.ts` sets it for the whole vitest suite, so a test that
needs offline behavior must pass `--fake`. Production must never set it.

## Logging

- The Claude shim owns one canonical JSON logging API in `src/uds/log.ts`,
  divided between normal and verbose emission functions. New or changed shim
  code uses that API only.
- Every shim has `--cwd`, so every durable shim record is workspace-bound and
  persists through `<workspace>/.claude/emacs/shim.log`. Every record carries
  the shim `pid` and every known agent-repl and Claude session identifier. A
  failed workspace binding is an invariant violation, never a global record.
- Every new or materially changed nontrivial function logs its entry. Every
  meaningful branch that selects a different nontrivial block, call, state
  transition, or outcome logs its selection.
- The normal helper persists through the daemon's captured shim log and emits
  to stderr. The verbose helper always persists and gates terminal visibility
  through the owning runtime's verbose setting.
- Each error is logged exactly once by its owning layer with session, store key,
  socket, request, operation, resolved inputs, branch outcome, and cause.
  Error-path tests assert the canonical record and its context.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct `console`, `process.stderr`, or ad hoc logger aliases are forbidden
  except a documented pre-logger bootstrap failure or logger-sink emergency path.

## Verification

- `npm run typecheck` type-checks and `npm run coverage` measures authored
  `src/**/*.ts`, including branch data, excluding declarations and generated
  sources.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module.
- Maintain at least 90% statement coverage. Never reduce the measured baseline,
  and add focused tests for every critical branch and every error path changed.
- Run `modules/app/agent-repl/bin/report-logging-density.sh shim` and report
  its source-line and canonical-call counts as a rough review aid. It is not
  semantic logging coverage, so directly audit all critical branches and
  errors even when the ratio rises.
- After a commit lands on `master`, run
  `modules/app/agent-repl/bin/test-all.sh --record`, inspect
  `modules/app/agent-repl/test_time.csv`, and surface every reported timing
   regression.
