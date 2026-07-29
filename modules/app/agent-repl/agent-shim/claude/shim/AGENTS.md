# agent-shim/claude/shim/

The per-session Claude shim (TypeScript/Node, one process per session).
Responsibility: drive the Claude Agent SDK (`query()`), convert the SDK stream
into agent-shim protocol events (loud validation: hard-error on missing
expected fields of a KNOWN family, capture an UNKNOWN discriminator onto the
`unknown` passthrough arm, capture-and-log unknown new fields), write PERSISTENT events
to the shim-store, forward the store-merged session stream plus EPHEMERAL
deltas to its daemon connection, and execute control messages (prompts,
interrupts, `canUseTool` permission round-trips).

It holds no cross-turn state, serves no frontend, and derives no render-state.
A daemon disconnect does not end the in-flight turn (reattach support).

Dependencies: `@anthropic-ai/claude-agent-sdk`, `proto/agentshim/` (generated
TS), the shim-store UDS socket.

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

- Before completing changes, run `npm run typecheck` and `npm run coverage`.
  Both commands must pass. Coverage measures authored `src/**/*.ts`, including
  branch data, and excludes declarations and generated sources.
- Before handoff, run `modules/app/agent-repl/bin/test-all.sh` from the
  repository root. Every tracked suite must pass.
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
