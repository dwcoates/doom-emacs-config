/**
 * fake/index.ts — the mocked vendor behind `--fake`.
 *
 * PLACEHOLDER. The mocked-vendor agent owns `src/fake/*` and replaces this file
 * with the real scenario engine (`scenario.ts`, `registry.ts`, `scenarios/*`,
 * `vendor-files.ts`). It exists now only so `main.ts` can wire `--fake` and
 * typecheck, and it REFUSES rather than pretending: a placeholder that answered
 * with plausible text would make every offline test pass against the
 * placeholder instead of the vendor script it meant to exercise.
 *
 * WHERE THE PLUMBING WENT. The previous `src/fake-query.ts` carried the parts
 * worth keeping — the `AsyncQueue` bridge between a push-style producer and the
 * SDK's pull-style streaming input, the `emit`/`emitStream` identity stamping
 * (fresh uuid per record, the session uuid currently in force, `ttft_ms` on a
 * `message_start`), the turn gate, the interrupt-releases-a-parked-turn hold,
 * and the `stopTask` → `task_notification{status:"stopped"}` answer. It is
 * deleted here, not lost: read it at `42655f0fd:modules/app/agent-repl/
 * agent-shim/claude/shim/src/fake-query.ts` (and `AsyncQueue` at the same
 * revision's `src/input-queue.ts`) when rebuilding this module.
 *
 * THE ENV CONTRACT the replacement must honor (fanout plan, "Process shell"):
 * `AGENT_REPL_FAKE_TURN_GATE` names a path and
 * `AGENT_REPL_FAKE_TURN_GATE_TEXT` the prompt that parks on it until the path
 * is removed; `AGENT_REPL_FAKE_SPOOL_ROOT` (default `/tmp/claude-<uid>`) roots
 * the spools the mock writes.
 */
import { Code, ConnectError } from "@connectrpc/connect";
import { bindLog } from "../log.js";
import type { CanUseToolLike, QueryLike, SdkUserMessage } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-fake", operation: "shim.fake.query" });

/** What the entrypoint knows about a fake session when it builds one. */
export interface FakeQueryOpts {
  /** The vendor session id the mock reports, pre-minted or resumed. */
  readonly sessionId: string;
  /** Mint a uuid. Injectable so goldens are stable. */
  readonly newUuid: () => string;
  /** The vendor session being continued, when this is a resume. */
  readonly resume?: string;
  /** Ends the fake stream when the session stands down. */
  readonly abortSignal?: AbortSignal;
}

/**
 * Build the offline stand-in for `query()`.
 *
 * Throws today. The refusal is a `ConnectError(Unimplemented)` rather than a
 * bare Error so a caller that surfaces it on an rpc says the honest thing: the
 * verb exists, this build cannot serve it.
 */
export function createFakeQuery(
  _prompt: AsyncIterable<SdkUserMessage>,
  _canUseTool: CanUseToolLike,
  opts: FakeQueryOpts,
): QueryLike {
  const message =
    "shim fake: the mocked vendor is not implemented in this build; " +
    "src/fake/index.ts is a scaffold placeholder owned by the mocked-vendor agent";
  LOGGER.log(
    { level: "error", agent_repl_session_id: opts.sessionId, resumed: opts.resume !== undefined },
    message,
  );
  throw new ConnectError(message, Code.Unimplemented);
}
