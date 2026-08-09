/**
 * agent-emission — `agentshim.frontend.v1.AgentEmission`, THE agent-output
 * vocabulary, decoded once for both places that carry it.
 *
 * agent-emission.proto states the point plainly: the SAME message rides the
 * top-level feed (`ConversationItem.agent`) and a detached agent's bubble
 * (`AsyncAgentBubble.emissions`, `AsyncAgentUpdate.emissions`), because a
 * detached agent is not a second, weaker kind of conversation — it is the same
 * conversation happening somewhere else.
 *
 * That identity has to be REAL in this codebase, not merely asserted in a
 * comment: this module is the one decoder both paths call, so the two cannot
 * disagree about what an emission is. It was lifted out of `frontend-proto.ts`
 * for exactly that reason when the async surface landed.
 *
 * The arm oneof is validated STRICTLY here — empty, multiple, or unrecognized
 * all throw — because `AgentEmission` is a `frontend.v1`-owned message. Only
 * the data.v1/core.v1 payload BELOW the arm is adopted by shape (see the §5.1
 * boundary note in `frontend-proto.ts`).
 */

import { ensureObject, num, str, type Obj } from "./proto-scalars.js";

/**
 * `AgentEmission` arm key → the flat decoded arm it unwraps to, and the field
 * of the emission that carries the payload the adapter reads.
 *
 * `usageStamp` on an `AgentResponse` is NOT carried through: it is a resolved
 * figure for the bubble's corner that this end does not render yet. It is
 * dropped here rather than smuggled into the payload so that whoever wires the
 * stamp up finds one place to add it.
 */
export const AGENT_EMISSION_ARMS = {
  response: { arm: "assistantMessage", body: "body" },
  thinking: { arm: "thinking", body: "body" },
  toolCall: { arm: "toolUse", body: "call" },
  toolResult: { arm: "toolResult", body: "result" },
  toolOutcome: { arm: "toolUseResult", body: "structured" },
  skillBody: { arm: "skillBody", body: "" },
  turnResult: { arm: "result", body: "" },
} as const;

/** The flat item arms an `AgentEmission` can unwrap to. */
export type AgentEmissionArm = (typeof AGENT_EMISSION_ARMS)[keyof typeof AGENT_EMISSION_ARMS]["arm"];

/** The `AgentEmission` oneof arm keys, as the wire spells them. */
export type AgentEmissionKey = keyof typeof AGENT_EMISSION_ARMS;

/** One unwrapped emission: which flat arm it is, and its adopted payload. */
export interface UnwrappedEmission {
  /** The `AgentEmission` oneof key the wire set — the emission's own identity. */
  emission: AgentEmissionKey;
  arm: AgentEmissionArm;
  payload: Obj;
  /**
   * A thinking block's statement about where it came from: the message it was
   * stripped from and the position it held there.
   */
  thinkingOrigin?: { apiMessageId: string; blockIndex: number };
  /**
   * THE CLASSIFICATION VERDICT this emission published, when it published one:
   * `AgentToolCall.spawned_bubble_id` / `AgentToolOutcome.spawned_bubble_id`.
   *
   * Non-empty exactly when the call detached work that has its own
   * `AsyncBubble`, and then equal to that bubble's id. EMPTY MEANS "this call
   * detached nothing", and that is the only reading of empty — see
   * tool-call.proto. It is carried up beside the payload because it sits on the
   * `AgentToolCall`/`AgentToolOutcome` envelope, one level ABOVE the verbatim
   * data.v1 block the payload unwraps to, so it would otherwise be discarded.
   */
  spawnedBubbleId?: string;
}

/**
 * Unwrap one `AgentEmission` into a flat arm + its payload.
 *
 * CTX names the containing message for the error text, so a bad emission in a
 * detached agent's fold is as findable as one in the feed.
 */
export function unwrapAgentEmission(v: unknown, ctx: string): UnwrappedEmission {
  const emission = ensureObject(v, ctx);
  const keys = Object.keys(emission);
  if (keys.length === 0) {
    throw new Error(`frontend-proto: ${ctx} carries no emission (empty oneof)`);
  }
  if (keys.length > 1) {
    throw new Error(`frontend-proto: ${ctx} sets multiple emissions: ${keys.join(", ")}`);
  }
  const key = keys[0];
  if (!Object.prototype.hasOwnProperty.call(AGENT_EMISSION_ARMS, key)) {
    throw new Error(`frontend-proto: ${ctx} has unrecognized emission '${key}'`);
  }
  const mapped = AGENT_EMISSION_ARMS[key as AgentEmissionKey];
  const value = ensureObject(emission[key], `${ctx}.${key}`);
  // An emission whose whole content IS the payload (skillBody, turnResult)
  // names no inner field; the others wrap theirs one level down.
  const payload = mapped.body === "" ? value : ensureObject(value[mapped.body], `${ctx}.${key}.${mapped.body}`);
  const out: UnwrappedEmission = { emission: key as AgentEmissionKey, arm: mapped.arm, payload };
  if (key === "thinking") {
    out.thinkingOrigin = {
      apiMessageId: str(value, "apiMessageId", `${ctx}.thinking`),
      blockIndex: num(value, "blockIndex", `${ctx}.thinking`),
    };
  }
  if (key === "toolCall" || key === "toolOutcome") {
    out.spawnedBubbleId = str(value, "spawnedBubbleId", `${ctx}.${key}`);
  }
  return out;
}
