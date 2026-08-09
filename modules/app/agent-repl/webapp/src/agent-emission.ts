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

import { ensureObject, generatedFieldSet, num, rejectUnknown, str, type Obj } from "./proto-scalars.js";
import { ResponseUsageStampSchema } from "../../proto/gen/ts/agentshim/frontend/v1/response-bubble_pb";

/**
 * The figures an assistant bubble's corner renders, resolved daemon-side.
 *
 * ABSENCE IS ABSENCE: a response that carried no usage record has no stamp, and
 * the bubble then renders NO figures. Zeros are never fabricated in its place.
 */
export interface ResponseUsageStamp {
  /** The headline: canonical `input_misses` total (written + unwritten). */
  expensiveInputTokens: number;
  cacheReadTokens: number;
  outputTokens: number;
  /** Display form; empty for synthetic records (never fabricated). */
  model: string;
}

const RESPONSE_USAGE_STAMP_KEYS = generatedFieldSet<
  keyof typeof ResponseUsageStampSchema.field
>()("expensiveInputTokens", "cacheReadTokens", "outputTokens", "model");

/**
 * Decode a `ResponseUsageStamp`.
 *
 * Called only where the wire CARRIES one. An absent stamp is never synthesized
 * here — the caller keeps the absence, and the bubble corner renders nothing,
 * because zeros would read as a response that cost nothing.
 *
 * It lives beside the emission unwrap rather than in `frontend-proto.ts`
 * because the stamp rides the `AgentResponse` ENVELOPE, one level above the
 * verbatim payload — exactly like `thinkingOrigin` and `spawnedBubbleId`. A
 * detached agent's responses are the same message, so they resolve their
 * corner figures through this same decoder rather than a second copy.
 */
export function decodeResponseUsageStamp(v: unknown, where: string): ResponseUsageStamp {
  const o = ensureObject(v, where);
  rejectUnknown(o, RESPONSE_USAGE_STAMP_KEYS, where);
  return {
    expensiveInputTokens: num(o, "expensiveInputTokens", where),
    cacheReadTokens: num(o, "cacheReadTokens", where),
    outputTokens: num(o, "outputTokens", where),
    model: str(o, "model", where),
  };
}

/**
 * `AgentEmission` arm key → the flat decoded arm it unwraps to, and the field
 * of the emission that carries the payload the adapter reads.
 *
 * `usageStamp` on an `AgentResponse` IS carried through, beside the payload
 * rather than inside it (see `UnwrappedEmission.usageStamp`): it is a
 * daemon-resolved figure for the bubble's corner, and the response BODY is
 * verbatim durable evidence that must not grow a field the vendor never sent.
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
  /**
   * The RESOLVED figures this response's bubble corner renders
   * (`AgentResponse.usage_stamp`). Present only on the `response` emission, and
   * only when the response actually carried a usage record.
   *
   * ABSENT MEANS ABSENT — the corner renders no figures rather than zeros.
   */
  usageStamp?: ResponseUsageStamp;
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
  if (key === "response") {
    // ABSENT STAMP STAYS ABSENT. A response that carried no usage record gets
    // no stamp field here, and the bubble corner then renders no figures —
    // never zeros, which would read as a response that cost nothing.
    const stamp = value.usageStamp;
    if (stamp !== undefined && stamp !== null) {
      out.usageStamp = decodeResponseUsageStamp(stamp, `${ctx}.response.usageStamp`);
    }
  }
  return out;
}
