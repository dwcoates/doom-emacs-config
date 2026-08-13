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

import {
  EMPTY_KEY_SET,
  ensureObject,
  generatedFieldSet,
  num,
  rejectUnknown,
  str,
  type Obj,
} from "./proto-scalars.js";
import { ResponseUsageStampSchema } from "../../proto/gen/ts/frontend/v1/agent-response_pb";
import { AgentToolOutcomeSchema } from "../../proto/gen/ts/frontend/v1/tool-call_pb";
import {
  DetachedFailedSchema,
  DetachedLostSchema,
  DetachedSkillSchema,
  DetachedUnclassifiedSchema,
  DetachedSucceededSchema,
  DetachedWorkEndedSchema,
  DetachedWorkKindSchema,
  DetachedWorkStartedSchema,
  type DetachedWorkEnded as GeneratedDetachedWorkEnded,
  type DetachedWorkKind as GeneratedDetachedWorkKind,
} from "../../proto/gen/ts/conversation/v1/payloads_pb";

/** A generated oneof's arm keys, with protobuf-es's "nothing set" arm dropped. */
type ArmKeys<Oneof extends { case: string | undefined }> = Exclude<Oneof["case"], undefined>;

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
 * verbatim payload — exactly like `thinkingOrigin` and `spawnedMessageId`. A
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

// --- the tool's TYPED OUTCOME (frontend.v1.AgentToolOutcome) ----------------
//
// It used to carry `data.v1.ToolUseResult`, a vendor union of five shapes
// (AgentAsyncLaunch, AgentResult, TaskOutputResult, TaskStopResult,
// WorkflowLaunchResult) that this end destructured into two facts: work
// detached, and work reached an end. conversation.v1 models those two facts
// directly, so the chip's facts are now the DETACHMENT'S OWN facts. THE TYPED
// OUTCOME IS THE CHIP; there is no separate chip arm.

/** What kind of work detached; the set arm IS the kind. */
export type DetachedKind =
  | { case: "agent" }
  | { case: "shell" }
  | { case: "workflow" }
  | { case: "skill"; skillName: string; args: string }
  | { case: "merge" }
  | { case: "unclassified"; toolName: string };

/** Work that detached from the turn and now runs alongside it. */
export interface DetachedStarted {
  /**
   * The tool call that spawned it. Empty ONLY for a `merge`, the one
   * detachment no tool spawns — see conversation.v1's DetachedMerge.
   */
  originToolCallId: string;
  /** What to call it in the feed, resolved by the producer. */
  label: string;
  kind: DetachedKind;
}

/**
 * Detached work reached an end, with the outcome it reached.
 *
 * `lost` is a SEPARATE outcome from `failed` on purpose: we do not know that
 * the work failed, only that we cannot see it any more, and folding the two
 * would have the chip assert something nothing observed.
 */
export type DetachedEnded =
  | { case: "succeeded"; summary: string }
  | { case: "failed"; summary: string }
  | { case: "cancelled" }
  | { case: "lost"; inference: string };

/**
 * The detachment a tool outcome reports: a launch or an ending, and never both.
 *
 * A oneof rather than one field because a launch and an ending are DIFFERENT
 * FACTS and a chip renders them differently.
 */
export type ToolDetachment =
  | { case: "started"; value: DetachedStarted }
  | { case: "ended"; value: DetachedEnded };

/** The decoded `AgentToolOutcome`. */
export interface ToolOutcome {
  /**
   * The tool_use id this outcome belongs to, carried explicitly because the
   * outcome has no correlation id of its own. NEVER EMPTY: an outcome that
   * cannot say which call it belongs to is unattachable, so it throws.
   */
  toolUseId: string;
  /**
   * The detachment, or ABSENT when the call returned ordinarily and detached
   * nothing. Absent is a fact, not a missing one.
   */
  detachment?: ToolDetachment;
}

const TOOL_OUTCOME_KEYS = generatedFieldSet<keyof typeof AgentToolOutcomeSchema.field>()(
  "started",
  "ended",
  "toolUseId",
  "spawnedMessageId",
);
const DETACHED_STARTED_KEYS = generatedFieldSet<keyof typeof DetachedWorkStartedSchema.field>()(
  "originToolCallId",
  "label",
  "kind",
);
const DETACHED_KIND_KEYS = generatedFieldSet<keyof typeof DetachedWorkKindSchema.field>()(
  "agent",
  "shell",
  "workflow",
  "unclassified",
  "skill",
  "merge",
);
const DETACHED_SKILL_KEYS = generatedFieldSet<keyof typeof DetachedSkillSchema.field>()(
  "skillName",
  "args",
);
const DETACHED_UNCLASSIFIED_KEYS = generatedFieldSet<
  keyof typeof DetachedUnclassifiedSchema.field
>()("toolName");
const DETACHED_ENDED_KEYS = generatedFieldSet<keyof typeof DetachedWorkEndedSchema.field>()(
  "succeeded",
  "failed",
  "cancelled",
  "lost",
);
const DETACHED_SUCCEEDED_KEYS = generatedFieldSet<keyof typeof DetachedSucceededSchema.field>()(
  "summary",
);
const DETACHED_FAILED_KEYS = generatedFieldSet<keyof typeof DetachedFailedSchema.field>()("summary");
const DETACHED_LOST_KEYS = generatedFieldSet<keyof typeof DetachedLostSchema.field>()("inference");

/** The kind arm keys, typed against the generated oneof. */
const DETACHED_KIND_ARMS = [
  "agent",
  "shell",
  "workflow",
  "unclassified",
  "skill",
  "merge",
] as const satisfies readonly ArmKeys<GeneratedDetachedWorkKind["kind"]>[];

/** The ending arm keys, typed against the generated oneof. */
const DETACHED_ENDED_ARMS = [
  "succeeded",
  "failed",
  "cancelled",
  "lost",
] as const satisfies readonly ArmKeys<GeneratedDetachedWorkEnded["outcome"]>[];

/** The single set arm of a oneof, refusing empty and multiple alike. */
function singleArm(o: Obj, arms: readonly string[], ctx: string): string {
  const set = arms.filter((k) => o[k] !== undefined && o[k] !== null);
  if (set.length === 0) {
    throw new Error(`frontend-proto: ${ctx} carries no arm (empty oneof)`);
  }
  if (set.length > 1) {
    throw new Error(`frontend-proto: ${ctx} sets multiple arms: ${set.join(", ")}`);
  }
  return set[0];
}

/**
 * Decode a `DetachedWorkKind`.
 *
 * SIX ARMS, and an unrecognized one throws rather than falling into
 * `unclassified`. `unclassified` is the daemon STATING that it could not tell,
 * which is a different assertion from this end failing to recognize an arm the
 * daemon does know.
 */
function decodeDetachedKind(v: unknown, ctx: string): DetachedKind {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, DETACHED_KIND_KEYS, ctx);
  const arm = singleArm(o, DETACHED_KIND_ARMS, ctx);
  switch (arm) {
    case "skill": {
      const s = ensureObject(o.skill, `${ctx}.skill`);
      rejectUnknown(s, DETACHED_SKILL_KEYS, `${ctx}.skill`);
      return {
        case: "skill",
        skillName: str(s, "skillName", `${ctx}.skill`),
        args: str(s, "args", `${ctx}.skill`),
      };
    }
    case "unclassified": {
      const u = ensureObject(o.unclassified, `${ctx}.unclassified`);
      rejectUnknown(u, DETACHED_UNCLASSIFIED_KEYS, `${ctx}.unclassified`);
      return { case: "unclassified", toolName: str(u, "toolName", `${ctx}.unclassified`) };
    }
    default: {
      // agent / shell / workflow / merge are EMPTY messages: being set is the
      // entire assertion, so there is nothing to read out of them.
      rejectUnknown(ensureObject(o[arm], `${ctx}.${arm}`), EMPTY_KEY_SET, `${ctx}.${arm}`);
      return { case: arm } as DetachedKind;
    }
  }
}

/** Decode a `DetachedWorkStarted`. */
function decodeDetachedStarted(v: unknown, ctx: string): DetachedStarted {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, DETACHED_STARTED_KEYS, ctx);
  if (o.kind === undefined || o.kind === null) {
    throw new Error(`frontend-proto: ${ctx} requires \`kind\``);
  }
  return {
    originToolCallId: str(o, "originToolCallId", ctx),
    label: str(o, "label", ctx),
    kind: decodeDetachedKind(o.kind, `${ctx}.kind`),
  };
}

/** Decode a `DetachedWorkEnded`. */
function decodeDetachedEnded(v: unknown, ctx: string): DetachedEnded {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, DETACHED_ENDED_KEYS, ctx);
  const arm = singleArm(o, DETACHED_ENDED_ARMS, ctx);
  switch (arm) {
    case "succeeded": {
      const s = ensureObject(o.succeeded, `${ctx}.succeeded`);
      rejectUnknown(s, DETACHED_SUCCEEDED_KEYS, `${ctx}.succeeded`);
      return { case: "succeeded", summary: str(s, "summary", `${ctx}.succeeded`) };
    }
    case "failed": {
      const f = ensureObject(o.failed, `${ctx}.failed`);
      rejectUnknown(f, DETACHED_FAILED_KEYS, `${ctx}.failed`);
      return { case: "failed", summary: str(f, "summary", `${ctx}.failed`) };
    }
    case "lost": {
      const l = ensureObject(o.lost, `${ctx}.lost`);
      rejectUnknown(l, DETACHED_LOST_KEYS, `${ctx}.lost`);
      return { case: "lost", inference: str(l, "inference", `${ctx}.lost`) };
    }
    default: {
      rejectUnknown(ensureObject(o.cancelled, `${ctx}.cancelled`), EMPTY_KEY_SET, `${ctx}.cancelled`);
      return { case: "cancelled" };
    }
  }
}

/**
 * Decode an `AgentToolOutcome`.
 *
 * THE OUTCOME ONEOF IS OPTIONAL and both arms set is refused: absent means the
 * call returned ordinarily and detached nothing, which is a stated fact, while
 * a launch and an ending at once is a producer fault the chip cannot draw.
 *
 * `toolUseId` is REQUIRED. The outcome has no correlation id of its own — on
 * disk it is associated with its tool_result line positionally, and a
 * positional association does not survive being pushed as an independent
 * emission — so an outcome without it can attach to nothing.
 */
export function decodeToolOutcome(v: unknown, ctx: string): ToolOutcome {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, TOOL_OUTCOME_KEYS, ctx);
  const toolUseId = str(o, "toolUseId", ctx);
  if (toolUseId === "") {
    throw new Error(
      `frontend-proto: ${ctx} missing required \`toolUseId\` — a tool outcome carries no correlation id of its own`,
    );
  }
  const hasStarted = o.started !== undefined && o.started !== null;
  const hasEnded = o.ended !== undefined && o.ended !== null;
  if (hasStarted && hasEnded) {
    throw new Error(
      `frontend-proto: ${ctx} sets both \`started\` and \`ended\`, which are one oneof`,
    );
  }
  const outcome: ToolOutcome = { toolUseId };
  if (hasStarted) {
    outcome.detachment = { case: "started", value: decodeDetachedStarted(o.started, `${ctx}.started`) };
  }
  if (hasEnded) {
    outcome.detachment = { case: "ended", value: decodeDetachedEnded(o.ended, `${ctx}.ended`) };
  }
  return outcome;
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
  // The TYPED OUTCOME. `body` is empty because the whole message IS the
  // payload now: `structured` (the vendor's data.v1.ToolUseResult) is gone,
  // and what is left — the detachment oneof, the tool_use id, the verdict —
  // is decoded strictly into `UnwrappedEmission.toolOutcome`.
  toolOutcome: { arm: "toolUseResult", body: "" },
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
   * `AgentToolCall.spawned_message_id` / `AgentToolOutcome.spawned_message_id`.
   *
   * Non-empty exactly when the call detached work, and then equal to the uuid
   * of the MESSAGE that work is carried by. EMPTY MEANS "this call detached
   * nothing", and that is the only reading of empty — see tool-call.proto. It
   * is carried up beside the payload because it sits on the
   * `AgentToolCall`/`AgentToolOutcome` envelope, one level ABOVE the verbatim
   * data.v1 block the payload unwraps to, so it would otherwise be discarded.
   */
  spawnedMessageId?: string;
  /**
   * The RESOLVED figures this response's bubble corner renders
   * (`AgentResponse.usage_stamp`). Present only on the `response` emission, and
   * only when the response actually carried a usage record.
   *
   * ABSENT MEANS ABSENT — the corner renders no figures rather than zeros.
   */
  usageStamp?: ResponseUsageStamp;
  /**
   * The DECODED `AgentToolOutcome` — the tool call's typed outcome, and the
   * chip itself (the outcome IS the chip; there is no separate chip arm).
   *
   * Present exactly on the `toolOutcome` emission. Its `detachment` is absent
   * when the call returned ordinarily and detached nothing.
   */
  toolOutcome?: ToolOutcome;
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
    out.spawnedMessageId = str(value, "spawnedMessageId", `${ctx}.${key}`);
  }
  if (key === "toolOutcome") {
    out.toolOutcome = decodeToolOutcome(value, `${ctx}.toolOutcome`);
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
