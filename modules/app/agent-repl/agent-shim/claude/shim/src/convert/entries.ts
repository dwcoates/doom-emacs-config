/**
 * convert/entries.ts — the shapes every converter builds, in one place.
 *
 * # Why a file of builders rather than `create()` at every site
 *
 * Every unit frame the fold produces is the same three nested envelopes:
 * an `AgentActivity` (which carries the identity and the usage), an
 * `AgentUpdate` (which says the frame is read-only conversation content), and
 * an `AgentFrame` (which says which agent it is about). Spelling those three out
 * at ~90 call sites would make a mistake at one of them invisible; here it is
 * one function and one test.
 *
 * The PROTO→CODE MAPPING convention is what these are: one base function per
 * message, one dedicated function per non-primitive use site, delegating to the
 * child's base.
 */
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../proto.js";
import { activityUpsertKey, terminalUpsertKey } from "../store/keys.js";
import type { PersistEntry, SourceCoordinates } from "../store/persistence.js";
import type { FoldContext } from "./fold-context.js";

// ---------------------------------------------------------------------------
// Instants
// ---------------------------------------------------------------------------

/** When a call was handed to whatever performs it. */
export function startedAt(atMs: number): conversationv1.AgentActivityStartedAt {
  return create(conversationv1.AgentActivityStartedAtSchema, { atMs: BigInt(Math.trunc(atMs)) });
}

/**
 * When a call reached its outcome, and the start it closes.
 *
 * THE START IS RESTATED ON THE SETTLE so a settled frame alone states the
 * call's runtime: its start and its settle upsert one unit, and a replay serves
 * the settle with no start beside it. The parameter is REQUIRED, so every
 * settle site decides: the call's own start instant, or `undefined` for an arm
 * whose start carries no instant (a prose or reasoning block) or a producer
 * that genuinely never saw the start.
 */
export function settledAt(
  atMs: number,
  startedAtMs: number | undefined,
): conversationv1.AgentActivitySettledAt {
  return create(conversationv1.AgentActivitySettledAtSchema, {
    atMs: BigInt(Math.trunc(atMs)),
    startedAt: startedAtMs === undefined ? undefined : startedAt(startedAtMs),
  });
}

/** The vendor's per-call liveness beat, relayed as observed. */
export function toolProgress(lastProgressAtMs: number): conversationv1.AgentToolCallProgress {
  return create(conversationv1.AgentToolCallProgressSchema, {
    lastProgressAtMs: BigInt(Math.trunc(lastProgressAtMs)),
  });
}

// ---------------------------------------------------------------------------
// Content
// ---------------------------------------------------------------------------

/** Words meant to be read as they are. */
export function textBlock(text: string): conversationv1.TextBlock {
  return create(conversationv1.TextBlockSchema, { text });
}

/** What the agent said, whole. */
export function prose(markdown: string): conversationv1.AgentResponseProse {
  return create(conversationv1.AgentResponseProseSchema, { markdown });
}

/** One text block as a tool result's content. */
export function toolResultText(text: string): conversationv1.ToolResultContent {
  return create(conversationv1.ToolResultContentSchema, {
    blocks: [
      create(conversationv1.ToolResultContentBlockSchema, {
        block: { case: "text", value: textBlock(text) },
      }),
    ],
  });
}

/**
 * The shared account of a failed call.
 *
 * `content` is UNSET when the producer observed a failed call with NO error
 * content at all, which happens and is different from an empty text block: a
 * consumer then draws the failure with no detail rather than an empty card.
 */
export function toolFailure(
  content: conversationv1.ToolResultContent | undefined,
  settledAtMs: number,
  startedAtMs: number | undefined,
): conversationv1.AgentToolFailure {
  return create(conversationv1.AgentToolFailureSchema, {
    content,
    settledAt: settledAt(settledAtMs, startedAtMs),
  });
}

// ---------------------------------------------------------------------------
// The three envelopes
// ---------------------------------------------------------------------------

/** What rides an activity's envelope: the response's cost and effort. */
interface ActivityEnvelope {
  /** Set ONLY on the unit for the API response's FIRST content block. */
  readonly usage?: conversationv1.TokenUsage;
  /** Set on the same unit as `usage`, and no other. */
  readonly effort?: conversationv1.AgentEffortLevel;
  /** The skill or plugin the producer attributed the unit to. */
  readonly attribution?: conversationv1.AgentActivityAttribution;
}

/**
 * The stands-alone contract revision this producer writes every unit under.
 *
 * STAMPED HERE, in the one activity constructor, and never by the per-kind
 * converters that do the restating: a new arm that forgets to restate is still
 * stamped, so a consumer grades its bare settle a defect rather than old data.
 */
export const ACTIVITY_CONTRACT =
  conversationv1.AgentActivityContract.SETTLES_STAND_ALONE;

/** One unit of work at whatever state it has reached. */
export function agentActivity(
  activityId: conversationv1.AgentActivityId,
  item: conversationv1.AgentActivity["item"],
  envelope: ActivityEnvelope = {},
): conversationv1.AgentActivity {
  return create(conversationv1.AgentActivitySchema, {
    activityId,
    item,
    usage: envelope.usage,
    effort: envelope.effort,
    attribution: envelope.attribution,
    contract: ACTIVITY_CONTRACT,
  });
}

/** An activity, as the read-only conversation content it is. */
function activityUpdate(activity: conversationv1.AgentActivity): conversationv1.AgentUpdate {
  return create(conversationv1.AgentUpdateSchema, {
    update: { case: "activity", value: activity },
  });
}

/** One frame of one agent's stream. */
export function agentFrame(
  agentId: conversationv1.AgentId,
  result: conversationv1.AgentFrame["result"],
): conversationv1.AgentFrame {
  return create(conversationv1.AgentFrameSchema, { agentId, result });
}

/** An update frame — the ordinary shape of everything an agent does. */
export function updateFrame(
  agentId: conversationv1.AgentId,
  update: conversationv1.AgentUpdate,
): conversationv1.AgentFrame {
  return agentFrame(agentId, { case: "update", value: update });
}

// ---------------------------------------------------------------------------
// Rows
// ---------------------------------------------------------------------------

/** Which agent a converted frame belongs to, and where it came from. */
export interface FrameOrigin {
  /** The book — a subagent's own id for its frames, the main agent's otherwise. */
  readonly agentId: conversationv1.AgentId;
  /** The SDK message's uuid. */
  readonly vendorUuid: string;
  /** The 0-based block index, for a block-derived frame. */
  readonly blockIndex?: number;
  /** The frame's arm path. */
  readonly discriminator: string;
}

/** The source coordinates one origin names. */
export function sourceOf(origin: FrameOrigin): SourceCoordinates {
  return {
    vendorUuid: origin.vendorUuid,
    ...(origin.blockIndex === undefined ? {} : { blockIndex: origin.blockIndex }),
    discriminator: origin.discriminator,
  };
}

/**
 * One unit frame as a row.
 *
 * Keyed by the UNIT, so every frame of it — start, progress, terminal, and the
 * post-terminal diagnostics consequence — replaces one row rather than
 * appending four.
 */
export function activityEntry(
  context: FoldContext,
  origin: FrameOrigin,
  activity: conversationv1.AgentActivity,
): PersistEntry {
  if (activity.activityId === undefined) {
    throw new Error("shim convert: an activity frame with no identity cannot be upserted");
  }
  return {
    agentId: origin.agentId,
    upsertKey: activityUpsertKey(activity.activityId),
    source: sourceOf(origin),
    keepalive: context.keepalive,
    turn: context.turnId,
    item: { kind: "frame", frame: updateFrame(origin.agentId, activityUpdate(activity)) },
  };
}

/**
 * An agent's TERMINAL frame as a row.
 *
 * Keyed by agent AND vendor record, because one agent terminates many times over
 * a conversation — every turn of the main agent ends — and a key of
 * `terminal:<agent>` alone would leave a conversation with exactly one visible
 * ending.
 */
export function terminalEntry(
  context: FoldContext,
  origin: FrameOrigin,
  result: conversationv1.AgentFrame["result"],
): PersistEntry {
  return {
    agentId: origin.agentId,
    upsertKey: terminalUpsertKey(origin.agentId, origin.vendorUuid),
    source: sourceOf(origin),
    keepalive: context.keepalive,
    turn: context.turnId,
    item: { kind: "frame", frame: agentFrame(origin.agentId, result) },
  };
}

/** A non-activity page line (a context cut, an API error, a blocking unit). */
export function pageLineEntry(
  context: FoldContext,
  origin: FrameOrigin,
  upsertKey: string,
  update: conversationv1.AgentUpdate,
): PersistEntry {
  return {
    agentId: origin.agentId,
    upsertKey,
    source: sourceOf(origin),
    keepalive: context.keepalive,
    turn: context.turnId,
    item: { kind: "frame", frame: updateFrame(origin.agentId, update) },
  };
}
