/**
 * store/keys.ts — THE one place `upsert_key` and `write_id` are minted.
 *
 * # What each key is FOR
 *
 * `upsert_key` names the THING a row is about. Every frame of one unit carries
 * the same upsert key, so a re-sent or updated frame REPLACES the row rather
 * than appending a second one — which is what makes a unit that starts, streams
 * progress, and terminates appear once in the record instead of three times.
 * The keys are a CROSS-PLANE contract agreed with the store lead: the sidecar
 * mints the same key for the same thing from the file plane, so the shim's
 * stream-plane row and the sidecar's file-plane row for one unit COLLIDE ON
 * PURPOSE and settle as one.
 *
 * `write_id` names the WRITE. It is deterministic — `sha256(producer | source
 * coordinates | discriminator)` — so re-sending a frame after a store outage
 * mints the SAME id and the store absorbs the duplicate instead of doubling the
 * row. That is the whole retry story: the writer can resend freely because
 * identity comes from the content's provenance, not from when it was sent.
 *
 * # Why every function takes a typed id message
 *
 * THE FOUR IDENTIFIER SPACES ARE NEVER INTERCHANGEABLE (standing convention):
 * the vendor's agent id names WHICH AGENT, its tool-use id names WHICH CALL,
 * our activity id names WHICH UNIT, our TurnId names WHICH TURN. A join on the
 * wrong one produces plausible, silently wrong attribution. Taking `AgentId`
 * rather than `string` makes that mistake a type error instead of a support
 * ticket, which is worth the wrapping at every call site.
 */
import { createHash } from "node:crypto";
import { bindLog } from "../log.js";
import type { conversationv1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-store-keys", operation: "shim.store.keys" });

/** Refuse an identity that is present but empty; a key built on one collides with every other. */
function requireValue(value: string, what: string): string {
  if (value === "") {
    const message = `shim store keys: ${what} is empty; a key built on an empty identity would collide with every other`;
    LOGGER.log({ level: "error", what }, message);
    throw new Error(message);
  }
  return value;
}

// ---------------------------------------------------------------------------
// producer
// ---------------------------------------------------------------------------

/**
 * This writer's name, as every row records it.
 *
 * Keyed by the ORIGINAL vendor session id — not the current one. A vendor
 * session id can rotate mid-conversation (`/clear` mints a new one), and a
 * producer name that rotated with it would make the same writer look like two,
 * splitting the deterministic write ids of one conversation across two
 * namespaces.
 */
export function producerId(originalVendorSessionId: string): string {
  return `claude-shim:${requireValue(originalVendorSessionId, "the original vendor session id")}`;
}

// ---------------------------------------------------------------------------
// upsert_key — one function per identity that owns rows
// ---------------------------------------------------------------------------

/**
 * A unit of work: every frame of it, from its first to its last.
 *
 * The activity id is the vendor `tool_use_id` for a tool call, and
 * `<message.id>:<block_index>` (0-based) for a text or thinking block.
 */
export function activityUpsertKey(unit: conversationv1.AgentActivityId): string {
  return `activity:${requireValue(unit.value, "the activity id")}`;
}

/**
 * The turn's prompt row.
 *
 * R15: this is the ONE served prompt row. The sidecar classifies the vendor's
 * own transcript user records as unserved precisely so the two do not both
 * appear in the feed.
 */
export function promptUpsertKey(turn: conversationv1.TurnId): string {
  return `prompt:${requireValue(turn.value, "the turn id")}`;
}

/** An open question to the user (the AskUserQuestion call's own tool_use_id). */
export function questionUpsertKey(question: conversationv1.AgentQuestionId): string {
  return `question:${requireValue(question.value, "the question id")}`;
}

/**
 * A permission gate (the GATED call's tool_use_id).
 *
 * Consent joins to the work it gates, which is why the id is the gated call's
 * and not one minted for the prompt. It never collides with a question key
 * because a question is never a permission's gated call.
 */
export function permissionUpsertKey(permission: conversationv1.AgentPermissionId): string {
  return `permission:${requireValue(permission.value, "the permission id")}`;
}

/**
 * An agent's terminal frame (its success or failure).
 *
 * Keyed by agent AND by the vendor record's uuid, because one agent can
 * terminate more than once over a conversation — every turn of the main agent
 * ends — and a key of `terminal:<agent>` alone would have each ending overwrite
 * the last, leaving a conversation with exactly one visible terminal.
 */
export function terminalUpsertKey(
  agent: conversationv1.AgentId,
  vendorRecordUuid: string,
): string {
  return `terminal:${requireValue(agent.value, "the agent id")}:${requireValue(
    vendorRecordUuid,
    "the vendor record uuid",
  )}`;
}

/**
 * A detached shell run's lifecycle rows, keyed by the RUN's activity id.
 *
 * The run is the bash tool call itself, so its lifecycle rows and its page line
 * are about the same unit — different key prefixes, one identity.
 */
export function bashUpsertKey(run: conversationv1.AgentActivityId): string {
  return `bash:${requireValue(run.value, "the bash run's activity id")}`;
}

/**
 * A session-level fact, keyed by WHICH KIND of fact and which vendor record
 * stated it.
 *
 * `arm` is the `SessionUpdate` oneof field name, so two different kinds of fact
 * arriving from one vendor record stay two rows. Shim-SYNTHESIZED facts
 * (`diagnostics`, `context_usage`) are never written at all — they are the
 * shim's own report about itself, not vendor conversation — so they never reach
 * this function.
 */
export function sessionUpsertKey(arm: string, vendorRecordUuid: string): string {
  return `session:${requireValue(arm, "the session update arm")}:${requireValue(
    vendorRecordUuid,
    "the vendor record uuid",
  )}`;
}

// ---------------------------------------------------------------------------
// write_id
// ---------------------------------------------------------------------------

/**
 * WHERE a frame came from in the vendor's own record.
 *
 * `blockIndex` is present exactly for a frame derived from one content block of
 * a message: without it, every block of one assistant message would share the
 * message's uuid and therefore mint one write id, and the store would absorb
 * all but the first as duplicates.
 */
export interface SourceCoordinates {
  /** The SDK message's uuid — the vendor's own name for the record. */
  readonly vendorRecordUuid: string;
  /** The 0-based content-block index, for a block-derived frame. */
  readonly blockIndex?: number;
}

/** The coordinates as they appear inside the hashed string. */
export function formatSourceCoordinates(coordinates: SourceCoordinates): string {
  const uuid = requireValue(coordinates.vendorRecordUuid, "the vendor record uuid");
  if (coordinates.blockIndex === undefined) return uuid;
  if (!Number.isInteger(coordinates.blockIndex) || coordinates.blockIndex < 0) {
    throw new Error(
      `shim store keys: block index ${String(coordinates.blockIndex)} is not a 0-based integer`,
    );
  }
  return `${uuid}:${coordinates.blockIndex}`;
}

/**
 * The deterministic identity of one write.
 *
 * `discriminator` is the frame's ARM PATH (e.g. `agent_frame.update.activity`),
 * which is what separates two frames the same vendor record produced — a tool
 * call's start arm and the session update that record also implied would
 * otherwise hash identically and one would be silently absorbed.
 */
export function writeId(
  producer: string,
  coordinates: SourceCoordinates,
  discriminator: string,
): string {
  const material = `${requireValue(producer, "the producer")}|${formatSourceCoordinates(
    coordinates,
  )}|${requireValue(discriminator, "the frame's arm path")}`;
  return createHash("sha256").update(material, "utf8").digest("hex");
}
