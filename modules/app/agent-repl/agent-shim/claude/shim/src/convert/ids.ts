/**
 * convert/ids.ts — THE one place the shim's identifiers are minted.
 *
 * THE FOUR IDENTIFIER SPACES ARE NEVER INTERCHANGEABLE (standing convention):
 * the vendor's agent id names WHICH AGENT, its tool-use id names WHICH CALL,
 * our activity id names WHICH UNIT OF WORK, our TurnId names WHICH TURN. Each
 * of those is a bare string on the vendor's side, so nothing but discipline
 * stops a join on the wrong one — and a wrong join does not crash, it produces
 * PLAUSIBLE, SILENTLY WRONG attribution: a tool result landing on another
 * call's unit, a subagent's frames drawn under the main agent.
 *
 * Every function here takes the vendor string it is entitled to and returns the
 * typed message for exactly one space. Downstream code holds typed ids from
 * that point on, so the compiler carries the discipline instead of the reader.
 */
import { createHash } from "node:crypto";
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-convert-ids", operation: "shim.convert.ids" });

/** Refuse a vendor identity that is present but empty. */
function requireVendorValue(value: string, what: string): string {
  if (value === "") {
    const message = `shim identities: ${what} is empty; an identity is never the empty string`;
    LOGGER.error({ what, detail: message }, message);
    throw new Error(message);
  }
  return value;
}

/**
 * The MAIN agent's identity: the conversation's ORIGINAL vendor session id
 * (R9).
 *
 * Original, not current. A vendor session id rotates mid-conversation (a
 * `/clear` retires the transcript identity and mints a new one, and
 * `SessionIdentityRotated` reports it), and an AgentId that rotated with it
 * would split one agent's book in two at the rotation — history before the
 * rotation would become unreachable under the name the consumer holds.
 * `engine/identity.ts` persists this value on the first fresh start and reports
 * it unchanged on every later StartSession.
 */
export function mainAgentId(originalVendorSessionId: string): conversationv1.AgentId {
  return create(conversationv1.AgentIdSchema, {
    value: requireVendorValue(originalVendorSessionId, "the original vendor session id"),
  });
}

/** A subagent's identity: the vendor's own `agent_id`, verbatim. */
export function subagentId(vendorAgentId: string): conversationv1.AgentId {
  return create(conversationv1.AgentIdSchema, {
    value: requireVendorValue(vendorAgentId, "the vendor agent id"),
  });
}

/**
 * A tool call's unit: the vendor `tool_use_id`, verbatim.
 *
 * The call's own identity is used rather than a minted one so the tool RESULT,
 * which carries the same id, joins its call by equality — the one lookup the
 * fold is allowed, and the reason it accumulates nothing.
 */
export function toolCallActivityId(toolUseId: string): conversationv1.AgentActivityId {
  return create(conversationv1.AgentActivityIdSchema, {
    value: requireVendorValue(toolUseId, "the tool use id"),
  });
}

/**
 * A text or thinking block's unit: `<message.id>:<block_index>`, 0-based.
 *
 * Blocks have no vendor id of their own, and a block is a UNIT — it starts,
 * streams deltas, and finishes — so it needs an identity stable across all
 * three. Message id plus position is the only thing that is: stable for the
 * life of the block and unique across the conversation, because a message id
 * is.
 */
export function blockActivityId(
  messageId: string,
  blockIndex: number,
): conversationv1.AgentActivityId {
  requireVendorValue(messageId, "the api message id");
  if (!Number.isInteger(blockIndex) || blockIndex < 0) {
    const message = `shim identities: block index ${String(blockIndex)} is not a 0-based integer`;
    LOGGER.error({ message_id: messageId, block_index: blockIndex, detail: message }, message);
    throw new Error(message);
  }
  return create(conversationv1.AgentActivityIdSchema, { value: `${messageId}:${blockIndex}` });
}

/**
 * An ATTACHMENT record's unit: the record's own uuid, verbatim.
 *
 * An injected memory file or skill document is an activity unit with no tool
 * call to borrow an id from and no api message id to index into — the vendor's
 * record uuid is the only identity it has, and it is stable, which is all an
 * activity id has to be. The residue paths through the attachment converter
 * never use it; it exists for the injection units, which do.
 */
export function attachmentActivityId(recordUuid: string): conversationv1.AgentActivityId {
  return create(conversationv1.AgentActivityIdSchema, {
    value: requireVendorValue(recordUuid, "the attachment record uuid"),
  });
}

/**
 * A model refusal's unit: the refusal record's own uuid, verbatim.
 *
 * A refusal that ended the response is a UNIT — it is the response, settled as
 * a failure — but the vendor writes it as its own `system` record rather than
 * as an assistant block, so there is no api message id to index into and no
 * tool-use id to borrow. The record uuid is the only identity it has, and it is
 * stable, which is all an activity id has to be.
 */
export function refusalActivityId(recordUuid: string): conversationv1.AgentActivityId {
  return create(conversationv1.AgentActivityIdSchema, {
    value: requireVendorValue(recordUuid, "the model refusal record uuid"),
  });
}

/**
 * A hook firing's unit: the vendor's `hook_id`, verbatim.
 *
 * A hook is an activity unit like any other — it starts and it settles — but it
 * is not a tool call, so it has no `tool_use_id` to borrow. The vendor's own
 * firing id is the only thing stable across its two records, which is exactly
 * what an activity id has to be.
 */
export function hookActivityId(hookId: string): conversationv1.AgentActivityId {
  return create(conversationv1.AgentActivityIdSchema, {
    value: requireVendorValue(hookId, "the hook firing id"),
  });
}

/**
 * An open question: the AskUserQuestion call's `tool_use_id`, verbatim.
 *
 * It never collides with a permission id even though both are tool-use ids: a
 * question is never a permission's GATED call, so the two draw from disjoint
 * sets of calls.
 */
export function questionId(askToolUseId: string): conversationv1.AgentQuestionId {
  return create(conversationv1.AgentQuestionIdSchema, {
    value: requireVendorValue(askToolUseId, "the AskUserQuestion tool use id"),
  });
}

/**
 * A permission gate: the GATED call's `tool_use_id`, verbatim.
 *
 * The gated call's id, not one minted for the prompt, because consent must join
 * to THE WORK IT GATES — a consumer showing an allow decision needs to draw it
 * against the call it allowed, and any other id would require a side table to
 * find it.
 */
export function permissionId(gatedToolUseId: string): conversationv1.AgentPermissionId {
  return create(conversationv1.AgentPermissionIdSchema, {
    value: requireVendorValue(gatedToolUseId, "the gated tool use id"),
  });
}

/**
 * Detached work: the SPAWNING CALL's `tool_use_id`, verbatim.
 *
 * NOT the vendor's `task_id` (ruling, landing 3). `DetachedWorkId.value` and
 * `AgentActivityId.value` are the SAME BYTES, which is what lets a terminal
 * retire a handle by equality instead of through a side table — and for a
 * subagent it is also its `AgentId`, so one identity addresses the work, its
 * unit and its book.
 *
 * The vendor's `task_id` stays SHIM-SIDE as the internal lookup for `stopTask`
 * and for the `background_tasks_changed` level; nothing task-id-shaped ever
 * goes on the wire.
 */
export function detachedWorkId(spawningToolUseId: string): conversationv1.DetachedWorkId {
  return create(conversationv1.DetachedWorkIdSchema, {
    value: requireVendorValue(spawningToolUseId, "the spawning call's tool use id"),
  });
}

/**
 * A history position, from the store's own pointer.
 *
 * PASSED THROUGH VERBATIM in both directions, and never parsed: the value is
 * the store's shape, and the shim reading it would make the store unable to
 * change it. See {@link storeItemPointerValue} for the way back.
 */
export function historyPointer(storePointerValue: string): conversationv1.HistoryPointer {
  return create(conversationv1.HistoryPointerSchema, {
    value: requireVendorValue(storePointerValue, "the store item pointer"),
  });
}

/** The store's pointer value back out of a history pointer the daemon echoed. */
export function storeItemPointerValue(pointer: conversationv1.HistoryPointer): string {
  return requireVendorValue(pointer.value, "the history pointer");
}

/**
 * THE NAMESPACE every turn's prompt uuid is derived under (RFC 4122 §4.3).
 *
 * Fixed for all time: a shim that derived under a different namespace would
 * look for its prompts under uuids no transcript holds. Minted once, at random,
 * on 2026-10-01; it names nothing else.
 */
export const TURN_PROMPT_UUID_NAMESPACE = "9f0a205c-d387-4732-826e-b46e8bdf5b78";

/** A uuid in canonical 8-4-4-4-12 hex form, any version. */
const CANONICAL_UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

/**
 * The VENDOR MESSAGE UUID a turn's prompt is sent under, derived from the turn
 * id (`StartTurnRequest.turn`, endpoint_start_turn.proto).
 *
 * A turn id that is already a uuid (a prompt the vendor recorded first) IS its
 * own vendor uuid; any other id (the daemon's 16-hex ids, the shim's own
 * `adopted-…` ids) maps to the version-5 uuid of its bytes under
 * {@link TURN_PROMPT_UUID_NAMESPACE}. Deterministic, so anyone holding the turn
 * id finds the prompt's transcript record with no mapping stored anywhere —
 * which is what `RollBackSession` stands on.
 */
export function promptVendorUuid(turnId: string): string {
  const id = requireVendorValue(turnId, "the turn id");
  if (CANONICAL_UUID.test(id)) return id;
  const namespace = Buffer.from(TURN_PROMPT_UUID_NAMESPACE.replace(/-/g, ""), "hex");
  const digest = createHash("sha1").update(namespace).update(id, "utf8").digest();
  const bytes = digest.subarray(0, 16);
  bytes[6] = (bytes[6] & 0x0f) | 0x50;
  bytes[8] = (bytes[8] & 0x3f) | 0x80;
  const hex = bytes.toString("hex");
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
}
