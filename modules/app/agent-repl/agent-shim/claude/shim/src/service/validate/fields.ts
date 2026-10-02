/**
 * service/validate/fields.ts — one validator per NON-PRIMITIVE field the shim's
 * requests carry.
 *
 * THE VALIDATION INVARIANT (standing convention): an unset non-optional field
 * is ILLEGAL, everywhere, immediately, and an unset oneof is an error by
 * default. A request carrying one is answered with `InvalidArgument` at once
 * rather than defaulted, because a default is a GUESS — and a guessed TurnId or
 * a guessed permission mode produces plausible, silently wrong behavior that
 * surfaces far from the request that caused it.
 *
 * THE PROTO→CODE MAPPING: every message gets one base function where its
 * validation lives once; every message-typed field and oneof arm gets its own
 * dedicated function delegating to the child's base. Primitives get no
 * wrappers — they are checked inline at the one site that owns them.
 *
 * `path` threads the field's position from the request root down, so a refusal
 * says `start_turn.said.content.blocks[0]` rather than "invalid request".
 */
import type { ConnectError } from "@connectrpc/connect";
import { conversationv1 } from "../../proto.js";
import { invalidArgument } from "../failures.js";

/** The refusal every validator raises. Never returned — always thrown. */
function unsetField(path: string): ConnectError {
  return invalidArgument(`${path} is unset; the field is not optional and has no legal default`);
}

/** The refusal for a oneof nobody set an arm of. */
export function unsetOneof(path: string): ConnectError {
  return invalidArgument(`${path} has no arm set; an unset oneof is an error by default`);
}

/**
 * An identity string that is present but EMPTY.
 *
 * A proto3 scalar has no presence, so "" is the only way an id can be unset —
 * and under the presence rule an empty id is a sentinel, not an absence. It is
 * refused for the same reason a missing message is.
 */
function emptyIdentity(path: string): ConnectError {
  return invalidArgument(`${path} is empty; an identity is never the empty string`);
}

/** `conversation.v1.AgentId` — WHICH AGENT. Never interchangeable with any other id space. */
export function validateAgentId(value: conversationv1.AgentId | undefined, path: string): void {
  if (value === undefined) throw unsetField(path);
  if (value.value === "") throw emptyIdentity(`${path}.value`);
}

/** `conversation.v1.TurnId` — WHICH TURN. Daemon-minted; the shim only adopts it. */
export function validateTurnId(value: conversationv1.TurnId | undefined, path: string): void {
  if (value === undefined) throw unsetField(path);
  if (value.value === "") throw emptyIdentity(`${path}.value`);
}

/** `conversation.v1.AgentActivityId` — WHICH UNIT OF WORK. */
export function validateAgentActivityId(
  value: conversationv1.AgentActivityId | undefined,
  path: string,
): void {
  if (value === undefined) throw unsetField(path);
  if (value.value === "") throw emptyIdentity(`${path}.value`);
}

/** `conversation.v1.DetachedWorkId` — the vendor's task id, verbatim. */
export function validateDetachedWorkId(
  value: conversationv1.DetachedWorkId | undefined,
  path: string,
): void {
  if (value === undefined) throw unsetField(path);
  if (value.value === "") throw emptyIdentity(`${path}.value`);
}

/**
 * `conversation.v1.HistoryPointer` — opaque in both directions.
 *
 * Validated for PRESENCE only: its value is the store's own pointer passed
 * through verbatim, and parsing it here would make the shim depend on a shape
 * the store is free to change.
 */
export function validateHistoryPointer(
  value: conversationv1.HistoryPointer | undefined,
  path: string,
): void {
  if (value === undefined) throw unsetField(path);
  if (value.value === "") throw emptyIdentity(`${path}.value`);
}

/**
 * `conversation.v1.ConversationThrough` — an inclusive bound on conversation
 * places, which must name a positive instant: a zero bound would read a book
 * as it stood before anything was said, which is a malformed request rather
 * than an empty page.
 */
export function validateConversationThrough(
  value: conversationv1.ConversationThrough | undefined,
  path: string,
): void {
  if (value === undefined) throw unsetField(path);
  if (value.atMs <= 0n) throw invalidArgument(`${path}.at_ms is ${value.atMs}; a bound on conversation places is a positive instant`);
}

/** `conversation.v1.AgentModel` — a model must be named to be selected. */
export function validateAgentModel(
  value: conversationv1.AgentModel | undefined,
  path: string,
): void {
  if (value === undefined) throw unsetField(path);
  if (value.name === "") throw invalidArgument(`${path}.name is empty; a model is named or absent`);
}

/** `conversation.v1.AgentPermissionMode` — a oneof, so every mode is a distinct arm. */
export function validateAgentPermissionMode(
  value: conversationv1.AgentPermissionMode | undefined,
  path: string,
): void {
  if (value === undefined) throw unsetField(path);
  if (value.mode.case === undefined) throw unsetOneof(`${path}.mode`);
}

/**
 * `conversation.v1.AgentEffortLevel` — an ENUM whose zero value means "unset".
 *
 * UNSPECIFIED is refused rather than mapped to any level: a guessed effort runs
 * every later turn at a reasoning budget nobody chose.
 */
export function validateAgentEffortLevel(value: conversationv1.AgentEffortLevel, path: string): void {
  if (value === conversationv1.AgentEffortLevel.UNSPECIFIED) {
    throw invalidArgument(`${path} is AGENT_EFFORT_LEVEL_UNSPECIFIED; an effort change names its level`);
  }
  if (conversationv1.AgentEffortLevel[value] === undefined) {
    throw invalidArgument(`${path} is ${value}, which is not an AgentEffortLevel value`);
  }
}

/**
 * `conversation.v1.PromptOrigin` — an ENUM, and the one place a zero value
 * means "unset".
 *
 * UNSPECIFIED is refused rather than mapped to "user sent": origin is persisted
 * with the prompt so replay can label restart re-drives and merge-born rows
 * instead of drawing them as fresh user turns. Guessing it corrupts the
 * durable record.
 */
export function validatePromptOrigin(value: conversationv1.PromptOrigin, path: string): void {
  if (value === conversationv1.PromptOrigin.UNSPECIFIED) {
    throw invalidArgument(`${path} is PROMPT_ORIGIN_UNSPECIFIED; the send site is never unstated`);
  }
}

/** `conversation.v1.UserContentBlock` — one arm per block kind. */
function validateUserContentBlock(
  value: conversationv1.UserContentBlock,
  path: string,
): void {
  if (value.block.case === undefined) throw unsetOneof(`${path}.block`);
}

/**
 * `conversation.v1.UserContent` — the blocks of one utterance.
 *
 * An EMPTY block list is refused: a prompt that says nothing has no turn to
 * open, and the vendor would receive an empty user message.
 */
export function validateUserContent(
  value: conversationv1.UserContent | undefined,
  path: string,
): void {
  if (value === undefined) throw unsetField(path);
  if (value.blocks.length === 0) {
    throw invalidArgument(`${path}.blocks is empty; an utterance with no content is not a prompt`);
  }
  value.blocks.forEach((block, index) => validateUserContentBlock(block, `${path}.blocks[${index}]`));
}

/** `conversation.v1.UserSaid` — what was said, whole. */
export function validateUserSaid(value: conversationv1.UserSaid | undefined, path: string): void {
  if (value === undefined) throw unsetField(path);
  validateUserContent(value.content, `${path}.content`);
}

/** `conversation.v1.AgentAnswer` — a question answer or a permission decision. */
function validateAgentAnswer(value: conversationv1.AgentAnswer, path: string): void {
  if (value.answer.case === undefined) throw unsetOneof(`${path}.answer`);
}

/**
 * `conversation.v1.AgentInput` — what a consumer may SAY to a live agent.
 *
 * `stop` is an empty message and is legal exactly as it arrives; the other two
 * arms delegate to their own base functions.
 */
export function validateAgentInput(
  value: conversationv1.AgentInput | undefined,
  path: string,
): void {
  if (value === undefined) throw unsetField(path);
  switch (value.input.case) {
    case "stop":
      return;
    case "answer":
      validateAgentAnswer(value.input.value, `${path}.answer`);
      return;
    case "prompt":
      validateUserSaid(value.input.value, `${path}.prompt`);
      return;
    default:
      throw unsetOneof(`${path}.input`);
  }
}

/** `conversation.v1.SessionColdRemediation` — how to pay for, or avoid, a cold context. */
export function validateSessionColdRemediation(
  value: conversationv1.SessionColdRemediation,
  path: string,
): void {
  if (value.remediation.case === undefined) throw unsetOneof(`${path}.remediation`);
  if (value.remediation.case === "compact") {
    validateAgentModel(value.remediation.value.model, `${path}.compact.model`);
  }
}

/**
 * A page budget.
 *
 * Zero is refused rather than defaulted: a page of nothing answers no question,
 * and silently substituting a default would let a caller with a bug receive a
 * page it never asked for and treat it as the whole history.
 */
export function validatePageSize(value: number, path: string): void {
  if (value === 0) {
    throw invalidArgument(`${path} is 0; a page budget of nothing is not a request`);
  }
}
