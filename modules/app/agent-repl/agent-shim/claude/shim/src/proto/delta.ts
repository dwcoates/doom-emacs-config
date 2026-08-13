/**
 * The STREAM PLANE's live relay, and the only place this shim produces a
 * `conversation.v1.MessageEntry`.
 *
 * # What the stream plane may and may not say
 *
 * The shim watches the SDK as it runs, so it is first to know and it is the
 * only source for anything not yet written to disk. It is authoritative for
 * session and turn LIFECYCLE. It is NOT authoritative for conversation
 * CONTENT — the file plane is, because that is what the vendor itself
 * recorded — and the shim cannot write settled content at all: a `MessageEntry`
 * must state its `parent`, and the SDK stream carries no parent pointer.
 *
 * The ONE exception is the live preview. `ContentArriving` is defined as
 * "handed straight to the daemon by the stream plane", and the completed
 * message the file plane writes later REPLACES it rather than appending beside
 * it. Both producers must therefore derive `message_id` from the same vendor
 * value, which is the ANTHROPIC MESSAGE ID (`msg_…`) — not the SDK envelope
 * `uuid`, which the SDK mints fresh for every emission including every
 * individual `stream_event`, so keying on it gave each chunk its own id and the
 * frontend grew one bubble per chunk.
 *
 * # Which route each relay takes
 *
 *   - `stream_event` / `content_block_delta` → `ContentArriving`, delivered
 *     LIVE. Never written, so never positioned, so a consumer has no field in
 *     which to advance a resume cursor past it.
 *   - `stream_event` / `message_start` carrying `ttft_ms` → `ResponseTiming`,
 *     which is DURABLE: a daemon that restarted mid-turn must rebuild the same
 *     latency from replay rather than lose it. The only other place the number
 *     appears is the turn's terminal result, which arrives when the turn is
 *     already over.
 *   - `tool_progress` → `AgentHeartbeat`, delivered live. It reports liveness and
 *     never content.
 *
 * The remaining structural frames (`content_block_start` / `_stop`,
 * `message_delta`, `message_stop`) carry nothing relayable and yield `null`.
 *
 * # What a preview must refuse to invent
 *
 * `MessageParent` is a oneof precisely so "this is a feed row" and "the producer
 * could not resolve a parent" cannot wear the same value. A preview of a
 * MAIN-conversation response is legitimately a feed row and says `root`. A
 * preview of SUBAGENT content sits INSIDE the detached work that spawned it,
 * and the containing message's id is minted by the file plane from the vendor
 * uuid of the tool-use record — a value the stream never carries. So a subagent
 * preview has no legal record to occupy and is REFUSED loudly rather than
 * emitted as a root, which would render the subagent's typing as a new
 * top-level row with nothing detecting it.
 */
import { create } from "@bufbuild/protobuf";
import {
  AuthorAgentSchema,
  BookkeepingEntrySchema,
  ContentArrivingSchema,
  EntrySchema,
  ExternalEntrySchema,
  AgentHeartbeatSchema,
  InternalEntrySchema,
  MessageAuthorSchema,
  MessageEntrySchema,
  MessageParentRootSchema,
  MessageParentSchema,
  PlaneSchema,
  PlaneStreamSchema,
  ResponseTimingSchema,
  type Entry,
  type ExternalEntry,
} from "../uds/proto.js";
import { bindLog } from "../uds/log.js";

const LOGGER = bindLog({ component: "claude-shim-delta", operation: "shim.delta.convert" });

/** Wall-clock injection for deterministic tests. */
export interface DeltaOptions {
  nowMs?: number;
  /**
   * The Anthropic id of the message currently being streamed, from the
   * `message_start` that opened it (see {@link StreamMessageTracker}). Every
   * fragment of one message carries the same value, which is what lets a
   * consumer grow one preview instead of opening a new one per chunk — and what
   * lets the file plane's settled message REPLACE that preview.
   */
  messageId?: string;
  /**
   * The tool-use identity bound to this API content block by its preceding
   * `content_block_start`.
   *
   * NOTHING CARRIES IT ANY MORE — `ContentArriving` identifies a fragment by
   * `block_index` alone — but the BINDING is still required for an
   * `arguments_json` fragment and its absence is still an invariant violation.
   * A stream that emits tool-argument chunks with no opening tool block is
   * malformed, and that was always what this check detected; dropping the check
   * because the field it used to fill is gone would trade a loud failure for a
   * silently wrong preview.
   */
  toolUseId?: string;
  /** Agent-repl session identity supplied by the owning UDS session. */
  agentReplSessionId?: string;
}

/**
 * Tracks the assistant message and tool identities the stream is emitting.
 *
 * A `content_block_delta` says nothing about which message it belongs to — the
 * identity arrives once, on the `message_start` that opened it. The SDK stream
 * is continuous for the life of a shim, so the shim always sees that frame
 * before the deltas that follow it; each tool `content_block_start` similarly
 * binds its API block index to the durable tool-use id before input chunks.
 * Only the DAEMON can attach mid-message, and it recovers the finished message
 * from the store instead.
 */
export class StreamMessageTracker {
  private messageId = "";
  private readonly toolUseIdsByBlockIndex = new Map<number, string>();

  /**
   * Observe one SDK message, updating the in-flight id. Call BEFORE converting
   * the message, so a `message_start`'s own id is already current for the
   * deltas that follow.
   */
  observe(msg: unknown, agentReplSessionId?: string): void {
    if (!isObject(msg) || msg["type"] !== "stream_event") return;
    const event = msg["event"];
    if (!isObject(event)) return;
    switch (event["type"]) {
      case "message_start": {
        const message = event["message"];
        this.messageId = isObject(message) && typeof message["id"] === "string" ? message["id"] : "";
        this.toolUseIdsByBlockIndex.clear();
        LOGGER.logVerbose({ message_id: this.messageId }, "stream message tracker opened assistant message");
        break;
      }
      case "content_block_start":
        this.observeContentBlockStart(msg, event, agentReplSessionId);
        break;
      case "message_stop":
        LOGGER.logVerbose({ message_id: this.messageId }, "stream message tracker closed assistant message");
        this.messageId = "";
        this.toolUseIdsByBlockIndex.clear();
        break;
      default:
        break;
    }
  }

  /** The Anthropic id of the message being streamed, or "" between messages. */
  current(): string {
    return this.messageId;
  }

  /** Return the bound tool-use identity for an input delta's API block. */
  toolUseIdFor(msg: unknown, agentReplSessionId?: string): string | undefined {
    if (!isObject(msg) || msg["type"] !== "stream_event") return undefined;
    const frame = frameOfType(msg, "content_block_delta");
    if (!frame || !isObject(frame["delta"]) || frame["delta"]["type"] !== "input_json_delta") return undefined;
    const index = frame["index"];
    if (typeof index !== "number" || !Number.isInteger(index) || index < 0) {
      LOGGER.logVerbose({
        agent_repl_session_id: agentReplSessionId ?? null,
        claude_session_id: sessionOf(msg),
        api_message_id: this.messageId || null,
        block_index: index,
        tool_use_id: null,
        outcome: "invalid_input_delta_block_index",
      }, "input-json delta has an invalid API block index for identity lookup");
      return undefined;
    }
    const toolUseId = this.toolUseIdsByBlockIndex.get(index);
    LOGGER.logVerbose({
      agent_repl_session_id: agentReplSessionId ?? null,
      claude_session_id: sessionOf(msg),
      api_message_id: this.messageId || null,
      block_index: index,
      tool_use_id: toolUseId ?? null,
      outcome: toolUseId === undefined ? "missing_tool_use_id_binding" : "resolved_tool_use_id_binding",
    }, "looked up input-json delta tool-use identity");
    return toolUseId;
  }

  private observeContentBlockStart(
    msg: Record<string, unknown>,
    event: Record<string, unknown>,
    agentReplSessionId?: string,
  ): void {
    const contentBlock = event["content_block"];
    const blockIndex = event["index"];
    const blockType = isObject(contentBlock) && typeof contentBlock["type"] === "string"
      ? contentBlock["type"]
      : null;
    const classificationContext = {
      agent_repl_session_id: agentReplSessionId ?? null,
      claude_session_id: sessionOf(msg),
      api_message_id: this.messageId || null,
      block_index: blockIndex,
      content_block_type: blockType,
    };
    if (!isObject(contentBlock)) {
      LOGGER.logVerbose({ ...classificationContext, outcome: "ignored_non_object_content_block" }, "ignored non-object content block start");
      return;
    }
    if (contentBlock["type"] !== "tool_use") {
      LOGGER.logVerbose({ ...classificationContext, outcome: "ignored_non_tool_content_block" }, "ignored non-tool content block start");
      return;
    }
    LOGGER.logVerbose({ ...classificationContext, outcome: "tool_use_content_block" }, "classified tool-use content block start");

    const toolUseId = contentBlock["id"];
    const baseContext = {
      agent_repl_session_id: agentReplSessionId ?? null,
      claude_session_id: sessionOf(msg),
      api_message_id: this.messageId || null,
      block_index: blockIndex,
      tool_use_id: typeof toolUseId === "string" && toolUseId.length > 0 ? toolUseId : null,
      delta_length: null,
      delta_arm: "arguments_json",
    };
    if (this.messageId.length === 0) {
      this.failIdentityInvariant(baseContext, "missing_api_message_id_at_tool_block_start", "tool block start has no active API message identity");
    }
    if (typeof blockIndex !== "number" || !Number.isInteger(blockIndex) || blockIndex < 0) {
      this.failIdentityInvariant(baseContext, "invalid_tool_block_index", "tool block start has an invalid API block index");
    }
    if (typeof toolUseId !== "string" || toolUseId.length === 0) {
      this.failIdentityInvariant(baseContext, "missing_tool_use_id_at_tool_block_start", "tool block start has no tool-use identity");
    }

    const existing = this.toolUseIdsByBlockIndex.get(blockIndex);
    if (existing !== undefined && existing !== toolUseId) {
      this.failIdentityInvariant({ ...baseContext, bound_tool_use_id: existing }, "conflicting_tool_use_id_at_block_index", "tool block redelivery conflicts with the bound tool-use identity");
    }
    if (existing === toolUseId) {
      LOGGER.logVerbose({ ...baseContext, outcome: "redelivery_preserved_binding" }, "tool block redelivery preserved identity binding");
      return;
    }
    this.toolUseIdsByBlockIndex.set(blockIndex, toolUseId);
    LOGGER.logVerbose({ ...baseContext, outcome: "bound_tool_use_id_to_block_index" }, "bound tool-use identity to API content block");
  }

  private failIdentityInvariant(context: Record<string, unknown>, outcome: string, message: string): never {
    LOGGER.log({
      level: "error",
      ...context,
      outcome,
      failed_operation: "stream_tool_identity_binding",
    }, message);
    throw new Error(`${message}: ${outcome}`);
  }
}

// ---------------------------------------------------------------------------
// Envelope construction
// ---------------------------------------------------------------------------

function producedAt(opts: DeltaOptions | undefined): bigint {
  return BigInt(opts?.nowMs ?? Date.now());
}

/**
 * The half of a record allowed to leave the shim: which conversation, when the
 * producer observed it, and one of the two arms.
 *
 * `produced_at_ms` sits here rather than on `MessageEntry` because a
 * bookkeeping entry happened at a moment too — a turn boundary has a time
 * whether or not anything draws it.
 */
function externalEntry(
  sessionId: string,
  producedAtMs: bigint,
  entry: ExternalEntry["entry"],
): ExternalEntry {
  return create(ExternalEntrySchema, { sessionId, producedAtMs, entry });
}

/**
 * A record the store will hold: the internal half (which plane observed it)
 * plus the external half the daemon receives by field access.
 *
 * `write_id` is left empty for the store client to mint ONCE, because a replay
 * must re-present the identity the record was first delivered under.
 */
function storedEntry(external: ExternalEntry): Entry {
  return create(EntrySchema, {
    internal: create(InternalEntrySchema, {
      plane: create(PlaneSchema, { plane: { case: "stream", value: create(PlaneStreamSchema, {}) } }),
    }),
    external,
  });
}

// ---------------------------------------------------------------------------
// stream_event → ContentArriving (LIVE)
// ---------------------------------------------------------------------------

/**
 * Map a raw `stream_event` SDK message to the live `ContentArriving` record, or
 * `null` when the frame carries no relayable fragment.
 *
 * Returns the record's EXTERNAL half; the caller wraps it in a
 * `LiveEntryDelivery`, which has no field a store position could go in.
 *
 * REFUSALS, each loud and each for a different reason:
 *   - no in-flight message id: the preview could name no message, so the
 *     settled message could never replace it and it would grow forever.
 *   - a `parent_tool_use_id`: subagent content, whose containing message id the
 *     stream does not carry (see the file header).
 *   - an `arguments_json` fragment with no bound tool block: a malformed
 *     stream, which THROWS rather than returning null, exactly as before.
 */
export function streamEventToContentArriving(
  msg: Record<string, unknown>,
  opts?: DeltaOptions,
): ExternalEntry | null {
  const frame = frameOfType(msg, "content_block_delta");
  if (!frame) return null;
  const delta = frame["delta"];
  if (!isObject(delta)) return null;

  const fragment = fragmentOf(delta);
  if (!fragment) return null;

  const messageId = opts?.messageId ?? "";
  const blockIndex = typeof frame["index"] === "number" && Number.isInteger(frame["index"]) && frame["index"] >= 0
    ? frame["index"]
    : 0;

  if (fragment.case === "argumentsJson" && (typeof opts?.toolUseId !== "string" || opts.toolUseId.length === 0)) {
    LOGGER.log({
      level: "error",
      claude_session_id: sessionOf(msg),
      agent_repl_session_id: opts?.agentReplSessionId ?? null,
      api_message_id: messageId || null,
      block_index: blockIndex,
      tool_use_id: null,
      delta_length: fragment.value.length,
      delta_arm: fragment.case,
      failed_operation: "stream_arguments_fragment_conversion",
      outcome: "missing_tool_use_id_binding",
    }, "arguments_json fragment has no bound tool-use identity");
    throw new Error("arguments_json fragment has no bound tool-use identity");
  }

  if (messageId === "") {
    LOGGER.log({
      level: "error",
      claude_session_id: sessionOf(msg),
      agent_repl_session_id: opts?.agentReplSessionId ?? null,
      block_index: blockIndex,
      delta_arm: fragment.case,
      failed_operation: "stream_content_arriving_conversion",
      outcome: "unattributed_fragment",
    }, "content fragment arrived with no in-flight message id and was not relayed");
    return null;
  }

  const parentToolUseId = firstString(msg["parent_tool_use_id"], msg["parentToolUseId"]);
  if (parentToolUseId !== "") {
    LOGGER.log({
      level: "error",
      claude_session_id: sessionOf(msg),
      agent_repl_session_id: opts?.agentReplSessionId ?? null,
      api_message_id: messageId,
      block_index: blockIndex,
      delta_arm: fragment.case,
      parent_tool_use_id: parentToolUseId,
      failed_operation: "stream_content_arriving_conversion",
      outcome: "unresolvable_detached_parent",
    }, "subagent content fragment names a parent tool call whose containing message id the stream does not carry; refused rather than emitted as a feed row");
    return null;
  }

  const external = externalEntry(sessionOf(msg), producedAt(opts), {
    case: "message",
    value: create(MessageEntrySchema, {
      messageId,
      // A main-conversation response IS its own feed row, so the row it belongs
      // to is itself. Storing it is why a page of ten messages costs one pass.
      topLevelMessageId: messageId,
      parent: create(MessageParentSchema, {
        parent: { case: "root", value: create(MessageParentRootSchema, {}) },
      }),
      // Resolved by the producer, never inferred by a reader from the arm.
      author: create(MessageAuthorSchema, {
        author: { case: "agent", value: create(AuthorAgentSchema, {}) },
      }),
      payload: {
        case: "contentArriving",
        value: create(ContentArrivingSchema, { blockIndex, fragment }),
      },
    }),
  });
  LOGGER.logVerbose({
    claude_session_id: sessionOf(msg),
    agent_repl_session_id: opts?.agentReplSessionId ?? null,
    api_message_id: messageId,
    block_index: blockIndex,
    delta_arm: fragment.case,
    delta_length: fragment.value.length,
  }, "converted live content fragment");
  return external;
}

/** The `ContentArriving.fragment` arm for one `content_block_delta.delta`. */
function fragmentOf(delta: Record<string, unknown>): ContentFragment | null {
  switch (delta["type"]) {
    case "text_delta":
      return { case: "text", value: strOf(delta["text"]) };
    case "thinking_delta":
      return { case: "thinking", value: strOf(delta["thinking"]) };
    case "input_json_delta":
      // A string because it is INCOMPLETE JSON until the last fragment lands;
      // typing it as Struct would claim it parses when it does not yet.
      return { case: "argumentsJson", value: strOf(delta["partial_json"]) };
    default:
      // `signature_delta` lands here. The neutral content model has no arm for
      // it and `ThinkingBlock` carries no signature, so there is nothing to
      // relay it into and nothing downstream that could use it.
      return null;
  }
}

type ContentFragment =
  | { case: "text"; value: string }
  | { case: "thinking"; value: string }
  | { case: "argumentsJson"; value: string };

// ---------------------------------------------------------------------------
// stream_event → ResponseTiming (DURABLE)
// ---------------------------------------------------------------------------

/**
 * Map a raw `stream_event` SDK message to a DURABLE `ResponseTiming` record, or
 * `null` for a frame that is not a `message_start` or that carries no usable
 * `ttft_ms` stamp.
 *
 * WHY THIS FRAME. `ttft_ms` is a top-level field of the `stream_event` envelope
 * and the SDK stamps it on the `message_start` that OPENS a streamed assistant
 * message — never on the `content_block_delta` chunks that follow. So the one
 * stream frame carrying first-token latency is precisely a frame
 * {@link streamEventToContentArriving} drops.
 *
 * It is BOOKKEEPING, not a message: it measures the conversation rather than
 * participating in it, which is why it may name a message without ever
 * spending a page slot.
 *
 * `total_ms` is deliberately left at zero here: this frame measures only the
 * time to the FIRST token, and claiming a total from it would report a
 * number nobody measured.
 *
 * Never throws: a shape it cannot read, or an absent/unusable stamp, yields
 * `null` rather than a timing nobody observed.
 */
export function streamEventToResponseTiming(
  msg: Record<string, unknown>,
  opts?: DeltaOptions,
): Entry | null {
  if (!frameOfType(msg, "message_start")) return null;
  const ttft = msg["ttft_ms"];
  if (typeof ttft !== "number" || !Number.isFinite(ttft) || ttft <= 0) return null;

  // A timing that cannot name the message it measures is not evidence.
  const messageId = opts?.messageId ?? "";
  if (messageId === "") {
    LOGGER.log({
      level: "error",
      claude_session_id: sessionOf(msg),
      agent_repl_session_id: opts?.agentReplSessionId ?? null,
      ttft_ms: ttft,
      failed_operation: "stream_response_timing_conversion",
      outcome: "unattributed_timing",
    }, "message_start latency arrived with no in-flight message id and was not relayed");
    return null;
  }

  const entry = storedEntry(externalEntry(sessionOf(msg), producedAt(opts), {
    case: "bookkeeping",
    value: create(BookkeepingEntrySchema, {
      kind: {
        case: "responseTiming",
        value: create(ResponseTimingSchema, {
          messageId,
          firstTokenMs: BigInt(Math.trunc(ttft)),
        }),
      },
    }),
  }));
  LOGGER.logVerbose({ claude_session_id: sessionOf(msg), message_id: messageId, ttft_ms: ttft },
    "converted durable response timing");
  return entry;
}

// ---------------------------------------------------------------------------
// tool_progress → AgentHeartbeat (LIVE)
// ---------------------------------------------------------------------------

/**
 * Map a raw `tool_progress` SDK message to a live `AgentHeartbeat`.
 *
 * `AgentHeartbeat` reports liveness and NEVER content: IDENTITIES rather than a
 * count, because a count cannot be reconciled against what a feed is showing
 * and a set can. The tool's own use-id is the one identity here — the tool
 * NAME, the parent tool call and the elapsed seconds the SDK also reports have
 * no field on this record.
 *
 * Returns `null` when the frame names no live work: a heartbeat with an empty
 * id set asserts "nothing is running", which is a different and much stronger
 * claim than "this frame told us nothing".
 */
export function toolProgressToHeartbeat(
  msg: Record<string, unknown>,
  opts?: DeltaOptions,
): ExternalEntry | null {
  const sessionId = firstString(msg["session_id"], msg["sessionId"]);
  const toolUseId = firstString(msg["tool_use_id"], msg["toolUseId"]);
  if (toolUseId === "") {
    LOGGER.log({
      level: "error",
      claude_session_id: sessionId,
      agent_repl_session_id: opts?.agentReplSessionId ?? null,
      failed_operation: "tool_progress_heartbeat_conversion",
      outcome: "unidentified_live_work",
    }, "tool_progress named no tool-use id, so it could assert no live work and was not relayed");
    return null;
  }
  const external = externalEntry(sessionId, producedAt(opts), {
    case: "bookkeeping",
    value: create(BookkeepingEntrySchema, {
      kind: { case: "heartbeat", value: create(AgentHeartbeatSchema, { liveWorkIds: [toolUseId] }) },
    }),
  });
  LOGGER.logVerbose({ claude_session_id: sessionId, tool_use_id: toolUseId }, "converted live tool heartbeat");
  return external;
}

// ---------------------------------------------------------------------------
// Dispatch
// ---------------------------------------------------------------------------

/**
 * Map one SDK message to the record it hands STRAIGHT to the daemon, or `null`
 * when it carries none. The caller wraps the result in a `LiveEntryDelivery`.
 *
 * `ResponseTiming` deliberately does not pass through here because it must be
 * stored and replayed.
 */
export function toLiveEntry(msg: unknown, opts?: DeltaOptions): ExternalEntry | null {
  if (!isObject(msg)) return null;
  switch (msg["type"]) {
    case "stream_event":
      return streamEventToContentArriving(msg, opts);
    case "tool_progress":
      return toolProgressToHeartbeat(msg, opts);
    default:
      return null;
  }
}

/**
 * Map one SDK message to its DURABLE stream-plane relay, or `null` when none
 * applies. The session loop writes this through the store, whose serial write
 * chain preserves its order before later terminal records and whose replay
 * makes it available after a daemon restart.
 */
export function toStoredStreamEntry(msg: unknown, opts?: DeltaOptions): Entry | null {
  if (!isObject(msg) || msg["type"] !== "stream_event") return null;
  return streamEventToResponseTiming(msg, opts);
}

/** True iff the live relay — not the persistent converter — owns `msg`. */
export function isEphemeral(msg: unknown): boolean {
  return isObject(msg) && (msg["type"] === "stream_event" || msg["type"] === "tool_progress");
}

// ---------------------------------------------------------------------------
// Shape helpers
// ---------------------------------------------------------------------------

function isObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

/**
 * Unwrap a `stream_event` envelope to its inner frame, when that frame is of
 * `type`; `null` for any other frame, or a shape this cannot read.
 *
 * Every stream mapper below is keyed to exactly ONE frame type — that is what
 * makes them mutually exclusive and lets the dispatcher try them in turn — so
 * this unwrap-and-discriminate is the whole of what they have in common.
 */
function frameOfType(msg: Record<string, unknown>, type: string): Record<string, unknown> | null {
  const event = msg["event"];
  if (!isObject(event) || event["type"] !== type) return null;
  return event;
}

/** The session id off a stream message's envelope, or "" when it has none. */
function sessionOf(msg: Record<string, unknown>): string {
  return strOf(msg["session_id"]);
}

function strOf(v: unknown): string {
  return typeof v === "string" ? v : "";
}

function firstString(...vs: unknown[]): string {
  for (const v of vs) if (typeof v === "string") return v;
  return "";
}
