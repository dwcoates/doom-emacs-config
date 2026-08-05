/**
 * Stream relay conversion (design §4.3, §5.2.3).
 *
 * `stream_event` partials (live typing) and `tool_progress` elapsed
 * heartbeats are the event classes that MUST NOT take the store
 * round-trip: they are forwarded shim → daemon directly and never persisted
 * nor replayed. Consumers reconcile per ANTHROPIC MESSAGE ID, REPLACING a
 * streamed preview with the store-delivered final message, so cross-path
 * ordering is irrelevant.
 *
 * That id — not the SDK envelope `uuid` — is the reconciliation key, because
 * it is the only identity shared by a streaming message and its finished form.
 * The SDK mints a FRESH envelope uuid for every message it emits, including
 * every individual `stream_event`, so keying deltas on it gave each chunk a
 * different id and the frontend rendered one bubble per chunk instead of
 * growing one. The envelope uuid still identifies a finished conversation
 * ITEM (it is what both planes dedup on, and the only id user turns,
 * attachments and system lines have at all) — it simply cannot identify a
 * message that has not finished being emitted.
 *
 * This module maps those two SDK stream messages into their core payloads:
 *   - `stream_event` → `core.ContentDelta` (live text/thinking/tool-input/
 *     signature typing), one per `content_block_delta` frame. Input-json
 *     deltas carry a tool-use identity bound from `content_block_start`.
 *   - `stream_event` → `core.MessageLatency`, one per `message_start` frame
 *     that carries a `ttft_ms` stamp (the ONE progress fact a structural frame
 *     carries; see {@link streamEventToMessageLatency}). Unlike deltas, this
 *     is durable so restored consumers can derive response timing.
 *   - `tool_progress` → `core.HeartbeatProgress`.
 * ContentDelta and HeartbeatProgress are wrapped in an `Event` with
 * `class = EPHEMERAL`. MessageLatency is PERSISTENT. The remaining
 * structural stream_event frames (`content_block_start`/`_stop`/
 * `message_delta`/`message_stop`) carry nothing relayable and yield `null`.
 *
 * ROUTING CONTRACT: the two event classes have distinct entry points, so the
 * session loop sends typing and heartbeat frames directly to the daemon while
 * it writes MessageLatency through `StoreWrite` before replaying it. See the
 * G4 report for the exact wiring.
 */
import { create } from "@bufbuild/protobuf";
import {
  ContentDeltaSchema,
  EventClass,
  EventSchema,
  HeartbeatProgressSchema,
  MessageLatencySchema,
  Plane,
  type Event,
} from "../uds/proto.js";
import { bindLog } from "../uds/log.js";

const LOGGER = bindLog({ component: "claude-shim-delta", operation: "shim.delta.convert" });

/** Wall-clock injection for deterministic tests. */
export interface DeltaOptions {
  nowMs?: number;
  /**
   * The Anthropic id of the message currently being streamed, from the
   * `message_start` that opened it (see {@link StreamMessageTracker}). Every
   * delta of one message carries the same value, which is what lets consumers
   * grow a single block instead of opening a new one per chunk.
   */
  messageId?: string;
  /**
   * The query() invocation the shim was running when the frame arrived,
   * stamped onto the envelope this mapper builds. The mapper is part of the
   * PRODUCER, so the fact is recorded here rather than inferred downstream
   * from how the event was delivered. See core.proto's contract.
   */
  queryInstanceId?: string;
  /**
   * The tool-use identity bound to this API content block by its preceding
   * `content_block_start`. It is required for `input_json_delta` because a
   * block ordinal cannot identify a durable tool after cross-plane ordering.
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
 * Only the DAEMON can attach mid-message, and it recovers the finished
 * message from the store instead.
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
      delta_arm: "input_json",
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

function producedAt(opts: DeltaOptions | undefined): bigint {
  return BigInt(opts?.nowMs ?? Date.now());
}

function isObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

/**
 * Unwrap a `stream_event` envelope to its inner frame, when that frame is of
 * `type`; `null` for any other frame, or a shape this cannot read.
 *
 * Every stream mapper below is keyed to exactly ONE frame type — that is what
 * makes them mutually exclusive and lets the dispatcher try them in turn — so
 * this unwrap-and-discriminate is the whole of what they have in common. One
 * site owns how the envelope is read, so a third mapper (or a change in the
 * envelope's shape) lands in one place rather than being copied a third time.
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

function ephemeralEvent(
  sessionId: string,
  producedAtMs: bigint,
  payload: Event["payload"],
  queryInstanceId: string,
): Event {
  return create(EventSchema, {
    sessionId,
    seq: 0n,
    plane: Plane.STREAM,
    class: EventClass.EPHEMERAL,
    requestId: "",
    queryInstanceId,
    producedAtMs,
    payload,
  });
}

/**
 * Map a raw `stream_event` SDK message to an EPHEMERAL `ContentDelta` Event,
 * or `null` for a frame that is not a `content_block_delta` (structural
 * frames carry no live-typing content). A shape it cannot read yields `null`
 * rather than a bogus delta. An `input_json_delta` without the preceding
 * tool-use binding is an invariant violation and throws before constructing
 * an event.
 */
export function streamEventToContentDelta(
  msg: Record<string, unknown>,
  opts?: DeltaOptions,
): Event | null {
  const frame = frameOfType(msg, "content_block_delta");
  if (!frame) return null;
  const delta = frame["delta"];
  if (!isObject(delta)) return null;

  const arm = deltaArm(delta);
  if (!arm) return null;

  // The message being streamed, NOT this event's own envelope uuid: the SDK
  // mints that fresh per emission, so it differs on every chunk.
  const uuid = opts?.messageId ?? "";
  const index = typeof frame["index"] === "number" ? frame["index"] : 0;
  const estimatedTokens =
    typeof delta["estimated_tokens"] === "number" && Number.isFinite(delta["estimated_tokens"])
      ? BigInt(Math.trunc(delta["estimated_tokens"] as number))
      : 0n;

  if (arm.case === "inputJson" && (typeof opts?.toolUseId !== "string" || opts.toolUseId.length === 0)) {
    LOGGER.log({
      level: "error",
      claude_session_id: sessionOf(msg),
      agent_repl_session_id: opts?.agentReplSessionId ?? null,
      api_message_id: uuid || null,
      block_index: index,
      tool_use_id: null,
      delta_length: arm.value.length,
      delta_arm: arm.case,
      failed_operation: "stream_input_json_delta_conversion",
      outcome: "missing_tool_use_id_binding",
    }, "input_json delta has no bound tool-use identity");
    throw new Error("input_json delta has no bound tool-use identity");
  }

  const event = ephemeralEvent(sessionOf(msg), producedAt(opts), {
    case: "contentDelta",
    value: create(ContentDeltaSchema, {
      uuid,
      blockIndex: index,
      ...(arm.case === "inputJson" ? { toolUseId: opts!.toolUseId } : {}),
      delta: arm,
      estimatedTokens,
    }),
  }, opts?.queryInstanceId ?? "");
  LOGGER.logVerbose({ claude_session_id: sessionOf(msg), agent_repl_session_id: opts?.agentReplSessionId ?? null, api_message_id: uuid, block_index: index, tool_use_id: arm.case === "inputJson" ? opts!.toolUseId : null, delta_arm: arm.case, delta_length: arm.value.length }, "converted ephemeral content delta");
  return event;
}

/**
 * Map a raw `stream_event` SDK message to a PERSISTENT `MessageLatency` Event,
 * or `null` for a frame that is not a `message_start` or that carries no
 * `ttft_ms` stamp.
 *
 * WHY THIS FRAME. `ttft_ms` is a top-level field of the `stream_event`
 * envelope, and the SDK stamps it on the `message_start` that OPENS a streamed
 * assistant message — never on the `content_block_delta` chunks that follow.
 * So the one stream frame carrying first-token latency is precisely a frame
 * {@link streamEventToContentDelta} drops. Persisting it as its own payload
 * makes the latency available both mid-turn and after a daemon restart: the
 * only other place the number appears is the turn's terminal result message,
 * which arrives when the turn is already over.
 *
 * The two mappers are mutually exclusive by construction — a `message_start`
 * never yields a ContentDelta and a `content_block_delta` never yields a
 * MessageLatency — so the session router can classify them separately.
 *
 * Never throws: a shape it cannot read, or an absent/unusable stamp, yields
 * `null` rather than a MessageLatency reporting a latency nobody measured.
 */
export function streamEventToMessageLatency(
  msg: Record<string, unknown>,
  opts?: DeltaOptions,
): Event | null {
  if (!frameOfType(msg, "message_start")) return null;
  const ttft = msg["ttft_ms"];
  if (typeof ttft !== "number" || !Number.isFinite(ttft) || ttft <= 0) return null;

  const event = create(EventSchema, {
    sessionId: sessionOf(msg),
    seq: 0n,
    plane: Plane.STREAM,
    class: EventClass.PERSISTENT,
    queryInstanceId: opts?.queryInstanceId ?? "",
    producedAtMs: producedAt(opts),
    payload: {
      case: "messageLatency",
      value: create(MessageLatencySchema, {
        // The message this stamp measures, keyed exactly as ContentDelta keys
        // its own chunks (see StreamMessageTracker), NOT the envelope uuid.
        uuid: opts?.messageId ?? "",
        ttftMs: BigInt(Math.trunc(ttft)),
      }),
    },
  });
  LOGGER.logVerbose({ claude_session_id: sessionOf(msg), message_id: opts?.messageId ?? "", ttft_ms: ttft }, "converted persistent message latency");
  return event;
}

/** The ContentDelta oneof arm for one `content_block_delta.delta`, or null. */
function deltaArm(delta: Record<string, unknown>): ContentDeltaArm | null {
  switch (delta["type"]) {
    case "text_delta":
      return { case: "text", value: strOf(delta["text"]) };
    case "thinking_delta":
      return { case: "thinking", value: strOf(delta["thinking"]) };
    case "input_json_delta":
      return { case: "inputJson", value: strOf(delta["partial_json"]) };
    case "signature_delta":
      return { case: "signature", value: strOf(delta["signature"]) };
    default:
      return null;
  }
}

type ContentDeltaArm =
  | { case: "text"; value: string }
  | { case: "thinking"; value: string }
  | { case: "inputJson"; value: string }
  | { case: "signature"; value: string };

/**
 * Map a raw `tool_progress` SDK message to an EPHEMERAL `HeartbeatProgress`
 * Event. Reads both the stream (`tool_use_id`, `elapsed_time_seconds`) and any
 * camelCase disk twin defensively; missing fields coerce to zero values.
 */
export function toolProgressToHeartbeat(
  msg: Record<string, unknown>,
  opts?: DeltaOptions,
): Event {
  const sessionId = firstString(msg["session_id"], msg["sessionId"]);
  const elapsed = firstNumber(msg["elapsed_time_seconds"], msg["elapsedTimeSeconds"], msg["elapsed_seconds"]);
  const event = ephemeralEvent(sessionId, producedAt(opts), {
    case: "heartbeatProgress",
    value: create(HeartbeatProgressSchema, {
      toolUseId: firstString(msg["tool_use_id"], msg["toolUseId"]),
      toolName: firstString(msg["tool_name"], msg["toolName"]),
      parentToolUseId: firstString(msg["parent_tool_use_id"], msg["parentToolUseId"]),
      elapsedSeconds: elapsed,
    }),
  }, opts?.queryInstanceId ?? "");
  LOGGER.logVerbose({ claude_session_id: sessionId, elapsed_seconds: elapsed }, "converted ephemeral tool heartbeat");
  return event;
}

/**
 * Map one SDK message to an EPHEMERAL direct-delivery Event, or `null` when
 * it carries no direct-delivery content. MessageLatency deliberately does not
 * pass through here because it must be stored and replayed.
 */
export function toEphemeralEvent(msg: unknown, opts?: DeltaOptions): Event | null {
  if (!isObject(msg)) return null;
  switch (msg["type"]) {
    case "stream_event":
      return streamEventToContentDelta(msg, opts);
    case "tool_progress":
      return toolProgressToHeartbeat(msg, opts);
    default:
      return null;
  }
}

/**
 * Map one SDK message to its PERSISTENT structural relay, or `null` when no
 * durable relay applies. The session loop writes this result through the
 * store, whose serial write chain preserves its order before later terminal
 * events and whose replay makes it available after a daemon restart.
 */
export function toPersistentEvent(msg: unknown, opts?: DeltaOptions): Event | null {
  if (!isObject(msg) || msg["type"] !== "stream_event") return null;
  return streamEventToMessageLatency(msg, opts);
}

/** True iff the delta bypass — not the persistent converter — owns `msg`. */
export function isEphemeral(msg: unknown): boolean {
  return isObject(msg) && (msg["type"] === "stream_event" || msg["type"] === "tool_progress");
}

function strOf(v: unknown): string {
  return typeof v === "string" ? v : "";
}

function firstString(...vs: unknown[]): string {
  for (const v of vs) if (typeof v === "string") return v;
  return "";
}

function firstNumber(...vs: unknown[]): number {
  for (const v of vs) if (typeof v === "number" && Number.isFinite(v)) return v;
  return 0;
}
