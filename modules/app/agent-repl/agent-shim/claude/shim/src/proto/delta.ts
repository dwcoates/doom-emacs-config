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
 *     signature typing), one per `content_block_delta` frame.
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
}

/**
 * Tracks which assistant message the stream is currently emitting.
 *
 * A `content_block_delta` says nothing about which message it belongs to — the
 * identity arrives once, on the `message_start` that opened it. The SDK stream
 * is continuous for the life of a shim, so the shim always sees that frame
 * before the deltas that follow it; only the DAEMON can attach mid-message,
 * and it recovers the finished message from the store instead.
 */
export class StreamMessageTracker {
  private messageId = "";

  /**
   * Observe one SDK message, updating the in-flight id. Call BEFORE converting
   * the message, so a `message_start`'s own id is already current for the
   * deltas that follow.
   */
  observe(msg: unknown): void {
    if (!isObject(msg) || msg["type"] !== "stream_event") return;
    const event = msg["event"];
    if (!isObject(event)) return;
    switch (event["type"]) {
      case "message_start": {
        const message = event["message"];
        this.messageId = isObject(message) && typeof message["id"] === "string" ? message["id"] : "";
        LOGGER.logVerbose({ message_id: this.messageId }, "stream message tracker opened assistant message");
        break;
      }
      case "message_stop":
        LOGGER.logVerbose({ message_id: this.messageId }, "stream message tracker closed assistant message");
        this.messageId = "";
        break;
      default:
        break;
    }
  }

  /** The Anthropic id of the message being streamed, or "" between messages. */
  current(): string {
    return this.messageId;
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
): Event {
  return create(EventSchema, {
    sessionId,
    seq: 0n,
    plane: Plane.STREAM,
    class: EventClass.EPHEMERAL,
    requestId: "",
    producedAtMs,
    payload,
  });
}

/**
 * Map a raw `stream_event` SDK message to an EPHEMERAL `ContentDelta` Event,
 * or `null` for a frame that is not a `content_block_delta` (structural
 * frames carry no live-typing content). Never throws: a shape it cannot read
 * yields `null` rather than a bogus delta.
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

  const event = ephemeralEvent(sessionOf(msg), producedAt(opts), {
    case: "contentDelta",
    value: create(ContentDeltaSchema, {
      uuid,
      blockIndex: index,
      delta: arm,
      estimatedTokens,
    }),
  });
  LOGGER.logVerbose({ claude_session_id: sessionOf(msg), message_id: uuid, block_index: index, delta_arm: arm.case, delta_length: arm.value.length }, "converted ephemeral content delta");
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
  });
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
