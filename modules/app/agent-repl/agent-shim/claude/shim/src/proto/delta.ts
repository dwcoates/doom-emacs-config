/**
 * The EPHEMERAL delta bypass (design §4.3, §5.2.3).
 *
 * `stream_event` partials (live typing) and `tool_progress` elapsed
 * heartbeats are the ONE class of event that MUST NOT take the store
 * round-trip: they are forwarded shim → daemon directly and never persisted
 * nor replayed. Consumers reconcile per claude `uuid`, REPLACING a streamed
 * preview with the store-delivered final message, so cross-path ordering is
 * irrelevant.
 *
 * This module maps those two SDK stream messages into their core payloads:
 *   - `stream_event` → `core.ContentDelta` (live text/thinking/tool-input/
 *     signature typing), one per `content_block_delta` frame.
 *   - `tool_progress` → `core.HeartbeatProgress`.
 * Each is wrapped in an `Event` with `class = EPHEMERAL`. The structural
 * stream_event frames (`message_start`/`content_block_start`/`_stop`/
 * `message_delta`/`message_stop`) carry no live-typing content and yield
 * `null` — there is nothing to relay ephemerally.
 *
 * ROUTING CONTRACT: these Events are returned DISTINCTLY (a dedicated entry
 * point, never mixed into {@link import("./convert.js")}'s persistent output)
 * so the session-loop wiring cannot accidentally route them to `StoreWrite`.
 * The stitch phase sends them straight to the daemon via the server. See the
 * G4 report for the exact wiring.
 */
import { create } from "@bufbuild/protobuf";
import {
  ContentDeltaSchema,
  EventClass,
  EventSchema,
  HeartbeatProgressSchema,
  Plane,
  type Event,
} from "../uds/proto.js";

/** Wall-clock injection for deterministic tests. */
export interface DeltaOptions {
  nowMs?: number;
}

function producedAt(opts: DeltaOptions | undefined): bigint {
  return BigInt(opts?.nowMs ?? Date.now());
}

function isObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
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
  const event = msg["event"];
  if (!isObject(event) || event["type"] !== "content_block_delta") return null;
  const delta = event["delta"];
  if (!isObject(delta)) return null;

  const arm = deltaArm(delta);
  if (!arm) return null;

  const uuid = typeof msg["uuid"] === "string" ? msg["uuid"] : "";
  const index = typeof event["index"] === "number" ? event["index"] : 0;
  const estimatedTokens =
    typeof delta["estimated_tokens"] === "number" && Number.isFinite(delta["estimated_tokens"])
      ? BigInt(Math.trunc(delta["estimated_tokens"] as number))
      : 0n;

  const sessionId = typeof msg["session_id"] === "string" ? msg["session_id"] : "";
  return ephemeralEvent(sessionId, producedAt(opts), {
    case: "contentDelta",
    value: create(ContentDeltaSchema, {
      uuid,
      blockIndex: index,
      delta: arm,
      estimatedTokens,
    }),
  });
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
  const sessionId = typeof msg["session_id"] === "string" ? msg["session_id"]
    : typeof msg["sessionId"] === "string" ? (msg["sessionId"] as string) : "";
  const elapsed = firstNumber(msg["elapsed_time_seconds"], msg["elapsedTimeSeconds"], msg["elapsed_seconds"]);
  return ephemeralEvent(sessionId, producedAt(opts), {
    case: "heartbeatProgress",
    value: create(HeartbeatProgressSchema, {
      toolUseId: firstString(msg["tool_use_id"], msg["toolUseId"]),
      toolName: firstString(msg["tool_name"], msg["toolName"]),
      parentToolUseId: firstString(msg["parent_tool_use_id"], msg["parentToolUseId"]),
      elapsedSeconds: elapsed,
    }),
  });
}

/**
 * Dispatcher: map any SDK message the delta bypass owns to its EPHEMERAL
 * Event, or `null` when the message is not EPHEMERAL (the persistent path in
 * {@link import("./convert.js")} owns it) or carries no relayable content.
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
