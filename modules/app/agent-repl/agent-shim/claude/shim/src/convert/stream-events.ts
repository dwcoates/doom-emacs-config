/**
 * convert/stream-events.ts — the agent's OWN OUTPUT: its reasoning and its prose.
 *
 * # Two sources, one unit, and why that is not a duplication
 *
 * With `includePartialMessages` the vendor emits BOTH a fine-grained
 * `stream_event` sequence and, at the end, the whole `assistant` message. They
 * are not two records of one fact: the stream events are what makes a bubble
 * visibly build (start, then deltas), and the assistant message is the SETTLED
 * whole (the full text, the stop reason, the usage). Because units upsert by
 * identity, the two are the same unit's early frames and its terminal — so the
 * split costs a consumer nothing and buys it live prose.
 *
 * The identity is `<message.id>:<block_index>`, 0-based. THE INDEX IS PER
 * MESSAGE, not per line: the vendor splits one assistant message into several
 * lines that each hold one block and share a `message.id`, so the counter runs
 * across those lines and is dropped at `message_stop`.
 *
 * # One block state PER STREAM, never one per fold
 *
 * The main agent and every subagent stream at once, and their events
 * INTERLEAVE on the one SDK iterator: a background subagent's `message_start`
 * can land between two deltas of the main agent's open block. Block state is
 * therefore keyed by the stream a message rides — the main stream, or the
 * spawning call a subagent's messages name, resolved by the SAME rule as the
 * book ({@link spawningCallOf}) — and held in {@link StreamBlocks}. A stream's
 * state exists from its `message_start` (or the first line of an un-streamed
 * response) until its `message_stop`, the next response on the same stream,
 * the turn's end (the main stream) or its agent's end (a subagent's). Every
 * converter resolves it from the message's OWN parent. There is no shared
 * cursor any agent's message could overwrite for another: when one fold held a
 * single message id, a subagent's `message_start` re-keyed the main agent's
 * next deltas onto the subagent's message id, and the main book grew a second,
 * never-settled bubble.
 *
 * A streamed unit that its stream STARTED and never SETTLED is an invariant
 * violation: it is logged at error level, once, with the stream, the message
 * id and the unit id, at whichever of those ends comes first — or at the
 * assistant line that moves past it. Nothing patches the row up.
 *
 * # Usage rides ONE unit
 *
 * A response's cost is a fact about the RESPONSE, and a response yields several
 * units. It rides the unit for the response's FIRST content block and no other,
 * so a consumer summing units gets the bill rather than the bill times the
 * number of blocks. `effort` follows the same rule.
 */
import { create } from "@bufbuild/protobuf";
import {
  InvalidModeledUsageError,
  normalizeApiUsage,
  type NormalizedApiUsage,
} from "../api-usage.js";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import type { PersistEntry } from "../store/persistence.js";
import { activityEntry, agentActivity, prose, settledAt, type FrameOrigin } from "./entries.js";
import { bookFor, spawningCallOf, type FoldContext } from "./fold-context.js";
import { blockActivityId, refusalActivityId } from "./ids.js";
import { residueEntry, residueForMessage } from "./residue.js";
import { convertToolUse, type CallRegistry, type PendingCall, type ToolConverter } from "./tool-calls.js";

const LOGGER = bindLog({ component: "shim-convert-stream", operation: "shim.convert.stream" });

// ---------------------------------------------------------------------------
// The fold's block state — one per stream
// ---------------------------------------------------------------------------

/**
 * One OPEN response's block bookkeeping, and nothing else.
 *
 * Exists only while its stream has a response open, which is why the message
 * id is not optional: "no response open" is the ABSENCE of a state, never a
 * state with no message.
 */
export interface BlockState {
  /** The API message id the counter belongs to. */
  readonly messageId: string;
  /** The next 0-based index to hand an assistant line's block. */
  nextIndex: number;
  /** The thinking block currently streaming, for the token-estimate relay. */
  openThinking?: conversationv1.AgentActivityId;
  /**
   * The VENDOR'S OWN index of the block currently open on the stream.
   *
   * WHY THIS EXISTS: with `includePartialMessages` the vendor emits ONE
   * `assistant` line PER BLOCK, and it arrives BEFORE that block's
   * `content_block_stop` — the line RESTATES the block the stream is streaming
   * rather than continuing past it. Allocating that line a fresh index made
   * every streamed response's terminal land on a different unit than its start
   * (`msg:0` streamed, `msg:1` settled) and made usage — which rides index 0
   * alone — never attach to anything at all. Observed in every streamed
   * capture; `prose-streamed` is the smallest.
   *
   * Unset when no block is open, which is the un-streamed case (a subagent's
   * forwarded lines, a replayed transcript message) where the line carries all
   * of its own blocks and numbering them from the counter is right.
   */
  openBlockIndex?: number;
  /** Whether this response's usage has already been attached to a unit. */
  usageAttached: boolean;
  /**
   * The units this response's STREAM started (`content_block_start` of prose
   * or reasoning) that no assistant line has settled yet, by block index.
   * Whatever is still here when the stream moves past it is a stuck bubble.
   */
  readonly unsettled: Map<number, conversationv1.AgentActivityId>;
}

/** The key of the MAIN stream; a spawning call's id is never empty. */
const MAIN_STREAM = "";

/** The stream key a message's `parent_tool_use_id` names — the book's own rule. */
function streamKeyOf(parentToolUseId: unknown): string {
  return spawningCallOf(parentToolUseId) ?? MAIN_STREAM;
}

/** The stream key as a log field: `main`, or the spawning call's id. */
function streamLabel(key: string): string {
  return key === MAIN_STREAM ? "main" : key;
}

/** Where a started-but-never-settled unit was caught. */
type UnsettledAt = "message_start" | "assistant" | "message_stop" | "turn_end" | "agent_end";

/**
 * Report every unit `state` started and never settled whose index passes
 * `stillOpen`'s test as FAILED to — an invariant violation, logged once each
 * and then forgotten so a later checkpoint does not report it twice.
 */
function reportUnsettled(
  key: string,
  state: BlockState,
  at: UnsettledAt,
  stillOpen: (index: number) => boolean = () => false,
): void {
  for (const [index, activityId] of state.unsettled) {
    if (stillOpen(index)) continue;
    state.unsettled.delete(index);
    LOGGER.error(
      {
        stream: streamLabel(key),
        message_id: state.messageId,
        activity_id: activityId.value,
        block_index: index,
        detected_at: at,
        detail: `the stream reached ${at} with unit ${activityId.value} started and never settled`,
      },
      "invariant violated: a streamed unit was started and never settled; its row stays unsettled",
    );
  }
}

/**
 * The block state of every stream with a response open, KEYED BY STREAM.
 *
 * Every method takes the vendor message's OWN `parent_tool_use_id` and resolves
 * the stream from it through {@link streamKeyOf} — the book's rule — so a
 * converter can only ever read or write the state of the stream the message it
 * is converting rides; there is no method that takes a state to act on.
 * Bounded by the number of agents streaming at once, and self-emptying: a
 * stream's entry is dropped at its `message_stop`, at the turn's end (the main
 * stream) and at its agent's end (a subagent's).
 */
export class StreamBlocks {
  readonly #open = new Map<string, BlockState>();

  /** How many streams have a response open. */
  get openStreams(): number {
    return this.#open.size;
  }

  /** The open response on the stream `parentToolUseId` names, if any. */
  openResponse(parentToolUseId: unknown): BlockState | undefined {
    return this.#open.get(streamKeyOf(parentToolUseId));
  }

  /**
   * Start a new API response's block numbering on that stream. A response the
   * stream still held is over: whatever it started and never settled is
   * reported.
   */
  beginResponse(parentToolUseId: unknown, messageId: string): BlockState {
    const key = streamKeyOf(parentToolUseId);
    const held = this.#open.get(key);
    if (held !== undefined) reportUnsettled(key, held, "message_start");
    const state: BlockState = { messageId, nextIndex: 0, usageAttached: false, unsettled: new Map() };
    this.#open.set(key, state);
    return state;
  }

  /**
   * The response a line of `messageId` on that stream belongs to, beginning it
   * when the stream holds none or holds a DIFFERENT message — whose started,
   * unsettled units are then reported, because the stream moved past them.
   */
  responseFor(parentToolUseId: unknown, messageId: string): BlockState {
    const key = streamKeyOf(parentToolUseId);
    const held = this.#open.get(key);
    if (held?.messageId === messageId) return held;
    if (held !== undefined) reportUnsettled(key, held, "assistant");
    const state: BlockState = { messageId, nextIndex: 0, usageAttached: false, unsettled: new Map() };
    this.#open.set(key, state);
    return state;
  }

  /**
   * Assert every unit the stream's response started BELOW `index` settled,
   * reporting those that did not: an assistant line for block `index` means
   * the stream has moved past them.
   */
  settledBelow(parentToolUseId: unknown, index: number): void {
    const key = streamKeyOf(parentToolUseId);
    const state = this.#open.get(key);
    if (state !== undefined) reportUnsettled(key, state, "assistant", (open) => open >= index);
  }

  /** Close the stream's response at its `message_stop`, reporting what never settled. */
  endResponse(parentToolUseId: unknown): void {
    this.#drop(streamKeyOf(parentToolUseId), "message_stop");
  }

  /**
   * Drop the MAIN stream's response because the turn ended, reporting what
   * never settled. A stop or a failure can end the turn with no `message_stop`;
   * a subagent's stream is not the turn's and outlives it when backgrounded.
   */
  endTurn(): void {
    this.#drop(MAIN_STREAM, "turn_end");
  }

  /**
   * Drop a subagent's stream because the AGENT ended, reporting what never
   * settled. A call that spawned no streaming agent holds no stream; that is
   * the ordinary case and changes nothing.
   */
  endAgent(spawningToolUseId: string): void {
    const key = streamKeyOf(spawningToolUseId);
    if (key === MAIN_STREAM) return;
    this.#drop(key, "agent_end");
  }

  #drop(key: string, at: UnsettledAt): void {
    const state = this.#open.get(key);
    if (state === undefined) {
      LOGGER.logVerbose({ stream: streamLabel(key), at }, "no response open on this stream; nothing to drop");
      return;
    }
    reportUnsettled(key, state, at);
    this.#open.delete(key);
    LOGGER.logVerbose(
      { stream: streamLabel(key), message_id: state.messageId, at },
      "a stream's response is over; its block state is dropped",
    );
  }
}

/** The next index for a block of the stream's open response, continuing across split lines. */
function nextIndexFor(state: BlockState): number {
  const index = state.nextIndex;
  state.nextIndex += 1;
  return index;
}

// ---------------------------------------------------------------------------
// Usage
// ---------------------------------------------------------------------------

/**
 * The vendor's usage counters, in the one canonical token shape.
 *
 * VALIDATED FIRST, never coerced. `normalizeApiUsage` is the strict reader
 * harvested from the old shim: it refuses a malformed usage block outright
 * rather than defaulting a counter to zero, and it collects every field the
 * typed contract cannot express. A usage block that does not validate produces
 * NO usage at all — absence means "not the carrying unit", and a zeroed bill
 * would be read as "this cost nothing".
 *
 * An UNMODELED usage field is a TRANSLATION defect and therefore the shim's own
 * business: the vendor sent something this contract cannot carry, and that is
 * worth a warning even though nothing is lost from the figures below.
 */
function tokenUsage(usage: unknown): conversationv1.TokenUsage | undefined {
  if (typeof usage !== "object" || usage === null) return undefined;
  let normalized: NormalizedApiUsage;
  try {
    normalized = normalizeApiUsage(usage);
  } catch (error) {
    LOGGER.error(
      {
        detail: error instanceof Error ? error.message : String(error),
        field_path: error instanceof InvalidModeledUsageError ? error.fieldPath : undefined,
      },
      "the vendor's usage block did not validate; this response carries NO usage rather than a zeroed bill",
    );
    return undefined;
  }
  const unknownFields = Object.keys(normalized.unknownUsageFields);
  if (unknownFields.length > 0) {
    // warn: a defect because unmodelled usage counters make the canonical bill incomplete.
    LOGGER.warn(
      { unmodeled_usage_fields: unknownFields },
      "the vendor's usage block carries fields this contract cannot express",
    );
  }
  const record = usage as Record<string, unknown>;
  const count = (key: string): bigint => {
    const value = record[key];
    return typeof value === "number" && Number.isFinite(value) && value > 0
      ? BigInt(Math.trunc(value))
      : 0n;
  };
  const details = normalized.outputTokensDetails;
  const reasoning = details === undefined ? undefined : details.reasoning_tokens;
  return create(conversationv1.TokenUsageSchema, {
    // ORGANIZED BY ECONOMICS, not by the vendor's field names: the cache READ is
    // the only cheap bucket, and both miss buckets together are the expensive
    // sum — which is why the nesting exists.
    inputHits: create(conversationv1.TokenCacheHitsSchema, {
      read: count("cache_read_input_tokens"),
    }),
    inputMisses: create(conversationv1.TokenCacheMissesSchema, {
      written: count("cache_creation_input_tokens"),
      unwritten: count("input_tokens"),
    }),
    outputTokens: count("output_tokens"),
    outputThinkingTokens:
      typeof reasoning === "number" && Number.isFinite(reasoning) && reasoning > 0
        ? BigInt(Math.trunc(reasoning))
        : 0n,
  });
}

// ---------------------------------------------------------------------------
// stream_event
// ---------------------------------------------------------------------------

/** The vendor's raw stream event, read loosely before it is typed. */
interface RawStreamEvent {
  readonly type?: string;
  readonly index?: number;
  readonly message?: { readonly id?: string; readonly usage?: unknown };
  readonly content_block?: { readonly type?: string };
  readonly delta?: { readonly type?: string; readonly text?: string; readonly thinking?: string };
}

/**
 * One `stream_event`: the frames that make a bubble build.
 *
 * `signature_delta` and `input_json_delta` produce nothing: a signature is
 * opaque vendor bookkeeping, and a tool call's arguments are only complete at
 * the `assistant` message, so a partial-JSON delta would be an argument list
 * that cannot be parsed.
 */
export function convertStreamEvent(
  message: Extract<SdkMessage, { type: "stream_event" }>,
  context: FoldContext,
  streams: StreamBlocks,
): readonly PersistEntry[] {
  const event = message.event as unknown as RawStreamEvent;
  const stream = message.parent_tool_use_id;
  const agentId = bookFor(context, stream);
  const origin = (discriminator: string, blockIndex?: number): FrameOrigin => ({
    agentId,
    vendorUuid: message.uuid,
    ...(blockIndex === undefined ? {} : { blockIndex }),
    discriminator,
  });

  switch (event.type) {
    case "message_start": {
      const id = event.message?.id;
      if (typeof id !== "string" || id === "") {
        // warn: a defect because a stream message without identity cannot key any block.
        LOGGER.warn(
          { stream: streamLabel(streamKeyOf(stream)) },
          "a message_start named no message id; no block can be identified",
        );
        return [];
      }
      streams.beginResponse(stream, id);
      LOGGER.logVerbose(
        { stream: streamLabel(streamKeyOf(stream)), message_id: id },
        "a new API response opened on this stream; its block numbering starts at 0",
      );
      return [];
    }

    case "content_block_start": {
      const index = event.index ?? 0;
      const state = streams.openResponse(stream);
      if (state === undefined) {
        // warn: a defect because an orphaned content block is omitted from the conversation.
        LOGGER.warn(
          { stream: streamLabel(streamKeyOf(stream)), index },
          "a content block opened with no message_start seen on its stream; skipped",
        );
        return [];
      }
      const messageId = state.messageId;
      // Keep the counter ahead of the stream's own numbering, so an assistant
      // line for a block the stream never opened continues rather than repeats.
      state.nextIndex = Math.max(state.nextIndex, index + 1);
      // THE BLOCK THE NEXT ASSISTANT LINE IS ABOUT, until the vendor closes it.
      state.openBlockIndex = index;
      const activityId = blockActivityId(messageId, index);
      const kind = event.content_block?.type;
      if (kind === "text") {
        state.unsettled.set(index, activityId);
        LOGGER.logVerbose({ message_id: messageId, index }, "a prose block opened");
        return [
          activityEntry(
            context,
            origin("activity.response.start", index),
            agentActivity(activityId, {
              case: "response",
              value: create(conversationv1.AgentResponseSchema, {
                result: {
                  case: "start",
                  value: create(conversationv1.AgentResponseStartSchema, {}),
                },
              }),
            }),
          ),
        ];
      }
      if (kind === "thinking" || kind === "redacted_thinking") {
        state.openThinking = activityId;
        state.unsettled.set(index, activityId);
        LOGGER.logVerbose({ message_id: messageId, index, kind }, "a reasoning block opened");
        return [
          activityEntry(
            context,
            origin("activity.thinking.start", index),
            agentActivity(activityId, {
              case: "thinking",
              value: create(conversationv1.AgentThinkingSchema, {
                result: {
                  case: "start",
                  value: create(conversationv1.AgentThinkingStartSchema, {}),
                },
              }),
            }),
          ),
        ];
      }
      LOGGER.logVerbose(
        { message_id: messageId, index, kind },
        "this block kind streams nothing; its unit is built from the assistant message",
      );
      return [];
    }

    case "content_block_delta": {
      const index = event.index ?? 0;
      const state = streams.openResponse(stream);
      if (state === undefined) {
        LOGGER.debug(
          { stream: streamLabel(streamKeyOf(stream)), index },
          "a delta arrived with no message_start seen on its stream; its block was already skipped",
        );
        return [];
      }
      const activityId = blockActivityId(state.messageId, index);
      const delta = event.delta;
      if (delta?.type === "text_delta" && typeof delta.text === "string") {
        return [
          activityEntry(
            context,
            origin("activity.response.update", index),
            agentActivity(activityId, {
              case: "response",
              value: create(conversationv1.AgentResponseSchema, {
                result: {
                  case: "update",
                  // A DELTA, NEVER THE WHOLE TEXT: the accumulator is the
                  // daemon's, and the terminal carries the whole regardless.
                  value: create(conversationv1.AgentResponseUpdateSchema, {
                    newMarkdown: delta.text,
                  }),
                },
              }),
            }),
          ),
        ];
      }
      if (delta?.type === "thinking_delta" && typeof delta.thinking === "string") {
        return [
          activityEntry(
            context,
            origin("activity.thinking.update", index),
            agentActivity(activityId, {
              case: "thinking",
              value: create(conversationv1.AgentThinkingSchema, {
                result: {
                  case: "update",
                  value: create(conversationv1.AgentThinkingUpdateSchema, {
                    reasoning: {
                      case: "text",
                      value: create(conversationv1.AgentThinkingTextDeltaSchema, {
                        newText: delta.thinking,
                      }),
                    },
                  }),
                },
              }),
            }),
          ),
        ];
      }
      LOGGER.logVerbose(
        { delta_type: delta?.type },
        "this delta carries no conversation content and is consumed",
      );
      return [];
    }

    case "content_block_stop": {
      // The assistant message settles every block with the whole text and the
      // stop reason; a terminal built here would be a second, thinner copy.
      // The block is no longer open, so a later assistant line for this message
      // is about a block the stream did not announce and takes a fresh index.
      const state = streams.openResponse(stream);
      if (state !== undefined) state.openBlockIndex = undefined;
      LOGGER.logVerbose({ event_type: event.type }, "consumed; the assistant message settles blocks");
      return [];
    }

    case "message_delta":
      LOGGER.logVerbose({ event_type: event.type }, "consumed; the assistant message settles blocks");
      return [];

    case "message_stop":
      streams.endResponse(stream);
      return [];

    default:
      LOGGER.debug(
        { event_type: event.type },
        "no converter owns this stream event; it lands as residue",
      );
      return [residueEntry(context, message, residueForMessage(message), "unknown.stream_event")];
  }
}

// ---------------------------------------------------------------------------
// The assistant message
// ---------------------------------------------------------------------------

/** The vendor's assistant message, read loosely before it is typed. */
interface RawAssistantMessage {
  readonly id?: string;
  readonly content?: unknown;
  readonly stop_reason?: string | null;
  readonly usage?: unknown;
}

/**
 * Whether the vendor MARKED this prose as its own synthesized notice.
 *
 * THE VENDOR SYNTHESIZES ERROR NOTICES AS ASSISTANT PROSE — "API Error:
 * connection closed mid-response", "you have hit your monthly spend limit" —
 * in exactly the shape of an answer. Only the producer sees the markers, so a
 * consumer that trusted the shape would draw an outage as something the agent
 * said. ABSENCE MEANS UNEVALUATED, never "the model wrote it".
 */
function synthesizedSubject(
  message: Extract<SdkMessage, { type: "assistant" }>,
): conversationv1.AgentResponseSynthesizedNotice | undefined {
  const record = message as unknown as Record<string, unknown>;
  const declaredError = record.error;
  // `isApiErrorMessage` is the CLI's own marker on such a record. The SDK does
  // not declare it; observed shapes beat declared types, so both are read.
  const observedMarker = record.isApiErrorMessage === true;
  if (typeof declaredError !== "string" && !observedMarker) return undefined;
  const subject: conversationv1.AgentResponseSynthesizedNotice["subject"] =
    declaredError === "rate_limit"
      ? {
          case: "usageLimit",
          value: create(conversationv1.AgentNoticeUsageLimitSchema, {}),
        }
      : declaredError === "billing_error"
        ? { case: "usageLimit", value: create(conversationv1.AgentNoticeUsageLimitSchema, {}) }
        : {
            case: "unclassified",
            value: create(conversationv1.AgentNoticeUnclassifiedSchema, {}),
          };
  return create(conversationv1.AgentResponseSynthesizedNoticeSchema, {
    subject,
    // The vendor attaches no HTTP status to an assistant record — the status
    // rides the RESULT (`api_error_status`) — so it stays unset here.
  });
}

/**
 * Why a response ended without completing, from the vendor's own stop fact.
 *
 * `end_turn`, `tool_use` and `pause_turn` are NOT here: the first two are the
 * ordinary completion, and `pause_turn` means the vendor resumes it itself, so
 * nothing ended.
 */
function responseFailureReason(
  stopReason: string | null | undefined,
  aborted: boolean,
): conversationv1.AgentResponseFailureReason | undefined {
  if (aborted) {
    return create(conversationv1.AgentResponseFailureReasonSchema, {
      reason: { case: "aborted", value: create(conversationv1.AgentResponseAbortedSchema, {}) },
    });
  }
  switch (stopReason) {
    case "max_tokens":
      return create(conversationv1.AgentResponseFailureReasonSchema, {
        reason: {
          case: "maxTokens",
          value: create(conversationv1.AgentResponseStoppedAtMaxTokensSchema, {}),
        },
      });
    case "refusal":
      return create(conversationv1.AgentResponseFailureReasonSchema, {
        reason: {
          case: "refused",
          // The vendor's explanation rides its own `model_refusal_*` record,
          // which is a different message; joining the two would be state.
          value: create(conversationv1.AgentResponseRefusedSchema, {}),
        },
      });
    case "model_context_window_exceeded":
      return create(conversationv1.AgentResponseFailureReasonSchema, {
        reason: {
          case: "contextWindowExceeded",
          value: create(conversationv1.AgentResponseContextWindowExceededSchema, {}),
        },
      });
    case "stop_sequence":
      return create(conversationv1.AgentResponseFailureReasonSchema, {
        reason: {
          case: "stopSequence",
          value: create(conversationv1.AgentResponseStoppedAtStopSequenceSchema, {}),
        },
      });
    default:
      return undefined;
  }
}

/**
 * A response terminal's settle instant, taken from THE RECORD'S OWN timestamp.
 *
 * TIME COMES FROM THE RECORD, NOT THE WALL CLOCK. This is the same invariant the
 * sidecar file plane already holds (`shim-claude-sidecar/internal/convert/
 * instants.go`, `settledAt(env.timestampMs)` off the transcript record's
 * `timestamp`): a settle instant stamped from `context.nowMs()` at conversion
 * time is NOT replay-stable, so the stream plane's frame and the file plane's
 * frame for one response carried DIFFERENT instants, and the corner's "N ago"
 * jumped whenever the surviving row switched planes (the sidecar's file-plane
 * row supersedes the shim's stream-plane row on catch-up) or a re-compose
 * re-stamped emit time. Reading the SDK record's own `timestamp` makes both
 * planes agree by construction, so the daemon's `stampSettled` receives the one
 * true settle instant on live, on replay, and on every re-resolve.
 *
 * REGRESSION WATCH (2026-09-15): do NOT revert this to `settledAt(context.
 * nowMs())`. The wall clock is the LAST RESORT ONLY — kept for a record that
 * carried no parseable timestamp, because at live settle `nowMs()` genuinely IS
 * the settle instant and dropping it would push the daemon onto its own
 * compose-time `Now()` on every later replay, which is the very reset this
 * avoids. The SDK stamps a `timestamp` on every real assistant/system record
 * (`testdata/corpus/stream/assistant.jsonl`), so the fallback is exercised only
 * by a genuinely timestamp-less record.
 */
function recordSettledAt(
  message: SdkMessage,
  context: FoldContext,
): conversationv1.AgentActivitySettledAt {
  // Observed shapes beat declared types: every real record carries a top-level
  // `timestamp` (see synthesizedSubject, which reads the record the same way),
  // but the SDK's own union does not declare it on every arm.
  const raw = (message as unknown as { readonly timestamp?: unknown }).timestamp;
  if (typeof raw === "string" && raw !== "") {
    const ms = Date.parse(raw);
    if (!Number.isNaN(ms)) return settledAt(ms);
  }
  return settledAt(context.nowMs());
}

/**
 * One `assistant` message: every block's settled unit, and every tool call's start.
 *
 * The usage rides the unit whose block index is 0 and no other.
 */
export function convertAssistantMessage(
  message: Extract<SdkMessage, { type: "assistant" }>,
  context: FoldContext,
  streams: StreamBlocks,
  registry: CallRegistry,
  converters: ReadonlyMap<string, ToolConverter>,
): readonly PersistEntry[] {
  const api = message.message as unknown as RawAssistantMessage;
  const messageId = api.id;
  if (typeof messageId !== "string" || messageId === "") {
    // warn: a defect because an assistant message without identity cannot produce addressable frames.
    LOGGER.warn(
      { uuid: message.uuid },
      "an assistant message named no id; its blocks have no identity and produce no frames",
    );
    return [residueEntry(context, message, residueForMessage(message, "assistant message has no id"), "residue.unparsed")];
  }
  const blocks = Array.isArray(api.content) ? (api.content as Record<string, unknown>[]) : [];
  const stream = message.parent_tool_use_id;
  const agentId = bookFor(context, stream);
  const aborted = (message as unknown as Record<string, unknown>).aborted === true;
  const failure = responseFailureReason(api.stop_reason, aborted);
  const notice = synthesizedSubject(message);
  const usage = tokenUsage(api.usage);
  const entries: PersistEntry[] = [];

  // THIS STREAM'S response, and no other agent's: resolved from the line's own
  // parent, so an interleaved subagent can never renumber the main agent's
  // blocks or claim its usage.
  const state = streams.responseFor(stream, messageId);
  // A LINE THAT RESTATES AN OPEN STREAMED BLOCK IS THAT BLOCK, not a new one.
  const streamed = state.openBlockIndex;
  let offset = 0;
  let firstIndex: number | undefined;

  for (const block of blocks) {
    const index = streamed === undefined ? nextIndexFor(state) : streamed + offset;
    offset += 1;
    firstIndex ??= index;
    // USAGE RIDES THE FIRST BLOCK'S UNIT AND NO OTHER.
    const envelope =
      index === 0 && !state.usageAttached && usage !== undefined ? { usage } : {};
    if (index === 0 && usage !== undefined) state.usageAttached = true;
    const origin = (discriminator: string): FrameOrigin => ({
      agentId,
      vendorUuid: message.uuid,
      blockIndex: index,
      discriminator,
    });
    const activityId = blockActivityId(messageId, index);
    const kind = block.type;

    if (kind === "text" && typeof block.text === "string") {
      const item: conversationv1.AgentActivity["item"] = {
        case: "response",
        value: create(conversationv1.AgentResponseSchema, {
          result:
            failure === undefined
              ? {
                  case: "success",
                  value: create(conversationv1.AgentResponseSuccessSchema, {
                    prose: prose(block.text),
                    authorship:
                      notice === undefined
                        ? {
                            case: "fromModel",
                            value: create(conversationv1.AgentResponseFromModelSchema, {}),
                          }
                        : { case: "synthesizedNotice", value: notice },
                    // THE SETTLE INSTANT RIDES THE FRAME, from the RECORD's own
                    // timestamp so a re-compose — or the file plane's later row
                    // for the same unit — reproduces the same "N ago" corner
                    // rather than the wall clock at conversion time. See
                    // recordSettledAt.
                    settledAt: recordSettledAt(message, context),
                  }),
                }
              : {
                  case: "failure",
                  value: create(conversationv1.AgentResponseFailureSchema, {
                    prose: prose(block.text),
                    reason: failure,
                    settledAt: recordSettledAt(message, context),
                  }),
                },
        }),
      };
      LOGGER.logVerbose(
        { message_id: messageId, index, settled: failure === undefined ? "success" : "failure" },
        "settling a prose block",
      );
      entries.push(
        activityEntry(
          context,
          origin(`activity.response.${failure === undefined ? "success" : "failure"}`),
          agentActivity(activityId, item, envelope),
        ),
      );
      state.unsettled.delete(index);
      continue;
    }

    if (kind === "thinking" || kind === "redacted_thinking") {
      const text = typeof block.thinking === "string" ? block.thinking : undefined;
      const item: conversationv1.AgentActivity["item"] = {
        case: "thinking",
        value: create(conversationv1.AgentThinkingSchema, {
          result: {
            case: "success",
            value: create(conversationv1.AgentThinkingSuccessSchema, {
              reasoning:
                text === undefined || text === ""
                  ? {
                      case: "withheld",
                      value: create(conversationv1.AgentThinkingWithheldSchema, {}),
                    }
                  : {
                      case: "text",
                      value: create(conversationv1.AgentThinkingTextSchema, { text }),
                    },
            }),
          },
        }),
      };
      LOGGER.logVerbose({ message_id: messageId, index, withheld: text === undefined }, "settling a reasoning block");
      entries.push(
        activityEntry(context, origin("activity.thinking.success"), agentActivity(activityId, item, envelope)),
      );
      state.unsettled.delete(index);
      continue;
    }

    if (kind === "tool_use") {
      const toolUseId = typeof block.id === "string" ? block.id : "";
      const toolName = typeof block.name === "string" ? block.name : "";
      if (toolUseId === "" || toolName === "") {
        // warn: a defect because a tool call without identity or a name cannot produce a unit.
        LOGGER.warn(
          { message_id: messageId, index },
          "a tool_use block named no id or no tool; no unit can be identified",
        );
        continue;
      }
      const call: PendingCall = {
        toolUseId,
        toolName,
        input:
          typeof block.input === "object" && block.input !== null
            ? (block.input as Record<string, unknown>)
            : {},
        // THE SHIM STAMPS THE START INSTANT AT ANNOUNCEMENT, once, and never
        // restates it: the vendor's elapsed figures are consumed, not forwarded.
        startedAtMs: context.nowMs(),
        agentId,
        // THE STREAM IT RODE, by the book's own rule: what the registry holds
        // it under, so its agent's end or handoff releases exactly its calls.
        spawningCall: spawningCallOf(stream),
      };
      entries.push(
        ...convertToolUse(
          converters,
          context,
          registry,
          call,
          { agentId, vendorUuid: message.uuid, blockIndex: index },
          envelope,
        ),
      );
      continue;
    }

    LOGGER.debug(
      { message_id: messageId, index, block_type: kind },
      "no converter owns this assistant content block; it lands as residue",
    );
    entries.push(residueEntry(context, block, residueForMessage(block), `unknown.content_block.${String(kind)}`));
  }

  // THE STREAM HAS MOVED PAST every block below this line's first: a unit it
  // started there and no line settled is a bubble that will never settle.
  if (firstIndex !== undefined) streams.settledBelow(stream, firstIndex);
  return entries;
}

/**
 * A MODEL REFUSAL WITH NO FALLBACK: the response, settled as a refusal.
 *
 * The vendor states this ending in a `system` record of its own rather than on
 * an assistant block — the transcript corpus carries the record ALONE, with no
 * accompanying assistant message whose `stop_reason` is `refusal` — so this is
 * the only place the fact is stated, and without a converter it reached no
 * conversation.v1 arm at all. `AgentResponseRefused.explanation` has no other
 * source either: the vendor's wording rides this record and nothing else.
 *
 * `model_refusal_fallback` is deliberately NOT folded here: there the vendor
 * fell back to another model and the response continued, so nothing ended.
 */
export function convertModelRefusal(
  message: Extract<SdkMessage, { type: "system"; subtype: "model_refusal_no_fallback" }>,
  context: FoldContext,
): readonly PersistEntry[] {
  const stated = message.api_refusal_explanation;
  const explanation =
    typeof stated === "string" && stated !== ""
      ? create(conversationv1.AgentResponseRefusalExplanationSchema, { text: stated })
      : undefined;
  LOGGER.info(
    { uuid: message.uuid, explained: explanation !== undefined },
    "the model refused the request; settling the response as a refusal",
  );
  return [
    activityEntry(
      context,
      {
        agentId: context.mainAgentId,
        vendorUuid: message.uuid,
        discriminator: "activity.response.failure.refused",
      },
      agentActivity(refusalActivityId(message.uuid), {
        case: "response",
        value: create(conversationv1.AgentResponseSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentResponseFailureSchema, {
              // The vendor sends `content: ""` with a refusal; the prose is
              // carried anyway, because a refusal that DID say something must
              // still show it.
              prose: prose(message.content),
              reason: create(conversationv1.AgentResponseFailureReasonSchema, {
                reason: {
                  case: "refused",
                  value: create(conversationv1.AgentResponseRefusedSchema, {
                    ...(explanation === undefined ? {} : { explanation }),
                  }),
                },
              }),
              // A refusal is a settled response like any other and carries its
              // settle instant, from the record's own timestamp — without it the
              // daemon fell back to compose-time Now() and the refusal bubble's
              // age reset on every re-resolve. See recordSettledAt.
              settledAt: recordSettledAt(message, context),
            }),
          },
        }),
      }),
    ),
  ];
}

/**
 * The vendor's live thinking-token estimate, relayed onto the open block.
 *
 * AN ESTIMATE AND NOT A CHARGE — the billed figure arrives at settle as
 * `TokenUsage.output_thinking_tokens` and the two will differ. It is drawn as a
 * live counter under a WITHHELD block, which is otherwise the one thing in a
 * turn that shows nothing at all while it runs.
 */
export function convertThinkingTokens(
  message: Extract<SdkMessage, { type: "system"; subtype: "thinking_tokens" }>,
  context: FoldContext,
  streams: StreamBlocks,
): readonly PersistEntry[] {
  // THE MAIN STREAM'S open block: the record names no parent, and the row it
  // produces is written to the main agent's book.
  const activityId = streams.openResponse(null)?.openThinking;
  if (activityId === undefined) {
    LOGGER.debug(
      {},
      "a thinking-token estimate arrived with no open reasoning block; nothing to upsert",
    );
    return [];
  }
  return [
    activityEntry(
      context,
      {
        agentId: context.mainAgentId,
        vendorUuid: message.uuid,
        discriminator: "activity.thinking.update.withheld",
      },
      agentActivity(activityId, {
        case: "thinking",
        value: create(conversationv1.AgentThinkingSchema, {
          result: {
            case: "update",
            value: create(conversationv1.AgentThinkingUpdateSchema, {
              reasoning: {
                case: "withheld",
                value: create(conversationv1.AgentThinkingWithheldSchema, {}),
              },
              estimated: create(conversationv1.AgentThinkingTokenEstimateSchema, {
                estimatedTokens: BigInt(Math.max(0, Math.trunc(message.estimated_tokens))),
                estimatedTokensDelta: BigInt(Math.max(0, Math.trunc(message.estimated_tokens_delta))),
              }),
            }),
          },
        }),
      }),
    ),
  ];
}
