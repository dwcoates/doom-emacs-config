/**
 * THE STREAMED RESPONSE: how a partial-message stream and the `assistant` lines
 * beside it name the SAME units.
 *
 * With `includePartialMessages` the vendor emits ONE `assistant` line PER
 * CONTENT BLOCK, and it arrives BEFORE that block's `content_block_stop` — the
 * line RESTATES the block being streamed rather than continuing past it. Every
 * real capture is shaped this way, and the two facts below are what a converter
 * that read it as a continuation got wrong: the terminal landed on a different
 * unit than the start, and usage — which rides block 0 alone — attached to
 * nothing at all.
 */
import { create } from "@bufbuild/protobuf";
import { writeSync } from "node:fs";
import { describe, expect, it, vi } from "vitest";
import { createFold } from "../../src/convert/fold.js";
import { convertAssistantMessage, StreamBlocks } from "../../src/convert/stream-events.js";
import { createCallRegistry } from "../../src/convert/tool-calls.js";
import { TOOL_CONVERTERS } from "../../src/convert/tools/registry.js";
import { conversationv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { activityOf, foldContext, MAIN_AGENT } from "./fold-harness.js";
import type { PersistEntry } from "../../src/store/persistence.js";

const MESSAGE_ID = "msg_streamed";

/** The vendor's usage block, as a real capture states one. */
const USAGE = {
  input_tokens: 10,
  cache_creation_input_tokens: 100,
  cache_read_input_tokens: 200,
  output_tokens: 30,
};

/** One `stream_event` wrapper, as the SDK hands it over. */
function streamEvent(event: Record<string, unknown>, uuid: string): SdkMessage {
  return {
    type: "stream_event",
    uuid,
    session_id: "session-1",
    parent_tool_use_id: null,
    event,
  } as unknown as SdkMessage;
}

/** The `assistant` line the vendor emits for ONE streamed block. */
function assistantLine(block: Record<string, unknown>, uuid: string): SdkMessage {
  return {
    type: "assistant",
    uuid,
    session_id: "session-1",
    parent_tool_use_id: null,
    message: {
      id: MESSAGE_ID,
      type: "message",
      role: "assistant",
      model: "claude-opus-5",
      content: [block],
      stop_reason: null,
      usage: USAGE,
    },
  } as unknown as SdkMessage;
}

/** One streamed response of two prose blocks, folded as the vendor sends it. */
function foldStreamedResponse(): PersistEntry[] {
  const fold = createFold();
  const context = foldContext();
  const entries: PersistEntry[] = [];
  const push = (message: SdkMessage): void => {
    entries.push(...fold.onSdkMessage(message, context).entries);
  };
  push(streamEvent({ type: "message_start", message: { id: MESSAGE_ID, usage: USAGE } }, "u-start"));
  push(streamEvent({ type: "content_block_start", index: 0, content_block: { type: "text" } }, "u-b0"));
  push(assistantLine({ type: "text", text: "first" }, "u-a0"));
  push(streamEvent({ type: "content_block_stop", index: 0 }, "u-s0"));
  push(streamEvent({ type: "content_block_start", index: 1, content_block: { type: "text" } }, "u-b1"));
  push(assistantLine({ type: "text", text: "second" }, "u-a1"));
  push(streamEvent({ type: "content_block_stop", index: 1 }, "u-s1"));
  push(streamEvent({ type: "message_stop" }, "u-stop"));
  return entries;
}

/** The upsert keys of the rows carrying one arm. */
function keysFor(entries: readonly PersistEntry[], arm: string): string[] {
  return entries.filter((entry) => entry.source.discriminator === arm).map((e) => e.upsertKey);
}

describe("a streamed response's block identities", () => {
  it("settles the first block under the identity the stream opened it with", () => {
    const entries = foldStreamedResponse();

    expect(keysFor(entries, "activity.response.success")[0]).toBe(`activity:${MESSAGE_ID}:0`);
  });

  it("settles the second block under the identity the stream opened it with", () => {
    const entries = foldStreamedResponse();

    expect(keysFor(entries, "activity.response.success")[1]).toBe(`activity:${MESSAGE_ID}:1`);
  });

  it("gives a block's start and its terminal the SAME key", () => {
    const entries = foldStreamedResponse();

    expect(keysFor(entries, "activity.response.success")).toEqual(
      keysFor(entries, "activity.response.start"),
    );
  });

  it("numbers a line whose block the stream never opened from the counter", () => {
    // No `content_block_start` for index 1, so the second line is about a block
    // the stream did not announce and takes the next free index of its own.
    const fold = createFold();
    const context = foldContext();
    const entries: PersistEntry[] = [];
    const push = (message: SdkMessage): void => {
      entries.push(...fold.onSdkMessage(message, context).entries);
    };
    push(streamEvent({ type: "message_start", message: { id: MESSAGE_ID, usage: USAGE } }, "u-start"));
    push(streamEvent({ type: "content_block_start", index: 0, content_block: { type: "text" } }, "u-b0"));
    push(assistantLine({ type: "text", text: "first" }, "u-a0"));
    push(streamEvent({ type: "content_block_stop", index: 0 }, "u-s0"));
    push(assistantLine({ type: "text", text: "second" }, "u-a1"));

    expect(keysFor(entries, "activity.response.success")[1]).toBe(`activity:${MESSAGE_ID}:1`);
  });
});

describe("usage over a streamed response", () => {
  it("attaches to the response's FIRST block", () => {
    const carrying = foldStreamedResponse().filter((e) => activityOf(e)?.usage !== undefined);

    expect(carrying.map((entry) => entry.upsertKey)).toEqual([`activity:${MESSAGE_ID}:0`]);
  });

  it("attaches exactly once even though every line restates it", () => {
    const carrying = foldStreamedResponse().filter((e) => activityOf(e)?.usage !== undefined);

    expect(carrying.length).toBe(1);
  });
});

/**
 * THE REFUSAL RECORD. The vendor states a refusal-with-no-fallback in its own
 * `system` record and in NO assistant block — the transcript corpus carries
 * the record alone — so this record is the sole witness that the response
 * ended on a refusal, and the sole source of the vendor's explanation.
 */
const REFUSAL_EXPLANATION =
  "API integrators: you can reduce refusals for your users by configuring a fallback model.";

/** One `system:model_refusal_no_fallback` line, as the corpus records one. */
function refusalMessage(overrides: Record<string, unknown> = {}): SdkMessage {
  return {
    type: "system",
    subtype: "model_refusal_no_fallback",
    uuid: "u-refusal",
    session_id: "session-1",
    original_model: "claude-opus-5",
    request_id: "req_fake_refusal",
    api_refusal_category: "bio",
    api_refusal_explanation: REFUSAL_EXPLANATION,
    refused_user_message_uuid: null,
    content: "",
    ...overrides,
  } as unknown as SdkMessage;
}

/** The entries one refusal record folds to. */
function foldRefusal(overrides: Record<string, unknown> = {}): PersistEntry[] {
  return [...createFold().onSdkMessage(refusalMessage(overrides), foldContext()).entries];
}

describe("a model refusal with no fallback", () => {
  it("settles the response as a FAILURE rather than reaching no arm at all", () => {
    const entries = foldRefusal();

    const response = activityOf(entries[0])?.item;
    expect(response?.case === "response" ? response.value.result.case : undefined).toBe("failure");
  });

  it("names the refusal as the reason the response ended", () => {
    const entries = foldRefusal();

    const response = activityOf(entries[0])?.item;
    const result = response?.case === "response" ? response.value.result : undefined;
    expect(result?.case === "failure" ? result.value.reason?.reason.case : undefined).toBe(
      "refused",
    );
  });

  it("carries the vendor's explanation, which rides no other record", () => {
    const entries = foldRefusal();

    const response = activityOf(entries[0])?.item;
    const result = response?.case === "response" ? response.value.result : undefined;
    const reason = result?.case === "failure" ? result.value.reason?.reason : undefined;
    expect(reason?.case === "refused" ? reason.value.explanation?.text : undefined).toBe(
      REFUSAL_EXPLANATION,
    );
  });

  it("leaves the explanation UNSET when the vendor stated none", () => {
    const entries = foldRefusal({ api_refusal_explanation: null });

    const response = activityOf(entries[0])?.item;
    const reason =
      response?.case === "response" && response.value.result.case === "failure"
        ? response.value.result.value.reason?.reason
        : undefined;
    expect(reason?.case).toBe("refused");
    expect(reason?.case === "refused" ? reason.value.explanation : undefined).toBeUndefined();
  });

  it("keys the unit on the record's own uuid, its only identity", () => {
    const entries = foldRefusal();

    expect(entries[0]?.upsertKey).toBe("activity:u-refusal");
  });

  it("carries the record's own timestamp as the settle instant", () => {
    // A refusal is a settled response and carries its settle instant; without it
    // the daemon fell back to compose-time Now() and the bubble's age reset on
    // every re-resolve. The record's timestamp wins over the wall clock.
    const entries = [
      ...createFold()
        .onSdkMessage(
          refusalMessage({ timestamp: "2026-07-23T17:42:47.752Z" }),
          foldContext({ nowMs: 4242 }),
        )
        .entries,
    ];

    const response = activityOf(entries[0])?.item;
    const result = response?.case === "response" ? response.value.result : undefined;
    const settledAt =
      result?.case === "failure" ? result.value.settledAt?.atMs : undefined;
    expect(settledAt).toBe(1784828567752n);
  });

  it("falls back to the live clock when the refusal record has no timestamp", () => {
    const entries = [
      ...createFold().onSdkMessage(refusalMessage(), foldContext({ nowMs: 4242 })).entries,
    ];

    const response = activityOf(entries[0])?.item;
    const result = response?.case === "response" ? response.value.result : undefined;
    const settledAt =
      result?.case === "failure" ? result.value.settledAt?.atMs : undefined;
    expect(settledAt).toBe(4242n);
  });
});

// ---------------------------------------------------------------------------
// The usage block: validated first, never coerced.
// ---------------------------------------------------------------------------

/** One `assistant` line carrying exactly the usage a test wants read. */
function assistantWithUsage(usage: unknown): SdkMessage {
  return {
    type: "assistant",
    uuid: "u-usage",
    session_id: "session-1",
    parent_tool_use_id: null,
    message: {
      id: "msg_usage",
      type: "message",
      role: "assistant",
      model: "claude-opus-5",
      content: [{ type: "text", text: "hello" }],
      stop_reason: "end_turn",
      usage,
    },
  } as unknown as SdkMessage;
}

/** The usage the response's first block ended up carrying, if any. */
function usageOf(usage: unknown) {
  const entries = [
    ...createFold().onSdkMessage(assistantWithUsage(usage), foldContext()).entries,
  ];
  return activityOf(entries[0])?.usage;
}

describe("the vendor's usage block", () => {
  it("carries NO usage when the vendor stated something that is not an object at all", () => {
    // A zeroed bill would be read as "this cost nothing"; absence says
    // "not the carrying unit".
    expect(usageOf("nope")).toBeUndefined();
  });

  it("carries NO usage when a modeled counter is malformed", () => {
    expect(usageOf({ ...USAGE, input_tokens: -1 })).toBeUndefined();
  });

  it("still carries the figures when the vendor added a field this contract cannot express", () => {
    const carried = usageOf({ ...USAGE, thermodynamic_tokens: 4 });

    expect(carried?.outputTokens).toBe(30n);
  });

  it("reads the reasoning count out of the vendor's output-token details", () => {
    const carried = usageOf({ ...USAGE, output_tokens_details: { reasoning_tokens: 7 } });

    expect(carried?.outputThinkingTokens).toBe(7n);
  });
});

// ---------------------------------------------------------------------------
// Stream events with no message to belong to.
// ---------------------------------------------------------------------------

describe("a stream event with no message behind it", () => {
  it("produces nothing for a message_start that named no message id", () => {
    const entries = createFold().onSdkMessage(
      streamEvent({ type: "message_start", message: {} }, "u-noid"),
      foldContext(),
    ).entries;

    expect(entries).toEqual([]);
  });

  it("skips a content block opened before any message_start was seen", () => {
    const entries = createFold().onSdkMessage(
      streamEvent({ type: "content_block_start", index: 0, content_block: { type: "text" } }, "u-orphan"),
      foldContext(),
    ).entries;

    expect(entries).toEqual([]);
  });

  it("skips a delta that arrived before any message_start was seen", () => {
    const entries = createFold().onSdkMessage(
      streamEvent({ type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "x" } }, "u-orphan-d"),
      foldContext(),
    ).entries;

    expect(entries).toEqual([]);
  });

  it("keeps a stream event no converter owns as residue rather than dropping it", () => {
    const entries = [
      ...createFold().onSdkMessage(streamEvent({ type: "citations_delta" }, "u-novel"), foldContext())
        .entries,
    ];

    expect(entries[0]?.source.discriminator).toBe("unknown.stream_event");
  });
});

// ---------------------------------------------------------------------------
// The assistant message's malformed shapes.
// ---------------------------------------------------------------------------

/** One `assistant` line with the API message's fields a test wants. */
function assistantMessage(api: Record<string, unknown>, uuid = "u-a"): SdkMessage {
  return {
    type: "assistant",
    uuid,
    session_id: "session-1",
    parent_tool_use_id: null,
    message: {
      type: "message",
      role: "assistant",
      model: "claude-opus-5",
      stop_reason: null,
      usage: USAGE,
      ...api,
    },
  } as unknown as SdkMessage;
}

function foldOne(message: SdkMessage): PersistEntry[] {
  return [...createFold().onSdkMessage(message, foldContext()).entries];
}

describe("an assistant message whose blocks have no identity", () => {
  it("lands the whole record as UNPARSED residue when it named no id", () => {
    const entries = foldOne(
      assistantMessage({ id: "", content: [{ type: "text", text: "orphan" }] }, "u-noid-a"),
    );

    expect(entries[0]?.source.discriminator).toBe("residue.unparsed");
  });
});

describe("an assistant content block no converter owns", () => {
  it("lands the block as residue named by its own kind", () => {
    const entries = foldOne(
      assistantMessage({ id: "msg_novel", content: [{ type: "server_tool_use_result" }] }),
    );

    expect(entries[0]?.source.discriminator).toBe("unknown.content_block.server_tool_use_result");
  });
});

describe("a tool_use block the vendor did not fully name", () => {
  it("produces no unit when the block named no tool_use id", () => {
    const entries = foldOne(
      assistantMessage({ id: "msg_tu", content: [{ type: "tool_use", name: "Read", input: {} }] }),
    );

    expect(entries).toEqual([]);
  });

  it("produces no unit when the block named no tool", () => {
    const entries = foldOne(
      assistantMessage({ id: "msg_tu", content: [{ type: "tool_use", id: "toolu_1", input: {} }] }),
    );

    expect(entries).toEqual([]);
  });

  it("reads a non-object input as NO arguments rather than failing the block", () => {
    const entries = foldOne(
      assistantMessage({
        id: "msg_tu",
        content: [{ type: "tool_use", id: "toolu_1", name: "mcp__Slack__send", input: "nope" }],
      }),
    );

    const item = activityOf(entries[0])?.item;
    const result = item?.case === "unmodeled" ? item.value.result : undefined;
    expect(result?.case === "start" ? result.value.arguments : undefined).toEqual({});
  });
});

// ---------------------------------------------------------------------------
// Why a response ended without completing.
// ---------------------------------------------------------------------------

/** The failure reason arm the settled prose block ended up carrying. */
function failureReasonOf(api: Record<string, unknown>): string | undefined {
  const entries = foldOne(
    assistantMessage({ id: "msg_fail", content: [{ type: "text", text: "partial" }], ...api }),
  );
  const item = activityOf(entries[0])?.item;
  const result = item?.case === "response" ? item.value.result : undefined;
  return result?.case === "failure" ? result.value.reason?.reason.case : undefined;
}

describe.each([
  ["max_tokens", "maxTokens"],
  ["refusal", "refused"],
  ["model_context_window_exceeded", "contextWindowExceeded"],
  ["stop_sequence", "stopSequence"],
])("a response the vendor stopped with stop_reason %s", (stopReason, expectedCase) => {
  it(`settles the prose as failure.${expectedCase}`, () => {
    expect(failureReasonOf({ stop_reason: stopReason })).toBe(expectedCase);
  });
});

describe("an aborted response", () => {
  it("settles as ABORTED whatever the vendor's stop reason said", () => {
    const entries = foldOne({
      ...(assistantMessage({
        id: "msg_abort",
        content: [{ type: "text", text: "partial" }],
        stop_reason: "max_tokens",
      }) as unknown as Record<string, unknown>),
      aborted: true,
    } as unknown as SdkMessage);

    const item = activityOf(entries[0])?.item;
    const result = item?.case === "response" ? item.value.result : undefined;
    expect(result?.case === "failure" ? result.value.reason?.reason.case : undefined).toBe(
      "aborted",
    );
  });
});

// ---------------------------------------------------------------------------
// A stream event that omits its block index.
// ---------------------------------------------------------------------------

/** Fold a message_start, then the events a test wants, over one fold. */
function foldStream(...events: Record<string, unknown>[]): PersistEntry[] {
  const fold = createFold();
  const context = foldContext();
  fold.onSdkMessage(
    streamEvent({ type: "message_start", message: { id: MESSAGE_ID } }, "u-start"),
    context,
  );
  return events.flatMap((event, at) => [
    ...fold.onSdkMessage(streamEvent(event, `u-ev-${String(at)}`), context).entries,
  ]);
}

describe("a content_block_start the vendor sent with no index", () => {
  it("reads it as block ZERO, so the unit is the first block and not an unnumbered one", () => {
    // Arrange, Act.
    const entries = foldStream({ type: "content_block_start", content_block: { type: "text" } });

    // Assert.
    expect(entries[0]?.source.blockIndex).toBe(0);
  });
});

describe("a content_block_delta the vendor sent with no index", () => {
  it("lands the text on block ZERO's unit rather than on an unnumbered one", () => {
    // Arrange, Act.
    const entries = foldStream(
      { type: "content_block_start", index: 0, content_block: { type: "text" } },
      { type: "content_block_delta", delta: { type: "text_delta", text: "hi" } },
    );

    // Assert.
    expect(entries[1]?.source.blockIndex).toBe(0);
  });
});

// ---------------------------------------------------------------------------
// The vendor's own synthesized notices.
// ---------------------------------------------------------------------------

/** The notice subject the settled prose block ended up carrying. */
function noticeSubjectOf(record: Record<string, unknown>): string | undefined {
  const entries = [
    ...createFold().onSdkMessage(
      {
        ...(assistantMessage({
          id: "msg_notice",
          content: [{ type: "text", text: "API Error" }],
        }) as unknown as Record<string, unknown>),
        ...record,
      } as unknown as SdkMessage,
      foldContext(),
    ).entries,
  ];
  const item = activityOf(entries[0])?.item;
  const result = item?.case === "response" ? item.value.result : undefined;
  const authorship = result?.case === "success" ? result.value.authorship : undefined;
  return authorship?.case === "synthesizedNotice"
    ? authorship.value.subject.case
    : undefined;
}

describe("prose the vendor synthesized for a rate limit", () => {
  it("classifies the notice as a USAGE LIMIT, not as an unclassified failure", () => {
    expect(noticeSubjectOf({ error: "rate_limit" })).toBe("usageLimit");
  });
});

// ---------------------------------------------------------------------------
// Interleaved streams: one block state PER STREAM.
// ---------------------------------------------------------------------------

const MAIN_ID = "msg_main";
const SUB_ID = "msg_sub";
const SPAWN = "toolu_spawn";

/** A stream event on the stream `parent` names (`null` is the main agent's). */
function eventOn(parent: string | null, event: Record<string, unknown>, uuid: string): SdkMessage {
  return {
    type: "stream_event",
    uuid,
    session_id: "session-1",
    parent_tool_use_id: parent,
    event,
  } as unknown as SdkMessage;
}

/** The assistant line restating one block of `messageId`, on the stream `parent` names. */
function lineOn(
  parent: string | null,
  messageId: string,
  block: Record<string, unknown>,
  uuid: string,
): SdkMessage {
  return {
    type: "assistant",
    uuid,
    session_id: "session-1",
    parent_tool_use_id: parent,
    message: {
      id: messageId,
      type: "message",
      role: "assistant",
      model: "claude-opus-5",
      content: [block],
      stop_reason: null,
      usage: USAGE,
    },
  } as unknown as SdkMessage;
}

const start = (id: string): Record<string, unknown> => ({ type: "message_start", message: { id, usage: USAGE } });
const blockStart = (index: number, type: string): Record<string, unknown> => ({
  type: "content_block_start",
  index,
  content_block: { type },
});
const textDelta = (index: number, text: string): Record<string, unknown> => ({
  type: "content_block_delta",
  index,
  delta: { type: "text_delta", text },
});
const thinkingDelta = (index: number, thinking: string): Record<string, unknown> => ({
  type: "content_block_delta",
  index,
  delta: { type: "thinking_delta", thinking },
});
const blockStop = (index: number): Record<string, unknown> => ({ type: "content_block_stop", index });
const STOP: Record<string, unknown> = { type: "message_stop" };

/** Fold the messages, in order, over ONE fold. */
function foldAll(messages: readonly SdkMessage[]): PersistEntry[] {
  const fold = createFold();
  const context = foldContext();
  return messages.flatMap((message) => [...fold.onSdkMessage(message, context).entries]);
}

/** The distinct upsert keys of the rows in the main agent's book. */
function mainBookKeys(entries: readonly PersistEntry[]): string[] {
  return [...new Set(entries.filter((e) => e.agentId.value === "main-agent").map((e) => e.upsertKey))];
}

/** The distinct upsert keys of the rows in any subagent's book. */
function subagentBookKeys(entries: readonly PersistEntry[]): string[] {
  return [...new Set(entries.filter((e) => e.agentId.value !== "main-agent").map((e) => e.upsertKey))];
}

/** The settled prose of the main book's response unit. */
function settledMainProse(entries: readonly PersistEntry[]): string | undefined {
  for (const entry of entries) {
    if (entry.agentId.value !== "main-agent") continue;
    const item = activityOf(entry)?.item;
    if (item?.case === "response" && item.value.result.case === "success") {
      return item.value.result.value.prose?.markdown;
    }
  }
  return undefined;
}

/** The settled reasoning text of the main book's thinking unit. */
function settledMainThinking(entries: readonly PersistEntry[]): string | undefined {
  for (const entry of entries) {
    if (entry.agentId.value !== "main-agent") continue;
    const item = activityOf(entry)?.item;
    if (item?.case !== "thinking" || item.value.result.case !== "success") continue;
    const reasoning = item.value.result.value.reasoning;
    return reasoning.case === "text" ? reasoning.value.text : undefined;
  }
  return undefined;
}

/**
 * A subagent's `message_start` lands BEFORE the main agent opens its thinking
 * block: the main stream's block must still be keyed to the MAIN message.
 */
function foldSubagentStartBeforeMainThinking(): PersistEntry[] {
  return foldAll([
    eventOn(null, start(MAIN_ID), "u-m-start"),
    eventOn(SPAWN, start(SUB_ID), "u-s-start"),
    eventOn(null, blockStart(0, "thinking"), "u-m-b0"),
    eventOn(null, thinkingDelta(0, "pondering"), "u-m-d0"),
    eventOn(SPAWN, blockStart(0, "text"), "u-s-b0"),
    eventOn(SPAWN, textDelta(0, "sub prose"), "u-s-d0"),
    lineOn(null, MAIN_ID, { type: "thinking", thinking: "pondering" }, "u-m-a0"),
    eventOn(null, blockStop(0), "u-m-s0"),
    lineOn(SPAWN, SUB_ID, { type: "text", text: "sub prose" }, "u-s-a0"),
    eventOn(SPAWN, blockStop(0), "u-s-s0"),
    eventOn(SPAWN, STOP, "u-s-stop"),
    eventOn(null, STOP, "u-m-stop"),
  ]);
}

describe("a subagent's message_start before the main agent's thinking block", () => {
  it("keeps exactly ONE main unit, keyed to the main message", () => {
    // Arrange, Act.
    const entries = foldSubagentStartBeforeMainThinking();

    // Assert.
    expect(mainBookKeys(entries)).toEqual([`activity:${MAIN_ID}:0`]);
  });

  it("keeps exactly ONE subagent unit, keyed to the subagent's message", () => {
    // Arrange, Act.
    const entries = foldSubagentStartBeforeMainThinking();

    // Assert.
    expect(subagentBookKeys(entries)).toEqual([`activity:${SUB_ID}:0`]);
  });
});

/** A subagent's whole response lands BETWEEN two deltas of the main agent's open block. */
function foldSubagentMidMainBlock(kind: "text" | "thinking"): PersistEntry[] {
  const delta = kind === "text" ? textDelta : thinkingDelta;
  const settled =
    kind === "text" ? { type: "text", text: "Hello world" } : { type: "thinking", thinking: "Hello world" };
  return foldAll([
    eventOn(null, start(MAIN_ID), "u-m-start"),
    eventOn(null, blockStart(0, kind), "u-m-b0"),
    eventOn(null, delta(0, "Hello "), "u-m-d0"),
    eventOn(SPAWN, start(SUB_ID), "u-s-start"),
    eventOn(SPAWN, blockStart(0, "text"), "u-s-b0"),
    eventOn(SPAWN, textDelta(0, "sub prose"), "u-s-d0"),
    eventOn(null, delta(0, "world"), "u-m-d1"),
    lineOn(SPAWN, SUB_ID, { type: "text", text: "sub prose" }, "u-s-a0"),
    lineOn(null, MAIN_ID, settled, "u-m-a0"),
    eventOn(null, blockStop(0), "u-m-s0"),
    eventOn(SPAWN, blockStop(0), "u-s-s0"),
    eventOn(null, STOP, "u-m-stop"),
    eventOn(SPAWN, STOP, "u-s-stop"),
  ]);
}

describe("a subagent's message_start in the middle of the main agent's text block", () => {
  it("keeps exactly ONE main unit", () => {
    // Arrange, Act.
    const entries = foldSubagentMidMainBlock("text");

    // Assert.
    expect(mainBookKeys(entries)).toEqual([`activity:${MAIN_ID}:0`]);
  });

  it("settles that unit with the WHOLE text", () => {
    // Arrange, Act.
    const entries = foldSubagentMidMainBlock("text");

    // Assert.
    expect(settledMainProse(entries)).toBe("Hello world");
  });
});

describe("a subagent's message_start in the middle of the main agent's thinking block", () => {
  it("keeps exactly ONE main unit", () => {
    // Arrange, Act.
    const entries = foldSubagentMidMainBlock("thinking");

    // Assert.
    expect(mainBookKeys(entries)).toEqual([`activity:${MAIN_ID}:0`]);
  });

  it("settles that unit with the WHOLE reasoning", () => {
    // Arrange, Act.
    const entries = foldSubagentMidMainBlock("thinking");

    // Assert.
    expect(settledMainThinking(entries)).toBe("Hello world");
  });
});

describe("a thinking-token estimate while a subagent streams its own reasoning", () => {
  it("reaches the MAIN agent's open reasoning unit", () => {
    // Arrange, Act.
    const entries = foldAll([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(null, blockStart(0, "thinking"), "u-m-b0"),
      eventOn(SPAWN, start(SUB_ID), "u-s-start"),
      eventOn(SPAWN, blockStart(0, "thinking"), "u-s-b0"),
      {
        type: "system",
        subtype: "thinking_tokens",
        estimated_tokens: 40,
        estimated_tokens_delta: 10,
        uuid: "u-tokens",
        session_id: "session-1",
      } as unknown as SdkMessage,
    ]);

    // Assert.
    expect(keysFor(entries, "activity.thinking.update.withheld")).toEqual([`activity:${MAIN_ID}:0`]);
  });
});

/** One `system:thinking_tokens` estimate. */
const THINKING_TOKENS = {
  type: "system",
  subtype: "thinking_tokens",
  estimated_tokens: 40,
  estimated_tokens_delta: 10,
  uuid: "u-tokens",
  session_id: "session-1",
} as unknown as SdkMessage;

describe("a thinking-token estimate after a subagent's un-streamed line", () => {
  it("still reaches the MAIN agent's open reasoning unit", () => {
    // Arrange, Act.
    const entries = foldAll([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(null, blockStart(0, "thinking"), "u-m-b0"),
      lineOn(SPAWN, SUB_ID, { type: "thinking", thinking: "sub reasoning" }, "u-s-a0"),
      THINKING_TOKENS,
    ]);

    // Assert.
    expect(keysFor(entries, "activity.thinking.update.withheld")).toEqual([`activity:${MAIN_ID}:0`]);
  });
});

describe("a thinking-token estimate with no response open on the main stream", () => {
  it("produces nothing, even while a subagent's reasoning is open", () => {
    // Arrange, Act.
    const entries = foldAll([
      eventOn(SPAWN, start(SUB_ID), "u-s-start"),
      eventOn(SPAWN, blockStart(0, "thinking"), "u-s-b0"),
      THINKING_TOKENS,
    ]);

    // Assert.
    expect(keysFor(entries, "activity.thinking.update.withheld")).toEqual([]);
  });
});

describe("usage over interleaved streams", () => {
  it("attaches once PER STREAM, to each stream's own first block", () => {
    // Arrange, Act: the main response's second line arrives after a subagent's line.
    const entries = foldAll([
      lineOn(null, MAIN_ID, { type: "text", text: "main first" }, "u-m-a0"),
      lineOn(SPAWN, SUB_ID, { type: "text", text: "sub first" }, "u-s-a0"),
      lineOn(null, MAIN_ID, { type: "text", text: "main second" }, "u-m-a1"),
    ]);

    // Assert.
    const carrying = entries.filter((e) => activityOf(e)?.usage !== undefined).map((e) => e.upsertKey);
    expect(carrying).toEqual([`activity:${MAIN_ID}:0`, `activity:${SUB_ID}:0`]);
  });
});

describe("a subagent block opened with no message_start on ITS stream", () => {
  it("is skipped rather than borrowing the main stream's open response", () => {
    // Arrange, Act.
    const entries = foldAll([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(SPAWN, blockStart(0, "text"), "u-s-b0"),
    ]);

    // Assert.
    expect(entries).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// A stream's block state is dropped at the stream's end.
// ---------------------------------------------------------------------------

describe("a stream's block state", () => {
  it("is created on the stream's first message_start", () => {
    // Arrange.
    const streams = new StreamBlocks();

    // Act.
    streams.beginResponse(SPAWN, SUB_ID);

    // Assert.
    expect(streams.openResponse(SPAWN)?.messageId).toBe(SUB_ID);
  });

  it("is created on the first line of an un-streamed response", () => {
    // Arrange.
    const streams = new StreamBlocks();

    // Act.
    streams.responseFor(SPAWN, SUB_ID);

    // Assert.
    expect(streams.openStreams).toBe(1);
  });

  it("is dropped at the stream's message_stop", () => {
    // Arrange.
    const streams = new StreamBlocks();
    streams.beginResponse(SPAWN, SUB_ID);

    // Act.
    streams.endResponse(SPAWN);

    // Assert.
    expect(streams.openStreams).toBe(0);
  });

  it("is NOT dropped by ANOTHER stream's message_stop", () => {
    // Arrange.
    const streams = new StreamBlocks();
    streams.beginResponse(null, MAIN_ID);
    streams.beginResponse(SPAWN, SUB_ID);

    // Act.
    streams.endResponse(SPAWN);

    // Assert.
    expect(streams.openResponse(null)?.messageId).toBe(MAIN_ID);
  });

  it("is dropped for the main stream at the turn's end", () => {
    // Arrange.
    const streams = new StreamBlocks();
    streams.beginResponse(null, MAIN_ID);

    // Act.
    streams.endTurn();

    // Assert.
    expect(streams.openStreams).toBe(0);
  });

  it("outlives the turn's end for a subagent's stream", () => {
    // Arrange.
    const streams = new StreamBlocks();
    streams.beginResponse(SPAWN, SUB_ID);

    // Act.
    streams.endTurn();

    // Assert.
    expect(streams.openResponse(SPAWN)?.messageId).toBe(SUB_ID);
  });

  it("is dropped for a subagent's stream at its agent's end", () => {
    // Arrange.
    const streams = new StreamBlocks();
    streams.beginResponse(SPAWN, SUB_ID);

    // Act.
    streams.endAgent(SPAWN);

    // Assert.
    expect(streams.openStreams).toBe(0);
  });

  it("is never dropped for the main stream by an agent's end", () => {
    // Arrange.
    const streams = new StreamBlocks();
    streams.beginResponse(null, MAIN_ID);

    // Act.
    streams.endAgent("");

    // Assert.
    expect(streams.openResponse(null)?.messageId).toBe(MAIN_ID);
  });

  it("keeps a unit AT or ABOVE an assistant line's block pending", () => {
    // Arrange.
    const streams = new StreamBlocks();
    const state = streams.beginResponse(null, MAIN_ID);
    state.unsettled.set(1, create(conversationv1.AgentActivityIdSchema, { value: `${MAIN_ID}:1` }));

    // Act.
    streams.settledBelow(null, 1);

    // Assert.
    expect([...state.unsettled.keys()]).toEqual([1]);
  });

  it("drops through the fold at message_stop, so a later delta is keyed to nothing", () => {
    // Arrange, Act.
    const entries = foldAll([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(null, STOP, "u-m-stop"),
      eventOn(null, textDelta(0, "late"), "u-m-late"),
    ]);

    // Assert.
    expect(entries).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// The log records these invariants are reported through.
// ---------------------------------------------------------------------------

const mockedWriteSync = vi.mocked(writeSync);

/** A persisted log record, as the canonical logger writes one. */
interface LogRecord {
  readonly level: string;
  readonly message: string;
  readonly context: Record<string, unknown>;
}

/** The records the canonical logger persisted while `act` ran. */
function recordsDuring(act: () => void): LogRecord[] {
  const before = mockedWriteSync.mock.calls.length;
  act();
  const calls = mockedWriteSync.mock.calls.slice(before) as unknown as Array<[number, Buffer, number, number]>;
  return calls.map(([, bytes, offset, length]) =>
    JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as LogRecord,
  );
}

const UNSETTLED = "invariant violated: a streamed unit was started and never settled; its row stays unsettled";

/** The unsettled-unit error records folding `messages` wrote. */
function unsettledReports(messages: readonly SdkMessage[]): LogRecord[] {
  return recordsDuring(() => foldAll(messages)).filter((record) => record.message === UNSETTLED);
}

describe("a streamed unit its stream started and never settled", () => {
  it("is logged at ERROR level at the stream's message_stop", () => {
    // Arrange, Act.
    const reports = unsettledReports([
      eventOn(SPAWN, start(SUB_ID), "u-s-start"),
      eventOn(SPAWN, blockStart(0, "text"), "u-s-b0"),
      eventOn(SPAWN, STOP, "u-s-stop"),
    ]);

    // Assert.
    expect(reports.map((r) => r.level)).toEqual(["error"]);
  });

  it("names the stream, the message id and the unit id", () => {
    // Arrange, Act.
    const reports = unsettledReports([
      eventOn(SPAWN, start(SUB_ID), "u-s-start"),
      eventOn(SPAWN, blockStart(0, "text"), "u-s-b0"),
      eventOn(SPAWN, STOP, "u-s-stop"),
    ]);

    // Assert.
    expect(reports[0]?.context).toMatchObject({
      stream: SPAWN,
      message_id: SUB_ID,
      activity_id: `${SUB_ID}:0`,
      detected_at: "message_stop",
    });
  });

  it("names the main stream as `main`", () => {
    // Arrange, Act.
    const reports = unsettledReports([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(null, blockStart(0, "thinking"), "u-m-b0"),
      eventOn(null, STOP, "u-m-stop"),
    ]);

    // Assert.
    expect(reports[0]?.context.stream).toBe("main");
  });

  it("is caught at the assistant line for a LATER block of the same response", () => {
    // Arrange, Act: block 0 closes without its line, and the line for block 1 arrives.
    const reports = unsettledReports([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(null, blockStart(0, "text"), "u-m-b0"),
      eventOn(null, blockStop(0), "u-m-s0"),
      lineOn(null, MAIN_ID, { type: "text", text: "second" }, "u-m-a1"),
    ]);

    // Assert.
    expect(reports.map((r) => r.context.detected_at)).toEqual(["assistant"]);
  });

  it("is caught at the assistant line of a DIFFERENT response on the same stream", () => {
    // Arrange, Act.
    const reports = unsettledReports([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(null, blockStart(0, "text"), "u-m-b0"),
      lineOn(null, "msg_next", { type: "text", text: "next" }, "u-n-a0"),
    ]);

    // Assert.
    expect(reports.map((r) => r.context.message_id)).toEqual([MAIN_ID]);
  });

  it("is caught at the next message_start on the same stream", () => {
    // Arrange, Act.
    const reports = unsettledReports([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(null, blockStart(0, "text"), "u-m-b0"),
      eventOn(null, start("msg_next"), "u-n-start"),
    ]);

    // Assert.
    expect(reports.map((r) => r.context.detected_at)).toEqual(["message_start"]);
  });

  it("is reported exactly once however many ends follow", () => {
    // Arrange, Act.
    const reports = unsettledReports([
      eventOn(null, start(MAIN_ID), "u-m-start"),
      eventOn(null, blockStart(0, "text"), "u-m-b0"),
      eventOn(null, blockStop(0), "u-m-s0"),
      lineOn(null, MAIN_ID, { type: "text", text: "second" }, "u-m-a1"),
      eventOn(null, STOP, "u-m-stop"),
    ]);

    // Assert.
    expect(reports).toHaveLength(1);
  });

  it("is NOT reported for a subagent message_start interleaved into an open main block", () => {
    // Arrange, Act.
    const reports = recordsDuring(() => foldSubagentMidMainBlock("text")).filter((r) => r.level === "error");

    // Assert.
    expect(reports).toEqual([]);
  });

  it("is NOT reported for a response whose every unit settled", () => {
    // Arrange, Act.
    const reports = recordsDuring(() => foldStreamedResponse()).filter((r) => r.message === UNSETTLED);

    // Assert.
    expect(reports).toEqual([]);
  });
});

describe("the warnings a stream with no identity still raises", () => {
  it("warns when a message_start names no message id", () => {
    // Arrange, Act.
    const records = recordsDuring(() => foldAll([eventOn(null, { type: "message_start", message: {} }, "u-noid")]));

    // Assert.
    expect(
      records
        .filter((r) => r.message === "a message_start named no message id; no block can be identified")
        .map((r) => ({ level: r.level, stream: r.context.stream })),
    ).toEqual([{ level: "warn", stream: "main" }]);
  });

  it("warns when a content block opens with no message_start on its stream", () => {
    // Arrange, Act.
    const records = recordsDuring(() => foldAll([eventOn(SPAWN, blockStart(0, "text"), "u-orphan")]));

    // Assert.
    expect(
      records
        .filter((r) => r.message === "a content block opened with no message_start seen on its stream; skipped")
        .map((r) => ({ level: r.level, stream: r.context.stream })),
    ).toEqual([{ level: "warn", stream: SPAWN }]);
  });
});

type AssistantMessage = Extract<SdkMessage, { type: "assistant" }>;

describe("a tool call's stream", () => {
  it("is recorded on the call as the spawning call its line named", () => {
    // Arrange: the spawn is held, so its agent's calls are too.
    const registry = createCallRegistry();
    registry.remember({ toolUseId: SPAWN, toolName: "Agent", input: {}, startedAtMs: 1, agentId: MAIN_AGENT });

    // Act
    convertAssistantMessage(
      lineOn(SPAWN, SUB_ID, { type: "tool_use", id: "toolu_sub", name: "Read", input: { file_path: "/a" } }, "u-sub") as AssistantMessage,
      foldContext(),
      new StreamBlocks(),
      registry,
      TOOL_CONVERTERS,
    );

    // Assert
    expect(registry.peek("toolu_sub")?.spawningCall).toBe(SPAWN);
  });

  it("is unset on a call the main agent made", () => {
    // Arrange
    const registry = createCallRegistry();

    // Act
    convertAssistantMessage(
      lineOn(null, MAIN_ID, { type: "tool_use", id: "toolu_main", name: "Read", input: { file_path: "/a" } }, "u-main") as AssistantMessage,
      foldContext(),
      new StreamBlocks(),
      registry,
      TOOL_CONVERTERS,
    );

    // Assert
    expect(registry.peek("toolu_main")?.spawningCall).toBeUndefined();
  });
});
