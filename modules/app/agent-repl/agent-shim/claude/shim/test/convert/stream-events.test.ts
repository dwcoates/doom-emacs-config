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
import { describe, expect, it } from "vitest";
import { createFold } from "../../src/convert/fold.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { activityOf, foldContext } from "./fold-harness.js";
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
