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
});
