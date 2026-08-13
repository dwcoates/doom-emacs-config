import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import {
  StreamMessageTracker,
  isEphemeral,
  streamEventToContentArriving,
  streamEventToResponseTiming,
  toLiveEntry,
  toStoredStreamEntry,
  toolProgressToHeartbeat,
} from "../src/proto/delta.js";
import type { ContentArriving, ExternalEntry } from "../src/uds/proto.js";

function loadStream(name: string): Record<string, unknown> {
  const line = readFileSync(new URL(`../../../../testdata/corpus/stream/${name}.jsonl`, import.meta.url), "utf8").split("\n")[0]!;
  return JSON.parse(line) as Record<string, unknown>;
}

/** The ContentArriving payload of a record this converter produced. */
function arriving(external: ExternalEntry | null): ContentArriving {
  if (external === null) throw new Error("expected a record");
  if (external.entry.case !== "message") throw new Error("expected the message arm");
  if (external.entry.value.payload.case !== "contentArriving") throw new Error("expected contentArriving");
  return external.entry.value.payload.value;
}

const MSG = "msg_01ABC";

// ---------------------------------------------------------------------------
// stream_event → ContentArriving: the fragment arms.
// ---------------------------------------------------------------------------

describe("streamEventToContentArriving fragment arms", () => {
  it("relays a text fragment verbatim", () => {
    const a = arriving(streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), { messageId: MSG }));
    if (a.fragment.case !== "text") throw new Error("arm");
    expect(a.fragment.value).toBe("hi");
  });

  it("relays a thinking fragment on its own arm, never as text", () => {
    const a = arriving(streamEventToContentArriving(loadStream("stream_event-content_block_delta-thinking"), { messageId: MSG }));
    expect(a.fragment.case).toBe("thinking");
  });

  it("relays tool arguments as INCOMPLETE json on the arguments arm", () => {
    const msg = {
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_delta", index: 2, delta: { type: "input_json_delta", partial_json: "{\"pat" } },
    };
    const a = arriving(streamEventToContentArriving(msg, { messageId: MSG, toolUseId: "toolu_1" }));
    if (a.fragment.case !== "argumentsJson") throw new Error("arm");
    expect(a.fragment.value).toBe("{\"pat");
  });

  it("carries the block index so two fragments of one response land apart", () => {
    const a = arriving(streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), { messageId: MSG }));
    expect(a.blockIndex).toBe(1);
  });

  it("yields nothing for a signature delta, which the content model cannot hold", () => {
    expect(streamEventToContentArriving(loadStream("stream_event-content_block_delta-signature"), { messageId: MSG })).toBeNull();
  });

  it("yields nothing for a structural frame carrying no fragment", () => {
    expect(streamEventToContentArriving(loadStream("stream_event-content_block_stop"), { messageId: MSG })).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// The identity a preview must carry for the settled message to replace it.
// ---------------------------------------------------------------------------

describe("streamEventToContentArriving identity", () => {
  it("keys on the streamed Anthropic message id, NOT the envelope uuid", () => {
    // The fixture's envelope uuid is unique to this one stream_event; keying on
    // it gave every chunk a different id, which opened a bubble per chunk.
    const external = streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), { messageId: MSG })!;
    if (external.entry.case !== "message") throw new Error("arm");
    expect(external.entry.value.messageId).toBe(MSG);
  });

  it("names itself as its own feed row, since a response is one", () => {
    const external = streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), { messageId: MSG })!;
    if (external.entry.case !== "message") throw new Error("arm");
    expect(external.entry.value.topLevelMessageId).toBe(MSG);
  });

  it("states root parentage explicitly rather than leaving it unresolved", () => {
    const external = streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), { messageId: MSG })!;
    if (external.entry.case !== "message") throw new Error("arm");
    expect(external.entry.value.parent?.parent.case).toBe("root");
  });

  it("attributes the preview to the agent, resolved rather than inferred", () => {
    const external = streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), { messageId: MSG })!;
    if (external.entry.case !== "message") throw new Error("arm");
    expect(external.entry.value.author?.author.case).toBe("agent");
  });

  it("carries the conversation the fragment belongs to", () => {
    const external = streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), { messageId: MSG })!;
    expect(external.sessionId).toBe("79f88fa5-93c3-45d0-8376-ef9812240092");
  });

  it("stamps the producer's observation time", () => {
    const external = streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), { messageId: MSG, nowMs: 1000 })!;
    expect(external.producedAtMs).toBe(1000n);
  });
});

// ---------------------------------------------------------------------------
// The refusals. Each is a different reason, and none invents a parent.
// ---------------------------------------------------------------------------

describe("streamEventToContentArriving refusals", () => {
  it("refuses a fragment with no in-flight message id", () => {
    expect(streamEventToContentArriving(loadStream("stream_event-content_block_delta-text"), {})).toBeNull();
  });

  it("refuses subagent content rather than emitting it as a feed row", () => {
    // Arrange: the stream marks subagent content by naming the tool call that
    // spawned it, and never carries the containing message's own id.
    const msg = {
      ...loadStream("stream_event-content_block_delta-text"),
      parent_tool_use_id: "toolu_parent",
    };

    // Act / Assert.
    expect(streamEventToContentArriving(msg, { messageId: MSG })).toBeNull();
  });

  it("throws on an arguments fragment with no bound tool block", () => {
    const msg = {
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_delta", index: 2, delta: { type: "input_json_delta", partial_json: "{" } },
    };
    expect(() => streamEventToContentArriving(msg, { messageId: MSG })).toThrow(/no bound tool-use identity/);
  });
});

// ---------------------------------------------------------------------------
// stream_event → ResponseTiming.
// ---------------------------------------------------------------------------

describe("streamEventToResponseTiming", () => {
  function timing(msg: Record<string, unknown>, messageId = MSG) {
    const entry = streamEventToResponseTiming(msg, { messageId });
    if (entry === null) throw new Error("expected a record");
    if (entry.external?.entry.case !== "bookkeeping") throw new Error("expected bookkeeping");
    if (entry.external.entry.value.kind.case !== "responseTiming") throw new Error("expected responseTiming");
    return entry.external.entry.value.kind.value;
  }

  it("carries the first-token latency the message_start stamped", () => {
    expect(timing(loadStream("stream_event-message_start")).firstTokenMs).toBe(865n);
  });

  it("names the message it measured", () => {
    expect(timing(loadStream("stream_event-message_start")).messageId).toBe(MSG);
  });

  it("leaves total_ms at zero, because this frame measured no total", () => {
    expect(timing(loadStream("stream_event-message_start")).totalMs).toBe(0n);
  });

  it("is DURABLE, so it carries the observing plane for the store", () => {
    const entry = streamEventToResponseTiming(loadStream("stream_event-message_start"), { messageId: MSG })!;
    expect(entry.internal?.plane?.plane.case).toBe("stream");
  });

  it("yields nothing for a frame that is not a message_start", () => {
    expect(streamEventToResponseTiming(loadStream("stream_event-content_block_delta-text"), { messageId: MSG })).toBeNull();
  });

  it("yields nothing when the message_start carries no ttft stamp", () => {
    const msg = { ...loadStream("stream_event-message_start") };
    delete msg["ttft_ms"];
    expect(streamEventToResponseTiming(msg, { messageId: MSG })).toBeNull();
  });

  it("yields nothing for a non-positive stamp rather than reporting it", () => {
    const msg = { ...loadStream("stream_event-message_start"), ttft_ms: 0 };
    expect(streamEventToResponseTiming(msg, { messageId: MSG })).toBeNull();
  });

  it("refuses a timing it cannot attribute to a message", () => {
    expect(streamEventToResponseTiming(loadStream("stream_event-message_start"), {})).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// tool_progress → Heartbeat.
// ---------------------------------------------------------------------------

describe("toolProgressToHeartbeat", () => {
  const progress = { type: "tool_progress", session_id: "sess", tool_use_id: "toolu_9", tool_name: "Bash", elapsed_time_seconds: 12 };

  it("reports the tool-use id as the live work identity", () => {
    const external = toolProgressToHeartbeat(progress)!;
    if (external.entry.case !== "bookkeeping") throw new Error("arm");
    if (external.entry.value.kind.case !== "heartbeat") throw new Error("kind");
    expect(external.entry.value.kind.value.liveWorkIds).toEqual(["toolu_9"]);
  });

  it("carries the conversation the work belongs to", () => {
    expect(toolProgressToHeartbeat(progress)!.sessionId).toBe("sess");
  });

  it("refuses a frame naming no tool, rather than asserting nothing is running", () => {
    expect(toolProgressToHeartbeat({ type: "tool_progress", session_id: "sess" })).toBeNull();
  });

  it("reads the camelCase disk spelling of the tool id", () => {
    const external = toolProgressToHeartbeat({ type: "tool_progress", sessionId: "sess", toolUseId: "toolu_9" })!;
    if (external.entry.case !== "bookkeeping") throw new Error("arm");
    if (external.entry.value.kind.case !== "heartbeat") throw new Error("kind");
    expect(external.entry.value.kind.value.liveWorkIds).toEqual(["toolu_9"]);
  });
});

// ---------------------------------------------------------------------------
// Dispatch: which route each SDK message takes.
// ---------------------------------------------------------------------------

describe("toLiveEntry", () => {
  it("routes a content fragment to the live path", () => {
    expect(toLiveEntry(loadStream("stream_event-content_block_delta-text"), { messageId: MSG })).not.toBeNull();
  });

  it("routes a tool_progress to the live path", () => {
    expect(toLiveEntry({ type: "tool_progress", session_id: "s", tool_use_id: "t" })).not.toBeNull();
  });

  it("routes nothing else to the live path", () => {
    expect(toLiveEntry(loadStream("assistant"))).toBeNull();
  });

  it("yields nothing for a non-object", () => {
    expect(toLiveEntry("nope")).toBeNull();
  });
});

describe("toStoredStreamEntry", () => {
  it("routes a stamped message_start to the durable path", () => {
    expect(toStoredStreamEntry(loadStream("stream_event-message_start"), { messageId: MSG })).not.toBeNull();
  });

  it("leaves a content fragment off the durable path", () => {
    expect(toStoredStreamEntry(loadStream("stream_event-content_block_delta-text"), { messageId: MSG })).toBeNull();
  });

  it("leaves a tool_progress off the durable path", () => {
    expect(toStoredStreamEntry({ type: "tool_progress", session_id: "s", tool_use_id: "t" })).toBeNull();
  });
});

describe("isEphemeral", () => {
  it("claims stream_event for the live relay", () => {
    expect(isEphemeral({ type: "stream_event" })).toBe(true);
  });

  it("claims tool_progress for the live relay", () => {
    expect(isEphemeral({ type: "tool_progress" })).toBe(true);
  });

  it("leaves every other family to the persistent converter", () => {
    expect(isEphemeral({ type: "assistant" })).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// StreamMessageTracker: the identity the stream carries only once.
// ---------------------------------------------------------------------------

describe("StreamMessageTracker", () => {
  it("adopts the message id a message_start announces", () => {
    const t = new StreamMessageTracker();
    t.observe(loadStream("stream_event-message_start"));
    expect(t.current()).toBe("msg_011CdKQJaCBXrp4nizdg3fXW");
  });

  it("clears the identity when the message stops", () => {
    const t = new StreamMessageTracker();
    t.observe(loadStream("stream_event-message_start"));
    t.observe(loadStream("stream_event-message_stop"));
    expect(t.current()).toBe("");
  });

  it("binds a tool block's use-id to its API block index", () => {
    const t = new StreamMessageTracker();
    t.observe(loadStream("stream_event-message_start"));
    t.observe({
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_start", index: 3, content_block: { type: "tool_use", id: "toolu_5" } },
    });
    const bound = t.toolUseIdFor({
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_delta", index: 3, delta: { type: "input_json_delta", partial_json: "{" } },
    });
    expect(bound).toBe("toolu_5");
  });

  it("fails loudly on a tool block start with no active message identity", () => {
    const t = new StreamMessageTracker();
    expect(() => t.observe({
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_start", index: 0, content_block: { type: "tool_use", id: "toolu_5" } },
    })).toThrow(/no active API message identity/);
  });

  it("fails loudly when a redelivered tool block contradicts its binding", () => {
    const t = new StreamMessageTracker();
    t.observe(loadStream("stream_event-message_start"));
    const start = (id: string) => ({
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_start", index: 1, content_block: { type: "tool_use", id } },
    });
    t.observe(start("toolu_a"));
    expect(() => t.observe(start("toolu_b"))).toThrow(/conflicts with the bound tool-use identity/);
  });

  it("ignores a non-tool content block start", () => {
    const t = new StreamMessageTracker();
    t.observe(loadStream("stream_event-message_start"));
    t.observe(loadStream("stream_event-content_block_start"));
    expect(t.current()).toBe("msg_011CdKQJaCBXrp4nizdg3fXW");
  });
});
