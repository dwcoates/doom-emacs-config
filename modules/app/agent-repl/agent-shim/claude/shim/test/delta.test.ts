import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import {
  StreamMessageTracker,
  isEphemeral,
  streamEventToContentDelta,
  toEphemeralEvent,
  toolProgressToHeartbeat,
} from "../src/proto/delta.js";
import { EventClass, Plane } from "../src/uds/proto.js";

function loadStream(name: string): Record<string, unknown> {
  const line = readFileSync(new URL(`../../../../testdata/corpus/stream/${name}.jsonl`, import.meta.url), "utf8").split("\n")[0]!;
  return JSON.parse(line) as Record<string, unknown>;
}

// ---------------------------------------------------------------------------
// stream_event → ContentDelta (every ContentBlockDelta arm).
// ---------------------------------------------------------------------------

describe("streamEventToContentDelta arms", () => {
  function delta(name: string) {
    const evt = streamEventToContentDelta(loadStream(name), { nowMs: 1000 });
    expect(evt).not.toBeNull();
    if (evt!.payload.case !== "contentDelta") throw new Error("not a content delta");
    return evt!.payload.value;
  }

  it("text arm carries the text and block_index", () => {
    const d = delta("stream_event-content_block_delta-text");
    expect(d.delta.case).toBe("text");
    if (d.delta.case !== "text") throw new Error("arm");
    expect(d.delta.value).toBe("hi");
    expect(d.blockIndex).toBe(1);
  });

  it("carries the streamed message id, NOT the event's own envelope uuid", () => {
    // The fixture's envelope uuid is unique to this one stream_event; keying on
    // it gave every chunk a different id, which is what made the frontend open
    // a bubble per chunk.
    const evt = streamEventToContentDelta(loadStream("stream_event-content_block_delta-thinking"), {
      messageId: "msg_01ABC",
    })!;
    if (evt.payload.case !== "contentDelta") throw new Error("case");
    expect(evt.payload.value.delta.case).toBe("thinking");
    expect(evt.payload.value.uuid).toBe("msg_01ABC");
    expect(evt.payload.value.uuid).not.toBe("aa3da566-9d60-4f94-9e99-b9c1f68fc9b4");
  });

  it("thinking arm with null estimated_tokens → 0", () => {
    expect(delta("stream_event-content_block_delta-thinking").estimatedTokens).toBe(0n);
  });

  it("thinking arm with a number estimated_tokens → bigint (synthetic)", () => {
    const msg = { type: "stream_event", event: { type: "content_block_delta", index: 0, delta: { type: "thinking_delta", thinking: "x", estimated_tokens: 42 } }, session_id: "s", uuid: "u" };
    const evt = streamEventToContentDelta(msg)!;
    if (evt.payload.case !== "contentDelta") throw new Error("case");
    expect(evt.payload.value.estimatedTokens).toBe(42n);
  });

  it("signature arm", () => {
    expect(delta("stream_event-content_block_delta-signature").delta.case).toBe("signature");
  });

  it("input_json arm (synthetic)", () => {
    const msg = { type: "stream_event", event: { type: "content_block_delta", index: 3, delta: { type: "input_json_delta", partial_json: "{\"k\":1}" } }, session_id: "s", uuid: "u" };
    const evt = streamEventToContentDelta(msg)!;
    if (evt.payload.case !== "contentDelta" || evt.payload.value.delta.case !== "inputJson") throw new Error("arm");
    expect(evt.payload.value.delta.value).toBe("{\"k\":1}");
  });

  it("is classified EPHEMERAL, plane STREAM, seq 0", () => {
    const evt = streamEventToContentDelta(loadStream("stream_event-content_block_delta-text"))!;
    expect(evt.class).toBe(EventClass.EPHEMERAL);
    expect(evt.plane).toBe(Plane.STREAM);
    expect(evt.seq).toBe(0n);
  });
});

// ---------------------------------------------------------------------------
// Structural stream_event frames carry no live-typing content → null.
// ---------------------------------------------------------------------------

describe("streamEventToContentDelta ignores structural frames", () => {
  for (const name of [
    "stream_event-message_start",
    "stream_event-message_stop",
    "stream_event-message_delta",
    "stream_event-content_block_start",
    "stream_event-content_block_stop",
  ]) {
    it(`${name} → null`, () => {
      expect(streamEventToContentDelta(loadStream(name))).toBeNull();
    });
  }
});

// ---------------------------------------------------------------------------
// tool_progress → HeartbeatProgress.
// ---------------------------------------------------------------------------

describe("toolProgressToHeartbeat", () => {
  const msg = {
    type: "tool_progress",
    tool_use_id: "toolu_abc",
    tool_name: "Bash",
    parent_tool_use_id: "toolu_parent",
    elapsed_time_seconds: 12.5,
    session_id: "s1",
    uuid: "u1",
  };

  it("maps tool_use_id and elapsed seconds", () => {
    const evt = toolProgressToHeartbeat(msg, { nowMs: 5 });
    if (evt.payload.case !== "heartbeatProgress") throw new Error("case");
    expect(evt.payload.value.toolUseId).toBe("toolu_abc");
    expect(evt.payload.value.elapsedSeconds).toBeCloseTo(12.5);
  });

  it("is classified EPHEMERAL", () => {
    expect(toolProgressToHeartbeat(msg).class).toBe(EventClass.EPHEMERAL);
  });
});

// ---------------------------------------------------------------------------
// Dispatcher + classification.
// ---------------------------------------------------------------------------

describe("toEphemeralEvent / isEphemeral", () => {
  it("routes stream_event to a ContentDelta", () => {
    const evt = toEphemeralEvent(loadStream("stream_event-content_block_delta-text"));
    expect(evt?.payload.case).toBe("contentDelta");
  });

  it("routes tool_progress to a HeartbeatProgress", () => {
    const evt = toEphemeralEvent({ type: "tool_progress", tool_use_id: "t", session_id: "s" });
    expect(evt?.payload.case).toBe("heartbeatProgress");
  });

  it("returns null for a non-ephemeral message (the persistent path owns it)", () => {
    expect(toEphemeralEvent(loadStream("assistant"))).toBeNull();
  });

  it("isEphemeral true for stream_event and tool_progress only", () => {
    expect(isEphemeral({ type: "stream_event" })).toBe(true);
    expect(isEphemeral({ type: "tool_progress" })).toBe(true);
    expect(isEphemeral({ type: "assistant" })).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// StreamMessageTracker — which message the deltas belong to.
//
// A content_block_delta says nothing about its message; the identity arrives
// once, on the message_start that opened it. Keying deltas on the SDK envelope
// uuid instead gave every chunk a different id, so the frontend opened a new
// bubble per chunk rather than growing one.
// ---------------------------------------------------------------------------

describe("StreamMessageTracker", () => {
  const start = (id: string) => ({
    type: "stream_event",
    session_id: "s",
    uuid: "envelope-of-the-start-event",
    event: { type: "message_start", message: { id, type: "message", role: "assistant" } },
  });
  const chunk = (text: string, envelopeUuid: string) => ({
    type: "stream_event",
    session_id: "s",
    uuid: envelopeUuid,
    event: { type: "content_block_delta", index: 0, delta: { type: "text_delta", text } },
  });
  const stop = () => ({
    type: "stream_event",
    session_id: "s",
    uuid: "envelope-of-the-stop-event",
    event: { type: "message_stop" },
  });

  it("adopts the message id from message_start", () => {
    // Arrange
    const t = new StreamMessageTracker();
    // Act
    t.observe(start("msg_01ABC"));
    // Assert
    expect(t.current()).toBe("msg_01ABC");
  });

  it("holds that id across every delta of the message", () => {
    // Arrange: the whole point — consecutive chunks must reconcile together.
    const t = new StreamMessageTracker();
    t.observe(start("msg_01ABC"));

    // Act: three chunks, each with its OWN envelope uuid, as the SDK emits them.
    const ids = ["e1", "e2", "e3"].map((e) => {
      const msg = chunk("x", e);
      t.observe(msg);
      const evt = streamEventToContentDelta(msg, { messageId: t.current() })!;
      if (evt.payload.case !== "contentDelta") throw new Error("case");
      return evt.payload.value.uuid;
    });

    // Assert: one id, not three.
    expect(ids).toEqual(["msg_01ABC", "msg_01ABC", "msg_01ABC"]);
  });

  it("clears the id at message_stop", () => {
    // Arrange
    const t = new StreamMessageTracker();
    t.observe(start("msg_01ABC"));
    // Act
    t.observe(stop());
    // Assert: nothing is in flight between messages.
    expect(t.current()).toBe("");
  });

  it("switches to the next message when a new one starts", () => {
    // Arrange: two messages in one turn must not share a block.
    const t = new StreamMessageTracker();
    t.observe(start("msg_FIRST"));
    t.observe(stop());
    // Act
    t.observe(start("msg_SECOND"));
    // Assert
    expect(t.current()).toBe("msg_SECOND");
  });

  it("ignores non-stream messages", () => {
    // Arrange: a persistent assistant message must not disturb the tracker.
    const t = new StreamMessageTracker();
    t.observe(start("msg_01ABC"));
    // Act
    t.observe({ type: "assistant", uuid: "u", message: { id: "msg_OTHER" } });
    // Assert
    expect(t.current()).toBe("msg_01ABC");
  });

  it("reports no message when message_start carries no id", () => {
    // Arrange / Act: a malformed start must not invent an id that would
    // silently collide with another message's blocks.
    const t = new StreamMessageTracker();
    t.observe({ type: "stream_event", session_id: "s", event: { type: "message_start", message: {} } });
    // Assert
    expect(t.current()).toBe("");
  });
});
