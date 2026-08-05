import { describe, expect, it, vi } from "vitest";
import { readFileSync } from "node:fs";
import {
  StreamMessageTracker,
  isEphemeral,
  streamEventToContentDelta,
  streamEventToMessageLatency,
  toEphemeralEvent,
  toPersistentEvent,
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
    expect(d.toolUseId).toBeUndefined();
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

  it("input_json arm carries the bound tool-use identity (synthetic)", () => {
    const msg = { type: "stream_event", event: { type: "content_block_delta", index: 3, delta: { type: "input_json_delta", partial_json: "{\"k\":1}" } }, session_id: "s", uuid: "u" };
    const evt = streamEventToContentDelta(msg, { messageId: "msg_1", toolUseId: "toolu_3" })!;
    if (evt.payload.case !== "contentDelta" || evt.payload.value.delta.case !== "inputJson") throw new Error("arm");
    expect(evt.payload.value.delta.value).toBe("{\"k\":1}");
    expect(evt.payload.value.toolUseId).toBe("toolu_3");
  });

  it("rejects an input_json arm without a tool-use binding before constructing an event", () => {
    const write = vi.spyOn(process.stderr, "write").mockImplementation(() => true);
    const msg = { type: "stream_event", event: { type: "content_block_delta", index: 3, delta: { type: "input_json_delta", partial_json: "{\"k\":1}" } }, session_id: "s", uuid: "u" };

    expect(() => streamEventToContentDelta(msg, { messageId: "msg_1", agentReplSessionId: "agent-session" })).toThrow("input_json delta has no bound tool-use identity");

    const records = write.mock.calls.map(([line]) => JSON.parse(String(line)) as { agent_repl_session_id?: string; claude_session_id?: string; context: Record<string, unknown> });
    expect(records).toContainEqual(expect.objectContaining({
      agent_repl_session_id: "test-agent-session",
      claude_session_id: "s",
      context: expect.objectContaining({
        api_message_id: "msg_1",
        block_index: 3,
        tool_use_id: null,
        delta_length: 7,
        outcome: "missing_tool_use_id_binding",
      }),
    }));
  });

  it("rejects an unsupported content delta arm without fabricating an event", () => {
    const msg = {
      type: "stream_event",
      event: {
        type: "content_block_delta",
        index: 0,
        delta: { type: "future_vendor_delta", value: "unmodeled" },
      },
      session_id: "s",
      uuid: "u",
    };
    expect(streamEventToContentDelta(msg)).toBeNull();
    expect(toEphemeralEvent(msg)).toBeNull();
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
// stream_event → MessageLatency (the ttft relay).
//
// `ttft_ms` is a top-level field of the stream_event envelope that the SDK
// stamps on message_start — the one structural frame carrying a progress fact,
// and one the ContentDelta mapper drops. Relaying it is what makes first-token
// latency reachable mid-turn instead of only at the turn's result.
// ---------------------------------------------------------------------------

describe("streamEventToMessageLatency", () => {
  /** A message_start frame with an arbitrary top-level ttft stamp. */
  const start = (ttft: unknown) => ({
    type: "stream_event",
    session_id: "s1",
    uuid: "envelope-of-the-start-event",
    ttft_ms: ttft,
    event: { type: "message_start", message: { id: "msg_01ABC", type: "message", role: "assistant" } },
  });

  function latency(msg: Record<string, unknown>, opts?: { nowMs?: number; messageId?: string }) {
    const evt = streamEventToMessageLatency(msg, opts);
    expect(evt).not.toBeNull();
    if (evt!.payload.case !== "messageLatency") throw new Error("not a message latency");
    return evt!.payload.value;
  }

  it("carries the corpus message_start's ttft stamp", () => {
    // Arrange / Act
    const l = latency(loadStream("stream_event-message_start"));
    // Assert: the observed stamp, verbatim.
    expect(l.ttftMs).toBe(865n);
  });

  it("keys the stamp to the streaming message id, not the envelope uuid", () => {
    // Arrange / Act
    const l = latency(start(700), { messageId: "msg_01ABC" });
    // Assert: same key ContentDelta uses, so both describe one message.
    expect(l.uuid).toBe("msg_01ABC");
  });

  it("carries the session id off the envelope", () => {
    expect(streamEventToMessageLatency(start(700))!.sessionId).toBe("s1");
  });

  it("is classified PERSISTENT, plane STREAM, seq 0", () => {
    // Arrange / Act
    const evt = streamEventToMessageLatency(start(700), { nowMs: 42 })!;
    // Assert: store sequencing assigns the durable sequence after write.
    expect([evt.class, evt.plane, evt.seq, evt.producedAtMs]).toEqual([
      EventClass.PERSISTENT,
      Plane.STREAM,
      0n,
      42n,
    ]);
  });

  it("truncates a fractional stamp rather than rejecting it", () => {
    expect(latency(start(864.7)).ttftMs).toBe(864n);
  });

  it("returns null when message_start carries no stamp", () => {
    // Arrange: absence is the common case, not an anomaly.
    const msg = start(undefined);
    delete (msg as Record<string, unknown>)["ttft_ms"];
    // Act / Assert
    expect(streamEventToMessageLatency(msg)).toBeNull();
  });

  it("returns null for a zero stamp (absence, not a measured zero)", () => {
    expect(streamEventToMessageLatency(start(0))).toBeNull();
  });

  it("returns null for a non-numeric stamp", () => {
    expect(streamEventToMessageLatency(start("865"))).toBeNull();
  });

  it("returns null for a content_block_delta frame", () => {
    // Arrange / Act / Assert: the two mappers are mutually exclusive.
    expect(streamEventToMessageLatency(loadStream("stream_event-content_block_delta-text"))).toBeNull();
  });

  it("returns null for a message_stop frame", () => {
    expect(streamEventToMessageLatency(loadStream("stream_event-message_stop"))).toBeNull();
  });
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

describe("toEphemeralEvent / toPersistentEvent / isEphemeral", () => {
  it("routes stream_event to a ContentDelta", () => {
    const evt = toEphemeralEvent(loadStream("stream_event-content_block_delta-text"));
    expect(evt?.payload.case).toBe("contentDelta");
  });

  it("routes a stamped message_start to a persistent MessageLatency", () => {
    // Arrange / Act: the frame the ContentDelta mapper drops.
    const evt = toPersistentEvent(loadStream("stream_event-message_start"));
    // Assert
    expect(evt?.payload.case).toBe("messageLatency");
    expect(evt?.class).toBe(EventClass.PERSISTENT);
  });

  it("returns null for a message_start with no ttft stamp", () => {
    // Arrange: a structural frame with nothing relayable stays dropped.
    const msg = { type: "stream_event", session_id: "s", event: { type: "message_start", message: { id: "m" } } };
    // Act / Assert
    expect(toPersistentEvent(msg)).toBeNull();
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
  const toolStart = (index: number, id: string) => ({
    type: "stream_event",
    session_id: "s",
    uuid: `envelope-tool-${index}`,
    event: { type: "content_block_start", index, content_block: { type: "tool_use", id, name: "Bash", input: {} } },
  });
  const inputChunk = (index: number, partialJson: string) => ({
    type: "stream_event",
    session_id: "s",
    uuid: `envelope-input-${index}`,
    event: { type: "content_block_delta", index, delta: { type: "input_json_delta", partial_json: partialJson } },
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

  it("binds multiple tool blocks by API block index", () => {
    const t = new StreamMessageTracker();
    t.observe(start("msg_tools"));
    t.observe(toolStart(1, "toolu_one"));
    t.observe(toolStart(4, "toolu_four"));

    const first = inputChunk(1, "{\"first\":true}");
    const second = inputChunk(4, "{\"second\":true}");
    expect(t.toolUseIdFor(first)).toBe("toolu_one");
    expect(t.toolUseIdFor(second)).toBe("toolu_four");

    const firstEvent = streamEventToContentDelta(first, { messageId: t.current(), toolUseId: t.toolUseIdFor(first) });
    const secondEvent = streamEventToContentDelta(second, { messageId: t.current(), toolUseId: t.toolUseIdFor(second) });
    if (firstEvent?.payload.case !== "contentDelta" || secondEvent?.payload.case !== "contentDelta") throw new Error("case");
    expect([firstEvent.payload.value.toolUseId, secondEvent.payload.value.toolUseId]).toEqual(["toolu_one", "toolu_four"]);
  });

  it("preserves an identical tool block binding on exact redelivery", () => {
    const t = new StreamMessageTracker();
    t.observe(start("msg_tools"));
    t.observe(toolStart(2, "toolu_two"));
    t.observe(toolStart(2, "toolu_two"));

    expect(t.toolUseIdFor(inputChunk(2, "{}"))).toBe("toolu_two");
  });

  it("does not bind a non-tool content block", () => {
    const t = new StreamMessageTracker();
    t.observe(start("msg_tools"));
    t.observe({
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_start", index: 2, content_block: { type: "text", text: "not a tool" } },
    });

    expect(t.toolUseIdFor(inputChunk(2, "{}"))).toBeUndefined();
  });

  it("ignores a malformed non-object content block start", () => {
    const t = new StreamMessageTracker();
    t.observe(start("msg_tools"));
    t.observe({
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_start", index: 2, content_block: null },
    });

    expect(t.toolUseIdFor(inputChunk(2, "{}"))).toBeUndefined();
  });

  it("rejects a malformed input delta index without selecting another tool", () => {
    const t = new StreamMessageTracker();
    t.observe(start("msg_tools"));
    t.observe(toolStart(2, "toolu_two"));

    expect(t.toolUseIdFor(inputChunk(-1, "{}"))).toBeUndefined();
  });

  it("rejects a conflicting tool block redelivery without changing its binding", () => {
    const write = vi.spyOn(process.stderr, "write").mockImplementation(() => true);
    const t = new StreamMessageTracker();
    t.observe(start("msg_tools"));
    t.observe(toolStart(2, "toolu_two"), "agent-session");

    expect(() => t.observe(toolStart(2, "toolu_conflict"), "agent-session")).toThrow("conflicting_tool_use_id_at_block_index");
    expect(t.toolUseIdFor(inputChunk(2, "{}"))).toBe("toolu_two");
    const records = write.mock.calls.map(([line]) => JSON.parse(String(line)) as { agent_repl_session_id?: string; claude_session_id?: string; context: Record<string, unknown> });
    expect(records).toContainEqual(expect.objectContaining({
      agent_repl_session_id: "test-agent-session",
      claude_session_id: "s",
      context: expect.objectContaining({
        api_message_id: "msg_tools",
        block_index: 2,
        tool_use_id: "toolu_conflict",
        bound_tool_use_id: "toolu_two",
        delta_length: null,
        outcome: "conflicting_tool_use_id_at_block_index",
      }),
    }));
  });

  it("clears tool block bindings at message_stop so a reconnect cannot reuse stale identity", () => {
    const t = new StreamMessageTracker();
    t.observe(start("msg_old"));
    t.observe(toolStart(0, "toolu_old"));
    t.observe(stop());
    t.observe(start("msg_new"));

    expect(t.toolUseIdFor(inputChunk(0, "{}"))).toBeUndefined();
    expect(() => streamEventToContentDelta(inputChunk(0, "{}"), { messageId: t.current(), toolUseId: t.toolUseIdFor(inputChunk(0, "{}")) })).toThrow("input_json delta has no bound tool-use identity");
  });

  it("rejects a tool block start without an API tool-use identity before binding it", () => {
    const write = vi.spyOn(process.stderr, "write").mockImplementation(() => true);
    const t = new StreamMessageTracker();
    t.observe(start("msg_tools"));
    const missingId = { ...toolStart(5, ""), event: { type: "content_block_start", index: 5, content_block: { type: "tool_use", name: "Bash", input: {} } } };

    expect(() => t.observe(missingId, "agent-session")).toThrow("missing_tool_use_id_at_tool_block_start");
    expect(t.toolUseIdFor(inputChunk(5, "{}"))).toBeUndefined();
    const records = write.mock.calls.map(([line]) => JSON.parse(String(line)) as { agent_repl_session_id?: string; claude_session_id?: string; context: Record<string, unknown> });
    expect(records).toContainEqual(expect.objectContaining({
      agent_repl_session_id: "test-agent-session",
      claude_session_id: "s",
      context: expect.objectContaining({
        api_message_id: "msg_tools",
        block_index: 5,
        tool_use_id: null,
        delta_length: null,
        outcome: "missing_tool_use_id_at_tool_block_start",
      }),
    }));
  });

  it("rejects a tool block start before message_start without changing tracker state", () => {
    const t = new StreamMessageTracker();

    expect(() => t.observe(toolStart(5, "toolu_five"), "agent-session")).toThrow("missing_api_message_id_at_tool_block_start");
    expect(t.current()).toBe("");
    expect(t.toolUseIdFor(inputChunk(5, "{}"))).toBeUndefined();
  });

  it("rejects a non-integral API tool block index before binding it", () => {
    const t = new StreamMessageTracker();
    t.observe(start("msg_tools"));
    const invalidIndex = { ...toolStart(0, "toolu_zero"), event: { type: "content_block_start", index: 1.5, content_block: { type: "tool_use", id: "toolu_fractional", name: "Bash", input: {} } } };

    expect(() => t.observe(invalidIndex, "agent-session")).toThrow("invalid_tool_block_index");
    expect(t.toolUseIdFor(inputChunk(0, "{}"))).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// Producer provenance: the query() invocation an event was built inside.
// ---------------------------------------------------------------------------

describe("delta mappers stamp the query they are running", () => {
  it("stamps the running query on a persistent MessageLatency", () => {
    // Arrange: a message_start carrying a ttft stamp, converted inside a query.
    const msg = loadStream("stream_event-message_start");

    // Act.
    const evt = toPersistentEvent(msg, { nowMs: 1000, queryInstanceId: "query-running" });

    // Assert: the envelope names the query that produced it.
    expect(evt).not.toBeNull();
    expect(evt!.queryInstanceId).toBe("query-running");
  });

  it("stamps the running query on an ephemeral ContentDelta", () => {
    // Arrange.
    const msg = loadStream("stream_event-content_block_delta-text");

    // Act.
    const evt = toEphemeralEvent(msg, { nowMs: 1000, queryInstanceId: "query-running" });

    // Assert.
    expect(evt).not.toBeNull();
    expect(evt!.queryInstanceId).toBe("query-running");
  });

  it("leaves the envelope empty when no query is supplied", () => {
    // Arrange: the single-message decode path used by probes has no query.
    const msg = loadStream("stream_event-content_block_delta-text");

    // Act.
    const evt = toEphemeralEvent(msg, { nowMs: 1000 });

    // Assert: empty, which consumers read as live. Never a substituted fact.
    expect(evt).not.toBeNull();
    expect(evt!.queryInstanceId).toBe("");
  });
});
