import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import {
  isEphemeral,
  streamEventToContentDelta,
  toEphemeralEvent,
  toolProgressToHeartbeat,
} from "../src/proto/delta.js";
import { EventClass, Plane } from "../src/uds/proto.js";

function loadStream(name: string): Record<string, unknown> {
  const line = readFileSync(new URL(`../../../testdata/corpus/stream/${name}.jsonl`, import.meta.url), "utf8").split("\n")[0]!;
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

  it("thinking arm carries the uuid consumers reconcile on", () => {
    const d = delta("stream_event-content_block_delta-thinking");
    expect(d.delta.case).toBe("thinking");
    expect(d.uuid).toBe("aa3da566-9d60-4f94-9e99-b9c1f68fc9b4");
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
