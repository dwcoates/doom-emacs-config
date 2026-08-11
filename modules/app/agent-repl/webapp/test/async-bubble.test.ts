/**
 * async-bubble — decode + loud validation of the detached-work surface
 * (agentshim.frontend.v1 detached-work.proto). One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import {
  decodeAsyncBubble,
  decodeAsyncBubbleUpdate,
  decodeLiveness,
  UPDATE_ARM_KIND,
  type DetachedWorkPackaging,
} from "../src/async-bubble.js";

/**
 * The message envelope's packaging, which the payload no longer carries: a
 * feed-row message, so its top-level id is its own uuid and its parent is empty.
 */
function pkg(over: Partial<DetachedWorkPackaging> = {}): DetachedWorkPackaging {
  return { id: "b1", parentMessageId: "", topLevelMessageId: "b1", ...over };
}

/** A minimal live detached agent PAYLOAD, as a newly-opened one arrives. */
function openedAgent(over: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    liveness: { live: {} },
    agent: {},
    ...over,
  };
}

describe("decodeAsyncBubble — identity", () => {
  it("decodes a newly-opened agent bubble with an empty body", () => {
    const bubble = decodeAsyncBubble(openedAgent(), pkg(), "b");

    // The whole-shape assertion pins EVERY field the decoder produces,
    // including the three the MESSAGE ENVELOPE now supplies: a shape check that
    // omitted them would stop proving the lift happened at all.
    expect(bubble).toEqual({
      id: "b1",
      workspace: "",
      originToolUseId: "",
      parentMessageId: "",
      topLevelMessageId: "b1",
      label: "",
      startedAtMs: 0,
      liveness: { case: "live", value: { lastActivityMs: 0 } },
      kind: { case: "agent", value: { emissions: [], fold: { droppedBefore: 0, tailCap: 0 } } },
    });
  });

  it("rejects an empty message id, the routing handle that is never empty", () => {
    expect(() => decodeAsyncBubble(openedAgent(), pkg({ id: "" }), "b")).toThrow(
      /routing handle is its message's uuid, which is never empty/,
    );
  });

  it("rejects an empty topLevelMessageId rather than deriving the root", () => {
    expect(() => decodeAsyncBubble(openedAgent(), pkg({ topLevelMessageId: "" }), "b")).toThrow(
      /top-level id is never empty/,
    );
  });

  it("lifts the envelope's parent_message_id as the one containment relation", () => {
    const bubble = decodeAsyncBubble(
      openedAgent(),
      pkg({ parentMessageId: "b0", topLevelMessageId: "root" }),
      "b",
    );

    expect(bubble.parentMessageId).toBe("b0");
  });

  it("lifts the envelope's denormalized top_level_message_id verbatim", () => {
    const bubble = decodeAsyncBubble(
      openedAgent(),
      pkg({ parentMessageId: "b0", topLevelMessageId: "root" }),
      "b",
    );

    expect(bubble.topLevelMessageId).toBe("root");
  });

  it("rejects an `id` on the payload, a wire name retired with the second id space", () => {
    expect(() => decodeAsyncBubble(openedAgent({ id: "b9" }), pkg(), "b")).toThrow(
      /unrecognized field/,
    );
  });

  it("rejects a `parentBubbleId` on the payload, retired with the second tree", () => {
    expect(() => decodeAsyncBubble(openedAgent({ parentBubbleId: "b0" }), pkg(), "b")).toThrow(
      /unrecognized field/,
    );
  });

  it("carries the workspace a snapshot scopes the bubble by", () => {
    const bubble = decodeAsyncBubble(openedAgent({ workspace: "/ws" }), pkg(), "b");

    expect(bubble.workspace).toBe("/ws");
  });

  it("carries the originating tool_use id the card attaches by", () => {
    const bubble = decodeAsyncBubble(openedAgent({ originToolUseId: "tu-9" }), pkg(), "b");

    expect(bubble.originToolUseId).toBe("tu-9");
  });

  it("parses started_at_ms from its int64 JSON string", () => {
    const bubble = decodeAsyncBubble(openedAgent({ startedAtMs: "1700000000000" }), pkg(), "b");

    expect(bubble.startedAtMs).toBe(1700000000000);
  });

  it("rejects an unrecognized field, the protojson analogue of an unknown one", () => {
    expect(() => decodeAsyncBubble(openedAgent({ nope: 1 }), pkg(), "b")).toThrow(/unrecognized field/);
  });
});

describe("decodeAsyncBubble — kind", () => {
  it("rejects a bubble that sets no kind arm", () => {
    expect(() => decodeAsyncBubble({ liveness: { live: {} } }, pkg(), "b")).toThrow(
      /requires exactly one of/,
    );
  });

  it("rejects a bubble that sets two kind arms, never picking one", () => {
    expect(() =>
      decodeAsyncBubble({ liveness: { live: {} }, agent: {}, shell: {} }, pkg(), "b"),
    ).toThrow(/requires exactly one of/);
  });

  it("decodes a workflow journal's rows with their status arms", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({
        agent: undefined,
        journal: { rows: [{ label: "plan", detail: "ok", done: {} }] },
      }),
      pkg(),
      "b",
    );

    expect(bubble.kind).toEqual({
      case: "journal",
      value: { rows: [{ label: "plan", detail: "ok", status: "done" }], fold: { droppedBefore: 0, tailCap: 0 } },
    });
  });

  it("rejects a journal row with no status arm rather than drawing it as running", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: undefined, journal: { rows: [{ label: "x" }] } }), pkg(), "b"),
    ).toThrow(/requires exactly one of running, done, failed/);
  });

  it("rejects a field added to an empty journal status marker", () => {
    expect(() =>
      decodeAsyncBubble(
        openedAgent({ agent: undefined, journal: { rows: [{ label: "x", running: { pct: 1 } }] } }),
        pkg(),
        "b",
      ),
    ).toThrow(/unrecognized field/);
  });

  it("decodes a shell bubble's command line and spool", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: undefined, shell: { command: "make -j8", output: { text: "hi", throughOffset: "2" } } }),
      pkg(),
      "b",
    );

    expect(bubble.kind).toEqual({
      case: "shell",
      value: { command: "make -j8", output: { text: "hi", throughOffset: 2 } },
    });
  });

  it("names the unrecognized tool rather than guessing the work into another kind", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: undefined, unclassified: { toolName: "Frobnicate" } }),
      pkg(),
      "b",
    );

    expect(bubble.kind).toEqual({
      case: "unclassified",
      value: { toolName: "Frobnicate", output: { text: "", throughOffset: 0 } },
    });
  });

  it("decodes a merge bubble's emissions in the feed's own vocabulary", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: undefined, merge: { emissions: [{ response: { body: { role: "assistant" } } }] } }),
      pkg(),
      "b",
    );

    expect(bubble.kind.case === "merge" && bubble.kind.value.emissions).toEqual([
      { emission: "response", arm: "assistantMessage", payload: { role: "assistant" } },
    ]);
  });

  it("rejects an unrecognized field on a merge bubble rather than dropping it", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: undefined, merge: { emissions: [], branch: "x" } }), pkg(), "b"),
    ).toThrow(/unrecognized field/);
  });

  it("rejects a negative spool offset, which no byte count can be", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: undefined, shell: { output: { throughOffset: -1 } } }), pkg(), "b"),
    ).toThrow(/non-negative safe integer offset/);
  });

  it("decodes an agent bubble's emissions in the feed's own vocabulary", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: { emissions: [{ response: { body: { role: "assistant" } } }] } }),
      pkg(),
      "b",
    );

    expect(bubble.kind.case === "agent" && bubble.kind.value.emissions).toEqual([
      { emission: "response", arm: "assistantMessage", payload: { role: "assistant" } },
    ]);
  });

  it("carries a tool call's classification verdict up beside its payload", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: { emissions: [{ toolCall: { call: { id: "tu1" }, spawnedMessageId: "b2" } }] } }),
      pkg(),
      "b",
    );

    expect(bubble.kind.case === "agent" && bubble.kind.value.emissions[0].spawnedMessageId).toBe("b2");
  });

  it("rejects an unrecognized emission arm inside a detached agent's fold", () => {
    expect(() => decodeAsyncBubble(openedAgent({ agent: { emissions: [{ nope: {} }] } }), pkg(), "b")).toThrow(
      /unrecognized emission 'nope'/,
    );
  });

  it("rejects an emission that sets no arm at all", () => {
    expect(() => decodeAsyncBubble(openedAgent({ agent: { emissions: [{}] } }), pkg(), "b")).toThrow(
      /carries no emission \(empty oneof\)/,
    );
  });

  it("rejects an emission that sets two arms, never picking one", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: { emissions: [{ response: { body: {} }, thinking: { body: {} } }] } }), pkg(), "b"),
    ).toThrow(/sets multiple emissions/);
  });

  it("names the emission's own position, so a bad one deep in a fold is findable", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: { emissions: [{ response: { body: {} } }, { nope: {} }] } }), pkg(), "b"),
    ).toThrow(/b\.agent\.emissions\[1\]/);
  });
});

describe("decodeAsyncBubble — fold", () => {
  it("reports the dropped count that drives the earlier-entries notice", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: { fold: { droppedBefore: "12", tailCap: 200 } } }),
      pkg(),
      "b",
    );

    expect(bubble.kind.case === "agent" && bubble.kind.value.fold).toEqual({ droppedBefore: 12, tailCap: 200 });
  });

  it("rejects a negative dropped count", () => {
    expect(() => decodeAsyncBubble(openedAgent({ agent: { fold: { droppedBefore: -1 } } }), pkg(), "b")).toThrow(
      /droppedBefore must not be negative/,
    );
  });

  it("rejects a negative tail cap", () => {
    expect(() => decodeAsyncBubble(openedAgent({ agent: { fold: { tailCap: -5 } } }), pkg(), "b")).toThrow(
      /tailCap must not be negative/,
    );
  });
});

describe("decodeLiveness", () => {
  it("rejects a bubble with no liveness block at all", () => {
    expect(() => decodeAsyncBubble({ agent: {} }, pkg(), "b")).toThrow(
      /liveness is absent — detached work is always live or settled/,
    );
  });

  it("reads a live bubble's last-activity stamp", () => {
    expect(decodeLiveness({ live: { lastActivityMs: "1700000000000" } }, "l")).toEqual({
      case: "live",
      value: { lastActivityMs: 1700000000000 },
    });
  });

  it("rejects a liveness that sets both live and settled", () => {
    expect(() => decodeLiveness({ live: {}, settled: { done: {} } }, "l")).toThrow(
      /requires exactly one of live, settled/,
    );
  });

  it("rejects a settled bubble with no outcome, which is unrepresentable", () => {
    expect(() => decodeLiveness({ settled: { settledAtMs: "1" } }, "l")).toThrow(
      /requires exactly one of done, error, killed/,
    );
  });

  it("keeps a shell's exit status beside its outcome so a card can show 'exited 137'", () => {
    expect(decodeLiveness({ settled: { settledAtMs: "5", shellExit: { code: 137 }, error: {} } }, "l")).toEqual({
      case: "settled",
      value: { settledAtMs: 5, shellExit: { code: 137 }, outcome: { case: "error", message: "" } },
    });
  });

  it("leaves shell_exit ABSENT for work that concluded rather than exited", () => {
    const liveness = decodeLiveness({ settled: { settledAtMs: "5", done: {} } }, "l");

    expect(liveness.case === "settled" && "shellExit" in liveness.value).toBe(false);
  });

  it("carries a killed bubble's attributed reason", () => {
    expect(decodeLiveness({ settled: { settledAtMs: "5", killed: { reason: "session teardown" } } }, "l")).toEqual({
      case: "settled",
      value: { settledAtMs: 5, outcome: { case: "killed", reason: "session teardown" } },
    });
  });

  it("rejects a field added to the empty done outcome", () => {
    expect(() => decodeLiveness({ settled: { done: { code: 0 } } }, "l")).toThrow(/unrecognized field/);
  });
});

describe("decodeAsyncBubbleUpdate", () => {
  it("routes by the message id and types by the arm", () => {
    const update = decodeAsyncBubbleUpdate({ messageId: "b1", shell: { text: "x", fromOffset: "4" } }, "u");

    expect(update).toEqual({ messageId: "b1", update: { case: "shell", value: { text: "x", fromOffset: 4 } } });
  });

  it("rejects an update with an empty message id, which is unroutable", () => {
    expect(() => decodeAsyncBubbleUpdate({ messageId: "", liveness: { liveness: { live: {} } } }, "u")).toThrow(
      /unroutable/,
    );
  });

  it("rejects an update that sets no arm", () => {
    expect(() => decodeAsyncBubbleUpdate({ messageId: "b1" }, "u")).toThrow(/requires exactly one of/);
  });

  it("rejects an update that sets two arms", () => {
    expect(() =>
      decodeAsyncBubbleUpdate({ messageId: "b1", shell: { fromOffset: "0" }, unclassified: { fromOffset: "0" } }, "u"),
    ).toThrow(/requires exactly one of/);
  });

  it("decodes an agent update's restated fold accounting", () => {
    const update = decodeAsyncBubbleUpdate(
      { messageId: "b1", agent: { emissions: [], fold: { droppedBefore: "3", tailCap: 50 } } },
      "u",
    );

    expect(update.update).toEqual({
      case: "agent",
      value: { emissions: [], fold: { droppedBefore: 3, tailCap: 50 } },
    });
  });

  it("decodes a liveness transition to settled", () => {
    const update = decodeAsyncBubbleUpdate(
      { messageId: "b1", liveness: { liveness: { settled: { settledAtMs: "9", done: {} } } } },
      "u",
    );

    expect(update.update).toEqual({
      case: "liveness",
      value: { case: "settled", value: { settledAtMs: 9, outcome: { case: "done" } } },
    });
  });

  it("keeps shell and unclassified as distinct arms carrying the same payload", () => {
    const update = decodeAsyncBubbleUpdate({ messageId: "b1", unclassified: { text: "y", fromOffset: "0" } }, "u");

    expect(update.update.case).toBe("unclassified");
  });
});

describe("UPDATE_ARM_KIND", () => {
  it("maps every kind-specific arm to its own kind and to nothing else", () => {
    expect(UPDATE_ARM_KIND).toEqual({
      agent: "agent",
      journal: "journal",
      shell: "shell",
      unclassified: "unclassified",
      merge: "merge",
      skill: "skill",
    });
  });
});
