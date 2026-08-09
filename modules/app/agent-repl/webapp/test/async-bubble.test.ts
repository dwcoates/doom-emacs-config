/**
 * async-bubble — decode + loud validation of the detached-work surface
 * (agentshim.frontend.v1 async-bubble.proto). One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import {
  decodeAsyncBubble,
  decodeAsyncBubbleDelta,
  decodeAsyncBubbleUpdate,
  decodeLiveness,
  UPDATE_ARM_KIND,
} from "../src/async-bubble.js";

/** A minimal live agent bubble, the shape a newly-opened one arrives in. */
function openedAgent(over: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    id: "b1",
    liveness: { live: {} },
    agent: {},
    ...over,
  };
}

describe("decodeAsyncBubble — identity", () => {
  it("decodes a newly-opened agent bubble with an empty body", () => {
    const bubble = decodeAsyncBubble(openedAgent(), "b");

    // Contract amendment 2 added `AsyncBubble.workspace`, so the whole-shape
    // assertion names it: this test exists to pin EVERY field the decoder
    // produces, and a shape check that omitted the newest one would stop
    // being that.
    expect(bubble).toEqual({
      id: "b1",
      workspace: "",
      originToolUseId: "",
      parentBubbleId: "",
      label: "",
      startedAtMs: 0,
      liveness: { case: "live", value: { lastActivityMs: 0 } },
      kind: { case: "agent", value: { emissions: [], fold: { droppedBefore: 0, tailCap: 0 } } },
    });
  });

  it("rejects an empty id, the routing handle that is never empty", () => {
    expect(() => decodeAsyncBubble(openedAgent({ id: "" }), "b")).toThrow(
      /routing handle is never empty/,
    );
  });

  it("carries the parent pointer that expresses the spawn tree", () => {
    const bubble = decodeAsyncBubble(openedAgent({ parentBubbleId: "b0" }), "b");

    expect(bubble.parentBubbleId).toBe("b0");
  });

  it("carries the workspace a snapshot scopes the bubble by", () => {
    const bubble = decodeAsyncBubble(openedAgent({ workspace: "/ws" }), "b");

    expect(bubble.workspace).toBe("/ws");
  });

  it("carries the originating tool_use id the card attaches by", () => {
    const bubble = decodeAsyncBubble(openedAgent({ originToolUseId: "tu-9" }), "b");

    expect(bubble.originToolUseId).toBe("tu-9");
  });

  it("parses started_at_ms from its int64 JSON string", () => {
    const bubble = decodeAsyncBubble(openedAgent({ startedAtMs: "1700000000000" }), "b");

    expect(bubble.startedAtMs).toBe(1700000000000);
  });

  it("rejects an unrecognized field, the protojson analogue of an unknown one", () => {
    expect(() => decodeAsyncBubble(openedAgent({ nope: 1 }), "b")).toThrow(/unrecognized field/);
  });
});

describe("decodeAsyncBubble — kind", () => {
  it("rejects a bubble that sets no kind arm", () => {
    expect(() => decodeAsyncBubble({ id: "b1", liveness: { live: {} } }, "b")).toThrow(
      /requires exactly one of/,
    );
  });

  it("rejects a bubble that sets two kind arms, never picking one", () => {
    expect(() =>
      decodeAsyncBubble({ id: "b1", liveness: { live: {} }, agent: {}, shell: {} }, "b"),
    ).toThrow(/requires exactly one of/);
  });

  it("decodes a workflow journal's rows with their status arms", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({
        agent: undefined,
        journal: { rows: [{ label: "plan", detail: "ok", done: {} }] },
      }),
      "b",
    );

    expect(bubble.kind).toEqual({
      case: "journal",
      value: { rows: [{ label: "plan", detail: "ok", status: "done" }], fold: { droppedBefore: 0, tailCap: 0 } },
    });
  });

  it("rejects a journal row with no status arm rather than drawing it as running", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: undefined, journal: { rows: [{ label: "x" }] } }), "b"),
    ).toThrow(/requires exactly one of running, done, failed/);
  });

  it("rejects a field added to an empty journal status marker", () => {
    expect(() =>
      decodeAsyncBubble(
        openedAgent({ agent: undefined, journal: { rows: [{ label: "x", running: { pct: 1 } }] } }),
        "b",
      ),
    ).toThrow(/unrecognized field/);
  });

  it("decodes a shell bubble's command line and spool", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: undefined, shell: { command: "make -j8", output: { text: "hi", throughOffset: "2" } } }),
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
      "b",
    );

    expect(bubble.kind.case === "merge" && bubble.kind.value.emissions).toEqual([
      { emission: "response", arm: "assistantMessage", payload: { role: "assistant" } },
    ]);
  });

  it("rejects an unrecognized field on a merge bubble rather than dropping it", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: undefined, merge: { emissions: [], branch: "x" } }), "b"),
    ).toThrow(/unrecognized field/);
  });

  it("rejects a negative spool offset, which no byte count can be", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: undefined, shell: { output: { throughOffset: -1 } } }), "b"),
    ).toThrow(/non-negative safe integer offset/);
  });

  it("decodes an agent bubble's emissions in the feed's own vocabulary", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: { emissions: [{ response: { body: { role: "assistant" } } }] } }),
      "b",
    );

    expect(bubble.kind.case === "agent" && bubble.kind.value.emissions).toEqual([
      { emission: "response", arm: "assistantMessage", payload: { role: "assistant" } },
    ]);
  });

  it("carries a tool call's classification verdict up beside its payload", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: { emissions: [{ toolCall: { call: { id: "tu1" }, spawnedBubbleId: "b2" } }] } }),
      "b",
    );

    expect(bubble.kind.case === "agent" && bubble.kind.value.emissions[0].spawnedBubbleId).toBe("b2");
  });

  it("rejects an unrecognized emission arm inside a detached agent's fold", () => {
    expect(() => decodeAsyncBubble(openedAgent({ agent: { emissions: [{ nope: {} }] } }), "b")).toThrow(
      /unrecognized emission 'nope'/,
    );
  });

  it("rejects an emission that sets no arm at all", () => {
    expect(() => decodeAsyncBubble(openedAgent({ agent: { emissions: [{}] } }), "b")).toThrow(
      /carries no emission \(empty oneof\)/,
    );
  });

  it("rejects an emission that sets two arms, never picking one", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: { emissions: [{ response: { body: {} }, thinking: { body: {} } }] } }), "b"),
    ).toThrow(/sets multiple emissions/);
  });

  it("names the emission's own position, so a bad one deep in a fold is findable", () => {
    expect(() =>
      decodeAsyncBubble(openedAgent({ agent: { emissions: [{ response: { body: {} } }, { nope: {} }] } }), "b"),
    ).toThrow(/b\.agent\.emissions\[1\]/);
  });
});

describe("decodeAsyncBubble — fold", () => {
  it("reports the dropped count that drives the earlier-entries notice", () => {
    const bubble = decodeAsyncBubble(
      openedAgent({ agent: { fold: { droppedBefore: "12", tailCap: 200 } } }),
      "b",
    );

    expect(bubble.kind.case === "agent" && bubble.kind.value.fold).toEqual({ droppedBefore: 12, tailCap: 200 });
  });

  it("rejects a negative dropped count", () => {
    expect(() => decodeAsyncBubble(openedAgent({ agent: { fold: { droppedBefore: -1 } } }), "b")).toThrow(
      /droppedBefore must not be negative/,
    );
  });

  it("rejects a negative tail cap", () => {
    expect(() => decodeAsyncBubble(openedAgent({ agent: { fold: { tailCap: -5 } } }), "b")).toThrow(
      /tailCap must not be negative/,
    );
  });
});

describe("decodeLiveness", () => {
  it("rejects a bubble with no liveness block at all", () => {
    expect(() => decodeAsyncBubble({ id: "b1", agent: {} }, "b")).toThrow(
      /liveness is absent — a bubble is always live or settled/,
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
  it("routes by the bubble id and types by the arm", () => {
    const update = decodeAsyncBubbleUpdate({ bubbleId: "b1", shell: { text: "x", fromOffset: "4" } }, "u");

    expect(update).toEqual({ bubbleId: "b1", update: { case: "shell", value: { text: "x", fromOffset: 4 } } });
  });

  it("rejects an update with an empty bubble id, which is unroutable", () => {
    expect(() => decodeAsyncBubbleUpdate({ bubbleId: "", liveness: { liveness: { live: {} } } }, "u")).toThrow(
      /unroutable/,
    );
  });

  it("rejects an update that sets no arm", () => {
    expect(() => decodeAsyncBubbleUpdate({ bubbleId: "b1" }, "u")).toThrow(/requires exactly one of/);
  });

  it("rejects an update that sets two arms", () => {
    expect(() =>
      decodeAsyncBubbleUpdate({ bubbleId: "b1", shell: { fromOffset: "0" }, unclassified: { fromOffset: "0" } }, "u"),
    ).toThrow(/requires exactly one of/);
  });

  it("decodes an agent update's restated fold accounting", () => {
    const update = decodeAsyncBubbleUpdate(
      { bubbleId: "b1", agent: { emissions: [], fold: { droppedBefore: "3", tailCap: 50 } } },
      "u",
    );

    expect(update.update).toEqual({
      case: "agent",
      value: { emissions: [], fold: { droppedBefore: 3, tailCap: 50 } },
    });
  });

  it("decodes a liveness transition to settled", () => {
    const update = decodeAsyncBubbleUpdate(
      { bubbleId: "b1", liveness: { liveness: { settled: { settledAtMs: "9", done: {} } } } },
      "u",
    );

    expect(update.update).toEqual({
      case: "liveness",
      value: { case: "settled", value: { settledAtMs: 9, outcome: { case: "done" } } },
    });
  });

  it("keeps shell and unclassified as distinct arms carrying the same payload", () => {
    const update = decodeAsyncBubbleUpdate({ bubbleId: "b1", unclassified: { text: "y", fromOffset: "0" } }, "u");

    expect(update.update.case).toBe("unclassified");
  });
});

describe("decodeAsyncBubbleDelta", () => {
  it("decodes a push of opened bubbles and updates", () => {
    const delta = decodeAsyncBubbleDelta({
      workspace: "/w",
      opened: [openedAgent()],
      updates: [{ bubbleId: "b1", liveness: { liveness: { live: { lastActivityMs: "7" } } } }],
      throughSeq: "42",
      fence: "f1",
    });

    expect({ opened: delta.opened.length, updates: delta.updates.length, throughSeq: delta.throughSeq }).toEqual({
      opened: 1,
      updates: 1,
      throughSeq: 42,
    });
  });

  it("rejects a push with no fence, which could not be gated at all", () => {
    expect(() => decodeAsyncBubbleDelta({ workspace: "/w" })).toThrow(/missing required `fence`/);
  });

  it("rejects an unrecognized field on the push envelope", () => {
    expect(() => decodeAsyncBubbleDelta({ workspace: "/w", fence: "f", sessionId: "s" })).toThrow(
      /unrecognized field/,
    );
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
