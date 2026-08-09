/**
 * async-routing — ID-ONLY routing (I2) and spool continuity (I4).
 *
 * Every violation path asserts BOTH halves of the guarantee: the loud rejection
 * AND that the registry was left exactly as it was. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { AsyncBubbleRegistry } from "../src/async-routing.js";
import type {
  AsyncBubble,
  AsyncBubbleDelta,
  AsyncBubbleUpdate,
  AsyncLiveness,
} from "../src/async-bubble.js";

const LIVE: AsyncLiveness = { case: "live", value: { lastActivityMs: 0 } };
const NO_FOLD = { droppedBefore: 0, tailCap: 0 };

function bubble(over: Partial<AsyncBubble> & Pick<AsyncBubble, "id" | "kind">): AsyncBubble {
  return {
    originToolUseId: "",
    parentBubbleId: "",
    label: "",
    startedAtMs: 0,
    liveness: LIVE,
    ...over,
  };
}

function agentBubble(id: string, over: Partial<AsyncBubble> = {}): AsyncBubble {
  return bubble({ id, kind: { case: "agent", value: { emissions: [], fold: NO_FOLD } }, ...over });
}

function shellBubble(id: string, text = "", throughOffset = 0): AsyncBubble {
  return bubble({
    id,
    kind: { case: "shell", value: { command: "make", output: { text, throughOffset } } },
  });
}

function unclassifiedBubble(id: string, text = "", throughOffset = 0): AsyncBubble {
  return bubble({
    id,
    kind: { case: "unclassified", value: { toolName: "Frobnicate", output: { text, throughOffset } } },
  });
}

function journalBubble(id: string): AsyncBubble {
  return bubble({ id, kind: { case: "journal", value: { rows: [], fold: NO_FOLD } } });
}

function push(opened: AsyncBubble[], updates: AsyncBubbleUpdate[] = []): AsyncBubbleDelta {
  return { workspace: "/w", opened, updates, throughSeq: 1, fence: "f1" };
}

/** A registry seeded through the normal open path, as a live one would be. */
function seeded(...bubbles: AsyncBubble[]): AsyncBubbleRegistry {
  const registry = new AsyncBubbleRegistry();
  registry.applyDelta(push(bubbles));
  return registry;
}

describe("opening bubbles", () => {
  it("opens a bubble under its own id", () => {
    const registry = new AsyncBubbleRegistry();

    const result = registry.applyDelta(push([agentBubble("b1")]));

    expect([result, registry.get("b1")?.id]).toEqual([{ ok: true, opened: 1, updated: 0 }, "b1"]);
  });

  it("REPLACES a bubble re-delivered in full rather than opening a second one", () => {
    const registry = seeded(shellBubble("b1", "old", 3));

    registry.applyDelta(push([shellBubble("b1", "fresh", 5)]));

    expect([registry.size, registry.get("b1")?.kind]).toEqual([
      1,
      { case: "shell", value: { command: "make", output: { text: "fresh", throughOffset: 5 } } },
    ]);
  });

  it("returns null for a bubble that is not open", () => {
    expect(new AsyncBubbleRegistry().get("nope")).toBeNull();
  });
});

describe("I2 — unknown bubble id", () => {
  it("rejects an update naming a bubble that is not open", () => {
    const registry = seeded(agentBubble("b1"));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "ghost", update: { case: "liveness", value: LIVE } }]),
    );

    expect(result.ok === false && result.gap.kind).toBe("unknown-bubble");
  });

  it("names the unroutable id on the gap so the rejection is diagnosable", () => {
    const registry = seeded(agentBubble("b1"));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "ghost", update: { case: "liveness", value: LIVE } }]),
    );

    expect(result.ok === false && result.gap.bubbleId).toBe("ghost");
  });

  it("does NOT buffer the update in the hope its bubble shows up", () => {
    const registry = seeded(agentBubble("b1"));
    registry.applyDelta(push([], [{ bubbleId: "ghost", update: { case: "liveness", value: LIVE } }]));

    // The bubble opens AFTERWARDS; a buffered update would land on it here.
    registry.applyDelta(push([agentBubble("ghost")]));

    expect(registry.get("ghost")?.liveness).toEqual(LIVE);
  });

  it("leaves the rest of the push unapplied — no partial mutation", () => {
    const registry = seeded(agentBubble("b1"));

    registry.applyDelta(
      push(
        [],
        [
          {
            bubbleId: "b1",
            update: { case: "agent", value: { emissions: [], fold: { droppedBefore: 9, tailCap: 9 } } },
          },
          { bubbleId: "ghost", update: { case: "liveness", value: LIVE } },
        ],
      ),
    );

    expect(registry.get("b1")?.kind).toEqual({ case: "agent", value: { emissions: [], fold: NO_FOLD } });
  });

  it("does not open the push's bubbles when a later update gaps", () => {
    const registry = new AsyncBubbleRegistry();

    registry.applyDelta(
      push([agentBubble("b1")], [{ bubbleId: "ghost", update: { case: "liveness", value: LIVE } }]),
    );

    expect(registry.size).toBe(0);
  });
});

describe("I2 — kind mismatch", () => {
  it("rejects a journal update addressed to a shell bubble", () => {
    const registry = seeded(shellBubble("b1"));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "journal", value: { rows: [], fold: NO_FOLD } } }]),
    );

    expect(result.ok === false && result.gap.kind).toBe("kind-mismatch");
  });

  it("reports the kind the bubble actually is", () => {
    const registry = seeded(shellBubble("b1"));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "journal", value: { rows: [], fold: NO_FOLD } } }]),
    );

    expect(result.ok === false && result.gap.bubbleKind).toBe("shell");
  });

  it("does not coerce the bubble into the arm's kind", () => {
    const registry = seeded(shellBubble("b1", "out", 3));

    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "journal", value: { rows: [], fold: NO_FOLD } } }]),
    );

    expect(registry.get("b1")?.kind.case).toBe("shell");
  });

  it("rejects a shell append addressed to an unclassified bubble, distinct arms for distinct kinds", () => {
    const registry = seeded(unclassifiedBubble("b1"));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "shell", value: { text: "x", fromOffset: 0 } } }]),
    );

    expect(result.ok === false && result.gap.kind).toBe("kind-mismatch");
  });

  it("rejects an agent update addressed to a journal bubble", () => {
    const registry = seeded(journalBubble("b1"));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "agent", value: { emissions: [], fold: NO_FOLD } } }]),
    );

    expect(result.ok === false && result.gap.kind).toBe("kind-mismatch");
  });
});

describe("I4 — spool continuity", () => {
  it("applies an append whose from_offset equals the spool's through_offset", () => {
    const registry = seeded(shellBubble("b1", "ab", 2));

    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "shell", value: { text: "cd", fromOffset: 2 } } }]),
    );

    expect(registry.get("b1")?.kind).toEqual({
      case: "shell",
      value: { command: "make", output: { text: "abcd", throughOffset: 4 } },
    });
  });

  it("rejects an append that starts BEYOND the spool's cursor", () => {
    const registry = seeded(shellBubble("b1", "ab", 2));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "shell", value: { text: "cd", fromOffset: 7 } } }]),
    );

    expect(result.ok === false && result.gap.kind).toBe("offset-gap");
  });

  it("rejects an append that starts BEHIND the spool's cursor", () => {
    const registry = seeded(shellBubble("b1", "ab", 2));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "shell", value: { text: "x", fromOffset: 0 } } }]),
    );

    expect(result.ok === false && result.gap.kind).toBe("offset-gap");
  });

  it("carries both offsets on the gap so the size of the hole is knowable", () => {
    const registry = seeded(shellBubble("b1", "ab", 2));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "shell", value: { text: "cd", fromOffset: 7 } } }]),
    );

    expect(result.ok === false && [result.gap.throughOffset, result.gap.fromOffset]).toEqual([2, 7]);
  });

  it("does not apply the bytes of a gapped append", () => {
    const registry = seeded(shellBubble("b1", "ab", 2));

    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "shell", value: { text: "cd", fromOffset: 7 } } }]),
    );

    expect(registry.get("b1")?.kind).toEqual({
      case: "shell",
      value: { command: "make", output: { text: "ab", throughOffset: 2 } },
    });
  });

  it("does not seek into the text to make a gapped append fit", () => {
    const registry = seeded(shellBubble("b1", "abcdef", 6));

    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "shell", value: { text: "XX", fromOffset: 3 } } }]),
    );

    expect(registry.get("b1")?.kind.case === "shell" && registry.get("b1")?.kind.value).toEqual({
      command: "make",
      output: { text: "abcdef", throughOffset: 6 },
    });
  });

  it("advances the cursor by BYTES, not by UTF-16 code units", () => {
    const registry = seeded(shellBubble("b1"));

    // "é" is two bytes in UTF-8 and one code unit in JavaScript.
    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "shell", value: { text: "é", fromOffset: 0 } } }]),
    );

    expect(registry.get("b1")?.kind.case === "shell" && registry.get("b1")?.kind.value.output.throughOffset).toBe(2);
  });

  it("applies an unclassified append on the same continuity rule", () => {
    const registry = seeded(unclassifiedBubble("b1", "a", 1));

    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "unclassified", value: { text: "b", fromOffset: 1 } } }]),
    );

    expect(registry.get("b1")?.kind.case === "unclassified" && registry.get("b1")?.kind.value.output).toEqual({
      text: "ab",
      throughOffset: 2,
    });
  });

  it("rejects a gapped unclassified append on the same continuity rule", () => {
    const registry = seeded(unclassifiedBubble("b1", "a", 1));

    const result = registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "unclassified", value: { text: "b", fromOffset: 4 } } }]),
    );

    expect(result.ok === false && result.gap.kind).toBe("offset-gap");
  });
});

describe("applying matched updates", () => {
  it("appends an agent's emissions to its fold", () => {
    const registry = seeded(agentBubble("b1"));

    registry.applyDelta(
      push(
        [],
        [
          {
            bubbleId: "b1",
            update: {
              case: "agent",
              value: {
                emissions: [{ emission: "response", arm: "assistantMessage", payload: { role: "assistant" } }],
                fold: NO_FOLD,
              },
            },
          },
        ],
      ),
    );

    expect(registry.get("b1")?.kind.case === "agent" && registry.get("b1")?.kind.value.emissions).toHaveLength(1);
  });

  it("RESTATES the fold accounting rather than accumulating it", () => {
    const registry = seeded(agentBubble("b1"));

    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "agent", value: { emissions: [], fold: { droppedBefore: 4, tailCap: 20 } } } }]),
    );
    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "agent", value: { emissions: [], fold: { droppedBefore: 7, tailCap: 20 } } } }]),
    );

    expect(registry.get("b1")?.kind.case === "agent" && registry.get("b1")?.kind.value.fold.droppedBefore).toBe(7);
  });

  it("appends journal rows without rewriting the ones already logged", () => {
    const registry = seeded(journalBubble("b1"));

    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "journal", value: { rows: [{ label: "s", detail: "", status: "running" }], fold: NO_FOLD } } }]),
    );
    registry.applyDelta(
      push([], [{ bubbleId: "b1", update: { case: "journal", value: { rows: [{ label: "s", detail: "ok", status: "done" }], fold: NO_FOLD } } }]),
    );

    expect(registry.get("b1")?.kind.case === "journal" && registry.get("b1")?.kind.value.rows.map((r) => r.status)).toEqual([
      "running",
      "done",
    ]);
  });

  it("lands a liveness transition on a bubble of ANY kind", () => {
    const registry = seeded(shellBubble("b1"));
    const settled: AsyncLiveness = {
      case: "settled",
      value: { settledAtMs: 9, shellExit: { code: 137 }, outcome: { case: "killed", reason: "stopped" } },
    };

    registry.applyDelta(push([], [{ bubbleId: "b1", update: { case: "liveness", value: settled } }]));

    expect(registry.get("b1")?.liveness).toEqual(settled);
  });

  it("counts what a successful push did", () => {
    const registry = seeded(agentBubble("b1"));

    const result = registry.applyDelta(
      push([agentBubble("b2")], [{ bubbleId: "b1", update: { case: "liveness", value: LIVE } }]),
    );

    expect(result).toEqual({ ok: true, opened: 1, updated: 1 });
  });
});

describe("snapshot adoption", () => {
  it("REPLACES the held set rather than merging with it", () => {
    const registry = seeded(agentBubble("b1"), agentBubble("b2"));

    registry.adoptSnapshot([agentBubble("b2")]);

    expect(registry.all().map((b) => b.id)).toEqual(["b2"]);
  });

  it("clears every bubble when the snapshot names none", () => {
    const registry = seeded(agentBubble("b1"));

    registry.adoptSnapshot([]);

    expect(registry.size).toBe(0);
  });
});

describe("tool-card attachment", () => {
  it("matches a card's classification verdict to its bubble", () => {
    const registry = seeded(agentBubble("b1"));

    expect(registry.bubbleForSpawn("b1")?.id).toBe("b1");
  });

  it("reads an EMPTY verdict as 'detached nothing' and never goes looking", () => {
    const registry = seeded(agentBubble("b1"));

    expect(registry.bubbleForSpawn("")).toBeNull();
  });

  it("returns null for a verdict naming a bubble not yet open, deriving nothing", () => {
    const registry = seeded(agentBubble("b1"));

    expect(registry.bubbleForSpawn("b9")).toBeNull();
  });
});

describe("the spawn tree", () => {
  it("lists bubbles with no parent pointer as roots", () => {
    const registry = seeded(agentBubble("b1"), agentBubble("b2", { parentBubbleId: "b1" }));

    expect(registry.roots().map((b) => b.id)).toEqual(["b1"]);
  });

  it("resolves children by parent POINTER, one lookup deep", () => {
    const registry = seeded(
      agentBubble("b1"),
      agentBubble("b2", { parentBubbleId: "b1" }),
      agentBubble("b3", { parentBubbleId: "b1" }),
    );

    expect(registry.children("b1").map((b) => b.id)).toEqual(["b2", "b3"]);
  });

  it("resolves a grandchild without recursing into any payload", () => {
    const registry = seeded(
      agentBubble("b1"),
      agentBubble("b2", { parentBubbleId: "b1" }),
      agentBubble("b3", { parentBubbleId: "b2" }),
    );

    expect(registry.children("b2").map((b) => b.id)).toEqual(["b3"]);
  });

  it("reports a bubble whose parent pointer resolves to nothing as an orphan", () => {
    const registry = seeded(agentBubble("b2", { parentBubbleId: "gone" }));

    expect(registry.orphans()).toEqual([
      { bubble: registry.get("b2"), missingParentId: "gone" },
    ]);
  });

  it("does not promote an orphan to a root", () => {
    const registry = seeded(agentBubble("b2", { parentBubbleId: "gone" }));

    expect(registry.roots()).toEqual([]);
  });
});
