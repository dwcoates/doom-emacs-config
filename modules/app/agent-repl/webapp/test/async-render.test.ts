/**
 * async-render — drawing an AsyncBubble as what it is, per kind, with its
 * liveness, its settled outcome, its earlier-entries notice and its spawn
 * tree. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import {
  AsyncBubbleCard,
  AsyncBubbleForCall,
  AsyncBubbleForest,
  bubbleFoldId,
  bubbleLabel,
  earlierEntriesNotice,
  type AsyncRenderContext,
} from "../src/async-render.js";
import { AsyncBubbleRegistry } from "../src/async-routing.js";
import type { AsyncBubble, AsyncBubbleDelta, AsyncLiveness } from "../src/async-bubble.js";

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

function push(opened: AsyncBubble[]): AsyncBubbleDelta {
  return { workspace: "/w", opened, updates: [], throughSeq: 1, fence: "f1" };
}

/**
 * A context whose emission renderer is a STUB naming what it was handed.
 *
 * The production wiring injects the feed's own `renderItem` path; this stub
 * asserts the WIRING — that the agent arm reaches the injected renderer with
 * the bubble's emissions — without re-testing the feed's renderers here.
 */
function ctxFor(registry: AsyncBubbleRegistry, open: readonly string[] = []): AsyncRenderContext {
  return {
    registry,
    isOpen: (id) => open.includes(id),
    renderEmissions: (emissions, bubbleId) =>
      `<div class="stub-emissions" data-bubble="${bubbleId}">${emissions.length}</div>`,
  };
}

function seeded(...bubbles: AsyncBubble[]): AsyncBubbleRegistry {
  const registry = new AsyncBubbleRegistry();
  registry.applyDelta(push(bubbles));
  return registry;
}

const agentKind: AsyncBubble["kind"] = { case: "agent", value: { emissions: [], fold: NO_FOLD } };

describe("bubbleLabel", () => {
  it("uses the daemon's label as the collapsed face", () => {
    expect(bubbleLabel(bubble({ id: "b1", kind: agentKind, label: "migrate call sites" }))).toBe(
      "migrate call sites",
    );
  });

  it("shows the id when the daemon had no label for the work", () => {
    expect(bubbleLabel(bubble({ id: "b1", kind: agentKind }))).toBe("b1");
  });
});

describe("liveness on the collapsed face", () => {
  it("reads running while the work is live", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("running");
  });

  it("spins the live arc only while the work is live", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("tool-spinner");
  });

  it("drops the arc once the work settles", () => {
    const settled: AsyncLiveness = { case: "settled", value: { settledAtMs: 1, outcome: { case: "done" } } };
    const registry = seeded(bubble({ id: "b1", kind: agentKind, liveness: settled }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).not.toContain("tool-spinner");
  });

  it("shows a shell's exit code beside its verdict, not instead of it", () => {
    const settled: AsyncLiveness = {
      case: "settled",
      value: { settledAtMs: 1, shellExit: { code: 137 }, outcome: { case: "killed", reason: "" } },
    };
    const registry = seeded(
      bubble({
        id: "b1",
        kind: { case: "shell", value: { command: "make", output: { text: "", throughOffset: 0 } } },
        liveness: settled,
      }),
    );

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("stopped · exited 137");
  });

  it("shows exited 0 as a real zero rather than omitting it", () => {
    const settled: AsyncLiveness = {
      case: "settled",
      value: { settledAtMs: 1, shellExit: { code: 0 }, outcome: { case: "done" } },
    };
    const registry = seeded(
      bubble({
        id: "b1",
        kind: { case: "shell", value: { command: "make", output: { text: "", throughOffset: 0 } } },
        liveness: settled,
      }),
    );

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("done · exited 0");
  });

  it("omits an exit clause for work that concluded rather than exited", () => {
    const settled: AsyncLiveness = { case: "settled", value: { settledAtMs: 1, outcome: { case: "done" } } };
    const registry = seeded(bubble({ id: "b1", kind: agentKind, liveness: settled }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).not.toContain("exited");
  });

  it("carries a failure's resolved message when the source gave one", () => {
    const settled: AsyncLiveness = {
      case: "settled",
      value: { settledAtMs: 1, outcome: { case: "error", message: "build broke" } },
    };
    const registry = seeded(bubble({ id: "b1", kind: agentKind, liveness: settled }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("failed · build broke");
  });

  it("never manufactures a reason a failing source did not give", () => {
    const settled: AsyncLiveness = {
      case: "settled",
      value: { settledAtMs: 1, outcome: { case: "error", message: "" } },
    };
    const registry = seeded(bubble({ id: "b1", kind: agentKind, liveness: settled }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("failed ");
  });
});

describe("the agent kind", () => {
  it("hands the bubble's emissions to the FEED's renderer, not a second one", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: {
          case: "agent",
          value: {
            emissions: [
              { emission: "response", arm: "assistantMessage", payload: {} },
              { emission: "thinking", arm: "thinking", payload: {} },
            ],
            fold: NO_FOLD,
          },
        },
      }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain('<div class="stub-emissions" data-bubble="b1">2</div>');
  });

  it("renders no body at all while the fold is closed", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: {
          case: "agent",
          value: { emissions: [{ emission: "response", arm: "assistantMessage", payload: {} }], fold: NO_FOLD },
        },
      }),
    );

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).not.toContain("stub-emissions");
  });
});

describe("the workflow journal kind", () => {
  const journal = (rows: { label: string; detail: string; status: "running" | "done" | "failed" }[]) =>
    seeded(bubble({ id: "b1", kind: { case: "journal", value: { rows, fold: NO_FOLD } } }));

  it("renders each step as a row with its label and detail", () => {
    const registry = journal([{ label: "plan", detail: "3 dimensions", status: "done" }]);

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("plan");
    expect(html).toContain("3 dimensions");
  });

  it("dots a running step with the running hue", () => {
    const registry = journal([{ label: "review", detail: "", status: "running" }]);

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("agent-dot agent-running");
  });

  it("dots a failed step with the error hue", () => {
    const registry = journal([{ label: "review", detail: "boom", status: "failed" }]);

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("agent-dot agent-error");
  });

  it("escapes a row's detail rather than trusting it as markup", () => {
    const registry = journal([{ label: "x", detail: "<img onerror=1>", status: "done" }]);

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).not.toContain("<img");
  });
});

describe("the spool kinds", () => {
  it("headers a shell bubble with its verbatim command line", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: { case: "shell", value: { command: "./scripts/soak.sh", output: { text: "ok", throughOffset: 2 } } },
      }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("./scripts/soak.sh");
  });

  it("keeps a shell's bytes unparsed, in a pre", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: { case: "shell", value: { command: "make", output: { text: "line 1\nline 2", throughOffset: 13 } } },
      }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("<pre class=\"tool-output task-live-output\">line 1\nline 2</pre>");
  });

  it("NAMES the tool an unclassified bubble could not be classified from", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: { case: "unclassified", value: { toolName: "Frobnicate", output: { text: "", throughOffset: 0 } } },
      }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("unclassified tool: Frobnicate");
  });

  it("draws an unclassified bubble rather than dropping the running work", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: { case: "unclassified", value: { toolName: "Frobnicate", output: { text: "", throughOffset: 0 } } },
      }),
    );

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("unclassified");
  });

  it("never guesses an unclassified bubble into another kind's face", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: { case: "unclassified", value: { toolName: "Frobnicate", output: { text: "", throughOffset: 0 } } },
      }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry));

    expect(html).not.toContain("shell ·");
  });
});

describe("earlierEntriesNotice", () => {
  it("draws nothing for a complete fold", () => {
    expect(earlierEntriesNotice({ droppedBefore: 0, tailCap: 200 }, "b1")).toBe("");
  });

  it("names the dropped count so a capped fold is not mistaken for a complete one", () => {
    expect(earlierEntriesNotice({ droppedBefore: 12, tailCap: 200 }, "b1")).toContain("12 earlier entries");
  });

  it("names the cap the daemon applied", () => {
    expect(earlierEntriesNotice({ droppedBefore: 12, tailCap: 200 }, "b1")).toContain("tail cap 200");
  });

  it("says entry, singular, for exactly one dropped", () => {
    expect(earlierEntriesNotice({ droppedBefore: 1, tailCap: 200 }, "b1")).toContain("1 earlier entry");
  });
});

describe("the spawn tree", () => {
  it("nests a child bubble inside the bubble it was spawned from", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind, label: "parent" }),
      bubble({ id: "b2", kind: agentKind, label: "child", parentBubbleId: "b1" }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("child");
  });

  it("nests a grandchild, resolved by pointer rather than by payload recursion", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind }),
      bubble({ id: "b2", kind: agentKind, parentBubbleId: "b1" }),
      bubble({ id: "b3", kind: agentKind, label: "deep", parentBubbleId: "b2" }),
    );

    const html = AsyncBubbleCard(
      registry.get("b1")!,
      ctxFor(registry, [bubbleFoldId("b1"), bubbleFoldId("b2")]),
    );

    expect(html).toContain("deep");
  });

  it("cuts a cyclic parent-pointer branch instead of recursing forever", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind, parentBubbleId: "b2" }),
      bubble({ id: "b2", kind: agentKind, parentBubbleId: "b1" }),
    );

    const html = AsyncBubbleCard(
      registry.get("b1")!,
      ctxFor(registry, [bubbleFoldId("b1"), bubbleFoldId("b2")]),
    );

    expect(html).toContain("parent pointers form a cycle");
  });
});

describe("AsyncBubbleForCall — attachment by the daemon's verdict", () => {
  it("draws the bubble a card's verdict names", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind, label: "detached" }));

    expect(AsyncBubbleForCall("b1", ctxFor(registry))).toContain("detached");
  });

  it("draws nothing for an ABSENT verdict — the call detached nothing", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind }));

    expect(AsyncBubbleForCall(undefined, ctxFor(registry))).toBe("");
  });

  it("draws nothing for an EMPTY verdict, never going looking for a candidate", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind }));

    expect(AsyncBubbleForCall("", ctxFor(registry))).toBe("");
  });

  it("draws nothing, and no placeholder, for a verdict naming an unopened bubble", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind }));

    expect(AsyncBubbleForCall("b9", ctxFor(registry))).toBe("");
  });
});

describe("AsyncBubbleForest", () => {
  it("draws the tree's roots", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind, label: "root" }),
      bubble({ id: "b2", kind: agentKind, label: "child", parentBubbleId: "b1" }),
    );

    const html = AsyncBubbleForest(ctxFor(registry));

    expect(html).toContain("root");
  });

  it("draws a child only once, inside its parent rather than beside it", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind }),
      bubble({ id: "b2", kind: agentKind, label: "child", parentBubbleId: "b1" }),
    );

    const html = AsyncBubbleForest(ctxFor(registry));

    expect(html).not.toContain("child");
  });

  it("draws an orphan set off, rather than dropping its live work", () => {
    const registry = seeded(bubble({ id: "b2", kind: agentKind, label: "stranded", parentBubbleId: "gone" }));

    const html = AsyncBubbleForest(ctxFor(registry));

    expect(html).toContain("stranded");
  });

  it("says WHICH parent never arrived rather than promoting the orphan to a root", () => {
    const registry = seeded(bubble({ id: "b2", kind: agentKind, parentBubbleId: "gone" }));

    const html = AsyncBubbleForest(ctxFor(registry));

    expect(html).toContain("parent gone not delivered");
  });
});
