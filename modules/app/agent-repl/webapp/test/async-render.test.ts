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
import type { UnwrappedEmission } from "../src/agent-emission.js";

const LIVE: AsyncLiveness = { case: "live", value: { lastActivityMs: 0 } };
const NO_FOLD = { droppedBefore: 0, tailCap: 0 };

function bubble(over: Partial<AsyncBubble> & Pick<AsyncBubble, "id" | "kind">): AsyncBubble {
  return {
    workspace: "/w",
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
const mergeKind: AsyncBubble["kind"] = { case: "merge", value: { emissions: [], fold: NO_FOLD } };

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

describe("the merge kind", () => {
  it("hands a merge run's emissions to the same FEED renderer an agent's go to", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: {
          case: "merge",
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

  it("says MERGE on the collapsed face, with the daemon's label verbatim", () => {
    const registry = seeded(bubble({ id: "b1", kind: mergeKind, label: "merge feat/x into master" }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain(
      "merge · merge feat/x into master · running",
    );
  });

  it("marks the fold with the merge kind class the merge amber hangs off", () => {
    const registry = seeded(bubble({ id: "b1", kind: mergeKind }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("async-fold async-kind-merge");
  });

  it("renders a settled merge run exactly as a settled agent run reads", () => {
    const settled: AsyncLiveness = { case: "settled", value: { settledAtMs: 1, outcome: { case: "done" } } };
    const registry = seeded(bubble({ id: "b1", kind: mergeKind, label: "l", liveness: settled }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("merge · l · done");
  });

  it("nests a subagent bubble parented under the merge bubble", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: mergeKind }),
      bubble({ id: "b2", kind: agentKind, label: "conflict resolver", parentBubbleId: "b1" }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("conflict resolver");
  });

  it("draws NO merge chrome for a feed whose bubbles are not merges", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind, label: "unrelated" }));

    expect(AsyncBubbleForest(ctxFor(registry))).not.toContain("merge");
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

describe("the skill kind", () => {
  const skillKind = (
    over: Partial<{ skillName: string; args: string; body: string; emissions: UnwrappedEmission[] }> = {},
  ): AsyncBubble["kind"] => ({
    case: "skill",
    value: {
      skillName: "demo",
      args: "",
      body: "",
      emissions: [],
      fold: NO_FOLD,
      ...over,
    },
  });

  it("hands a skill window's emissions to the same FEED renderer an agent's go to", () => {
    const registry = seeded(
      bubble({
        id: "b1",
        kind: skillKind({
          emissions: [
            { emission: "response", arm: "assistantMessage", payload: {} },
            { emission: "thinking", arm: "thinking", payload: {} },
          ],
        }),
      }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain('<div class="stub-emissions" data-bubble="b1">2</div>');
  });

  it("says SKILL on the collapsed face, with the daemon's label verbatim", () => {
    const registry = seeded(bubble({ id: "b1", kind: skillKind(), label: "/create-or-update-workspace merge" }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain(
      "skill · /create-or-update-workspace merge · running",
    );
  });

  it("marks the fold with the skill kind class the stylesheet dresses", () => {
    const registry = seeded(bubble({ id: "b1", kind: skillKind() }));

    expect(AsyncBubbleCard(registry.get("b1")!, ctxFor(registry))).toContain("async-fold async-kind-skill");
  });

  it("draws the SKILL's own document as the bubble's body", () => {
    const registry = seeded(bubble({ id: "b1", kind: skillKind({ body: "# Demo\n\nrun it" }) }));

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("run it");
  });

  it("draws no body section at all until the contents resolve", () => {
    const registry = seeded(bubble({ id: "b1", kind: skillKind() }));

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).not.toContain("skill-content");
  });

  it("nests a subagent bubble parented under the skill bubble", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: skillKind() }),
      bubble({ id: "b2", kind: agentKind, label: "dispatched worker", parentBubbleId: "b1" }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("dispatched worker");
  });

  it("nests a skill bubble invoked inside another skill bubble", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: skillKind() }),
      bubble({ id: "b2", kind: skillKind({ skillName: "inner" }), label: "/inner", parentBubbleId: "b1" }),
    );

    const html = AsyncBubbleCard(registry.get("b1")!, ctxFor(registry, [bubbleFoldId("b1")]));

    expect(html).toContain("skill · /inner · running");
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

describe("AsyncBubbleForCall — attachment by the daemon's classification", () => {
  it("draws the bubble whose origin_tool_use_id names the card", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind, label: "detached", originToolUseId: "tu1" }),
    );

    expect(AsyncBubbleForCall("tu1", undefined, ctxFor(registry))).toContain("detached");
  });

  it("draws the bubble a card's verdict names when no bubble names the card", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind, label: "detached" }));

    expect(AsyncBubbleForCall("tu1", "b1", ctxFor(registry))).toContain("detached");
  });

  it("draws EVERY bubble the daemon attributed to one call, hiding none", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind, label: "first", originToolUseId: "tu1" }),
      bubble({ id: "b2", kind: agentKind, label: "second", originToolUseId: "tu1" }),
    );

    const html = AsyncBubbleForCall("tu1", undefined, ctxFor(registry));

    expect([html.includes("first"), html.includes("second")]).toEqual([true, true]);
  });

  it("draws nothing when the two ends of the classification DISAGREE", () => {
    // Arrange — the bubble says it came from tu1; the card's verdict names a
    // different bubble. Neither statement is preferred over the other.
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind, label: "by origin", originToolUseId: "tu1" }),
      bubble({ id: "b2", kind: agentKind, label: "by verdict" }),
    );

    expect(AsyncBubbleForCall("tu1", "b2", ctxFor(registry))).toBe("");
  });

  it("draws the bubble when the two ends AGREE", () => {
    const registry = seeded(
      bubble({ id: "b1", kind: agentKind, label: "agreed", originToolUseId: "tu1" }),
    );

    expect(AsyncBubbleForCall("tu1", "b1", ctxFor(registry))).toContain("agreed");
  });

  it("draws nothing for a card no bubble names and no verdict on it", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind }));

    expect(AsyncBubbleForCall("tu1", undefined, ctxFor(registry))).toBe("");
  });

  it("draws nothing for an EMPTY verdict, never going looking for a candidate", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind }));

    expect(AsyncBubbleForCall("tu1", "", ctxFor(registry))).toBe("");
  });

  it("draws nothing, and no placeholder, for a verdict naming an unopened bubble", () => {
    const registry = seeded(bubble({ id: "b1", kind: agentKind }));

    expect(AsyncBubbleForCall("tu1", "b9", ctxFor(registry))).toBe("");
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
