// @vitest-environment jsdom
/**
 * Clear / compaction, END TO END: protojson `ConversationDelta` frames →
 * `StateAdapter` → `ConversationStore` → `FeedRenderer` → the DOM.
 *
 * The unit coverage (test/clear-compact.test.ts, test/state-adapter.test.ts,
 * test/render.test.ts) already pins each stage in isolation. What is NOT
 * covered there is the stage-crossing contract these tests exist for:
 *
 * - a clear or a compaction TRUNCATES — nothing that preceded it in the
 *   conversation may remain in the DOM once one lands;
 * - a clear draws the red rule (`.clear-divider`), a compaction the orange one
 *   (`.compact-rule`) — the two hooks styles.css binds to `--clear-divider`
 *   and `--compact-rule`;
 * - a successful compaction's summary lands as a response bubble wearing the
 *   green-border/purple-wash pair (`.bubble.assistant.compact-summary`, bound
 *   to `--final-response` and `--compact-summary-bg`);
 * - the webapp does NO bookkeeping of its own — it renders exactly what the
 *   daemon sent, live and replayed alike.
 */
import { afterEach, describe, expect, it, vi } from "vitest";

import { sessionSubagents } from "../src/agents.js";
import { decodeFrontendFrame } from "../src/frontend-proto.js";
import { Actions, FeedRenderer } from "../src/render.js";
import { StateAdapter } from "../src/state-adapter.js";
import { ConversationStore } from "../src/store.js";

/** One protojson `ConversationItem`, as it rides inside a `ConversationDelta`. */
type WireItem = Record<string, unknown>;

/** When every item in these streams was stamped. */
const TS_MS = Date.parse("2026-05-24T09:00:00.000Z");

/** The reader's clock, pinned so a relative bubble stamp cannot drift mid-test. */
const NOW_MS = TS_MS + 60_000;

/**
 * A feed renderer's action sink. Local rather than shared: every test here
 * asserts on markup, and none clicks anything.
 */
const FLOW_ACTIONS: Actions = {
  decidePermission() {},
  answerQuestions() {},
  cancelQueued() {},
  runQueuedNow() {},
  acceptQueued() {},
};

/** A user prompt. */
function promptItem(uuid: string, text: string, requestId = uuid): WireItem {
  return { uuid, tsMs: String(TS_MS), requestId, userMessage: { contentString: text } };
}

/** An agent answer, as one text block of an assistant message. */
function answerItem(uuid: string, text: string): WireItem {
  return {
    uuid,
    tsMs: String(TS_MS),
    assistantMessage: { id: `msg-${uuid}`, content: [{ text: { text } }] },
  };
}

/** A subagent spawn, so the roster projection has something to lose. */
function subagentItem(uuid: string, toolUseId: string, description: string): WireItem {
  return {
    uuid,
    tsMs: String(TS_MS),
    assistantMessage: {
      id: `msg-${uuid}`,
      content: [
        {
          toolUse: {
            id: toolUseId,
            name: "Agent",
            input: { description, subagent_type: "Explore" },
          },
        },
      ],
    },
  };
}

/** A context clear — an EMPTY message whose existence and place are the fact. */
function clearItem(uuid = "cut-clear"): WireItem {
  return { uuid, tsMs: String(TS_MS), contextCleared: {} };
}

/** A compaction, defaulted to a successful manual one carrying a summary. */
function compactItem(over: Record<string, unknown> = {}, uuid = "cut-compact"): WireItem {
  return {
    uuid,
    tsMs: String(TS_MS),
    contextCompacted: {
      trigger: "CONTEXT_COMPACT_TRIGGER_MANUAL",
      preTokens: "287028",
      postTokens: "9001",
      durationMs: "4200",
      summary: "the story so far",
      ...over,
    },
  };
}

/** One end-to-end pipeline: frames in, painted DOM out. */
interface Flow {
  container: HTMLElement;
  feed: FeedRenderer;
  store: ConversationStore;
  /** Fold one `ConversationDelta` carrying ITEMS onto the store. */
  ingest(items: readonly WireItem[]): void;
}

function flow(): Flow {
  const container = document.createElement("div");
  const feed = new FeedRenderer(container, FLOW_ACTIONS);
  const store = new ConversationStore();
  const adapter = new StateAdapter();
  let seq = 0;
  return {
    container,
    feed,
    store,
    ingest(items) {
      seq += items.length;
      const frame = decodeFrontendFrame(
        JSON.stringify({
          conversationDelta: {
            workspace: "/ws",
            fence: "s1",
            throughSeq: String(seq),
            // The daemon stamps a provenance on every item it builds; the
            // adapter's provenance gate refuses an envelope without one.
            items: items.map((item) => ({ source: "CONVERSATION_SOURCE_USER", ...item })),
          },
        }),
      );
      store.ingest(adapter.apply(frame));
    },
  };
}

/** Deliver ITEMS the LIVE way: one frame each, painted as each lands. */
function live(items: readonly WireItem[]): Flow {
  const f = flow();
  for (const item of items) {
    f.ingest([item]);
    f.feed.render(f.store.state);
  }
  return f;
}

/** Deliver ITEMS the REPLAY way: one batched frame, painted as a fresh join. */
function replayed(items: readonly WireItem[]): Flow {
  const f = flow();
  f.ingest(items);
  f.feed.renderRestored(f.store.state);
  return f;
}

/** The feed's mounted item nodes. */
function feedItems(container: HTMLElement): HTMLElement[] {
  return [...container.querySelectorAll<HTMLElement>(".feed-item")];
}

afterEach(() => {
  vi.restoreAllMocks();
});

describe("a clear or a compaction landing live over a drawn conversation", () => {
  it("takes every preceding item off screen and leaves the red clear rule", () => {
    // Arrange — a rendered exchange the reader can see.
    vi.spyOn(Date, "now").mockReturnValue(NOW_MS);
    const f = live([
      promptItem("u1", "seed the history please"),
      answerItem("a1", "here is the seeded history"),
    ]);
    // Act — the daemon reports the context cleared.
    f.ingest([clearItem()]);
    f.feed.render(f.store.state);
    // Assert
    expect(f.container.textContent).not.toContain("seed the history please");
    expect(f.container.querySelector(".clear-divider")).not.toBeNull();
  });

  it("takes every preceding item off screen and leaves the orange compaction rule", () => {
    // Arrange
    vi.spyOn(Date, "now").mockReturnValue(NOW_MS);
    const f = live([
      promptItem("u1", "seed the history please"),
      answerItem("a1", "here is the seeded history"),
    ]);
    // Act
    f.ingest([compactItem()]);
    f.feed.render(f.store.state);
    // Assert
    expect(f.container.textContent).not.toContain("seed the history please");
    expect(f.container.querySelector(".compact-rule")).not.toBeNull();
  });
});

describe("a compaction's summary", () => {
  it("renders as the green-bordered, purple-washed response bubble", () => {
    // Arrange — the summary is the ONLY surviving account of what it replaced,
    // so it wears the final-response green on the purple wash no other bubble
    // uses (`.bubble.assistant.compact-summary` in styles.css).
    vi.spyOn(Date, "now").mockReturnValue(NOW_MS);
    // Act
    const f = live([promptItem("u1", "/compact"), compactItem()]);
    // Assert
    const bubble = f.container.querySelector(".bubble.assistant.compact-summary");
    expect(bubble?.textContent).toContain("the story so far");
  });

  it("paints above the boundary marker that survives the truncation with it", () => {
    // Arrange — the truncation keeps the compaction item itself, and the
    // summary rides in that same item's markup, so it stays on the visible
    // side; what this pins is the ORDER inside it.
    vi.spyOn(Date, "now").mockReturnValue(NOW_MS);
    const f = live([promptItem("u1", "/compact"), compactItem()]);
    // Act
    const bubble = f.container.querySelector(".bubble.assistant.compact-summary");
    const rule = f.container.querySelector(".compact-rule");
    // Assert — DOCUMENT_POSITION_FOLLOWING: the rule comes after the bubble.
    expect(bubble?.compareDocumentPosition(rule as Node)).toBe(
      Node.DOCUMENT_POSITION_FOLLOWING,
    );
  });

  it("renders an automatic compaction exactly as it renders a manual one", () => {
    // Arrange — an AUTO compaction has no `/compact` prompt above it at all;
    // whether the system or the user asked changes nothing a reader can act on.
    vi.spyOn(Date, "now").mockReturnValue(NOW_MS);
    const auto = live([
      answerItem("a1", "still working"),
      compactItem({ trigger: "CONTEXT_COMPACT_TRIGGER_AUTO" }),
    ]);
    const manual = live([promptItem("u1", "/compact"), compactItem()]);
    // Act — the compaction truncates, so it heads both feeds.
    const autoHtml = feedItems(auto.container)[0]?.innerHTML;
    const manualHtml = feedItems(manual.container)[0]?.innerHTML;
    // Assert
    expect(autoHtml).toBe(manualHtml);
  });
});


describe("a clear that opens an EMPTY conversation", () => {
  it("draws the red rule alone, with no other node in the feed", () => {
    // Arrange — nothing has ever been rendered; the clear is item one.
    vi.spyOn(Date, "now").mockReturnValue(NOW_MS);
    // Act
    const f = live([clearItem()]);
    // Assert — exactly one mounted item, and it is the rule.
    const items = feedItems(f.container);
    expect(items).toHaveLength(1);
    expect(items[0].querySelector(".clear-divider")).not.toBeNull();
  });
});

describe("replay and live agree on the painted feed", () => {
  it("paints the same DOM for a batched replay as for the incremental stream", () => {
    // Arrange — one sequence, two delivery shapes. A fresh join replays the
    // whole history in a single frame; a running session receives the same
    // items one frame at a time, over an already-painted feed.
    vi.spyOn(Date, "now").mockReturnValue(NOW_MS);
    const items: WireItem[] = [
      promptItem("u1", "seed the history please"),
      answerItem("a1", "here is the seeded history"),
      compactItem(),
      promptItem("u2", "carry on"),
      answerItem("a2", "carrying on"),
    ];
    // Act
    const incremental = live(items).container.innerHTML;
    const batched = replayed(items).container.innerHTML;
    // Assert
    expect(incremental).toBe(batched);
  });
});

describe("the store keeps no shadow state across a clear", () => {
  it("drops a pre-clear subagent from the roster the topbar reads", () => {
    // Arrange — a subagent spawned before the clear; the roster is a
    // projection of what the daemon sent, not a tally the webapp maintains.
    vi.spyOn(Date, "now").mockReturnValue(NOW_MS);
    const f = live([
      promptItem("u1", "go hunt the flake"),
      subagentItem("a1", "t1", "hunt the flake"),
    ]);
    expect(sessionSubagents(f.store.state.items)).toHaveLength(1);
    // Act
    f.ingest([clearItem()]);
    f.feed.render(f.store.state);
    // Assert
    expect(sessionSubagents(f.store.state.items)).toEqual([]);
  });
});
