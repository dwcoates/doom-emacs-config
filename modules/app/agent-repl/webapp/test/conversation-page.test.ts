import { describe, expect, it } from "vitest";

import { ConversationStore, type ConversationItem, type TextItem } from "../src/store.js";
import type { AdapterEffect, WorkspaceStatusInput } from "../src/state-adapter.js";

/**
 * THE COLD OPEN IS A PAGE, AND THE SPLICE ONTO THE LIVE STREAM IS GAP-FREE.
 *
 * A cold mount used to ask for `from_seq: 0` and receive every store event the
 * session ever produced (259k events / 186MB in the worst workspace observed)
 * to draw a screen whose visible tail is about ten items. These cases pin the
 * replacement: a bounded tail page renders immediately, a load-more walks
 * backwards from it, and the mark the live stream is rejoined at cannot lose
 * an item produced while the page was in flight.
 */

// --- fixtures ---------------------------------------------------------------

function workspaceEffect(over: Partial<WorkspaceStatusInput> = {}): AdapterEffect {
  return {
    kind: "workspace-state",
    value: {
      workspace: "ws",
      sessionId: "s1",
      fence: "f1",
      state: "idle",
      turnActive: false,
      liveTaskCount: 0,
      causeKind: "turn_ended",
      causeSeq: 1,
      atMs: 1000,
      connectivity: "operational",
      sessionStatus: "ready",
      controllerGenerationId: "g1",
      activeFaults: [],
      mergeLeaseHeld: false,
      mergeStatus: null,
      mergeDequeueOffer: null,
      ...over,
    },
  };
}

function textItem(blockId: string, text: string): TextItem {
  return { kind: "text", blockId, messageId: `m-${blockId}`, text, done: true, ts: "2026-01-01T00:00:00Z" };
}

function pageEffect(over: {
  requestId: string;
  items: ConversationItem[];
  continuation?: { case: "more"; cursor: string } | { case: "start" };
  liveJoinSeq?: number;
  fence?: string;
}): AdapterEffect {
  return {
    kind: "conversation-page",
    workspace: "ws",
    fence: over.fence ?? "f1",
    requestId: over.requestId,
    items: over.items,
    continuation: over.continuation ?? { case: "start" },
    liveJoinSeq: over.liveJoinSeq ?? 0,
  };
}

function deltaEffect(items: ConversationItem[], throughSeq: number): AdapterEffect {
  return { kind: "conversation-items", workspace: "ws", fence: "f1", throughSeq, items };
}

/** A store with a live workspace fence, which is what a page is measured against. */
function armedStore(): ConversationStore {
  const store = new ConversationStore();
  store.ingest([workspaceEffect()]);
  return store;
}

const textOf = (store: ConversationStore): string[] =>
  store.state.items.filter((i) => i.kind === "text").map((i) => (i as { text: string }).text);

// --- the tail page ----------------------------------------------------------

describe("the cold-open tail page", () => {
  it("renders its items immediately, oldest first", () => {
    // Arrange — the cold open asked for the newest three.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    // Act
    store.ingest([
      pageEffect({
        requestId: "r-1",
        items: [textItem("a", "18"), textItem("b", "19"), textItem("c", "20")],
        continuation: { case: "more", cursor: "cp1-A" },
        liveJoinSeq: 20,
      }),
    ]);
    // Assert
    expect(textOf(store)).toEqual(["18", "19", "20"]);
  });

  it("adopts live_join_seq as the mark the live stream is rejoined at", () => {
    // Arrange
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    // Act
    store.ingest([pageEffect({ requestId: "r-1", items: [textItem("c", "20")], liveJoinSeq: 20 })]);
    // Assert — the next resync asks from here, INCLUSIVELY.
    expect(store.state.lastSeq).toBe(20);
  });

  it("does not lose an item produced between the page's mint and the subscribe", () => {
    // Arrange — THE GAP-FREE SPLICE. The page is current through seq 20; the
    // session produced seq 21 while the page was still travelling. Because the
    // client rejoins at 20 and the daemon's replay is inclusive of the mark,
    // that item arrives on the very first delta rather than being skipped.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    store.ingest([pageEffect({ requestId: "r-1", items: [textItem("c", "20")], liveJoinSeq: 20 })]);
    // Act — the replay from lastSeq re-delivers 20 (deduped by id) and 21.
    store.ingest([deltaEffect([textItem("c", "20")], 20), deltaEffect([textItem("d", "21")], 21)]);
    // Assert
    expect(textOf(store)).toEqual(["20", "21"]);
    expect(store.state.lastSeq).toBe(21);
  });

  it("an empty conversation pages to nothing without breaking the mark", () => {
    // Arrange — a fresh workspace. Silence is the empty conversation it is.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    // Act
    store.ingest([pageEffect({ requestId: "r-1", items: [], continuation: { case: "start" } })]);
    // Assert
    expect(store.state.items).toHaveLength(0);
    expect(store.state.lastSeq).toBe(0);
    expect(store.state.paging.reachedStart).toBe(true);
  });
});

// --- load-more --------------------------------------------------------------

describe("load-more", () => {
  it("prepends an older page ABOVE everything the feed already holds", () => {
    // Arrange — the tail page is on screen.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    store.ingest([
      pageEffect({
        requestId: "r-1",
        items: [textItem("c", "20")],
        continuation: { case: "more", cursor: "cp1-A" },
        liveJoinSeq: 20,
      }),
    ]);
    // Act
    store.notePageRequested({ requestId: "r-2", anchor: "before", fence: "f1" });
    store.ingest([
      pageEffect({
        requestId: "r-2",
        items: [textItem("a", "18"), textItem("b", "19")],
        continuation: { case: "more", cursor: "cp1-B" },
      }),
    ]);
    // Assert — history reads in conversation order, not arrival order.
    expect(textOf(store)).toEqual(["18", "19", "20"]);
  });

  it("keeps walking down across successive pages", () => {
    // Arrange
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    store.ingest([
      pageEffect({ requestId: "r-1", items: [textItem("c", "3")], continuation: { case: "more", cursor: "A" }, liveJoinSeq: 3 }),
    ]);
    store.notePageRequested({ requestId: "r-2", anchor: "before", fence: "f1" });
    store.ingest([
      pageEffect({ requestId: "r-2", items: [textItem("b", "2")], continuation: { case: "more", cursor: "B" } }),
    ]);
    // Act
    store.notePageRequested({ requestId: "r-3", anchor: "before", fence: "f1" });
    store.ingest([pageEffect({ requestId: "r-3", items: [textItem("a", "1")], continuation: { case: "start" } })]);
    // Assert
    expect(textOf(store)).toEqual(["1", "2", "3"]);
  });

  it("a before page never moves the live mark backwards", () => {
    // Arrange — a load-more is history; adopting a mark from it would make the
    // next resync re-replay everything above it.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    store.ingest([pageEffect({ requestId: "r-1", items: [textItem("c", "20")], liveJoinSeq: 20, continuation: { case: "more", cursor: "A" } })]);
    // Act
    store.notePageRequested({ requestId: "r-2", anchor: "before", fence: "f1" });
    store.ingest([pageEffect({ requestId: "r-2", items: [textItem("b", "19")], continuation: { case: "start" } })]);
    // Assert
    expect(store.state.lastSeq).toBe(20);
  });

  it("the continuation cursor is retained verbatim for the next request", () => {
    // Arrange — opaque: this end stores and returns it, never parses it.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    // Act
    store.ingest([
      pageEffect({ requestId: "r-1", items: [], continuation: { case: "more", cursor: "cp1-OPAQUE" } }),
    ]);
    // Assert
    expect(store.state.paging.cursor).toBe("cp1-OPAQUE");
  });

  it("the conversation's beginning RETIRES the cursor", () => {
    // Arrange
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    store.ingest([pageEffect({ requestId: "r-1", items: [], continuation: { case: "more", cursor: "A" } })]);
    // Act
    store.notePageRequested({ requestId: "r-2", anchor: "before", fence: "f1" });
    store.ingest([pageEffect({ requestId: "r-2", items: [], continuation: { case: "start" } })]);
    // Assert
    expect(store.state.paging.reachedStart).toBe(true);
    expect(store.state.paging.cursor).toBeNull();
  });
});

// --- refusals ---------------------------------------------------------------

describe("a page the store will not adopt", () => {
  it("a stale fence discards the page WHOLE and marks it for re-request", () => {
    // Arrange — a page minted under a generation this client no longer reads.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    // Act
    store.ingest([
      pageEffect({
        requestId: "r-1",
        items: [textItem("a", "18"), textItem("b", "19")],
        fence: "f-retired",
        liveJoinSeq: 20,
      }),
    ]);
    // Assert — not one item of it was adopted, and the mark did not move.
    expect(store.state.items).toHaveLength(0);
    expect(store.state.lastSeq).toBe(0);
    expect(store.state.paging.staleFenceRequestId).toBe("r-1");
  });

  it("a page answering a request this client abandoned is discarded", () => {
    // Arrange — adopting it would splice history in at a cursor the feed no
    // longer holds.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    store.forgetPageRequest("r-1");
    // Act
    store.ingest([pageEffect({ requestId: "r-1", items: [textItem("a", "18")], liveJoinSeq: 20 })]);
    // Assert
    expect(store.state.items).toHaveLength(0);
  });

  it("a page whose id is not the outstanding one is discarded", () => {
    // Arrange — a cold open and a load-more can both be in flight; only the
    // echo distinguishes their answers.
    const store = armedStore();
    store.notePageRequested({ requestId: "r-2", anchor: "before", fence: "f1" });
    // Act
    store.ingest([pageEffect({ requestId: "r-1", items: [textItem("a", "18")] })]);
    // Assert
    expect(store.state.items).toHaveLength(0);
  });
});
