/**
 * The store's ruling on a positionless history page: correlation, rank, and
 * the live splice. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";

import { ConversationStore, type ConversationItem, type TextItem } from "../src/store.js";
import type { AdapterEffect, WorkspaceStatusInput } from "../src/state-adapter.js";

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

function historyPageEffect(over: {
  requestId: string;
  items: ConversationItem[];
  continuation?: { case: "more" } | { case: "start" };
  liveJoinSeq?: number;
}): AdapterEffect {
  return {
    kind: "conversation-history-page",
    workspace: "ws",
    requestId: over.requestId,
    items: over.items,
    anchored: [],
    continuation: over.continuation ?? { case: "start" },
    liveJoinSeq: over.liveJoinSeq ?? 0,
  };
}

function armedStore(): ConversationStore {
  const store = new ConversationStore();
  store.ingest([workspaceEffect()]);
  return store;
}

const textOf = (store: ConversationStore): string[] =>
  store.state.items.filter((i) => i.kind === "text").map((i) => (i as { text: string }).text);

describe("applying a positionless history page", () => {
  it("renders a FIRST page's messages", () => {
    // Arrange
    const store = armedStore();
    store.noteHistoryPageRequested("r-1", "first");
    // Act
    store.ingest([historyPageEffect({ requestId: "r-1", items: [textItem("b1", "hello")] })]);
    // Assert
    expect(textOf(store)).toEqual(["hello"]);
  });

  it("accepts a SHORT page rather than treating it as an error", () => {
    // Arrange — at the top of a conversation this is the normal case.
    const store = armedStore();
    store.noteHistoryPageRequested("r-1", "next");
    // Act
    const result = store.ingest([
      historyPageEffect({ requestId: "r-1", items: [textItem("b1", "the beginning")] }),
    ]);
    // Assert
    expect(result.changed).toBe(true);
    expect(textOf(store)).toEqual(["the beginning"]);
  });

  it("DISCARDS a page whose request id is not the one awaited", () => {
    // Arrange — the case a generation change produces, handled with no fence.
    const store = armedStore();
    store.noteHistoryPageRequested("r-2", "first");
    // Act
    store.ingest([historyPageEffect({ requestId: "r-1", items: [textItem("b1", "stale")] })]);
    // Assert
    expect(textOf(store)).toEqual([]);
  });

  it("DISCARDS a page when no request is outstanding at all", () => {
    // Arrange
    const store = armedStore();
    // Act
    store.ingest([historyPageEffect({ requestId: "r-1", items: [textItem("b1", "unasked")] })]);
    // Assert
    expect(textOf(store)).toEqual([]);
  });

  it("splices onto the live stream at a FIRST page's live_join_seq", () => {
    // Arrange
    const store = armedStore();
    store.noteHistoryPageRequested("r-1", "first");
    // Act
    store.ingest([
      historyPageEffect({ requestId: "r-1", items: [textItem("b1", "tail")], liveJoinSeq: 4096 }),
    ]);
    // Assert — the mark the next resync asks from.
    expect(store.state.lastSeq).toBe(4096);
  });

  it("never moves the live mark from a NEXT page", () => {
    // Arrange — a next page is history and carries no live edge.
    const store = armedStore();
    store.noteHistoryPageRequested("r-1", "first");
    store.ingest([historyPageEffect({ requestId: "r-1", items: [], liveJoinSeq: 4096 })]);
    store.noteHistoryPageRequested("r-2", "next");
    // Act
    store.ingest([historyPageEffect({ requestId: "r-2", items: [textItem("b2", "older")], liveJoinSeq: 0 })]);
    // Assert
    expect(store.state.lastSeq).toBe(4096);
  });

  it("ranks a NEXT page's messages BELOW everything the feed holds", () => {
    // Arrange
    const store = armedStore();
    store.noteHistoryPageRequested("r-1", "first");
    store.ingest([
      historyPageEffect({ requestId: "r-1", items: [textItem("b1", "newer")], liveJoinSeq: 10 }),
    ]);
    store.noteHistoryPageRequested("r-2", "next");
    // Act
    store.ingest([
      historyPageEffect({ requestId: "r-2", items: [textItem("b0", "older")], continuation: { case: "more" } }),
    ]);
    // Assert
    expect(textOf(store)).toEqual(["older", "newer"]);
  });

  it("retires the load-more affordance on the start arm", () => {
    // Arrange
    const store = armedStore();
    store.noteHistoryPageRequested("r-1", "next");
    // Act
    store.ingest([historyPageEffect({ requestId: "r-1", items: [], continuation: { case: "start" } })]);
    // Assert
    expect(store.state.paging.reachedStart).toBe(true);
  });

  it("records a first-page request with NO fence, because there is none to echo", () => {
    // Arrange
    const store = armedStore();
    // Act
    store.noteHistoryPageRequested("r-1", "first");
    // Assert
    expect(store.state.paging.inFlight).toEqual({ requestId: "r-1", anchor: "tail", fence: "" });
  });
});
