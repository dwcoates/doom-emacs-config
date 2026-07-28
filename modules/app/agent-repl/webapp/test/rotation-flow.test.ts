// @vitest-environment jsdom
/**
 * THE VENDOR SESSION ROTATION, as the client experiences it: protojson frames →
 * `StateAdapter` → the `SessionRebase` ruling → `ConversationStore` →
 * `FeedRenderer` → the DOM, folded in exactly the order main.ts folds them
 * (rebase BEFORE the ingest, resync AFTER it).
 *
 * The unit suites pin each piece — test/session-rebase.test.ts the verdicts,
 * test/store.test.ts the wipe, test/paint-attest.test.ts the watermark. What is
 * only true ACROSS them is what this file covers, and it is the defect itself:
 *
 * - a `/clear` rotates the vendor session uuid, so the daemon's seqs restart at
 *   1 while this end holds a mark from the retired space (1060, in production);
 * - every post-rotation item therefore arrives at-or-below that mark, and a
 *   client that keeps its retired-space state ranks the new conversation
 *   underneath a thousand items that no longer exist — the observed "no red
 *   line and no truncation, ever";
 * - and the protection that mark exists for — a re-delivered item REPLACES
 *   rather than duplicates — has to survive the fix untouched.
 */
import { describe, expect, it } from "vitest";

import { decodeFrontendFrame } from "../src/frontend-proto.js";
import { Actions, FeedRenderer } from "../src/render.js";
import { SessionRebase, claudeSessionIdOf } from "../src/session-rebase.js";
import { StateAdapter } from "../src/state-adapter.js";
import { ConversationStore } from "../src/store.js";

/** One protojson `ConversationItem`, as it rides inside a `ConversationDelta`. */
type WireItem = Record<string, unknown>;

const TS_MS = Date.parse("2026-07-27T09:00:00.000Z");

const ROTATION_ACTIONS: Actions = {
  decidePermission() {},
  answerQuestions() {},
  cancelQueued() {},
  runQueuedNow() {},
  acceptQueued() {},
};

function answerItem(uuid: string, text: string): WireItem {
  return {
    uuid,
    tsMs: String(TS_MS),
    assistantMessage: { id: `msg-${uuid}`, content: [{ text: { text } }] },
  };
}

function clearItem(uuid: string): WireItem {
  return { uuid, tsMs: String(TS_MS), contextCleared: {} };
}

/** The client, folding frames the way main.ts's onMessage does. */
interface Client {
  container: HTMLElement;
  store: ConversationStore;
  /** Every `from_seq` the rebase asked a resync with. */
  resyncs: number[];
  /** Fold one `ConversationDelta` carrying ITEMS at THROUGHSEQ. */
  delta(items: readonly WireItem[], throughSeq: number): void;
  /** Fold one `SessionView` announcing CLAUDESESSIONID. */
  announce(claudeSessionId: string): void;
}

function client(): Client {
  const container = document.createElement("div");
  const feed = new FeedRenderer(container, ROTATION_ACTIONS);
  const store = new ConversationStore();
  const adapter = new StateAdapter();
  const rebase = new SessionRebase();
  const resyncs: number[] = [];
  const fold = (frameJson: string): void => {
    const effects = adapter.apply(decodeFrontendFrame(frameJson));
    // main.ts: the retired space's items and marks go BEFORE the new space's
    // items land, so those items rank in the space they belong to.
    const verdict = rebase.observe(claudeSessionIdOf(effects));
    if (verdict === "rotated") store.rebaseSeqSpace();
    store.ingest(effects);
    // main.ts: and the replay is asked for AFTER, when the workspace is known.
    if (verdict === "rotated") resyncs.push(0);
    feed.render(store.state);
  };
  return {
    container,
    store,
    resyncs,
    delta(items, throughSeq) {
      fold(
        JSON.stringify({
          conversationDelta: {
            workspace: "/ws",
            sessionId: "s1",
            throughSeq: String(throughSeq),
            items,
          },
        }),
      );
    },
    announce(claudeSessionId) {
      fold(
        JSON.stringify({
          sessionView: {
            workspace: "/ws",
            sessionId: "s1",
            model: "claude-opus",
            slug: "",
            title: "",
            totalTokens: "0",
            totalCostUsd: 0,
            contextWindow: "0",
            permissionMode: "default",
            shimAttached: true,
            claudeSessionId,
            cwd: "/ws",
            configDir: "",
          },
        }),
      );
    },
  };
}

/** A client that has drawn a long conversation under uuid-a. */
function drawnConversation(): Client {
  const c = client();
  c.announce("uuid-a");
  c.delta([answerItem("a1", "the retired conversation")], 1060);
  return c;
}

describe("a vendor session uuid rotating under a drawn conversation", () => {
  it("draws the new space's clear and takes the retired history off screen", () => {
    // Arrange — THE DEFECT. The sidecar files the clear under the ROTATED uuid,
    // so it arrives at seq 12 while this end holds 1060.
    const c = drawnConversation();
    // Act
    c.announce("uuid-b");
    c.delta([clearItem("c-1")], 12);
    // Assert
    expect(c.container.querySelector(".clear-divider")).not.toBeNull();
    expect(c.container.textContent).not.toContain("the retired conversation");
  });

  it("renders a new-space item that lands below the retired mark", () => {
    // Arrange — every seq the rotated conversation produces is below 1060 for a
    // long while; none of them may be treated as history already drawn.
    const c = drawnConversation();
    c.announce("uuid-b");
    // Act
    c.delta([answerItem("b1", "the rebased conversation")], 1);
    // Assert — at the feed's TAIL, where a live item belongs. Ranked against
    // the retired mark it would sit a thousand positions above, which is the
    // "delivered but never seen" half of the defect.
    const items = [...c.container.querySelectorAll<HTMLElement>(".feed-item")];
    expect(items.at(-1)?.textContent).toContain("the rebased conversation");
  });

  it("asks the daemon for the new space from its replay floor", () => {
    // Arrange — the rebased view holds nothing, and the items it dropped may
    // have arrived before this end learned the rotation.
    const c = drawnConversation();
    // Act
    c.announce("uuid-b");
    // Assert
    expect(c.resyncs).toEqual([0]);
  });

  it("recovers a new-space item that arrived BEFORE the rotation was announced", () => {
    // Arrange — THE ORDERING HAZARD. The item is folded while this end still
    // believes it is in the retired space, so the wipe discards it.
    const c = drawnConversation();
    c.delta([answerItem("b1", "pushed ahead of the announcement")], 2);
    c.announce("uuid-b");
    expect(c.container.textContent).not.toContain("pushed ahead of the announcement");
    // Act — the resync the rebase asked for replays the new space.
    c.delta([answerItem("b1", "pushed ahead of the announcement")], 2);
    // Assert
    expect(c.container.textContent).toContain("pushed ahead of the announcement");
  });

  it("asks for nothing on the FIRST uuid a fresh view is told", () => {
    // Arrange — a fresh mount's first SessionView is an adoption. Rebasing there
    // would wipe the history the connect resync just delivered and ask again,
    // forever.
    const c = client();
    // Act
    c.announce("uuid-a");
    // Assert
    expect(c.resyncs).toEqual([]);
  });
});

describe("re-delivery protection, unweakened by the rebase", () => {
  it("replaces rather than duplicates an item re-delivered at-or-below the mark", () => {
    // Arrange — a resync replay of the SAME space re-sends what the feed already
    // holds; the store reconciles it by uuid.
    const c = client();
    c.announce("uuid-a");
    c.delta([answerItem("a1", "the only copy")], 1060);
    // Act — the same item comes back below the mark, as a replay does.
    c.delta([answerItem("a1", "the only copy")], 900);
    // Assert
    expect(c.store.state.items).toHaveLength(1);
  });

  it("leaves the mark standing when nothing rotated", () => {
    // Arrange — a replay must never drag the watermark backwards: the next
    // resync would re-ask for history already drawn.
    const c = client();
    c.announce("uuid-a");
    c.delta([answerItem("a1", "the only copy")], 1060);
    // Act
    c.delta([answerItem("a1", "the only copy")], 900);
    // Assert
    expect(c.store.state.lastSeq).toBe(1060);
  });
});
