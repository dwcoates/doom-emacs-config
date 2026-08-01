import { describe, expect, it } from "vitest";

// The shared fixture, imported rather than read off disk: the webapp targets
// the browser and carries no node typings, so a `node:fs` read here would not
// type-check even though vitest would run it.
import fixtureJson from "../../testdata/stream-contract/one-message.json";
import { decodeFrontendFrame } from "../src/frontend-proto.js";
import { StateAdapter } from "../src/state-adapter.js";
import { ConversationStore } from "../src/store.js";
import type { TextItem, ThinkingItem } from "../src/store.js";

/**
 * The streaming identity CONTRACT, asserted at this end of the chain.
 *
 * One assistant message is pushed through the real decode → adapt → store path
 * exactly as the daemon delivers it, and must land as exactly one item per
 * content block. Every earlier hop asserts its own half against the SAME
 * fixture (see the file's `$comment`), so a hop that starts deriving identity
 * differently fails here or there rather than silently rendering a stalled
 * preview beside its final card.
 *
 * The message deliberately holds TWO blocks. That is the shape the bug lived
 * in: the deltas number the blocks 0 and 1, while each finished record carries
 * a `content` array of length one whose index is therefore always 0. A
 * single-block fixture reconciles even when the two ordinals disagree, so it
 * would pass throughout and prove nothing.
 */

interface Fixture {
  sessionId: string;
  workspace: string;
  messageId: string;
  frontendFrames: unknown[];
  expectItems: Array<{
    kind: string;
    text: string;
    done: boolean;
    messageId: string;
    blockId: string;
  }>;
}

const fixture = fixtureJson as unknown as Fixture;

/**
 * Stamp `CONVERSATION_SOURCE_USER` on every conversation item the fixture
 * carries.
 *
 * The fixture is SHARED with the Go end and lives outside this package, so it
 * still predates `ConversationItem.source`. The adapter's provenance gate
 * refuses an item without one (the daemon always sets it), so leaving the
 * fixture unstamped would test the gate rather than the streaming identity this
 * file exists to pin. Nothing else about the frames is touched.
 */
function stampUserSource(frame: unknown): unknown {
  if (typeof frame !== "object" || frame === null) return frame;
  const delta = (frame as Record<string, unknown>).conversationDelta;
  if (typeof delta !== "object" || delta === null) return frame;
  const items = (delta as Record<string, unknown>).items;
  if (!Array.isArray(items)) return frame;
  return {
    ...(frame as Record<string, unknown>),
    conversationDelta: {
      ...(delta as Record<string, unknown>),
      items: items.map((item) => ({
        source: "CONVERSATION_SOURCE_USER",
        ...(item as Record<string, unknown>),
      })),
    },
  };
}

/** Push every fixture frame through the real decode → adapt → store path. */
function drainFixture(): ConversationStore {
  const adapter = new StateAdapter();
  const store = new ConversationStore();
  for (const raw of fixture.frontendFrames) {
    store.ingest(adapter.apply(decodeFrontendFrame(JSON.stringify(stampUserSource(raw)))));
  }
  return store;
}

/** The prose blocks the fixture's message produced, in feed order. */
function blocks(store: ConversationStore): Array<TextItem | ThinkingItem> {
  return store.state.items.filter(
    (i): i is TextItem | ThinkingItem => i.kind === "text" || i.kind === "thinking",
  );
}

describe("one streamed message lands in exactly one item per block", () => {
  it("produces one item per content block, and no more", () => {
    // Arrange / Act
    const store = drainFixture();
    // Assert: the bug rendered three — a stalled text preview beside its final.
    expect(blocks(store)).toHaveLength(fixture.expectItems.length);
  });

  it("lands each block complete, in order, with its final text", () => {
    // Arrange / Act
    const store = drainFixture();
    // Assert
    expect(
      blocks(store).map((b) => ({
        kind: b.kind,
        text: b.text,
        done: b.done,
        messageId: b.messageId,
        blockId: b.blockId,
      })),
    ).toEqual(fixture.expectItems);
  });

  it("keeps every block on the feed place its own preview opened", () => {
    // The reveal cursor and the DOM node are both keyed on it, so a settling
    // block that moved would re-type prose already on screen.
    // Arrange / Act
    const store = drainFixture();
    // Assert
    expect(blocks(store).map((b) => b.blockId)).toEqual([
      `${fixture.messageId}:0`,
      `${fixture.messageId}:1`,
    ]);
  });

  it("gives the two blocks of one message distinct record identities", () => {
    // Both records repeat the same message id, so identities derived from it
    // would collide and one block would silently overwrite the other.
    // Arrange / Act
    const store = drainFixture();
    // Assert
    const uuids = blocks(store).map((b) => b.uuid);
    expect(new Set(uuids).size).toBe(uuids.length);
  });

  it("replaces rather than duplicates when the whole stream is replayed", () => {
    // Arrange: a reconnect redelivers the persistent frames.
    const store = drainFixture();
    const adapter = new StateAdapter();

    // Act
    for (const raw of fixture.frontendFrames) {
      store.ingest(adapter.apply(decodeFrontendFrame(JSON.stringify(raw))));
    }

    // Assert
    expect(blocks(store)).toHaveLength(fixture.expectItems.length);
  });
});
