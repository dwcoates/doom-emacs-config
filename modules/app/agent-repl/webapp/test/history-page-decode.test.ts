/**
 * Decoding a ConversationHistoryPage — the positionless page's ten slots.
 *
 * The slots are COMPLETE feed envelopes, identical in shape to the ones a
 * `ConversationDelta` carries, precisely so no second rendering path exists.
 * Absent slots mean a SHORT page, which at the top of a conversation is the
 * NORMAL case and never an error.
 *
 * One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { decodeFrontendFrame } from "../src/frontend-proto.js";
import { StateAdapter } from "../src/state-adapter.js";

const message = (uuid: string, text: string): Record<string, unknown> => ({
  source: "CONVERSATION_SOURCE_USER",
  uuid,
  assistantMessage: { content: [{ text: { text } }] },
});

const page = (over: Record<string, unknown> = {}): string =>
  JSON.stringify({
    conversationHistoryPage: {
      workspace: "/ws/a",
      requestId: "r-1",
      start: {},
      liveJoinSeq: "0",
      ...over,
    },
  });

describe("decoding a ConversationHistoryPage frame", () => {
  it("accepts a SHORT page as the normal top-of-conversation case", () => {
    // Arrange — two of ten slots filled; the rest are simply absent.
    const raw = page({ message1: message("m1", "older"), message2: message("m2", "newer") });
    // Act
    const frame = decodeFrontendFrame(raw);
    // Assert
    if (frame.frame.case !== "conversationHistoryPage") throw new Error("wrong arm");
    expect(frame.frame.value.messages).toHaveLength(2);
  });

  it("reads the slots OLDEST FIRST, in slot order", () => {
    // Arrange
    const raw = page({ message1: message("m1", "oldest"), message3: message("m3", "newest") });
    // Act
    const frame = decodeFrontendFrame(raw);
    // Assert
    if (frame.frame.case !== "conversationHistoryPage") throw new Error("wrong arm");
    expect(frame.frame.value.messages.map((m) => m.uuid)).toEqual(["m1", "m3"]);
  });

  it("accepts an EMPTY page, which a fresh workspace's first page is", () => {
    // Arrange / Act
    const frame = decodeFrontendFrame(page());
    // Assert
    if (frame.frame.case !== "conversationHistoryPage") throw new Error("wrong arm");
    expect(frame.frame.value.messages).toEqual([]);
  });

  it("reads live_join_seq as a number the store can rank by", () => {
    // Arrange — protojson renders uint64 as a string.
    // Act
    const frame = decodeFrontendFrame(page({ liveJoinSeq: "4096" }));
    // Assert
    if (frame.frame.case !== "conversationHistoryPage") throw new Error("wrong arm");
    expect(frame.frame.value.liveJoinSeq).toBe(4096);
  });

  it("reads the more arm as a bare FACT carrying no position", () => {
    // Arrange — the cursor that used to live here is the position the client
    // no longer holds.
    // Act
    const frame = decodeFrontendFrame(page({ start: undefined, more: {} }));
    // Assert
    if (frame.frame.case !== "conversationHistoryPage") throw new Error("wrong arm");
    expect(frame.frame.value.continuation).toEqual({ case: "more" });
  });

  it("refuses a more arm that smuggles a position back in", () => {
    // Arrange — a field here would be a position the client holds.
    // Act / Assert
    expect(() => decodeFrontendFrame(page({ start: undefined, more: { cursor: "c" } }))).toThrow(
      /unrecognized field/,
    );
  });

  it("refuses a page carrying NEITHER continuation arm", () => {
    // Arrange / Act / Assert
    expect(() => decodeFrontendFrame(page({ start: undefined }))).toThrow(/neither `more` nor `start`/);
  });

  it("refuses a page carrying BOTH continuation arms", () => {
    // Arrange / Act / Assert
    expect(() => decodeFrontendFrame(page({ more: {} }))).toThrow(/both `more` and `start`/);
  });

  it("refuses a page with no request_id, which nothing could correlate", () => {
    // Arrange — correlation is the whole staleness story; there is no fence.
    // Act / Assert
    expect(() => decodeFrontendFrame(page({ requestId: "" }))).toThrow(/request_id/);
  });

  it("projects a paged message through the SAME projection a pushed one uses", () => {
    // Arrange — the same envelope, delivered by each route.
    const envelope = message("m1", "hello");
    const pushed = new StateAdapter().apply(
      decodeFrontendFrame(
        JSON.stringify({
          conversationDelta: { fence: "f1", workspace: "/ws/a", throughSeq: "9", messages: [envelope] },
        }),
      ),
    );
    // Act
    const paged = new StateAdapter().apply(decodeFrontendFrame(page({ message1: envelope })));
    // Assert — identical items, so one renderer serves both.
    const pushedItems = pushed.find((e) => e.kind === "conversation-items");
    const pagedItems = paged.find((e) => e.kind === "conversation-history-page");
    if (pushedItems?.kind !== "conversation-items") throw new Error("no pushed items");
    if (pagedItems?.kind !== "conversation-history-page") throw new Error("no paged items");
    expect(pagedItems.items).toEqual(pushedItems.items);
  });
});
