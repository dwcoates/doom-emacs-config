import { describe, expect, it } from "vitest";
import { decodeFrontendFrame } from "../src/frontend-proto.js";

/**
 * THE PAGE DECODER REFUSES WHAT A RENDERER COULD NOT ACT ON.
 *
 * A page's continuation is the whole of the load-more contract: `more` says
 * older history remains, `start` retires the affordance. A page carrying
 * neither is rejected here rather than defaulted to either answer — defaulting
 * to `more` gives an affordance that can never succeed, and defaulting to
 * `start` silently hides the rest of a conversation.
 */

const page = (over: Record<string, unknown> = {}): string =>
  JSON.stringify({
    conversationPage: {
      workspace: "/ws/a",
      requestId: "r-1",
      messages: [],
      start: {},
      liveJoinSeq: 0,
      fence: "f1",
      ...over,
    },
  });

describe("decoding a ConversationPage frame", () => {
  it("reads the start arm as the conversation's beginning", () => {
    // Arrange / Act
    const frame = decodeFrontendFrame(page());
    // Assert
    expect(frame.frame.case).toBe("conversationPage");
    if (frame.frame.case !== "conversationPage") throw new Error("wrong arm");
    expect(frame.frame.value.continuation).toEqual({ case: "start" });
  });

  it("reads the EMPTY more arm as older history remaining", () => {
    // Arrange — HistoryHasMore carries nothing: "there is more" is a fact the
    // client acts on by calling NextPageCmd, not a handle it stores.
    // Act
    const frame = decodeFrontendFrame(page({ start: undefined, more: {} }));
    // Assert
    if (frame.frame.case !== "conversationPage") throw new Error("wrong arm");
    expect(frame.frame.value.continuation).toEqual({ case: "more" });
  });

  it("carries live_join_seq as a number the store can rank by", () => {
    // Arrange — protojson renders uint64 as a string.
    // Act
    const frame = decodeFrontendFrame(page({ liveJoinSeq: "4096" }));
    // Assert
    if (frame.frame.case !== "conversationPage") throw new Error("wrong arm");
    expect(frame.frame.value.liveJoinSeq).toBe(4096);
  });

  it("refuses a page carrying NEITHER continuation arm", () => {
    // Arrange — it would render a load-more that can never retire.
    // Act / Assert
    expect(() => decodeFrontendFrame(page({ start: undefined }))).toThrow(
      /NEITHER `more` nor `start`/,
    );
  });

  it("refuses a page carrying BOTH continuation arms", () => {
    // Arrange — they are one oneof; both set means the frame is malformed.
    // Act / Assert
    expect(() => decodeFrontendFrame(page({ more: {} }))).toThrow(/both `more` and `start`/);
  });

  it("refuses a more arm carrying a cursor, which is a position this client does not hold", () => {
    // Arrange — HistoryHasMore is empty by construction; a field on it would
    // be exactly the position the contract removed.
    // Act / Assert
    expect(() => decodeFrontendFrame(page({ start: undefined, more: { cursor: "c" } }))).toThrow(
      /unrecognized field/,
    );
  });

  it("refuses a page with no fence, which has nothing to measure staleness against", () => {
    // Arrange / Act / Assert
    expect(() => decodeFrontendFrame(page({ fence: "" }))).toThrow(/missing required `fence`/);
  });

  it("refuses a page with no request_id, which cannot be correlated", () => {
    // Arrange — a client with a cold open and a load-more both in flight has
    // two pages coming, and only the echo tells them apart.
    // Act / Assert
    expect(() => decodeFrontendFrame(page({ requestId: "" }))).toThrow(/missing required `request_id`/);
  });

  it("refuses an unknown field rather than silently ignoring it", () => {
    // Arrange — canonical protojson decoding is strict, and a field this
    // client does not know is a contract it is not speaking.
    // Act / Assert
    expect(() => decodeFrontendFrame(page({ surprise: 1 }))).toThrow(/ConversationPage/);
  });
});
