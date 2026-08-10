/**
 * async-teal — the derivation that replaced the teal TOOL-NAME LIST.
 *
 * The invariant under test is "a teal card IS an async bubble": the wash comes
 * from the daemon's classification verdict (`spawned_bubble_id`) and from
 * nothing else, so no kind can be painted teal without a bubble behind it.
 */
import { describe, expect, it } from "vitest";

import { ASYNC_BUBBLE_CLASS, asyncBubbleClass, hasAsyncBubble } from "../src/async-teal.js";

describe("hasAsyncBubble", () => {
  it("calls a stamped call an async bubble", () => {
    // Arrange / Act / Assert — a non-empty verdict is the whole of it.
    expect(hasAsyncBubble({ spawnedBubbleId: "bubble:t1" })).toBe(true);
  });

  it("calls an empty verdict no bubble at all", () => {
    // Arrange / Act / Assert — tool-call.proto: empty means "this call
    // detached nothing", and that is the ONLY reading of empty.
    expect(hasAsyncBubble({ spawnedBubbleId: "" })).toBe(false);
  });

  it("calls an absent verdict no bubble at all", () => {
    // Arrange / Act / Assert — an unstamped call is an unclassified one.
    expect(hasAsyncBubble({})).toBe(false);
  });

  it("reads no tool name when deciding", () => {
    // Arrange — the retired list held "Skill"; the name alone must no longer
    // buy the wash, which is the whole point of the derivation.
    const skillWithoutBubble = { toolName: "Skill" } as { spawnedBubbleId?: string };
    // Act / Assert
    expect(hasAsyncBubble(skillWithoutBubble)).toBe(false);
  });
});

describe("asyncBubbleClass", () => {
  it("emits the wash class for a call that detached a bubble", () => {
    // Arrange / Act / Assert — leading space, so it appends to a class list.
    expect(asyncBubbleClass({ spawnedBubbleId: "bubble:t1" })).toBe(` ${ASYNC_BUBBLE_CLASS}`);
  });

  it("emits nothing for a call that detached nothing", () => {
    // Arrange / Act / Assert
    expect(asyncBubbleClass({ spawnedBubbleId: "" })).toBe("");
  });
});
