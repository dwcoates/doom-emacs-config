import { describe, expect, it } from "vitest";

import { META_CLOSE, META_OPEN } from "../src/meta.js";
import { UserTurnItem } from "../src/store.js";
import { userTurnText } from "../src/turn.js";

/** A user turn carrying the given prompt text. */
function userTurn(text: string): UserTurnItem {
  return {
    kind: "user-turn",
    requestId: "r1",
    content: [{ type: "text", text }],
    ts: "2026-05-24T09:05:00Z",
  };
}

describe("userTurnText", () => {
  it("reads the turn's own prompt text", () => {
    // Arrange + Act + Assert
    expect(userTurnText(userTurn("fix the bug"))).toBe("fix the bug");
  });

  it("drops the host's injected meta spans", () => {
    // Arrange
    const item = userTurn(`${META_OPEN}read the metaprompt${META_CLOSE}fix the bug`);
    // Act + Assert
    expect(userTurnText(item)).toBe("fix the bug");
  });

  it("stands a non-text block in as its kind", () => {
    // Arrange
    const item: UserTurnItem = {
      kind: "user-turn",
      requestId: "r1",
      content: [{ type: "image" } as unknown as { type: string }],
      ts: "2026-05-24T09:05:00Z",
    };
    // Act + Assert
    expect(userTurnText(item)).toBe("[image]");
  });
});
