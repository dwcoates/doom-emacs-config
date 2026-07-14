import { describe, expect, it } from "vitest";

import {
  NON_SKILL_SLASH_COMMANDS,
  countedTurns,
  isCountedTurn,
} from "../src/turn-clock.js";
import { ConversationItem, UserTurnItem } from "../src/store.js";

/** A user turn carrying one text block. */
function userTurn(text: string, over: Partial<UserTurnItem> = {}): UserTurnItem {
  return {
    kind: "user-turn",
    requestId: "r1",
    content: [{ type: "text", text }],
    ts: "2026-05-24T09:00:00Z",
    ...over,
  };
}

describe("NON_SKILL_SLASH_COMMANDS", () => {
  it("lists a representative built-in command", () => {
    // Arrange + Act + Assert
    expect(NON_SKILL_SLASH_COMMANDS.has("clear")).toBe(true);
  });
});

describe("isCountedTurn", () => {
  it("counts an ordinary prompt", () => {
    // Arrange + Act + Assert
    expect(isCountedTurn(userTurn("fix the bug"))).toBe(true);
  });

  it("skips a bare built-in slash command", () => {
    // Arrange + Act + Assert
    expect(isCountedTurn(userTurn("/clear"))).toBe(false);
  });

  it("skips the bare /model command", () => {
    // Arrange + Act + Assert
    expect(isCountedTurn(userTurn("/model"))).toBe(false);
  });

  it("counts a skill invocation not on the built-in denylist", () => {
    // Arrange + Act + Assert
    expect(isCountedTurn(userTurn("/analyze-position"))).toBe(true);
  });

  it("counts a slash command carrying trailing prose as a real prompt", () => {
    // Arrange + Act + Assert
    expect(isCountedTurn(userTurn("/model give me opus"))).toBe(true);
  });

  it("counts a bare slash command after stripping host-injected spans", () => {
    // Arrange — a meta span precedes the user's own /clear.
    const item = userTurn("<!--agent-repl:meta-->read this<!--/agent-repl:meta-->\n/clear");
    // Act + Assert — stripping leaves just /clear, which is not counted.
    expect(isCountedTurn(item)).toBe(false);
  });
});

describe("countedTurns", () => {
  it("counts the counted user turns in the feed", () => {
    // Arrange
    const items: ConversationItem[] = [userTurn("a"), userTurn("/clear"), userTurn("b")];
    // Act + Assert
    expect(countedTurns(items)).toBe(2);
  });

  it("ignores non-user-turn items", () => {
    // Arrange
    const items: ConversationItem[] = [
      userTurn("a"),
      { kind: "text", blockId: "b", messageId: "m", text: "hi", done: true, ts: "" },
    ];
    // Act + Assert
    expect(countedTurns(items)).toBe(1);
  });

  it("counts only through the given end index", () => {
    // Arrange
    const items: ConversationItem[] = [userTurn("a"), userTurn("b"), userTurn("c")];
    // Act + Assert — through index 1 there are two turns.
    expect(countedTurns(items, 1)).toBe(2);
  });

  it("is zero for an empty feed", () => {
    // Arrange + Act + Assert
    expect(countedTurns([])).toBe(0);
  });
});
