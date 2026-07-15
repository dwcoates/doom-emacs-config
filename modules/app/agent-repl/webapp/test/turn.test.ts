import { describe, expect, it } from "vitest";

import { META_CLOSE, META_OPEN } from "../src/meta.js";
import { ConversationItem, ToolItem, UserTurnItem } from "../src/store.js";
import { isClearTurn, itemsFromLastClear, itemsSinceClear, userTurnText } from "../src/turn.js";

/** A user turn carrying the given prompt text. */
function userTurn(text: string): UserTurnItem {
  return {
    kind: "user-turn",
    requestId: "r1",
    content: [{ type: "text", text }],
    ts: "2026-05-24T09:05:00Z",
  };
}

/** A settled tool call, named so a roster-style filter can pick it out. */
function tool(toolUseId: string): ToolItem {
  return {
    kind: "tool",
    toolUseId,
    toolName: "Agent",
    messageId: "m1",
    inputJson: "",
    input: { description: "hunt the flake" },
    inputDone: true,
    result: { isError: false, content: "found it" },
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

describe("isClearTurn", () => {
  it("spots the /clear prompt", () => {
    // Arrange + Act + Assert
    expect(isClearTurn(userTurn("/clear"))).toBe(true);
  });

  it("spots a /clear prompt padded with whitespace", () => {
    // Arrange + Act + Assert
    expect(isClearTurn(userTurn("  /clear\n"))).toBe(true);
  });

  it("spots a /clear the host wrapped in a meta span", () => {
    // Arrange
    const item = userTurn(`${META_OPEN}read the metaprompt${META_CLOSE}/clear`);
    // Act + Assert
    expect(isClearTurn(item)).toBe(true);
  });

  it("rejects a prompt that merely mentions /clear", () => {
    // Arrange + Act + Assert
    expect(isClearTurn(userTurn("run /clear when you are done"))).toBe(false);
  });

  it("rejects an ordinary prompt", () => {
    // Arrange + Act + Assert
    expect(isClearTurn(userTurn("do the thing"))).toBe(false);
  });
});

describe("itemsSinceClear", () => {
  it("keeps every item of a session that never cleared", () => {
    // Arrange
    const items: ConversationItem[] = [userTurn("do the thing"), tool("t1")];
    // Act + Assert
    expect(itemsSinceClear(items)).toEqual(items);
  });

  it("drops the items the /clear discarded", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), userTurn("/clear"), tool("t2")];
    // Act
    const kept = itemsSinceClear(items) as ToolItem[];
    // Assert
    expect(kept.map((i) => i.toolUseId)).toEqual(["t2"]);
  });

  it("drops the /clear turn itself", () => {
    // Arrange
    const items: ConversationItem[] = [userTurn("/clear")];
    // Act + Assert
    expect(itemsSinceClear(items)).toEqual([]);
  });

  it("cuts at the LAST /clear when the session cleared twice", () => {
    // Arrange
    const items: ConversationItem[] = [
      userTurn("/clear"),
      tool("t1"),
      userTurn("/clear"),
      tool("t2"),
    ];
    // Act
    const kept = itemsSinceClear(items) as ToolItem[];
    // Assert
    expect(kept.map((i) => i.toolUseId)).toEqual(["t2"]);
  });

  it("keeps the items a system init left standing, since a resume re-inits too", () => {
    // Arrange -- an init lands after the replayed history of a RESUMED session,
    // whose context still carries that history.  Only a /clear cuts.
    const items: ConversationItem[] = [tool("t1"), { kind: "system", subtype: "init" }];
    // Act + Assert
    expect(itemsSinceClear(items)).toEqual(items);
  });

  it("leaves the source items untouched", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), userTurn("/clear"), tool("t2")];
    // Act
    itemsSinceClear(items);
    // Assert
    expect(items).toHaveLength(3);
  });
});

describe("itemsFromLastClear", () => {
  it("keeps every item of a session that never cleared", () => {
    // Arrange
    const items: ConversationItem[] = [userTurn("do the thing"), tool("t1")];
    // Act + Assert
    expect(itemsFromLastClear(items)).toEqual(items);
  });

  it("drops the items before the /clear", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), userTurn("/clear"), tool("t2")];
    // Act
    const kept = itemsFromLastClear(items);
    // Assert
    expect(kept).toEqual([items[1], items[2]]);
  });

  it("keeps the /clear turn itself as the head of the cut", () => {
    // Arrange
    const clear = userTurn("/clear");
    const items: ConversationItem[] = [tool("t1"), clear];
    // Act + Assert
    expect(itemsFromLastClear(items)[0]).toBe(clear);
  });

  it("cuts at the LAST /clear when the session cleared twice", () => {
    // Arrange
    const items: ConversationItem[] = [
      userTurn("/clear"),
      tool("t1"),
      userTurn("/clear"),
      tool("t2"),
    ];
    // Act + Assert
    expect(itemsFromLastClear(items)).toEqual([items[2], items[3]]);
  });

  it("keeps the items a system init left standing, since a resume re-inits too", () => {
    // Arrange -- only a typed /clear cuts the feed; a resume's init does not.
    const items: ConversationItem[] = [tool("t1"), { kind: "system", subtype: "init" }];
    // Act + Assert
    expect(itemsFromLastClear(items)).toEqual(items);
  });

  it("leaves the source items untouched", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), userTurn("/clear"), tool("t2")];
    // Act
    itemsFromLastClear(items);
    // Assert
    expect(items).toHaveLength(3);
  });
});
