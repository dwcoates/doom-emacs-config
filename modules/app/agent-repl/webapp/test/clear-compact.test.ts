import { describe, expect, it } from "vitest";

import {
  ContextClearedItem,
  ContextCompactedItem,
  ConversationItem,
  ToolItem,
} from "../src/store.js";
import {
  clearOrCompactKey,
  isClearOrCompact,
  itemsFromClearOrCompact,
  itemsSinceClearOrCompact,
} from "../src/clear-compact.js";

/** A context clear at UUID. */
function clear(uuid = "c1"): ContextClearedItem {
  return { kind: "context-cleared", uuid };
}

/** A compaction at UUID, defaulted to a successful automatic one. */
function compact(over: Partial<ContextCompactedItem> = {}): ContextCompactedItem {
  return {
    kind: "context-compacted",
    uuid: "k1",
    trigger: "auto",
    preTokens: 120_000,
    postTokens: 8_000,
    durationMs: 4_200,
    summary: "the story so far",
    result: "success",
    error: "",
    ...over,
  };
}

/** A settled tool call, so the survivors are identifiable by id. */
function tool(toolUseId: string): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
    toolUseId,
    toolName: "Bash",
    messageId: "m1",
    inputJson: "",
    input: {},
    inputDone: true,
  };
}

describe("isClearOrCompact", () => {
  it("reads a clear as a boundary", () => {
    // Arrange + Act + Assert
    expect(isClearOrCompact(clear())).toBe(true);
  });

  it("reads a compaction as a boundary", () => {
    // Arrange + Act + Assert
    expect(isClearOrCompact(compact())).toBe(true);
  });

  it("reads an ordinary item as neither", () => {
    // Arrange + Act + Assert
    expect(isClearOrCompact(tool("t1"))).toBe(false);
  });

  it("reads a prompt that merely says /clear as neither", () => {
    // Arrange — the string match this replaced would have fired here.
    const item: ConversationItem = {
      kind: "user-turn",
      requestId: "r1",
      content: [{ type: "text", text: "/clear" }],
      ts: "2026-05-24T09:05:00Z",
    };
    // Act + Assert
    expect(isClearOrCompact(item)).toBe(false);
  });
});

describe("itemsSinceClearOrCompact", () => {
  it("keeps every item of a session that neither cleared nor compacted", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), tool("t2")];
    // Act + Assert
    expect(itemsSinceClearOrCompact(items)).toEqual(items);
  });

  it("drops the items a clear discarded", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), clear(), tool("t2")];
    // Act
    const kept = itemsSinceClearOrCompact(items) as ToolItem[];
    // Assert
    expect(kept.map((i) => i.toolUseId)).toEqual(["t2"]);
  });

  it("drops the items a compaction discarded", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), compact(), tool("t2")];
    // Act
    const kept = itemsSinceClearOrCompact(items) as ToolItem[];
    // Assert
    expect(kept.map((i) => i.toolUseId)).toEqual(["t2"]);
  });

  it("drops the clear itself, which is chrome rather than context", () => {
    // Arrange + Act + Assert
    expect(itemsSinceClearOrCompact([clear()])).toEqual([]);
  });

  it("starts at the LAST of the two when both happened", () => {
    // Arrange
    const items: ConversationItem[] = [clear("c1"), tool("t1"), compact({ uuid: "k1" }), tool("t2")];
    // Act
    const kept = itemsSinceClearOrCompact(items) as ToolItem[];
    // Assert
    expect(kept.map((i) => i.toolUseId)).toEqual(["t2"]);
  });

  it("keeps the items a system init left standing, since a resume re-inits too", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), { kind: "system", subtype: "init" }];
    // Act + Assert
    expect(itemsSinceClearOrCompact(items)).toEqual(items);
  });

  it("leaves the source items untouched", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), clear(), tool("t2")];
    // Act
    itemsSinceClearOrCompact(items);
    // Assert
    expect(items).toHaveLength(3);
  });
});

describe("itemsFromClearOrCompact", () => {
  it("keeps every item of a session that neither cleared nor compacted", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), tool("t2")];
    // Act + Assert
    expect(itemsFromClearOrCompact(items)).toEqual(items);
  });

  it("drops the items above a clear", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), clear(), tool("t2")];
    // Act + Assert
    expect(itemsFromClearOrCompact(items)).toEqual([items[1], items[2]]);
  });

  it("drops the items above a compaction exactly as a clear does", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), compact(), tool("t2")];
    // Act + Assert
    expect(itemsFromClearOrCompact(items)).toEqual([items[1], items[2]]);
  });

  it("keeps the clear itself as the head of the sliced feed", () => {
    // Arrange
    const cleared = clear();
    const items: ConversationItem[] = [tool("t1"), cleared];
    // Act + Assert
    expect(itemsFromClearOrCompact(items)[0]).toBe(cleared);
  });

  it("starts at the LAST of the two when both happened", () => {
    // Arrange — a clear then a compaction; the later one wins.
    const items: ConversationItem[] = [clear("c1"), tool("t1"), compact({ uuid: "k1" }), tool("t2")];
    // Act + Assert
    expect(itemsFromClearOrCompact(items)).toEqual([items[2], items[3]]);
  });

  it("starts at the later of two compactions", () => {
    // Arrange
    const items: ConversationItem[] = [
      compact({ uuid: "k1" }),
      tool("t1"),
      compact({ uuid: "k2" }),
    ];
    // Act + Assert
    expect(itemsFromClearOrCompact(items)).toEqual([items[2]]);
  });

  it("leaves the source items untouched", () => {
    // Arrange
    const items: ConversationItem[] = [tool("t1"), clear(), tool("t2")];
    // Act
    itemsFromClearOrCompact(items);
    // Assert
    expect(items).toHaveLength(3);
  });
});

describe("clearOrCompactKey", () => {
  it("names the clear a truncated feed opens on", () => {
    // Arrange — the sliced list, as itemsFromClearOrCompact hands the renderer.
    const items: ConversationItem[] = [clear("c7"), tool("t1")];
    // Act + Assert
    expect(clearOrCompactKey(items)).toBe("context-cleared:c7");
  });

  it("names the compaction a truncated feed opens on", () => {
    // Arrange
    const items: ConversationItem[] = [compact({ uuid: "k7" }), tool("t1")];
    // Act + Assert
    expect(clearOrCompactKey(items)).toBe("context-compacted:k7");
  });

  it("reports null for a feed that neither cleared nor compacted", () => {
    // Arrange + Act + Assert
    expect(clearOrCompactKey([tool("t1")])).toBeNull();
  });

  it("reports null for an empty feed", () => {
    // Arrange + Act + Assert
    expect(clearOrCompactKey([])).toBeNull();
  });

  it("reports null when neither heads the feed", () => {
    // Arrange — a clear that is not the head did not produce this slice.
    const items: ConversationItem[] = [tool("t1"), clear()];
    // Act + Assert
    expect(clearOrCompactKey(items)).toBeNull();
  });

  it("distinguishes two clears by their uuids", () => {
    // Arrange — the second clear must read as a new boundary, or the renderer
    // would reconcile stale nodes across it instead of rebuilding.
    const first = clearOrCompactKey([clear("c1")]);
    // Act
    const second = clearOrCompactKey([clear("c2")]);
    // Assert
    expect(second).not.toBe(first);
  });

  it("distinguishes a clear from a compaction sharing one uuid", () => {
    // Arrange + Act + Assert
    expect(clearOrCompactKey([clear("x")])).not.toBe(clearOrCompactKey([compact({ uuid: "x" })]));
  });
});
