import { describe, expect, it } from "vitest";
import { ConversationItem, ToolItem } from "../src/store.js";
import { isWatcher, watchersByBubble } from "../src/watchers.js";

function userTurn(requestId = "u1"): ConversationItem {
  return {
    kind: "user-turn",
    requestId,
    content: [{ type: "text", text: "go" }],
    ts: "2026-05-24T10:00:00.000Z",
  } as unknown as ConversationItem;
}

function text(blockId: string, parent?: string): ConversationItem {
  return {
    kind: "text",
    blockId,
    messageId: "m1",
    parentToolUseId: parent,
    text: "done",
    done: true,
    ts: "2026-05-24T10:00:00.000Z",
  };
}

function tool(id: string, name = "Bash", parent?: string): ToolItem {
  return {
    kind: "tool",
    toolUseId: id,
    messageId: "m1",
    toolName: name,
    parentToolUseId: parent,
    inputJson: "{}",
    input: {},
    inputDone: true,
  };
}

/** A backgrounded-Bash spawner announcing its id in its result text. */
function bgSpawner(id: string, taskId: string): ToolItem {
  return {
    ...tool(id),
    result: { isError: false, content: `Command running in background with ID: ${taskId}.` },
  };
}

function result(subtype = "success"): ConversationItem {
  return { kind: "result", subtype } as unknown as ConversationItem;
}

describe("isWatcher", () => {
  it("is true for a tool that announced a background id", () => {
    // Arrange / Act / Assert
    expect(isWatcher(bgSpawner("t1", "bg1"))).toBe(true);
  });

  it("is false for a plain tool that spawned nothing", () => {
    // Arrange / Act / Assert
    expect(isWatcher(tool("t1"))).toBe(false);
  });

  it("is false for a non-tool item", () => {
    // Arrange / Act / Assert
    expect(isWatcher(text("b1"))).toBe(false);
  });
});

describe("watchersByBubble", () => {
  it("maps a final bubble to a backgrounded-Bash watcher armed in its turn", () => {
    // Arrange
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn(), watcher, text("b1"), result()];
    // Act
    const byBubble = watchersByBubble(items);
    // Assert
    expect(byBubble.get("b1")).toEqual([watcher]);
  });

  it("maps a background-agent watcher named by agentId in its result", () => {
    // Arrange
    const agent: ToolItem = {
      ...tool("t1", "Agent"),
      result: { isError: false, content: "Launched. agentId: abc1" },
    };
    const items = [userTurn(), agent, text("b1"), result()];
    // Act
    const byBubble = watchersByBubble(items);
    // Assert
    expect(byBubble.get("b1")).toEqual([agent]);
  });

  it("maps a watcher whose id came only from its landed notification", () => {
    // Arrange — no id in the result text, the notification is authoritative.
    const watcher: ToolItem = {
      ...tool("t1", "Agent"),
      notification: { taskId: "abc1", text: "<task-notification/>" },
    };
    const items = [userTurn(), watcher, text("b1"), result()];
    // Act
    const byBubble = watchersByBubble(items);
    // Assert
    expect(byBubble.get("b1")).toEqual([watcher]);
  });

  it("omits a bubble whose turn armed no watcher", () => {
    // Arrange
    const items = [userTurn(), tool("t1"), text("b1"), result()];
    // Act
    const byBubble = watchersByBubble(items);
    // Assert
    expect(byBubble.size).toBe(0);
  });

  it("omits a turn that did not end in success", () => {
    // Arrange — an aborted turn's last text is a severed thought, not an answer.
    const items = [userTurn(), bgSpawner("t1", "bg1"), text("b1"), result("aborted")];
    // Act
    const byBubble = watchersByBubble(items);
    // Assert
    expect(byBubble.size).toBe(0);
  });

  it("keys the LAST main-chain text before the result, not an earlier one", () => {
    // Arrange — commentary then the real answer.
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn(), text("commentary"), watcher, text("answer"), result()];
    // Act
    const byBubble = watchersByBubble(items);
    // Assert
    expect(byBubble.has("commentary")).toBe(false);
    expect(byBubble.get("answer")).toEqual([watcher]);
  });

  it("never keys a subagent's parented prose as the final response", () => {
    // Arrange — the parented text is commentary inside a card, not the answer.
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn(), watcher, text("nested", "a1"), text("answer"), result()];
    // Act
    const byBubble = watchersByBubble(items);
    // Assert
    expect(byBubble.has("nested")).toBe(false);
    expect(byBubble.get("answer")).toEqual([watcher]);
  });

  it("scopes each bubble to its own turn's watchers, not a prior turn's", () => {
    // Arrange — two turns, one watcher each.
    const first = bgSpawner("t1", "bg1");
    const second = bgSpawner("t2", "bg2");
    const items = [
      userTurn("u1"),
      first,
      text("a1"),
      result(),
      userTurn("u2"),
      second,
      text("a2"),
      result(),
    ];
    // Act
    const byBubble = watchersByBubble(items);
    // Assert
    expect(byBubble.get("a1")).toEqual([first]);
    expect(byBubble.get("a2")).toEqual([second]);
  });
});
