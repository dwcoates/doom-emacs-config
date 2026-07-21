import { describe, expect, it } from "vitest";
import { ConversationItem, ToolItem } from "../src/store.js";
import { asyncByBubble, isWatcher, watcherRef } from "../src/watchers.js";

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
    ts: "2026-05-24T10:00:00.000Z",
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
    result: { isError: false, content: `Command running in background with ID: ${taskId}. Output is being written to: /tmp/claude-1/s/tasks/${taskId}.output` },
  };
}

function result(subtype = "success"): ConversationItem {
  return { kind: "result", subtype } as unknown as ConversationItem;
}

describe("watcherRef", () => {
  it("keys off the classification first, as the most structural tier", () => {
    // Arrange — classified AND announced: the classification wins.
    const item: ToolItem = {
      ...bgSpawner("t1", "bg1"),
      asyncSource: { source_id: "src-1", kind: "shell", status: "running" },
    };
    // Act / Assert
    expect(watcherRef(item)).toEqual({ id: "src-1", origin: "classified" });
  });

  it("falls back to a landed notification's task id", () => {
    // Arrange — no classification, but the harness correlated a completion.
    const item: ToolItem = {
      ...tool("t1"),
      notification: { taskId: "bg9", text: "<task-notification/>" },
    };
    // Act / Assert
    expect(watcherRef(item)).toEqual({ id: "bg9", origin: "notification" });
  });

  it("accepts a PAIRED prose announcement as the last tier", () => {
    // Arrange — pre-structured history: id next to its spool path.
    const item = bgSpawner("t1", "bg1");
    // Act / Assert
    expect(watcherRef(item)).toEqual({ id: "bg1", origin: "announced" });
  });

  it("refuses a sync agent's bare completion handle — the phantom-member regression", () => {
    // Arrange — every finished foreground agent's result carries this
    // handle, which once made the settled call an amber-forever member.
    const item: ToolItem = {
      ...tool("t1", "Agent"),
      result: {
        isError: false,
        content: "agentId: a36ef865012a4672a (use SendMessage with to: 'a36ef865012a4672a')",
      },
    };
    // Act / Assert
    expect(watcherRef(item)).toBeNull();
  });

  it("returns null for a plain call that spawned nothing", () => {
    // Arrange / Act / Assert
    expect(watcherRef(tool("t1"))).toBeNull();
  });
});

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

describe("asyncByBubble", () => {
  it("maps a final bubble to a backgrounded-Bash member armed in its turn", () => {
    // Arrange
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn(), watcher, text("b1"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.get("b1")).toEqual([watcher]);
  });

  it("maps a background-agent member named by agentId in its result", () => {
    // Arrange
    const agent: ToolItem = {
      ...tool("t1", "Agent"),
      result: { isError: false, content: "Async agent launched. agentId: abc1, output_file: /tmp/claude-1/s/tasks/abc1.output" },
    };
    const items = [userTurn(), agent, text("b1"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.get("b1")).toEqual([agent]);
  });

  it("maps a member whose id came only from its landed notification", () => {
    // Arrange — no id in the result text, the notification is authoritative.
    const watcher: ToolItem = {
      ...tool("t1", "Agent"),
      notification: { taskId: "abc1", text: "<task-notification/>" },
    };
    const items = [userTurn(), watcher, text("b1"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.get("b1")).toEqual([watcher]);
  });

  it("omits a bubble whose turn armed no async work", () => {
    // Arrange
    const items = [userTurn(), tool("t1"), text("b1"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.size).toBe(0);
  });

  it("maps an interrupted turn's survivors to its last text, never orphaning them", () => {
    // Arrange — an aborted turn's background work outlives the severed turn.
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn(), watcher, text("b1"), result("aborted")];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.get("b1")).toEqual([watcher]);
  });

  it("hosts a tools-only turn's members on its prompt bubble when no final text exists", () => {
    // Arrange — the turn armed background work but wrote no answer to host it.
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn("u7"), watcher, result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert — the prompt bubble (the user-turn's request id) is the host.
    expect(byBubble.get("u7")).toEqual([watcher]);
  });

  it("keys the LAST main-chain text before the result, not an earlier one", () => {
    // Arrange — commentary then the real answer.
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn(), text("commentary"), watcher, text("answer"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.has("commentary")).toBe(false);
    expect(byBubble.get("answer")).toEqual([watcher]);
  });

  it("never keys a subagent's parented prose as the host bubble", () => {
    // Arrange — the parented text is commentary inside a card, not the answer.
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn(), watcher, text("nested", "a1"), text("answer"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.has("nested")).toBe(false);
    expect(byBubble.get("answer")).toEqual([watcher]);
  });

  it("keys nothing for a still-streaming turn, whose frontier is not yet quiescent", () => {
    // Arrange — a live member armed but no result closing the turn yet.
    const watcher = bgSpawner("t1", "bg1");
    const items = [userTurn(), watcher, text("b1")];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.size).toBe(0);
  });

  it("scopes each bubble to its own turn's members, not a prior turn's", () => {
    // Arrange — two turns, one member each.
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
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.get("a1")).toEqual([first]);
    expect(byBubble.get("a2")).toEqual([second]);
  });
});
