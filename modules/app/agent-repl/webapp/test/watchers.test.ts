/**
 * watchers — the async projection, on ONE identity tier: the daemon's
 * classification verdict (`AgentToolCall.spawned_bubble_id`).
 *
 * The three-tier identity ladder this module used to walk is gone, so the
 * lower rungs get tests of their own here proving they no longer establish an
 * identity — a notification alone and result prose alone must both resolve to
 * "detached nothing". One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { ConversationItem, ToolItem } from "../src/store.js";
import { asyncByBubble, isWatcher, watcherRef, type AsyncClassification } from "../src/watchers.js";

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

/** A call the DAEMON classified as detaching work, naming the bubble it minted. */
function spawner(id: string, bubbleId: string): ToolItem {
  return { ...tool(id), spawnedBubbleId: bubbleId };
}

function result(subtype = "success"): ConversationItem {
  return { kind: "result", subtype } as unknown as ConversationItem;
}

describe("watcherRef", () => {
  it("reads the daemon's classification verdict, the one tier there is", () => {
    // Arrange / Act / Assert
    expect(watcherRef(spawner("t1", "b1"))).toBe("b1");
  });

  it("returns null for a plain call that spawned nothing", () => {
    // Arrange / Act / Assert
    expect(watcherRef(tool("t1"))).toBeNull();
  });

  it("reads an EMPTY verdict as 'detached nothing', never as a bubble named ''", () => {
    // Arrange / Act / Assert
    expect(watcherRef({ ...tool("t1"), spawnedBubbleId: "" })).toBeNull();
  });

  it("returns null for a non-tool item, which cannot detach anything", () => {
    // Arrange / Act / Assert
    expect(watcherRef(text("b1"))).toBeNull();
  });

  it("no longer accepts a landed notification's task id — that rung is gone", () => {
    // Arrange — the harness correlated a completion, but the daemon published
    // no verdict. Under the old ladder this manufactured an identity.
    const item: ToolItem = { ...tool("t1"), notification: { taskId: "bg9", text: "<task-notification/>" } };

    // Act / Assert
    expect(watcherRef(item)).toBeNull();
  });

  it("no longer accepts a prose announcement — that rung is gone too", () => {
    // Arrange — an id sitting next to its spool path in result text, the old
    // last-resort tier.
    const item: ToolItem = {
      ...tool("t1"),
      result: {
        isError: false,
        content: "Command running in background with ID: bg1. Output is being written to: /tmp/bg1.output",
      },
    };

    // Act / Assert
    expect(watcherRef(item)).toBeNull();
  });

  it("refuses a sync agent's bare completion handle — the phantom-member regression", () => {
    // Arrange — every finished foreground agent's result carries this handle,
    // which once made the settled call an amber-forever member.
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
});

describe("watcherRef against the registry", () => {
  /** A minimal classification surface: the registry's one lookup. */
  function classification(links: Record<string, string>): AsyncClassification {
    return {
      bubbleForCall: (toolUseId) =>
        links[toolUseId] === undefined ? null : ({ id: links[toolUseId] } as never),
    };
  }

  it("recognizes a member the daemon linked by origin_tool_use_id alone", () => {
    // Arrange — no verdict on the call; the bubble names the call instead.
    const item = tool("t1");

    // Act / Assert
    expect(watcherRef(item, classification({ t1: "b1" }))).toBe("b1");
  });

  it("returns null for a call the registry links to nothing", () => {
    // Arrange / Act / Assert
    expect(watcherRef(tool("t1"), classification({}))).toBeNull();
  });
});

describe("isWatcher", () => {
  it("is true for a call the daemon classified as detaching work", () => {
    // Arrange / Act / Assert
    expect(isWatcher(spawner("t1", "b1"))).toBe(true);
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
  it("maps a final bubble to a classified member armed in its turn", () => {
    // Arrange
    const watcher = spawner("t1", "b-1");
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

  it("omits a bubble whose turn only produced an unclassified notification", () => {
    // Arrange — the ladder's middle rung would have hosted this member.
    const noisy: ToolItem = { ...tool("t1"), notification: { taskId: "bg9", text: "<task-notification/>" } };
    const items = [userTurn(), noisy, text("b1"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.size).toBe(0);
  });

  it("maps an interrupted turn's survivors to its last text, never orphaning them", () => {
    // Arrange — an aborted turn's background work outlives the severed turn.
    const watcher = spawner("t1", "b-1");
    const items = [userTurn(), watcher, text("b1"), result("aborted")];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.get("b1")).toEqual([watcher]);
  });

  it("hosts a tools-only turn's members on its prompt bubble when no final text exists", () => {
    // Arrange — the turn armed background work but wrote no answer to host it.
    const watcher = spawner("t1", "b-1");
    const items = [userTurn("u7"), watcher, result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert — the prompt bubble (the user-turn's request id) is the host.
    expect(byBubble.get("u7")).toEqual([watcher]);
  });

  it("keys the LAST main-chain text before the result, not an earlier one", () => {
    // Arrange — commentary then the real answer.
    const watcher = spawner("t1", "b-1");
    const items = [userTurn(), text("commentary"), watcher, text("answer"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.has("commentary")).toBe(false);
    expect(byBubble.get("answer")).toEqual([watcher]);
  });

  it("never keys a subagent's parented prose as the host bubble", () => {
    // Arrange — the parented text is commentary inside a card, not the answer.
    const watcher = spawner("t1", "b-1");
    const items = [userTurn(), watcher, text("nested", "a1"), text("answer"), result()];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.has("nested")).toBe(false);
    expect(byBubble.get("answer")).toEqual([watcher]);
  });

  it("keys nothing for a still-streaming turn, whose frontier is not yet quiescent", () => {
    // Arrange — a live member armed but no result closing the turn yet.
    const watcher = spawner("t1", "b-1");
    const items = [userTurn(), watcher, text("b1")];
    // Act
    const byBubble = asyncByBubble(items);
    // Assert
    expect(byBubble.size).toBe(0);
  });

  it("scopes each bubble to its own turn's members, not a prior turn's", () => {
    // Arrange — two turns, one member each.
    const first = spawner("t1", "b-1");
    const second = spawner("t2", "b-2");
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
