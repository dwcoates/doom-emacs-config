import { describe, expect, it } from "vitest";
import { partitionFeed } from "../src/partition.js";
import { ConversationItem } from "../src/store.js";

function tool(id: string, name = "Bash", parent?: string): ConversationItem {
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

function text(blockId: string, parent?: string): ConversationItem {
  return {
    kind: "text",
    blockId,
    messageId: "m1",
    parentToolUseId: parent,
    text: "hi",
    done: true,
    ts: "2026-05-24T10:00:00.000Z",
  };
}

function thinking(blockId: string, parent?: string): ConversationItem {
  return {
    kind: "thinking",
    blockId,
    messageId: "m1",
    parentToolUseId: parent,
    text: "hmm",
    done: true,
  };
}

function permission(requestId: string, toolUseId: string): ConversationItem {
  return {
    kind: "permission",
    requestId,
    toolUseId,
    toolName: "Bash",
    input: {},
  };
}

describe("partitionFeed", () => {
  it("keeps a parentless feed entirely top-level", () => {
    // Arrange
    const items = [text("b1"), tool("t1")];
    // Act
    const part = partitionFeed(items);
    // Assert
    expect(part.top).toEqual(items);
    expect(part.children.size).toBe(0);
  });

  it("moves a nested tool call into its spawner's child list", () => {
    // Arrange
    const agent = tool("a1", "Agent");
    const child = tool("t2", "Grep", "a1");
    // Act
    const part = partitionFeed([agent, child]);
    // Assert
    expect(part.top).toEqual([agent]);
    expect(part.children.get("a1")).toEqual([child]);
  });

  it("moves a subagent's text block into the child list", () => {
    // Arrange
    const agent = tool("a1", "Agent");
    const prose = text("b1", "a1");
    // Act
    const part = partitionFeed([agent, prose]);
    // Assert
    expect(part.children.get("a1")).toEqual([prose]);
  });

  it("moves a subagent's thinking block into the child list", () => {
    // Arrange
    const agent = tool("a1", "Agent");
    const thought = thinking("b1", "a1");
    // Act
    const part = partitionFeed([agent, thought]);
    // Assert
    expect(part.children.get("a1")).toEqual([thought]);
  });

  it("keeps an orphan child top-level rather than dropping it", () => {
    // Arrange — the spawning card fell out of the ring.
    const orphan = tool("t2", "Grep", "a-gone");
    // Act
    const part = partitionFeed([orphan]);
    // Assert
    expect(part.top).toEqual([orphan]);
    expect(part.children.size).toBe(0);
  });

  it("lands a nested call's permission prompt in the spawning card's list", () => {
    // Arrange — the prompt gates t2, which a1 spawned.
    const agent = tool("a1", "Agent");
    const child = tool("t2", "Bash", "a1");
    const prompt = permission("p1", "t2");
    // Act
    const part = partitionFeed([agent, child, prompt]);
    // Assert
    expect(part.children.get("a1")).toEqual([child, prompt]);
  });

  it("keeps a main-chain permission prompt top-level", () => {
    // Arrange — the gated tool has no parent.
    const call = tool("t1");
    const prompt = permission("p1", "t1");
    // Act
    const part = partitionFeed([call, prompt]);
    // Assert
    expect(part.top).toEqual([call, prompt]);
  });

  it("nests an agent's own subagent one level down, not two", () => {
    // Arrange — a1 spawns a2, a2 runs t3.
    const outer = tool("a1", "Agent");
    const inner = tool("a2", "Agent", "a1");
    const leaf = tool("t3", "Read", "a2");
    // Act
    const part = partitionFeed([outer, inner, leaf]);
    // Assert — each child keys by its DIRECT parent.
    expect(part.top).toEqual([outer]);
    expect(part.children.get("a1")).toEqual([inner]);
    expect(part.children.get("a2")).toEqual([leaf]);
  });

  it("nests children under a non-agent parent identically", () => {
    // Arrange — a Workflow's spawned agents carry its tool_use_id.
    const wf = tool("w1", "Workflow");
    const child = tool("t2", "Grep", "w1");
    // Act
    const part = partitionFeed([wf, child]);
    // Assert
    expect(part.children.get("w1")).toEqual([child]);
  });

  it("folds a TaskOutput poll onto the card whose result announced the task id", () => {
    // Arrange — a backgrounded Bash announcing bg1, then its poll.
    const spawn = {
      ...tool("t1"),
      result: { isError: false, content: "Command running in background with ID: bg1." },
    } as ConversationItem;
    const poll = { ...tool("t2", "TaskOutput"), input: { task_id: "bg1" } } as ConversationItem;
    // Act
    const part = partitionFeed([spawn, poll]);
    // Assert
    expect(part.top).toEqual([spawn]);
    expect(part.children.get("t1")).toEqual([poll]);
  });

  it("folds a TaskStop onto the spawner named by its landed notification", () => {
    // Arrange — the notification is the authoritative id source.
    const spawn = {
      ...tool("t1", "Agent"),
      notification: { taskId: "abc1", text: "<task-notification/>" },
    } as ConversationItem;
    const stop = { ...tool("t2", "TaskStop"), input: { task_id: "abc1" } } as ConversationItem;
    // Act
    const part = partitionFeed([spawn, stop]);
    // Assert
    expect(part.children.get("t1")).toEqual([stop]);
  });

  it("keeps a poll of an unknown task id top-level", () => {
    // Arrange
    const poll = { ...tool("t2", "TaskOutput"), input: { task_id: "bg-gone" } } as ConversationItem;
    // Act
    const part = partitionFeed([poll]);
    // Assert
    expect(part.top).toEqual([poll]);
  });
});
