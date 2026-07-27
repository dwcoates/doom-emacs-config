import { describe, expect, it } from "vitest";
import { partitionFeed } from "../src/partition.js";
import { ConversationItem, ToolItem } from "../src/store.js";

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

  it("folds a TaskUpdate onto the create card whose result named its task", () => {
    // Arrange — a settled create reporting #1, then an update to task 1.
    const create = {
      ...tool("c1", "TaskCreate"),
      result: { isError: false, content: "Task #1 created successfully: wire" },
    };
    const update = { ...tool("u1", "TaskUpdate"), input: { taskId: "1" } };
    // Act
    const part = partitionFeed([create, update]);
    // Assert
    expect(part.children.get("c1")).toEqual([update]);
  });

  it("keeps an update top-level when no create reported its task id", () => {
    // Arrange — the create settled as #1, but the update names task 9.
    const create = {
      ...tool("c1", "TaskCreate"),
      result: { isError: false, content: "Task #1 created successfully: wire" },
    };
    const update = { ...tool("u1", "TaskUpdate"), input: { taskId: "9" } };
    // Act
    const part = partitionFeed([create, update]);
    // Assert
    expect(part.top).toContain(update);
  });

  it("keeps an update top-level while its create's result is pending", () => {
    // Arrange — an unsettled create has reported no harness id yet.
    const update = { ...tool("u1", "TaskUpdate"), input: { taskId: "1" } };
    // Act
    const part = partitionFeed([tool("c1", "TaskCreate"), update]);
    // Assert
    expect(part.top).toContain(update);
  });

  it("hands a reused task id to its most recent owner", () => {
    // Arrange — two creates both settled as #1 (a reset task counter);
    // the update belongs to the newer claimant, whose calls still arrive.
    const older = {
      ...tool("c1", "TaskCreate"),
      result: { isError: false, content: "Task #1 created successfully: old" },
    };
    const newer = {
      ...tool("c2", "TaskCreate"),
      result: { isError: false, content: "Task #1 created successfully: new" },
    };
    const update = { ...tool("u1", "TaskUpdate"), input: { taskId: "1" } };
    // Act
    const part = partitionFeed([older, newer, update]);
    // Assert
    expect(part.children.get("c2")).toEqual([update]);
  });

  it("folds each task's updates onto its own create", () => {
    // Arrange — two settled creates, one update each.
    const create1 = {
      ...tool("c1", "TaskCreate"),
      result: { isError: false, content: "Task #1 created successfully: one" },
    };
    const create2 = {
      ...tool("c2", "TaskCreate"),
      result: { isError: false, content: "Task #2 created successfully: two" },
    };
    const update1 = { ...tool("u1", "TaskUpdate"), input: { taskId: "1" } };
    const update2 = { ...tool("u2", "TaskUpdate"), input: { taskId: "2" } };
    // Act
    const part = partitionFeed([create1, create2, update1, update2]);
    // Assert
    expect(part.children.get("c1")).toEqual([update1]);
    expect(part.children.get("c2")).toEqual([update2]);
  });

  it("folds a TaskOutput poll onto the card whose result announced the task id", () => {
    // Arrange — a backgrounded Bash announcing bg1, then its poll.
    const spawn = {
      ...tool("t1"),
      result: { isError: false, content: "Command running in background with ID: bg1. Output is being written to: /tmp/claude-1/s/tasks/bg1.output" },
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
