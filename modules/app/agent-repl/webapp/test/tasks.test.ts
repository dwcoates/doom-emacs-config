import { describe, expect, it } from "vitest";

import {
  TASK_TOOLS,
  agentTasks,
  taskCreateToolUseId,
  taskIdFromCreateResult,
  tasksMenuHtml,
} from "../src/tasks.js";
import { CounterEntry } from "../src/counter-menu.js";
import { ConversationItem, ToolItem, UserTurnItem } from "../src/store.js";

/** A settled TaskCreate call naming task #1. */
function taskCreate(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
    toolUseId: "c1",
    toolName: "TaskCreate",
    messageId: "m1",
    inputJson: "",
    input: { subject: "wire the counter", description: "d" },
    inputDone: true,
    result: { isError: false, content: "Task #1 created successfully: wire the counter" },
    ...over,
  };
}

/** A settled TaskUpdate call against task #1. */
function taskUpdate(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
    toolUseId: "u1",
    toolName: "TaskUpdate",
    messageId: "m2",
    inputJson: "",
    input: { taskId: "1", status: "in_progress" },
    inputDone: true,
    ...over,
  };
}

/** A user turn carrying one text block. */
function userTurn(text: string): UserTurnItem {
  return {
    kind: "user-turn",
    requestId: "r1",
    content: [{ type: "text", text }],
    ts: "2026-05-24T09:00:00Z",
  };
}

/** A counter entry, for the render delegation test. */
function entry(over: Partial<CounterEntry> = {}): CounterEntry {
  return {
    id: "1",
    summary: "wire the counter",
    detail: "",
    status: "running",
    nested: false,
    ...over,
  };
}

describe("TASK_TOOLS", () => {
  it("projects from TaskCreate", () => {
    // Arrange + Act + Assert
    expect(TASK_TOOLS.has("TaskCreate")).toBe(true);
  });

  it("projects from TaskUpdate", () => {
    // Arrange + Act + Assert
    expect(TASK_TOOLS.has("TaskUpdate")).toBe(true);
  });
});

describe("taskIdFromCreateResult", () => {
  it("reads the id the harness prints in the create result", () => {
    // Arrange + Act + Assert
    expect(taskIdFromCreateResult(taskCreate())).toBe("1");
  });

  it("is null when the create has not yet produced a result", () => {
    // Arrange + Act + Assert
    expect(taskIdFromCreateResult(taskCreate({ result: undefined }))).toBeNull();
  });

  it("is null when the result names no id", () => {
    // Arrange
    const item = taskCreate({ result: { isError: false, content: "created" } });
    // Act + Assert
    expect(taskIdFromCreateResult(item)).toBeNull();
  });
});

describe("taskCreateToolUseId", () => {
  it("maps a settled create's harness id back to its tool-use id", () => {
    // Arrange + Act + Assert
    expect(taskCreateToolUseId([taskCreate()], "1")).toBe("c1");
  });

  it("maps an unsettled create by its tool-use id fallback", () => {
    // Arrange — no result yet, so the roster row keys off the tool-use id.
    const item = taskCreate({ result: undefined });
    // Act + Assert
    expect(taskCreateToolUseId([item], "c1")).toBe("c1");
  });

  it("is null when no create names the id", () => {
    // Arrange + Act + Assert
    expect(taskCreateToolUseId([taskCreate()], "9")).toBeNull();
  });

  it("never resolves through a TaskUpdate, only through the create", () => {
    // Arrange — an update against task #1 whose create is absent.
    const items = [taskUpdate()];
    // Act + Assert
    expect(taskCreateToolUseId(items, "1")).toBeNull();
  });

  it("resolves each of several creates to its own card", () => {
    // Arrange — a second create carrying task #2.
    const second = taskCreate({
      toolUseId: "c2",
      result: { isError: false, content: "Task #2 created successfully: another" },
    });
    // Act + Assert
    expect(taskCreateToolUseId([taskCreate(), second], "2")).toBe("c2");
  });
});

describe("agentTasks", () => {
  it("keeps only the task calls the given agent itself made", () => {
    // Arrange — one create by agent a1, one main-chain create.
    const items = [
      taskCreate({ toolUseId: "c1", parentToolUseId: "a1" }),
      taskCreate({
        toolUseId: "c2",
        input: { subject: "main-chain task" },
        result: { isError: false, content: "Task #2 created successfully: main-chain task" },
      }),
    ];
    // Act + Assert
    expect(agentTasks(items, "a1").map((t) => t.id)).toEqual(["1"]);
  });

  it("rows a task the agent only updated, never created", () => {
    // Arrange — the harness task list is session-global, so an update by
    // the agent against a main-chain task still counts as the agent's.
    const items = [
      taskCreate(),
      taskUpdate({ parentToolUseId: "a1", input: { taskId: "1", subject: "s", status: "in_progress" } }),
    ];
    // Act
    const rows = agentTasks(items, "a1");
    // Assert
    expect(rows.map((t) => ({ id: t.id, status: t.status }))).toEqual([
      { id: "1", status: "running" },
    ]);
  });

  it("keeps a nested agent's task calls out of another agent's roster", () => {
    // Arrange — a2's create is not a1's.
    const items = [taskCreate({ parentToolUseId: "a2" })];
    // Act + Assert
    expect(agentTasks(items, "a1")).toEqual([]);
  });
});

describe("tasksMenuHtml", () => {
  it("labels the chip with the running-task count", () => {
    // Arrange + Act
    const html = tasksMenuHtml([entry()], false);
    // Assert
    expect(html).toContain("1 task");
  });

  it("uses the tasks chip class", () => {
    // Arrange + Act
    const html = tasksMenuHtml([entry()], false);
    // Assert
    expect(html).toContain("info-tasks");
  });

  it("renders nothing when the session created no tasks", () => {
    // Arrange + Act + Assert
    expect(tasksMenuHtml([], false)).toBe("");
  });

  it("hides a completed task from the roster", () => {
    // Arrange — a done task is no longer active.
    const html = tasksMenuHtml([entry({ status: "done" })], false);
    // Act + Assert — the chip disappears once nothing is active.
    expect(html).toBe("");
  });

  it("counts only the still-active tasks", () => {
    // Arrange — one running, one done.
    const tasks = [entry({ id: "1", status: "running" }), entry({ id: "2", status: "done" })];
    // Act + Assert
    expect(tasksMenuHtml(tasks, false)).toContain("1 task");
  });
});
