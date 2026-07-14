import { describe, expect, it } from "vitest";

import {
  TASK_TOOLS,
  sessionTasks,
  taskIdFromCreateResult,
  tasksMenuHtml,
} from "../src/tasks.js";
import { CounterEntry } from "../src/counter-menu.js";
import { ConversationItem, ToolItem, UserTurnItem } from "../src/store.js";

/** A settled TaskCreate call naming task #1. */
function taskCreate(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
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
    deactivatedAtTurn: null,
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

describe("sessionTasks", () => {
  it("derives one entry per created task", () => {
    // Arrange + Act
    const tasks = sessionTasks([taskCreate()]);
    // Assert
    expect(tasks).toHaveLength(1);
  });

  it("names the task by its create subject", () => {
    // Arrange + Act
    const [task] = sessionTasks([taskCreate()]);
    // Assert
    expect(task.summary).toBe("wire the counter");
  });

  it("reads a freshly created task as pending (starting)", () => {
    // Arrange + Act
    const [task] = sessionTasks([taskCreate()]);
    // Assert
    expect(task.status).toBe("starting");
  });

  it("reads an in-progress task as running", () => {
    // Arrange + Act
    const [task] = sessionTasks([taskCreate(), taskUpdate()]);
    // Assert
    expect(task.status).toBe("running");
  });

  it("reads a completed task as done", () => {
    // Arrange + Act
    const [task] = sessionTasks([taskCreate(), taskUpdate({ input: { taskId: "1", status: "completed" } })]);
    // Assert
    expect(task.status).toBe("done");
  });

  it("drops a deleted task entirely", () => {
    // Arrange + Act
    const tasks = sessionTasks([taskCreate(), taskUpdate({ input: { taskId: "1", status: "deleted" } })]);
    // Assert
    expect(tasks).toHaveLength(0);
  });

  it("correlates an update to its create by the result id", () => {
    // Arrange — the update's taskId matches the id parsed from the create result.
    const tasks = sessionTasks([taskCreate(), taskUpdate({ input: { taskId: "1", status: "completed" } })]);
    // Act + Assert — one folded row, not two stranded ones.
    expect(tasks).toHaveLength(1);
  });

  it("renames a task when an update carries a new subject", () => {
    // Arrange + Act
    const [task] = sessionTasks([
      taskCreate(),
      taskUpdate({ input: { taskId: "1", subject: "rewire the counter" } }),
    ]);
    // Assert
    expect(task.summary).toBe("rewire the counter");
  });

  it("stamps the completion turn from the counted clock", () => {
    // Arrange — one counted turn precedes the completing update.
    const items: ConversationItem[] = [
      userTurn("go"),
      taskCreate(),
      taskUpdate({ input: { taskId: "1", status: "completed" } }),
    ];
    // Act
    const [task] = sessionTasks(items);
    // Assert
    expect(task.deactivatedAtTurn).toBe(1);
  });

  it("clears the deactivation stamp when a task reopens", () => {
    // Arrange — completed then pushed back to in_progress.
    const items: ConversationItem[] = [
      taskCreate(),
      taskUpdate({ input: { taskId: "1", status: "completed" } }),
      taskUpdate({ input: { taskId: "1", status: "in_progress" } }),
    ];
    // Act
    const [task] = sessionTasks(items);
    // Assert
    expect(task.deactivatedAtTurn).toBeNull();
  });

  it("keeps tasks in creation order", () => {
    // Arrange
    const items: ConversationItem[] = [
      taskCreate({ toolUseId: "c1", result: { isError: false, content: "Task #1 created successfully: first" } }),
      taskCreate({ toolUseId: "c2", input: { subject: "second", description: "d" }, result: { isError: false, content: "Task #2 created successfully: second" } }),
    ];
    // Act
    const tasks = sessionTasks(items);
    // Assert
    expect(tasks.map((t) => t.summary)).toEqual(["wire the counter", "second"]);
  });

  it("ignores non-task tool calls", () => {
    // Arrange
    const items: ConversationItem[] = [taskCreate({ toolName: "Bash" })];
    // Act + Assert
    expect(sessionTasks(items)).toHaveLength(0);
  });

  it("ignores non-tool items", () => {
    // Arrange + Act + Assert
    expect(sessionTasks([userTurn("hi")])).toHaveLength(0);
  });

  it("carries no detail chip for a task", () => {
    // Arrange + Act
    const [task] = sessionTasks([taskCreate()]);
    // Assert
    expect(task.detail).toBe("");
  });

  it("never nests a task row", () => {
    // Arrange + Act
    const [task] = sessionTasks([taskCreate()]);
    // Assert
    expect(task.nested).toBe(false);
  });
});

describe("tasksMenuHtml", () => {
  it("labels the chip with the task count", () => {
    // Arrange + Act
    const html = tasksMenuHtml([entry()], false, 0);
    // Assert
    expect(html).toContain("1 task");
  });

  it("uses the tasks chip class", () => {
    // Arrange + Act
    const html = tasksMenuHtml([entry()], false, 0);
    // Assert
    expect(html).toContain("info-tasks");
  });

  it("renders nothing when the session created no tasks", () => {
    // Arrange + Act + Assert
    expect(tasksMenuHtml([], false, 0)).toBe("");
  });
});
