/**
 * One act on the task tracker. `AgentTaskAct` has no lifecycle at all, so both
 * halves of the converter produce the SAME shape and the second replaces the
 * first — except for a create, whose tracker identity does not exist until it
 * returns.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { taskActConverter } from "../../../src/convert/tools/task-act.js";
import { toolResultText } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { toolUseResult } from "./corpus.js";

function call(toolName: string, input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_task",
    toolName,
    input,
    startedAtMs: 4_000,
    agentId: create(conversationv1.AgentIdSchema, { value: "caller" }),
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return {
    content: isError ? toolResultText("refused") : undefined,
    isError,
    structured,
    settledAtMs: 6_000,
  };
}

function actOf(item: conversationv1.AgentActivity["item"]): conversationv1.AgentTaskAct {
  expect(item.case).toBe("taskAct");
  return item.value as conversationv1.AgentTaskAct;
}

describe("taskActConverter.start", () => {
  it("produces NO frame for a create, whose tracker identity does not exist yet", () => {
    // Arrange, Act.
    const item = taskActConverter.start(call("TaskCreate", { subject: "s", description: "d" }));

    // Assert.
    expect(item.case).toBeUndefined();
  });

  it("produces the act an update's input implies, keyed by the task it names", () => {
    // Arrange, Act.
    const act = actOf(taskActConverter.start(call("TaskUpdate", { taskId: "1", status: "in_progress" })));

    // Assert.
    expect([act.task?.value, act.act.case]).toEqual(["1", "changed"]);
  });

  it("produces NO frame for an update that names no task", () => {
    // Arrange, Act.
    const item = taskActConverter.start(call("TaskUpdate", { status: "completed" }));

    // Assert.
    expect(item.case).toBeUndefined();
  });
});

describe("taskActConverter.settle — TaskCreate", () => {
  it("keys the act by the identity the tracker minted", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(
        call("TaskCreate", { subject: "s", description: "d" }),
        outcome(toolUseResult("task_create")),
      )!,
    );

    // Assert.
    expect(act.task?.value).toBe("2");
  });

  it("is the `created` arm, so a consumer adds an entry rather than replacing one", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(call("TaskCreate", {}), outcome(toolUseResult("task_create")))!,
    );

    // Assert.
    expect(act.act.case).toBe("created");
  });

  it("takes the subject from the tracker's own echo, which is what the task IS", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(
        call("TaskCreate", { subject: "what I asked for" }),
        outcome(toolUseResult("task_create")),
      )!,
    );

    // Assert.
    expect(act.state?.subject).toBe("Bug 2: badges show spent token count");
  });

  it("takes the description from the INPUT, because the create's result carries none", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(
        call("TaskCreate", { description: "the badge count is wrong" }),
        outcome(toolUseResult("task_create")),
      )!,
    );

    // Assert.
    expect(act.state?.description).toBe("the badge count is wrong");
  });

  it("leaves a created task `pending`, since the create tool takes no status", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(call("TaskCreate", {}), outcome(toolUseResult("task_create")))!,
    );

    // Assert.
    expect(act.state?.status.case).toBe("pending");
  });

  it("produces NO frame when the tracker returned no identity", () => {
    // Arrange, Act, Assert.
    expect(taskActConverter.settle(call("TaskCreate", {}), outcome({ task: {} }))).toBeUndefined();
  });
});

describe("taskActConverter.settle — TaskUpdate", () => {
  it("keys the act by the identity the tracker echoed", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(toolUseResult("task_update")))!,
    );

    // Assert.
    expect(act.task?.value).toBe("1");
  });

  it("is the `changed` arm, so a consumer replaces what it holds", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(toolUseResult("task_update")))!,
    );

    // Assert.
    expect(act.act.case).toBe("changed");
  });

  it("takes the resolved status from the tracker's own statusChange", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(toolUseResult("task_update")))!,
    );

    // Assert.
    expect(act.state?.status.case).toBe("running");
  });

  it("carries the running phrasing the caller supplied", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(
        call("TaskUpdate", { taskId: "1", activeForm: "fixing the badges" }),
        outcome(toolUseResult("task_update")),
      )!,
    );

    // Assert.
    const running = act.state?.status.value as conversationv1.AgentTaskRunning;
    expect(running.activeForm).toBe("fixing the badges");
  });

  it("leaves the running phrasing UNSET when the caller gave none", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(toolUseResult("task_update")))!,
    );

    // Assert.
    const running = act.state?.status.value as conversationv1.AgentTaskRunning;
    expect(running.activeForm).toBeUndefined();
  });

  it("maps a completed status onto the completed arm", () => {
    // Arrange.
    const structured = { success: true, taskId: "1", updatedFields: ["status"], statusChange: { from: "in_progress", to: "completed" } };

    // Act.
    const act = actOf(taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(structured))!);

    // Assert.
    expect(act.state?.status.case).toBe("completed");
  });

  it("maps a deleted status onto the deleted arm", () => {
    // Arrange.
    const structured = { success: true, taskId: "1", statusChange: { from: "pending", to: "deleted" } };

    // Act.
    const act = actOf(taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(structured))!);

    // Assert.
    expect(act.state?.status.case).toBe("deleted");
  });

  it("maps a pending status onto the pending arm", () => {
    // Arrange.
    const structured = { success: true, taskId: "1", statusChange: { from: "in_progress", to: "pending" } };

    // Act.
    const act = actOf(taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(structured))!);

    // Assert.
    expect(act.state?.status.case).toBe("pending");
  });

  it("leaves the status UNSET for an update that changed only the subject", () => {
    // Arrange.
    const structured = { success: true, taskId: "1", updatedFields: ["subject"] };

    // Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1", subject: "renamed" }), outcome(structured))!,
    );

    // Assert.
    expect(act.state?.status.case).toBeUndefined();
  });

  it("carries the tasks this one blocks, as tracker identities", () => {
    // Arrange.
    const structured = { success: true, taskId: "1" };

    // Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1", addBlocks: ["3", "4"] }), outcome(structured))!,
    );

    // Assert.
    expect(act.state?.blocks.map((task) => task.value)).toEqual(["3", "4"]);
  });

  it("carries the tasks blocking this one, as tracker identities", () => {
    // Arrange.
    const structured = { success: true, taskId: "1" };

    // Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1", addBlockedBy: ["2"] }), outcome(structured))!,
    );

    // Assert.
    expect(act.state?.blockedBy.map((task) => task.value)).toEqual(["2"]);
  });

  it("carries the owner when the act handed the task off", () => {
    // Arrange.
    const structured = { success: true, taskId: "1" };

    // Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1", owner: "vetter" }), outcome(structured))!,
    );

    // Assert.
    expect(act.state?.owner).toBe("vetter");
  });

  it("leaves the owner UNSET for a task the agent kept for itself", () => {
    // Arrange.
    const structured = { success: true, taskId: "1" };

    // Act.
    const act = actOf(taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(structured))!);

    // Assert.
    expect(act.state?.owner).toBeUndefined();
  });

  it("is the `rejected` arm when the tracker answered success:false", () => {
    // Arrange.
    const structured = { success: false, taskId: "1", updatedFields: [], error: "no such task" };

    // Act.
    const act = actOf(taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome(structured))!);

    // Assert.
    expect(act.act.case).toBe("rejected");
  });

  it("is the `rejected` arm when the vendor marked the result an error", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome({ taskId: "1" }, true))!,
    );

    // Assert.
    expect(act.act.case).toBe("rejected");
  });

  it("carries what the tracker said was wrong with a refused act", () => {
    // Arrange, Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1" }), outcome({ taskId: "1" }, true))!,
    );

    // Assert.
    const rejected = act.act.value as conversationv1.AgentTaskRejected;
    expect(rejected.error?.content).toEqual(toolResultText("refused"));
  });

  it("leaves a refused act's status UNSET, because the requested status is not the standing one", () => {
    // Arrange.
    const structured = { success: false, taskId: "1", error: "no such task" };

    // Act.
    const act = actOf(
      taskActConverter.settle(call("TaskUpdate", { taskId: "1", status: "completed" }), outcome(structured))!,
    );

    // Assert.
    expect(act.state?.status.case).toBeUndefined();
  });

  it("falls back to the call's own taskId when the tracker echoed none", () => {
    // Arrange, Act.
    const act = actOf(taskActConverter.settle(call("TaskUpdate", { taskId: "7" }), outcome({ success: true }))!);

    // Assert.
    expect(act.task?.value).toBe("7");
  });

  it("produces NO frame when neither the call nor the tracker names a task", () => {
    // Arrange, Act, Assert.
    expect(taskActConverter.settle(call("TaskUpdate", {}), outcome({ success: true }))).toBeUndefined();
  });
});

describe("taskActConverter", () => {
  it("declares no progress arm, because an act is instantaneous", () => {
    // Arrange, Act, Assert.
    expect(taskActConverter.carriesProgress).toBe(false);
  });
});
