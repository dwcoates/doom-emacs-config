/**
 * The live set of detached work.
 *
 * WHAT THIS GUARDS: that the shim's answer to "what is running" is the VENDOR's
 * answer, applied by replacement. The failure mode being excluded is a level
 * diffed into a retained set — one missed message then wedges an indicator
 * permanently, and no later message can unwedge it.
 */
import { describe, expect, it } from "vitest";
import { LiveWorkTable } from "../../src/engine/detached.js";
import type {
  SdkBackgroundTasksChangedMessage,
  SdkTaskNotificationMessage,
  SdkTaskStartedMessage,
  SdkTaskUpdatedMessage,
} from "../../src/sdk/types.js";

function started(overrides: Partial<SdkTaskStartedMessage> = {}): SdkTaskStartedMessage {
  return {
    type: "system",
    subtype: "task_started",
    task_id: "b01",
    tool_use_id: "toolu_1",
    description: "run the build",
    task_type: "bash",
    uuid: "00000000-0000-4000-8000-000000000001",
    session_id: "s-1",
    ...overrides,
  } as SdkTaskStartedMessage;
}

function level(tasks: { task_id: string; task_type: string; description: string }[]): SdkBackgroundTasksChangedMessage {
  return {
    type: "system",
    subtype: "background_tasks_changed",
    tasks,
    uuid: "00000000-0000-4000-8000-000000000002",
    session_id: "s-1",
  } as SdkBackgroundTasksChangedMessage;
}

describe("a task that started", () => {
  it("joins the live set", () => {
    const table = new LiveWorkTable();

    table.onTaskStarted(started());

    expect(table.get("b01")?.taskId).toBe("b01");
  });

  it("records the call it belongs to, so the mapping is one lookup and never a scan", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started());

    expect(table.byToolUseId("toolu_1")?.taskId).toBe("b01");
  });

  it("records the turn that spawned it — KillTurn's transitive set", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started(), "turn-1");

    expect(table.spawnedBy("turn-1").map((entry) => entry.taskId)).toEqual(["b01"]);
  });

  it("does not attribute it to another turn", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started(), "turn-1");

    expect(table.spawnedBy("turn-2")).toEqual([]);
  });
});

describe("ambient work", () => {
  it("counts for liveness", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started({ skip_transcript: true }));

    expect(table.empty).toBe(false);
  });

  it("is never announced", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started({ skip_transcript: true }));

    expect(table.announceable()).toEqual([]);
  });
});

describe("a task update", () => {
  it("applies the patched status", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started());

    table.onTaskUpdated({
      type: "system",
      subtype: "task_updated",
      task_id: "b01",
      patch: { status: "running" },
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    } as SdkTaskUpdatedMessage);

    expect(table.get("b01")?.status).toBe("running");
  });

  it("applies the backgrounded flag", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started());

    table.onTaskUpdated({
      type: "system",
      subtype: "task_updated",
      task_id: "b01",
      patch: { is_backgrounded: true },
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    } as SdkTaskUpdatedMessage);

    expect(table.get("b01")?.backgrounded).toBe(true);
  });

  it("ignores a patch for a task it never saw start", () => {
    const table = new LiveWorkTable();

    expect(
      table.onTaskUpdated({
        type: "system",
        subtype: "task_updated",
        task_id: "unknown",
        patch: {},
        uuid: "00000000-0000-4000-8000-000000000000",
        session_id: "s",
      } as SdkTaskUpdatedMessage),
    ).toBeUndefined();
  });
});

describe("a task notification", () => {
  it("removes the item from the live set", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started());

    table.onTaskNotification({
      type: "system",
      subtype: "task_notification",
      task_id: "b01",
      status: "completed",
      output_file: "/spool/b01.output",
      summary: "",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    } as SdkTaskNotificationMessage);

    expect(table.get("b01")).toBeUndefined();
  });
});

describe("the vendor's level", () => {
  it("DROPS an item the level omits", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started());

    table.onLevel(level([]));

    expect(table.empty).toBe(true);
  });

  it("KEEPS the tool_use_id of an item the level names, which the level itself does not carry", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started());

    table.onLevel(level([{ task_id: "b01", task_type: "bash", description: "run the build (still)" }]));

    expect(table.get("b01")?.toolUseId).toBe("toolu_1");
  });

  it("takes the level's description for an item it already knew", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started());

    table.onLevel(level([{ task_id: "b01", task_type: "bash", description: "renamed" }]));

    expect(table.get("b01")?.description).toBe("renamed");
  });

  it("creates an item the level names that it never saw start", () => {
    const table = new LiveWorkTable();

    table.onLevel(level([{ task_id: "b99", task_type: "agent", description: "someone else's" }]));

    expect(table.get("b99")?.taskType).toBe("agent");
  });

  it("invents no tool_use_id for an item it only learned from the level", () => {
    const table = new LiveWorkTable();

    table.onLevel(level([{ task_id: "b99", task_type: "agent", description: "x" }]));

    expect(table.get("b99")?.toolUseId).toBeUndefined();
  });
});

describe("the live set as the wire names it", () => {
  it("names work by the SPAWNING CALL, never by the vendor task id", () => {
    const table = new LiveWorkTable();
    table.onTaskStarted(started());

    // `DetachedWorkId.value == AgentActivityId.value` (ruling, landing 3), so a
    // terminal retires a handle by equality; the task id stays shim-side for
    // stopTask and the level.
    expect(table.workIds().map((id) => id.value)).toEqual(["toolu_1"]);
  });

  it("omits work with no originating call, which has no wire name at all", () => {
    const table = new LiveWorkTable();

    table.onLevel(level([{ task_id: "b99", task_type: "agent", description: "x" }]));

    // Still tracked for liveness — the level governs no-wedge — but nothing can
    // address it, and inventing a handle would point a watch at nothing.
    expect(table.workIds()).toEqual([]);
    expect(table.get("b99")).toBeDefined();
  });
});
