/**
 * The shell family. The assertions divide in two.
 *
 * FOREGROUND: what the tool result says, because that is the only place a
 * shell's cause of backgrounding, its spill and its image-ness are ever stated.
 *
 * DETACHED: what the SPOOL says, because `AgentBashUpdate` is structurally
 * detach-only and every byte of it comes from the sidecar tailing that file.
 * A detached test that only checked messages would pass against a mock that
 * wrote nothing at all.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, ofType, toolUseResults, toolUses } from "../harness.js";

const result = async (prompt: string): Promise<Record<string, unknown>> => {
  const driven = await driveScenario([prompt]);
  return toolUseResults(driven.transcript())[0] as Record<string, unknown>;
};

describe("foreground shells", () => {
  it("emits NOTHING between the call and the result, because output is unobservable while running", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash echo hi"]);
    const between = ofType(driven, "tool_progress").length + ofType(driven, "system", "task_started").length;

    // Assert
    expect(between).toBe(0);
  });

  it("passes the prompt's argument through as the command", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash echo custom"]);

    // Assert
    expect((toolUses(driven)[0] as { input: { command: string } }).input.command).toBe("echo custom");
  });

  it("reports a non-zero exit as a completed run with stderr, not as a missing result", async () => {
    // Arrange + Act
    const failed = await result("!bash-fail");

    // Assert
    expect({ interrupted: failed.interrupted, stderr: failed.stderr }).toEqual({
      interrupted: false,
      stderr: "boom\n",
    });
  });

  it("marks a non-zero run's tool_result an error for the model", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-fail"]);
    const block = (
      driven.transcript().find((l) => l.toolUseResult !== undefined)?.message as {
        content: { is_error: boolean }[];
      }
    ).content[0];

    // Assert
    expect(block?.is_error).toBe(true);
  });

  it("states a TIMEOUT backgrounding through timedOutAfterMs on the tool result", async () => {
    // Arrange + Act. The task stream says nothing about the cause; this field is
    // the only discriminator between a timeout and a vendor-backgrounded detach.
    const timedOut = await result("!bash-timeout");

    // Assert
    expect({ ms: timedOut.timedOutAfterMs, byUser: timedOut.backgroundedByUser }).toEqual({
      ms: 120_000,
      byUser: undefined,
    });
  });

  it("leaves a timed-out run's spool UNTERMINATED, because the run is still going", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-timeout"]);
    const taskId = String(ofType(driven, "system", "task_started")[0]?.task_id);

    // Assert
    expect(driven.spool(taskId)).not.toContain("EXIT=");
  });

  it("states a spill through persistedOutputPath and its size", async () => {
    // Arrange + Act
    const spilled = await result("!bash-spill");

    // Assert
    expect({
      path: typeof spilled.persistedOutputPath === "string",
      size: spilled.persistedOutputSize,
    }).toEqual({ path: true, size: 200_000 });
  });

  it("marks an image-producing run isImage and answers with an image block", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-image"]);
    const tur = toolUseResults(driven.transcript())[0] as Record<string, unknown>;
    const block = (
      driven.transcript().find((l) => l.toolUseResult !== undefined)?.message as {
        content: { content: { type: string }[] }[];
      }
    ).content[0]?.content[0];

    // Assert
    expect({ isImage: tur.isImage, blockType: block?.type }).toEqual({ isImage: true, blockType: "image" });
  });
});

describe("detached shells", () => {
  it("names the run's spool with a b-prefixed task id under tasks/", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach"]);

    // Assert
    expect(driven.spoolIds()).toEqual([expect.stringMatching(/^b[a-z0-9]{8}$/)]);
  });

  it("writes the spool INCREMENTALLY, terminated by EXIT=0", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach"]);
    const taskId = String(ofType(driven, "system", "task_started")[0]?.task_id);

    // Assert
    expect(driven.spool(taskId)).toBe("line-1\nline-2\nline-3\nEXIT=0\n");
  });

  it("returns the tool result while the run is STILL GOING, carrying only the task id", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach"]);
    const tur = toolUseResults(driven.transcript())[0] as Record<string, unknown>;

    // Assert. Empty stdout with a task id is what "detached" looks like.
    expect({ stdout: tur.stdout, hasTask: typeof tur.backgroundTaskId === "string" }).toEqual({
      stdout: "",
      hasTask: true,
    });
  });

  it("announces the live set when the task starts and again when it ends", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach"]);
    const announcements = ofType(driven, "system", "background_tasks_changed");

    // Assert. REPLACE semantics: the last announcement is the empty set.
    expect({
      count: announcements.length,
      last: (announcements.at(-1)?.tasks as unknown[]).length,
    }).toEqual({ count: 2, last: 0 });
  });

  it("ends with a completed task_notification naming the output file", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach"]);
    const notification = ofType(driven, "system", "task_notification")[0];

    // Assert
    expect({
      status: notification?.status,
      file: String(notification?.output_file).endsWith(".output"),
    }).toEqual({ status: "completed", file: true });
  });

  it("terminates a failing run's spool with its real exit code", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-fail"]);
    const taskId = String(ofType(driven, "system", "task_started")[0]?.task_id);

    // Assert
    expect(driven.spool(taskId)).toBe("error\nEXIT=3\n");
  });

  it("reports a failing run through task_updated as well as the notification", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-fail"]);

    // Assert
    expect((ofType(driven, "system", "task_updated")[0]?.patch as { status: string }).status).toBe(
      "failed",
    );
  });

  it("leaves a never-ending run's spool with NO terminator and the task live", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-live"]);
    const taskId = String(ofType(driven, "system", "task_started")[0]?.task_id);

    // Assert. This is the corpus's `bash-midoutput.output` shape.
    expect({
      body: driven.spool(taskId),
      notifications: ofType(driven, "system", "task_notification").length,
    }).toEqual({ body: "partial output with no terminator\n", notifications: 0 });
  });
});

describe("explicit poll/retrieval of a detached shell", () => {
  // UNGROUNDED, INVENTED (testdata/captures/MANIFEST.md): no capture ever
  // calls `TaskOutput`, only lists it in `init.tools`.
  it("issues THREE TaskOutput calls: two RUNNING and one terminal", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-poll"]);
    const polls = toolUses(driven).filter((t) => (t as { name: string }).name === "TaskOutput");

    // Assert
    expect(polls).toHaveLength(3);
  });

  it("grows the reported output across the RUNNING polls", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-poll"]);
    const results = toolUseResults(driven.transcript()).filter(
      (r) => (r as { task?: { status?: string } }).task?.status === "RUNNING",
    ) as { task: { output: string } }[];

    // Assert
    expect(results.map((r) => r.task.output)).toEqual(["compiling\n", "compiling\nlinking\n"]);
  });

  it("reports the terminal poll with a completed status and an exit code", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-poll"]);
    const results = toolUseResults(driven.transcript()) as { task?: Record<string, unknown> }[];
    const terminal = results.find((r) => r.task?.status === "COMPLETED");

    // Assert
    expect({ exitCode: terminal?.task?.exitCode, exitCodeSet: terminal?.task?.exitCodeSet }).toEqual({
      exitCode: 0,
      exitCodeSet: true,
    });
  });

  it("still terminates the spool with EXIT=0, same as an ordinary detach", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-poll"]);
    const taskId = String(ofType(driven, "system", "task_started")[0]?.task_id);

    // Assert
    expect(driven.spool(taskId)).toBe("compiling\nlinking\ndone\nEXIT=0\n");
  });
});

describe("VendorBackgrounded", () => {
  /**
   * Drive `!vendor-backgrounded` all the way through its detach.
   *
   * The scenario PARKS until something backgrounds the call, because a real
   * vendor-side detach is not something the scenario can decide the moment
   * of. Nothing detaches it here, so a plain `driveScenario` would sit on
   * the parked run forever.
   */
  const driveVendorBackgrounded = async (): Promise<Record<string, unknown>> => {
    const driven = await driveScenario(["!vendor-backgrounded"], {
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
        const started = messages.find((m) => m.subtype === "task_started");
        if (started === undefined) throw new Error("no task to detach");
        await query.backgroundTasks(String(started.tool_use_id));
      },
    });
    return toolUseResults(driven.transcript())[0] as Record<string, unknown>;
  };

  it("states a USER backgrounding through backgroundedByUser, not timedOutAfterMs", async () => {
    // Arrange + Act
    const detached = await driveVendorBackgrounded();

    // Assert
    expect({ byUser: detached.backgroundedByUser, timedOut: detached.timedOutAfterMs }).toEqual({
      byUser: true,
      timedOut: undefined,
    });
  });

  it("keeps the output produced before the detach on the foreground result", async () => {
    // Arrange + Act
    const detached = await driveVendorBackgrounded();

    // Assert
    expect(detached.stdout).toBe("first line before the detach\n");
  });

  it("puts the backgrounded result on the stream BEFORE backgroundTasks answers", async () => {
    // A real vendor-side detach has already published the detachment by the
    // time the binary reports it; answering first would let a caller observe
    // DetachForeground succeeding against a conversation that still shows
    // foreground work.
    // Arrange.
    let resultsWhenAnswered = -1;

    // Act.
    await driveScenario(["!vendor-backgrounded"], {
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
        const started = messages.find((m) => m.subtype === "task_started");
        if (started === undefined) throw new Error("no task to detach");
        await query.backgroundTasks(String(started.tool_use_id));
        resultsWhenAnswered = messages.filter(
          (m) => m.type === "user" && m.tool_use_result !== undefined,
        ).length;
      },
    });

    // Assert.
    expect(resultsWhenAnswered).toBe(1);
  });

  it("marks the task is_backgrounded when backgroundTasks names its tool call", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!vendor-backgrounded"], {
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
        const started = messages.find((m) => m.subtype === "task_started");
        if (started === undefined) throw new Error("no task to detach");
        await query.backgroundTasks(String(started.tool_use_id));
      },
    });

    // Assert
    expect((ofType(driven, "system", "task_updated")[0]?.patch as { is_backgrounded: boolean })).toEqual(
      { is_backgrounded: true },
    );
  });


  it("parks a foreground Bash until an interrupt lands, with no terminal on its own", async () => {
    // !bash-hold's run() calls ctx.awaitInterrupt() and never returns on its
    // own -- a bare driveScenario() would hang the suite forever, which is why
    // this scenario had never been driven at the fake level (lifecycle.test.ts
    // documents the same shape for !hold/!interrupt as the ENGINE suite's job).
    // Driving the interrupt through `during` releases it in-process instead.
    const driven = await driveScenario(["!bash-hold"], {
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
        expect(messages.some((m) => (m as { type: string }).type === "assistant")).toBe(true);
        await query.interrupt();
      },
    });

    // The vendor's own generic interrupt terminal, not something bash-hold's
    // run() produced -- it never calls conclude().
    const results = ofType(driven, "result");
    expect(results).toHaveLength(1);
    expect(results[0]?.terminal_reason).toBe("aborted_streaming");
  });
});
