/**
 * WORK WE STOPPED BEING ABLE TO SEE.
 *
 * The producers here are called by the ENGINE, not by the fold: the only
 * stream-plane evidence of disappearance is a task leaving the
 * `background_tasks_changed` level, and reading that requires DIFFING the level,
 * which the contract forbids the fold to do. So these tests pin the SHAPE — the
 * arm a reader is told, which is the whole point of `lost` — and the ruling
 * itself belongs to whoever holds the live set.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import {
  convertDetached,
  createTaskKindRegistry,
  TASK_KIND_CAPACITY,
  lostAgentEntry,
  lostBashEntry,
  lostSubagentEntry,
  outputPathFromProse,
  wentSilent,
} from "../../src/convert/detached.js";
import { toolResultText } from "../../src/convert/entries.js";
import { foldContext, MAIN_AGENT } from "./fold-harness.js";

const RUN = create(conversationv1.AgentActivityIdSchema, { value: "run-1" });

/** The start a lost shell run's terminal restates. */
function start(): conversationv1.AgentBashStart {
  return create(conversationv1.AgentBashStartSchema, {
    command: create(conversationv1.AgentBashCommandSchema, { line: "tail -f log" }),
    startedAt: create(conversationv1.AgentActivityStartedAtSchema, { atMs: 5n }),
  });
}

describe("wentSilent", () => {
  it("is the arm for a run that produced nothing past the silence ruling", () => {
    expect(wentSilent().how.case).toBe("wentSilent");
  });
});

describe("lostBashEntry", () => {
  it("settles the run as interrupted by loss, restating the recorded command", () => {
    const entry = lostBashEntry(foldContext(), MAIN_AGENT, RUN, start(), wentSilent());

    const frame = entry?.item.kind === "bash_run" ? entry.item.frame : undefined;
    const success = frame?.result.value as conversationv1.AgentBashSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(success.command?.line).toBe("tail -f log");
    expect(interrupted.cause.case).toBe("lost");
  });

  it("states not_observed rather than claiming output it did not see", () => {
    // LANDING 5: distinct from empty text (a command that printed nothing) and
    // from a `partial` omission of zero bytes, which claimed we had seen all
    // none of what it printed.
    const entry = lostBashEntry(foldContext(), MAIN_AGENT, RUN, start(), wentSilent());

    const frame = entry?.item.kind === "bash_run" ? entry.item.frame : undefined;
    const success = frame?.result.value as conversationv1.AgentBashSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(interrupted.output?.form.case).toBe("notObserved");
  });

  it("refuses to invent a command when the record holds none", () => {
    const entry = lostBashEntry(
      foldContext(),
      MAIN_AGENT,
      RUN,
      create(conversationv1.AgentBashStartSchema, {}),
      wentSilent(),
    );

    expect(entry).toBeUndefined();
  });

  it("keys the row as the run's TERMINAL, which supersedes nothing", () => {
    // A run's rows are a SEQUENCE — start, deltas, terminal — so a terminal
    // sharing the start's key would erase the output the run produced.
    const entry = lostBashEntry(foldContext(), MAIN_AGENT, RUN, start(), wentSilent());

    expect(entry?.upsertKey).toBe("bash:run-1:terminal");
  });
});

describe("lostSubagentEntry", () => {
  it("settles the spawn unit's failure cause as lost, never as an error", () => {
    const entry = lostSubagentEntry(foldContext(), MAIN_AGENT, RUN, wentSilent());

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate).update;
    const activity = update.value as conversationv1.AgentActivity;
    const subagent = activity.item.value as conversationv1.AgentSubagent;
    const failed = subagent.result.value as conversationv1.AgentSubagentFailure;
    expect(failed.cause.case).toBe("lost");
    expect(failed.error).toBeUndefined();
  });
});

describe("lostAgentEntry", () => {
  it("closes the agent's own book with the lost arm", () => {
    const entry = lostAgentEntry(foldContext(), MAIN_AGENT, wentSilent());

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const failure = frame?.result.value as conversationv1.AgentFailure;
    expect(failure.failure.case).toBe("lost");
  });

  it("marks a keep-alive turn's loss never-served, like every other row of it", () => {
    const entry = lostAgentEntry(foldContext({ keepalive: true }), MAIN_AGENT, wentSilent());

    expect(entry.keepalive).toBe(true);
  });
});

describe("outputPathFromProse", () => {
  // `toolUseResult` on a backgrounded Bash carries `backgroundTaskId` and
  // NOTHING else (testdata/corpus/tool-results/bash-background.jsonl), so this
  // sentence is the vendor's only statement of where the output accumulates at
  // announcement time. Reading it wrong leaves every detached shell announced
  // with no output and its readability unset.

  it("reads the spool path out of the vendor's captured sentence", () => {
    // Arrange. Byte-for-byte the shape the corpus capture carries.
    const content = toolResultText(
      "Command running in background with ID: bvif9m46l. Output is being written to: " +
        "/private/tmp/claude-501/x/f2c3c473/tasks/bvif9m46l.output. You will be notified when it " +
        "completes. To check interim output, use Read on that file path.",
    );

    // Act, Assert.
    expect(outputPathFromProse(content)).toBe(
      "/private/tmp/claude-501/x/f2c3c473/tasks/bvif9m46l.output",
    );
  });

  it("stops at the sentence's period rather than swallowing it into the path", () => {
    // Arrange.
    const content = toolResultText("Output is being written to: /tmp/a.output. Then more prose.");

    // Act, Assert.
    expect(outputPathFromProse(content)).toBe("/tmp/a.output");
  });

  it("yields NO path when the vendor said something else, rather than a wrong one", () => {
    // A reworded sentence must degrade to an announcement with no output — the
    // task_notification still supplies the path at the end — never to a guess.
    // Arrange.
    const content = toolResultText("Command running in background with ID: b1.");

    // Act, Assert.
    expect(outputPathFromProse(content)).toBeUndefined();
  });

  it("yields no path when there is no content at all", () => {
    // Arrange, Act, Assert.
    expect(outputPathFromProse(undefined)).toBeUndefined();
  });
});

describe("a settling task's KIND decides whether it settles a subagent", () => {
  /** The vendor's `task_started`, as the stream states one. */
  function taskStarted(taskType: string): Extract<SdkMessage, { type: "system" }> {
    return {
      type: "system",
      subtype: "task_started",
      uuid: "uuid-started",
      session_id: "session-1",
      task_id: "task-1",
      tool_use_id: "toolu_1",
      description: "the work",
      task_type: taskType,
    } as unknown as Extract<SdkMessage, { type: "system" }>;
  }

  /** The vendor's `task_notification`, which states NO kind of its own. */
  function taskNotification(): Extract<SdkMessage, { type: "system" }> {
    return {
      type: "system",
      subtype: "task_notification",
      uuid: "uuid-notified",
      session_id: "session-1",
      task_id: "task-1",
      tool_use_id: "toolu_1",
      status: "completed",
      output_file: "/tmp/task-1.output",
      summary: "done",
    } as unknown as Extract<SdkMessage, { type: "system" }>;
  }

  /** The arms one start-then-notify pair produces. */
  function armsFor(taskType: string | undefined): string[] {
    const registry = createTaskKindRegistry();
    const context = foldContext();
    const entries =
      taskType === undefined
        ? []
        : [...convertDetached(taskStarted(taskType), context, registry)];
    entries.push(...convertDetached(taskNotification(), context, registry));
    return entries.map((entry) => entry.source.discriminator);
  }

  it("settles an agent run as a subagent", () => {
    expect(armsFor("local_agent")).toContain("activity.subagent.success");
  });

  it("never settles a backgrounded SHELL command as a subagent", () => {
    // The `Bash` unit already settled on its own tool result saying it moved to
    // the background; a subagent terminal here would restate a shell command as
    // an agent run and invent an empty spawn prompt for it.
    expect(armsFor("local_bash")).not.toContain("activity.subagent.success");
  });

  it("still announces the shell task's detachment", () => {
    expect(armsFor("local_bash")).toContain("agent_frame.detached_work.detached.requested");
  });

  it("settles a task whose kind was never stated, rather than losing the terminal", () => {
    expect(armsFor(undefined)).toContain("activity.subagent.success");
  });

  it("forgets a task once it settles, so the table empties itself", () => {
    const registry = createTaskKindRegistry();
    registry.remember("task-1", "local_bash");
    expect(registry.settlesAsSubagent("task-1")).toBe(false);
    expect(registry.settlesAsSubagent("task-1")).toBe(true);
  });

  it("forgets the oldest task when the cap is reached", () => {
    const registry = createTaskKindRegistry();
    registry.remember("oldest", "local_bash");
    for (let index = 0; index < TASK_KIND_CAPACITY; index += 1) {
      registry.remember(`task-${index}`, "local_bash");
    }
    expect(registry.settlesAsSubagent("oldest")).toBe(true);
  });
});
