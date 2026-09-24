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
import { writeSync } from "node:fs";
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import type { PersistEntry } from "../../src/store/persistence.js";
import {
  bashDetachmentEntry,
  convertDetached,
  createTaskKindRegistry,
  TASK_KIND_CAPACITY,
  lostAgentEntry,
  lostBashEntry,
  lostSubagentEntry,
  outputPathFromProse,
  patchBackgrounds,
  resultBackgroundTaskId,
  startedInForeground,
  wentSilent,
} from "../../src/convert/detached.js";
import { toolResultText } from "../../src/convert/entries.js";
import {
  createCallRegistry,
  type CallRegistry,
  type PendingCall,
} from "../../src/convert/tool-calls.js";
import { activityOf, foldContext, MAIN_AGENT } from "./fold-harness.js";

/**
 * Run a converter for its SIDE EFFECT on the registry, discarding what it
 * yields. Spelled as a call rather than a bare `[...gen]` statement, so it
 * cannot be mistaken for an expression whose value someone forgot to use.
 */
function drain(entries: Iterable<unknown>): void {
  for (const _entry of entries) void _entry;
}

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
    const entry = lostSubagentEntry(foldContext(), MAIN_AGENT, lostSpawn(), wentSilent());

    const failed = lostFailure(entry);
    expect(failed.cause.case).toBe("lost");
    expect(failed.error).toBeUndefined();
  });

  it("restates what the lost spawn was asked", () => {
    const failed = lostFailure(lostSubagentEntry(foldContext(), MAIN_AGENT, lostSpawn(), wentSilent()));

    expect(failed.prompt?.description).toBe("watch the build");
  });

  it("restates the agent the lost spawn created", () => {
    const failed = lostFailure(lostSubagentEntry(foldContext(), MAIN_AGENT, lostSpawn(), wentSilent()));

    expect(failed.createdAgentId?.value).toBe("run-1");
  });
});

/** The spawning call of a detached run the engine later concludes lost. */
function lostSpawn(): PendingCall {
  return {
    toolUseId: "run-1",
    toolName: "Agent",
    input: { description: "watch the build", prompt: "Watch it." },
    startedAtMs: 5,
    agentId: MAIN_AGENT,
  };
}

/** The failure a lost-spawn row settles with. */
function lostFailure(entry: PersistEntry): conversationv1.AgentSubagentFailure {
  const subagent = activityOf(entry)?.item.value as conversationv1.AgentSubagent;
  return subagent.result.value as conversationv1.AgentSubagentFailure;
}

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
        : [...convertDetached(taskStarted(taskType), context, registry, createCallRegistry())];
    entries.push(...convertDetached(taskNotification(), context, registry, createCallRegistry()));
    return entries.map((entry) => entry.source.discriminator);
  }

  it("settles an agent run as a subagent", () => {
    expect(armsFor("local_agent")).toContain("activity.subagent.success");
  });

  it("names the created agent on the settling notification's success", () => {
    // This terminal can be the only frame of the spawn a consumer receives:
    // the launch's own start rode the tool result, on a delivery this one need
    // not share. Without the id the bubble drawn from it addresses nothing.
    // Arrange.
    const registry = createTaskKindRegistry();
    const context = foldContext();
    drain(convertDetached(taskStarted("local_agent"), context, registry, createCallRegistry()));

    // Act.
    const entries = [...convertDetached(taskNotification(), context, registry, createCallRegistry())];
    const settled = entries.find(
      (entry) => entry.source.discriminator === "activity.subagent.success",
    );

    // Assert: the spawning call's own id, per the minting rule.
    const update = (settled?.item as { frame: conversationv1.AgentFrame }).frame.result
      .value as conversationv1.AgentUpdate;
    const activity = update.update.value as conversationv1.AgentActivity;
    const spawn = (activity.item.value as conversationv1.AgentSubagent).result
      .value as conversationv1.AgentSubagentSuccess;
    expect(spawn.createdAgentId?.value).toBe("toolu_1");
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

// ---------------------------------------------------------------------------
// The BASH RESULT is the one record that states WHY a shell left the turn
// ---------------------------------------------------------------------------

/** The detachment announcement one Bash result produces, if it produces one. */
function bashDetachment(
  structured: unknown,
  resultContent?: conversationv1.ToolResultContent,
  registry = createTaskKindRegistry(),
) {
  return bashDetachmentEntry(
    foldContext(),
    MAIN_AGENT,
    "uuid-result",
    "toolu_bash",
    structured,
    resultContent,
    registry,
  );
}

/** The `detached` origin a detachment row carries. */
function detachedOrigin(entry: PersistEntry | undefined): conversationv1.DetachedWorkDetached {
  const frame = entry?.item.kind === "frame" ? entry.item.frame : undefined;
  const work = frame?.result.value as conversationv1.AgentDetachedWork;
  return work.origin.value as conversationv1.DetachedWorkDetached;
}

/** The announcement a detachment row carries. */
function announcement(entry: PersistEntry | undefined): conversationv1.AgentDetachedWork {
  const frame = entry?.item.kind === "frame" ? entry.item.frame : undefined;
  return frame?.result.value as conversationv1.AgentDetachedWork;
}

describe("bashDetachmentEntry", () => {
  it("announces nothing for a result the vendor never backgrounded", () => {
    expect(bashDetachment({ stdout: "done" })).toBeUndefined();
  });

  it("announces nothing when the vendor named an EMPTY background task id", () => {
    expect(bashDetachment({ backgroundTaskId: "" })).toBeUndefined();
  });

  it("states `requested` for a run_in_background launch, which nobody interrupted", () => {
    expect(detachedOrigin(bashDetachment({ backgroundTaskId: "bt-1" })).cause.case).toBe("requested");
  });

  it("states `by_user` when the vendor said a person backgrounded it", () => {
    const entry = bashDetachment({ backgroundTaskId: "bt-1", backgroundedByUser: true });

    expect(detachedOrigin(entry).cause.case).toBe("byUser");
  });

  it("states `timed_out` when the vendor reported the limit it exceeded", () => {
    const entry = bashDetachment({ backgroundTaskId: "bt-1", timedOutAfterMs: 120_000 });

    const cause = detachedOrigin(entry).cause;
    expect(cause.case).toBe("timedOut");
    expect((cause.value as conversationv1.DetachedCauseTimedOut).timeoutMs).toBe(120_000n);
  });

  it("carries THE CONFIGURED LIMIT, not the work's runtime, on the timed-out arm", () => {
    // The work is still running, so its runtime is not yet a fact.
    const entry = bashDetachment({ backgroundTaskId: "bt-1", timedOutAfterMs: 0 });

    const cause = detachedOrigin(entry).cause;
    expect((cause.value as conversationv1.DetachedCauseTimedOut).timeoutMs).toBe(0n);
  });

  it("prefers the vendor's DECLARED output path over its prose", () => {
    // A declared field outranks a sentence: a SPILLED foreground result sets it.
    const entry = bashDetachment(
      { backgroundTaskId: "bt-1", persistedOutputPath: "/tmp/declared.output" },
      toolResultText("Output is being written to: /tmp/prose.output."),
    );

    expect(announcement(entry).output?.path).toBe("/tmp/declared.output");
  });

  it("falls back to the prose path when the vendor declared no field", () => {
    const entry = bashDetachment(
      { backgroundTaskId: "bt-1" },
      toolResultText("Output is being written to: /tmp/prose.output."),
    );

    expect(announcement(entry).output?.path).toBe("/tmp/prose.output");
  });

  it("announces the detachment with NO output when neither field nor sentence names one", () => {
    const entry = bashDetachment({ backgroundTaskId: "bt-1" });

    expect(announcement(entry).output).toBeUndefined();
  });

  it("marks the named file READABLE, since the vendor tells the model to open it", () => {
    const entry = bashDetachment(
      { backgroundTaskId: "bt-1", persistedOutputPath: "/tmp/a.output" },
      undefined,
    );

    expect(announcement(entry).output?.readability.case).toBe("readable");
  });

  it("names the call's own agent as the work's OWNER", () => {
    // Arrange: the call was made by a subagent, whose book the result rides.
    const subagent = create(conversationv1.AgentIdSchema, { value: "toolu_spawn" });

    // Act.
    const entry = bashDetachmentEntry(
      foldContext(),
      subagent,
      "uuid-result",
      "toolu_bash",
      { backgroundTaskId: "bt-1" },
      undefined,
      undefined,
    );

    // Assert.
    expect(announcement(entry).owner?.value).toBe("toolu_spawn");
  });

  it("remembers the stated cause so the later notification cannot overwrite it", () => {
    const registry = createTaskKindRegistry();
    bashDetachment({ backgroundTaskId: "bt-1", backgroundedByUser: true }, undefined, registry);

    expect(registry.causeOf("bt-1")).toBe("by_user");
  });

  it("announces even with no cause registry to remember into", () => {
    const entry = bashDetachmentEntry(
      foldContext(),
      MAIN_AGENT,
      "uuid-result",
      "toolu_bash",
      { backgroundTaskId: "bt-1" },
      undefined,
      undefined,
    );

    expect(entry?.source.discriminator).toBe("agent_frame.detached_work.detached.requested");
  });
});

describe("outputPathFromProse skips the blocks that are not words", () => {
  it("ignores a non-text block rather than reading a path out of it", () => {
    // Arrange. An image block beside the sentence: the path is still the
    // sentence's, and the image contributes nothing.
    const content = create(conversationv1.ToolResultContentSchema, {
      blocks: [
        create(conversationv1.ToolResultContentBlockSchema, {
          block: {
            case: "unsupported",
            value: create(conversationv1.UnsupportedBlockSchema, {}),
          },
        }),
        create(conversationv1.ToolResultContentBlockSchema, {
          block: { case: "text", value: create(conversationv1.TextBlockSchema, {
            text: "Output is being written to: /tmp/b.output.",
          }) },
        }),
      ],
    });

    // Act, Assert.
    expect(outputPathFromProse(content)).toBe("/tmp/b.output");
  });
});

// ---------------------------------------------------------------------------
// The task stream
// ---------------------------------------------------------------------------

/** One vendor task-stream record. */
function taskMessage(fields: Record<string, unknown>): Extract<SdkMessage, { type: "system" }> {
  return {
    type: "system",
    uuid: "uuid-task",
    session_id: "session-1",
    ...fields,
  } as unknown as Extract<SdkMessage, { type: "system" }>;
}

/** What one task message converts to. */
function convert(
  fields: Record<string, unknown>,
  overrides: Parameters<typeof foldContext>[0] = {},
  registry = createTaskKindRegistry(),
  calls: CallRegistry = createCallRegistry(),
): readonly PersistEntry[] {
  return convertDetached(taskMessage(fields), foldContext(overrides), registry, calls);
}

describe("convertDetached: the level and the ambient task", () => {
  it("produces nothing from the background-task LEVEL, which the engine owns", () => {
    expect(convert({ subtype: "background_tasks_changed" })).toEqual([]);
  });

  it("drops an ambient housekeeping task from every announcement", () => {
    expect(
      convert({ subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1", skip_transcript: true }),
    ).toEqual([]);
  });
});

describe("convertDetached: a task message that names no task", () => {
  it("lands as residue, since nothing can be addressed by it", () => {
    const entries = convert({ subtype: "task_started", tool_use_id: "toolu_1" });

    expect(entries[0]?.source.discriminator).toBe("residue.unparsed");
  });

  it("lands as residue when the task id is the EMPTY string", () => {
    const entries = convert({ subtype: "task_started", task_id: "" });

    expect(entries[0]?.source.discriminator).toBe("residue.unparsed");
  });
});

describe("convertDetached: task_started", () => {
  it("lands as residue when no originating call is known, rather than inventing one", () => {
    const entries = convert({ subtype: "task_started", task_id: "t1", task_type: "local_agent" });

    expect(entries[0]?.source.discriminator).toBe("residue.unknown.task_started");
  });

  it("recovers the originating call from the engine's live-task table", () => {
    // The vendor's own record names no `tool_use_id`; the engine's join does.
    const entries = convert(
      { subtype: "task_started", task_id: "t1", task_type: "local_agent" },
      { liveTask: () => ({ toolUseId: "toolu_join" }) },
    );

    expect(detachedOrigin(entries[0]).detachedFromId?.value).toBe("toolu_join");
  });

  it("books the announcement against the SUBAGENT the engine knows is running it", () => {
    const subagent = create(conversationv1.AgentIdSchema, { value: "sub-1" });
    const entries = convert(
      { subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1", task_type: "local_agent" },
      { liveTask: () => ({ toolUseId: "toolu_1", agentId: subagent }) },
    );

    expect(entries[0]?.agentId.value).toBe("sub-1");
  });

  it("announces an agent task whose kind the vendor left unstated", () => {
    const entries = convert({ subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1" });

    expect(entries[0]?.source.discriminator).toBe("agent_frame.detached_work.detached.requested");
  });

  it("remembers `requested` for an agent task, so its notification upserts that cause", () => {
    const registry = createTaskKindRegistry();
    convert({ subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1" }, {}, registry);

    expect(registry.causeOf("t1")).toBe("requested");
  });
});

describe("convertDetached: task_updated", () => {
  it("produces nothing from a patch that states no detachment", () => {
    expect(
      convert({ subtype: "task_updated", task_id: "t1", tool_use_id: "toolu_1", patch: { status: "running" } }),
    ).toEqual([]);
  });

  it("upserts nothing when a hand-backgrounded task names no originating call", () => {
    expect(
      convert({ subtype: "task_updated", task_id: "t1", patch: { is_backgrounded: true } }),
    ).toEqual([]);
  });

  it("announces `by_user` when a person backgrounded running work by hand", () => {
    const entries = convert({
      subtype: "task_updated",
      task_id: "t1",
      tool_use_id: "toolu_1",
      patch: { is_backgrounded: true },
    });

    expect(detachedOrigin(entries[0]).cause.case).toBe("byUser");
  });

  it("remembers `by_user`, so the notification does not restate it as requested", () => {
    const registry = createTaskKindRegistry();
    convert(
      { subtype: "task_updated", task_id: "t1", tool_use_id: "toolu_1", patch: { is_backgrounded: true } },
      {},
      registry,
    );

    expect(registry.causeOf("t1")).toBe("by_user");
  });
});

/** The progress arm one task-progress beat produced, when it produced one. */
function progressOf(entry: PersistEntry | undefined): conversationv1.AgentSubagentProgress {
  const subagent = activityOf(entry)?.item.value as conversationv1.AgentSubagent;
  const update = subagent.result.value as conversationv1.AgentSubagentUpdate;
  return update.progress as conversationv1.AgentSubagentProgress;
}

describe("convertDetached: task_progress", () => {
  it("advances the subagent unit with the beat's running token sum", () => {
    // A beat carrying `tool_use_id` directly (the corpus shape) joins to its
    // spawn unit and surfaces the running total the settled frame supersedes.
    const entries = convert({
      subtype: "task_progress",
      task_id: "t1",
      tool_use_id: "toolu_1",
      usage: { total_tokens: 4_200, tool_uses: 1, duration_ms: 500 },
    });

    expect(progressOf(entries[0]).totalTokens).toBe(4_200n);
  });

  it("keys the beat to the SPAWN unit, so successive beats upsert one row", () => {
    // The unit is `toolCallActivityId(toolUseId)` — its value is the tool-use
    // id — exactly as the terminal keys it, so a later beat replaces this one.
    const entries = convert({
      subtype: "task_progress",
      task_id: "t1",
      tool_use_id: "toolu_1",
      usage: { total_tokens: 4_200 },
    });

    expect(activityOf(entries[0])?.activityId?.value).toBe("toolu_1");
  });

  it("carries the beat's tool-call count and elapsed wall-clock", () => {
    const entries = convert({
      subtype: "task_progress",
      task_id: "t1",
      tool_use_id: "toolu_1",
      usage: { total_tokens: 4_200, tool_uses: 3, duration_ms: 1_403 },
    });

    const progress = progressOf(entries[0]);
    expect(progress.toolUseCount).toBe(3);
    expect(progress.durationMs).toBe(1_403n);
  });

  it("states zero rather than inventing figures the beat omitted", () => {
    const entries = convert({
      subtype: "task_progress",
      task_id: "t1",
      tool_use_id: "toolu_1",
    });

    const progress = progressOf(entries[0]);
    expect(progress.totalTokens).toBe(0n);
    expect(progress.toolUseCount).toBe(0);
    expect(progress.durationMs).toBe(0n);
  });

  it("joins to the spawn REMEMBERED from an earlier message when the beat restates no call", () => {
    // `task_started` states the spawning call; a later beat need not restate it,
    // so the registry supplies the join rather than the beat being dropped.
    const registry = createTaskKindRegistry();
    convert(
      { subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1", task_type: "local_agent" },
      {},
      registry,
    );

    const entries = convert(
      { subtype: "task_progress", task_id: "t1", usage: { total_tokens: 9_000 } },
      {},
      registry,
    );

    expect(activityOf(entries[0])?.activityId?.value).toBe("toolu_1");
    expect(progressOf(entries[0]).totalTokens).toBe(9_000n);
  });

  it("drops a beat that names no spawning call and none is remembered", () => {
    expect(
      convert({ subtype: "task_progress", task_id: "t1", usage: { total_tokens: 4_200 } }),
    ).toEqual([]);
  });

  it("shares the spawn unit's key with the settled terminal, so the total supersedes the beat", () => {
    // Both frames key by `activityUpsertKey(toolCallActivityId(toolUseId))`, so
    // the notification's settled `total_only` upserts the same row the running
    // beat did — the running figure is replaced, never left standing beside it.
    const registry = createTaskKindRegistry();
    const beat = convert(
      { subtype: "task_progress", task_id: "t1", tool_use_id: "toolu_1", usage: { total_tokens: 4_200 } },
      {},
      registry,
    );
    const settle = convert(
      {
        subtype: "task_notification",
        task_id: "t1",
        tool_use_id: "toolu_1",
        status: "completed",
        usage: { total_tokens: 11_114 },
      },
      {},
      registry,
    );

    const settledSuccess = (activityOf(settle.at(-1))?.item.value as conversationv1.AgentSubagent).result
      .value as conversationv1.AgentSubagentSuccess;
    const settledUsage = settledSuccess.totals?.usage.value as conversationv1.AgentSubagentAsyncUsage;
    expect(beat[0]?.upsertKey).toBe(settle.at(-1)?.upsertKey);
    expect(settledUsage.totalTokens).toBe(11_114n);
  });
});

describe("convertDetached: task_notification", () => {
  it("upserts the announcement with the REMEMBERED cause, never a fresh requested", () => {
    const registry = createTaskKindRegistry();
    registry.rememberCause("t1", "timed_out");
    const entries = convert(
      {
        subtype: "task_notification",
        task_id: "t1",
        tool_use_id: "toolu_1",
        output_file: "/tmp/t1.output",
        status: "completed",
      },
      {},
      registry,
    );

    expect(detachedOrigin(entries[0]).cause.case).toBe("timedOut");
  });

  it("falls back to `requested` when no cause was ever stated for the task", () => {
    const entries = convert({
      subtype: "task_notification",
      task_id: "t1",
      tool_use_id: "toolu_1",
      output_file: "/tmp/t1.output",
      status: "completed",
    });

    expect(detachedOrigin(entries[0]).cause.case).toBe("requested");
  });

  it("upserts no announcement when the notification names no output file", () => {
    const entries = convert({
      subtype: "task_notification",
      task_id: "t1",
      tool_use_id: "toolu_1",
      status: "completed",
    });

    expect(entries.map((entry) => entry.source.discriminator)).toEqual([
      "activity.subagent.success",
    ]);
  });

  it("settles nothing when the notification names no originating call", () => {
    expect(convert({ subtype: "task_notification", task_id: "t1", status: "completed" })).toEqual([]);
  });

  it("settles a STOPPED run as stopped_by_user rather than as an error", () => {
    const entries = convert({
      subtype: "task_notification",
      task_id: "t1",
      tool_use_id: "toolu_1",
      status: "stopped",
    });

    expect(entries[0]?.source.discriminator).toBe("activity.subagent.failure.stopped_by_user");
  });

  it("settles a FAILED run as a subagent failure carrying the vendor's summary", () => {
    const entries = convert({
      subtype: "task_notification",
      task_id: "t1",
      tool_use_id: "toolu_1",
      status: "failed",
      summary: "the run blew up",
    });

    const activity = activityOf(entries[0]);
    const subagent = activity?.item.value as conversationv1.AgentSubagent;
    const failure = subagent.result.value as conversationv1.AgentSubagentFailure;
    expect(failure.error?.content?.blocks[0]?.block.value).toMatchObject({
      text: "the run blew up",
    });
  });

  it("leaves a failed run's error content UNSET when the vendor stated no summary", () => {
    const entries = convert({
      subtype: "task_notification",
      task_id: "t1",
      tool_use_id: "toolu_1",
      status: "failed",
    });

    const activity = activityOf(entries[0]);
    const subagent = activity?.item.value as conversationv1.AgentSubagent;
    const failure = subagent.result.value as conversationv1.AgentSubagentFailure;
    expect(failure.error?.content).toBeUndefined();
  });

  it("carries the vendor's token TOTAL, the only usage an async run reports", () => {
    const entries = convert({
      subtype: "task_notification",
      task_id: "t1",
      tool_use_id: "toolu_1",
      status: "completed",
      usage: { total_tokens: 4_242 },
    });

    const activity = activityOf(entries[0]);
    const subagent = activity?.item.value as conversationv1.AgentSubagent;
    const success = subagent.result.value as conversationv1.AgentSubagentSuccess;
    const usage = success.totals?.usage.value as conversationv1.AgentSubagentAsyncUsage;
    expect(usage.totalTokens).toBe(4_242n);
  });

  it("leaves the token total UNSET when the notification reported no usage", () => {
    const entries = convert({
      subtype: "task_notification",
      task_id: "t1",
      tool_use_id: "toolu_1",
      status: "completed",
    });

    const activity = activityOf(entries[0]);
    const subagent = activity?.item.value as conversationv1.AgentSubagent;
    const success = subagent.result.value as conversationv1.AgentSubagentSuccess;
    const usage = success.totals?.usage.value as conversationv1.AgentSubagentAsyncUsage;
    expect(usage.totalTokens).toBeUndefined();
  });

  it("reports an empty summary rather than omitting the report entirely", () => {
    const entries = convert({
      subtype: "task_notification",
      task_id: "t1",
      tool_use_id: "toolu_1",
      status: "completed",
    });

    const activity = activityOf(entries[0]);
    const subagent = activity?.item.value as conversationv1.AgentSubagent;
    const success = subagent.result.value as conversationv1.AgentSubagentSuccess;
    expect(success.report?.prose?.markdown).toBe("");
  });
});

/**
 * A detached spawn's terminal restates its spawn: the notification states
 * neither the prompt nor the start, and a replay serves the terminal alone.
 */
describe("convertDetached: a notification's terminal restates its spawn", () => {
  /** The spawning Agent call, open in the call registry when the task starts. */
  function openSpawn(): CallRegistry {
    const calls = createCallRegistry();
    calls.remember({
      toolUseId: "toolu_1",
      toolName: "Agent",
      input: { description: "tidy the docs", prompt: "Tidy every doc.", subagent_type: "general" },
      startedAtMs: 1_000,
      agentId: MAIN_AGENT,
    });
    return calls;
  }

  /** The settled spawn a start-then-notify pair produces, for a given status. */
  function settledSpawn(
    status: string,
    calls: CallRegistry = openSpawn(),
  ): conversationv1.AgentSubagent | undefined {
    const registry = createTaskKindRegistry();
    drain(
      convert({ subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1" }, {}, registry, calls),
    );
    const entries = convert(
      { subtype: "task_notification", task_id: "t1", tool_use_id: "toolu_1", status },
      {},
      registry,
      createCallRegistry(),
    );
    const settled = entries.find((entry) => entry.source.discriminator.startsWith("activity.subagent."));
    return activityOf(settled)?.item.value as conversationv1.AgentSubagent | undefined;
  }

  it.each([["completed"], ["failed"]])(
    "restates the spawn's start instant on a %s run's settle",
    (status) => {
      // Arrange + Act
      const spawn = settledSpawn(status);

      // Assert
      const settledAt =
        spawn?.result.case === "success"
          ? spawn.result.value.settledAt
          : (spawn?.result.value as conversationv1.AgentSubagentFailure).error?.settledAt;
      expect(settledAt?.startedAt?.atMs).toBe(1_000n);
    },
  );

  it.each([["completed"], ["failed"], ["stopped"]])(
    "restates the spawn's prompt on a %s run's settle",
    (status) => {
      // Arrange + Act
      const spawn = settledSpawn(status);

      // Assert
      const settled = spawn?.result.value as
        | conversationv1.AgentSubagentSuccess
        | conversationv1.AgentSubagentFailure;
      expect(settled.prompt?.description).toBe("tidy the docs");
    },
  );

  it.each([["failed"], ["stopped"]])(
    "restates the created agent on a %s run's failure",
    (status) => {
      // Arrange + Act
      const spawn = settledSpawn(status);

      // Assert
      const failure = spawn?.result.value as conversationv1.AgentSubagentFailure;
      expect(failure.createdAgentId?.value).toBe("toolu_1");
    },
  );

  it("restates an EMPTY prompt, never an invented one, when the spawning call was never seen", () => {
    // Arrange + Act
    const spawn = settledSpawn("failed", createCallRegistry());

    // Assert
    const failure = spawn?.result.value as conversationv1.AgentSubagentFailure;
    expect(failure.prompt?.text).toBe("");
  });

  it("restates the spawn's prompt on a running beat, which upserts the spawn's row", () => {
    // Arrange
    const registry = createTaskKindRegistry();
    drain(
      convert(
        { subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1" },
        {},
        registry,
        openSpawn(),
      ),
    );

    // Act
    const beat = convert(
      { subtype: "task_progress", task_id: "t1", usage: { total_tokens: 5 } },
      {},
      registry,
      createCallRegistry(),
    );

    // Assert
    const update = (activityOf(beat[0])?.item.value as conversationv1.AgentSubagent).result
      .value as conversationv1.AgentSubagentUpdate;
    expect(update.prompt?.description).toBe("tidy the docs");
  });

  it("restates no start when the fold never saw the spawning call open", () => {
    // Arrange + Act
    const spawn = settledSpawn("completed", createCallRegistry());

    // Assert
    const success = spawn?.result.value as conversationv1.AgentSubagentSuccess;
    expect(success.settledAt?.startedAt).toBeUndefined();
  });
});

describe("convertDetached: a task subtype no converter owns", () => {
  it("lands as residue named for the subtype", () => {
    const entries = convert({ subtype: "task_teleported", task_id: "t1" });

    expect(entries[0]?.source.discriminator).toBe("unknown.task_teleported");
  });
});

// ---------------------------------------------------------------------------
// Foreground work is never detached work
// ---------------------------------------------------------------------------

describe("the shared rule: when a task is detached work", () => {
  it.each([
    { name: "a foreground start", started: { is_backgrounded: false }, want: true },
    { name: "a background start", started: { is_backgrounded: true }, want: false },
    { name: "a start of a kind the vendor does not flag", started: {}, want: false },
  ])("reads $name as foreground=$want", ({ started, want }) => {
    expect(startedInForeground(started)).toBe(want);
  });

  it.each([
    { name: "a patch that backgrounds", patch: { is_backgrounded: true }, want: true },
    { name: "a patch that says foreground", patch: { is_backgrounded: false }, want: false },
    { name: "a patch that states no side", patch: {}, want: false },
    { name: "no patch at all", patch: undefined, want: false },
  ])("reads $name as a move=$want", ({ patch, want }) => {
    expect(patchBackgrounds(patch)).toBe(want);
  });

  it.each([
    { name: "a result naming its background task", structured: { backgroundTaskId: "b1" }, want: "b1" },
    { name: "a result that ended its work", structured: { stdout: "done" }, want: undefined },
    { name: "an EMPTY background task id", structured: { backgroundTaskId: "" }, want: undefined },
    { name: "a non-string background task id", structured: { backgroundTaskId: 7 }, want: undefined },
    { name: "no structured result", structured: undefined, want: undefined },
  ])("reads $name as moved task $want", ({ structured, want }) => {
    expect(resultBackgroundTaskId(structured)).toBe(want);
  });
});

describe("convertDetached: foreground work", () => {
  // THE 0.3.280 VENDOR starts a task for every `Bash` call and every
  // synchronous spawn; `is_backgrounded: false` says the call blocks on it.
  const KINDS = [{ kind: "local_bash" }, { kind: "local_agent" }];

  const foregroundStart = (kind: string) => ({
    subtype: "task_started",
    task_id: "t1",
    tool_use_id: "toolu_1",
    task_type: kind,
    is_backgrounded: false,
  });

  const notification = {
    subtype: "task_notification",
    task_id: "t1",
    tool_use_id: "toolu_1",
    output_file: "/tmp/t1.output",
    status: "completed",
  };

  it("announces no detachment when a SYNCHRONOUS spawn starts", () => {
    expect(convert(foregroundStart("local_agent"))).toEqual([]);
  });

  it("announces no detachment when a foreground Bash starts", () => {
    expect(convert(foregroundStart("local_bash"))).toEqual([]);
  });

  it("records a foreground start at debug, naming the task", () => {
    // Arrange.
    const written = vi.mocked(writeSync);
    const before = written.mock.calls.length;

    // Act.
    convert(foregroundStart("local_bash"));

    // Assert.
    const records = (written.mock.calls.slice(before) as unknown as Array<[number, Buffer, number, number]>)
      .map(([, bytes, offset, length]) =>
        JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as Record<string, unknown>,
      )
      .filter((record) => record.message === "a task started in the foreground; it is not detached work and nothing is announced")
      .map((record) => ({ level: record.level, task: (record.context as Record<string, unknown>).task_id }));
    expect(records).toEqual([{ level: "debug", task: "t1" }]);
  });

  it.each(KINDS)("writes nothing when a foreground $kind concludes without ever moving", ({ kind }) => {
    // Arrange.
    const registry = createTaskKindRegistry();
    convert(foregroundStart(kind), {}, registry);

    // Act.
    const entries = convert(notification, {}, registry);

    // Assert.
    expect(entries).toEqual([]);
  });

  it("announces `by_user` when a patch moves a foreground spawn", () => {
    // Arrange.
    const registry = createTaskKindRegistry();
    convert(foregroundStart("local_agent"), {}, registry);

    // Act.
    const entries = convert(
      { subtype: "task_updated", task_id: "t1", tool_use_id: "toolu_1", patch: { is_backgrounded: true } },
      {},
      registry,
    );

    // Assert.
    expect(detachedOrigin(entries[0]).cause.case).toBe("byUser");
  });

  it("settles a foreground spawn that a patch moved, at its notification", () => {
    // Arrange.
    const registry = createTaskKindRegistry();
    convert(foregroundStart("local_agent"), {}, registry);
    convert(
      { subtype: "task_updated", task_id: "t1", tool_use_id: "toolu_1", patch: { is_backgrounded: true } },
      {},
      registry,
    );

    // Act.
    const entries = convert(notification, {}, registry);

    // Assert.
    expect(entries.map((entry) => entry.source.discriminator)).toEqual([
      "agent_frame.detached_work.detached.by_user",
      "activity.subagent.success",
    ]);
  });

  it("upserts the moved shell's announcement at its notification once its result stated the move", () => {
    // Arrange.
    const registry = createTaskKindRegistry();
    convert(foregroundStart("local_bash"), {}, registry);
    drain([bashDetachment({ backgroundTaskId: "t1", timedOutAfterMs: 120_000 }, undefined, registry)]);

    // Act.
    const entries = convert(notification, {}, registry);

    // Assert.
    expect(detachedOrigin(entries[0]).cause.case).toBe("timedOut");
  });
});

// ---------------------------------------------------------------------------
// WHOSE WORK IT IS: the owner is the spawning call's agent, never the book
// ---------------------------------------------------------------------------

describe("convertDetached: the owner of task-stream work", () => {
  const SUBAGENT = create(conversationv1.AgentIdSchema, { value: "toolu_parent_spawn" });

  /** A call registry holding one open call, made by `agent`. */
  function callsWith(toolUseId: string, agent: conversationv1.AgentId): CallRegistry {
    const calls = createCallRegistry();
    calls.remember({ toolUseId, toolName: "Agent", input: {}, startedAtMs: 1, agentId: agent });
    return calls;
  }

  /** The announcement a converted batch carries. */
  function announced(entries: readonly PersistEntry[]): conversationv1.AgentDetachedWork | undefined {
    const row = entries.find((entry) => entry.source.discriminator.startsWith("agent_frame.detached_work"));
    return row === undefined ? undefined : announcement(row);
  }

  const STARTED = { subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1", task_type: "local_agent" };

  it("names the agent that made the spawning call as the owner", () => {
    // Arrange.
    const calls = callsWith("toolu_1", SUBAGENT);

    // Act.
    const entries = convert(STARTED, {}, createTaskKindRegistry(), calls);

    // Assert.
    expect(announced(entries)?.owner?.value).toBe(SUBAGENT.value);
  });

  it("still rides the main agent's book, which is the announcer and not the owner", () => {
    // Arrange.
    const calls = callsWith("toolu_1", SUBAGENT);

    // Act.
    const entries = convert(STARTED, {}, createTaskKindRegistry(), calls);

    // Assert.
    expect(entries[0]?.agentId.value).toBe(MAIN_AGENT.value);
  });

  it("restates the owner on the closing notification after the call itself was forgotten", () => {
    // Arrange: the start saw the open call; by the notification it is gone.
    const registry = createTaskKindRegistry();
    drain(convert(STARTED, {}, registry, callsWith("toolu_1", SUBAGENT)));

    // Act.
    const entries = convert(
      { subtype: "task_notification", task_id: "t1", tool_use_id: "toolu_1", status: "completed", output_file: "/tmp/t1.output" },
      {},
      registry,
      createCallRegistry(),
    );

    // Assert.
    expect(announced(entries)?.owner?.value).toBe(SUBAGENT.value);
  });

  it("leaves the owner UNSET for a call the fold never saw, rather than naming the main agent", () => {
    // Arrange: a backgrounded subagent's own call never reaches this stream.
    const calls = createCallRegistry();

    // Act.
    const entries = convert(
      { subtype: "task_notification", task_id: "t9", tool_use_id: "toolu_unseen", status: "completed", output_file: "/tmp/t9.output" },
      {},
      createTaskKindRegistry(),
      calls,
    );

    // Assert.
    expect(announced(entries)?.owner).toBeUndefined();
  });
});

describe("convertDetached: the call's handoff", () => {
  /** A call registry holding one spawning call. */
  function holding(toolUseId: string): CallRegistry {
    const calls = createCallRegistry();
    calls.remember({ toolUseId, toolName: "Agent", input: {}, startedAtMs: 1, agentId: MAIN_AGENT });
    return calls;
  }

  it("hands the call off when a task says it started in the background", () => {
    // Arrange
    const calls = holding("toolu_1");

    // Act
    convert(
      { subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1", task_type: "local_agent", is_backgrounded: true },
      {},
      createTaskKindRegistry(),
      calls,
    );

    // Assert
    expect(calls.isDetached("toolu_1")).toBe(true);
  });

  it("does not hand the call off when the task states no background flag", () => {
    // Arrange: an older vendor's foreground work states no flag either.
    const calls = holding("toolu_1");

    // Act
    convert(
      { subtype: "task_started", task_id: "t1", tool_use_id: "toolu_1", task_type: "local_agent" },
      {},
      createTaskKindRegistry(),
      calls,
    );

    // Assert
    expect(calls.isDetached("toolu_1")).toBe(false);
  });

  it("hands the call off when a person backgrounds the work by hand", () => {
    // Arrange
    const calls = holding("toolu_1");

    // Act
    convert(
      { subtype: "task_updated", task_id: "t1", tool_use_id: "toolu_1", patch: { is_backgrounded: true } },
      {},
      createTaskKindRegistry(),
      calls,
    );

    // Assert
    expect(calls.isDetached("toolu_1")).toBe(true);
  });
});
