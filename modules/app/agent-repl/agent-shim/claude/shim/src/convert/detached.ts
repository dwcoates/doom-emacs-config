/**
 * convert/detached.ts — WORK THAT LEFT THE TURN.
 *
 * # One row, upserted as knowledge improves
 *
 * A detachment is learned in pieces: `task_started` says a handle exists and
 * which call it came from, the BASH TOOL RESULT says WHY it detached (the task
 * stream carries no cause on any frame), `task_updated` says a person
 * backgrounded it by hand, and `task_notification` says where its output is and
 * that it ended. All four write the SAME row under the same upsert key, so the
 * announcement improves in place rather than appearing four times. That is what
 * upsert-by-identity is for, and it is why nothing here has to accumulate.
 *
 * # Liveness is structural
 *
 * `background_tasks_changed` is a LEVEL with replace semantics, and the shim
 * consumes it WITHOUT diffing it and without pairing the edge bookends into a
 * retained set. The set of open detached-item streams IS the live set, and that
 * is the engine's business — so this file produces nothing from the level.
 *
 * # `skip_transcript`
 *
 * An ambient housekeeping task is dropped from every announcement: no bubble, no
 * row. The vendor's own level set still governs liveness, so no indicator wedges.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import {
  activityUpsertKey,
  bashTerminalUpsertKey,
  detachedWorkUpsertKey,
  terminalUpsertKey,
} from "../store/keys.js";
import type { PersistEntry } from "../store/persistence.js";
import { agentFrame, prose, settledAt, updateFrame } from "./entries.js";
import type { FoldContext } from "./fold-context.js";
import { detachedWorkId, subagentId, toolCallActivityId } from "./ids.js";
import { residueEntry, residueForMessage } from "./residue.js";
import { subagentPrompt } from "./tools/subagent.js";
import type { CallRegistry, PendingCall } from "./tool-calls.js";
import { activityEntry, agentActivity } from "./entries.js";

const LOGGER = bindLog({ component: "shim-convert-detached", operation: "shim.convert.detached" });

/** Why a unit left the turn. */
export type DetachCause = "requested" | "by_user" | "timed_out";

// ---------------------------------------------------------------------------
// FOREGROUND WORK IS NEVER DETACHED WORK — the one rule, shared.
// ---------------------------------------------------------------------------
//
// The vendor tracks FOREGROUND work as a task too: from SDK 0.3.280 every
// `Bash` call and every synchronous `Agent` spawn emits `task_started`, and
// `is_backgrounded` is what says which side of the line the work began on
// ("registered in the background (true) or in the foreground with the spawning
// tool call blocking on it (false)"; set for `local_agent` and `local_bash`).
// A task is detached work when ANY of three statements says so:
//
//   - it STARTED backgrounded — `task_started.is_backgrounded` is not `false`
//     (a kind the vendor does not flag, a monitor or a workflow, has no
//     foreground phase at all);
//   - a later PATCH moved it — `task_updated.patch.is_backgrounded: true`, a
//     hand-backgrounded shell or agent;
//   - its own TOOL RESULT says it moved — a `Bash` result naming a
//     `backgroundTaskId` (a timeout, a Ctrl-B, a `run_in_background` launch).
//
// The converter decides what to ANNOUNCE by these three and the engine decides
// what is LIVE by the same three, which is why they are written once, here. A
// foreground task left out of this rule was announced and recorded as detached
// work, and every ordinary shell call drew as a detached shell that never
// settled.

/** Whether a started task began in the FOREGROUND, its spawning call blocking on it. */
export function startedInForeground(started: { readonly is_backgrounded?: boolean }): boolean {
  return started.is_backgrounded === false;
}

/** Whether a task patch moves the task to the background. */
export function patchBackgrounds(
  patch: { readonly is_backgrounded?: boolean } | undefined,
): boolean {
  return patch?.is_backgrounded === true;
}

/**
 * The vendor task a tool result says MOVED to the background, when it says one did.
 *
 * `toolUseResult.backgroundTaskId` is the vendor's own statement that the call's
 * work left rather than ended; a result without it ended its work.
 */
export function resultBackgroundTaskId(structured: unknown): string | undefined {
  if (typeof structured !== "object" || structured === null) return undefined;
  const taskId = (structured as { backgroundTaskId?: unknown }).backgroundTaskId;
  return typeof taskId === "string" && taskId !== "" ? taskId : undefined;
}

/** The cause arm, with the timeout figure the timed-out arm carries. */
function detachCause(
  cause: DetachCause,
  timeoutMs: number | undefined,
): conversationv1.DetachedWorkDetached["cause"] {
  switch (cause) {
    case "by_user":
      return { case: "byUser", value: create(conversationv1.DetachedCauseByUserSchema, {}) };
    case "timed_out":
      return {
        case: "timedOut",
        value: create(conversationv1.DetachedCauseTimedOutSchema, {
          // THE CONFIGURED LIMIT, not the work's runtime: the work is still
          // running, so its runtime is not yet a fact.
          timeoutMs: BigInt(Math.max(0, Math.trunc(timeoutMs ?? 0))),
        }),
      };
    default:
      return { case: "requested", value: create(conversationv1.DetachedCauseRequestedSchema, {}) };
  }
}

/** Where a detached unit's output is accumulating, and whether it may be opened. */
function detachedOutput(path: string, readable: boolean): conversationv1.DetachedWorkOutput {
  return create(conversationv1.DetachedWorkOutputSchema, {
    path,
    readability: readable
      ? { case: "readable", value: create(conversationv1.DetachedWorkOutputReadableSchema, {}) }
      : {
          case: "unreadable",
          value: create(conversationv1.DetachedWorkOutputUnreadableSchema, {}),
        },
  });
}

/** What one detachment announcement says. */
interface DetachmentFacts {
  /**
   * The in-turn unit it detached FROM — and, by the same bytes, the HANDLE the
   * work is addressed by.
   *
   * `DetachedWorkId.value == AgentActivityId.value` (ruling, landing 3): one
   * identity addresses the work, its unit, and (for a subagent) its book, so a
   * terminal retires the handle by equality. The vendor's `task_id` stays
   * shim-side as the lookup for `stopTask` and the live level.
   */
  readonly detachedFromToolUseId: string;
  /** Why it left. */
  readonly cause: DetachCause;
  /** The timeout that was exceeded, for the timed-out cause. */
  readonly timeoutMs?: number;
  /** Where its output accumulates, when the producer named a file. */
  readonly outputPath?: string;
  /** Whether this reader may open that file. */
  readonly outputReadable?: boolean;
  /**
   * WHOSE WORK IT IS: the agent that made the spawning call, when this fold
   * observed that call. UNDEFINED when it did not — a backgrounded subagent's
   * own calls never reach this stream — and then the announcement leaves the
   * owner unset rather than naming the agent whose book it happens to ride.
   */
  readonly owner?: conversationv1.AgentId;
}

/**
 * One detachment announcement, as a row.
 *
 * Always the `detached` arm: ALL THREE CAUSES ARRIVE ON IT, including work that
 * asked for the background up front — such a call is streamed as a progress item
 * before it backgrounds, so it always has an item it detached FROM; it simply
 * never has a foreground running phase.
 */
function detachmentEntry(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  vendorUuid: string,
  facts: DetachmentFacts,
): PersistEntry {
  const work = detachedWorkId(facts.detachedFromToolUseId);
  const announcement = create(conversationv1.AgentDetachedWorkSchema, {
    work,
    owner: facts.owner,
    output:
      facts.outputPath === undefined
        ? undefined
        : detachedOutput(facts.outputPath, facts.outputReadable !== false),
    origin: {
      case: "detached",
      value: create(conversationv1.DetachedWorkDetachedSchema, {
        detachedFromId: toolCallActivityId(facts.detachedFromToolUseId),
        cause: detachCause(facts.cause, facts.timeoutMs),
      }),
    },
  });
  return {
    agentId,
    upsertKey: detachedWorkUpsertKey(work),
    source: {
      vendorUuid,
      discriminator: `agent_frame.detached_work.detached.${facts.cause}`,
    },
    keepalive: context.keepalive,
    item: {
      kind: "frame",
      frame: agentFrame(agentId, {
        case: "detachedWork",
        value: announcement,
      }),
    },
  };
}

/**
 * The detachment a BACKGROUNDED SHELL COMMAND's own result announces.
 *
 * BACKGROUNDING CAUSES FOR SHELLS ARE HARVESTED FROM THE BASH TOOL RESULT —
 * `backgroundedByUser` and `timedOutAfterMs` — and never from the task stream,
 * which carries no cause on any frame. The call's own `run_in_background` is the
 * ordinary path.
 */
export function bashDetachmentEntry(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  vendorUuid: string,
  toolUseId: string,
  structured: unknown,
  resultContent?: conversationv1.ToolResultContent,
  taskKinds?: TaskKindRegistry,
): PersistEntry | undefined {
  const output = structured as Record<string, unknown> | undefined;
  // THE VENDOR'S TASK ID IS ONLY EVIDENCE THAT IT BACKGROUNDED, not the handle:
  // the wire handle is the spawning call's own id (ruling, landing 3), and the
  // task id stays shim-side for `stopTask` and the live level.
  const vendorTaskId = resultBackgroundTaskId(structured);
  if (vendorTaskId === undefined) return undefined;
  const timedOut = output?.timedOutAfterMs;
  const cause: DetachCause =
    typeof timedOut === "number"
      ? "timed_out"
      : output?.backgroundedByUser === true
        ? "by_user"
        : "requested";
  LOGGER.info(
    { vendor_task_id: vendorTaskId, work: toolUseId, tool_use_id: toolUseId, cause },
    "a shell command moved to the background rather than ending",
  );
  // SO THE `task_notification` DOES NOT OVERWRITE IT with a hard-coded
  // `requested` when it upserts the same row to add the output path.
  taskKinds?.rememberCause(vendorTaskId, cause);
  return detachmentEntry(context, agentId, vendorUuid, {
    detachedFromToolUseId: toolUseId,
    cause,
    // THE CALL'S OWN AGENT OWNS IT: this result settles a call the fold
    // registered under the agent that made it.
    owner: agentId,
    timeoutMs: typeof timedOut === "number" ? timedOut : undefined,
    // WHERE THE OUTPUT PATH ACTUALLY COMES FROM. `toolUseResult` on a
    // backgrounded Bash carries `backgroundTaskId` and NOTHING ELSE — the
    // corpus capture (testdata/corpus/tool-results/bash-background.jsonl) has
    // no `persistedOutputPath` — and the vendor states the path only in the
    // result's PROSE. Reading `persistedOutputPath` alone therefore left every
    // detached shell announced with no output at all, so `readability` was
    // unset and a surface had no file to offer once the stream was gone.
    //
    // `persistedOutputPath` is still preferred where the vendor sets it (a
    // SPILLED foreground result does): a declared field outranks a sentence.
    outputPath:
      typeof output?.persistedOutputPath === "string"
        ? output.persistedOutputPath
        : outputPathFromProse(resultContent),
    // READABLE: the vendor's own sentence tells the model to Read that path,
    // so the file it names is one this reader may open.
    outputReadable: true,
  });
}

/**
 * The spool path the vendor's backgrounding sentence names, if it named one.
 *
 * PROSE IS NOT A PREFERENCE, IT IS THE ONLY STATEMENT. The captured sentence is
 * "Command running in background with ID: <id>. Output is being written to:
 * <path>. ..." and the path is the vendor's sole account of where a detached
 * shell's output accumulates at announcement time — `task_notification` repeats
 * it, but only once the run has ENDED, which is far too late for the
 * announcement that tells a consumer to open a stream.
 *
 * A sentence the vendor rewords yields no path rather than a wrong one: the
 * announcement then carries no output, exactly as it did before, and the
 * `task_notification` still supplies it at the end.
 */
export function outputPathFromProse(
  content: conversationv1.ToolResultContent | undefined,
): string | undefined {
  if (content === undefined) return undefined;
  const prose = content.blocks
    .map((block) => (block.block.case === "text" ? block.block.value.text : ""))
    .join("\n");
  const match = /Output is being written to:\s*(\S+?)\.?(?:\s|$)/.exec(prose);
  const path = match?.[1];
  if (path === undefined || path === "") {
    LOGGER.logVerbose(
      {},
      "the backgrounding result stated no output path; the announcement carries none",
    );
    return undefined;
  }
  return path;
}

/** The vendor's task-stream messages, read loosely for their optional fields. */
interface RawTask {
  readonly subtype?: string;
  readonly task_id?: string;
  readonly task_type?: string;
  readonly tool_use_id?: string;
  readonly skip_transcript?: boolean;
  readonly is_backgrounded?: boolean;
  readonly output_file?: string;
  readonly status?: string;
  readonly summary?: string;
  readonly usage?: {
    readonly total_tokens?: number;
    // A RUNNING BEAT'S EXTRA FIGURES. `task_progress` states the tool-call count
    // and elapsed wall-clock alongside the running token sum (`tool_uses`,
    // `duration_ms` in the vendor's `usage`); the settled `task_notification`
    // carries only `total_tokens`, so these two are absent there.
    readonly tool_uses?: number;
    readonly duration_ms?: number;
  };
  readonly patch?: { readonly is_backgrounded?: boolean; readonly status?: string };
}

/**
 * WHAT KIND OF WORK EACH LIVE TASK IS, from the one message that says.
 *
 * `task_started` states `task_type` — `local_agent` for a spawned agent,
 * `local_bash` for a shell command that outlived its timeout — and
 * `task_notification` states NOTHING about the kind. Without this join a
 * notification cannot tell the two apart, and the shell case is not a harmless
 * ambiguity: settling a Bash unit with an `AgentSubagent` terminal restates that
 * unit as a subagent run and invents an empty spawn prompt for it (observed in
 * the `ctrl-b-detach-of-foreground-work` capture, where the moved-to-background
 * `Bash` call was settled as `activity.subagent.success`).
 *
 * BOUNDED AND SELF-EMPTYING, like the call registry beside it: an entry is
 * dropped the moment its task settles, and the table is capped so a vendor that
 * starts tasks and never notifies cannot grow it without end.
 */
export const TASK_KIND_CAPACITY = 512;

/**
 * The kinds of the tasks in flight, held BY THE FOLD and never module-wide: two
 * concurrent sessions must not be able to read each other's tasks.
 */
export interface TaskKindRegistry {
  /** Remember one task's kind, forgetting the oldest when the cap is reached. */
  remember(taskId: string, taskType: string): void;
  /**
   * Remember WHY a task's work left the turn.
   *
   * THE CAUSE IS STATED ONCE AND RESTATED NEVER. A shell's cause rides its own
   * tool result (`backgroundedByUser`, `timedOutAfterMs`); an agent's rides
   * `task_started` or, for a hand-backgrounded one, `task_updated`. The
   * `task_notification` that later supplies the output path upserts THE SAME
   * ROW, so a notification that restated a hard-coded `requested` would
   * silently overwrite a `by_user` or `timed_out` cause with the wrong one.
   */
  rememberCause(taskId: string, cause: DetachCause): void;
  /** The cause remembered for a task, or `undefined` if none was stated. */
  causeOf(taskId: string): DetachCause | undefined;
  /**
   * Remember which spawning call a task's frames belong to.
   *
   * THE JOIN A RUNNING BEAT NEEDS. `task_progress` is the only per-task message
   * a running spawn emits, and the subagent unit it advances is keyed by the
   * SPAWN's `tool_use_id` (`toolCallActivityId(toolUseId)`). Its earlier
   * messages (`task_started`) state that id; a later `task_progress` need not
   * restate it, so it is remembered when first stated and recovered here rather
   * than dropped for want of a correlation.
   */
  rememberToolUse(taskId: string, toolUseId: string): void;
  /** The spawning call remembered for a task, or `undefined` if none was stated. */
  toolUseFor(taskId: string): string | undefined;
  /**
   * Remember WHOSE work a task is: the agent that made its spawning call.
   *
   * LEARNED ONCE, while the call is still open. The task stream names no agent
   * at all, and the call registry forgets a call the moment its result lands —
   * which, for a backgrounded launch, is long before the `task_notification`
   * that upserts the announcement with its output path. Without this the
   * closing upsert could not restate the owner the opening one stated.
   */
  rememberOwner(taskId: string, owner: conversationv1.AgentId): void;
  /** The owner remembered for a task, or `undefined` if the fold never saw its call. */
  ownerOf(taskId: string): conversationv1.AgentId | undefined;
  /**
   * Remember the SPAWNING CALL itself: what it was asked and when it started.
   *
   * LEARNED ONCE, while the call is still open, exactly as the owner is. A
   * `task_notification` states neither the spawn's prompt nor its start
   * instant, yet the terminal it produces is a SETTLED FRAME, and a settled
   * frame stands alone: a replay serves it with no start beside it, so it must
   * restate both. The call is the one authority for them, and this table is
   * where it survives the call registry forgetting it.
   */
  rememberCall(taskId: string, call: PendingCall): void;
  /** The spawning call remembered for a task, or `undefined` if the fold never saw it open. */
  callOf(taskId: string): PendingCall | undefined;
  /**
   * Whether a settling task is an AGENT run, and so owns a subagent terminal.
   *
   * A task whose kind was never stated answers `true`: the subagent terminal is
   * the long-standing behavior for an untyped task, and silently dropping it
   * would lose a real settle. Only a task the vendor NAMED as something other
   * than an agent is refused one. Answering also FORGETS the task, which is
   * what keeps the table self-emptying.
   */
  settlesAsSubagent(taskId: string): boolean;
  /**
   * Remember that a task STARTED in the foreground ({@link startedInForeground}).
   *
   * Its later detachment is stated by a cause — a patch or its own tool result
   * — so a foreground task with no remembered cause never left the turn.
   */
  rememberForeground(taskId: string): void;
  /**
   * Whether a settling task is FOREGROUND WORK that never left the turn: it
   * started in the foreground and no patch or tool result ever gave it a
   * cause. Such a task's own tool result settles its unit, so its notification
   * writes nothing. Answering `true` also FORGETS the task.
   */
  concludesInForeground(taskId: string): boolean;
}

export function createTaskKindRegistry(): TaskKindRegistry {
  const facts = new Map<
    string,
    {
      kind?: string;
      cause?: DetachCause;
      toolUseId?: string;
      foreground?: boolean;
      owner?: conversationv1.AgentId;
      call?: PendingCall;
    }
  >();
  /** Make room for one more task, forgetting the oldest when the cap is hit. */
  const reserve = (): void => {
    if (facts.size < TASK_KIND_CAPACITY) return;
    const [oldest] = facts.keys();
    if (oldest === undefined) return;
    facts.delete(oldest);
    // warn: a defect because bounded task bookkeeping discarded facts needed to type a later notification.
    LOGGER.warn(
      { task_id: oldest },
      "the task-facts table is full; the oldest task's kind and cause are forgotten and its notification cannot be typed",
    );
  };
  return {
    remember(taskId, taskType) {
      reserve();
      facts.set(taskId, { ...facts.get(taskId), kind: taskType });
    },
    rememberCause(taskId, cause) {
      reserve();
      facts.set(taskId, { ...facts.get(taskId), cause });
    },
    causeOf(taskId) {
      return facts.get(taskId)?.cause;
    },
    rememberToolUse(taskId, toolUseId) {
      reserve();
      facts.set(taskId, { ...facts.get(taskId), toolUseId });
    },
    toolUseFor(taskId) {
      return facts.get(taskId)?.toolUseId;
    },
    rememberOwner(taskId, owner) {
      reserve();
      facts.set(taskId, { ...facts.get(taskId), owner });
    },
    ownerOf(taskId) {
      return facts.get(taskId)?.owner;
    },
    rememberCall(taskId, call) {
      reserve();
      facts.set(taskId, { ...facts.get(taskId), call });
    },
    callOf(taskId) {
      return facts.get(taskId)?.call;
    },
    settlesAsSubagent(taskId) {
      const kind = facts.get(taskId)?.kind;
      facts.delete(taskId);
      return kind === undefined || kind === "local_agent";
    },
    rememberForeground(taskId) {
      reserve();
      facts.set(taskId, { ...facts.get(taskId), foreground: true });
    },
    concludesInForeground(taskId) {
      const fact = facts.get(taskId);
      if (fact?.foreground !== true || fact.cause !== undefined) return false;
      facts.delete(taskId);
      return true;
    },
  };
}

/** Every task-stream message. */
export function convertDetached(
  message: Extract<SdkMessage, { type: "system" }>,
  context: FoldContext,
  taskKinds: TaskKindRegistry,
  calls: CallRegistry,
): readonly PersistEntry[] {
  const raw = message as unknown as RawTask;
  const uuid = (message as { uuid: string }).uuid;

  if (raw.subtype === "background_tasks_changed") {
    // A LEVEL, consumed without diffing: the live set is the set of open
    // detached-item streams, which is the engine's structural fact, not a row.
    LOGGER.logVerbose({ uuid }, "the background-task level changed; consumed, never recorded");
    return [];
  }

  if (raw.skip_transcript === true) {
    LOGGER.logVerbose(
      { uuid, task_id: raw.task_id },
      "an ambient task is dropped from every announcement; the vendor's level still governs liveness",
    );
    return [];
  }

  const taskId = raw.task_id;
  if (typeof taskId !== "string" || taskId === "") {
    LOGGER.debug(
      { uuid, subtype: raw.subtype },
      "a task message named no task; nothing can be addressed by it",
    );
    return [residueEntry(context, message, residueForMessage(message, "task message has no task_id"), "residue.unparsed")];
  }

  const known = context.liveTask(taskId);
  // A DIRECTLY STATED spawning call is remembered, so a later `task_progress`
  // that does not restate it can still be joined to its subagent unit; a
  // message that states none falls back to the remembered join.
  const statedToolUse = raw.tool_use_id ?? known?.toolUseId;
  if (statedToolUse !== undefined && statedToolUse !== "") {
    taskKinds.rememberToolUse(taskId, statedToolUse);
  }
  const toolUseId = statedToolUse ?? taskKinds.toolUseFor(taskId);
  const agentId = known?.agentId ?? context.mainAgentId;
  // WHOSE WORK THIS IS, which is NOT the book above: the task stream is
  // session-wide, so every announcement it yields rides the main agent's book
  // whoever spawned the work. The owner is the agent that made the spawning
  // call, read off the call registry while that call is still open and
  // remembered for the task's later messages. A call this fold never saw — a
  // backgrounded subagent's own calls never reach this stream — leaves it
  // UNDEFINED, and the announcement says so rather than naming the main agent.
  const openCall = toolUseId === undefined || toolUseId === "" ? undefined : calls.peek(toolUseId);
  if (openCall !== undefined) {
    taskKinds.rememberOwner(taskId, openCall.agentId);
    taskKinds.rememberCall(taskId, openCall);
  }
  const owner = openCall?.agentId ?? taskKinds.ownerOf(taskId);
  // Read NOW, before a settling notification forgets the task's facts.
  const spawnCall = openCall ?? taskKinds.callOf(taskId);

  switch (raw.subtype) {
    case "task_started": {
      if (toolUseId === undefined || toolUseId === "") {
        // NO ORIGINATING CALL AND NO PAYLOAD WE CAN BUILD: `created` needs a
        // DetachableWork describing the work, and nothing here states one. An
        // invented description would be worse than residue.
        // warn: a defect because an unowned task start cannot be announced on the wire.
        LOGGER.warn(
          { uuid, task_id: taskId },
          "a task started with no originating call; it cannot be announced and lands as residue",
        );
        return [
          residueEntry(
            context,
            message,
            residueForMessage(message, "task_started names no originating call"),
            "residue.unknown.task_started",
          ),
        ];
      }
      if (raw.task_type !== undefined && raw.task_type !== "") {
        taskKinds.remember(taskId, raw.task_type);
      }
      // FOREGROUND WORK IS NEVER DETACHED WORK. The vendor tracks a blocking
      // `Bash` call or a synchronous spawn as a task from its first moment, and
      // `is_backgrounded: false` says the spawning call is still waiting on it.
      // Announcing it here put every ordinary shell call on the stream as a
      // detached shell. If it moves later, the patch or its own tool result
      // says so, and that is where the announcement is made.
      if (startedInForeground(raw)) {
        taskKinds.rememberForeground(taskId);
        LOGGER.debug(
          { uuid, task_id: taskId, tool_use_id: toolUseId, task_type: raw.task_type ?? "" },
          "a task started in the foreground; it is not detached work and nothing is announced",
        );
        return [];
      }
      // WORK THE VENDOR SAYS STARTED IN THE BACKGROUND is HANDED OFF on this
      // plane: a backgrounded agent's calls reach this stream without their
      // results, and a stop must not cut work still running elsewhere. Only
      // the explicit flag says so — an older vendor's `task_started` states no
      // `is_backgrounded` for foreground work too. The call itself stays held
      // until its own receipt, which still restates its facts.
      if (raw.is_backgrounded === true) calls.detach(toolUseId);
      // A SHELL TASK IS NOT A DETACHMENT YET. The vendor tracks a FOREGROUND
      // shell as a task the moment it starts — that is what makes Ctrl-B
      // addressable at all — so `task_started` says nothing about whether the
      // work left the turn, let alone why. The Bash result is the one record
      // that states the cause (`backgroundedByUser`, `timedOutAfterMs`, or
      // neither for a `run_in_background` launch), and it is where the
      // announcement is made. Announcing `requested` here instead put a
      // WRONG-CAUSE announcement on the stream ahead of the right one, which is
      // exactly what `!ctrl-b` observed. Symmetrical with `task_notification`,
      // which already refuses to settle a shell task's unit for the same
      // reason. (No real capture carries a `task` message at all; the ctrl-b
      // and timeout captures announce from the result.)
      if (raw.task_type === "local_bash") {
        LOGGER.debug(
          { uuid, task_id: taskId, tool_use_id: toolUseId },
          "a shell task started; its own tool result announces the detachment and states the cause",
        );
        return [];
      }
      taskKinds.rememberCause(taskId, "requested");
      LOGGER.info(
        { uuid, task_id: taskId, tool_use_id: toolUseId, task_type: raw.task_type ?? "" },
        "work left the turn",
      );
      return [
        detachmentEntry(context, agentId, uuid, {
          detachedFromToolUseId: toolUseId,
          cause: "requested",
          owner,
        }),
      ];
    }

    case "task_updated": {
      if (!patchBackgrounds(raw.patch)) {
        LOGGER.logVerbose(
          { uuid, task_id: taskId, status: raw.patch?.status },
          "a task patch with no detachment fact; the unit's own frames carry the rest",
        );
        return [];
      }
      if (toolUseId === undefined || toolUseId === "") {
        // warn: a defect because a hand-backgrounded task without an origin cannot update a unit.
        LOGGER.warn(
          { uuid, task_id: taskId },
          "a task was backgrounded by hand but names no originating call; nothing can be upserted",
        );
        return [];
      }
      // THE CANDIDATE PRODUCER FOR A BACKGROUNDED AGENT, confirmed at this wave:
      // a person backgrounded running work by hand, which the shell path
      // harvests from its tool result and the agent path only states here.
      LOGGER.info({ uuid, task_id: taskId }, "a person backgrounded running work by hand");
      calls.detach(toolUseId);
      taskKinds.rememberCause(taskId, "by_user");
      return [
        detachmentEntry(context, agentId, uuid, {
          detachedFromToolUseId: toolUseId,
          cause: "by_user",
          owner,
        }),
      ];
    }

    case "task_progress": {
      // A RUNNING BEAT IS THE SUBAGENT UNIT'S `update` ARM. It carries the
      // running token sum a BACKGROUNDED spawn would otherwise surface nowhere
      // until it settled — an awaited spawn streams the same figure from its
      // transcript, but a detached one's transcript does not reach this stream,
      // so this beat is the only account of what it has spent so far.
      if (toolUseId === undefined || toolUseId === "") {
        // NO KNOWN CORRELATION: a beat that arrived before its spawn's own
        // messages stated the call it belongs to cannot be keyed to a unit, and
        // guessing one would advance the wrong row. Dropped, as before.
        LOGGER.logVerbose(
          { uuid, task_id: taskId },
          "a task-progress beat names no spawning call and none is remembered; it cannot advance a unit and is dropped",
        );
        return [];
      }
      LOGGER.logVerbose(
        { uuid, task_id: taskId, tool_use_id: toolUseId, total_tokens: raw.usage?.total_tokens ?? 0 },
        "a running beat advances the subagent unit's spend",
      );
      return subagentProgressEntries(context, agentId, uuid, toolUseId, raw, spawnCall);
    }

    case "task_notification": {
      if (taskKinds.concludesInForeground(taskId)) {
        // FOREGROUND WORK THAT NEVER LEFT THE TURN ends on its own tool result,
        // which settles its unit. A detachment upsert here would announce, at
        // its very end, work that was never detached, and a subagent terminal
        // would settle a synchronous spawn a second time.
        LOGGER.debug(
          { uuid, task_id: taskId, tool_use_id: toolUseId ?? "" },
          "foreground work concluded without ever leaving the turn; its own tool result settles it",
        );
        return [];
      }
      const entries: PersistEntry[] = [];
      if (toolUseId !== undefined && toolUseId !== "" && raw.output_file !== undefined) {
        entries.push(
          detachmentEntry(context, agentId, uuid, {
            detachedFromToolUseId: toolUseId,
            // THE REMEMBERED CAUSE, never a fresh `requested`: this row is an
            // UPSERT of the announcement already made, and the notification
            // states nothing about why the work left.
            cause: taskKinds.causeOf(taskId) ?? "requested",
            outputPath: raw.output_file,
            outputReadable: true,
            owner,
          }),
        );
      }
      if (toolUseId === undefined || toolUseId === "") {
        // warn: a defect because an unowned task notification cannot settle its unit.
        LOGGER.warn(
          { uuid, task_id: taskId },
          "a task notification names no originating call; its unit cannot be settled",
        );
        return entries;
      }
      if (!taskKinds.settlesAsSubagent(taskId)) {
        // A SHELL TASK'S UNIT SETTLES ON ITS OWN TOOL RESULT, never here: the
        // vendor already returned the `Bash` call with the backgrounding notice,
        // and stamping a subagent terminal on it would restate a shell command
        // as an agent run carrying a prompt it never had.
        LOGGER.debug(
          { uuid, task_id: taskId, tool_use_id: toolUseId },
          "the settling task is not an agent run; its own unit's result settles it and no subagent terminal is written",
        );
        return entries;
      }
      entries.push(...subagentTerminalEntries(context, agentId, uuid, toolUseId, raw, spawnCall));
      return entries;
    }

    default:
      LOGGER.debug(
        { uuid, subtype: raw.subtype },
        "no detached-work converter owns this task message; it lands as residue",
      );
      return [residueEntry(context, message, residueForMessage(message), `unknown.${String(raw.subtype)}`)];
  }
}

/**
 * The spawn unit's running beat, from the task's own progress message.
 *
 * A COARSE RUNNING TOTAL, NOT A RECONCILED CHARGE. `AgentSubagentProgress` is
 * "what a run has spent, while it is still running", superseded by the full
 * accounting at conclusion — so this carries only the vendor's `total_tokens`
 * (plus the tool-call count and elapsed wall-clock the beat states), and the
 * settled `total_only` on the terminal replaces it. The row is keyed by the
 * spawn unit exactly as the terminal is (`toolCallActivityId(toolUseId)`), so
 * each successive beat UPSERTS one row rather than appending, and the terminal
 * upserts the same row.
 *
 * THE PROMPT IS RESTATED FROM THE SPAWNING CALL. `AgentSubagentUpdate.prompt` is
 * repeated on every frame so a frame can stand alone, and a `task_progress`
 * message carries no prompt of its own — so the task table's remembered call
 * is its source. The beat UPSERTS the spawn's row, so a beat with an empty
 * prompt erased the commission the start had put there, and a replay of a run
 * still going drew no description. Empty only when the fold never saw the
 * spawning call open.
 */
function subagentProgressEntries(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  vendorUuid: string,
  toolUseId: string,
  raw: RawTask,
  spawnCall: PendingCall | undefined,
): readonly PersistEntry[] {
  const activityId = toolCallActivityId(toolUseId);
  const totalTokens = raw.usage?.total_tokens;
  const toolUses = raw.usage?.tool_uses;
  const durationMs = raw.usage?.duration_ms;
  return [
    activityEntry(
      context,
      { agentId, vendorUuid, discriminator: "activity.subagent.update" },
      agentActivity(activityId, {
        case: "subagent",
        value: create(conversationv1.AgentSubagentSchema, {
          result: {
            case: "update",
            value: create(conversationv1.AgentSubagentUpdateSchema, {
              prompt: spawnPrompt(spawnCall),
              progress: create(conversationv1.AgentSubagentProgressSchema, {
                totalTokens: nonNegativeBigInt(totalTokens),
                toolUseCount: nonNegativeInt(toolUses),
                durationMs: nonNegativeBigInt(durationMs),
              }),
            }),
          },
        }),
      }),
    ),
  ];
}

/** A finite non-negative count as a `bigint`, or `0n` when the vendor stated none. */
function nonNegativeBigInt(value: number | undefined): bigint {
  if (typeof value !== "number" || !Number.isFinite(value) || value < 0) return 0n;
  return BigInt(Math.trunc(value));
}

/** A finite non-negative count as a `number`, or `0` when the vendor stated none. */
function nonNegativeInt(value: number | undefined): number {
  if (typeof value !== "number" || !Number.isFinite(value) || value < 0) return 0;
  return Math.trunc(value);
}

/**
 * What a detached spawn was asked, restated from its spawning call.
 *
 * The call is the one authority — {@link subagentPrompt} reads it exactly as
 * the start did, so the two cannot disagree. A call the fold never saw open
 * leaves an EMPTY prompt, never an invented one.
 */
function spawnPrompt(spawnCall: PendingCall | undefined): conversationv1.AgentSubagentPrompt {
  return spawnCall === undefined
    ? create(conversationv1.AgentSubagentPromptSchema, { text: "" })
    : subagentPrompt(spawnCall);
}

/**
 * The spawn unit's terminal, from the task's own notification.
 *
 * AN ASYNC RUN'S SETTLED USAGE IS AS THIN AS THE VENDOR REPORTS IT: a
 * notification carries at most a total-tokens scalar, so a full breakdown is
 * unproducible here and the contract makes that unrepresentable rather than
 * letting a producer fake one.
 */
function subagentTerminalEntries(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  vendorUuid: string,
  toolUseId: string,
  raw: RawTask,
  spawnCall: PendingCall | undefined,
): readonly PersistEntry[] {
  const activityId = toolCallActivityId(toolUseId);
  if (spawnCall === undefined) {
    LOGGER.debug(
      { task_id: raw.task_id, tool_use_id: toolUseId },
      "the fold never saw this task's spawning call open; its terminal restates no start",
    );
  }
  // THE SPAWN'S OWN START, restated on the settle so a replayed bubble states
  // how long the run took.
  const settled = settledAt(context.nowMs(), spawnCall?.startedAtMs);
  const totalTokens = raw.usage?.total_tokens;
  if (raw.status === "stopped") {
    LOGGER.info({ task_id: raw.task_id }, "a person stopped the detached run");
    return [
      activityEntry(
        context,
        { agentId, vendorUuid, discriminator: "activity.subagent.failure.stopped_by_user" },
        agentActivity(activityId, {
          case: "subagent",
          value: create(conversationv1.AgentSubagentSchema, {
            result: {
              case: "failure",
              value: create(conversationv1.AgentSubagentFailureSchema, {
                cause: {
                  case: "stoppedByUser",
                  value: create(conversationv1.AgentSubagentStoppedByUserSchema, {}),
                },
                // RESTATED, so the stopped spawn a replay serves alone still
                // draws its label and addresses its sub-feed: the spawning
                // call's prompt, and the created agent the minting rule names.
                prompt: spawnPrompt(spawnCall),
                createdAgentId: subagentId(toolUseId),
              }),
            },
          }),
        }),
      ),
    ];
  }
  if (raw.status === "failed") {
    LOGGER.info({ task_id: raw.task_id }, "a detached run reached a failure terminal");
    return [
      activityEntry(
        context,
        { agentId, vendorUuid, discriminator: "activity.subagent.failure" },
        agentActivity(activityId, {
          case: "subagent",
          value: create(conversationv1.AgentSubagentSchema, {
            result: {
              case: "failure",
              value: create(conversationv1.AgentSubagentFailureSchema, {
                error: create(conversationv1.AgentToolFailureSchema, {
                  content:
                    raw.summary === undefined
                      ? undefined
                      : create(conversationv1.ToolResultContentSchema, {
                          blocks: [
                            create(conversationv1.ToolResultContentBlockSchema, {
                              block: {
                                case: "text",
                                value: create(conversationv1.TextBlockSchema, { text: raw.summary }),
                              },
                            }),
                          ],
                        }),
                  settledAt: settled,
                }),
                prompt: spawnPrompt(spawnCall),
                createdAgentId: subagentId(toolUseId),
              }),
            },
          }),
        }),
      ),
    ];
  }
  // `completed`: the ASYNC path, where the vendor gives a summary and a token
  // total. This message carries no prompt, so the one the spawn was given is
  // restated from its spawning call — a settled frame describes itself.
  LOGGER.info({ task_id: raw.task_id }, "a detached run completed");
  return [
    activityEntry(
      context,
      { agentId, vendorUuid, discriminator: "activity.subagent.success" },
      agentActivity(activityId, {
        case: "subagent",
        value: create(conversationv1.AgentSubagentSchema, {
          result: {
            case: "success",
            value: create(conversationv1.AgentSubagentSuccessSchema, {
              // THE CREATED AGENT, named on the conclusion. This notification
              // is a settled frame that can arrive with no start beside it —
              // the launch's own start rides the tool result, on a delivery
              // this one need not share — so the id is restated here from the
              // spawning call, which the minting rule makes the same value.
              createdAgentId: subagentId(toolUseId),
              prompt: spawnPrompt(spawnCall),
              report: create(conversationv1.AgentSubagentReportSchema, {
                prose: prose(raw.summary ?? ""),
              }),
              totals: create(conversationv1.AgentSubagentTotalsSchema, {
                durationMs: 0n,
                usage: {
                  case: "totalOnly",
                  value: create(conversationv1.AgentSubagentAsyncUsageSchema, {
                    totalTokens:
                      typeof totalTokens === "number" && Number.isFinite(totalTokens)
                        ? BigInt(Math.trunc(totalTokens))
                        : undefined,
                  }),
                },
              }),
              settledAt: settled,
            }),
          },
        }),
      }),
    ),
  ];
}

// ---------------------------------------------------------------------------
// Work we stopped being able to see
// ---------------------------------------------------------------------------

/**
 * WHY the stream plane can produce `went_silent` but never judges it here.
 *
 * `DetachedLost.went_silent` says "the run produced nothing past the reader's
 * silence ruling". On this plane the only evidence of disappearance is a task
 * leaving the `background_tasks_changed` LEVEL without a notification — and
 * reading that requires DIFFING the level, which the contract explicitly forbids
 * the fold to do (the level is consumed with replace semantics, and the live set
 * IS the set of open detached-item streams, which is the engine's). So the
 * engine's own live-set bookkeeping makes the ruling and calls these builders;
 * the fold states the shape and nothing else.
 *
 * `file_vanished` is NEVER ours: only the sidecar reads files.
 */
export function wentSilent(): conversationv1.DetachedLost {
  return create(conversationv1.DetachedLostSchema, {
    how: { case: "wentSilent", value: create(conversationv1.DetachedLostWentSilentSchema, {}) },
  });
}

/**
 * A detached SHELL run the engine concluded went silent.
 *
 * The original start is passed in because a settled bash frame restates its
 * command, and after the run left the turn only the record holds it.
 */
export function lostBashEntry(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  run: conversationv1.AgentActivityId,
  originalStart: conversationv1.AgentBashStart,
  how: conversationv1.DetachedLost,
): PersistEntry | undefined {
  if (originalStart.command === undefined) {
    LOGGER.debug(
      { run: run.value },
      "the recorded start for this shell run states no command; no terminal is produced",
    );
    return undefined;
  }
  // warn: a defect because a shell run reached teardown without a success or failure outcome.
  LOGGER.warn(
    { run: run.value, how: how.how.case },
    "a detached shell run was concluded LOST: not known to have failed, not known to have finished",
  );
  return {
    agentId,
    upsertKey: bashTerminalUpsertKey(run),
    source: {
      vendorUuid: `lost:${run.value}`,
      discriminator: `agent_bash.success.interrupted.lost.${String(how.how.case)}`,
    },
    keepalive: context.keepalive,
    item: {
      kind: "bash_run",
      run,
      frame: create(conversationv1.AgentBashSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentBashSuccessSchema, {
            command: originalStart.command,
            outcome: {
              case: "interrupted",
              value: create(conversationv1.AgentBashInterruptedSchema, {
                // NOT OURS TO STATE, AND NOW SAYABLE (landing 5): nothing
                // observed what the command said after we lost sight of it, so
                // the form is `not_observed` — distinct from empty text (a
                // command that printed nothing) and from a `partial` omission
                // of zero bytes, which claimed we had seen all none of it.
                output: create(conversationv1.AgentBashOutputSchema, {
                  form: {
                    case: "notObserved",
                    value: create(conversationv1.AgentBashOutputNotObservedSchema, {}),
                  },
                }),
                cause: { case: "lost", value: how },
              }),
            },
          }),
        },
      }),
    },
  };
}

/**
 * A detached SPAWN unit the engine concluded went silent.
 *
 * The spawning call is passed in because the lost failure is a settled frame
 * and restates what the spawn was asked and which agent it created.
 */
export function lostSubagentEntry(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  spawnCall: PendingCall,
  how: conversationv1.DetachedLost,
): PersistEntry {
  const spawn = toolCallActivityId(spawnCall.toolUseId);
  // warn: a defect because a detached spawn reached teardown without a success or failure outcome.
  LOGGER.warn(
    { spawn: spawn.value, how: how.how.case },
    "a detached spawn was concluded LOST: not known to have failed, not known to have finished",
  );
  return {
    agentId,
    upsertKey: activityUpsertKey(spawn),
    source: {
      vendorUuid: `lost:${spawn.value}`,
      discriminator: `activity.subagent.failure.lost.${String(how.how.case)}`,
    },
    keepalive: context.keepalive,
    item: {
      kind: "frame",
      frame: updateFrame(
        agentId,
        create(conversationv1.AgentUpdateSchema, {
          update: {
            case: "activity",
            value: agentActivity(spawn, {
              case: "subagent",
              value: create(conversationv1.AgentSubagentSchema, {
                result: {
                  case: "failure",
                  value: create(conversationv1.AgentSubagentFailureSchema, {
                    cause: { case: "lost", value: how },
                    prompt: subagentPrompt(spawnCall),
                    createdAgentId: subagentId(spawnCall.toolUseId),
                  }),
                },
              }),
            }),
          },
        }),
      ),
    },
  };
}

/** A detached AGENT's own book, closed as lost. */
export function lostAgentEntry(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  how: conversationv1.DetachedLost,
): PersistEntry {
  // warn: a defect because a detached agent reached teardown without a success or failure outcome.
  LOGGER.warn(
    { agent: agentId.value, how: how.how.case },
    "a detached agent was concluded LOST: not known to have failed, not known to have finished",
  );
  const coordinate = `lost:${agentId.value}`;
  return {
    agentId,
    upsertKey: terminalUpsertKey(agentId, coordinate),
    source: {
      vendorUuid: coordinate,
      discriminator: `agent_frame.failure.lost.${String(how.how.case)}`,
    },
    keepalive: context.keepalive,
    item: {
      kind: "frame",
      frame: agentFrame(agentId, {
        case: "failure",
        value: create(conversationv1.AgentFailureSchema, {
          failure: { case: "lost", value: how },
        }),
      }),
    },
  };
}
