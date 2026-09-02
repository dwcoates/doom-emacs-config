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
import { detachedWorkId, toolCallActivityId } from "./ids.js";
import { residueEntry, residueForMessage } from "./residue.js";
import { activityEntry, agentActivity } from "./entries.js";

const LOGGER = bindLog({ component: "shim-convert-detached", operation: "shim.convert.detached" });

/** Why a unit left the turn. */
export type DetachCause = "requested" | "by_user" | "timed_out";

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
  const vendorTaskId = output?.backgroundTaskId;
  if (typeof vendorTaskId !== "string" || vendorTaskId === "") return undefined;
  const timedOut = output?.timedOutAfterMs;
  const cause: DetachCause =
    typeof timedOut === "number"
      ? "timed_out"
      : output?.backgroundedByUser === true
        ? "by_user"
        : "requested";
  LOGGER.log(
    { vendor_task_id: vendorTaskId, work: toolUseId, tool_use_id: toolUseId, cause },
    "a shell command moved to the background rather than ending",
  );
  // SO THE `task_notification` DOES NOT OVERWRITE IT with a hard-coded
  // `requested` when it upserts the same row to add the output path.
  taskKinds?.rememberCause(vendorTaskId, cause);
  return detachmentEntry(context, agentId, vendorUuid, {
    detachedFromToolUseId: toolUseId,
    cause,
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
  readonly output_file?: string;
  readonly status?: string;
  readonly summary?: string;
  readonly usage?: { readonly total_tokens?: number };
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
   * Whether a settling task is an AGENT run, and so owns a subagent terminal.
   *
   * A task whose kind was never stated answers `true`: the subagent terminal is
   * the long-standing behavior for an untyped task, and silently dropping it
   * would lose a real settle. Only a task the vendor NAMED as something other
   * than an agent is refused one. Answering also FORGETS the task, which is
   * what keeps the table self-emptying.
   */
  settlesAsSubagent(taskId: string): boolean;
}

export function createTaskKindRegistry(): TaskKindRegistry {
  const facts = new Map<string, { kind?: string; cause?: DetachCause }>();
  /** Make room for one more task, forgetting the oldest when the cap is hit. */
  const reserve = (): void => {
    if (facts.size < TASK_KIND_CAPACITY) return;
    const [oldest] = facts.keys();
    if (oldest === undefined) return;
    facts.delete(oldest);
    LOGGER.log(
      { level: "warn", task_id: oldest },
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
    settlesAsSubagent(taskId) {
      const kind = facts.get(taskId)?.kind;
      facts.delete(taskId);
      return kind === undefined || kind === "local_agent";
    },
  };
}

/** Every task-stream message. */
export function convertDetached(
  message: Extract<SdkMessage, { type: "system" }>,
  context: FoldContext,
  taskKinds: TaskKindRegistry,
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
    LOGGER.log(
      { level: "error", uuid, subtype: raw.subtype },
      "a task message named no task; nothing can be addressed by it",
    );
    return [residueEntry(context, message, residueForMessage(message, "task message has no task_id"), "residue.unparsed")];
  }

  const known = context.liveTask(taskId);
  const toolUseId = raw.tool_use_id ?? known?.toolUseId;
  const agentId = known?.agentId ?? context.mainAgentId;

  switch (raw.subtype) {
    case "task_started": {
      if (toolUseId === undefined || toolUseId === "") {
        // NO ORIGINATING CALL AND NO PAYLOAD WE CAN BUILD: `created` needs a
        // DetachableWork describing the work, and nothing here states one. An
        // invented description would be worse than residue.
        LOGGER.log(
          { level: "warn", uuid, task_id: taskId },
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
        LOGGER.log(
          { uuid, task_id: taskId, tool_use_id: toolUseId },
          "a shell task started; its own tool result announces the detachment and states the cause",
        );
        return [];
      }
      taskKinds.rememberCause(taskId, "requested");
      LOGGER.log(
        { uuid, task_id: taskId, tool_use_id: toolUseId, task_type: raw.task_type ?? "" },
        "work left the turn",
      );
      return [
        detachmentEntry(context, agentId, uuid, {
          detachedFromToolUseId: toolUseId,
          cause: "requested",
        }),
      ];
    }

    case "task_updated": {
      if (raw.patch?.is_backgrounded !== true) {
        LOGGER.logVerbose(
          { uuid, task_id: taskId, status: raw.patch?.status },
          "a task patch with no detachment fact; the unit's own frames carry the rest",
        );
        return [];
      }
      if (toolUseId === undefined || toolUseId === "") {
        LOGGER.log(
          { level: "warn", uuid, task_id: taskId },
          "a task was backgrounded by hand but names no originating call; nothing can be upserted",
        );
        return [];
      }
      // THE CANDIDATE PRODUCER FOR A BACKGROUNDED AGENT, confirmed at this wave:
      // a person backgrounded running work by hand, which the shell path
      // harvests from its tool result and the agent path only states here.
      LOGGER.log({ uuid, task_id: taskId }, "a person backgrounded running work by hand");
      taskKinds.rememberCause(taskId, "by_user");
      return [
        detachmentEntry(context, agentId, uuid, {
          detachedFromToolUseId: toolUseId,
          cause: "by_user",
        }),
      ];
    }

    case "task_progress":
      // The vendor's per-task progress is the SUBAGENT unit's `update` arm, and
      // building one needs the spawn's prompt, which this message does not
      // carry. Consumed; the unit's own frames are the account.
      LOGGER.logVerbose({ uuid, task_id: taskId }, "task progress consumed; the unit's frames carry it");
      return [];

    case "task_notification": {
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
          }),
        );
      }
      if (toolUseId === undefined || toolUseId === "") {
        LOGGER.log(
          { level: "warn", uuid, task_id: taskId },
          "a task notification names no originating call; its unit cannot be settled",
        );
        return entries;
      }
      if (!taskKinds.settlesAsSubagent(taskId)) {
        // A SHELL TASK'S UNIT SETTLES ON ITS OWN TOOL RESULT, never here: the
        // vendor already returned the `Bash` call with the backgrounding notice,
        // and stamping a subagent terminal on it would restate a shell command
        // as an agent run carrying a prompt it never had.
        LOGGER.log(
          { uuid, task_id: taskId, tool_use_id: toolUseId },
          "the settling task is not an agent run; its own unit's result settles it and no subagent terminal is written",
        );
        return entries;
      }
      entries.push(...subagentTerminalEntries(context, agentId, uuid, toolUseId, raw));
      return entries;
    }

    default:
      LOGGER.log(
        { level: "warn", uuid, subtype: raw.subtype },
        "no detached-work converter owns this task message; it lands as residue",
      );
      return [residueEntry(context, message, residueForMessage(message), `unknown.${String(raw.subtype)}`)];
  }
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
): readonly PersistEntry[] {
  const activityId = toolCallActivityId(toolUseId);
  const settled = settledAt(context.nowMs());
  const totalTokens = raw.usage?.total_tokens;
  if (raw.status === "stopped") {
    LOGGER.log({ task_id: raw.task_id }, "a person stopped the detached run");
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
              }),
            },
          }),
        }),
      ),
    ];
  }
  if (raw.status === "failed") {
    LOGGER.log({ level: "warn", task_id: raw.task_id }, "a detached run failed");
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
              }),
            },
          }),
        }),
      ),
    ];
  }
  // `completed`. THE PROMPT IS NOT RESTATED HERE: a settled frame is supposed to
  // describe itself, and this message carries no prompt — so the spawn's own
  // success frame (from its tool result) is the self-describing one, and this is
  // the ASYNC path, where the vendor gives a summary and a token total.
  LOGGER.log({ task_id: raw.task_id }, "a detached run completed");
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
              prompt: create(conversationv1.AgentSubagentPromptSchema, { text: "" }),
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
    LOGGER.log(
      { level: "error", run: run.value },
      "the recorded start for this shell run states no command; no terminal is produced",
    );
    return undefined;
  }
  LOGGER.log(
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

/** A detached SPAWN unit the engine concluded went silent. */
export function lostSubagentEntry(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  spawn: conversationv1.AgentActivityId,
  how: conversationv1.DetachedLost,
): PersistEntry {
  LOGGER.log(
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
  LOGGER.log(
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
