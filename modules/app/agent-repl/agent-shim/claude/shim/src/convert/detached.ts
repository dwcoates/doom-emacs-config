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
  bashStartUpsertKey,
  bashTerminalUpsertKey,
  detachedWorkUpsertKey,
  terminalUpsertKey,
} from "../store/keys.js";
import type { PersistEntry } from "../store/persistence.js";
import { agentFrame, prose, settledAt, updateFrame } from "./entries.js";
import type { FoldContext } from "./fold-context.js";
import { detachedWorkId, subagentId, toolCallActivityId } from "./ids.js";
import { residueEntry, residueForMessage } from "./residue.js";
import { TOOL_CONVERTERS } from "./tools/registry.js";
import { subagentConverter, subagentPrompt } from "./tools/subagent.js";
import { bashConverter } from "./tools/bash.js";
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

// ---------------------------------------------------------------------------
// WHAT KIND OF WORK IT IS — the vendor's own word, and nothing else.
// ---------------------------------------------------------------------------
//
// Every announcement states its kind (`AgentDetachedWork.kind`), and the
// vendor's `task_type` is the authority for it. The unit the work detached
// from is NOT: a subagent RESUMED BY `SendMessage` is detached from the send,
// and a consumer that read the kind off that unit found no kind at all
// (daemon.sessionwatcher.detached_kind_unknown, 2026-09-27, the agent never
// reached the live set or the footer).

/** The kinds a detached-work announcement can name. */
export type DetachedKindName = "subagent" | "bash" | "workflow" | "monitor";

/**
 * The vendor's `task_type` words this shim can announce, and the kind each is.
 *
 * A CLOSED TABLE. A word absent here is a task this shim does not know how to
 * describe, and its announcement is refused rather than given a kind.
 */
const VENDOR_TASK_KINDS: ReadonlyMap<string, DetachedKindName> = new Map<string, DetachedKindName>([
  ["local_agent", "subagent"],
  ["local_bash", "bash"],
  ["local_workflow", "workflow"],
  ["monitor", "monitor"],
]);

/** The kind a vendor `task_type` names, or `undefined` for a word this shim does not know. */
export function taskKindOf(taskType: string | undefined): DetachedKindName | undefined {
  return taskType === undefined ? undefined : VENDOR_TASK_KINDS.get(taskType);
}

/**
 * Whether a task is an AGENT run by its vendor `task_type`: one that names the
 * subagent kind, or one whose kind was never stated.
 *
 * THE UNSTATED CASE IS AN AGENT, and that is a standing rule rather than a
 * default this helper invents: the subagent terminal is the long-standing
 * behavior for an untyped task, and only a task the vendor NAMED as something
 * else is refused one. ONE PLACE for every caller that asks it.
 */
export function isAgentTaskType(taskType: string | undefined): boolean {
  return taskType === undefined || taskType === "" || taskKindOf(taskType) === "subagent";
}

/**
 * The kind one announcement states. A subagent's carries the agent that is
 * running, so a subagent kind with no agent cannot be built.
 */
export type AnnouncedKind =
  | { readonly kind: "subagent"; readonly agent: conversationv1.AgentId }
  | { readonly kind: "bash" | "workflow" | "monitor" };

/** The wire form of one announced kind. */
export function detachedWorkKind(announced: AnnouncedKind): conversationv1.DetachedWorkKind {
  switch (announced.kind) {
    case "subagent":
      return create(conversationv1.DetachedWorkKindSchema, {
        kind: {
          case: "subagent",
          value: create(conversationv1.DetachedWorkKindSubagentSchema, { agentId: announced.agent }),
        },
      });
    case "bash":
      return create(conversationv1.DetachedWorkKindSchema, {
        kind: { case: "bash", value: create(conversationv1.DetachedWorkKindBashSchema, {}) },
      });
    case "workflow":
      return create(conversationv1.DetachedWorkKindSchema, {
        kind: { case: "workflow", value: create(conversationv1.DetachedWorkKindWorkflowSchema, {}) },
      });
    case "monitor":
      return create(conversationv1.DetachedWorkKindSchema, {
        kind: { case: "monitor", value: create(conversationv1.DetachedWorkKindMonitorSchema, {}) },
      });
  }
}

/**
 * The kind a RECORDED description of detached work states — for the `created`
 * announcements a restarted consumer is sent, where no task record survives and
 * the recorded start is the one statement of what the work is.
 *
 * `undefined` for a description with no kind arm, or a subagent start that
 * names no created agent: neither can be announced, and the caller refuses it.
 */
export function detachableKind(work: conversationv1.DetachableWork): AnnouncedKind | undefined {
  switch (work.work.case) {
    case "subagent": {
      const result = work.work.value.result;
      const agent = result.case === "start" ? result.value.createdAgentId : undefined;
      return agent === undefined || agent.value === "" ? undefined : { kind: "subagent", agent };
    }
    case "bash":
      return { kind: "bash" };
    case "workflow":
      return { kind: "workflow" };
    case "monitor":
      return { kind: "monitor" };
    case undefined:
      return undefined;
  }
}

/** Whether a call is a subagent SPAWN — the one call whose id IS the created agent. */
function isSpawnCall(call: PendingCall): boolean {
  return TOOL_CONVERTERS.get(call.toolName) === subagentConverter;
}

/** One task's facts, as an announcement is being built from them. */
interface TaskFacts {
  readonly uuid: string;
  readonly taskId: string;
  readonly toolUseId: string;
  /** The vendor's `task_type`, as stated now or remembered from the start. */
  readonly taskType: string | undefined;
  /** The task's own call, when this fold saw it open. */
  readonly call: PendingCall | undefined;
}

/**
 * THE AGENT A SUBAGENT TASK IS RUNNING, under the cross-plane minting rule: the
 * `tool_use_id` of the call that SPAWNED it, which is what every plane books
 * the agent under.
 *
 * THE TASK'S OWN CALL IS NOT ALWAYS THAT SPAWN. The vendor's task id for a
 * subagent is the agent's own locator and stays the same across a resume, but
 * the call a resume's `task_started` names is the `SendMessage` that woke it.
 * So the spawn is learned once, when a task starts from a spawn call, and every
 * later start of the same task id is resolved through that join.
 *
 * - The call is a spawn: its id is the agent, and the join is recorded.
 * - The join already names the agent: that is the agent, whatever the call.
 * - The call was never seen here (a backgrounded subagent's own spawn does not
 *   reach this stream): the task's call is taken as its spawn, as the minting
 *   rule reads a task's `tool_use_id`, and the join is recorded.
 * - Otherwise the call is known NOT to be a spawn and no join names the agent —
 *   a resume of an agent this process never saw spawn. The engine has already
 *   asked the STORE for the pairing by then ({@link taskAwaitingAgent}); a
 *   found agent was remembered as the join above, so reaching here means the
 *   store named none either. Refused, at ERROR, with the store's answer: the
 *   send's id is not the agent, and naming it as one would address a book
 *   nobody writes.
 */
function runningAgent(facts: TaskFacts, taskKinds: TaskKindRegistry): conversationv1.AgentId | undefined {
  if (facts.call !== undefined && isSpawnCall(facts.call)) {
    const agent = subagentId(facts.toolUseId);
    taskKinds.rememberAgent(facts.taskId, agent);
    return agent;
  }
  const joined = taskKinds.agentOf(facts.taskId);
  if (joined !== undefined) {
    LOGGER.debug(
      { uuid: facts.uuid, task_id: facts.taskId, tool_use_id: facts.toolUseId, agent: joined.value },
      "a subagent task started from a call that is not its spawn; the agent is the one its spawn created",
    );
    return joined;
  }
  if (facts.call === undefined) {
    const agent = subagentId(facts.toolUseId);
    taskKinds.rememberAgent(facts.taskId, agent);
    return agent;
  }
  // THE STORE WAS ASKED FIRST (engine: `taskAwaitingAgent`), and its answer is
  // part of this record: a refusal the store could have prevented reads
  // differently from one it could not.
  const store = taskKinds.storeAnswerOf(facts.taskId) ?? "not asked";
  const detail = `the task's call ${facts.toolUseId} is a ${facts.call.toolName}, not a spawn, no spawn of task ${facts.taskId} was seen by this process, and the store answered ${store}`;
  LOGGER.error(
    {
      uuid: facts.uuid,
      task_id: facts.taskId,
      tool_use_id: facts.toolUseId,
      tool: facts.call.toolName,
      store_answer: store,
      detail,
    },
    "a subagent task started from a call that is not its spawn, and this process never saw the spawn; the running agent cannot be named and the announcement is refused",
  );
  return undefined;
}

/**
 * The kind an announcement about this task states, or `undefined` — logged at
 * ERROR — when the task names no kind this shim knows, or a subagent whose
 * running agent cannot be named. Never a default.
 */
function announcedKind(facts: TaskFacts, taskKinds: TaskKindRegistry): AnnouncedKind | undefined {
  const kind = taskKindOf(facts.taskType);
  if (kind === undefined) {
    LOGGER.error(
      {
        uuid: facts.uuid,
        task_id: facts.taskId,
        tool_use_id: facts.toolUseId,
        task_type: facts.taskType ?? "",
        detail: `task_type ${JSON.stringify(facts.taskType ?? null)} is not one of ${[...VENDOR_TASK_KINDS.keys()].join(", ")}`,
      },
      "a task names no kind this shim knows; its announcement is malformed and is refused",
    );
    return undefined;
  }
  if (kind !== "subagent") return { kind };
  const agent = runningAgent(facts, taskKinds);
  return agent === undefined ? undefined : { kind, agent };
}

/** What one detachment announcement says. */
interface DetachmentFacts {
  /** What kind of work it is. */
  readonly kind: AnnouncedKind;
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
    kind: detachedWorkKind(facts.kind),
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
    turn: context.turnId,
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
    // A SHELL, BY THE VENDOR'S OWN TOOL: this is the `Bash` call's own result
    // saying its work moved to the background.
    kind: { kind: "bash" },
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

// ---------------------------------------------------------------------------
// THE SHELL RUN'S START — the shim writes it; the run's END is the sidecar's.
// ---------------------------------------------------------------------------
//
// A detached shell's OUTPUT and its END are the sidecar's to write, read off
// the spool. Its START is not: the sidecar mints no start at all (a spool exists
// only after the launch this plane announced). A hand-backgrounded shell that
// concluded within seconds (2026-09-27, task bfa5s1wjd) got no row whatsoever,
// so `WatchBashRun` refused it forever and the run's announcement stayed open.
//
// So the shim writes THE START at the announcement, ahead of the announcement
// row in the one ordered buffer — a consumer that learned of the run from the
// record can always open its stream and is sent `start` first — and restates it
// at the task's notification, so a run concluded before any announcement
// reached the record still replays its start ahead of its end.
//
// THE SHIM NEVER WRITES THE RUN'S TERMINAL (owner ruling, 2026-09-29). The
// store ends a run's stream at its FIRST terminal, and the shim's had no exit
// status and no output: it landed ~40ms before the sidecar read the spool's
// terminator, so the exit code and the last lines were never delivered. The
// sidecar is the only writer of a shell run's terminal — from the spool's own
// terminator, or from the vendor's task notification once the spool has been
// read to its end — so nothing can land after the end.
//
// ONE WRITE IDENTITY PER RUN. The coordinate is the run itself, so every
// restatement of the start (the tool result, a by-hand backgrounding, the
// notification) mints the SAME write id and the store's ledger absorbs all but
// the first. The key is the cross-plane one (`bash:<run>:start`).

/** The source coordinate every row this shim writes about one shell RUN shares. */
function shellRunCoordinate(run: conversationv1.AgentActivityId): string {
  return `shell-run:${run.value}`;
}

/**
 * A detached shell run's START row, from the call that spawned it.
 *
 * `undefined` when the call was never seen open by this fold, or named no
 * command line: a start states the command and nothing else can supply it.
 */
export function shellRunStartEntry(
  context: FoldContext,
  agentId: conversationv1.AgentId,
  toolUseId: string,
  call: PendingCall | undefined,
): PersistEntry | undefined {
  const run = toolCallActivityId(toolUseId);
  if (call === undefined) {
    LOGGER.debug(
      { run: run.value },
      "this fold never saw the shell run's call open; it cannot state the run's start",
    );
    return undefined;
  }
  const started = bashConverter.start(call);
  if (started?.case !== "bash" || started.value.result.case !== "start") return undefined;
  LOGGER.logVerbose({ run: run.value }, "a detached shell run's start row, written at its announcement");
  return {
    agentId,
    upsertKey: bashStartUpsertKey(run),
    source: { vendorUuid: shellRunCoordinate(run), discriminator: "agent_bash.start" },
    keepalive: context.keepalive,
    turn: context.turnId,
    item: { kind: "bash_run", run, frame: started.value },
  };
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
  /** Why a task ended other than ordinarily (`worker_restart`), when stated. */
  readonly reason?: string;
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
  /** The vendor `task_type` remembered for a task, or `undefined` if none was stated. */
  kindOf(taskId: string): string | undefined;
  /**
   * Remember WHICH AGENT a subagent task is running (see `runningAgent`).
   *
   * OUTLIVES THE TASK'S SETTLE, unlike every other fact here: a subagent's task
   * id is the agent's own locator and comes back when the agent is RESUMED, and
   * the resume's own call is the send, not the spawn. Bounded all the same.
   */
  rememberAgent(taskId: string, agent: conversationv1.AgentId): void;
  /** The agent remembered for a task, or `undefined` if this process never named one. */
  agentOf(taskId: string): conversationv1.AgentId | undefined;
  /**
   * Remember what the STORE answered when asked which agent a task names, for
   * a task it did not name one for — so the refusal that follows can say so.
   * Forgotten with the task's other facts.
   */
  rememberStoreAnswer(taskId: string, answer: string): void;
  /** The store's remembered answer for a task, or `undefined` if it was never asked. */
  storeAnswerOf(taskId: string): string | undefined;
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
      storeAnswer?: string;
    }
  >();
  /** Make room for one more task, forgetting the oldest when the cap is hit. */
  /** The agents subagent tasks run, by task id; kept past each task's settle. */
  const agents = new Map<string, conversationv1.AgentId>();
  /**
   * Make room for one more entry in either table, forgetting the oldest when
   * the cap is hit. ONE BOUND, ONE RECORD for both tables: what is lost differs
   * (a notification's type and cause, or a later resume's agent), and the
   * record names which.
   */
  const reserve = (table: Map<string, unknown>, lost: string): void => {
    if (table.size < TASK_KIND_CAPACITY) return;
    const [oldest] = table.keys();
    if (oldest === undefined) return;
    table.delete(oldest);
    // warn: a defect because bounded task bookkeeping discarded facts a later message of that task needs.
    LOGGER.warn(
      { task_id: oldest, lost },
      "a bounded task table is full; the oldest task's entry is forgotten",
    );
  };
  const FACTS_LOST = "its kind and cause, so its notification cannot be typed";
  return {
    remember(taskId, taskType) {
      reserve(facts, FACTS_LOST);
      facts.set(taskId, { ...facts.get(taskId), kind: taskType });
    },
    kindOf(taskId) {
      return facts.get(taskId)?.kind;
    },
    rememberAgent(taskId, agent) {
      agents.delete(taskId);
      reserve(agents, "the agent it ran, so a resume of that agent cannot be named");
      agents.set(taskId, agent);
    },
    agentOf(taskId) {
      return agents.get(taskId);
    },
    rememberStoreAnswer(taskId, storeAnswer) {
      reserve(facts, FACTS_LOST);
      facts.set(taskId, { ...facts.get(taskId), storeAnswer });
    },
    storeAnswerOf(taskId) {
      return facts.get(taskId)?.storeAnswer;
    },
    rememberCause(taskId, cause) {
      reserve(facts, FACTS_LOST);
      facts.set(taskId, { ...facts.get(taskId), cause });
    },
    causeOf(taskId) {
      return facts.get(taskId)?.cause;
    },
    rememberToolUse(taskId, toolUseId) {
      reserve(facts, FACTS_LOST);
      facts.set(taskId, { ...facts.get(taskId), toolUseId });
    },
    toolUseFor(taskId) {
      return facts.get(taskId)?.toolUseId;
    },
    rememberOwner(taskId, owner) {
      reserve(facts, FACTS_LOST);
      facts.set(taskId, { ...facts.get(taskId), owner });
    },
    ownerOf(taskId) {
      return facts.get(taskId)?.owner;
    },
    rememberCall(taskId, call) {
      reserve(facts, FACTS_LOST);
      facts.set(taskId, { ...facts.get(taskId), call });
    },
    callOf(taskId) {
      return facts.get(taskId)?.call;
    },
    settlesAsSubagent(taskId) {
      const kind = facts.get(taskId)?.kind;
      facts.delete(taskId);
      return isAgentTaskType(kind);
    },
    rememberForeground(taskId) {
      reserve(facts, FACTS_LOST);
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

/** The task subtypes whose conversion names the agent a subagent task runs. */
const AGENT_NAMING_SUBTYPES: ReadonlySet<string> = new Set(["task_started", "task_updated", "task_notification"]);

/**
 * THE TASK WHOSE AGENT THE STORE MUST NAME BEFORE THIS MESSAGE IS FOLDED, or
 * `undefined` when the fold can name it itself (or need not name one at all).
 *
 * It is exactly the case {@link runningAgent} would otherwise refuse: a subagent
 * task whose call is known and is NOT its spawn (a `SendMessage` that resumed
 * it), with no join in this process naming the agent — a resume of an agent
 * this process never saw spawn, typically because it restarted since. The
 * engine awaits the store's answer and hands it back through
 * {@link TaskKindRegistry.rememberAgent} / {@link TaskKindRegistry.rememberStoreAnswer}
 * before folding, so the fold itself stays synchronous.
 *
 * A PURE READ of what the fold already holds: it records nothing, so asking it
 * twice, or asking and then not folding, changes nothing.
 */
export function taskAwaitingAgent(
  message: SdkMessage,
  context: FoldContext,
  taskKinds: TaskKindRegistry,
  calls: CallRegistry,
): string | undefined {
  if (message.type !== "system") return undefined;
  const raw = message as unknown as RawTask;
  if (raw.subtype === undefined || !AGENT_NAMING_SUBTYPES.has(raw.subtype)) return undefined;
  if (raw.skip_transcript === true) return undefined;
  const taskId = raw.task_id;
  if (typeof taskId !== "string" || taskId === "") return undefined;
  if (taskKinds.agentOf(taskId) !== undefined) return undefined;
  const known = context.liveTask(taskId);
  const taskType = raw.task_type ?? taskKinds.kindOf(taskId) ?? known?.taskType;
  if (taskKindOf(taskType) !== "subagent") return undefined;
  const toolUseId = raw.tool_use_id ?? known?.toolUseId ?? taskKinds.toolUseFor(taskId);
  if (toolUseId === undefined || toolUseId === "") return undefined;
  const call = calls.peek(toolUseId) ?? taskKinds.callOf(taskId);
  if (call === undefined || isSpawnCall(call)) return undefined;
  return taskId;
}

/** What the fold knows about WHICH AGENT one vendor task is running. */
export type TaskAgentKnowledge =
  /** The agent, by a join this process holds or by the task's own spawning call. */
  | { readonly kind: "named"; readonly agent: conversationv1.AgentId }
  /** The task started from a call known NOT to be its spawn, and nothing names its agent. */
  | { readonly kind: "not_its_spawn" }
  /** The fold holds nothing about this task's agent. */
  | { readonly kind: "unknown" };

/**
 * Which agent a vendor task is running, AS THE FOLD KNOWS IT — the one answer
 * every other reader of the pairing (the permission gate's book for an ask)
 * takes, so the gate cannot credit a resumed agent's ask to the send that
 * woke it.
 */
export function taskAgentKnowledge(taskKinds: TaskKindRegistry, taskId: string): TaskAgentKnowledge {
  const joined = taskKinds.agentOf(taskId);
  if (joined !== undefined) return { kind: "named", agent: joined };
  const call = taskKinds.callOf(taskId);
  if (call === undefined) return { kind: "unknown" };
  return isSpawnCall(call) ? { kind: "named", agent: subagentId(call.toolUseId) } : { kind: "not_its_spawn" };
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
  // THE VENDOR'S WORD FOR WHAT THE TASK IS: stated on this message, or on the
  // task's start, or on the vendor's live level (which is all a process that
  // started after the task has).
  const taskType = raw.task_type ?? taskKinds.kindOf(taskId) ?? known?.taskType;
  const taskFacts = (call: string): TaskFacts => ({ uuid, taskId, toolUseId: call, taskType, call: spawnCall });

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
      if (taskKindOf(raw.task_type) === "bash") {
        LOGGER.debug(
          { uuid, task_id: taskId, tool_use_id: toolUseId },
          "a shell task started; its own tool result announces the detachment and states the cause",
        );
        return [];
      }
      // REFUSED, NOT RESIDUE: the refusal is already recorded at ERROR with
      // the task's whole context, and a residue row would record it twice.
      const kind = announcedKind(taskFacts(toolUseId), taskKinds);
      if (kind === undefined) return [];
      taskKinds.rememberCause(taskId, "requested");
      LOGGER.info(
        {
          uuid,
          task_id: taskId,
          tool_use_id: toolUseId,
          task_type: raw.task_type ?? "",
          kind: kind.kind,
          agent: kind.kind === "subagent" ? kind.agent.value : "",
        },
        "work left the turn",
      );
      return [
        detachmentEntry(context, agentId, uuid, {
          kind,
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
      // THE WORK STILL LEFT THE TURN when its kind cannot be named: the call is
      // handed off and the cause kept above. Only the announcement is refused.
      const kind = announcedKind(taskFacts(toolUseId), taskKinds);
      if (kind === undefined) return [];
      const announcement = detachmentEntry(context, agentId, uuid, {
        kind,
        detachedFromToolUseId: toolUseId,
        cause: "by_user",
        owner,
      });
      // A HAND-BACKGROUNDED SHELL'S START RIDES AHEAD OF ITS ANNOUNCEMENT, so
      // a consumer told of the run can open its stream at once. This is the
      // incident's path: the Ctrl-B'd shell was announced here and nowhere
      // else before it concluded.
      const start =
        kind.kind === "bash" ? shellRunStartEntry(context, agentId, toolUseId, spawnCall) : undefined;
      return start === undefined ? [announcement] : [start, announcement];
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
      // THE ANNOUNCEMENT'S KIND IS READ BEFORE THE SETTLE BELOW FORGETS IT, and
      // an upsert that cannot state one is refused like any other.
      const kind =
        toolUseId !== undefined && toolUseId !== "" && raw.output_file !== undefined
          ? announcedKind(taskFacts(toolUseId), taskKinds)
          : undefined;
      if (toolUseId !== undefined && toolUseId !== "" && kind !== undefined) {
        entries.push(
          detachmentEntry(context, agentId, uuid, {
            kind,
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
        // THE RUN ENDS HERE, but its terminal is the SIDECAR's to write, from
        // the spool (see "THE SHELL RUN'S START"). The start is restated — the
        // same write identity, absorbed if it already landed — so a run
        // concluded before any announcement reached the record still replays
        // its start ahead of the end the sidecar writes.
        if (taskKindOf(taskType) === "bash") {
          const start = shellRunStartEntry(context, agentId, toolUseId, spawnCall);
          if (start !== undefined) entries.push(start);
          LOGGER.debug(
            { task_id: taskId, tool_use_id: toolUseId, status: raw.status ?? "" },
            "a detached shell run concluded; its terminal is the sidecar's to write from its spool, so the shim writes none",
          );
        }
        return entries;
      }
      // THE AGENT THE TASK RAN, which for a subagent RESUMED BY A SEND is not
      // the send's id: the join its spawn recorded names it. A task this
      // process never named an agent for keeps the minting rule's reading of
      // its own call.
      const createdAgent = taskKinds.agentOf(taskId) ?? subagentId(toolUseId);
      entries.push(
        ...subagentTerminalEntries(context, agentId, uuid, toolUseId, createdAgent, raw, spawnCall),
      );
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
  createdAgent: conversationv1.AgentId,
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
                createdAgentId: createdAgent,
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
                createdAgentId: createdAgent,
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
              createdAgentId: createdAgent,
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
    turn: context.turnId,
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
    turn: context.turnId,
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
    turn: context.turnId,
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
