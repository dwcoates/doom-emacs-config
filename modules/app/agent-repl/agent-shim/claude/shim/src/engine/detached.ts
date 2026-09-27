/**
 * engine/detached.ts — the live set of detached work, and the verbs that
 * address it.
 *
 * RESPONSIBILITY. Fold `task_started`, `background_tasks_changed`,
 * `task_updated` and `task_notification` into the set of work that is currently
 * live, maintain the `DetachedWorkId` ↔ underlying-identity mapping, and serve
 * the kill paths and `SessionLive`/`TurnLive` from it.
 *
 * THE MAPPING IS ONE LOOKUP, NEVER A SCAN. `task_started` carries BOTH the
 * vendor task id and the tool_use_id it belongs to, so the association is
 * recorded once when it is stated. Reconstructing it later by matching commands
 * or timings would be a guess, and a wrong guess routes one run's output onto
 * another run's stream.
 *
 * THE LEVEL IS A REPLACEMENT, NOT A DIFF. `background_tasks_changed` states the
 * WHOLE live set. It is applied by replacement — entries it omits are gone,
 * entries it names are kept or created — and the difference between the old set
 * and the new one is never computed into anything retained. Diffing a level
 * into edges is how a missed message becomes a permanently wedged indicator.
 *
 * SPAWN PROVENANCE IS THE ONE BOUNDED STATE EXCEPTION. Each live entry
 * remembers the turn that spawned it, solely so `KillTurn` can NAME its
 * transitive refusal set. It is bounded by the number of live tasks and never
 * appears on the wire except inside a refusal.
 *
 * `skip_transcript` WORK IS TRACKED AND NEVER ANNOUNCED. The vendor's own level
 * set still governs liveness, so an ambient task must count for
 * "is anything running" — but it produces no bubble and no announcement, which
 * is exactly the split {@link LiveWorkTable.announceable} draws.
 *
 * FOREGROUND WORK IS KNOWN, NEVER LIVE. The vendor tracks a blocking `Bash`
 * call and a synchronous spawn as a task too (`is_backgrounded: false`), and
 * the one rule in `convert/detached.ts` — the same one the converter announces
 * by — decides when such a task becomes detached work: a patch or its own tool
 * result moving it. Until then it is held APART from the live set, only so the
 * move can be recognized and a subagent's ask can still be routed to its book;
 * it is never announced, never in `SessionLive`/`TurnLive`, never a WatchBash
 * run and never part of `KillTurn`'s refusal set.
 *
 * WHAT A STOP ACTUALLY IS. `query.stopTask` — the SDK's native per-task stop.
 * There is no process to kill at this boundary: detached work runs INSIDE the
 * agent binary, not as a child of the shim, so the shim owns no process
 * boundary that could reach it.
 */
import { bindLog } from "../log.js";
import {
  patchBackgrounds,
  resultBackgroundTaskId,
  startedInForeground,
} from "../convert/detached.js";
import { detachedWorkId } from "../convert/ids.js";
import type { conversationv1 } from "../proto.js";
import type { PersistEntry } from "../store/persistence.js";
import type {
  SdkBackgroundTasksChangedMessage,
  SdkTaskNotificationMessage,
  SdkTaskStartedMessage,
  SdkTaskUpdatedMessage,
} from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-engine-detached", operation: "shim.engine.detached" });

/** One live item, as the shim knows it. */
export interface LiveWorkEntry {
  /** The vendor task id, which IS the `DetachedWorkId`. */
  readonly taskId: string;
  /** The call this work belongs to, when `task_started` stated one. */
  readonly toolUseId?: string;
  /** The vendor's own kind word (`bash`, `agent`, …), when stated. */
  readonly taskType?: string;
  /** The subagent definition, for agent work. */
  readonly subagentType?: string;
  /** The vendor's description of the work. */
  readonly description: string;
  /** Ambient work: tracked for liveness, never announced. */
  readonly skipTranscript: boolean;
  /** The turn that spawned it — the provenance `KillTurn` names its set from. */
  readonly turnId?: string;
  /** The vendor's last stated status, when one was stated. */
  readonly status?: string;
  /** Whether the vendor has reported this work backgrounded. */
  readonly backgrounded?: boolean;
}

/**
 * The live table.
 *
 * Constant-size in the number of LIVE items — nothing terminal is retained,
 * because a terminal item is not live and remembering it would make this a
 * history of the session, which the store already is.
 */
/**
 * How many retired handles the table remembers.
 *
 * CONSTANT SIZE, which is the whole point: the shim accumulates nothing that
 * grows with the session, so this is a fixed ring rather than a history. It
 * exists because `StopBashFailure` distinguishes "the shell already ended" from
 * "no live shell carries this handle", and only something that saw the handle
 * retire can tell those apart. A consumer still holding a handle long after 64
 * further items have retired is told `unknown_work`, which is the weaker but
 * never wrong answer.
 */
const RETIRED_HANDLES_REMEMBERED = 64;

/**
 * How many FOREGROUND tasks the table remembers at once.
 *
 * Constant size for the same reason as the retired ring: a foreground task
 * normally leaves at its own notification, but nothing lets one that never
 * concludes grow the table without end. The oldest is forgotten first; a move
 * to the background it can no longer be matched to still arrives on the
 * vendor's level, which creates the live entry from what it states.
 */
const FOREGROUND_TASKS_REMEMBERED = 256;

export class LiveWorkTable {
  private readonly entries = new Map<string, LiveWorkEntry>();

  /**
   * Work that started in the FOREGROUND and has not moved: known, never live.
   * Insertion-ordered, so the bound forgets the oldest first.
   */
  private readonly foreground = new Map<string, LiveWorkEntry>();

  /**
   * The most recently retired WIRE handles, oldest first.
   *
   * Keyed by `tool_use_id` because that is what a caller addresses work by;
   * an item whose start named no call has no wire handle and is not recorded.
   */
  private readonly retiredHandles: string[] = [];

  /** Remember that a handle retired, evicting the oldest beyond the bound. */
  private retire(entry: LiveWorkEntry | undefined): void {
    const handle = entry?.toolUseId;
    if (handle === undefined || handle === "") return;
    const already = this.retiredHandles.indexOf(handle);
    if (already !== -1) this.retiredHandles.splice(already, 1);
    this.retiredHandles.push(handle);
    while (this.retiredHandles.length > RETIRED_HANDLES_REMEMBERED) this.retiredHandles.shift();
  }

  /**
   * Whether this handle names work that STARTED and has since ended.
   *
   * False means "not within the remembered window", never "never existed" — so
   * a caller reports the weaker `unknown_work` on false and the sharper
   * `already_ended` on true.
   */
  retired(toolUseId: string): boolean {
    return this.retiredHandles.includes(toolUseId);
  }

  /** A task began: the one moment the id, the call and the kind are all stated. */
  onTaskStarted(message: SdkTaskStartedMessage, turnId?: string): LiveWorkEntry {
    const entry: LiveWorkEntry = {
      taskId: message.task_id,
      description: message.description,
      skipTranscript: message.skip_transcript === true,
      ...(message.tool_use_id === undefined ? {} : { toolUseId: message.tool_use_id }),
      ...(message.task_type === undefined ? {} : { taskType: message.task_type }),
      ...(message.subagent_type === undefined ? {} : { subagentType: message.subagent_type }),
      ...(turnId === undefined ? {} : { turnId }),
      ...(message.is_backgrounded === undefined ? {} : { backgrounded: message.is_backgrounded }),
    };
    if (startedInForeground(message)) {
      this.holdForeground(entry);
      LOGGER.debug(
        {
          task_id: entry.taskId,
          tool_use_id: entry.toolUseId ?? "",
          task_type: entry.taskType ?? "",
          turn_id: entry.turnId ?? "",
        },
        "a task started in the foreground; it is tracked apart and is not live detached work",
      );
      return entry;
    }
    this.entries.set(entry.taskId, entry);
    LOGGER.info(
      {
        task_id: entry.taskId,
        tool_use_id: entry.toolUseId ?? "",
        task_type: entry.taskType ?? "",
        skip_transcript: entry.skipTranscript,
        turn_id: entry.turnId ?? "",
      },
      "recorded a live detached-work item and its spawn provenance",
    );
    return entry;
  }

  /** A patch to one task. Unknown ids are ignored, loudly. */
  onTaskUpdated(message: SdkTaskUpdatedMessage): LiveWorkEntry | undefined {
    const held = this.foreground.get(message.task_id);
    if (held !== undefined) return this.onForegroundUpdated(held, message);
    const existing = this.entries.get(message.task_id);
    if (existing === undefined) {
      LOGGER.debug(
        { task_id: message.task_id },
        "task_updated for a task this shim never saw start; ignored",
      );
      return undefined;
    }
    const updated: LiveWorkEntry = {
      ...existing,
      ...(message.patch.description === undefined ? {} : { description: message.patch.description }),
      ...(message.patch.status === undefined ? {} : { status: message.patch.status }),
      ...(message.patch.is_backgrounded === undefined
        ? {}
        : { backgrounded: message.patch.is_backgrounded }),
    };
    this.entries.set(updated.taskId, updated);
    LOGGER.debug(
      {
        task_id: updated.taskId,
        status: updated.status ?? "",
        backgrounded: updated.backgrounded ?? false,
      },
      "applied a detached-work state transition",
    );
    return updated;
  }

  /** A patch to foreground work: applied, and a move to the background makes it live. */
  private onForegroundUpdated(
    held: LiveWorkEntry,
    message: SdkTaskUpdatedMessage,
  ): LiveWorkEntry | undefined {
    const updated: LiveWorkEntry = {
      ...held,
      ...(message.patch.description === undefined ? {} : { description: message.patch.description }),
      ...(message.patch.status === undefined ? {} : { status: message.patch.status }),
    };
    if (patchBackgrounds(message.patch)) return this.promote(updated, "task_updated");
    this.foreground.set(updated.taskId, updated);
    LOGGER.debug(
      { task_id: updated.taskId, status: updated.status ?? "" },
      "applied a state transition to foreground work; it stays out of the live set",
    );
    return undefined;
  }

  /**
   * A tool result arrived. When it says its work MOVED to the background, the
   * foreground task it names becomes live detached work.
   */
  onToolResult(structured: unknown): LiveWorkEntry | undefined {
    const taskId = resultBackgroundTaskId(structured);
    if (taskId === undefined) return undefined;
    const held = this.foreground.get(taskId);
    if (held === undefined) {
      LOGGER.logVerbose(
        { task_id: taskId, live: this.entries.has(taskId) },
        "a tool result named backgrounded work that is not held as foreground work; nothing to move",
      );
      return undefined;
    }
    return this.promote(held, "tool_result");
  }

  /** Foreground work moved to the background: it leaves the held set and joins the live one. */
  private promote(entry: LiveWorkEntry, via: string): LiveWorkEntry {
    this.foreground.delete(entry.taskId);
    const live: LiveWorkEntry = { ...entry, backgrounded: true };
    this.entries.set(live.taskId, live);
    LOGGER.info(
      {
        task_id: live.taskId,
        tool_use_id: live.toolUseId ?? "",
        task_type: live.taskType ?? "",
        skip_transcript: live.skipTranscript,
        turn_id: live.turnId ?? "",
        via,
      },
      "foreground work moved to the background; recorded a live detached-work item and its spawn provenance",
    );
    return live;
  }

  /** Hold one foreground task, forgetting the oldest beyond the bound. */
  private holdForeground(entry: LiveWorkEntry): void {
    this.foreground.delete(entry.taskId);
    this.foreground.set(entry.taskId, entry);
    while (this.foreground.size > FOREGROUND_TASKS_REMEMBERED) {
      const [oldest] = this.foreground.keys();
      if (oldest === undefined) break;
      this.foreground.delete(oldest);
      LOGGER.debug(
        { task_id: oldest, bound: FOREGROUND_TASKS_REMEMBERED },
        "the foreground-task bound was reached; the oldest held foreground task is forgotten",
      );
    }
  }

  /** A task concluded. Its terminal fact is the vendor's; the entry leaves the set. */
  onTaskNotification(message: SdkTaskNotificationMessage): LiveWorkEntry | undefined {
    if (this.foreground.delete(message.task_id)) {
      // NOTHING RETIRES: the work was never announced, so there is no handle a
      // consumer could still be holding.
      LOGGER.debug(
        { task_id: message.task_id, status: message.status },
        "foreground work concluded without ever becoming detached work",
      );
      return undefined;
    }
    const existing = this.entries.get(message.task_id);
    this.entries.delete(message.task_id);
    this.retire(existing);
    LOGGER.info(
      { task_id: message.task_id, status: message.status, known: existing !== undefined },
      "a detached-work item concluded and left the live set",
    );
    return existing;
  }

  /**
   * The vendor stated the WHOLE live set. Apply it by replacement.
   *
   * Entries the level names are kept with everything already known about them
   * (the level carries no tool_use_id, and losing that mapping would orphan the
   * run's output); entries it omits are dropped; entries it names that are new
   * are created with what the level states and nothing invented.
   */
  onLevel(message: SdkBackgroundTasksChangedMessage): void {
    const next = new Map<string, LiveWorkEntry>();
    for (const task of message.tasks) {
      // A LEVEL NAMES ONLY BACKGROUND WORK, so a held foreground task it names
      // has moved — and it keeps everything its start stated.
      const held = this.foreground.get(task.task_id);
      const existing =
        this.entries.get(task.task_id) ??
        (held === undefined ? undefined : this.promote(held, "background_tasks_changed"));
      next.set(
        task.task_id,
        existing === undefined
          ? {
              taskId: task.task_id,
              description: task.description,
              taskType: task.task_type,
              skipTranscript: false,
            }
          : { ...existing, description: task.description, taskType: task.task_type },
      );
    }
    const dropped = [...this.entries.keys()].filter((id) => !next.has(id));
    // A LEVEL THAT OMITS AN ITEM RETIRES IT, exactly as a notification does —
    // the vendor's level is the whole truth about what is live.
    for (const id of dropped) this.retire(this.entries.get(id));
    this.entries.clear();
    for (const [id, entry] of next) this.entries.set(id, entry);
    LOGGER.debug(
      { level_size: next.size, dropped: dropped.join(" ") },
      "applied the vendor's live-task LEVEL by replacement",
    );
  }

  /** One item by its work id. */
  get(taskId: string): LiveWorkEntry | undefined {
    return this.entries.get(taskId);
  }

  /**
   * A task by its vendor id, LIVE OR FOREGROUND.
   *
   * For the lookups that translate the vendor's task id into the spawning call
   * — a subagent's ask routed to its book, a task frame joined to its unit — and
   * never for liveness: foreground work is not live work.
   */
  tracked(taskId: string): LiveWorkEntry | undefined {
    return this.entries.get(taskId) ?? this.foreground.get(taskId);
  }

  /** The item a tool call spawned, if it is still live. */
  byToolUseId(toolUseId: string): LiveWorkEntry | undefined {
    for (const entry of this.entries.values()) {
      if (entry.toolUseId === toolUseId) return entry;
    }
    return undefined;
  }

  /** Everything live, ambient work included. */
  all(): readonly LiveWorkEntry[] {
    return [...this.entries.values()];
  }

  /** Everything live that a consumer may be told about. */
  announceable(): readonly LiveWorkEntry[] {
    return this.all().filter((entry) => !entry.skipTranscript);
  }

  /** Everything one turn spawned — `KillTurn`'s transitive set. */
  spawnedBy(turnId: string): readonly LiveWorkEntry[] {
    return this.all().filter((entry) => entry.turnId === turnId);
  }

  /**
   * The live set as the WIRE names it: by the SPAWNING CALL, never by task id.
   *
   * `DetachedWorkId.value == AgentActivityId.value` (ruling, landing 3), so a
   * terminal retires a handle by equality. A task the vendor started with no
   * originating call has no wire name at all and is omitted — it is tracked for
   * liveness (the level still governs no-wedge) but nothing can address it.
   */
  workIds(entries: readonly LiveWorkEntry[] = this.all()): conversationv1.DetachedWorkId[] {
    const ids: conversationv1.DetachedWorkId[] = [];
    for (const entry of entries) {
      if (entry.toolUseId === undefined || entry.toolUseId === "") {
        LOGGER.debug(
          { task_id: entry.taskId },
          "live work with no originating call has no wire handle; omitted from the named set",
        );
        continue;
      }
      ids.push(detachedWorkId(entry.toolUseId));
    }
    return ids;
  }

  /** Nothing is live. */
  get empty(): boolean {
    return this.entries.size === 0;
  }
}

/**
 * How many shell runs' start rows the engine remembers.
 *
 * CONSTANT SIZE, like every table here: the live runs plus the retired ring's
 * worth, with room to spare. A run whose start was forgotten is still served
 * from the store, which holds the start it was written with; only the
 * durability barrier below is skipped for it.
 */
const SHELL_RUN_STARTS_REMEMBERED = 256;

/**
 * The START row the fold produced for each detached shell run, by run value.
 *
 * WHY THE ENGINE KEEPS IT. A consumer opens `WatchBash` the instant it learns
 * of a run, and the run's start — written ahead of the announcement — may still
 * be in the writer's buffer. Re-writing the SAME entry durably is the barrier:
 * it carries the same write identity, so the store absorbs it if the original
 * already landed, and the writer's one ordered buffer lands it behind the
 * original if not. Either way the open that follows finds the run's first row,
 * so the stream is sent `start` at once rather than waiting on anything.
 */
export class ShellRunStarts {
  private readonly starts = new Map<string, PersistEntry>();

  /** Remember every shell run START among what the fold just produced. */
  note(entries: readonly PersistEntry[]): void {
    for (const entry of entries) {
      if (entry.item.kind !== "bash_run" || entry.item.frame.result.case !== "start") continue;
      const run = entry.item.run.value;
      this.starts.delete(run);
      this.starts.set(run, entry);
      while (this.starts.size > SHELL_RUN_STARTS_REMEMBERED) {
        const [oldest] = this.starts.keys();
        if (oldest === undefined) break;
        this.starts.delete(oldest);
        LOGGER.debug(
          { run: oldest, bound: SHELL_RUN_STARTS_REMEMBERED },
          "the shell-run start bound was reached; the oldest run's start is forgotten",
        );
      }
    }
  }

  /** The start row this process produced for a run, if it remembers one. */
  get(work: conversationv1.DetachedWorkId): PersistEntry | undefined {
    return this.starts.get(work.value);
  }
}
