/**
 * Task roster — the session's harness task list (`TaskCreate` /
 * `TaskUpdate`), surfaced as a topbar counter beside the subagent roster.
 *
 * Unlike the subagent roster (one tool call = one agent), a task's life
 * spans a `TaskCreate` and any number of later `TaskUpdate` calls, keyed
 * by a task id. That id is assigned by the harness and reported in the
 * `TaskCreate` RESULT (not its input), so the projection folds the create
 * (subject + initial `pending`) with every update (status, renamed
 * subject) keyed by that id.
 *
 * ASSUMPTION: the `TaskCreate` result text names the id as `#<id>` (as the
 * harness prints `Task #1 created successfully: …`). See
 * `taskIdFromCreateResult`. If that wording changes, updates stop
 * correlating and each create would strand as its own row.
 *
 * A `deleted` task is dropped outright (no dead row). Terminal
 * (`completed`) tasks linger under the shared recency window (see
 * `counter-menu.ts`), then prune.
 */
import {
  CounterEntry,
  CounterSpec,
  CounterStatus,
  counterMenuHtml,
} from "./counter-menu.js";
import { ConversationItem, ToolItem } from "./store.js";
import { countedTurns } from "./turn-clock.js";

/** The harness task-list tools the roster is projected from. */
export const TASK_TOOLS: ReadonlySet<string> = new Set(["TaskCreate", "TaskUpdate"]);

/** The tasks counter's DOM/vocabulary knobs. */
export const TASKS_SPEC: CounterSpec = {
  menu: "tasks",
  item: "task",
  noun: "task",
  busyNoun: "active",
  title: "session tasks",
  placeholder: "pending…",
};

/**
 * Map a harness task status onto the shared counter vocabulary:
 * `pending` reads as `starting`, `in_progress` as `running`, `completed`
 * as `done`. `deleted` maps to nothing — a deleted task is removed, not
 * shown. Tasks have no error state, so `error` never arises here.
 */
function mapTaskStatus(status: string): CounterStatus {
  switch (status) {
    case "in_progress":
      return "running";
    case "completed":
      return "done";
    case "pending":
    default:
      return "starting";
  }
}

function stringField(item: ToolItem, key: string): string {
  const value = item.input?.[key];
  return typeof value === "string" ? value : "";
}

/** A tool result's text, flattened from string or text-block content. */
function resultText(item: ToolItem): string {
  const content = item.result?.content;
  if (content === undefined) return "";
  if (typeof content === "string") return content;
  return content.map((b) => b.text).join("");
}

/**
 * The harness task id named in a `TaskCreate` result, e.g. `1` from
 * `Task #1 created successfully: …`, or null when no `#<id>` is present
 * (the create has not settled, or the wording changed).
 */
export function taskIdFromCreateResult(item: ToolItem): string | null {
  const match = resultText(item).match(/#(\S+)/);
  return match ? match[1] : null;
}

/**
 * The `TaskCreate` tool-use id behind roster entry TASKID — the reverse of
 * the roster projection, mapping a clicked row back to the feed card that
 * created its task. That card is the task's one bubble: the partition
 * confines the task's `TaskUpdate` calls inside its activity panel (see
 * partition.ts). A row keys off the harness id its create's result
 * reported, falling back to the create's own tool-use id while the result
 * is pending (the same fallback `sessionTasks` applies), so the lookup
 * matches by the same key. Null when no create matches — the id names no
 * task this session created.
 */
export function taskCreateToolUseId(
  items: readonly ConversationItem[],
  taskId: string,
): string | null {
  for (const item of items) {
    if (item.kind !== "tool" || item.toolName !== "TaskCreate") continue;
    if ((taskIdFromCreateResult(item) ?? item.toolUseId) === taskId) {
      return item.toolUseId;
    }
  }
  return null;
}

/** A task under construction as the fold accumulates create+update calls. */
interface TaskRecord {
  id: string;
  subject: string;
  status: CounterStatus;
  deleted: boolean;
  deactivatedAtTurn: number | null;
  order: number;
}

/**
 * Every task the session created, in creation order, folded from its
 * `TaskCreate` and `TaskUpdate` calls. A row keeps its final subject and
 * current status; a `completed` task carries the counted-turn ordinal it
 * completed on, so the recency window can age it out.
 */
export function sessionTasks(items: readonly ConversationItem[]): CounterEntry[] {
  return taskEntries(items, () => true);
}

/**
 * The tasks the agent AGENTID itself drove — its own `TaskCreate` /
 * `TaskUpdate` calls, folded exactly as the session roster folds — the
 * agent-scoped twin of `sessionTasks`, feeding a bubble topbar's counter.
 * The harness task list is session-global, so a task the agent only
 * UPDATED still rows here (its subject as the agent's calls named it);
 * completion ages stay on the session's counted-turn clock.
 */
export function agentTasks(
  items: readonly ConversationItem[],
  agentId: string,
): CounterEntry[] {
  return taskEntries(items, (item) => item.parentToolUseId === agentId);
}

/**
 * The task fold behind both rosters: ITEMS is always the FULL session
 * list (so `countedTurns` stamps ages on the session clock), and IN-SCOPE
 * picks which task calls the roster counts.
 */
function taskEntries(
  items: readonly ConversationItem[],
  inScope: (item: ToolItem) => boolean,
): CounterEntry[] {
  const byId = new Map<string, TaskRecord>();
  let order = 0;

  const recordFor = (id: string, subject: string): TaskRecord => {
    let rec = byId.get(id);
    if (!rec) {
      rec = {
        id,
        subject,
        status: "starting",
        deleted: false,
        deactivatedAtTurn: null,
        order: order++,
      };
      byId.set(id, rec);
    }
    return rec;
  };

  for (let i = 0; i < items.length; i++) {
    const item = items[i];
    if (item.kind !== "tool" || !TASK_TOOLS.has(item.toolName)) continue;
    if (!inScope(item)) continue;

    if (item.toolName === "TaskCreate") {
      // Before the result lands the id is unknown, so the row keys off the
      // tool-use id; once the create settles it re-keys to the real id.
      const id = taskIdFromCreateResult(item) ?? item.toolUseId;
      recordFor(id, stringField(item, "subject"));
      continue;
    }

    // TaskUpdate.
    if (!item.inputDone) continue;
    const id = stringField(item, "taskId");
    if (id === "") continue;
    const rec = recordFor(id, stringField(item, "subject"));
    const subject = stringField(item, "subject");
    if (subject !== "") rec.subject = subject;
    const status = stringField(item, "status");
    if (status === "deleted") {
      rec.deleted = true;
    } else if (status !== "") {
      rec.status = mapTaskStatus(status);
      rec.deactivatedAtTurn =
        rec.status === "done" ? countedTurns(items, i) : null;
    }
  }

  return [...byId.values()]
    .filter((rec) => !rec.deleted)
    .sort((a, b) => a.order - b.order)
    .map((rec) => ({
      id: rec.id,
      summary: rec.subject,
      detail: "",
      status: rec.status,
      nested: false,
      deactivatedAtTurn: rec.deactivatedAtTurn,
    }));
}

/** The tasks counter's chip and (when open) its overlay. */
export function tasksMenuHtml(
  entries: readonly CounterEntry[],
  open: boolean,
  currentTurn: number,
): string {
  return counterMenuHtml(TASKS_SPEC, entries, open, currentTurn);
}
