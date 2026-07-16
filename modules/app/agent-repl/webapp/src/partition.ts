/**
 * Feed partition — confining subagent traffic to the card that spawned
 * it.
 *
 * The store stays one flat item list (the derive-don't-track philosophy
 * agents.ts established); this module is the projection that splits it:
 * items carrying a parent_tool_use_id leave the top-level feed and land
 * in their owning card's child list, which the renderer draws inside
 * that card's activity panel. The mechanism is parent-generic — an
 * Agent's children and a Workflow's children nest identically — and two
 * kinds of main-chain call claim a parent by TASK ID instead: a poll of
 * a detached task folds into the card that spawned it, and a task-list
 * update folds into the TaskCreate card that named its task.
 *
 * Orphans stay top-level: a child whose parent card the feed does not
 * hold (ring eviction, partial replay) renders where it always did
 * rather than vanishing — pollution is recoverable, silence is not.
 */
import { ConversationItem, PermissionItem, ToolItem } from "./store.js";
import { taskIdFromCreateResult } from "./tasks.js";

export interface FeedPartition {
  /** Top-level items in feed order: parentless items plus orphans. */
  top: ConversationItem[];
  /** Child items per owning tool_use_id, in feed order. */
  children: ReadonlyMap<string, ConversationItem[]>;
}

/** Tool names that poll or stop a detached task rather than doing work. */
const TASK_POLL_TOOLS = new Set(["TaskOutput", "TaskStop", "TaskGet"]);

/**
 * The ids a spawning call announces its detached work under: "running in
 * background with ID: bg1" for shells, "agentId: abc1" for background
 * agents. Task notifications carry the id verbatim, so the notification
 * field is the authoritative source when it has landed.
 */
const SPAWNED_ID_RE = /\b(?:ID|agentId):\s*([A-Za-z0-9_-]+)/g;

function resultText(item: ToolItem): string {
  const content = item.result?.content;
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  return content.map((b) => b.text ?? "").join("\n");
}

/** Every task id ITEM's spawn result or notification announced. */
export function spawnedTaskIds(item: ToolItem): string[] {
  const ids = [...resultText(item).matchAll(SPAWNED_ID_RE)].map((m) => m[1]);
  if (item.notification?.taskId) ids.push(item.notification.taskId);
  return ids;
}

/** The parent id an item claims, or undefined for main-chain items. */
function claimedParent(
  item: ConversationItem,
  toolParents: ReadonlyMap<string, string | undefined>,
  spawnByTaskId: ReadonlyMap<string, string>,
  createByTaskId: ReadonlyMap<string, string>,
): string | undefined {
  switch (item.kind) {
    case "tool": {
      if (item.parentToolUseId !== undefined) return item.parentToolUseId;
      // A poll of a detached task belongs to the card that spawned the
      // task, so the polling call folds into the spawner's panel.
      if (TASK_POLL_TOOLS.has(item.toolName) && typeof item.input?.task_id === "string") {
        return spawnByTaskId.get(item.input.task_id);
      }
      // A task-list update belongs to the card that created its task, so
      // the task's history folds into that card's panel as nested bubbles.
      if (item.toolName === "TaskUpdate" && typeof item.input?.taskId === "string") {
        return createByTaskId.get(item.input.taskId);
      }
      return undefined;
    }
    case "text":
    case "thinking":
      return item.parentToolUseId;
    case "permission":
      // A permission prompt gates a tool call; when that call is nested,
      // the prompt belongs beside it in the spawning card's panel. The
      // gated call's own id never keys a panel — its PARENT does.
      return toolParents.get((item as PermissionItem).toolUseId);
    default:
      return undefined;
  }
}

/**
 * Split ITEMS into the top-level feed and per-card child lists. A claim
 * on a parent the feed holds no tool item for is an orphan claim, and
 * the item stays top-level.
 */
export function partitionFeed(items: readonly ConversationItem[]): FeedPartition {
  const known = new Set<string>();
  const toolParents = new Map<string, string | undefined>();
  const spawnByTaskId = new Map<string, string>();
  const createByTaskId = new Map<string, string>();
  for (const item of items) {
    if (item.kind !== "tool") continue;
    known.add(item.toolUseId);
    toolParents.set(item.toolUseId, item.parentToolUseId);
    for (const id of spawnedTaskIds(item)) {
      spawnByTaskId.set(id, item.toolUseId);
    }
    // The harness id a settled TaskCreate reported names the card its
    // task's updates fold under (see claimedParent).
    if (item.toolName === "TaskCreate") {
      const id = taskIdFromCreateResult(item);
      if (id !== null && !createByTaskId.has(id)) {
        createByTaskId.set(id, item.toolUseId);
      }
    }
  }
  const top: ConversationItem[] = [];
  const children = new Map<string, ConversationItem[]>();
  for (const item of items) {
    const parent = claimedParent(item, toolParents, spawnByTaskId, createByTaskId);
    if (parent !== undefined && known.has(parent) && !(item.kind === "tool" && parent === item.toolUseId)) {
      const list = children.get(parent) ?? [];
      list.push(item);
      children.set(parent, list);
    } else {
      top.push(item);
    }
  }
  return { top, children };
}
