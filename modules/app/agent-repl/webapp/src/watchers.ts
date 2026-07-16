/**
 * Watcher projection — which detached background tasks a final-response
 * bubble should fold in.
 *
 * A turn can arm a long-running background watcher (a backgrounded Bash
 * poll, a detached agent) whose progress outlives the turn that armed it.
 * The final-response bubble that talks about that watcher is inert prose;
 * this projection links the bubble to the spawning tool items so the
 * renderer can fold their live tail into it.
 *
 * DERIVE-DON'T-TRACK, like agents.ts and partition.ts: the spawn ids are
 * already in the store (a spawning call's result / notification, read by
 * `spawnedTaskIds`), so this is a projection of `items` rather than a
 * second copy. Turn-SCOPED on purpose: it lists every watcher a bubble's
 * turn spawned, because matching the prose wording ("the pickup watcher")
 * back to a raw id is not reliable, whereas the turn's spawn set is exact.
 */
import { spawnedTaskIds } from "./partition.js";
import { ConversationItem, ToolItem } from "./store.js";

/** A tool call is a watcher when it announced detached work of its own. */
export function isWatcher(item: ConversationItem): item is ToolItem {
  return item.kind === "tool" && spawnedTaskIds(item).length > 0;
}

/**
 * Map each turn-concluding text block to the watcher tool items its turn
 * armed, in spawn order.
 *
 * A turn is the span between a `user-turn` and the `result` that closes it;
 * a watcher spawned anywhere in that span belongs to the turn's concluding
 * bubble. Only a turn that ran to completion (a `success` result) keys the
 * map — the same bubbles `finalResponses` pairs a chip to — so a mid-turn
 * commentary bubble or a severed errored turn folds in nothing.
 */
export function watchersByBubble(
  items: readonly ConversationItem[],
): Map<string, ToolItem[]> {
  const byBubble = new Map<string, ToolItem[]>();
  let turnWatchers: ToolItem[] = [];
  let lastText: string | null = null;
  for (const item of items) {
    if (item.kind === "user-turn") {
      turnWatchers = [];
      lastText = null;
    } else if (isWatcher(item)) {
      turnWatchers.push(item);
    } else if (item.kind === "text" && item.parentToolUseId === undefined) {
      lastText = item.blockId;
    } else if (item.kind === "result") {
      if (lastText !== null && item.subtype === "success" && turnWatchers.length > 0) {
        byBubble.set(lastText, turnWatchers);
      }
      turnWatchers = [];
      lastText = null;
    }
  }
  return byBubble;
}
