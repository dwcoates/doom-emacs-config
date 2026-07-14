/**
 * Feed partition — confining subagent traffic to the card that spawned
 * it.
 *
 * The store stays one flat item list (the derive-don't-track philosophy
 * agents.ts established); this module is the projection that splits it:
 * items carrying a parent_tool_use_id leave the top-level feed and land
 * in their owning card's child list, which the renderer draws inside
 * that card's activity panel. The mechanism is parent-generic — an
 * Agent's children and a Workflow's children nest identically.
 *
 * Orphans stay top-level: a child whose parent card the feed does not
 * hold (ring eviction, partial replay) renders where it always did
 * rather than vanishing — pollution is recoverable, silence is not.
 */
import { ConversationItem, PermissionItem, ToolItem } from "./store.js";

export interface FeedPartition {
  /** Top-level items in feed order: parentless items plus orphans. */
  top: ConversationItem[];
  /** Child items per owning tool_use_id, in feed order. */
  children: ReadonlyMap<string, ConversationItem[]>;
}

/** The parent id an item claims, or undefined for main-chain items. */
function claimedParent(
  item: ConversationItem,
  toolParents: ReadonlyMap<string, string | undefined>,
): string | undefined {
  switch (item.kind) {
    case "tool":
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
  for (const item of items) {
    if (item.kind === "tool") {
      known.add(item.toolUseId);
      toolParents.set(item.toolUseId, (item as ToolItem).parentToolUseId);
    }
  }
  const top: ConversationItem[] = [];
  const children = new Map<string, ConversationItem[]>();
  for (const item of items) {
    const parent = claimedParent(item, toolParents);
    if (parent !== undefined && known.has(parent)) {
      const list = children.get(parent) ?? [];
      list.push(item);
      children.set(parent, list);
    } else {
      top.push(item);
    }
  }
  return { top, children };
}
