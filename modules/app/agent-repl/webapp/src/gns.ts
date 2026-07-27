/**
 * gns-sockets fold projection — which feed items are bridge upkeep the
 * green-bordered final-response bubble above them should swallow.
 *
 * A session with a live Slack-thread subscription (the gns-sockets skill)
 * keeps a `sockets-listener` subagent bridging its idle moments. That
 * bridge is respawned by the skill's Stop hook: after the agent writes its
 * real answer, the hook blocks the stop and the agent appends a bridge
 * respawn (an Agent spawn plus a one-line acknowledgment) to the SAME
 * turn — which steals the green border from the answer. And when the
 * bridge's own background task ends, the harness notification wakes a
 * WHOLE turn of nothing but bridge upkeep ("window closed", respawn,
 * "respawned"). Both are session plumbing, not conversation, so this
 * projection folds them into the final response that precedes them as
 * expandable nested content.
 *
 * DERIVE-DON'T-TRACK, like watchers.ts: the one deterministic signal —
 * the spawn's `subagent_type: "sockets-listener"` input, dictated
 * verbatim by the Stop hook's directive — is already in the store, so
 * this is a projection of `items` rather than a second copy. The prose
 * around the spawns is model-authored and is deliberately never matched.
 *
 * Two shapes are folded, both only from turns that ran to completion (a
 * `success` result), mirroring `finalResponses`:
 *
 * - TAIL segment: within a turn, the run from the first bridge spawn
 *   after the turn's last real main-chain tool through the turn's end.
 *   The last main-chain text BEFORE the segment is the turn's real
 *   answer; it becomes the fold's host (and, with the segment filtered
 *   out, regains the green border the respawn stole). A turn whose
 *   spawn is followed by further real tools folds nothing — that spawn
 *   is the turn's own work, not a Stop-hook continuation.
 *
 * - WHOLE turn: a turn no user prompt opened (`user-turn`-less, i.e.
 *   notification-woken) that either spawns the bridge or carries the
 *   bridge's own parented output (its wake-up call) folds entirely —
 *   including its result chip — into the previous turn's final-response
 *   bubble. Successive such turns stack under the same host.
 *
 * A fold with no host to attach to (no completed answer above it) stays
 * visible — pollution is recoverable, silence is not (partition.ts).
 */
import { SUBAGENT_TOOLS } from "./agents.js";
import { ConversationItem, ResultItem, TextItem, ToolItem } from "./store.js";

/** The subagent type the gns-sockets Stop hook directs the agent to spawn. */
export const BRIDGE_SUBAGENT_TYPE = "sockets-listener";

/** A tool call is a bridge spawn when it spawns the sockets-listener. */
export function isBridgeSpawn(item: ConversationItem): item is ToolItem {
  return (
    item.kind === "tool" &&
    item.parentToolUseId === undefined &&
    SUBAGENT_TOOLS.has(item.toolName) &&
    item.input?.subagent_type === BRIDGE_SUBAGENT_TYPE
  );
}

export interface GnsFolds {
  /** Host bubble block id → the items its fold panel renders, in feed order. */
  readonly byBubble: ReadonlyMap<string, readonly ConversationItem[]>;
  /** Every folded item, for suppression from the top feed and projections. */
  readonly folded: ReadonlySet<ConversationItem>;
}

/** Main-chain text: the only thing that can host a fold. */
function isMainText(item: ConversationItem): item is TextItem {
  return item.kind === "text" && item.parentToolUseId === undefined;
}

/** Main-chain tool: the only thing that can start or break a fold tail. */
function isMainTool(item: ConversationItem): boolean {
  return item.kind === "tool" && item.parentToolUseId === undefined;
}

/**
 * What a fold may swallow: the segment's own prose, thinking, spawns, and
 * the permission prompts gating them. Everything else (system notes,
 * errors, retries, dividers) stays in the feed — errors in particular must
 * never be foldable out of sight.
 */
function isFoldable(item: ConversationItem): boolean {
  switch (item.kind) {
    case "text":
    case "thinking":
    case "tool":
      return item.parentToolUseId === undefined;
    case "permission":
      return true;
    default:
      return false;
  }
}

/**
 * Index of the tail bridge segment's first item, or -1 when the turn has
 * none: the first bridge spawn with no real main-chain tool after it.
 * A later real tool means the agent went back to work, so the spawn was
 * part of the turn's own doing rather than a Stop-hook continuation.
 */
function tailSegmentStart(turn: readonly ConversationItem[]): number {
  let start = -1;
  for (let i = 0; i < turn.length; i++) {
    const item = turn[i];
    if (!isMainTool(item)) continue;
    if (isBridgeSpawn(item)) {
      if (start === -1) start = i;
    } else {
      start = -1;
    }
  }
  return start;
}

/**
 * Fold the session's gns-sockets bridge upkeep under its final-response
 * bubbles. ITEMS is the feed's flat item list (already truncated at any
 * clear or compaction); the
 * renderer filters `folded` out of the top feed and its turn projections,
 * and renders each `byBubble` list inside its host bubble's fold panel.
 */
export function gnsFolds(items: readonly ConversationItem[]): GnsFolds {
  const byBubble = new Map<string, ConversationItem[]>();
  const folded = new Set<ConversationItem>();
  /** Every bridge spawn's id so far — how a wake-up call is recognized. */
  const bridgeIds = new Set<string>();
  /** The last completed turn's (post-fold) answer, hosting whole-turn folds. */
  let host: string | null = null;
  let turn: ConversationItem[] = [];
  let sawUserTurn = false;

  const attach = (hostBlock: string, foldedItems: ConversationItem[]): void => {
    if (foldedItems.length === 0) return;
    const list = byBubble.get(hostBlock) ?? [];
    for (const item of foldedItems) {
      list.push(item);
      folded.add(item);
    }
    byBubble.set(hostBlock, list);
  };

  const finishTurn = (result: ResultItem): void => {
    const buf = turn;
    const wasUser = sawUserTurn;
    turn = [];
    sawUserTurn = false;
    for (const item of buf) {
      if (isBridgeSpawn(item)) bridgeIds.add(item.toolUseId);
    }
    if (result.subtype !== "success") {
      // A severed turn's last text is not an answer (finalResponses), so
      // it neither folds anything nor hosts later folds.
      host = null;
      return;
    }
    const tail = tailSegmentStart(buf);
    if (!wasUser && host !== null) {
      // A turn no prompt opened was woken by the harness; when it spawns
      // the bridge, or was woken BY the bridge (its parented output rode
      // in with the wake), the whole turn — chip included — is upkeep.
      const wokenByBridge = buf.some(
        (item) =>
          (item.kind === "text" || item.kind === "thinking" || item.kind === "tool") &&
          item.parentToolUseId !== undefined &&
          bridgeIds.has(item.parentToolUseId),
      );
      if (tail !== -1 || wokenByBridge) {
        attach(host, [...buf.filter(isFoldable), result]);
        return;
      }
    }
    if (tail !== -1) {
      // Stop-hook continuation: the segment folds into the turn's own
      // answer, which the filtered feed then pairs with the result — the
      // green border lands back on the real response.
      let pre: string | null = null;
      for (let i = 0; i < tail; i++) {
        const item = buf[i];
        if (isMainText(item)) pre = item.blockId;
      }
      if (pre !== null) {
        host = pre;
        attach(pre, buf.slice(tail).filter(isFoldable));
        return;
      }
    }
    // An ordinary completed turn: its answer hosts what follows it. A
    // turn that answered with no text (tools only) hosts nothing — noise
    // must not fold into a bubble above newer visible content.
    let last: string | null = null;
    for (const item of buf) {
      if (isMainText(item)) last = item.blockId;
    }
    host = last;
  };

  for (const item of items) {
    if (item.kind === "user-turn") {
      // A prompt landing on an unclosed turn (interrupt races, dangling
      // tails) abandons that buffer unfolded: without a result there is
      // no completed turn to fold.
      turn = [];
      sawUserTurn = true;
      continue;
    }
    if (item.kind === "result") {
      finishTurn(item);
      continue;
    }
    turn.push(item);
  }
  // A still-streaming tail is never folded: its next block could always
  // continue it, exactly as finalResponses withholds the green border.
  return { byBubble, folded };
}
