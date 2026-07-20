/**
 * Stream-member resolution — the ONE place a tool call's streaming nature
 * is decided.
 *
 * Before this module the answers lived scattered: the effective source in
 * subfeed.ts, settledness in render.ts (`watcherSettled`), the face status
 * in the async fold, and the tail's ws-vs-poll precedence in three call
 * sites with three subtly different rules. Every streaming surface (the
 * card, the pill, the fold) now resolves ONE record here and renders from
 * it, so the surfaces cannot disagree about what a member is, whether it
 * lives, or what its stream says.
 *
 * Presentation-free on purpose: the resolver names WHAT streams (status,
 * tail, bodies in Shape A stacking order) and never how to draw it. The
 * caller supplies context (the partition's child lists, the poller's
 * tails) through MemberContext, so the record is a pure function of
 * store + render state.
 */
import { TranscriptStats, transcriptStatsCached } from "./async-stream.js";
import { AsyncSource } from "./protocol.js";
import { spawnedTaskIds } from "./partition.js";
import { ConversationItem, ToolItem } from "./store.js";
import { effectiveAsyncSource } from "./subfeed.js";
import { TaskTail } from "./watcher-poll.js";

/** The one status vocabulary every streaming surface renders from. */
export type MemberStatus = "running" | "done" | "error" | "killed";

/** What the resolver reads from render context (partition + poller). */
export interface MemberContext {
  /** The call's confined child items, as the caller chooses to scope them. */
  children(toolUseId: string): readonly ConversationItem[];
  /** The poller's accumulated tail for a source id, when one exists. */
  taskTail(sourceId: string): TaskTail | undefined;
}

/** One expandable body a member owns, in Shape A stacking order. */
export type BodySpec =
  | { kind: "child-feed"; items: readonly ConversationItem[] }
  | { kind: "transcript"; text: string; sourceId: string }
  | { kind: "journal"; text: string }
  | { kind: "raw"; text: string };

/** The resolved streaming identity of one tool call. */
export interface StreamMember {
  item: ToolItem;
  /** The effective async source, when the call owns a detached stream. */
  source: AsyncSource | undefined;
  /** Task ids the call's spawn announced, notification id included. */
  taskIds: string[];
  status: MemberStatus;
  /** No longer live: nothing left to poll, stop, or watch. */
  settled: boolean;
  /** Live elapsed milliseconds: poll-fed first, frozen heartbeat fallback. */
  elapsedMs: number | undefined;
  /** The stream's accumulated text: the ws-streamed tail first, else polled. */
  tail: string;
  /**
   * Output tokens the detached work has spent, read from its transcript's
   * own usage records (see transcriptStats) — live while it runs, the
   * terminal record's total once it lands. Undefined for streams that
   * carry no usage (a shell's spool, a journal).
   */
  outputTokens: number | undefined;
  /** The member's expandable bodies, child feed above stream (Shape A). */
  bodies: BodySpec[];
}

/**
 * Notification statuses that read as failure or as a deliberate stop. The
 * daemon relays the harness's wording verbatim, so matching is by family
 * rather than one exact token; anything unrecognized reads as a plain
 * completion, which is what the notification fundamentally announces.
 */
const ERROR_STATUSES = new Set(["error", "errored", "failed", "failure"]);
const KILLED_STATUSES = new Set(["killed", "stopped", "cancelled", "canceled", "aborted"]);

/** The status a completion notification carries, null without one. */
function notificationStatus(item: ToolItem): MemberStatus | null {
  if (!item.notification) return null;
  const s = item.notification.status ?? "";
  if (ERROR_STATUSES.has(s)) return "error";
  if (KILLED_STATUSES.has(s)) return "killed";
  return "done";
}

/**
 * Whether a successful TaskStop for one of the member's task ids sits in
 * its child list — the prompt-mediated stop's only completion signal,
 * since daemon liveness is notification-only (see tailer.go). An
 * in-flight or errored TaskStop settles nothing.
 */
function stoppedByTaskStop(
  children: readonly ConversationItem[],
  taskIds: readonly string[],
): boolean {
  const ids = new Set(taskIds);
  return children.some(
    (c) =>
      c.kind === "tool" &&
      c.toolName === "TaskStop" &&
      c.result !== undefined &&
      !c.result.isError &&
      typeof c.input?.task_id === "string" &&
      ids.has(c.input.task_id),
  );
}

/**
 * The poller tail the member's stream reads from: the classified source id
 * first (the id the daemon tails under), then any other announced id — the
 * union of the fold's and the old watcher row's probe orders, so neither
 * surface loses a tail the other could see.
 */
function polledTail(
  ctx: MemberContext,
  source: AsyncSource | undefined,
  taskIds: readonly string[],
): TaskTail | undefined {
  const probe = source ? [source.source_id, ...taskIds] : [...taskIds];
  for (const id of probe) {
    const tail = ctx.taskTail(id);
    if (tail) return tail;
  }
  return undefined;
}

/**
 * Resolve ITEM's streaming identity, or null when nothing about the call
 * streams (no detached source, no announced task, no confined children,
 * no tail) — the null is what "renders no streaming surface at all" keys
 * off.
 *
 * Status resolution order, most authoritative first:
 * 1. The completion notification, error and killed statuses SURFACED
 *    (before this module a killed task's badge read as a green done).
 * 2. A successful TaskStop folded under the member reads as killed.
 * 3. The daemon-classified source status, when it has gone terminal.
 * 4. The poll's own done flag, which outruns a notification that has not
 *    landed yet.
 * 5. The transcript's own terminal record (transcriptStats), which no
 *    lost notification can take away — the stream says when it ended.
 * 6. The call's own result, for members with nothing detached (an inline
 *    subagent, an announcement-less tail).
 * 7. Running.
 */
export function resolveMember(item: ToolItem, ctx: MemberContext): StreamMember | null {
  const source = effectiveAsyncSource(item);
  const taskIds = spawnedTaskIds(item);
  const children = ctx.children(item.toolUseId);
  const polled = polledTail(ctx, source, taskIds);
  const tail =
    item.taskOutput !== undefined && item.taskOutput !== "" ? item.taskOutput : polled?.text ?? "";

  if (!source && taskIds.length === 0 && children.length === 0 && tail === "") {
    return null;
  }

  const stats: TranscriptStats | undefined =
    source?.stream?.format === "jsonl-transcript" && tail !== ""
      ? transcriptStatsCached(source.source_id, tail)
      : undefined;

  const fromNotification = notificationStatus(item);
  let status: MemberStatus;
  if (fromNotification !== null) {
    status = fromNotification;
  } else if (stoppedByTaskStop(children, taskIds)) {
    status = "killed";
  } else if (source && source.status !== "running") {
    status = source.status;
  } else if (polled?.done) {
    status = "done";
  } else if (stats?.finished) {
    status = stats.error ? "error" : "done";
  } else if (!source && taskIds.length === 0) {
    status = item.result ? (item.result.isError ? "error" : "done") : "running";
  } else {
    status = "running";
  }

  const bodies: BodySpec[] = [];
  if (children.length > 0) bodies.push({ kind: "child-feed", items: children });
  if (source) {
    switch (source.stream?.format) {
      case "jsonl-transcript":
        bodies.push({ kind: "transcript", text: tail, sourceId: source.source_id });
        break;
      case "jsonl-journal":
        bodies.push({ kind: "journal", text: tail });
        break;
      default:
        bodies.push({ kind: "raw", text: tail });
        break;
    }
  } else if (tail !== "") {
    bodies.push({ kind: "raw", text: tail });
  }

  return {
    item,
    source,
    taskIds,
    status,
    settled: status !== "running",
    elapsedMs:
      polled?.elapsedMs ??
      (item.progressElapsedS !== undefined ? item.progressElapsedS * 1000 : undefined),
    tail,
    outputTokens: stats?.outputTokens,
    bodies,
  };
}

/**
 * Every poll-transport source id owned by a still-LIVE member, fold state
 * notwithstanding — the always-on half of the poll set (the other half,
 * openSubfeedSourceIds, follows what the user has expanded).
 *
 * A collapsed badge used to have exactly one settling signal: the harness
 * notification landing with a matching tool_use_id. Everything else the
 * resolver trusts — the poll's done flag, the tail a settling scan reads —
 * only existed for OPEN folds, so a missed notification left the badge
 * amber forever and its final output unfetched. Polling every live member
 * keeps those signals flowing while there is anything to settle; the
 * moment a member settles it drops out of this set, so a quiesced session
 * polls nothing.
 *
 * WS members are excluded on the same grounds openSubfeedSourceIds excludes
 * them: their tail already arrives as task-output-delta frames, and the poll
 * route's done flag flips on the very notification a ws member settles by —
 * a poll would add a round trip and no signal.
 */
export function livePollSourceIds(
  watchers: ReadonlyMap<string, readonly ToolItem[]>,
  ctx: MemberContext,
): Set<string> {
  const ids = new Set<string>();
  for (const list of watchers.values()) {
    for (const item of list) {
      const member = resolveMember(item, ctx);
      if (member === null || member.settled) continue;
      if (member.source?.stream?.transport !== "poll") continue;
      ids.add(member.source.source_id);
    }
  }
  return ids;
}
