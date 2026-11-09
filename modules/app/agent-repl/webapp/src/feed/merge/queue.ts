/**
 * queue — the merge queue tab: who is ahead, this workspace, who is behind.
 *
 * THE SNAPSHOT IS STRUCTURAL. `ahead`, `current` and `behind` are three fields,
 * so this workspace's own row is READ, never derived by comparing workspace ids
 * against the page's own — the client would get that wrong the moment a
 * workspace appears twice or the ids are respelled, and it has no business
 * deciding it. That row is marked `data-queue-place="current"`, which the
 * stylesheet draws as a subtle highlight.
 *
 * A TABLE (owner request, 2026-10-01): a header row, then one row per entry,
 * every row the same height, in three columns — the workspace, the worktree
 * its merge lands in (`target`, owner request 2026-10-23) and how long it has
 * been in its stage. The workspace header counts the entries, "workspace (3)".
 * It is a COLUMN TABLE exactly as the expanded footer's
 * agents panel is (src/columns.ts): one grid whose header and rows share its
 * columns through `subgrid`, the duration column floored for "5hr 30m 30s"
 * and growing to its widest value. The duration ticks from the entry's
 * `stage_entered_at_ms`, so it starts over at zero whenever the daemon pushes
 * a new stage.
 *
 * EVERY ENTRY IS A CROSS-WORKSPACE JUMP, and a cross-workspace click is
 * `SelectWorkspace` and nothing else (R8): the editor switches, the roster
 * stream reflects it, and this bubble draws nothing on success. The refusal is
 * this click's own answer, so it lands AT THE ROW that was clicked — with the
 * four cross-cutting causes worded by the shared `refusalSentence` so a
 * transferring daemon reads the same here as it does in the rail.
 *
 * THE MERGE RUNNING WEARS THE MERGE PURPLE, every merge waiting is greyed
 * (owner request, 2026-10-23): the row's `data-queue-status`, read off the
 * entry's status arm, is what the stylesheet colors.
 *
 * A PAUSED QUEUE SAYS SO ABOVE ITS TABLE (owner request, 2026-10-23): a
 * "queue paused" banner in the paused amber, while the snapshot carries
 * `frontend.v1.FeedMergeQueue.paused`.
 *
 * A WAITING MERGE IS DRAGGED TO ANOTHER PLACE IN ITS LINE (owner request,
 * 2026-11-09; it was a pair of up and down chevrons), through the webapp's
 * one drag gesture (`drag.ts`), exactly as the sidebar's Merge Queue drags
 * one: the row lifts, its slot travels among the waiting rows, and the drop
 * asks `UpdateMergeQueue` to move it beside the waiting merge it now stands
 * next to (`UpdateMergeQueueMoveTo`, placed by `merge-queue-move.ts`). The
 * merge running is no grip. THE TABLE IS HELD WHILE THE ROW IS CARRIED AND
 * ASKED ABOUT (`createFeedMergeQueueTable`, the shared `hold.ts`), so a
 * snapshot pushed meanwhile never throws the dragged row away; the hold lets
 * go with the answer, drawing the newest snapshot. A refusal is drawn above
 * the table, outside the drawing the hold redraws.
 */
import { createControl } from "../../control.js";
import { columnHeader, COLUMNS_ROW_CLASS } from "../../columns.js";
import { pressToDrag, type DragSpec } from "../../drag.js";
import { liveElapsedClock } from "../../elapsed-clock.js";
import { createDrawingHold, type DrawingHold } from "../../hold.js";
import { log } from "../../log.js";
import { moveToAction, waitingMergeDrag } from "../../merge-queue-move.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { guardMalformed } from "../../rpc/guard.js";
import { UPDATE_MERGE_QUEUE_SENTENCES } from "../../rpc/merge-queue-refusal.js";
import { clearRefusals, drawMalformedRefusal, drawTransportRefusal, drawTypedRefusal } from "../../rpc/refuse.js";
import { userRequester } from "../../rpc/requester.js";
import { callUnary } from "../../rpc/unary.js";
import {
  UpdateMergeQueueResponseSchema,
  type UpdateMergeQueueMoveTo,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import type {
  FeedMergeQueue,
  FeedMergeQueueBubble,
  FeedMergeQueueEntry,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { stopTicking } from "../ticking.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { jumpToWorkspace, type JumpSite } from "./jump.js";
import { drawPausedMark } from "./paused.js";

const PATH = "FeedMergeQueue";

/** Where an entry stands relative to this workspace's own. */
export type QueuePlace = "ahead" | "current" | "behind";

/** The table's columns, in order, each named by its header. */
export const QUEUE_COLUMNS = ["workspace", "target", "duration"] as const;

/** The banner a paused queue draws above its table. */
export const QUEUE_PAUSED_TEXT = "queue paused";

/** The workspace column's header: its name and how many entries the table holds. */
export function workspaceHeaderText(count: number): string {
  return `workspace (${count})`;
}

/** The class every waiting row's drag lifts, and the table's drag kind. */
export const QUEUE_DRAG_KIND = "feed-merge-queue";

/** The class of the line a refused move is drawn in, above the table. */
export const QUEUE_STATUS_CLASS = "merge-queue-status";

/**
 * What a queue table's drags need from the panel the table is drawn in: the
 * hold on its drawing, the layer the dragged row's copy is drawn in, and the
 * line a refused move is drawn in. All of it outlives every redraw.
 */
export interface QueueArrange {
  hold: DrawingHold<FeedMergeQueue>;
  readonly layer: HTMLElement;
  status: HTMLElement;
}

/** A queue tab's drawing: its element, and the snapshot to draw, held while a row is dragged. */
export interface FeedMergeQueueTable {
  readonly element: HTMLElement;
  offer(queue: FeedMergeQueue): void;
}

/**
 * The queue tab's drawing: the line a refused move is drawn in, then the
 * table, redrawn in place from each snapshot offered unless a row is being
 * dragged or its move asked about (file comment).
 */
export function createFeedMergeQueueTable(rc: RowContext): FeedMergeQueueTable {
  const element = document.createElement("div");
  element.className = "merge-queue-panel";
  const status = document.createElement("div");
  status.className = QUEUE_STATUS_CLASS;
  let table: HTMLElement | null = null;
  const arrange: QueueArrange = {
    hold: createDrawingHold<FeedMergeQueue>({
      draw: (queue) => {
        const next = drawFeedMergeQueue(queue, rc, arrange);
        if (table === null) element.append(next);
        else {
          stopTicking(table);
          table.replaceWith(next);
        }
        table = next;
      },
      operation: "merge.queue-hold-released",
    }),
    // THE COPY IS DRAWN ON THE PAGE'S BODY, outside every bubble's scroll
    // and transform, read when a row lifts.
    get layer(): HTMLElement {
      return element.ownerDocument.body;
    },
    status,
  };
  element.append(status);
  return { element, offer: (queue) => arrange.hold.offer(queue) };
}

/**
 * The whole snapshot: ahead front-first, this workspace, behind
 * nearest-first. A press on a waiting row drags it among the waiting, through
 * ARRANGE (`queueEntryDrag`).
 */
export function drawFeedMergeQueue(queue: FeedMergeQueue, rc: RowContext, arrange: QueueArrange): HTMLElement {
  log.debug("drawing a merge queue snapshot", {
    operation: "merge.draw-queue",
    context: { ahead: queue.ahead.length, behind: queue.behind.length },
  });
  const el = document.createElement("div");
  el.className = "merge-queue list-rows";
  const rows: { entry: FeedMergeQueueEntry; place: QueuePlace }[] = [
    ...queue.ahead.map((entry) => ({ entry, place: "ahead" as const })),
    { entry: requireMessage(queue.current, `${PATH}.current`), place: "current" },
    ...queue.behind.map((entry) => ({ entry, place: "behind" as const })),
  ];
  if (queue.paused !== undefined) {
    el.setAttribute("data-queue-paused", "");
    el.append(drawPausedMark("merge-queue-paused", QUEUE_PAUSED_TEXT));
  }
  el.append(drawQueueHeader(rows.length));
  const refs = new Map<HTMLElement, WorkspaceRef>();
  rows.forEach(({ entry, place }, i) => {
    const drawn = drawFeedMergeQueueEntry(entry, rc, place);
    if (entry.status.case === "waiting") {
      refs.set(drawn, requireMessage(requireMessage(entry.workspace, `${PATH}.rows[${i}].workspace`).ref, `${PATH}.rows[${i}].workspace.ref`));
    }
    el.append(drawn);
  });
  el.addEventListener("pointerdown", (event) => {
    if (event.button !== 0 || !event.isPrimary) return;
    const spec = queueEntryDrag(rc, arrange, el, event.target as Element | null, refs);
    if (spec !== null) pressToDrag(arrange, spec, event);
  });
  return el;
}

/**
 * A press on a waiting row of TABLE drags it among the waiting rows there; a
 * drop elsewhere than where it started moves it beside the waiting merge it
 * now stands next to. The merge running is no grip, nor is a refusal drawn
 * in a row. REFS names each waiting row's workspace.
 */
export function queueEntryDrag(
  rc: RowContext,
  arrange: QueueArrange,
  table: HTMLElement,
  target: Element | null,
  refs: ReadonlyMap<HTMLElement, WorkspaceRef>,
): DragSpec | null {
  if (target === null || target.closest(".refusal") !== null) return null;
  const item = target.closest<HTMLElement>(".merge-queue-entry");
  if (item === null || item.parentElement !== table) return null;
  if (!refs.has(item)) return null;
  return waitingMergeDrag({
    kind: QUEUE_DRAG_KIND,
    item,
    list: table,
    box: table,
    refs,
    dressGhost: (ghost) => dressQueueGhost(ghost, item, table),
    move: (workspace, place) => void guardMalformed(rc.ctx, "feed.merge.queue.move", moveInQueue(rc, arrange, workspace, place)),
  });
}

/**
 * Dress GHOST, the copy of ITEM, for the page's body: a row of TABLE takes
 * its columns from the table it no longer stands in, so the copy is laid out
 * at the widths the row was drawn at, in the table's own font.
 */
export function dressQueueGhost(ghost: HTMLElement, item: HTMLElement, table: HTMLElement): void {
  const line = item.querySelector<HTMLElement>(":scope > .merge-queue-line");
  if (line === null) throw new Error("invariant: every merge queue row draws its line");
  const tableStyle = getComputedStyle(table);
  ghost.style.display = "grid";
  ghost.style.gridTemplateColumns = [...line.children].map((cell) => `${cell.getBoundingClientRect().width}px`).join(" ");
  ghost.style.columnGap = tableStyle.columnGap;
  ghost.style.font = tableStyle.font;
  const ghostLine = ghost.querySelector<HTMLElement>(":scope > .merge-queue-line");
  if (ghostLine === null) throw new Error("invariant: a merge queue row's copy draws its line");
  ghostLine.style.display = "grid";
  ghostLine.style.gridTemplateColumns = "subgrid";
  ghostLine.style.gridColumn = "1 / -1";
}

/**
 * The header row: one header per column, above its column, the workspace
 * header counting the COUNT entries beneath it.
 */
function drawQueueHeader(count: number): HTMLElement {
  const header = document.createElement("div");
  header.className = `merge-queue-header ${COLUMNS_ROW_CLASS}`;
  for (const column of QUEUE_COLUMNS) {
    const cell = columnHeader(column);
    if (column === "workspace") cell.textContent = workspaceHeaderText(count);
    header.append(cell);
  }
  return header;
}

/**
 * One entry: its name, the worktree it lands in, how long it has been in its
 * stage, and its jump.
 */
export function drawFeedMergeQueueEntry(entry: FeedMergeQueueEntry, rc: RowContext, place: QueuePlace): HTMLElement {
  const status = requireCase(entry.status, `${PATH}Entry.status`);
  const ref = requireMessage(
    requireMessage(entry.workspace, `${PATH}Entry.workspace`).ref,
    `${PATH}Entry.workspace.ref`,
  );

  const el = document.createElement("div");
  el.className = `merge-queue-entry ${COLUMNS_ROW_CLASS}`;
  el.setAttribute("data-queue-place", place);
  el.setAttribute("data-queue-status", status.case);

  const line = createControl();
  line.className = `merge-queue-line ${COLUMNS_ROW_CLASS}`;
  line.setAttribute("data-select", "");
  el.append(line);

  const workspace = document.createElement("span");
  workspace.className = "merge-queue-workspace";
  const label = document.createElement("span");
  label.className = "merge-queue-label";
  label.textContent = requireMessage(entry.label, `${PATH}Entry.label`).text;
  workspace.append(label);
  line.append(workspace);

  const target = requireMessage(entry.target, `${PATH}Entry.target`);
  const targetCell = document.createElement("span");
  targetCell.className = "merge-queue-target";
  targetCell.textContent = target.text;
  targetCell.title = target.dir;
  line.append(targetCell);

  let enteredMs: number;
  switch (status.case) {
    case "merging":
      enteredMs = msOf(status.value.stageEnteredAtMs, `${PATH}Merging.stage_entered_at_ms`);
      break;
    case "waiting":
      enteredMs = msOf(status.value.stageEnteredAtMs, `${PATH}Waiting.stage_entered_at_ms`);
      break;
    default:
      return unreachableArm(`${PATH}Entry.status`, armName(status));
  }

  line.append(liveElapsedClock(rc.ctx.ticker, "footer-row-clock merge-queue-duration", enteredMs));

  line.addEventListener("click", () => {
    void guardMalformed(
      rc.ctx,
      "feed.merge.queue.select",
      entry.bubble === undefined
        ? selectQueueEntryWorkspace(el, rc, ref)
        : openQueueEntryBubble(el, rc, entry.bubble),
    );
  });
  return el;
}

/** Where a merge queue entry's jump is recorded and its refusal drawn. */
const QUEUE_JUMP: JumpSite = {
  describe: "a merge queue entry",
  operation: "merge.queue-select",
  refusalClass: "merge-queue-refusal",
};

/**
 * The jump: `SelectWorkspace`, echoed verbatim, answered at the row
 * (`jumpToWorkspace`).
 */
export function selectQueueEntryWorkspace(
  entry: HTMLElement,
  rc: RowContext,
  workspace: WorkspaceRef,
): Promise<void> {
  return jumpToWorkspace(entry, rc.ctx, workspace, QUEUE_JUMP);
}

/**
 * An ASYNC merge's jump (owner ruling, 2026-10-17): its entry is the hidden
 * merge workspace, which a click never selects. It selects the requester and
 * opens the merge's bubble in its feed (`FeedMergeQueueBubble`); a bubble with
 * no row (a merge this daemon holds no bubble identity for) selects the
 * requester alone.
 */
export function openQueueEntryBubble(
  entry: HTMLElement,
  rc: RowContext,
  bubble: FeedMergeQueueBubble,
): Promise<void> {
  const requester = requireMessage(bubble.requester, `${PATH}Bubble.requester`);
  return jumpToWorkspace(entry, rc.ctx, requester, QUEUE_JUMP, bubble.row);
}

/**
 * Ask the daemon to move WORKSPACE's waiting merge to PLACE, the table held
 * by ARRANGE until the answer: the hold then lets go, drawing the newest
 * snapshot, and a refusal is drawn at ARRANGE's status line.
 */
export async function moveInQueue(
  rc: RowContext,
  arrange: QueueArrange,
  workspace: WorkspaceRef,
  place: UpdateMergeQueueMoveTo["place"],
): Promise<void> {
  const context = { workspace: workspace.id, side: place.case, anchor: place.value?.id };
  log.info("moving a merge in its queue from the feed", { operation: "merge.queue-move", context });
  arrange.hold.await();
  try {
    const response = await callUnary(
      rc.ctx,
      "UpdateMergeQueue",
      // The webapp acts on the queue for the user alone, and says so: every
      // queue action names who asked.
      (client) =>
        client.updateMergeQueue({
          action: moveToAction(workspace, place),
          requester: userRequester(),
        }),
      UpdateMergeQueueResponseSchema,
    );
    const result = requireCase(response.result, "UpdateMergeQueueResponse.result");
    switch (result.case) {
      case "success":
        // The snapshot is republished in its new order.
        clearRefusals(arrange.status);
        return;
      case "error": {
        const arm = drawTypedRefusal(
          arrange.status,
          "UpdateMergeQueueError.cause",
          "UpdateMergeQueue",
          result.value.cause,
          UPDATE_MERGE_QUEUE_SENTENCES,
        );
        log.warn(`UpdateMergeQueue refused a move request: ${arm}`, {
          operation: "merge.queue-move-refused",
          context: { arm, ...context },
        });
        return;
      }
      default:
        return unreachableArm("UpdateMergeQueueResponse.result", armName(result));
    }
  } catch (err) {
    if (drawMalformedRefusal(rc.ctx, arrange.status, "merge.queue-move-unreadable", err)) return;
    drawTransportRefusal(arrange.status, err);
    log.error(`UpdateMergeQueue failed for a move request: ${String(err)}`, {
      operation: "merge.queue-move-failed",
      context: { ...context, cause: err },
    });
  } finally {
    // EVERY ANSWER LETS GO: the drop rearranged the table's DOM, which only
    // the newest snapshot may stand in for.
    arrange.hold.settle(null);
  }
}
