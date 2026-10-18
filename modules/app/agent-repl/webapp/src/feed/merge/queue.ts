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
 * every row the same height, in three columns — the workspace, its stage (the
 * front's active tab; "waiting" for everyone behind it) and how long it has
 * been in that stage. It is a COLUMN TABLE exactly as the expanded footer's
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
 * THE FRONT ENTRY SHOWS WHAT IT IS DOING, as the very same `FeedMergeTabLabel`
 * its own bubble draws — imported, never respelled — so a waiting user watches
 * real progress instead of a spinner.
 *
 * EVERY WAITING ROW CARRIES TWO MOVE CONTROLS (owner request, 2026-10-12), a
 * subtle up chevron and down chevron left of the workspace's name, which ask
 * `UpdateMergeQueue` to move that merge one place up or down. The merge
 * running carries none, and an empty slot of the same width keeps every name
 * in one column. A chevron with no waiting merge on its side is disabled: the
 * snapshot's order is read, the first waiting row's neighbor above it is the
 * merge running, and the last row has nothing below it. Success draws nothing:
 * the daemon republishes the snapshot in its new order. A refusal lands at the
 * row, as the jump's does.
 */
import { createControl, labelledControl, type Control } from "../../control.js";
import { columnHeader, COLUMNS_ROW_CLASS } from "../../columns.js";
import { liveElapsedClock } from "../../elapsed-clock.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { guardMalformed } from "../../rpc/guard.js";
import {
  drawMalformedRefusal,
  drawTransportRefusal,
  drawTypedRefusal,
  type SentenceTable,
} from "../../rpc/refuse.js";
import { callUnary } from "../../rpc/unary.js";
import { UpdateMergeQueueResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import type {
  FeedMergeQueue,
  FeedMergeQueueBubble,
  FeedMergeQueueEntry,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { jumpToWorkspace, type JumpSite } from "./jump.js";
import { drawFeedMergeTabLabel } from "./tab-strip.js";

const PATH = "FeedMergeQueue";

/** Where an entry stands relative to this workspace's own. */
export type QueuePlace = "ahead" | "current" | "behind";

/** The table's columns, in order, each named by its header. */
export const QUEUE_COLUMNS = ["workspace", "stage", "duration"] as const;

/** A waiting entry's stage, drawn in the stage column. */
export const WAITING_STAGE = "waiting";

/** Which way a move control moves its row's merge. */
export type QueueMove = "up" | "down";

/** Each move control's accessible name, by its direction. */
export const MOVE_LABELS: Readonly<Record<QueueMove, string>> = {
  up: "move up the merge queue",
  down: "move down the merge queue",
};

/** Whether a row's merge has a waiting merge to pass above it and below it. */
export interface QueueMoves {
  up: boolean;
  down: boolean;
}

/** The whole snapshot: ahead front-first, this workspace, behind nearest-first. */
export function drawFeedMergeQueue(queue: FeedMergeQueue, rc: RowContext): HTMLElement {
  log.debug("drawing a merge queue snapshot", {
    operation: "merge.draw-queue",
    context: { ahead: queue.ahead.length, behind: queue.behind.length },
  });
  const el = document.createElement("div");
  el.className = "merge-queue list-rows";
  el.append(drawQueueHeader());
  const rows: { entry: FeedMergeQueueEntry; place: QueuePlace }[] = [
    ...queue.ahead.map((entry) => ({ entry, place: "ahead" as const })),
    { entry: requireMessage(queue.current, `${PATH}.current`), place: "current" },
    ...queue.behind.map((entry) => ({ entry, place: "behind" as const })),
  ];
  const waiting = rows.map(({ entry }) => entry.status.case === "waiting");
  rows.forEach(({ entry, place }, i) => {
    const moves: QueueMoves = {
      up: waiting[i] && i > 0 && waiting[i - 1],
      down: waiting[i] && i < rows.length - 1 && waiting[i + 1],
    };
    el.append(drawFeedMergeQueueEntry(entry, rc, place, moves));
  });
  return el;
}

/** The header row: one header per column, above its column. */
function drawQueueHeader(): HTMLElement {
  const header = document.createElement("div");
  header.className = `merge-queue-header ${COLUMNS_ROW_CLASS}`;
  for (const column of QUEUE_COLUMNS) header.append(columnHeader(column));
  return header;
}

/**
 * One entry: its move controls and name, its stage and how long it has been in
 * it, and its jump. MOVES says which of a waiting entry's chevrons have a
 * waiting merge to pass.
 */
export function drawFeedMergeQueueEntry(
  entry: FeedMergeQueueEntry,
  rc: RowContext,
  place: QueuePlace,
  moves: QueueMoves,
): HTMLElement {
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
  const moveSlot = document.createElement("span");
  moveSlot.className = "merge-queue-moves";
  const label = document.createElement("span");
  label.className = "merge-queue-label";
  label.textContent = requireMessage(entry.label, `${PATH}Entry.label`).text;
  workspace.append(moveSlot, label);
  line.append(workspace);

  const stage = document.createElement("span");
  stage.className = "merge-queue-stage";
  let enteredMs: number;
  switch (status.case) {
    case "merging":
      stage.append(
        drawFeedMergeTabLabel(
          requireMessage(status.value.activeTab, `${PATH}Merging.active_tab`),
        ),
      );
      enteredMs = msOf(status.value.stageEnteredAtMs, `${PATH}Merging.stage_entered_at_ms`);
      break;
    case "waiting":
      stage.textContent = WAITING_STAGE;
      moveSlot.append(drawMoveControl(el, rc, ref, "up", moves.up), drawMoveControl(el, rc, ref, "down", moves.down));
      enteredMs = msOf(status.value.stageEnteredAtMs, `${PATH}Waiting.stage_entered_at_ms`);
      break;
    default:
      return unreachableArm(`${PATH}Entry.status`, armName(status));
  }
  line.append(stage);

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
  return jumpToWorkspace(entry, rc, workspace, QUEUE_JUMP);
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
  return jumpToWorkspace(entry, rc, requester, QUEUE_JUMP, bubble.row);
}

/** The refusals of a move request that are this endpoint's own. */
const MOVE_SENTENCES: SentenceTable = {
  noSuchQueuedMerge: () => "this merge is no longer waiting on the queue",
};

/**
 * One move control: a chevron pointing MOVE's way, named for screen readers
 * and hover by its label, disabled when there is no waiting merge to pass
 * (ENABLED false). It sits inside the row's jump, so its click is kept from
 * reaching the jump.
 */
function drawMoveControl(
  entry: HTMLElement,
  rc: RowContext,
  workspace: WorkspaceRef,
  move: QueueMove,
  enabled: boolean,
): Control {
  const control = labelledControl({
    className: "merge-queue-move",
    hook: ["data-merge-move", move],
    label: "",
    onClick: (clicked) => {
      void guardMalformed(rc.ctx, "feed.merge.queue.move", moveInQueue(entry, rc, workspace, move, clicked));
    },
  });
  control.setAttribute("aria-label", MOVE_LABELS[move]);
  control.title = MOVE_LABELS[move];
  control.disabled = !enabled;
  control.addEventListener("click", (event: MouseEvent) => event.stopPropagation());
  return control;
}

/** The `UpdateMergeQueue` action that moves WORKSPACE's merge MOVE's way. */
function moveAction(workspace: WorkspaceRef, move: QueueMove) {
  return move === "up"
    ? { case: "moveUp" as const, value: { workspace } }
    : { case: "moveDown" as const, value: { workspace } };
}

/**
 * Ask the daemon to move WORKSPACE's merge one place MOVE's way, drawing a
 * refusal at ENTRY. The control is disabled while the call is out, and given
 * back when it was refused.
 */
export async function moveInQueue(
  entry: HTMLElement,
  rc: RowContext,
  workspace: WorkspaceRef,
  move: QueueMove,
  control: Control,
): Promise<void> {
  log.info("moving a merge one place in its queue", {
    operation: "merge.queue-move",
    context: { workspace: workspace.id, move },
  });
  control.disabled = true;
  try {
    const response = await callUnary(
      rc.ctx,
      "UpdateMergeQueue",
      (client) => client.updateMergeQueue({ action: moveAction(workspace, move) }),
      UpdateMergeQueueResponseSchema,
    );
    const result = requireCase(response.result, "UpdateMergeQueueResponse.result");
    switch (result.case) {
      case "success":
        // Nothing is drawn: the snapshot is republished in its new order.
        return;
      case "error": {
        const arm = drawTypedRefusal(
          entry,
          "UpdateMergeQueueError.cause",
          "UpdateMergeQueue",
          result.value.cause,
          MOVE_SENTENCES,
        );
        log.warn(`UpdateMergeQueue refused a move request: ${arm}`, {
          operation: "merge.queue-move-refused",
          context: { arm, workspace: workspace.id, move },
        });
        control.disabled = false;
        return;
      }
      default:
        return unreachableArm("UpdateMergeQueueResponse.result", armName(result));
    }
  } catch (err) {
    if (drawMalformedRefusal(rc.ctx, entry, "merge.queue-move-unreadable", err)) return;
    drawTransportRefusal(entry, err);
    log.error(`UpdateMergeQueue failed for a move request: ${String(err)}`, {
      operation: "merge.queue-move-failed",
      context: { workspace: workspace.id, move, cause: err },
    });
    control.disabled = false;
  }
}
