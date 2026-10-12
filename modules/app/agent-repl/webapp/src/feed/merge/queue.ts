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
 * THIS WORKSPACE'S OWN WAITING ROW CARRIES A FRONT CONTROL, which asks
 * `UpdateMergeQueue` to move this merge ahead of every ordinary merge waiting.
 * Success draws nothing: the daemon republishes the snapshot in its new order.
 * A refusal lands at the row, as the jump's does.
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

/** The words on this workspace's own waiting row's front control. */
export const FRONT_LABEL = "move to front";

/** The whole snapshot: ahead front-first, this workspace, behind nearest-first. */
export function drawFeedMergeQueue(queue: FeedMergeQueue, rc: RowContext): HTMLElement {
  log.debug("drawing a merge queue snapshot", {
    operation: "merge.draw-queue",
    context: { ahead: queue.ahead.length, behind: queue.behind.length },
  });
  const el = document.createElement("div");
  el.className = "merge-queue list-rows";
  el.append(drawQueueHeader());
  for (const entry of queue.ahead) el.append(drawFeedMergeQueueEntry(entry, rc, "ahead"));
  el.append(
    drawFeedMergeQueueEntry(requireMessage(queue.current, `${PATH}.current`), rc, "current"),
  );
  for (const entry of queue.behind) el.append(drawFeedMergeQueueEntry(entry, rc, "behind"));
  return el;
}

/** The header row: one header per column, above its column. */
function drawQueueHeader(): HTMLElement {
  const header = document.createElement("div");
  header.className = `merge-queue-header ${COLUMNS_ROW_CLASS}`;
  for (const column of QUEUE_COLUMNS) header.append(columnHeader(column));
  return header;
}

/** One entry: its name, its stage and how long it has been in it, and its jump. */
export function drawFeedMergeQueueEntry(
  entry: FeedMergeQueueEntry,
  rc: RowContext,
  place: QueuePlace,
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

  const label = document.createElement("span");
  label.className = "merge-queue-label";
  label.textContent = requireMessage(entry.label, `${PATH}Entry.label`).text;
  line.append(label);

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
      if (place === "current") stage.append(drawFrontControl(el, rc, ref));
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
      selectQueueEntryWorkspace(el, rc, ref),
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

/** The refusals of a front request that are this endpoint's own. */
const FRONT_SENTENCES: SentenceTable = {
  noSuchQueuedMerge: () => "this merge is no longer waiting on the queue",
};

/**
 * The front control. It sits inside the row's jump, so its click is kept from
 * reaching the jump.
 */
function drawFrontControl(entry: HTMLElement, rc: RowContext, workspace: WorkspaceRef): Control {
  const control = labelledControl({
    className: "merge-queue-front",
    hook: ["data-merge-front", ""],
    label: FRONT_LABEL,
    onClick: (clicked) => {
      void guardMalformed(rc.ctx, "feed.merge.queue.front", moveToFront(entry, rc, workspace, clicked));
    },
  });
  control.addEventListener("click", (event: MouseEvent) => event.stopPropagation());
  return control;
}

/**
 * Ask the daemon to move WORKSPACE's merge to the front, drawing a refusal at
 * ENTRY. The control is disabled while the call is out, and given back when it
 * was refused.
 */
export async function moveToFront(
  entry: HTMLElement,
  rc: RowContext,
  workspace: WorkspaceRef,
  control: Control,
): Promise<void> {
  log.info("moving a merge to the front of its queue", {
    operation: "merge.queue-front",
    context: { workspace: workspace.id },
  });
  control.disabled = true;
  try {
    const response = await callUnary(
      rc.ctx,
      "UpdateMergeQueue",
      (client) => client.updateMergeQueue({ action: { case: "front", value: { workspace } } }),
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
          FRONT_SENTENCES,
        );
        log.warn(`UpdateMergeQueue refused a front request: ${arm}`, {
          operation: "merge.queue-front-refused",
          context: { arm, workspace: workspace.id },
        });
        control.disabled = false;
        return;
      }
      default:
        return unreachableArm("UpdateMergeQueueResponse.result", armName(result));
    }
  } catch (err) {
    if (drawMalformedRefusal(rc.ctx, entry, "merge.queue-front-unreadable", err)) return;
    drawTransportRefusal(entry, err);
    log.error(`UpdateMergeQueue failed for a front request: ${String(err)}`, {
      operation: "merge.queue-front-failed",
      context: { workspace: workspace.id, cause: err },
    });
    control.disabled = false;
  }
}
