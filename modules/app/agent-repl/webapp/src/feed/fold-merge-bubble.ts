/**
 * fold-merge-bubble — THE WEBAPP'S ONE CALL OF `FoldMergeBubble`.
 *
 * A MERGE BUBBLE IS OPEN BY DEFAULT AND NEVER COLLAPSES AUTOMATICALLY, save
 * a merge that ended in success (owner ruling, 2026-10-08), and the reader's
 * fold outlives the page and wins over that default: the daemon records it on
 * the bubble's durable head row (endpoint_fold_merge_bubble.proto), so every
 * later push, page, reload and daemon restart draws the bubble the way the
 * reader left it. The webapp tells the daemon each fold the READER makes on a merge
 * bubble's head, and nothing else: never a fold the daemon applied, never a
 * jump's expansion, never a page replace's reopening.
 *
 * NONE ACTS ON THE ANSWER. The bubble already stands the way the reader left
 * it; the daemon's push carries the recorded fold, which the bubble reads as
 * a repeat.
 *
 * A REFUSED OR FAILED FOLD is an error the reader must see: it is filed on the
 * topbar's warning chip (`control_plane_failed`), the webapp's one error
 * surface, and logged. The bubble keeps the reader's toggle.
 */
import { create } from "@bufbuild/protobuf";
import {
  FoldMergeBubbleRequestSchema,
  FoldMergeBubbleResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_merge_bubble_pb";
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { controlCall } from "../rpc/control-call.js";

/** The chip's `request` evidence for a fold the reader made, by its arm. */
export const OPEN_MERGE_BUBBLE_REQUEST = "record that you opened a merge bubble";
export const CLOSE_MERGE_BUBBLE_REQUEST = "record that you closed a merge bubble";

/** FoldMergeBubble's own refusal arm, in the reader's words. */
const FOLD_MERGE_BUBBLE_REFUSALS = {
  notAMergeBubble: (value: { row?: { value: string } }) =>
    `that row is not a merge bubble the daemon holds (${value.row?.value ?? "no row named"})`,
};

/**
 * Record that the reader FOLDED (true: closed, false: opened) the merge bubble
 * whose head is ROW. Answers once the daemon has answered and any failure has
 * been filed; nothing is thrown for a refusal or a transport failure. A
 * malformed answer throws, for `guardMalformed`.
 */
export async function foldMergeBubble(ctx: AppContext, row: FeedId, folded: boolean): Promise<void> {
  const what = folded ? CLOSE_MERGE_BUBBLE_REQUEST : OPEN_MERGE_BUBBLE_REQUEST;
  const fold = folded
    ? ({ case: "close", value: {} } as const)
    : ({ case: "open", value: {} } as const);
  log.info(`the reader ${folded ? "closed" : "opened"} a merge bubble; recording the fold`, {
    operation: "feed.merge-fold-sent",
    context: { row: row.value, folded },
  });
  const success = await controlCall(ctx, {
    rpc: "FoldMergeBubble",
    what,
    send: (client) => client.foldMergeBubble(create(FoldMergeBubbleRequestSchema, { workspace: ctx.workspace, row, fold })),
    schema: FoldMergeBubbleResponseSchema,
    result: (response) => response.result,
    refusals: FOLD_MERGE_BUBBLE_REFUSALS,
    refusedOperation: "feed.merge-fold-refused",
    context: { row: row.value, folded },
  });
  if (success === undefined) return;
  log.debug("the daemon recorded the reader's merge bubble fold", {
    operation: "feed.merge-fold-recorded",
    context: { row: row.value, folded },
  });
}
