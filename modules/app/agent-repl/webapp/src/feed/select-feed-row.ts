/**
 * select-feed-row — THE WEBAPP'S ONE CALL OF `SelectFeedRow`.
 *
 * The daemon owns the feed's selection (endpoint_select_feed_row.proto). The
 * webapp moves it in three ways: a click on a bubble (`bubble`, which the
 * daemon answers by deselecting when it names the selected one,
 * bubble-selection.ts), a click on the feed's
 * background (`clear`, background-click.ts), and the selected row leaving the
 * viewport entirely (`left_view`, selection-visibility.ts). None acts on the
 * answer: the daemon's selection push on the feed's watch is what changes the
 * feed (`applySelection`). So every caller sends through here and shares one
 * reading of the answer.
 *
 * A REFUSED OR FAILED MOVE is an error the reader must see: it is filed on the
 * topbar's warning chip (`control_plane_failed`), the webapp's one error
 * surface, and logged.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  SelectFeedRowRequestSchema,
  SelectFeedRowResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_feed_row_pb";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { controlCall } from "../rpc/control-call.js";
import { requireCase } from "../rpc/strict.js";

/** SelectFeedRow's own refusal arm, in the reader's words. */
const SELECT_FEED_ROW_REFUSALS = {
  notSelectable: (value: { row?: { value: string } }) =>
    `that bubble cannot be selected (${value.row?.value ?? "no row named"})`,
};

/** The move one call makes: an arm of `SelectFeedRowRequest.move`. */
type FeedRowMove = NonNullable<MessageInitShape<typeof SelectFeedRowRequestSchema>["move"]>;

/**
 * Send MOVE for this page's workspace. WHAT is the chip's `request` evidence
 * (what the reader would say they asked for). Answers once the daemon has
 * answered and any failure has been filed; nothing is thrown for a refusal or
 * a transport failure. A malformed answer throws, for `guardMalformed`.
 */
export async function selectFeedRow(ctx: AppContext, move: FeedRowMove, what: string): Promise<void> {
  const success = await controlCall(ctx, {
    rpc: "SelectFeedRow",
    what,
    send: (client) => client.selectFeedRow(create(SelectFeedRowRequestSchema, { workspace: ctx.workspace, move })),
    schema: SelectFeedRowResponseSchema,
    result: (response) => response.result,
    refusals: SELECT_FEED_ROW_REFUSALS,
    refusedOperation: "feed.select-feed-row-refused",
    context: { move: move.case ?? "unset" },
  });
  if (success === undefined) return;
  const outcome = requireCase(success.outcome, "SelectFeedRowSuccess.outcome");
  log.debug(`the daemon applied the selection move (${what}); its push changes the feed`, {
    operation: "feed.select-feed-row-answered",
    context: { move: move.case ?? "unset", outcome: outcome.case },
  });
}
