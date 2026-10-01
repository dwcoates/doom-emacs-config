/**
 * select-feed-row — THE WEBAPP'S ONE CALL OF `SelectFeedRow`.
 *
 * The daemon owns the feed's selection (endpoint_select_feed_row.proto). The
 * webapp moves it in exactly two ways, both of which end it: a click on the
 * feed's background (`clear`, background-click.ts) and the selected row leaving
 * the viewport entirely (`left_view`, selection-visibility.ts). Neither acts on
 * the answer: the daemon's selection push on the feed's watch is what changes
 * the feed (`applySelection`). So both callers send through here and share one
 * reading of the answer.
 *
 * A REFUSED OR FAILED MOVE is an error the reader must see: it is filed on the
 * topbar's warning chip (`control_plane_failed`), the webapp's one error
 * surface, and logged.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { ConnectError } from "@connectrpc/connect";
import {
  SelectFeedRowRequestSchema,
  SelectFeedRowResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_feed_row_pb";
import { controlPlaneFailed } from "../failure/sink.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { callFailure, refusalOf } from "../rpc/refuse.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";

/** The move one call makes: an arm of `SelectFeedRowRequest.move`. */
type FeedRowMove = NonNullable<MessageInitShape<typeof SelectFeedRowRequestSchema>["move"]>;

/**
 * Send MOVE for this page's workspace. WHAT is the chip's `request` evidence
 * (what the reader would say they asked for). Answers once the daemon has
 * answered and any failure has been filed; nothing is thrown for a refusal or
 * a transport failure. A malformed answer throws, for `guardMalformed`.
 */
export async function selectFeedRow(ctx: AppContext, move: FeedRowMove, what: string): Promise<void> {
  let response;
  try {
    response = await callUnary(
      ctx,
      "SelectFeedRow",
      (client) =>
        client.selectFeedRow(create(SelectFeedRowRequestSchema, { workspace: ctx.workspace, move })),
      SelectFeedRowResponseSchema,
    );
  } catch (err) {
    // `callUnary` already logged the transport failure; the chip is told here.
    if (!(err instanceof ConnectError)) throw err;
    ctx.failures.report(controlPlaneFailed(what, callFailure(err).text));
    return;
  }
  const result = requireCase(response.result, "SelectFeedRowResponse.result");
  switch (result.case) {
    case "success": {
      const outcome = requireCase(result.value.outcome, "SelectFeedRowSuccess.outcome");
      log.debug(`the daemon applied the selection move (${what}); its push changes the feed`, {
        operation: "feed.select-feed-row-answered",
        context: { move: move.case ?? "unset", outcome: outcome.case },
      });
      return;
    }
    case "error": {
      const said = refusalOf(result.value.cause, {}, "SelectFeedRowError.cause");
      log.error(`the daemon refused to ${what}: ${said.text}`, {
        operation: "feed.select-feed-row-refused",
        context: { move: move.case ?? "unset", arm: said.arm },
      });
      ctx.failures.report(controlPlaneFailed(what, said.text));
      return;
    }
    default: {
      const other: { case: string } = result;
      return unreachableArm("SelectFeedRowResponse.result", other.case);
    }
  }
}
