/**
 * THE DAEMON'S `selectable` STAMP, AS A TEST STATES IT.
 *
 * The daemon decides which feed rows can be selected and stamps
 * `FeedRow.selectable` on them; a test that draws a selectable bubble states
 * that stamp through this one helper, whichever harness it runs under.
 */
import { create } from "@bufbuild/protobuf";
import { FeedRowSelectableSchema, type FeedRow } from "../../proto/gen/ts/frontend/v1/feed_pb";

/** ROW, stamped selectable as the daemon publishes a landed root-feed bubble. */
export function selectable(row: FeedRow): FeedRow {
  row.selectable = create(FeedRowSelectableSchema, {});
  return row;
}
