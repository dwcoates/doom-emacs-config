/**
 * THE FIXTURES' ORDER KEYS — every test row carries `FeedRow.order`, as every
 * row the daemon serves does.
 *
 * NOT A SUITE. A fixture row's key is minted the FIRST time its id is seen in a
 * test and reused for every later build of that id, so a re-push of a row keeps
 * its key exactly as the daemon's does (FeedRowOrder: fixed for the row's life),
 * and rows built in the order a test builds them sort in that order. The
 * registry is reset before every test (test/setup.ts), so no test's keys leak
 * into the next.
 *
 * A test about ORDER states its keys itself (`withOrder`), and never relies on
 * build order: a late row's key is the point of such a test.
 */
import { clone, create } from "@bufbuild/protobuf";
import {
  FeedRowOrderSchema,
  FeedRowSchema,
  type FeedRow,
  type FeedRowOrder,
} from "../../proto/gen/ts/frontend/v1/feed_pb";

const minted = new Map<string, string>();

/** The order key fixture row ID carries: minted on first sight, then kept. */
export function orderFor(id: string): FeedRowOrder {
  let key = minted.get(id);
  if (key === undefined) {
    key = `k${minted.size.toString().padStart(8, "0")}`;
    minted.set(id, key);
  }
  return create(FeedRowOrderSchema, { key });
}

/** A copy of ROW carrying exactly KEY. */
export function withOrder(row: FeedRow, key: string): FeedRow {
  const copy = clone(FeedRowSchema, row);
  copy.order = create(FeedRowOrderSchema, { key });
  return copy;
}

/** A copy of ROW carrying no order at all: the malformed row. */
export function withoutOrder(row: FeedRow): FeedRow {
  const copy = clone(FeedRowSchema, row);
  copy.order = undefined;
  return copy;
}

/** Forget every minted key. Called before every test. */
export function resetOrderKeys(): void {
  minted.clear();
}
