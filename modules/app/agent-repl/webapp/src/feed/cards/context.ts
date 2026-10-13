/**
 * context — the seam a card renderer is handed: THE ONE `RowContext`.
 *
 * The feed core owns the row chrome (the wrapper element, its `data-feed-row`
 * and `data-row-kind` attributes, the upsert by `FeedId`) and calls one
 * renderer per activity unit to draw the row's BODY. This file declares the
 * shape of that call — nothing more — and `src/feed/renderers.ts` re-exports
 * it, so the core and the cards are handed one declaration that cannot drift
 * into two.
 *
 * WHY `previous` IS PART OF THE CALL. A re-push redraws a row WHOLE — the core
 * replaces the body element — so a renderer keeps no state of its own between
 * pushes. Anything that must survive one (a type-out's position, a locally
 * toggled disclosure) is carried on the element the previous draw returned and
 * re-applied by the next draw. That is the only channel: a module-level cache
 * keyed by row id would outlive the row it described.
 */
import type { FeedId, FeedRow } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { AppContext } from "../../rpc/context.js";

/**
 * How a row first entered its feed: a live PUSH on the feed's tail, or a PAGE
 * (the opening page, a re-open, an older page the walk loaded). Only a pushed
 * row is new to a reader watching, so only its card types its title out
 * (title-reveal.ts); a paged row is history and is drawn whole.
 */
export type RowArrival = "push" | "page";

/** What a row renderer is told about the row it is drawing. */
export interface RowContext {
  /** The app-wide capabilities: the client, the workspace, the ticker. */
  ctx: AppContext;
  /** The feed this row belongs to — the root feed, or a bubble's sub-feed. */
  feed: FeedId | "root";
  /** The whole row, for renderers that need its identity or its parent. */
  row: FeedRow;
  /** How the row first entered this feed; fixed for the row's life. */
  readonly arrival: RowArrival;
  /**
   * Jump to a row: open what must open to get there, expand the entry, center
   * it (`entryJumped`, scroll.ts; owner request, 2026-10-01) and mark it.
   * Answers whether the row could be reached (a collapsed shell bubble cannot, so `false` is a real
   * answer, not an error).
   */
  readonly revealRow: (id: FeedId) => Promise<boolean>;
  /**
   * The body element the PREVIOUS draw of this same row returned, when there
   * was one. Absent on a row's first draw. A renderer MAY update it in place
   * and return it, and the core then replaces nothing: that is how a bubble
   * whose scroll box the reader is inside keeps its position across a push.
   */
  previous?: HTMLElement;
  /**
   * Find another row's element ON THIS FEED. Provided by the controller that
   * owns the rows; a renderer with nothing to look up simply never calls it.
   */
  findRowElement?(id: FeedId): HTMLElement | null;
}
