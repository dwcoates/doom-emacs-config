/**
 * context — the seam a card renderer is handed, declared LOCALLY.
 *
 * The feed core owns the row chrome (the wrapper element, its `data-feed-row`
 * and `data-row-kind` attributes, the upsert by `FeedId`) and calls one
 * renderer per activity unit to draw the row's BODY. This file declares the
 * shape of that call — nothing more — so the card renderers can be written,
 * typechecked and tested against it while `src/feed/renderers.ts` is being
 * written concurrently. The wiring agent reconciles the import: the shape here
 * is the shape the preamble prescribes, so reconciliation is a re-export, not
 * a redesign.
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

/** What a row renderer is told about the row it is drawing. */
export interface RowContext {
  /** The app-wide capabilities: the client, the workspace, the ticker. */
  ctx: AppContext;
  /** The feed this row belongs to — the root feed, or a bubble's sub-feed. */
  feed: FeedId | "root";
  /** The whole row, for renderers that need its identity or its parent. */
  row: FeedRow;
  /**
   * Find a row and mark it, opening what must open to get there; it NEVER
   * scrolls (the user owns the scroll, scroll.ts). Answers whether the row
   * could be reached (a collapsed shell bubble cannot, so `false` is a real
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
}
