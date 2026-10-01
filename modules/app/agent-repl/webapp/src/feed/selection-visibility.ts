/**
 * selection-visibility — A SELECTION ENDS WHEN ITS ROW LEAVES THE VIEWPORT.
 *
 * While a row is selected (a final response or a prompt), this watches that
 * row against the feed's scroll box. Once the row has been SEEN in the
 * viewport, the first time it lies ENTIRELY outside it again (the reader
 * scrolled away, or new rows pushed it out) the daemon is told so, once, with
 * `SelectFeedRow { left_view { row } }`. The daemon ends the selection only if
 * that row is still the selected one, and pushes `none.stay`, which drops the
 * mark and leaves the viewport where the reader has it.
 *
 * WHY "SEEN" FIRST. A row is selected by a keystroke and then centered; it can
 * be out of view at the moment it is selected (an older response far above).
 * The observer's first report about it can come before the centering scroll
 * has landed, and a report of "not visible" then is no departure. So a
 * departure counts only after an arrival.
 *
 * ONE REPORT PER SELECTION. Moving the selection to another row (or ending it)
 * resets the watch, so a report about a row the reader has stepped away from
 * is never sent, and a row that keeps wandering in and out is reported once.
 *
 * A DETACHED ROW IS NOT A DEPARTURE. A page replace or a row removal detaches
 * the element; the observer then reports it not intersecting, which is no
 * scroll at all. Such a report is logged and ignored.
 *
 * Absent `IntersectionObserver` (a bare jsdom), there is no watch at all: the
 * same standing overscan.ts gives it.
 */
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../log.js";

/** What the chip's `request` evidence names a failed left-view report as. */
export const LEFT_VIEW_REQUEST = "end the selection of a row that left the view";

/** Watches the selected row and reports it leaving the viewport. */
export interface SelectionVisibility {
  /**
   * The selection now names ROW (its element and id), or nothing (null). A
   * call naming the element already watched keeps its state.
   */
  watch(row: { element: HTMLElement; id: FeedId } | null): void;
  /** Tear the observer down when the feed disposes. */
  dispose(): void;
}

/**
 * Build the watch rooted on the feed's scroll BOX. LEFTVIEW is told the id of
 * a selected row that left the viewport after having been seen in it.
 */
export function createSelectionVisibility(
  box: HTMLElement,
  leftView: (row: FeedId) => void,
): SelectionVisibility | null {
  const Ctor = (globalThis as { IntersectionObserver?: typeof IntersectionObserver })
    .IntersectionObserver;
  if (Ctor === undefined) return null;
  let current: { element: HTMLElement; id: FeedId; seen: boolean; reported: boolean } | null = null;
  const observer = new Ctor(
    (entries) => {
      for (const entry of entries) {
        if (current === null || entry.target !== current.element) continue;
        if (entry.isIntersecting) {
          current.seen = true;
          continue;
        }
        if (!current.element.isConnected) {
          log.debug("the selected row was detached, not scrolled away; the selection stands", {
            operation: "feed.selection-row-detached",
            context: { row: current.id.value },
          });
          continue;
        }
        if (!current.seen || current.reported) continue;
        current.reported = true;
        log.info("the selected row left the viewport; the daemon is told", {
          operation: "feed.selection-left-view",
          context: { row: current.id.value },
        });
        leftView(current.id);
      }
    },
    { root: box, threshold: 0 },
  );
  return {
    watch: (row) => {
      if (row !== null && current !== null && row.element === current.element) return;
      if (current !== null) observer.unobserve(current.element);
      current = row === null ? null : { ...row, seen: false, reported: false };
      if (current !== null) observer.observe(current.element);
    },
    dispose: () => {
      current = null;
      observer.disconnect();
    },
  };
}
