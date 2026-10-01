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
 * WHAT "LEFT" MEANS is the one shared detector's (left-view.ts): a departure
 * counts only after the row was SEEN (it can be out of view at the moment it
 * is selected, before the centering scroll lands), it is reported once, and a
 * DETACHED row (a page replace, a removal) is no departure and is logged and
 * ignored.
 *
 * ONE REPORT PER SELECTION. Moving the selection to another row (or ending it)
 * resets the watch, so a report about a row the reader has stepped away from
 * is never sent.
 *
 * Absent `IntersectionObserver` (a bare jsdom), there is no watch at all: the
 * same standing overscan.ts gives it.
 */
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../log.js";
import { createLeftViewWatch } from "./left-view.js";

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
  const detector = createLeftViewWatch(box);
  if (detector === null) return null;
  let current: { element: HTMLElement; unwatch: () => void } | null = null;
  return {
    watch: (row) => {
      if (row !== null && current !== null && row.element === current.element) return;
      current?.unwatch();
      current =
        row === null
          ? null
          : {
              element: row.element,
              unwatch: detector.watch(row.element, {
                onDetached: () => {
                  log.debug("the selected row was detached, not scrolled away; the selection stands", {
                    operation: "feed.selection-row-detached",
                    context: { row: row.id.value },
                  });
                },
                onLeft: () => {
                  log.info("the selected row left the viewport; the daemon is told", {
                    operation: "feed.selection-left-view",
                    context: { row: row.id.value },
                  });
                  leftView(row.id);
                },
              }),
            };
    },
    dispose: () => {
      current = null;
      detector.dispose();
    },
  };
}
