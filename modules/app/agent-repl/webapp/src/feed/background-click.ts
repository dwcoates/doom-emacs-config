/**
 * background-click — a click on the feed OUTSIDE ANY BUBBLE ends the feed's
 * selection (owner ruling, 2026-09-23).
 *
 * While a row (a final response or a prompt) is selected the latest-visible
 * follow is held off (`TailFollow.selectionMoved`), so streaming rows cannot
 * pull the reader off the row they picked. The hold-off lasts until the reader
 * clicks the feed's background: that click asks the DAEMON to clear the
 * selection (`SelectFeedRow` with the `clear` move, the move double-escape
 * sends), because the daemon owns the selection. Nothing is cleared here. The
 * daemon's `none.return_to_tail` push then reaches `applySelection`, which parks
 * at the tail through the existing named cause (`selectionCleared`), and the
 * follow resumes.
 *
 * A REFUSED OR FAILED CLEAR is filed and logged by `selectFeedRow`.
 */
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { selectFeedRow } from "./select-feed-row.js";

/** The class every root row's full-width wrapper wears (feed-view.ts). */
const ROW_WRAPPER_CLASS = "feed-item";

/** What the chip's `request` evidence names this call as. */
export const CLEAR_REQUEST = "clear the feed selection";

/**
 * WHETHER TARGET IS THE FEED'S BACKGROUND inside the scroll BOX. The one hit
 * test.
 *
 * The background is the layout that holds the rows, never anything drawn in
 * them:
 * - the scroll box itself;
 * - one of its direct children (the feed host and the hold tray host), which
 *   is where a click between rows lands, in the flex gap;
 * - a ROOT row's full-width wrapper (`.feed-item` in the feed host), which is
 *   where a click beside a narrower bubble or card lands.
 *
 * Everything else is not: a bubble or a card and anything inside it (each has
 * its own click semantics, expand among them), an interactive control, and a
 * row nested in a bubble's sub-feed, which is part of its bubble.
 */
export function isFeedBackground(target: EventTarget | null, box: Element): boolean {
  if (!(target instanceof Element) || !box.contains(target)) return false;
  if (target === box) return true;
  const parent = target.parentElement;
  if (parent === box) return true;
  return target.classList.contains(ROW_WRAPPER_CLASS) && parent?.parentElement === box;
}

/**
 * Arm the click-to-clear on the scroll BOX. SELECTIONACTIVE answers whether the
 * daemon's last pushed selection names a row; a background click sends the clear
 * only then. Answers the uninstall.
 */
export function installBackgroundClear(
  box: HTMLElement,
  ctx: AppContext,
  selectionActive: () => boolean,
): () => void {
  const onClick = (event: MouseEvent): void => {
    if (!isFeedBackground(event.target, box)) return;
    if (!selectionActive()) {
      log.debug("a click on the feed background, with no selection to clear", {
        operation: "feed.background-click-idle",
        verbosity: "verbose",
      });
      return;
    }
    if (document.getSelection()?.isCollapsed === false) {
      // A drag that selected text ends in a click on the rows' common ancestor,
      // which is the background; it is a text selection, not a dismissal.
      log.debug("a text drag ended on the feed background; the selection stands", {
        operation: "feed.background-click-text-drag",
      });
      return;
    }
    log.info("a click on the feed background clears the selection", {
      operation: "feed.background-click-clear",
    });
    void guardMalformed(
      ctx,
      "feed.selection-clear",
      selectFeedRow(ctx, { case: "clear", value: {} }, CLEAR_REQUEST),
    );
  };
  box.addEventListener("click", onClick);
  return () => {
    box.removeEventListener("click", onClick);
  };
}
