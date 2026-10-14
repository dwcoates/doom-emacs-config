/**
 * shown-edge — A PAGE IN THE WEBHOST IS SWITCHED TO WHEN THE WEBHOST SHOWS IT.
 *
 * The webhost shows exactly the page of the workspace Emacs is standing in, so
 * the moment it starts showing a page is the moment the user switched to that
 * workspace, by whatever path. A page on its own learns the same thing from
 * its sidebar watching the roster's `current` (sidebar/selection-edge.ts); a
 * page in the webhost draws no sidebar, so this is its one edge.
 */
import { log } from "../log.js";
import type { Presence } from "../presence.js";
import { nextFrame } from "../frame.js";

/** The clock and frame scheduler the shown frame is timed with. */
export interface ShownEdgeTiming {
  /** Monotonic milliseconds. */
  readonly now: () => number;
  /** Run FN on the next animation frame. */
  readonly frame: (fn: () => void) => void;
}

/** The page's own clock and frames. */
const PAGE_TIMING: ShownEdgeTiming = {
  now: () => performance.now(),
  frame: nextFrame,
};

/**
 * Call SWITCHED each time PRESENCE goes from not shown to shown; answers the stop.
 *
 * WHAT A SHOW COSTS IN THE PAGE IS RECORDED (`page.shown-frame`): the time
 * from the edge to the second animation frame after it, which runs once the
 * first frame showing the page has painted. Everything the show set off in
 * the page (the feed returning to its tail, its animations resuming, the
 * style the page owes) lands in it; the editor's side of a switch is recorded
 * by Emacs.
 */
export function watchShownEdge(presence: Presence, switched: () => void, timing: ShownEdgeTiming = PAGE_TIMING): () => void {
  let shown = presence.shown();
  return presence.onChange(() => {
    const now = presence.shown();
    const edge = now && !shown;
    shown = now;
    if (!edge) return;
    const began = timing.now();
    log.info("this workspace was switched to; the feed returns to its tail", {
      operation: "page.switched-to",
      context: {},
    });
    switched();
    timing.frame(() => {
      timing.frame(() => {
        log.info("the switched-to page painted its first frame", {
          operation: "page.shown-frame",
          context: { ms: Math.round((timing.now() - began) * 10) / 10 },
        });
      });
    });
  });
}
