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

/** Call SWITCHED each time PRESENCE goes from not shown to shown; answers the stop. */
export function watchShownEdge(presence: Presence, switched: () => void): () => void {
  let shown = presence.shown();
  return presence.onChange(() => {
    const now = presence.shown();
    const edge = now && !shown;
    shown = now;
    if (!edge) return;
    log.info("this workspace was switched to; the feed returns to its tail", {
      operation: "page.switched-to",
      context: {},
    });
    switched();
  });
}
