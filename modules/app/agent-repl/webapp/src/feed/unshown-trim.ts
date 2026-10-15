/**
 * unshown-trim — a page the host stops showing frees its older rows.
 *
 * WHY IT EXISTS (owner ruling, 2026-10-15). Every workspace page lives in the
 * one WebKit process, and a page the reader switched away from kept every row
 * it ever drew. Two properties hold instead:
 *
 *   - A page the host stops showing frees every row older than its newest
 *     daemon page (`FeedController.freeOlderThanNewestPage`).
 *   - Its stream stays open, so every update made while it is hidden is drawn
 *     as it arrives and is there when the reader switches back.
 *
 * A switch back lands at the tail (the switch ruling), so the freed history
 * is never on screen when the reader returns; "load previous page" brings it
 * back. Each switch away trims again, so visiting a workspace is what bounds it.
 *
 * THE SWITCH NEVER WAITS ON IT. The host has already shown another page when
 * this page hears it was hidden; the trim runs here, on the hidden page, on
 * its own time.
 */
import type { Presence } from "../presence.js";
import { guardMalformed, type GuardContext } from "../rpc/guard.js";
import { log } from "../log.js";

/** What the trim needs: when the host shows the page, and how to trim it. */
export interface UnshownTrimEnv {
  /** Whether the host shows the page, and its changes. */
  presence: Pick<Presence, "shown" | "onChange">;
  /** Where a malformed answer is filed (`guardMalformed`). */
  ctx: GuardContext;
  /** Free every row older than the newest daemon page. */
  trim(): Promise<void>;
}

/** Trim the page each time the host stops showing it; answers the uninstall. */
export function installUnshownTrim(env: UnshownTrimEnv): () => void {
  let shown = env.presence.shown();
  return env.presence.onChange((_seen, cause) => {
    if (cause !== "host") return;
    const now = env.presence.shown();
    const hidden = shown && !now;
    shown = now;
    if (!hidden) return;
    log.debug("the host stopped showing this page; freeing rows older than its newest daemon page", {
      operation: "feed.unshown-trim-started",
      context: {},
    });
    void guardMalformed(env.ctx, "feed.unshown-trim", env.trim());
  });
}
