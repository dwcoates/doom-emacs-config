/**
 * work-id — THE DETACHED-WORK ID an async bubble's head draws (owner ruling,
 * 2026-09-23): every async bubble names its work, so any detached work can be
 * referred to stably. The subagent head and the shell head both draw it
 * through this one function, so the two cannot drift apart.
 *
 * THE DAEMON DECIDES WHETHER THERE IS ONE. `work_id` is set iff the bubble is
 * detached work; an unset field draws nothing (a synchronous spawn is the
 * turn's own progress and has no work id). The text is drawn VERBATIM — the
 * same string the daemon keys the work by and the footer's row names — and is
 * never parsed or shortened here.
 */
import type { FeedDetachedWorkId } from "../../../proto/gen/ts/frontend/v1/feed_pb";

/** The class and hook every work-id element wears. */
export const WORK_ID_CLASS = "async-work-id";

/** The attribute carrying the id, for the suite and for a reader's selection. */
export const WORK_ID_ATTRIBUTE = "data-work-id";

/** The head's work-id element, or null when the daemon named no work. */
export function drawDetachedWorkId(u: FeedDetachedWorkId | undefined): HTMLElement | null {
  if (u === undefined) return null;
  const el = document.createElement("span");
  el.className = WORK_ID_CLASS;
  el.setAttribute(WORK_ID_ATTRIBUTE, u.text);
  el.textContent = u.text;
  return el;
}
