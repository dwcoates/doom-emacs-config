/**
 * subagent-handback — A SUBAGENT REPORTED BACK: the small inline badge a
 * subagent's hand-back leaves in the main feed.
 *
 * ITS SPEC, AND NOTHING ELSE (owner ruling, 2026-09-27): "a little marker in the
 * feed is good, but it shouldnt be a bubble or card, just a little badge". So it
 * is the shared `.badge` pill, quiet (muted), carrying the daemon's label and
 * nothing else — no body, no toggle, no bubble chrome. The report itself belongs
 * to the subagent's own card and is never drawn here.
 */
import { log } from "../../log.js";
import { requireMessage } from "../../rpc/strict.js";
import type { FeedSubagentHandbackBadge } from "../../../../proto/gen/ts/frontend/v1/feed_pb";

/** The class the hand-back badge's hooks and its muted tone know it by. */
export const SUBAGENT_HANDBACK_BADGE_CLASS = "subagent-handback-badge";

/** The hand-back badge: the daemon's label, drawn verbatim in the shared pill. */
export function drawFeedSubagentHandback(badge: FeedSubagentHandbackBadge): HTMLElement {
  const text = requireMessage(badge.label, "FeedSubagentHandbackBadge.label").text;
  log.info("drawing a subagent hand-back badge", {
    operation: "feed.draw-subagent-handback",
    context: { label: text },
  });
  const el = document.createElement("div");
  el.className = `badge ${SUBAGENT_HANDBACK_BADGE_CLASS}`;
  // The daemon composes the label ("agent <sender> reported back"); drawn verbatim.
  el.textContent = text;
  return el;
}
