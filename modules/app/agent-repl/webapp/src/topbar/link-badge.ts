/**
 * The pull request badge a summary-menu row draws in its right margin. The
 * daemon names the state from its latest read of the pull request
 * (frontend.v1.TopbarTitleLinkBadge), so this file never inspects a url: it
 * only draws the word and color the arm names.
 */
import type { TopbarTitleLinkBadge } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { unreachableArm } from "../rpc/strict.js";

/** The badge's class; `data-badge` names the state it draws. */
export const LINK_BADGE_CLASS = "topbar-title-link-badge";

/**
 * The badge for a menu row, or null for a row with none: no badge served, or
 * a badge whose state this schema revision does not name. A state arm this
 * build has no case for throws, so a newer daemon's state is a loud refusal
 * rather than a silently missing badge.
 */
export function drawLinkBadge(badge: TopbarTitleLinkBadge | undefined): HTMLElement | null {
  const state = badge?.state.case;
  if (state === undefined) return null;
  const el = document.createElement("span");
  el.className = LINK_BADGE_CLASS;
  el.dataset.badge = state;
  el.textContent = wordFor(state);
  return el;
}

function wordFor(state: NonNullable<TopbarTitleLinkBadge["state"]["case"]>): string {
  switch (state) {
    case "open":
      return "open";
    case "draft":
      return "draft";
    case "merged":
      return "merged";
    case "closed":
      return "closed";
    default:
      return unreachableArm("TopbarTitleLinkBadge.state", String(state));
  }
}
