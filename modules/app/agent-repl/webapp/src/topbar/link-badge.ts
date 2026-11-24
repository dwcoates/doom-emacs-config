/**
 * The badges a summary-menu row draws. The pull request badge sits in the
 * row's right margin, its state named by the daemon from its latest read of
 * the pull request (frontend.v1.TopbarTitleLinkBadge). The row's marks sit
 * beside its label: "owned" for a link the workspace's work made, and
 * "subscribed" for a link whose updates the workspace is told
 * (frontend.v1.TopbarTitleLinkMenuItem.owned and .subscribed). This file never
 * inspects a url: it only draws what the daemon served.
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

/** A label mark's class; `data-mark` names which mark it draws. */
export const LINK_MARK_CLASS = "topbar-title-link-mark";

/** The marks a row can carry beside its label, in the order drawn. */
export type LinkMark = "owned" | "subscribed";

/**
 * The marks beside a menu row's label, in the order drawn: "owned" when the
 * workspace's work made what the link points at, then "subscribed" when the
 * workspace is told about the link's updates. Each wears the feed's shared
 * badge pill (`.badge`), so a mark reads as the same badge a tool card draws.
 */
export function drawLinkMarks(item: { owned: boolean; subscribed: boolean }): HTMLElement[] {
  const marks: LinkMark[] = [];
  if (item.owned) marks.push("owned");
  if (item.subscribed) marks.push("subscribed");
  return marks.map((mark) => {
    const el = document.createElement("span");
    el.classList.add("badge", LINK_MARK_CLASS);
    el.dataset.mark = mark;
    el.textContent = mark;
    return el;
  });
}
