/**
 * The service icon a summary-menu row draws on its label's left. The daemon
 * names the service from the url's host (frontend.v1.TopbarTitleLinkIcon), so
 * this file never inspects a url: it only draws the mark the enum arm names.
 * Every mark is a monochrome glyph in the row's text color.
 */
import { TopbarTitleLinkIcon } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { SVG_NS } from "../dom.js";
import { unreachableArm } from "../rpc/strict.js";

/** One path of a mark on its 24 by 24 grid. */
type Mark = readonly string[];

const GITHUB: Mark = [
  "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12",
];
/** Slack's pinwheel: four rounded bars, each with its dot. */
const SLACK: Mark = [
  "M5.04 15.16a2.52 2.52 0 1 1-2.52-2.52h2.52zM6.31 15.16a2.52 2.52 0 0 1 5.04 0v6.31a2.52 2.52 0 1 1-5.04 0z",
  "M8.83 5.04a2.52 2.52 0 1 1 2.52-2.52v2.52zM8.83 6.31a2.52 2.52 0 0 1 0 5.04H2.52a2.52 2.52 0 1 1 0-5.04z",
  "M18.96 8.83a2.52 2.52 0 1 1 2.52 2.52h-2.52zM17.69 8.83a2.52 2.52 0 0 1-5.04 0V2.52a2.52 2.52 0 1 1 5.04 0z",
  "M15.17 18.96a2.52 2.52 0 1 1-2.52 2.52v-2.52zM15.17 17.69a2.52 2.52 0 0 1 0-5.04h6.31a2.52 2.52 0 1 1 0 5.04z",
];
/** Notion's tile with its N cut out of it. */
const NOTION: Mark = [
  "M4 2.5h13.5L21 6v14.5a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1v-17a1 1 0 0 1 1-1zm4 5v9h1.6v-5.9l5.2 6.3 1.2-.2V7.5h-1.6v5.8L9.4 7.3z",
];
/** GNS: a ring with its gap on the right, the mark of a "G". */
const GNS: Mark = ["M12 2a10 10 0 1 0 10 10h-9v2.5h6.2A7.5 7.5 0 1 1 12 4.5a7.4 7.4 0 0 1 5.2 2.1l1.8-1.8A9.9 9.9 0 0 0 12 2z"];

/** The icon's class, and the data attribute naming which service it draws. */
export const LINK_ICON_CLASS = "topbar-title-link-icon";

/**
 * The mark for a menu row, or null for the row whose host has no icon. An
 * arm this build does not know throws, so a newer daemon's icon is a loud
 * refusal rather than a silently missing mark.
 */
export function drawLinkIcon(icon: TopbarTitleLinkIcon): SVGSVGElement | null {
  const marks = markFor(icon);
  if (marks === null) return null;
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("viewBox", "0 0 24 24");
  svg.setAttribute("aria-hidden", "true");
  svg.setAttribute("class", LINK_ICON_CLASS);
  svg.setAttribute("data-link-icon", TopbarTitleLinkIcon[icon]);
  for (const d of marks) {
    const path = document.createElementNS(SVG_NS, "path");
    path.setAttribute("d", d);
    path.setAttribute("fill", "currentColor");
    svg.append(path);
  }
  return svg;
}

function markFor(icon: TopbarTitleLinkIcon): Mark | null {
  switch (icon) {
    case TopbarTitleLinkIcon.UNSPECIFIED:
      return null;
    case TopbarTitleLinkIcon.GITHUB:
      return GITHUB;
    case TopbarTitleLinkIcon.SLACK:
      return SLACK;
    case TopbarTitleLinkIcon.GNS:
      return GNS;
    case TopbarTitleLinkIcon.NOTION:
      return NOTION;
    default:
      return unreachableArm("TopbarTitleLinkIcon", String(icon));
  }
}
