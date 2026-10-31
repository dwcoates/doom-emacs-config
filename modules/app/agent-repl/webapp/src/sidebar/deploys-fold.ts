/**
 * recently-deployed — WHETHER THE DEPLOYS SECTION'S "RECENTLY DEPLOYED" ROWS
 * ARE SHOWN (owner request, 2026-10-27).
 *
 * THEY ARE SHOWN ONLY WHILE ASKED FOR. The heading is collapsed until it is
 * clicked, and a click on it again collapses it. It draws no chevron (owner
 * request, 2026-10-27): the heading itself is what is clicked. It collapses again by
 * itself the moment the user is done with it:
 *   - the focus leaves the rail's page (`focus-left.ts`): a click into a
 *     feed, into another Emacs window, or Emacs moving its own focus;
 *   - a workspace outside the rows is clicked: an open row, a hidden row, or
 *     a merge in the deploy queue;
 *   - a workspace is reopened from the rows (`row.ts`'s band returns).
 *
 * THE EXPANSION IS THIS PAGE'S OWN, never the daemon's view state: it is a
 * glance that lasts while the user is in the rail, so it is never shared
 * with another page or kept across a restart. It outlives every draw, so a
 * push that redraws the Deploys section draws the heading as it stands.
 *
 * EXPANDED, THE ROWS CLAIM ROOM FOR `RECENTLY_DEPLOYED_FLOOR` OF THEMSELVES
 * (`band-fit.ts`), taken from Hidden Workspaces first and then from the Open
 * section. Collapsed, the heading alone is a line, and claims that line.
 */
import { queryAll } from "../dom.js";
import type { FocusLeft } from "../focus-left.js";
import { log } from "../log.js";

/** The class of the "Recently Deployed" heading over the merged groups, below the queues. */
export const RECENTLY_DEPLOYED_LABEL_CLASS = "sb-merged-label";

/** The attribute the heading wears while its rows are not shown. */
export const RECENTLY_DEPLOYED_COLLAPSED_ATTRIBUTE = "data-collapsed";

/** How many rows an expanded "Recently Deployed" claims room for (owner request, 2026-10-27). */
export const RECENTLY_DEPLOYED_FLOOR = 5;

/** What a workspace outside "Recently Deployed" is drawn as, which a click on collapses it. */
const OTHER_WORKSPACE_SELECTOR = "[data-roster-row], .sb-mq-head, .sb-mq-remediating, .sb-mq-waiting";

/** The class of one repository's merged group, inside which a click is the rows' own. */
const MERGED_GROUP_SELECTOR = ".merged-group";

/** The page's one "Recently Deployed" expansion. */
export interface RecentlyDeployed {
  /** Whether the rows are shown. */
  expanded(): boolean;
  /** Show the rows when they are not, collapse them when they are; the heading's click. */
  toggle(): void;
  /** Collapse the rows, WHY naming what the user did; nothing when they are collapsed. */
  collapse(why: string): void;
  /** Draw HEADING, freshly drawn, as the expansion stands. */
  paint(heading: HTMLElement): void;
}

/**
 * The expansion of the rail drawn in ROOT. Each change repaints every heading
 * in ROOT and tells CHANGED, so the bands are fitted again.
 */
export function createRecentlyDeployed(root: ParentNode, changed: () => void): RecentlyDeployed {
  let expanded = false;
  const paint = (heading: HTMLElement): void => {
    heading.toggleAttribute(RECENTLY_DEPLOYED_COLLAPSED_ATTRIBUTE, !expanded);
  };
  const set = (to: boolean, why: string): void => {
    if (expanded === to) return;
    expanded = to;
    log.debug(to ? "showed the Recently Deployed rows" : "collapsed the Recently Deployed rows", {
      operation: "sidebar.recently-deployed",
      context: { expanded: to, why },
    });
    for (const heading of queryAll<HTMLElement>(root, `.${RECENTLY_DEPLOYED_LABEL_CLASS}`)) paint(heading);
    changed();
  };
  return {
    expanded: () => expanded,
    toggle: () => set(!expanded, "its heading was clicked"),
    collapse: (why) => set(false, why),
    paint,
  };
}

/**
 * Collapse RECENTLY's rows whenever FOCUS leaves the page, and whenever a
 * workspace outside them is clicked in the rail HOST. Answers the uninstall.
 */
export function installRecentlyDeployedCollapse(host: HTMLElement, recently: RecentlyDeployed, focus: FocusLeft): () => void {
  const unsubscribe = focus.onLeft((why) => recently.collapse(why));
  // CAPTURED, because a row's own click handler may stop the click.
  const clicked = (event: MouseEvent): void => {
    const target = event.target;
    if (!(target instanceof Element)) return;
    if (target.closest(OTHER_WORKSPACE_SELECTOR) === null || target.closest(MERGED_GROUP_SELECTOR) !== null) return;
    recently.collapse("a workspace outside Recently Deployed was clicked");
  };
  host.addEventListener("click", clicked, true);
  return () => {
    unsubscribe();
    host.removeEventListener("click", clicked, true);
  };
}
