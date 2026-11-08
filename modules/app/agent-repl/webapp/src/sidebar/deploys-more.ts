/**
 * deploys-more — HOW MANY OF EACH REPOSITORY'S RECENT MERGES ITS RECENTLY
 * MERGED GROUP SHOWS (owner request, 2026-10-31, reworked 2026-11-08;
 * frontend.v1.RosterMergedRepoGroup).
 *
 * A GROUP SHOWS ITS TEN MOST RECENT MERGES, however long ago they landed.
 * Every older one is drawn in a page of ten (`DEPLOYS_PAGE_CLASS`) that stays
 * hidden until the reader scrolls down to the end of the group, where the
 * feed's loading spinner (`feed/walk-control.ts`) stands while a page is
 * still hidden: each time it comes into view, one more page is shown.
 *
 * SCROLLING BACK UP LETS THE PAGES GO: a page lying wholly below the
 * Recently Merged viewport is hidden again, the last one first, and so is
 * every page of a group folded away. Pages below the viewport shift nothing
 * in view when they go.
 *
 * THE COUNT IS THIS PAGE'S OWN, never the daemon's view state, and it
 * outlives every draw, so a push that redraws the Recently Merged section draws each
 * group with the pages it had.
 */
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { drawWalkControl, setWalkLoading } from "../feed/walk-control.js";
import {
  DEPLOYS_GROUP_FOLDED_CLASS,
  RECENTLY_MERGED_GROUP_SELECTOR,
  deploysGroupRepository,
} from "./deploys-fold.js";

/** How many deploys a group shows before its arrow is clicked, and how many each click adds. */
export const DEPLOYS_PAGE_SIZE = 10;

/** The class of one page of older deploys inside a group's body. */
export const DEPLOYS_PAGE_CLASS = "merged-page";

/** The class of the loading spinner under a group that shows its next page. */
export const DEPLOYS_MORE_CLASS = "sb-deploys-more";

/** The attribute marking that spinner as this walk control's face. */
export const DEPLOYS_MORE_ATTRIBUTE = "data-deploys-more";

/** The Recently Merged section's rows region, the viewport a page is in view of or not. */
const DEPLOYS_REGION_SELECTOR = ".merged-section > .rows";

/** The page's one count of the Recently Merged pages each group shows. */
export interface DeploysMore {
  /** Show GROUP's next page. */
  more(group: HTMLElement): void;
  /** Hide GROUP's last page shown. */
  less(group: HTMLElement): void;
  /** Draw GROUP, freshly drawn, with the pages it shows. */
  paint(group: HTMLElement): void;
  /** Look at where each group's end stands in its viewport, showing or hiding pages to match. */
  check(): void;
  /** The rail is going away. */
  dispose(): void;
}

/** The repository GROUP's pages are counted under. */
const repositoryOf = (group: HTMLElement): string => deploysGroupRepository(group, "sidebar.deploys-more.malformed");

/** GROUP's pages of older deploys, in order. */
function pagesOf(group: HTMLElement): HTMLElement[] {
  return queryAll<HTMLElement>(group, `.${DEPLOYS_PAGE_CLASS}`);
}

/** The Recently Merged viewport GROUP is drawn in. A group always hangs in one. */
function regionOf(group: HTMLElement): HTMLElement {
  const region = group.closest<HTMLElement>(DEPLOYS_REGION_SELECTOR);
  if (region === null) {
    log.error("a Recently Merged group's older merges cannot be looked at: it hangs in no Recently Merged viewport", {
      operation: "sidebar.deploys-more.malformed",
      context: { repository: repositoryOf(group) },
    });
    throw new Error("deploys more: a Recently Merged group hangs in no Recently Merged viewport");
  }
  return region;
}

/** Whether EL shows a sliver inside REGION's viewport. */
export function inView(el: HTMLElement, region: HTMLElement): boolean {
  const view = region.getBoundingClientRect();
  const box = el.getBoundingClientRect();
  return box.height > 0 && box.bottom > view.top && box.top < view.bottom;
}

/** Whether EL lies wholly below REGION's viewport. */
function belowView(el: HTMLElement, region: HTMLElement): boolean {
  const box = el.getBoundingClientRect();
  return box.height > 0 && box.top >= region.getBoundingClientRect().bottom;
}

/**
 * The pages of the groups drawn in ROOT. Each change repaints the group it is
 * of and tells CHANGED, so the bands are fitted again.
 */
export function createDeploysMore(root: ParentNode, changed: () => void): DeploysMore {
  const shown = new Map<string, number>();
  const groups = (): HTMLElement[] => queryAll<HTMLElement>(root, RECENTLY_MERGED_GROUP_SELECTOR);
  const shownOf = (group: HTMLElement): number => shown.get(repositoryOf(group)) ?? 0;
  const paint = (group: HTMLElement): void => {
    const pages = shownOf(group);
    const all = pagesOf(group);
    all.forEach((page, i) => {
      page.hidden = i >= pages;
    });
    const spinner = group.querySelector<HTMLElement>(`.${DEPLOYS_MORE_CLASS}`);
    if (spinner !== null) spinner.hidden = pages >= all.length;
  };
  const move = (group: HTMLElement, by: 1 | -1, why: string): void => {
    const repository = repositoryOf(group);
    const pages = shownOf(group) + by;
    if (pages === 0) shown.delete(repository);
    else shown.set(repository, pages);
    log.debug(by > 0 ? "showed a Recently Merged group's next older merges" : "let a Recently Merged group's last older merges go", {
      operation: "sidebar.deploys-more",
      context: { repository, pages, why },
    });
    paint(group);
    changed();
  };
  const check = (): void => {
    for (const group of groups()) {
      const total = pagesOf(group).length;
      if (group.classList.contains(DEPLOYS_GROUP_FOLDED_CLASS)) {
        while (shownOf(group) > 0) move(group, -1, "its group was folded");
        continue;
      }
      const region = regionOf(group);
      const spinner = group.querySelector<HTMLElement>(`.${DEPLOYS_MORE_CLASS}`);
      if (spinner !== null && !spinner.hidden && inView(spinner, region)) {
        while (shownOf(group) < total && spinner !== null && inView(spinner, region)) {
          move(group, 1, "its end was scrolled into view");
        }
        continue;
      }
      for (;;) {
        const last = pagesOf(group)[shownOf(group) - 1];
        if (last === undefined || !belowView(last, region)) break;
        move(group, -1, "it was scrolled back above");
      }
    }
  };
  return {
    more: (group) => move(group, 1, "its next page was asked for"),
    less: (group) => move(group, -1, "its last page was let go"),
    paint,
    check,
    dispose: () => shown.clear(),
  };
}

/** The feed's loading spinner under GROUP, standing while a page of older merges is hidden. */
export function drawDeploysMoreSpinner(): HTMLElement {
  const spinner = drawWalkControl(DEPLOYS_MORE_ATTRIBUTE);
  spinner.classList.add(DEPLOYS_MORE_CLASS);
  setWalkLoading(spinner, true);
  return spinner;
}
