/**
 * deploys-more — HOW MANY OF EACH REPOSITORY'S RECENT DEPLOYS ITS DEPLOYS
 * GROUP SHOWS (owner request, 2026-10-31; frontend.v1.RosterMergedRepoGroup).
 *
 * A GROUP SHOWS ITS TEN MOST RECENT DEPLOYS. Every older one is drawn in a
 * page of ten (`DEPLOYS_PAGE_CLASS`) that stays hidden until the small arrow
 * under the group (`DEPLOYS_MORE_CLASS`) is clicked, each click showing one
 * more page. The arrow is drawn only while a page is still hidden.
 *
 * THE PAGES SHOWN GO AGAIN once none of them has been in view for
 * `DEPLOYS_MORE_LINGER_MS`: scrolled out of the Deploys viewport, or folded
 * away with their group. One coming back into view before then keeps them.
 * Letting them go keeps whatever the viewport showed where it stood, so a
 * page above the viewport's top shifts nothing in view.
 *
 * THE COUNT IS THIS PAGE'S OWN, never the daemon's view state, and it
 * outlives every draw, so a push that redraws the Deploys section draws each
 * group with the pages it had.
 */
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { scrollKept } from "../scroll.js";
import type { Timers } from "../timers.js";
import { DEPLOYS_GROUP_CLASS, deploysGroupRepository } from "./deploys-fold.js";

/** How many deploys a group shows before its arrow is clicked, and how many each click adds. */
export const DEPLOYS_PAGE_SIZE = 10;

/** How long the pages shown stay once none of them is in view (owner request, 2026-10-31). */
export const DEPLOYS_MORE_LINGER_MS = 15_000;

/** The class of one page of older deploys inside a group's body. */
export const DEPLOYS_PAGE_CLASS = "merged-page";

/** The class of the arrow under a group that shows its next page. */
export const DEPLOYS_MORE_CLASS = "sb-deploys-more";

/** What the arrow draws: a small downward arrow. */
export const DEPLOYS_MORE_GLYPH = "▾";

/** The Deploys section's rows region, the viewport a page is in view of or not. */
const DEPLOYS_REGION_SELECTOR = ".merged-section > .rows";

/** The page's one count of the Deploys pages each group shows. */
export interface DeploysMore {
  /** Show GROUP's next page; the arrow's click. */
  more(group: HTMLElement): void;
  /** Draw GROUP, freshly drawn, with the pages it shows. */
  paint(group: HTMLElement): void;
  /** Look at which groups' pages are in view, starting or stopping each one's linger. */
  check(): void;
  /** Stop every linger; the rail is going away. */
  dispose(): void;
}

/** One repository's pages shown, and the linger running while none is in view. */
interface Shown {
  pages: number;
  linger: number | null;
}

/** The repository GROUP's pages are counted under. */
const repositoryOf = (group: HTMLElement): string => deploysGroupRepository(group, "sidebar.deploys-more.malformed");

/** GROUP's pages of older deploys, in order. */
function pagesOf(group: HTMLElement): HTMLElement[] {
  return queryAll<HTMLElement>(group, `.${DEPLOYS_PAGE_CLASS}`);
}

/** The Deploys viewport GROUP is drawn in. A group always hangs in one. */
function regionOf(group: HTMLElement): HTMLElement {
  const region = group.closest<HTMLElement>(DEPLOYS_REGION_SELECTOR);
  if (region === null) {
    log.error("a Deploys group's older deploys cannot be looked at: it hangs in no Deploys viewport", {
      operation: "sidebar.deploys-more.malformed",
      context: { repository: repositoryOf(group) },
    });
    throw new Error("deploys more: a Deploys group hangs in no Deploys viewport");
  }
  return region;
}

/** Whether any of PAGES shows a sliver inside REGION's viewport. */
export function pagesInView(pages: readonly HTMLElement[], region: HTMLElement): boolean {
  const view = region.getBoundingClientRect();
  return pages.some((page) => {
    const box = page.getBoundingClientRect();
    return box.height > 0 && box.bottom > view.top && box.top < view.bottom;
  });
}

/**
 * The pages of the groups drawn in ROOT, their lingers timed on TIMERS. Each
 * change repaints the group it is of and tells CHANGED, so the bands are
 * fitted again.
 */
export function createDeploysMore(root: ParentNode, timers: Timers, changed: () => void): DeploysMore {
  const shown = new Map<string, Shown>();
  const groupOf = (repository: string): HTMLElement | null =>
    queryAll<HTMLElement>(root, `.${DEPLOYS_GROUP_CLASS}`).find((group) => repositoryOf(group) === repository) ?? null;
  const paint = (group: HTMLElement): void => {
    const pages = shown.get(repositoryOf(group))?.pages ?? 0;
    const all = pagesOf(group);
    all.forEach((page, i) => {
      page.hidden = i >= pages;
    });
    const arrow = group.querySelector<HTMLElement>(`.${DEPLOYS_MORE_CLASS}`);
    if (arrow !== null) arrow.hidden = pages >= all.length;
  };
  const letGo = (repository: string): void => {
    shown.delete(repository);
    const group = groupOf(repository);
    log.debug("let a Deploys group's older deploys go", {
      operation: "sidebar.deploys-more",
      context: { repository, why: `none was in view for ${DEPLOYS_MORE_LINGER_MS}ms`, drawn: group !== null },
    });
    if (group === null) return;
    const region = regionOf(group);
    const top = region.getBoundingClientRect().top;
    const above = pagesOf(group)
      .filter((page) => !page.hidden && page.getBoundingClientRect().bottom <= top)
      .reduce((sum, page) => sum + page.getBoundingClientRect().height, 0);
    paint(group);
    scrollKept(region, region.scrollTop - above);
    changed();
  };
  const check = (): void => {
    for (const [repository, state] of shown) {
      const group = groupOf(repository);
      const inView = group !== null && pagesInView(pagesOf(group).filter((page) => !page.hidden), regionOf(group));
      if (inView && state.linger !== null) {
        timers.clearTimeout(state.linger);
        state.linger = null;
      } else if (!inView && state.linger === null) {
        state.linger = timers.setTimeout(() => letGo(repository), DEPLOYS_MORE_LINGER_MS);
      }
    }
  };
  return {
    more: (group) => {
      const repository = repositoryOf(group);
      const state = shown.get(repository) ?? { pages: 0, linger: null };
      state.pages += 1;
      shown.set(repository, state);
      log.debug("showed a Deploys group's next older deploys", {
        operation: "sidebar.deploys-more",
        context: { repository, pages: state.pages, why: "its arrow was clicked" },
      });
      paint(group);
      changed();
      check();
    },
    paint,
    check,
    dispose: () => {
      for (const state of shown.values()) if (state.linger !== null) timers.clearTimeout(state.linger);
      shown.clear();
    },
  };
}

/** The arrow under GROUP that shows its next page through MORE. */
export function drawDeploysMoreArrow(group: HTMLElement, more: DeploysMore): HTMLElement {
  const arrow = document.createElement("div");
  arrow.className = DEPLOYS_MORE_CLASS;
  arrow.textContent = DEPLOYS_MORE_GLYPH;
  arrow.title = `Show ${DEPLOYS_PAGE_SIZE} more deploys`;
  arrow.addEventListener("click", () => more.more(group));
  return arrow;
}
