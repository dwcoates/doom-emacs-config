/**
 * page-census — each workspace page's own statement of its size, every
 * CENSUS_EVERY_MS, hidden or shown.
 *
 * WHY IT EXISTS (2026-10-15). Every workspace page is a cross-origin iframe
 * of the webhost, sharing its one web process, so the webhost cannot read a
 * page's document: its census saw 15 frames and no element of any of them.
 * A page states its own size instead, on its own workspace's log, and a
 * `webhost.stall` is read against the censuses of the pages around it.
 *
 * IT COUNTS ITS ANIMATIONS TOO (2026-10-25). The shared web process held
 * 5,280 live CSS animations across 18 pages, each with its keyframes and a
 * layer of its own, paused or not; the census names how many each page
 * holds and which, so the ones that outlive their purpose can be found.
 */
import { log } from "./log.js";
import { pageSize } from "./page-size.js";
import { every, pageAfter, type After } from "./every.js";

/** How often a page states its size. */
export const CENSUS_EVERY_MS = 60_000;

/** How many animation names a census names, most numerous first. */
export const CENSUS_ANIMATION_NAMES = 5;

/** One page's live animations: how many, and the most numerous names. */
export interface AnimationCensus {
  readonly animations: number;
  readonly animation_names: Record<string, number>;
}

/**
 * DOC's live animations, counted by name: a CSS animation by its
 * `animation-name`, anything else as `script`. Only the most numerous
 * CENSUS_ANIMATION_NAMES names are named.
 */
export function animationCensus(doc: Document): AnimationCensus {
  const counts = new Map<string, number>();
  const all = doc.getAnimations();
  for (const animation of all) {
    const name = (animation as { animationName?: string }).animationName ?? "script";
    counts.set(name, (counts.get(name) ?? 0) + 1);
  }
  const named = [...counts.entries()]
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .slice(0, CENSUS_ANIMATION_NAMES);
  return { animations: all.length, animation_names: Object.fromEntries(named) };
}

/** Install the census on DOC's page; answers the stop. */
export function installPageCensus(doc: Document, after: After = pageAfter): () => void {
  return every(after, CENSUS_EVERY_MS, () => {
    const size = pageSize(doc);
    const animations = animationCensus(doc);
    log.info(
      `the page holds ${size.dom_nodes} elements, ${size.feed_rows} feed rows and ${animations.animations} animations`,
      {
        operation: "page.census",
        context: { ...size, ...animations, visibility: doc.visibilityState },
      },
    );
  });
}
