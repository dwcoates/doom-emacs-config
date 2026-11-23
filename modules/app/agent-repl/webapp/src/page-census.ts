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
 * They are counted off computed style, every element's and its
 * pseudo-elements', never by asking WebKit for them: an animation script has
 * asked for is never freed (2026-11-18; the webapp's AGENTS.md, Performance
 * lessons), so a census that asked would leak what it counted. A script's own
 * `animate()` runs on no style and is not counted.
 */
import { createLogger } from "./log.js";
import { pageSize } from "./page-size.js";
import { every, pageAfter, type After } from "./every.js";
import { animatedParts } from "./animation-delays.js";
import { blankView, feedGeometry } from "./feed-geometry.js";

const log = createLogger("webapp.page-census");

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
 * DOC's live CSS animations, counted by `animation-name` off VIEW's computed
 * style. Only the most numerous CENSUS_ANIMATION_NAMES names are named.
 */
export function animationCensus(doc: Document, view: Window & typeof globalThis = requireView(doc)): AnimationCensus {
  const counts = new Map<string, number>();
  let total = 0;
  const walker = doc.createTreeWalker(doc.documentElement, NodeFilter.SHOW_ELEMENT);
  for (let node: Node | null = walker.currentNode; node !== null; node = walker.nextNode()) {
    for (const { names } of animatedParts(node as Element, view)) {
      for (const name of names) {
        if (name === "none") continue;
        counts.set(name, (counts.get(name) ?? 0) + 1);
        total += 1;
      }
    }
  }
  const named = [...counts.entries()]
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .slice(0, CENSUS_ANIMATION_NAMES);
  return { animations: total, animation_names: Object.fromEntries(named) };
}

/** DOC's window, which every page's document has. */
function requireView(doc: Document): Window & typeof globalThis {
  const view = doc.defaultView;
  if (view === null) {
    log.error("a census was asked of a document with no window", {
      operation: "page.census-windowless",
      context: { document: doc.URL },
    });
    throw new Error(`page-census: ${doc.URL} has no window`);
  }
  return view;
}

/** Install the census on DOC's page; answers the stop. */
export function installPageCensus(doc: Document, after: After = pageAfter): () => void {
  return every(after, CENSUS_EVERY_MS, () => {
    const { dom_nodes, feed_rows, ...size } = pageSize(doc);
    const { animations, ...named } = animationCensus(doc);
    const geometry = feedGeometry(doc);
    log.info(
      "the page holds {dom_nodes} elements, {feed_rows} feed rows and {animations} animations",
      {
        operation: "page.census",
        context: { ...size, ...named, visibility: doc.visibilityState, feed_geometry: geometry },
      }, { dom_nodes, feed_rows, animations },
    );
    if (geometry !== null && blankView(geometry)) {
      log.warn("the feed holds {root_rows} rows and none of them is in its view", {
        operation: "feed.blank-view",
        context: { feed_geometry: geometry, visibility: doc.visibilityState },
      }, { root_rows: geometry.root_rows });
    }
  });
}
