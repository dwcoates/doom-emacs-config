/**
 * page-census — each workspace page's own statement of its size, every
 * CENSUS_EVERY_MS, hidden or shown.
 *
 * WHY IT EXISTS (2026-10-15). Every workspace page is a cross-origin iframe
 * of the webhost, sharing its one web process, so the webhost cannot read a
 * page's document: its census saw 15 frames and no element of any of them.
 * A page states its own size instead, on its own workspace's log, and a
 * `webhost.stall` is read against the censuses of the pages around it.
 */
import { log } from "./log.js";
import { pageSize } from "./page-size.js";
import { every, pageAfter, type After } from "./every.js";
import type { BusyProbe } from "./busy-probe.js";

/** How often a page states its size. */
export const CENSUS_EVERY_MS = 60_000;

/**
 * Install the census on DOC's page; answers the stop. BUSY, when the page
 * runs a busy probe (busy-probe.ts), adds the interval's callback time.
 */
export function installPageCensus(doc: Document, after: After = pageAfter, busy?: BusyProbe): () => void {
  return every(after, CENSUS_EVERY_MS, () => {
    const size = pageSize(doc);
    log.info(`the page holds ${size.dom_nodes} elements and ${size.feed_rows} feed rows`, {
      operation: "page.census",
      context: { ...size, visibility: doc.visibilityState, ...(busy === undefined ? {} : { busy: busy.take() }) },
    });
  });
}
