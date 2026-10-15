/**
 * load-census — the webhost's record of the load its workspace pages put on
 * the ONE web process they share, and of the main-thread stalls that load
 * causes.
 *
 * WHY IT EXISTS (2026-10-14). Every open workspace's page is an iframe of
 * this one document, so every page shares one main thread and one heap: a
 * page that grew to 50k elements slows every switch, not only its own. The
 * pages' own `page.repaint` records showed small pages painting 2.7s late,
 * all at one instant, and nothing recorded what the process as a whole was
 * carrying or when its main thread stopped answering.
 *
 * - THE CENSUS: every CENSUS_EVERY_MS, how many pages the webhost holds, their
 *   elements and feed rows in total, and the largest pages by name.
 * - THE STALL PROBE: a timer that should fire every STALL_PROBE_MS; one firing
 *   STALL_THRESHOLD_MS or more late means the main thread was blocked that
 *   long, recorded with the census at that instant. A probe across a time the
 *   webhost was hidden proves nothing (WebKit throttles a hidden page's
 *   timers), so it is never reported.
 */
import { log } from "../log.js";
import { pageSize, type PageSize } from "../page-size.js";
import { FRAME_SHOWN_ATTRIBUTE, FRAME_WORKSPACE_ATTRIBUTE } from "./host.js";

/** How often the stall probe expects to fire. */
export const STALL_PROBE_MS = 250;
/** How late a probe must fire to be a stall: past every frame budget a user feels. */
export const STALL_THRESHOLD_MS = 100;
/** How often the census is recorded. */
export const CENSUS_EVERY_MS = 60_000;
/** How many of the largest pages a record names. */
export const LARGEST_NAMED = 3;

/** One workspace page's size, by the workspace its iframe holds. */
export interface FrameSize extends PageSize {
  readonly workspace: string;
  readonly shown: boolean;
}

/** The pages the webhost holds, as one record states them. */
export interface CensusSummary {
  readonly pages: number;
  readonly pages_unloaded: number;
  readonly total_dom_nodes: number;
  readonly total_feed_rows: number;
  readonly shown_workspace: string;
  readonly largest: readonly FrameSize[];
}

/** Size every workspace page under ROOT; a page whose document is not loaded yet is counted apart. */
export function frameCensus(root: ParentNode): { sizes: FrameSize[]; unloaded: number } {
  const sizes: FrameSize[] = [];
  let unloaded = 0;
  for (const iframe of root.querySelectorAll<HTMLIFrameElement>(`iframe[${FRAME_WORKSPACE_ATTRIBUTE}]`)) {
    const doc = iframe.contentDocument;
    if (doc === null) {
      unloaded++;
      continue;
    }
    sizes.push({
      workspace: iframe.getAttribute(FRAME_WORKSPACE_ATTRIBUTE) ?? "",
      shown: iframe.hasAttribute(FRAME_SHOWN_ATTRIBUTE),
      ...pageSize(doc),
    });
  }
  return { sizes, unloaded };
}

/** The census of ROOT as one record's context. */
export function summarize(root: ParentNode): CensusSummary {
  const { sizes, unloaded } = frameCensus(root);
  const largest = [...sizes].sort((a, b) => b.dom_nodes - a.dom_nodes).slice(0, LARGEST_NAMED);
  return {
    pages: sizes.length,
    pages_unloaded: unloaded,
    total_dom_nodes: sizes.reduce((sum, size) => sum + size.dom_nodes, 0),
    total_feed_rows: sizes.reduce((sum, size) => sum + size.feed_rows, 0),
    shown_workspace: sizes.find((size) => size.shown)?.workspace ?? "",
    largest,
  };
}

/** What the census needs from its environment, injectable for tests. */
export interface LoadCensusEnv {
  readonly doc: Document;
  /** Monotonic milliseconds. */
  readonly now: () => number;
  /** Run FN once after MS; answers the cancel. */
  readonly after: (fn: () => void, ms: number) => () => void;
}

/** The installed census; `stop` cancels both loops. */
export interface LoadCensus {
  stop(): void;
}

/** The default environment: the webhost's own document and timers. */
function loadCensusEnv(doc: Document): LoadCensusEnv {
  return {
    doc,
    now: () => performance.now(),
    after: (fn, ms) => {
      const handle = setTimeout(fn, ms);
      return () => clearTimeout(handle);
    },
  };
}

/** Install the census and the stall probe on the webhost's DOC. */
export function installLoadCensus(doc: Document, env: LoadCensusEnv = loadCensusEnv(doc)): LoadCensus {
  const { now, after } = env;
  // A HIDDEN STRETCH VOIDS THE PROBE that spans it: the timer was throttled,
  // not blocked.
  let hiddenSinceProbe = env.doc.visibilityState !== "visible";
  const onVisibility = (): void => {
    if (env.doc.visibilityState !== "visible") hiddenSinceProbe = true;
  };
  env.doc.addEventListener("visibilitychange", onVisibility);

  let cancelProbe = (): void => undefined;
  const probe = (): void => {
    const expected = now() + STALL_PROBE_MS;
    cancelProbe = after(() => {
      const late = Math.round(now() - expected);
      const voided = hiddenSinceProbe || env.doc.visibilityState !== "visible";
      hiddenSinceProbe = false;
      if (!voided && late >= STALL_THRESHOLD_MS) {
        log.info(`the web process's main thread was blocked ${late}ms`, {
          operation: "webhost.stall",
          context: { blocked_ms: late, ...summarize(env.doc) },
        });
      }
      probe();
    }, STALL_PROBE_MS);
  };

  let cancelCensus = (): void => undefined;
  const census = (): void => {
    cancelCensus = after(() => {
      const summary = summarize(env.doc);
      log.info(`the webhost holds ${summary.pages} pages with ${summary.total_dom_nodes} elements`, {
        operation: "webhost.census",
        context: { ...summary },
      });
      census();
    }, CENSUS_EVERY_MS);
  };

  probe();
  census();
  return {
    stop(): void {
      cancelProbe();
      cancelCensus();
      env.doc.removeEventListener("visibilitychange", onVisibility);
    },
  };
}
