/**
 * load-census — the webhost's record of the pages it holds in the ONE web
 * process they share, and of the main-thread stalls in that process.
 *
 * WHY IT EXISTS (2026-10-14). Every open workspace's page is an iframe of
 * this one document, so every page shares one main thread: a heavy page, or
 * the webhost's own rail, slows every switch. The pages' own `page.repaint`
 * records showed small pages painting 2.7s late, all at one instant, and
 * nothing recorded what the process carried or when its main thread stopped
 * answering.
 *
 * The pages are CROSS-ORIGIN to the webhost, so it cannot read their
 * documents; each page states its own size (`page.census`). What the webhost
 * states is what it can read: how many pages it holds, which one is shown,
 * and the size of its own document, which holds the rail.
 *
 * - THE CENSUS: every CENSUS_EVERY_MS, that state.
 * - THE STALL PROBE: a timer that should fire every STALL_PROBE_MS; one firing
 *   STALL_THRESHOLD_MS or more late means the main thread was blocked that
 *   long, recorded with the census at that instant. A probe across a time the
 *   webhost was hidden proves nothing (WebKit throttles a hidden page's
 *   timers), so it is never reported.
 */
import { log } from "../log.js";
import { pageSize, type PageSize } from "../page-size.js";
import { CENSUS_EVERY_MS } from "../page-census.js";
import { every, pageAfter, type After } from "../every.js";
import { FRAME_SHOWN_ATTRIBUTE, FRAME_WORKSPACE_ATTRIBUTE } from "./host.js";

export { CENSUS_EVERY_MS };

/** How often the stall probe expects to fire. */
export const STALL_PROBE_MS = 250;
/** How late a probe must fire to be a stall: past every frame budget a user feels. */
export const STALL_THRESHOLD_MS = 100;

/** What the webhost can state about the pages it holds, as one record's context. */
export interface CensusSummary {
  readonly pages: number;
  readonly shown_workspace: string;
  readonly host: PageSize;
}

/** The census of the webhost's DOC. */
export function summarize(doc: Document): CensusSummary {
  const frames = doc.querySelectorAll<HTMLIFrameElement>(`iframe[${FRAME_WORKSPACE_ATTRIBUTE}]`);
  const shown = doc.querySelector<HTMLIFrameElement>(`iframe[${FRAME_WORKSPACE_ATTRIBUTE}][${FRAME_SHOWN_ATTRIBUTE}]`);
  return {
    pages: frames.length,
    shown_workspace: shown?.getAttribute(FRAME_WORKSPACE_ATTRIBUTE) ?? "",
    host: pageSize(doc),
  };
}

/** What the census needs from its environment, injectable for tests. */
export interface LoadCensusEnv {
  readonly doc: Document;
  /** Monotonic milliseconds. */
  readonly now: () => number;
  readonly after: After;
}

/** The installed census; `stop` cancels both loops. */
export interface LoadCensus {
  stop(): void;
}

/** Install the census and the stall probe on the webhost's DOC. */
export function installLoadCensus(
  doc: Document,
  env: LoadCensusEnv = { doc, now: () => performance.now(), after: pageAfter },
): LoadCensus {
  const { now, after } = env;
  // A HIDDEN STRETCH VOIDS THE PROBE that spans it: the timer was throttled,
  // not blocked.
  let hiddenSinceProbe = env.doc.visibilityState !== "visible";
  const onVisibility = (): void => {
    if (env.doc.visibilityState !== "visible") hiddenSinceProbe = true;
  };
  env.doc.addEventListener("visibilitychange", onVisibility);

  let expected = now() + STALL_PROBE_MS;
  const stopProbe = every(after, STALL_PROBE_MS, () => {
    const late = Math.round(now() - expected);
    expected = now() + STALL_PROBE_MS;
    const voided = hiddenSinceProbe || env.doc.visibilityState !== "visible";
    hiddenSinceProbe = false;
    if (voided || late < STALL_THRESHOLD_MS) return;
    log.info(`the web process's main thread was blocked ${late}ms`, {
      operation: "webhost.stall",
      context: { blocked_ms: late, ...summarize(env.doc) },
    });
  });

  const stopCensus = every(after, CENSUS_EVERY_MS, () => {
    const summary = summarize(env.doc);
    log.info(`the webhost holds ${summary.pages} pages; its own document holds ${summary.host.dom_nodes} elements`, {
      operation: "webhost.census",
      context: { ...summary },
    });
  });

  return {
    stop(): void {
      stopProbe();
      stopCensus();
      env.doc.removeEventListener("visibilitychange", onVisibility);
    },
  };
}
