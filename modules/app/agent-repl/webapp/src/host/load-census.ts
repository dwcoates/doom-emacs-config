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
 * - THE FRAMES IT HOLDS (the host document only, `reportsFrames`): every
 *   census names the workspaces it holds a frame for (`frames`), and a change
 *   to them is a census of its own (`trigger: change`), so the daemon counts
 *   the pages the web view truly holds and compares them with what Emacs
 *   reports it asked for (daemon/internal/pageheld). Emacs's own table is what
 *   it asked the host to hold: a call made while no host stood, a reload that
 *   retired every frame, or a call the host failed leaves the two apart.
 * - THE STALL PROBE: a heartbeat on a clock the main thread cannot delay (a
 *   worker's) posts a message to the main thread every STALL_PROBE_MS; a
 *   message delivered STALL_THRESHOLD_MS or more after it was sent means the
 *   main thread was blocked that long, recorded with the census at that
 *   instant. It is NOT a main-thread timer: macOS throttles the timers of a
 *   covered (occluded, still "visible") WebKit view to about 1s, which made a
 *   timer probe record a 750ms "stall" every tick (2026-11-22), while message
 *   delivery is not throttled. A beat across a time the webhost was hidden
 *   proves nothing, so it is never reported.
 */
import { createLogger } from "../log.js";
import { pageSize, type PageSize } from "../page-size.js";
import { CENSUS_EVERY_MS } from "../page-census.js";
import { every, pageAfter, type After } from "../every.js";
import { workerHeartbeat, type Heartbeat } from "./stall-heartbeat.js";
import { FRAME_SHOWN_ATTRIBUTE, FRAME_WORKSPACE_ATTRIBUTE } from "./host.js";
import { queryAll } from "../dom.js";

const log = createLogger("webapp.host.load-census");

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
  const frames = queryAll<HTMLIFrameElement>(doc, `iframe[${FRAME_WORKSPACE_ATTRIBUTE}]`);
  const shown = doc.querySelector<HTMLIFrameElement>(`iframe[${FRAME_WORKSPACE_ATTRIBUTE}][${FRAME_SHOWN_ATTRIBUTE}]`);
  return {
    pages: frames.length,
    shown_workspace: shown?.getAttribute(FRAME_WORKSPACE_ATTRIBUTE) ?? "",
    host: pageSize(doc),
  };
}

/**
 * Every workspace DOC holds a frame for, once each and sorted: a page being
 * replaced holds two frames for one workspace until the fresh one draws.
 */
export function heldFrames(doc: Document): string[] {
  const ids = new Set<string>();
  for (const frame of queryAll<HTMLIFrameElement>(doc, `iframe[${FRAME_WORKSPACE_ATTRIBUTE}]`)) {
    const id = frame.getAttribute(FRAME_WORKSPACE_ATTRIBUTE);
    if (id !== null && id !== "") ids.add(id);
  }
  return [...ids].sort();
}

/** Why a census was taken: its period came round, or the frames changed. */
export type CensusTrigger = "tick" | "change";

/** What the census needs from its environment, injectable for tests. */
export interface LoadCensusEnv {
  readonly doc: Document;
  /** Monotonic milliseconds. */
  readonly now: () => number;
  readonly after: After;
  /** The beats of the clock the main thread cannot delay. */
  readonly heartbeat: Heartbeat;
}

/** The installed census; `stop` cancels both loops. */
export interface LoadCensus {
  stop(): void;
}

/**
 * Install the census and the stall probe on the webhost's DOC. REPORTS_FRAMES
 * is set for the host document alone, which holds the workspaces' frames; the
 * sidebar document holds none.
 */
export function installLoadCensus(
  doc: Document,
  reportsFrames: boolean,
  env: LoadCensusEnv = {
    doc,
    now: () => performance.now(),
    after: pageAfter,
    heartbeat: workerHeartbeat,
  },
): LoadCensus {
  const { now, after } = env;
  // A HIDDEN STRETCH VOIDS THE PROBE that spans it: the timer was throttled,
  // not blocked.
  let hiddenSinceProbe = env.doc.visibilityState !== "visible";
  const onVisibility = (): void => {
    if (env.doc.visibilityState !== "visible") hiddenSinceProbe = true;
  };
  env.doc.addEventListener("visibilitychange", onVisibility);

  // A beat is the OLDEST of its block when it was sent after the previous
  // beat was delivered; beats queued behind the same block were sent before
  // that delivery and are not a second stall.
  let lastDeliveredAt = now();
  const stopProbe = env.heartbeat(STALL_PROBE_MS, (sentAt) => {
    const deliveredAt = now();
    const late = Math.round(deliveredAt - sentAt);
    const oldestOfBlock = sentAt >= lastDeliveredAt;
    lastDeliveredAt = deliveredAt;
    const voided = hiddenSinceProbe || env.doc.visibilityState !== "visible";
    hiddenSinceProbe = false;
    if (voided || !oldestOfBlock || late < STALL_THRESHOLD_MS) return;
    log.info("the web process's main thread was blocked {late}ms", {
      operation: "webhost.stall",
      context: { blocked_ms: late, ...summarize(env.doc) },
    }, { late });
  });

  const census = (trigger: CensusTrigger): void => {
    const { pages, ...summary } = summarize(env.doc);
    const frames = reportsFrames ? { frames: heldFrames(env.doc), trigger } : {};
    log.info("the webhost holds {pages} pages; its own document holds {dom_nodes} elements", {
      operation: "webhost.census",
      context: { ...summary, ...frames },
    }, { pages, dom_nodes: summary.host.dom_nodes });
  };
  const stopCensus = every(after, CENSUS_EVERY_MS, () => census("tick"));

  // A CHANGE TO THE FRAMES IS A CENSUS OF ITS OWN, so the daemon's count
  // follows a load or an unload at once rather than at the next period. The
  // host appends every frame to its body (host.ts `frameParent`); one task's
  // changes (a replay's many frames) arrive as one batch, so one census.
  let lastFrames = reportsFrames ? heldFrames(env.doc).join("\n") : "";
  const observer = reportsFrames
    ? new MutationObserver(() => {
        const now = heldFrames(env.doc).join("\n");
        if (now === lastFrames) return;
        lastFrames = now;
        census("change");
      })
    : null;
  observer?.observe(env.doc.body, { childList: true });

  return {
    stop(): void {
      stopProbe();
      stopCensus();
      observer?.disconnect();
      env.doc.removeEventListener("visibilitychange", onVisibility);
    },
  };
}
