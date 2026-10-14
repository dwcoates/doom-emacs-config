/**
 * stream-cost — HOW MUCH MAIN-THREAD WORK EACH OF THIS PAGE'S STREAMS COSTS,
 * and how big each component's drawing is, one record per window.
 *
 * Every workspace page in the webhost shares one WebKit process and one main
 * thread, so the work one page does on a push delays every other page. Which
 * component that work belongs to (the sidebar, the topbar, the footer, the
 * feed) decides whether keeping every page live is affordable, and the
 * process-wide CPU samples cannot split it. Every stream frame runs through
 * `watchStream`, which times its handling (the strict check and the
 * component's draw) and records it here; once a window, the page states the
 * totals per stream with each component's DOM size beside them, the nearest
 * thing to its memory the page can count.
 *
 * ONE RECORD PER WINDOW, AND NONE FOR AN IDLE ONE: a page whose streams carried
 * nothing in a window says nothing, except its first window, which always
 * records the page's DOM sizes.
 *
 * The decode of a frame happens in the transport, before `watchStream` sees
 * it, and is not in these totals.
 */
import { log } from "./log.js";

/** How long one window lasts. */
export const STREAM_COST_WINDOW_MS = 60_000;

/** The page regions whose DOM size is recorded, by component. */
export const STREAM_COST_REGIONS: Readonly<Record<string, string>> = Object.freeze({
  sidebar: "#ws-sidebar",
  topbar: "#topbar",
  feed: "#feed-scroll",
  footer: "#footer",
});

/** What the meter needs from its environment, injectable for tests. */
export interface StreamCostEnv {
  /** Monotonic milliseconds. */
  readonly now: () => number;
  /** Run FN every MS milliseconds; answers the stop. */
  readonly every: (fn: () => void, ms: number) => () => void;
  /** The element count of each component's region, and of the whole page. */
  readonly nodes: () => Record<string, number>;
  /** Whether the webhost shows the page. */
  readonly presence: () => { shown: boolean };
}

/** The meter `watchStream` records into. */
export interface StreamCostMeter {
  /** One frame of STREAM took MS of main-thread work. */
  record(stream: string, ms: number): void;
  /** State the window now and start the next. */
  flush(): void;
  /** Stop the window timer. */
  dispose(): void;
}

/** One stream's totals in a window. */
interface StreamTotals {
  pushes: number;
  ms: number;
  max_ms: number;
}

/** Round MS to a tenth of a millisecond, for the record. */
function tenths(ms: number): number {
  return Math.round(ms * 10) / 10;
}

/** The element count of every region of DOC, and of the whole page. */
export function regionNodes(doc: Document): Record<string, number> {
  const counts: Record<string, number> = { page: doc.getElementsByTagName("*").length };
  for (const [component, selector] of Object.entries(STREAM_COST_REGIONS)) {
    counts[component] = doc.querySelector(selector)?.getElementsByTagName("*").length ?? 0;
  }
  return counts;
}

/** Meter this page's streams over ENV, stating one window every STREAM_COST_WINDOW_MS. */
export function createStreamCostMeter(env: StreamCostEnv): StreamCostMeter {
  let totals = new Map<string, StreamTotals>();
  let started = env.now();
  let first = true;

  const flush = (): void => {
    const at = env.now();
    const window = totals;
    totals = new Map();
    const wasFirst = first;
    first = false;
    const windowMs = at - started;
    started = at;
    if (window.size === 0 && !wasFirst) return;
    const streams: Record<string, StreamTotals> = {};
    let total = 0;
    for (const [name, t] of window) {
      streams[name] = { pushes: t.pushes, ms: tenths(t.ms), max_ms: tenths(t.max_ms) };
      total += t.ms;
    }
    log.info("the page's stream work over the last window", {
      operation: "page.stream-cost",
      context: {
        window_ms: Math.round(windowMs),
        total_ms: tenths(total),
        streams,
        nodes: env.nodes(),
        ...env.presence(),
      },
    });
  };

  const stop = env.every(flush, STREAM_COST_WINDOW_MS);

  return {
    record(stream, ms) {
      const t = totals.get(stream);
      if (t === undefined) {
        totals.set(stream, { pushes: 1, ms, max_ms: ms });
        return;
      }
      t.pushes += 1;
      t.ms += ms;
      if (ms > t.max_ms) t.max_ms = ms;
    },
    flush,
    dispose: stop,
  };
}
