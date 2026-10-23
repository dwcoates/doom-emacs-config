/**
 * page-stall — HOW LONG THIS PAGE'S MAIN THREAD STALLED WHILE SOMEONE SAW IT.
 *
 * `page_stall` (proto/vocab/latencies.json): the page paints nothing and
 * answers nothing while its main thread is busy. A steady sampling timer runs
 * every `PAGE_STALL_INTERVAL_MS`; the time between two runs, less the
 * interval, is how long the thread could not run it. A stall of at least
 * `PAGE_STALL_FLOOR_MS` (the vocabulary's detection floor) is reported;
 * anything shorter is a healthy page and sends nothing.
 *
 * ONLY A SEEN PAGE SAMPLES (presence.ts `seen`). WebKit slows the timers of a
 * page nobody can see, which would read as stalls, and nobody is waiting on
 * it anyway. Each time the page comes into view the sampler starts afresh.
 *
 * A GAP PAST `PAGE_STALL_UNCLASSIFIED_MS` IS NOT A STALL: the host slept, or
 * WebKit suspended the view. It is recorded at info and reported as nothing.
 */
import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { log } from "./log.js";
import type { Presence } from "./presence.js";
import type { RecordLatency } from "./wall-latency.js";

/** Milliseconds between two runs of the sampler. */
export const PAGE_STALL_INTERVAL_MS = 100;
/** The shortest stall reported: `page_stall`'s detection floor. */
export const PAGE_STALL_FLOOR_MS = 50;
/** The longest gap measured as a stall; a longer one is a host sleep. */
export const PAGE_STALL_UNCLASSIFIED_MS = 60_000;

/** The clock and timer the sampler runs on. */
export interface PageStallEnv {
  /** Monotonic milliseconds. */
  readonly now: () => number;
  /** Run FN every MS milliseconds; answers the stop. */
  readonly every: (fn: () => void, ms: number) => () => void;
}

/** Sample the page PRESENCE describes while it is seen; answers the stop. */
export function watchPageStall(presence: Presence, env: PageStallEnv, record: RecordLatency): () => void {
  let stopSampling: (() => void) | undefined;
  let last = 0;
  const sample = (): void => {
    const now = env.now();
    const stall = now - last - PAGE_STALL_INTERVAL_MS;
    last = now;
    if (stall < PAGE_STALL_FLOOR_MS) return;
    if (stall > PAGE_STALL_UNCLASSIFIED_MS) {
      log.info("the page's sampler ran after a gap longer than any stall; the host slept", {
        operation: "page.stall-unclassified",
        context: { gap_ms: Math.round(stall + PAGE_STALL_INTERVAL_MS) },
      });
      return;
    }
    record(ReportedLatencyKind.PAGE_STALL, stall / 1000);
  };
  const follow = (): void => {
    const seen = presence.seen();
    if (seen && stopSampling === undefined) {
      last = env.now();
      stopSampling = env.every(sample, PAGE_STALL_INTERVAL_MS);
    } else if (!seen && stopSampling !== undefined) {
      stopSampling();
      stopSampling = undefined;
    }
  };
  const stopFollowing = presence.onChange(follow);
  follow();
  return () => {
    stopFollowing();
    stopSampling?.();
    stopSampling = undefined;
  };
}
