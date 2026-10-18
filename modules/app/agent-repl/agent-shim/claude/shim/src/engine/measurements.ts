/**
 * engine/measurements.ts — the shim's performance counters for the daemon's
 * always-on metrics (docs/PLAN-runtime-metrics.md,
 * docs/protobuf-design/runtime-metrics.md).
 *
 * WHAT IS MEASURED HERE: the latencies whose end only the shim sees. Today
 * that is `vendor_first_output` (proto/vocab/latencies.json): from a prompt
 * registered with the send ledger, just before it is pushed into the vendor,
 * to the first top-level reply frame the vendor stamps with that prompt's
 * uuid. The shim's own keep-alive sends are not prompts and are not counted.
 *
 * EVERYTHING IS CUMULATIVE FROM ONE COUNTING INSTANT. A frame states every
 * figure since `countedSinceUnixMs`, so a frame lost with a broken stream is
 * subsumed by the next one, and a daemon that adopted this shim learns its
 * totals from the first frame it sees. A new Measurements is a new counting
 * run: its instant is strictly later than any earlier run's in this process,
 * so the daemon can never mistake one run for another.
 *
 * VENDOR TRAFFIC IS NOT COUNTED. The vendor's HTTP is made by the agent SDK's
 * own child process, outside this one, so `vendorTraffic` is left unset
 * rather than reported as zero (docs/METRICS-JUDGEMENT-CALLS.md, call 6).
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { shimv1 } from "../proto.js";

const LOGGER = bindLog({
  component: "shim-measurements",
  operation: "shim.engine.measurements",
});

/** How often, at most, a changed set of figures is sent on a watch. */
export const MEASUREMENTS_INTERVAL_MS = 15_000;

/**
 * `vendor_first_output`'s histogram bounds, in seconds. proto/vocab/latencies.json
 * holds the same list; the daemon refuses a frame whose bounds differ, and
 * test/engine/measurements.test.ts compares the two.
 */
export const VENDOR_FIRST_OUTPUT_BUCKETS: readonly number[] = [0.5, 1, 2, 3, 5, 8, 13, 21, 34, 60];

/** The last counting instant handed out in this process. */
let lastCountingInstant = 0;

/** A counting instant strictly after every earlier one in this process. */
function nextCountingInstant(nowMs: number): number {
  lastCountingInstant = Math.max(Math.trunc(nowMs), lastCountingInstant + 1);
  return lastCountingInstant;
}

/** One counting run's figures. */
export class Measurements {
  /** When this run started counting, Unix milliseconds. */
  readonly countedSinceMs: number;
  private readonly firstOutputCounts: number[] = VENDOR_FIRST_OUTPUT_BUCKETS.map(() => 0);
  private firstOutputCount = 0;
  private firstOutputSum = 0;
  private changes = 0;

  constructor(nowMs: () => number) {
    this.countedSinceMs = nextCountingInstant(nowMs());
  }

  /**
   * How many times a figure changed: a watch sends a frame only when this
   * moved since the frame before.
   */
  get version(): number {
    return this.changes;
  }

  /**
   * One vendor first output, in seconds, measured on a monotonic clock. A
   * negative or non-finite duration cannot come from a monotonic clock and is
   * a defect in the caller, so it throws.
   */
  observeFirstOutput(seconds: number): void {
    if (!Number.isFinite(seconds) || seconds < 0) {
      throw new Error(`measurements: a vendor first output of ${seconds} seconds is not a duration`);
    }
    VENDOR_FIRST_OUTPUT_BUCKETS.forEach((bound, i) => {
      if (seconds <= bound) this.firstOutputCounts[i] += 1;
    });
    this.firstOutputCount += 1;
    this.firstOutputSum += seconds;
    this.changes += 1;
  }

  /** Every figure as the session stream's measurements arm states it. */
  snapshot(): shimv1.ShimMeasurements {
    const latencies: shimv1.ShimLatencyHistogram[] = [];
    if (this.firstOutputCount > 0) {
      latencies.push(
        create(shimv1.ShimLatencyHistogramSchema, {
          kind: shimv1.ShimLatencyKind.FIRST_OUTPUT,
          buckets: VENDOR_FIRST_OUTPUT_BUCKETS.map((bound, i) =>
            create(shimv1.ShimLatencyBucketSchema, {
              upperBoundSeconds: bound,
              cumulativeCount: BigInt(this.firstOutputCounts[i]),
            }),
          ),
          count: BigInt(this.firstOutputCount),
          sumSeconds: this.firstOutputSum,
        }),
      );
    }
    return create(shimv1.ShimMeasurementsSchema, {
      countedSinceUnixMs: BigInt(this.countedSinceMs),
      latencies,
    });
  }
}

/** Waits `ms`, settling early when `signal` aborts. */
export type Wait = (ms: number, signal: AbortSignal) => Promise<void>;

/** The process's own timer. */
export const timerWait: Wait = (ms, signal) =>
  new Promise((resolve) => {
    if (signal.aborted) {
      resolve();
      return;
    }
    const timer = setTimeout(resolve, ms);
    signal.addEventListener(
      "abort",
      () => {
        clearTimeout(timer);
        resolve();
      },
      { once: true },
    );
  });

function measurementsFrame(m: Measurements): shimv1.WatchSessionResponse {
  return create(shimv1.WatchSessionResponseSchema, {
    frame: { case: "measurements", value: m.snapshot() },
  });
}

/**
 * The session stream's frames with the measurements arm merged in: every
 * frame of `frames` passes through unchanged and in order, and on a tick of
 * MEASUREMENTS_INTERVAL_MS that starts with the watch's first frame, one
 * measurements frame whenever the figures differ from the last one this watch
 * carried (a new watch's first tick sends any already counted). Nothing is
 * inserted among the opening frames, whose order is the readiness protocol.
 * Ending `frames`, or the consumer leaving, ends it and stops the timer.
 */
export async function* withMeasurements(
  frames: AsyncIterable<shimv1.WatchSessionResponse>,
  measurements: Measurements,
  wait: Wait = timerWait,
): AsyncGenerator<shimv1.WatchSessionResponse> {
  const iterator = frames[Symbol.asyncIterator]();
  const stop = new AbortController();
  type Next =
    | {
        readonly kind: "frame";
        readonly result: IteratorResult<shimv1.WatchSessionResponse>;
      }
    | { readonly kind: "failed"; readonly error: unknown }
    | { readonly kind: "tick" };
  // A PULL NEVER REJECTS: its failure is a value, rethrown where it is
  // awaited, so a pull still in flight when the consumer leaves can never
  // become an unobserved rejection.
  const pull = (): Promise<Next> =>
    iterator.next().then(
      (result): Next => ({ kind: "frame", result }),
      (error: unknown): Next => ({ kind: "failed", error }),
    );
  let pendingFrame: Promise<Next> = pull();
  let pendingTick: Promise<Next> | undefined;
  let sentVersion = 0;
  try {
    for (;;) {
      const next = pendingTick === undefined ? await pendingFrame : await Promise.race([pendingFrame, pendingTick]);
      if (next.kind === "tick") {
        pendingTick = wait(MEASUREMENTS_INTERVAL_MS, stop.signal).then(() => ({
          kind: "tick",
        }));
        if (measurements.version !== sentVersion) {
          sentVersion = measurements.version;
          yield measurementsFrame(measurements);
        }
        continue;
      }
      if (next.kind === "failed") throw next.error;
      if (next.result.done === true) return;
      // THE NEXT PULL AND THE TICK START BEFORE THE FRAME IS HANDED OVER, so
      // the clock runs from the watch's first frame whatever the consumer's
      // pace.
      pendingFrame = pull();
      pendingTick ??= wait(MEASUREMENTS_INTERVAL_MS, stop.signal).then(() => ({ kind: "tick" }));
      yield next.result.value;
    }
  } finally {
    stop.abort();
    // NOT AWAITED. The inner stream holds a pull in flight (pendingFrame), so
    // its return settles only once that pull does, at its next frame; waiting
    // here would hold this stream's own end hostage to it. The inner stream
    // ends itself at that frame either way.
    iterator.return?.().catch((error: unknown) => {
      LOGGER.error({ cause: String(error) }, "the session stream under the measurements did not end cleanly");
    });
  }
}
