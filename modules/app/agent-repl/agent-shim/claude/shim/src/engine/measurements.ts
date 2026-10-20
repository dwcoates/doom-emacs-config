/**
 * engine/measurements.ts — the shim's performance counters for the daemon's
 * always-on metrics (docs/PLAN-runtime-metrics.md,
 * docs/protobuf-design/runtime-metrics.md).
 *
 * WHAT IS MEASURED HERE: what only the shim can see (proto/vocab/latencies.json).
 * `vendor_first_output` is from a prompt registered with the send ledger,
 * just before it is pushed into the vendor, to the first top-level reply frame
 * the vendor stamps with that prompt's uuid; the shim's own keep-alive sends
 * are not prompts and are not counted. `shim_event_loop_delay` is how late a
 * steady timer ran (sampleEventLoop). The heap is read as each frame is built.
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

/** `shim_event_loop_delay`'s histogram bounds, in seconds, as the vocabulary states them. */
export const EVENT_LOOP_DELAY_BUCKETS: readonly number[] = [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5];

/** How often the event-loop sampler's timer is due. */
export const EVENT_LOOP_SAMPLE_INTERVAL_MS = 500;

/** The shim's heap as process.memoryUsage() reads it, in bytes. */
export interface HeapReading {
  readonly heapUsed: number;
  readonly heapTotal: number;
  readonly external: number;
}

/** Reads the shim's heap now. */
export type ReadHeap = () => HeapReading;

/** The process's own heap. */
export const processHeap: ReadHeap = () => process.memoryUsage();

/** The last counting instant handed out in this process. */
let lastCountingInstant = 0;

/** A counting instant strictly after every earlier one in this process. */
function nextCountingInstant(nowMs: number): number {
  lastCountingInstant = Math.max(Math.trunc(nowMs), lastCountingInstant + 1);
  return lastCountingInstant;
}

/** One latency kind's cumulative distribution within a counting run. */
class CumulativeHistogram {
  private readonly counts: number[];
  private count = 0;
  private sum = 0;

  constructor(
    private readonly kind: shimv1.ShimLatencyKind,
    private readonly bounds: readonly number[],
  ) {
    this.counts = bounds.map(() => 0);
  }

  /**
   * One observation, in seconds. A negative or non-finite duration cannot
   * come from a monotonic clock and is a defect in the caller, so it throws.
   */
  observe(seconds: number, what: string): void {
    if (!Number.isFinite(seconds) || seconds < 0) {
      throw new Error(`measurements: ${what} of ${seconds} seconds is not a duration`);
    }
    this.bounds.forEach((bound, i) => {
      if (seconds <= bound) this.counts[i] += 1;
    });
    this.count += 1;
    this.sum += seconds;
  }

  /** The histogram as the measurements arm states it, or nothing before its first observation. */
  snapshot(): shimv1.ShimLatencyHistogram | undefined {
    if (this.count === 0) return undefined;
    return create(shimv1.ShimLatencyHistogramSchema, {
      kind: this.kind,
      buckets: this.bounds.map((bound, i) =>
        create(shimv1.ShimLatencyBucketSchema, {
          upperBoundSeconds: bound,
          cumulativeCount: BigInt(this.counts[i]),
        }),
      ),
      count: BigInt(this.count),
      sumSeconds: this.sum,
    });
  }
}

/** One counting run's figures. */
export class Measurements {
  /** When this run started counting, Unix milliseconds. */
  readonly countedSinceMs: number;
  private readonly firstOutput = new CumulativeHistogram(shimv1.ShimLatencyKind.FIRST_OUTPUT, VENDOR_FIRST_OUTPUT_BUCKETS);
  private readonly eventLoopDelay = new CumulativeHistogram(shimv1.ShimLatencyKind.EVENT_LOOP_DELAY, EVENT_LOOP_DELAY_BUCKETS);
  private changes = 0;

  constructor(
    nowMs: () => number,
    private readonly readHeap: ReadHeap,
  ) {
    this.countedSinceMs = nextCountingInstant(nowMs());
  }

  /**
   * How many times a figure changed: a watch sends a frame only when this
   * moved since the frame before.
   */
  get version(): number {
    return this.changes;
  }

  /** One vendor first output, in seconds, measured on a monotonic clock. */
  observeFirstOutput(seconds: number): void {
    this.firstOutput.observe(seconds, "a vendor first output");
    this.changes += 1;
  }

  /** One event-loop delay sample, in seconds, measured on a monotonic clock. */
  observeEventLoopDelay(seconds: number): void {
    this.eventLoopDelay.observe(seconds, "an event-loop delay");
    this.changes += 1;
  }

  /** Every figure as the session stream's measurements arm states it, the heap read now. */
  snapshot(): shimv1.ShimMeasurements {
    const latencies = [this.firstOutput.snapshot(), this.eventLoopDelay.snapshot()].filter(
      (h): h is shimv1.ShimLatencyHistogram => h !== undefined,
    );
    const heap = this.readHeap();
    return create(shimv1.ShimMeasurementsSchema, {
      countedSinceUnixMs: BigInt(this.countedSinceMs),
      latencies,
      heap: create(shimv1.ShimHeapSchema, {
        usedBytes: BigInt(Math.trunc(heap.heapUsed)),
        totalBytes: BigInt(Math.trunc(heap.heapTotal)),
        externalBytes: BigInt(Math.trunc(heap.external)),
      }),
    });
  }
}

/**
 * Samples the event loop's delay into `measurements` until `signal` aborts:
 * a timer is armed for EVENT_LOOP_SAMPLE_INTERVAL_MS, and how much later than
 * due it ran (on the monotonic clock `monoMs`) is one sample. The loop busy
 * elsewhere when the timer was due is exactly what delays every frame and
 * every answer the shim sends.
 */
export async function sampleEventLoop(
  measurements: Measurements,
  wait: Wait,
  monoMs: () => number,
  signal: AbortSignal,
): Promise<void> {
  while (!signal.aborted) {
    const due = monoMs() + EVENT_LOOP_SAMPLE_INTERVAL_MS;
    await wait(EVENT_LOOP_SAMPLE_INTERVAL_MS, signal);
    if (signal.aborted) return;
    measurements.observeEventLoopDelay(Math.max(0, monoMs() - due) / 1000);
  }
}

/** Waits `ms`, settling early when `signal` aborts. */
export type Wait = (ms: number, signal: AbortSignal) => Promise<void>;

/** A wait on the process's own timer, which holds the process open unless `background`. */
function waitOn(background: boolean): Wait {
  return (ms, signal) =>
    new Promise((resolve) => {
      if (signal.aborted) {
        resolve();
        return;
      }
      const timer = setTimeout(resolve, ms);
      if (background) timer.unref();
      signal.addEventListener(
        "abort",
        () => {
          clearTimeout(timer);
          resolve();
        },
        { once: true },
      );
    });
}

/** The process's own timer. */
export const timerWait: Wait = waitOn(false);

/**
 * The process's own timer, never holding the process open: for sampling that
 * runs for the process's whole life and must not keep it alive past its end.
 */
export const backgroundTimerWait: Wait = waitOn(true);

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
