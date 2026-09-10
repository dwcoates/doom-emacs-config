/**
 * THE WEBAPP LAYER'S PERF INSTRUMENT — `e2e/PERF-SPEC.md` §A4, §A5, §D1.
 *
 * Two facts govern every line in this file, and both are the spec's, not this
 * file's invention:
 *
 *  1. THE PAGE RUNS ON A FAKE CLOCK (§A4). `mountApp` installs
 *     `vi.useFakeTimers({ shouldAdvanceTime: true, now: HARNESS_EPOCH_MS })`,
 *     so `Date.now()` on the page reads ~1970 and a webapp-layer log timestamp
 *     can NEVER be subtracted from a daemon one. Only DURATIONS computed
 *     page-side mean anything. The escape is the one `mountApp` already uses
 *     for `yieldToIo` — capture the real primitive BEFORE the clock is faked —
 *     and that is what `realNow` and `microtask` below are.
 *
 *  2. `settle()` RETURNS LATE (§A4.4). It returns only after
 *     SETTLE_STABLE_ROUNDS quiet rounds, so a measurement taken after it
 *     over-counts by at least four rounds. NO INTERVAL HERE IS CLOSED BY
 *     `settle()`. It is used only to hand the event loop back so the real
 *     socket can deliver; the interval is closed by the probe, which fires on
 *     the frame itself.
 *
 * THE TERMINAL STAMP, and why it is a microtask rather than a MutationObserver.
 * §A5 establishes that `webapp/src/rpc/streams.ts:watchStream`'s inner
 * `consume` is the ONE place every server-streaming frame passes through:
 *
 *     assertNoUnknownFields(...) -> ctx.notePush?.() -> opts.onPush(response)
 *
 * `opts.onPush` writes the DOM SYNCHRONOUSLY — no virtual DOM, no diff, no
 * batching, no rAF anywhere in the apply path. So a microtask queued from
 * inside `notePush` runs after that synchronous apply has completed, i.e. at
 * `consume`'s return, which is exactly the terminal stamp §A5 names. A
 * MutationObserver would answer the same question one batch later and would
 * fold several frames into one callback when the stream delivers a burst; the
 * microtask is per-frame and cannot.
 *
 * WHAT THE INSTRUMENT COSTS, stated rather than hidden: the terminal stamp is
 * taken one microtask after the apply returns, so every duration here carries
 * one microtask's scheduling on top of the hop. That is a real term of the
 * number and is never subtracted out.
 *
 * NOTHING IN THIS FILE TOUCHES `webapp/src`. Wrapping `notePush` is a
 * test-side wrap of a page-wide observer the production context already
 * exposes for exactly this purpose (`webapp/src/rpc/context.ts`).
 */
import type { JsonObject } from "@bufbuild/protobuf";

import type { MountedApp } from "../integration/harness";

/**
 * The REAL clock, captured before anything fakes it.
 *
 * `performance.now()` rather than `Date.now()`: jsdom resolves the former to
 * sub-millisecond floats, and §C row 3b already warns that a millisecond
 * quantum is marginal against these budgets. The function is bound at module
 * evaluation, which ESM guarantees happens before any test body runs and so
 * before `mountApp` calls `vi.useFakeTimers`.
 */
export const realNow: () => number = globalThis.performance.now.bind(globalThis.performance);

/**
 * The REAL microtask queue, captured for the same reason and at the same
 * moment. Vitest's fake timers do not fake `queueMicrotask` by default; taking
 * the reference now means a future change to that default cannot silently move
 * this instrument's terminal stamp onto a fake clock's queue.
 */
const microtask: (fn: () => void) => void = ((): ((fn: () => void) => void) => {
  const real = globalThis.queueMicrotask;
  return (fn) => real.call(globalThis, fn);
})();

/**
 * N per assertion. §B fixes it at 20, and a recorder finishing short FAILS
 * rather than reporting a percentile over fewer samples (§D1).
 */
export const PERF_SAMPLES = 20;

/**
 * The recorder — the TypeScript twin of the Go `PerfRecorder`, with the
 * identical percentile rule (§D1).
 *
 * NEAREST-RANK, NO INTERPOLATION: p50 is sample 10 of a sorted 20 (the lower
 * median), p95 is sample 19 (ceil(0.95 x 20)). With N=20 an interpolated
 * percentile is a value no sample took, and a budget must be violated by a
 * real observation. NO SAMPLE IS EVER DISCARDED — no trimming, no outlier
 * rejection, no best-of; the p95 exists precisely to hold the tail.
 */
export class PerfRecorder {
  private readonly samples: number[] = [];

  constructor(readonly name: string) {}

  record(ms: number): void {
    this.samples.push(ms);
  }

  get n(): number {
    return this.samples.length;
  }

  private percentile(q: number): number {
    if (this.samples.length === 0) return 0;
    const sorted = [...this.samples].sort((a, b) => a - b);
    const rank = Math.min(Math.max(Math.ceil(q * sorted.length), 1), sorted.length);
    return sorted[rank - 1];
  }

  p50(): number {
    return this.percentile(0.5);
  }

  p95(): number {
    return this.percentile(0.95);
  }

  min(): number {
    return this.percentile(0);
  }

  max(): number {
    return this.percentile(1);
  }

  /** This assertion's numbers, flattened into the shipped record's context. */
  toContext(): Record<string, number> {
    return {
      [`${this.name}.n`]: this.n,
      [`${this.name}.min_ms`]: round3(this.min()),
      [`${this.name}.p50_ms`]: round3(this.p50()),
      [`${this.name}.p95_ms`]: round3(this.p95()),
      [`${this.name}.max_ms`]: round3(this.max()),
    };
  }
}

function round3(v: number): number {
  return Math.round(v * 1000) / 1000;
}

/**
 * A probe over the page's own push chokepoint.
 *
 * One per mounted app, installed once and left installed: it wraps
 * `ctx.notePush` (which production calls immediately before `onPush` for every
 * frame of all seven server-streaming rpcs) and stamps each frame's arrival.
 * An armed waiter is closed on the FIRST frame after which its predicate
 * holds.
 */
export class ApplyProbe {
  private frameStamp: number | undefined;
  // SEVERAL WAITERS AT ONCE, because one turn carries several of the intervals
  // this area measures: rows 1b and 3a are two different hops over the same
  // twenty turns, and both must be armed before the send that starts them.
  // Each waiter closes on its own first satisfying frame, independently.
  private waiters: Array<{
    origin: number | "frame";
    predicate: () => boolean;
    settle: (ms: number) => void;
  }> = [];

  private constructor(private readonly restore: () => void) {}

  /** Install the probe on a mounted app. */
  static install(app: MountedApp): ApplyProbe {
    // `AppContext.notePush` is a plain method on the context object
    // production builds; wrapping it here adds a stamp and changes no
    // behavior — the inner call still runs, in order, before `onPush`.
    const ctx = app.ctx as unknown as { notePush: () => void };
    const inner = ctx.notePush.bind(ctx);
    // Assigned below, and the wrap is handed to `ctx` before that happens: a push that
    // arrived in between would find no probe, so it says so rather than reading undefined.
    let probe: ApplyProbe | undefined = undefined;
    ctx.notePush = (): void => {
      if (probe === undefined) {
        throw new Error("perf: a push reached notePush before the probe was constructed");
      }
      probe.onFrame();
      inner();
    };
    probe = new ApplyProbe(() => {
      ctx.notePush = inner;
    });
    return probe;
  }

  /** Remove the wrap. Called from a test's teardown. */
  uninstall(): void {
    this.restore();
  }

  private onFrame(): void {
    const stamp = realNow();
    this.frameStamp = stamp;
    if (this.waiters.length === 0) return;
    const armed = [...this.waiters];
    // The terminal stamp: one microtask after this frame's SYNCHRONOUS apply,
    // which is `consume`'s return (§A5).
    microtask(() => {
      const closed = realNow();
      for (const waiter of armed) {
        if (!this.waiters.includes(waiter)) continue;
        if (!waiter.predicate()) continue;
        this.waiters = this.waiters.filter((w) => w !== waiter);
        waiter.settle(closed - (waiter.origin === "frame" ? stamp : waiter.origin));
      }
    });
  }

  /**
   * Wait for the first frame after which `predicate` holds, measuring from
   * THAT FRAME'S ARRIVAL. This is the `consume` entry -> `consume` return
   * interval §A5 defines: decode plus the synchronous DOM write, and nothing
   * else.
   */
  armFromFrame(predicate: () => boolean): Promise<number> {
    return this.arm("frame", predicate);
  }

  /**
   * Wait for the first frame after which `predicate` holds, measuring from an
   * ORIGIN THE CALLER STAMPED — a click, typically. This is a full round trip
   * (the page's rpc, the daemon's work, the push back, the apply), and the
   * rows that use it say so.
   */
  armFrom(origin: number, predicate: () => boolean): Promise<number> {
    return this.arm(origin, predicate);
  }

  private arm(origin: number | "frame", predicate: () => boolean): Promise<number> {
    return new Promise<number>((settle) => {
      this.waiters.push({ origin, predicate, settle });
    });
  }

  /** Discard every armed waiter, so a failed sample cannot close a later one. */
  disarm(): void {
    this.waiters = [];
  }

  /** The most recent frame's arrival stamp, for a diagnostic. */
  lastFrameStamp(): number | undefined {
    return this.frameStamp;
  }
}

/**
 * Drive the page until an armed probe closes, and answer its duration.
 *
 * `settle()` here is a PUMP, never the terminal stamp: it hands the event loop
 * back so the real socket can deliver, and the interval was already closed by
 * the probe's own microtask by the time this loop next looks. That is the
 * distinction §A4.4 requires.
 */
export async function awaitSample(
  app: MountedApp,
  probe: ApplyProbe,
  what: string,
  armed: Promise<number>,
  budgetMs: number,
): Promise<number> {
  let closed: number | undefined;
  void armed.then((ms) => {
    closed = ms;
  });
  const deadline = realNow() + budgetMs;
  for (;;) {
    await app.settle();
    if (closed !== undefined) return closed;
    if (realNow() >= deadline) {
      probe.disarm();
      throw new Error(
        `${what}: the probe never closed within ${budgetMs}ms; ` +
          `failure arms: [${app.failureArms().join(", ")}]; ` +
          `refusal arms: [${app.refusalArms().join(", ")}]`,
      );
    }
  }
}

/**
 * THE OPERATION THE GO DRIVER MATCHES ON, VERBATIM.
 *
 * Matched against `wlPerfSamplesOperation` in `e2e/perf_webapp_test.go`; the
 * two constants are documented on each other and move together.
 */
export const PERF_SAMPLES_OPERATION = "webapp-layer.perf.samples";

/**
 * Ship every recorder's finished p50/p95 to Go as ONE `ClientLog` record.
 *
 * ONE AGGREGATED RECORD, NEVER ONE PER SAMPLE (§D1, §A5): the page's own log
 * sink passes records through `clientlog-throttle.ts`
 * (DEFAULT_INTERVAL_MS = 2000), so a per-sample record would be throttled
 * away. This call goes straight down the client rather than through the sink,
 * for the same reason the handover area's mounted marker does.
 */
export async function shipSamples(app: MountedApp, recorders: PerfRecorder[]): Promise<void> {
  const context: Record<string, number> = {};
  for (const recorder of recorders) Object.assign(context, recorder.toContext());
  await app.ctx.client.clientLog({
    workspace: app.ctx.workspace,
    record: {
      level: { case: "info", value: {} },
      operation: PERF_SAMPLES_OPERATION,
      message: "the webapp layer's perf phase finished; its percentiles ride this record's context",
      context: context as unknown as JsonObject,
    },
  });
}
