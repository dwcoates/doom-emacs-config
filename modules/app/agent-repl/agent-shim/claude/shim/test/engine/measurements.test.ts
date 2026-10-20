/**
 * The shim's performance counters and the frames that carry them on a
 * session watch (src/engine/measurements.ts).
 */
import { readFileSync } from "node:fs";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import {
  MEASUREMENTS_INTERVAL_MS,
  Measurements,
  EVENT_LOOP_DELAY_BUCKETS,
  EVENT_LOOP_SAMPLE_INTERVAL_MS,
  VENDOR_FIRST_OUTPUT_BUCKETS,
  sampleEventLoop,
  withMeasurements,
  type Wait,
} from "../../src/engine/measurements.js";
import { conversationv1, shimv1 } from "../../src/proto.js";

/** A heap reader answering one fixed reading. */
const fixedHeap = (): { heapUsed: number; heapTotal: number; external: number } => ({ heapUsed: 10, heapTotal: 20, external: 3 });

const VOCAB = new URL("../../../../../proto/vocab/latencies.json", import.meta.url);

/** A frame the inner stream yields, told apart by its update arm's title. */
function frame(title: string): shimv1.WatchSessionResponse {
  return create(shimv1.WatchSessionResponseSchema, {
    frame: {
      case: "update",
      value: create(conversationv1.SessionUpdateSchema, {
        update: {
          case: "title",
          value: create(conversationv1.SessionTitleSchema, { text: title }),
        },
      }),
    },
  });
}

/** A frame's name for an expectation: its title, or "measurements". */
function nameOf(f: shimv1.WatchSessionResponse): string {
  if (f.frame.case === "measurements") return "measurements";
  if (f.frame.case === "update" && f.frame.value.update.case === "title") return f.frame.value.update.value.text;
  return f.frame.case ?? "unset";
}

/** An inner stream a test feeds by hand. */
class Feed implements AsyncIterable<shimv1.WatchSessionResponse> {
  private readonly queue: IteratorResult<shimv1.WatchSessionResponse>[] = [];
  private waiting: ((r: IteratorResult<shimv1.WatchSessionResponse>) => void) | undefined;
  private failure: ((e: unknown) => void) | undefined;
  returned = false;

  push(f: shimv1.WatchSessionResponse): void {
    this.settle({ value: f, done: false });
  }

  end(): void {
    this.settle({ value: undefined, done: true });
  }

  fail(error: unknown): void {
    const reject = this.failure;
    this.waiting = undefined;
    this.failure = undefined;
    reject?.(error);
  }

  private settle(r: IteratorResult<shimv1.WatchSessionResponse>): void {
    const resolve = this.waiting;
    this.waiting = undefined;
    this.failure = undefined;
    if (resolve === undefined) this.queue.push(r);
    else resolve(r);
  }

  [Symbol.asyncIterator](): AsyncIterator<shimv1.WatchSessionResponse> {
    return {
      next: () => {
        const queued = this.queue.shift();
        if (queued !== undefined) return Promise.resolve(queued);
        return new Promise((resolve, reject) => {
          this.waiting = resolve;
          this.failure = reject;
        });
      },
      return: () => {
        this.returned = true;
        return Promise.resolve({ value: undefined, done: true });
      },
    };
  }
}

/** A wait a test releases by hand, one tick at a time. */
class Ticks {
  private release: (() => void) | undefined;
  readonly intervals: number[] = [];
  signal: AbortSignal | undefined;

  readonly wait: Wait = (ms, signal) => {
    this.intervals.push(ms);
    this.signal = signal;
    return new Promise((resolve) => {
      this.release = resolve;
    });
  };

  tick(): void {
    const release = this.release;
    this.release = undefined;
    if (release === undefined) throw new Error("no tick is pending");
    release();
  }
}

/** Lets every pending promise reaction run. */
const settled = (): Promise<void> => new Promise((resolve) => setImmediate(resolve));

describe("Measurements", () => {
  it("holds vendor_first_output's buckets exactly as the vocabulary states them", () => {
    // Arrange
    const vocab = JSON.parse(readFileSync(VOCAB, "utf8")) as {
      latencies: {
        name: string;
        reported_as: string | null;
        buckets_seconds: number[] | null;
      }[];
    };

    // Act
    const entry = vocab.latencies.find((l) => l.name === "vendor_first_output");

    // Assert
    expect(entry?.reported_as).toBe("SHIM_LATENCY_KIND_FIRST_OUTPUT");
    expect(entry?.buckets_seconds).toEqual([...VENDOR_FIRST_OUTPUT_BUCKETS]);
  });

  it("states cumulative buckets, count and sum since its counting instant", () => {
    // Arrange
    const m = new Measurements(() => 1_700_000_000_000, fixedHeap);

    // Act
    m.observeFirstOutput(0.4);
    m.observeFirstOutput(2.5);
    const snapshot = m.snapshot();

    // Assert
    expect(snapshot.countedSinceUnixMs).toBe(BigInt(m.countedSinceMs));
    expect(snapshot.vendorTraffic).toBeUndefined();
    expect(snapshot.latencies).toHaveLength(1);
    const h = snapshot.latencies[0];
    expect(h.kind).toBe(shimv1.ShimLatencyKind.FIRST_OUTPUT);
    expect(h.buckets.map((b) => b.upperBoundSeconds)).toEqual([...VENDOR_FIRST_OUTPUT_BUCKETS]);
    expect(h.buckets.map((b) => Number(b.cumulativeCount))).toEqual([1, 1, 1, 2, 2, 2, 2, 2, 2, 2]);
    expect(h.count).toBe(2n);
    expect(h.sumSeconds).toBeCloseTo(2.9);
    expect(m.version).toBe(2);
  });

  it("states no histogram before its first observation", () => {
    // Act
    const snapshot = new Measurements(() => 1_700_000_000_000, fixedHeap).snapshot();

    // Assert
    expect(snapshot.latencies).toEqual([]);
  });

  it("gives every run a counting instant later than the run before, even within one millisecond", () => {
    // Act
    const first = new Measurements(() => 1_700_000_000_000, fixedHeap);
    const second = new Measurements(() => 1_700_000_000_000, fixedHeap);

    // Assert
    expect(second.countedSinceMs).toBeGreaterThan(first.countedSinceMs);
  });

  it.each([
    ["a negative duration", -0.1],
    ["a non-finite duration", Number.NaN],
  ])("refuses %s", (_name, seconds) => {
    // Arrange
    const m = new Measurements(() => 1_700_000_000_000, fixedHeap);

    // Act / Assert
    expect(() => m.observeFirstOutput(seconds)).toThrow("is not a duration");
    expect(m.version).toBe(0);
  });
});

describe("withMeasurements", () => {
  it("passes every frame through in order and inserts nothing before its first tick", async () => {
    // Arrange
    const feed = new Feed();
    const ticks = new Ticks();
    const m = new Measurements(() => 1_700_000_000_000, fixedHeap);
    m.observeFirstOutput(1);
    const stream = withMeasurements(feed, m, ticks.wait);

    // Act
    feed.push(frame("opening"));
    feed.push(frame("reannounced"));
    const first = await stream.next();
    const second = await stream.next();

    // Assert
    expect([first.value, second.value].map((f) => nameOf(f as shimv1.WatchSessionResponse))).toEqual([
      "opening",
      "reannounced",
    ]);
    expect(ticks.intervals).toEqual([MEASUREMENTS_INTERVAL_MS]);
    await stream.return(undefined);
  });

  it("sends the figures on a tick when they changed, and nothing on a tick when they did not", async () => {
    // Arrange
    const feed = new Feed();
    const ticks = new Ticks();
    const m = new Measurements(() => 1_700_000_000_000, fixedHeap);
    const stream = withMeasurements(feed, m, ticks.wait);
    feed.push(frame("opening"));
    await stream.next();
    const next = stream.next();

    // Act: an unchanged tick, then a change and a tick.
    ticks.tick();
    await settled();
    m.observeFirstOutput(1);
    ticks.tick();
    const measured = await next;

    // Assert: the unchanged tick sent nothing, so the next frame is the change.
    expect(nameOf(measured.value as shimv1.WatchSessionResponse)).toBe("measurements");
    expect(ticks.intervals).toEqual([MEASUREMENTS_INTERVAL_MS, MEASUREMENTS_INTERVAL_MS, MEASUREMENTS_INTERVAL_MS]);
    await stream.return(undefined);
  });

  it("ends when the inner stream ends", async () => {
    // Arrange
    const feed = new Feed();
    const stream = withMeasurements(feed, new Measurements(() => 1, fixedHeap), new Ticks().wait);

    // Act
    feed.end();
    const result = await stream.next();

    // Assert
    expect(result.done).toBe(true);
  });

  it("rethrows the inner stream's failure", async () => {
    // Arrange
    const feed = new Feed();
    const stream = withMeasurements(feed, new Measurements(() => 1, fixedHeap), new Ticks().wait);
    const pending = stream.next();

    // Act
    feed.fail(new Error("the subscriber overflowed"));

    // Assert
    await expect(pending).rejects.toThrow("the subscriber overflowed");
  });

  it("stops its timer and ends the inner stream when the consumer leaves", async () => {
    // Arrange
    const feed = new Feed();
    const ticks = new Ticks();
    const stream = withMeasurements(feed, new Measurements(() => 1, fixedHeap), ticks.wait);
    feed.push(frame("opening"));
    await stream.next();

    // Act
    await stream.return(undefined);

    // Assert
    expect(ticks.signal?.aborted).toBe(true);
    expect(feed.returned).toBe(true);
  });
});

describe("the event loop and the heap", () => {
  it("holds shim_event_loop_delay's buckets exactly as the vocabulary states them", () => {
    // Arrange
    const vocab = JSON.parse(readFileSync(VOCAB, "utf8")) as {
      latencies: { name: string; reported_as: string | null; buckets_seconds: number[] | null }[];
    };

    // Act
    const entry = vocab.latencies.find((l) => l.name === "shim_event_loop_delay");

    // Assert
    expect(entry?.reported_as).toBe("SHIM_LATENCY_KIND_EVENT_LOOP_DELAY");
    expect(entry?.buckets_seconds).toEqual([...EVENT_LOOP_DELAY_BUCKETS]);
  });

  it("states an event-loop delay as its own cumulative histogram", () => {
    // Arrange
    const m = new Measurements(() => 1_700_000_000_000, fixedHeap);

    // Act
    m.observeEventLoopDelay(0.02);
    const h = m.snapshot().latencies.find((l) => l.kind === shimv1.ShimLatencyKind.EVENT_LOOP_DELAY);

    // Assert
    expect(h?.count).toBe(1n);
    expect(h?.buckets.find((b) => b.upperBoundSeconds === 0.025)?.cumulativeCount).toBe(1n);
    expect(h?.buckets.find((b) => b.upperBoundSeconds === 0.01)?.cumulativeCount).toBe(0n);
    expect(m.version).toBe(1);
  });

  it("refuses an event-loop delay that is not a duration", () => {
    // Arrange
    const m = new Measurements(() => 1_700_000_000_000, fixedHeap);

    // Act and assert
    expect(() => m.observeEventLoopDelay(-1)).toThrow("is not a duration");
  });

  it("carries the heap read as the frame is built", () => {
    // Arrange
    const m = new Measurements(() => 1_700_000_000_000, fixedHeap);

    // Act
    const heap = m.snapshot().heap;

    // Assert
    expect(heap?.usedBytes).toBe(10n);
    expect(heap?.totalBytes).toBe(20n);
    expect(heap?.externalBytes).toBe(3n);
  });

  it("samples how late each steady timer ran, until its lifetime ends", async () => {
    // Arrange: the timer is due 500ms on; it runs 20ms late, then on time.
    const m = new Measurements(() => 1_700_000_000_000, fixedHeap);
    const ticks = new Ticks();
    let mono = 0;
    const life = new AbortController();
    const sampling = sampleEventLoop(m, ticks.wait, () => mono, life.signal);

    // Act
    mono = EVENT_LOOP_SAMPLE_INTERVAL_MS + 20;
    ticks.tick();
    await settled();
    mono += EVENT_LOOP_SAMPLE_INTERVAL_MS;
    ticks.tick();
    await settled();
    life.abort();
    ticks.tick();
    await sampling;

    // Assert
    const h = m.snapshot().latencies.find((l) => l.kind === shimv1.ShimLatencyKind.EVENT_LOOP_DELAY);
    expect(h?.count).toBe(2n);
    expect(h?.sumSeconds).toBeCloseTo(0.02, 6);
    expect(ticks.intervals.every((ms) => ms === EVENT_LOOP_SAMPLE_INTERVAL_MS)).toBe(true);
  });
});
