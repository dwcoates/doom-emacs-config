import { describe, expect, it } from "vitest";

import { ClientLogThrottle, type ClientLogSend } from "../src/clientlog-throttle.js";
import type { ClientLogContext } from "../src/protocol.js";
import type { ClientLogLevel } from "../src/wslog.js";

interface SentRecord {
  level: ClientLogLevel;
  message: string;
  context?: ClientLogContext;
}

/** A manual timer, so every flush deadline in these tests is fired explicitly. */
class ManualTimer {
  private pending: (() => void) | null = null;

  readonly set = (fn: () => void): unknown => {
    this.pending = fn;
    return 1;
  };

  readonly clear = (): void => {
    this.pending = null;
  };

  armed(): boolean {
    return this.pending !== null;
  }

  fire(): void {
    const fn = this.pending;
    if (fn === null) throw new Error("no flush deadline was armed");
    this.pending = null;
    fn();
  }
}

function harness(options: { accept?: boolean; maxBatch?: number; maxBuffer?: number } = {}): {
  throttle: ClientLogThrottle;
  timer: ManualTimer;
  sent: SentRecord[];
  setAccept: (accept: boolean) => void;
} {
  const sent: SentRecord[] = [];
  let accept = options.accept ?? true;
  const timer = new ManualTimer();
  const send: ClientLogSend = (level, message, context) => {
    if (!accept) return false;
    sent.push({ level, message, context });
    return true;
  };
  const throttle = new ClientLogThrottle({
    send,
    intervalMs: 2000,
    maxBatch: options.maxBatch ?? 50,
    maxBuffer: options.maxBuffer ?? 500,
    setTimer: timer.set,
    clearTimer: timer.clear,
  });
  return { throttle, timer, sent, setAccept: (value) => { accept = value; } };
}

describe("ClientLogThrottle", () => {
  it("buffers a record below both flush thresholds instead of sending it", () => {
    // Arrange.
    const { throttle, sent } = harness();

    // Act.
    throttle.write("info", "one");

    // Assert.
    expect(sent).toEqual([]);
    expect(throttle.bufferedCount()).toBe(1);
  });

  it("flushes once the buffer reaches the batch size", () => {
    // Arrange.
    const { throttle, sent } = harness({ maxBatch: 3 });

    // Act.
    throttle.write("info", "one");
    throttle.write("info", "two");
    throttle.write("info", "three");

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["one", "two", "three"]);
    expect(throttle.bufferedCount()).toBe(0);
  });

  it("flushes buffered records when the interval deadline fires", () => {
    // Arrange.
    const { throttle, timer, sent } = harness();
    throttle.write("info", "one");
    expect(sent).toEqual([]);

    // Act.
    timer.fire();

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["one"]);
  });

  it("flushes immediately on an error, carrying the buffered records ahead of it", () => {
    // Arrange.
    const { throttle, sent } = harness();
    throttle.write("info", "earlier");

    // Act.
    throttle.write("error", "boom");

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["earlier", "boom"]);
  });

  it("refuses and counts a record that arrives with the buffer bound already full", () => {
    // Arrange.
    const { throttle } = harness({ maxBatch: 50, maxBuffer: 2 });
    throttle.write("info", "one");
    throttle.write("info", "two");

    // Act.
    const accepted = throttle.write("info", "three");

    // Assert.
    expect(accepted).toBe(false);
    expect(throttle.droppedCount()).toBe(1);
    expect(throttle.bufferedCount()).toBe(2);
  });

  it("reports the drop count as its own record ahead of the next flush", () => {
    // Arrange.
    const { throttle, timer, sent } = harness({ maxBuffer: 1 });
    throttle.write("info", "kept");
    throttle.write("info", "lost");

    // Act.
    timer.fire();

    // Assert.
    expect(sent[0].level).toBe("warn");
    expect(sent[0].message).toContain("dropped 1 record(s)");
    expect(sent[0].context?.dropped).toBe(1);
    expect(sent.map((r) => r.message.includes("kept"))).toContain(true);
    expect(throttle.droppedCount()).toBe(0);
  });

  it("releases at most one batch per flush and re-arms for the remainder", () => {
    // Arrange: a burst larger than one batch.
    const { throttle, timer, sent } = harness({ maxBatch: 2, maxBuffer: 10 });

    // Act.
    throttle.write("info", "one");
    throttle.write("info", "two");
    throttle.write("info", "three");

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["one", "two"]);
    expect(throttle.bufferedCount()).toBe(1);
    expect(timer.armed()).toBe(true);
  });

  it("retains a refused record at the head so the next flush retries it in order", () => {
    // Arrange: the transport is down when the deadline fires.
    const { throttle, timer, sent, setAccept } = harness({ accept: false });
    throttle.write("info", "one");
    throttle.write("info", "two");
    timer.fire();
    expect(sent).toEqual([]);

    // Act.
    setAccept(true);
    timer.fire();

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["one", "two"]);
  });
});
