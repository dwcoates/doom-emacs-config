import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";

import { ClientLogThrottle, type ClientLogSend } from "../src/clientlog-throttle.js";
import type { ClientLogLevel } from "../src/log.js";
import {
  ClientLogRecordSchema,
  type ClientLogRecord,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";

function record(level: ClientLogLevel, message: string): ClientLogRecord {
  return create(ClientLogRecordSchema, {
    level: { case: level, value: {} },
    operation: "test.throttle",
    message,
    context: {},
    timestamp: "2026-09-10T12:00:00.000000-04:00",
    verbose: false,
  });
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
  sent: ClientLogRecord[];
  setAccept: (accept: boolean) => void;
} {
  const sent: ClientLogRecord[] = [];
  let accept = options.accept ?? true;
  const timer = new ManualTimer();
  const send: ClientLogSend = (entry) => {
    if (!accept) return false;
    sent.push(entry);
    return true;
  };
  const throttle = new ClientLogThrottle({
    send,
    droppedRecord: (dropped, bufferBound) =>
      create(ClientLogRecordSchema, {
        level: { case: "warn", value: {} },
        operation: "webapp.client-log-throttle-dropped",
        message: `client log forwarding dropped ${dropped} record(s) over its ${bufferBound}-record buffer bound`,
        context: { dropped, buffer_bound: bufferBound },
        timestamp: "2026-09-10T12:00:00.000000-04:00",
        verbose: false,
      }),
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
    throttle.write(record("info", "one"));

    // Assert.
    expect(sent).toEqual([]);
    expect(throttle.bufferedCount()).toBe(1);
  });

  it("flushes once the buffer reaches the batch size", () => {
    // Arrange.
    const { throttle, sent } = harness({ maxBatch: 3 });

    // Act.
    throttle.write(record("info", "one"));
    throttle.write(record("info", "two"));
    throttle.write(record("info", "three"));

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["one", "two", "three"]);
    expect(throttle.bufferedCount()).toBe(0);
  });

  it("flushes buffered records when the interval deadline fires", () => {
    // Arrange.
    const { throttle, timer, sent } = harness();
    throttle.write(record("info", "one"));
    expect(sent).toEqual([]);

    // Act.
    timer.fire();

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["one"]);
  });

  it("flushes immediately on an error, carrying the buffered records ahead of it", () => {
    // Arrange.
    const { throttle, sent } = harness();
    throttle.write(record("info", "earlier"));

    // Act.
    throttle.write(record("error", "boom"));

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["earlier", "boom"]);
  });

  it("refuses and counts a record that arrives with the buffer bound already full", () => {
    // Arrange.
    const { throttle } = harness({ maxBatch: 50, maxBuffer: 2 });
    throttle.write(record("info", "one"));
    throttle.write(record("info", "two"));

    // Act.
    const accepted = throttle.write(record("info", "three"));

    // Assert.
    expect(accepted).toBe(false);
    expect(throttle.droppedCount()).toBe(1);
    expect(throttle.bufferedCount()).toBe(2);
  });

  it("reports the drop count as its own record ahead of the next flush", () => {
    // Arrange.
    const { throttle, timer, sent } = harness({ maxBuffer: 1 });
    throttle.write(record("info", "kept"));
    throttle.write(record("info", "lost"));

    // Act.
    timer.fire();

    // Assert.
    expect(sent[0].level.case).toBe("warn");
    expect(sent[0].message).toContain("dropped 1 record(s)");
    expect(sent[0].context?.dropped).toBe(1);
    expect(sent.map((r) => r.message.includes("kept"))).toContain(true);
    expect(throttle.droppedCount()).toBe(0);
  });

  it("releases at most one batch per flush and re-arms for the remainder", () => {
    // Arrange: a burst larger than one batch.
    const { throttle, timer, sent } = harness({ maxBatch: 2, maxBuffer: 10 });

    // Act.
    throttle.write(record("info", "one"));
    throttle.write(record("info", "two"));
    throttle.write(record("info", "three"));

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["one", "two"]);
    expect(throttle.bufferedCount()).toBe(1);
    expect(timer.armed()).toBe(true);
  });

  it("retains a refused record at the head so the next flush retries it in order", () => {
    // Arrange: the transport is down when the deadline fires.
    const { throttle, timer, sent, setAccept } = harness({ accept: false });
    throttle.write(record("info", "one"));
    throttle.write(record("info", "two"));
    timer.fire();
    expect(sent).toEqual([]);

    // Act.
    setAccept(true);
    timer.fire();

    // Assert.
    expect(sent.map((r) => r.message)).toEqual(["one", "two"]);
  });
});

describe("ClientLogThrottle: a refused drop summary", () => {
  it("keeps owing the count when the summary itself is refused", () => {
    // Arrange: one record fits, the second is dropped and counted.
    const { throttle, setAccept, sent } = harness({ maxBuffer: 1 });
    throttle.write(record("info", "kept"));
    throttle.write(record("info", "lost"));
    setAccept(false);

    // Act: the refused flush must not forget the drop.
    throttle.flush();
    setAccept(true);
    throttle.flush();

    // Assert.
    expect(sent[0].message).toBe(
      "client log forwarding dropped 1 record(s) over its 1-record buffer bound",
    );
  });

  it("reports the drop exactly once across the two flushes", () => {
    // Arrange.
    const { throttle, setAccept, sent } = harness({ maxBuffer: 1 });
    throttle.write(record("info", "kept"));
    throttle.write(record("info", "lost"));
    setAccept(false);

    // Act.
    throttle.flush();
    setAccept(true);
    throttle.flush();
    throttle.flush();

    // Assert.
    expect(sent.filter((r) => r.message.startsWith("client log forwarding dropped"))).toHaveLength(1);
  });
});

describe("discard: the sink is gone, not failing", () => {
  it("answers how many buffered records it threw away", () => {
    // Arrange.
    const { throttle } = harness({});
    throttle.write(record("info", "one"));
    throttle.write(record("info", "two"));

    // Act.
    const lost = throttle.discard();

    // Assert.
    expect(lost).toBe(2);
  });

  it("counts the unreported drops in what it threw away", () => {
    // Arrange.
    const { throttle } = harness({ maxBuffer: 1 });
    throttle.write(record("info", "kept"));
    throttle.write(record("info", "lost to the bound"));

    // Act.
    const lost = throttle.discard();

    // Assert.
    expect(lost).toBe(2);
  });

  it("leaves the buffer empty, so a later flush sends nothing", () => {
    // Arrange.
    const { throttle, sent } = harness({});
    throttle.write(record("info", "one"));
    throttle.discard();

    // Act.
    throttle.flush();

    // Assert.
    expect(sent).toHaveLength(0);
  });
});
