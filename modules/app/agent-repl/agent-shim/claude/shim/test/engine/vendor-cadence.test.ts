/**
 * test/engine/vendor-cadence.test.ts — a long wait on the vendor is recorded,
 * with how busy the shim was across it; an ordinary one is not.
 */
import type { EventLoopUtilization } from "node:perf_hooks";
import { describe, expect, it } from "vitest";
import { VENDOR_PAUSE_MS, VendorCadence, type LoopMeter } from "../../src/engine/vendor-cadence.js";
import { logRecordsSince, logSinkMark, type LogRecord } from "../log-records.js";

const OPERATION = "shim.engine.vendor_cadence";

/** A clock the test moves, and a loop meter that answers a set utilization. */
function cadenceAt(utilization = 0.01): { cadence: VendorCadence; advance: (ms: number) => void } {
  let now = 1_000;
  const reading: EventLoopUtilization = { idle: 0, active: 0, utilization: 0 };
  const meter: LoopMeter = { mark: () => reading, utilizationSince: () => utilization };
  return { cadence: new VendorCadence(() => now, meter), advance: (ms) => (now += ms) };
}

/** The cadence records written since MARK. */
function pauses(mark: number): LogRecord[] {
  return logRecordsSince(mark).filter((record) => record.operation === OPERATION);
}

/** Receive N messages back to back, each asked for and arriving at once. */
function clump(cadence: VendorCadence, n: number): void {
  for (let i = 0; i < n; i++) {
    cadence.waiting();
    cadence.received();
  }
}

describe("VendorCadence", () => {
  it("records a long wait with the shim's utilization across it and the clump before it", () => {
    // Arrange
    const { cadence, advance } = cadenceAt(0.02);
    cadence.received();
    clump(cadence, 10);
    cadence.waiting();
    advance(450);
    const mark = logSinkMark();

    // Act
    cadence.received();

    // Assert
    expect(pauses(mark).map((record) => [record.level, record.context])).toEqual([
      ["debug", expect.objectContaining({ waited_ms: 450, event_loop_utilization: 0.02, messages_before: 11 })],
    ]);
  });

  it("records nothing for a wait shorter than the pause", () => {
    // Arrange
    const { cadence, advance } = cadenceAt();
    cadence.received();
    cadence.waiting();
    advance(VENDOR_PAUSE_MS - 1);
    const mark = logSinkMark();

    // Act
    cadence.received();

    // Assert
    expect(pauses(mark)).toEqual([]);
  });

  it("records a wait of exactly the pause", () => {
    // Arrange
    const { cadence, advance } = cadenceAt();
    cadence.received();
    cadence.waiting();
    advance(VENDOR_PAUSE_MS);
    const mark = logSinkMark();

    // Act
    cadence.received();

    // Assert
    expect(pauses(mark)).toHaveLength(1);
  });

  it("records nothing for a loop's first message, whose wait was never measured", () => {
    // Arrange
    const { cadence, advance } = cadenceAt();
    advance(5_000);
    const mark = logSinkMark();

    // Act
    cadence.received();

    // Assert
    expect(pauses(mark)).toEqual([]);
  });

  it("counts only the messages since the previous recorded pause", () => {
    // Arrange: a first clump of 3 ended by a pause, then a clump of 2.
    const { cadence, advance } = cadenceAt();
    clump(cadence, 3);
    cadence.waiting();
    advance(300);
    cadence.received();
    clump(cadence, 1);
    cadence.waiting();
    advance(300);
    const mark = logSinkMark();

    // Act
    cadence.received();

    // Assert
    expect(pauses(mark).map((record) => record.context.messages_before)).toEqual([2]);
  });
});
