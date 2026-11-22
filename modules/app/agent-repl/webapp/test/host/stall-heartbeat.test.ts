// @vitest-environment jsdom
// The stall probe's worker heartbeat. See src/host/stall-heartbeat.ts.
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { workerHeartbeat } from "../../src/host/stall-heartbeat.js";

class FakeWorker {
  static last: FakeWorker | null = null;
  onmessage: ((event: MessageEvent<number>) => void) | null = null;
  readonly posted: unknown[] = [];
  terminated = false;
  constructor(
    readonly url: string,
    readonly options: { name: string },
  ) {
    FakeWorker.last = this;
  }
  postMessage(message: unknown): void {
    this.posted.push(message);
  }
  terminate(): void {
    this.terminated = true;
  }
}

beforeEach(() => {
  FakeWorker.last = null;
  vi.stubGlobal("Worker", FakeWorker);
  URL.createObjectURL = () => "blob:heartbeat";
  URL.revokeObjectURL = () => undefined;
});

afterEach(() => {
  vi.unstubAllGlobals();
});

describe("workerHeartbeat", () => {
  it("hands the worker its interval as its name", () => {
    // Arrange / Act
    workerHeartbeat(250, () => undefined);

    // Assert
    expect(FakeWorker.last?.options.name).toBe("250");
  });

  it("states each beat's send time on the page's own monotonic clock", () => {
    // Arrange
    const beats: number[] = [];
    workerHeartbeat(250, (sentAt) => beats.push(sentAt));

    // Act
    FakeWorker.last?.onmessage?.({ data: performance.timeOrigin + 1234 } as MessageEvent<number>);

    // Assert
    expect(beats).toEqual([1234]);
  });

  it("stops and terminates the worker when stopped", () => {
    // Arrange
    const stop = workerHeartbeat(250, () => undefined);

    // Act
    stop();

    // Assert
    expect(FakeWorker.last?.posted).toEqual(["stop"]);
    expect(FakeWorker.last?.terminated).toBe(true);
  });
});
