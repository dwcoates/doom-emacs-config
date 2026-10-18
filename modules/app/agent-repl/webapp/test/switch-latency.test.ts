// @vitest-environment jsdom
// How long a switch to this page, and this page's boot, took. See
// src/switch-latency.ts.
import { afterEach, describe, expect, it } from "vitest";

import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { createPresence, type PresenceSource } from "../src/presence.js";
import { watchPageBoot, watchSwitchLatency, type SwitchLatencyEnv } from "../src/switch-latency.js";
import { frameQueue, type FrameQueue } from "./frame-queue.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

/** An environment whose clocks, frames and drawn mark a test drives. */
class FakeEnv implements SwitchLatencyEnv {
  wall = 0;
  mono = 0;
  isDrawn = false;
  private readonly frames: FrameQueue = frameQueue();
  private drawnWaiters = new Set<() => void>();

  readonly wallNow = (): number => this.wall;
  readonly monoNow = (): number => this.mono;
  readonly frame = (fn: () => void): void => this.frames.frame(fn);
  readonly drawn = (): boolean => this.isDrawn;
  readonly onDrawn = (fn: () => void): (() => void) => {
    if (this.isDrawn) {
      fn();
      return () => undefined;
    }
    this.drawnWaiters.add(fn);
    return () => this.drawnWaiters.delete(fn);
  };

  /** Start one animation frame. */
  runFrames(): void {
    this.frames.run();
  }

  /** The page draws its conversation. */
  draw(): void {
    this.isDrawn = true;
    for (const fn of [...this.drawnWaiters]) fn();
    this.drawnWaiters.clear();
  }
}

type Recorded = [ReportedLatencyKind, number];

let presence: PresenceSource | null = null;

afterEach(() => {
  presence?.dispose();
  presence = null;
});

function watched(env: FakeEnv): { presence: PresenceSource; recorded: Recorded[] } {
  presence = createPresence(document, true);
  const recorded: Recorded[] = [];
  watchSwitchLatency(presence, env, (kind, seconds) => recorded.push([kind, seconds]));
  return { presence, recorded };
}

describe("watchSwitchLatency", () => {
  it("measures a switch to a drawn page to its first shown frame", () => {
    // Arrange
    const env = new FakeEnv();
    env.isDrawn = true;
    const { presence, recorded } = watched(env);

    // Act
    presence.setShown(true, 1_000);
    env.wall = 1_040;
    env.runFrames();
    env.wall = 1_060;
    env.runFrames();

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.SWITCH_REPEAT, 0.06]]);
  });

  it("measures a first switch to a page not yet drawn to its conversation drawn", () => {
    // Arrange
    const env = new FakeEnv();
    const { presence, recorded } = watched(env);

    // Act
    presence.setShown(true, 1_000);
    env.wall = 1_700;
    env.draw();

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.SWITCH_FIRST, 0.7]]);
  });

  it("measures nothing for a first switch hidden again before the page drew", () => {
    // Arrange
    const env = new FakeEnv();
    const { presence, recorded } = watched(env);
    presence.setShown(true, 1_000);

    // Act
    presence.setShown(false);
    env.draw();

    // Assert
    expect(recorded).toEqual([]);
  });

  it("measures nothing for a show no switch made", () => {
    // Arrange
    const env = new FakeEnv();
    env.isDrawn = true;
    const { presence, recorded } = watched(env);

    // Act
    presence.setShown(true);
    env.runFrames();
    env.runFrames();

    // Assert
    expect(recorded).toEqual([]);
  });

  it("reports nothing, and warns, when the wall clock stepped back across the switch", async () => {
    // Arrange
    const capture = captureLogRecords();
    const env = new FakeEnv();
    env.isDrawn = true;
    const { presence, recorded } = watched(env);

    // Act
    presence.setShown(true, 5_000);
    env.wall = 4_000;
    env.runFrames();
    env.runFrames();

    // Assert
    expect(recorded).toEqual([]);
    const record = await forwardedRecord(capture, "latency.switch-clock-stepped");
    expect(record.level.case).toBe("warn");
  });
});

describe("watchPageBoot", () => {
  it("measures the boot to the conversation drawn on the monotonic clock", () => {
    // Arrange
    const env = new FakeEnv();
    env.mono = 100;
    const recorded: Recorded[] = [];
    watchPageBoot(100, env, (kind, seconds) => recorded.push([kind, seconds]));

    // Act
    env.mono = 450;
    env.draw();

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.PAGE_BOOT, 0.35]]);
  });

  it("measures at once for a page already drawn", () => {
    // Arrange
    const env = new FakeEnv();
    env.isDrawn = true;
    env.mono = 300;
    const recorded: Recorded[] = [];

    // Act
    watchPageBoot(100, env, (kind, seconds) => recorded.push([kind, seconds]));

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.PAGE_BOOT, 0.2]]);
  });
});
