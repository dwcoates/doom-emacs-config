// @vitest-environment jsdom
// How long this page's main thread stalled while someone saw it. See
// src/page-stall.ts.
import { afterEach, describe, expect, it } from "vitest";

import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import {
  PAGE_STALL_FLOOR_MS,
  PAGE_STALL_INTERVAL_MS,
  watchPageStall,
  type PageStallEnv,
} from "../src/page-stall.js";
import { createPresence, type PresenceSource } from "../src/presence.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

/** A clock and one interval timer a test drives. */
class FakeStallEnv implements PageStallEnv {
  clock = 0;
  tick: (() => void) | null = null;
  intervals: number[] = [];

  readonly now = (): number => this.clock;
  readonly every = (fn: () => void, ms: number): (() => void) => {
    this.tick = fn;
    this.intervals.push(ms);
    return () => {
      if (this.tick === fn) this.tick = null;
    };
  };

  /** Advance the clock by MS and run the timer, if one runs. */
  runAfter(ms: number): void {
    this.clock += ms;
    this.tick?.();
  }
}

type Recorded = [ReportedLatencyKind, number];

let presence: PresenceSource | null = null;

afterEach(() => {
  presence?.dispose();
  presence = null;
});

function watched(env: FakeStallEnv, embedded = true): { presence: PresenceSource; recorded: Recorded[] } {
  presence = createPresence(document, embedded);
  const recorded: Recorded[] = [];
  watchPageStall(presence, env, (kind, seconds) => recorded.push([kind, seconds]));
  return { presence, recorded };
}

describe("watchPageStall", () => {
  it("samples nothing while the page is not shown", () => {
    // Arrange
    const env = new FakeStallEnv();

    // Act
    watched(env);

    // Assert
    expect(env.tick).toBeNull();
  });

  it("starts sampling at the steady interval once the page is shown", () => {
    // Arrange
    const env = new FakeStallEnv();
    const { presence } = watched(env);

    // Act
    presence.setShown(true);

    // Assert
    expect(env.intervals).toEqual([PAGE_STALL_INTERVAL_MS]);
    expect(env.tick).not.toBeNull();
  });

  it("samples a page in no host at once, since it is shown by definition", () => {
    // Arrange
    const env = new FakeStallEnv();

    // Act
    watched(env, false);

    // Assert
    expect(env.tick).not.toBeNull();
  });

  it("reports nothing for a run on time or late by less than the floor", () => {
    // Arrange
    const env = new FakeStallEnv();
    const { presence, recorded } = watched(env);
    presence.setShown(true);

    // Act
    env.runAfter(PAGE_STALL_INTERVAL_MS);
    env.runAfter(PAGE_STALL_INTERVAL_MS + PAGE_STALL_FLOOR_MS - 1);

    // Assert
    expect(recorded).toEqual([]);
  });

  it("reports a run late by the floor or more as a stall of the lateness", () => {
    // Arrange
    const env = new FakeStallEnv();
    const { presence, recorded } = watched(env);
    presence.setShown(true);

    // Act
    env.runAfter(PAGE_STALL_INTERVAL_MS + PAGE_STALL_FLOOR_MS);
    env.runAfter(PAGE_STALL_INTERVAL_MS + 750);

    // Assert
    expect(recorded).toEqual([
      [ReportedLatencyKind.PAGE_STALL, PAGE_STALL_FLOOR_MS / 1000],
      [ReportedLatencyKind.PAGE_STALL, 0.75],
    ]);
  });

  it("stops sampling when the page is hidden and starts afresh when shown again", () => {
    // Arrange
    const env = new FakeStallEnv();
    const { presence, recorded } = watched(env);
    presence.setShown(true);

    // Act
    presence.setShown(false);
    const stopped = env.tick === null;
    env.clock += 30_000;
    presence.setShown(true);
    env.runAfter(PAGE_STALL_INTERVAL_MS);

    // Assert
    expect(stopped).toBe(true);
    expect(recorded).toEqual([]);
  });

  it("records a gap past any stall at info and reports nothing", async () => {
    // Arrange
    const capture = captureLogRecords();
    const env = new FakeStallEnv();
    const { presence, recorded } = watched(env);
    presence.setShown(true);

    // Act
    env.runAfter(3_600_000);

    // Assert
    expect(recorded).toEqual([]);
    const record = await forwardedRecord(capture, "page.stall-unclassified");
    expect(record.level.case).toBe("info");
  });
});
