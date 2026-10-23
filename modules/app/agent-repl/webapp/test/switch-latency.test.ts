// @vitest-environment jsdom
// How long a switch to this page, and this page's boot, took. See
// src/switch-latency.ts.
import { afterEach, describe, expect, it } from "vitest";

import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { createPresence, type PresenceSource } from "../src/presence.js";
import { watchPageBoot, watchSwitchLatency } from "../src/switch-latency.js";
import { FakeLatencyEnv as FakeEnv } from "./fake-latency-env.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

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
    const record = await forwardedRecord(capture, "latency.clock-stepped");
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
