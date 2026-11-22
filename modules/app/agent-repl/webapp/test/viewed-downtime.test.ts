// @vitest-environment jsdom
// How long the viewed workspace could not be used during a daemon change. See
// src/viewed-downtime.ts.
import { afterEach, describe, expect, it } from "vitest";

import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { createPresence, type PresenceSource } from "../src/presence.js";
import { watchViewedDowntime } from "../src/viewed-downtime.js";
import { FakeLatencyEnv } from "./fake-latency-env.js";
import { captureLogRecords, forwardedRecord, forwardedRecords } from "./log-capture.js";

type Recorded = [ReportedLatencyKind, number];

let presence: PresenceSource | null = null;

afterEach(() => {
  presence?.dispose();
  presence = null;
});

function watched(env: FakeLatencyEnv): { presence: PresenceSource; recorded: Recorded[] } {
  presence = createPresence(document, true);
  const recorded: Recorded[] = [];
  watchViewedDowntime(presence, env, (kind, seconds) => recorded.push([kind, seconds]));
  return { presence, recorded };
}

describe("watchViewedDowntime", () => {
  it("measures a page shown when it draws from the daemon change's start", () => {
    // Arrange
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setDowntimeStartedAt(1_000);
    presence.setShown(true);

    // Act
    env.wall = 9_400;
    env.draw();

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.VIEWED_DOWNTIME, 8.4]]);
  });

  it("measures nothing for a page held hidden when it draws", () => {
    // Arrange
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setDowntimeStartedAt(1_000);

    // Act
    env.wall = 9_400;
    env.draw();

    // Assert
    expect(recorded).toEqual([]);
  });

  it("measures nothing for a page no daemon change preceded", () => {
    // Arrange
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setShown(true);

    // Act
    env.wall = 9_400;
    env.draw();

    // Assert
    expect(recorded).toEqual([]);
  });

  it("measures once, at the first draw", () => {
    // Arrange
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setDowntimeStartedAt(1_000);
    presence.setShown(true);
    env.wall = 3_000;
    env.draw();

    // Act
    env.wall = 90_000;
    env.draw();

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.VIEWED_DOWNTIME, 2]]);
  });

  it.each([
    ["the stream was slower", 1_000, 7_000, 8_000, "stream", 6, 1],
    ["the page was slower", 1_000, 2_000, 8_000, "page", 1, 6],
  ])("records which part was slower when %s", async (_name, start, reload, end, slower, streamSeconds, pageSeconds) => {
    // Arrange
    const capture = captureLogRecords();
    const env = new FakeLatencyEnv();
    const { presence } = watched(env);
    presence.setDowntimeStartedAt(start);
    presence.setReloadStartedAt(reload);
    presence.setShown(true);

    // Act
    env.wall = end;
    env.draw();

    // Assert
    const record = await forwardedRecord(capture, "page.viewed-downtime-parts");
    expect(record.level.case).toBe("info");
    expect(record.context).toMatchObject({ streamSeconds, pageSeconds, slower });
  });

  it("records no parts when the page's reload began before the downtime did", async () => {
    // Arrange
    const capture = captureLogRecords();
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setDowntimeStartedAt(5_000);
    presence.setReloadStartedAt(4_000);
    presence.setShown(true);

    // Act
    env.wall = 8_000;
    env.draw();

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.VIEWED_DOWNTIME, 3]]);
    const records = await forwardedRecords(capture);
    expect(records.some((r) => r.operation === "page.viewed-downtime-parts")).toBe(false);
  });

  it("records a stepped clock at warn and reports nothing", async () => {
    // Arrange
    const capture = captureLogRecords();
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setDowntimeStartedAt(5_000);
    presence.setShown(true);

    // Act
    env.wall = 4_000;
    env.draw();

    // Assert
    expect(recorded).toEqual([]);
    const record = await forwardedRecord(capture, "latency.clock-stepped");
    expect(record.level.case).toBe("warn");
  });
});
