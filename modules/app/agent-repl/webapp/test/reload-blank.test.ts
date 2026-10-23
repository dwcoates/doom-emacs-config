// @vitest-environment jsdom
// How long a reload left this page blank in front of the user. See
// src/reload-blank.ts.
import { afterEach, describe, expect, it } from "vitest";

import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { createPresence, type PresenceSource } from "../src/presence.js";
import { watchReloadBlank } from "../src/reload-blank.js";
import { FakeLatencyEnv } from "./fake-latency-env.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

type Recorded = [ReportedLatencyKind, number];

let presence: PresenceSource | null = null;

afterEach(() => {
  presence?.dispose();
  presence = null;
});

function watched(env: FakeLatencyEnv): { presence: PresenceSource; recorded: Recorded[] } {
  presence = createPresence(document, true);
  const recorded: Recorded[] = [];
  watchReloadBlank(presence, env, (kind, seconds) => recorded.push([kind, seconds]));
  return { presence, recorded };
}

describe("watchReloadBlank", () => {
  it("measures a reloaded page shown when it draws from the reload's start", () => {
    // Arrange
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setReloadStartedAt(1_000);
    presence.setShown(true);

    // Act
    env.wall = 3_500;
    env.draw();

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.RELOAD_BLANK, 2.5]]);
  });

  it("measures nothing for a reloaded page held hidden when it draws", () => {
    // Arrange
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setReloadStartedAt(1_000);

    // Act
    env.wall = 3_500;
    env.draw();
    presence.setShown(true);

    // Assert
    expect(recorded).toEqual([]);
  });

  it("measures nothing for a page no reload loaded", () => {
    // Arrange
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setShown(true);

    // Act
    env.wall = 3_500;
    env.draw();

    // Assert
    expect(recorded).toEqual([]);
  });

  it("measures once, at the first draw", () => {
    // Arrange
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setReloadStartedAt(1_000);
    presence.setShown(true);
    env.wall = 1_200;
    env.draw();

    // Act
    env.wall = 9_000;
    env.draw();

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.RELOAD_BLANK, 0.2]]);
  });

  it("records a stepped clock at warn and reports nothing", async () => {
    // Arrange
    const capture = captureLogRecords();
    const env = new FakeLatencyEnv();
    const { presence, recorded } = watched(env);
    presence.setReloadStartedAt(5_000);
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
