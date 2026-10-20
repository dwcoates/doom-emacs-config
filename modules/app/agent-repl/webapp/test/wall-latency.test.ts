// A latency that started in another process and ended in the page, on the
// host's wall clock. See src/wall-latency.ts.
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { recordWallLatency } from "../src/wall-latency.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

type Recorded = [ReportedLatencyKind, number];

describe("recordWallLatency", () => {
  it.each([
    ["a positive span", 1_000, 1_250, 0.25],
    ["a zero span", 1_000, 1_000, 0],
  ])("records %s in seconds", (_name, started, ended, seconds) => {
    // Arrange
    const recorded: Recorded[] = [];

    // Act
    recordWallLatency(ReportedLatencyKind.SWITCH_REPEAT, started, ended, (kind, s) => recorded.push([kind, s]));

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.SWITCH_REPEAT, seconds]]);
  });

  it("reports nothing, and warns naming the kind, when the clock stepped back", async () => {
    // Arrange
    const capture = captureLogRecords();
    const recorded: Recorded[] = [];

    // Act
    recordWallLatency(ReportedLatencyKind.PROMPT_ECHO, 5_000, 4_000, (kind, s) => recorded.push([kind, s]));

    // Assert
    expect(recorded).toEqual([]);
    const record = await forwardedRecord(capture, "latency.clock-stepped");
    expect(record.level.case).toBe("warn");
    expect(JSON.stringify(record)).toContain("PROMPT_ECHO");
  });
});

describe("the wall-clock latency sites", () => {
  // EVERY latency that starts in another process ends through the one helper,
  // so none can drift on the clock-step rule or the unit.
  it.each(["src/switch-latency.ts"])("%s measures through recordWallLatency", (file) => {
    const source = readFileSync(new URL(`../${file}`, import.meta.url), "utf8");
    expect(source).toContain("recordWallLatency(");
    expect(source).not.toMatch(/wallNow\(\)\s*-/);
  });
});
