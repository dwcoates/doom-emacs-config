// @vitest-environment jsdom
// What one roster push costs the rail. See src/sidebar/draw-timing.ts.
import { beforeEach, describe, expect, it } from "vitest";

import { captureLogRecords, type LogCapture } from "../log-capture.js";
import { ROSTER_DRAW_SLOW_MS, timeRosterDraw, type DrawTimingEnv } from "../../src/sidebar/draw-timing.js";

let capture: LogCapture;
let clock: number;
let frames: (() => void)[];
let env: DrawTimingEnv;

/** A rail holding ROWS rows. */
function rail(rows: number): HTMLElement {
  const body = document.createElement("div");
  for (let i = 0; i < rows; i++) {
    const row = document.createElement("div");
    row.className = "row";
    body.append(row);
  }
  return body;
}

async function drawRecords(level: "info" | "debug"): Promise<{ level: string; context: Record<string, unknown> }[]> {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent
    .filter((record) => record.operation === "sidebar.roster-draw")
    .map((record) => ({ level: record.level.case ?? "", context: (record.context ?? {}) as Record<string, unknown> }))
    .filter((record) => record.level === level);
}

beforeEach(() => {
  capture = captureLogRecords("debug");
  clock = 0;
  frames = [];
  env = { now: () => clock, frame: (callback) => void frames.push(callback) };
});

describe("timeRosterDraw", () => {
  it("records a draw that reaches a frame's budget at INFO, with the rail's size", async () => {
    // Arrange
    const body = rail(3);

    // Act
    timeRosterDraw(body, "webhost", () => void (clock += 10), env);
    clock += ROSTER_DRAW_SLOW_MS - 10;
    frames.shift()?.();

    // Assert
    const records = await drawRecords("info");
    expect(records.map((record) => record.context)).toEqual([
      expect.objectContaining({ placement: "webhost", draw_ms: 10, frame_ms: 6, total_ms: 16, rows: 3 }),
    ]);
  });

  it("records a draw under a frame's budget at DEBUG only", async () => {
    // Arrange
    const body = rail(1);

    // Act
    timeRosterDraw(body, "page", () => void (clock += 5), env);
    clock += ROSTER_DRAW_SLOW_MS - 6;
    frames.shift()?.();

    // Assert
    expect(await drawRecords("info")).toEqual([]);
    expect(await drawRecords("debug")).toHaveLength(1);
  });

  it("runs the draw before the record waits for the next frame", () => {
    // Arrange
    let drew = false;

    // Act
    timeRosterDraw(rail(0), "page", () => void (drew = true), env);

    // Assert
    expect(drew).toBe(true);
    expect(frames).toHaveLength(1);
  });
});
