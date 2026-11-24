// @vitest-environment jsdom
// What one roster push costs the rail. See src/sidebar/draw-timing.ts.
import { beforeEach, describe, expect, it } from "vitest";

import { frameQueue, type FrameQueue } from "../frame-queue.js";
import { captureLogRecords, type LogCapture } from "../log-capture.js";
import { ROSTER_DRAW_SLOW_MS, timeRosterDraw, type DrawTimingEnv, type RosterDrawCounts, type RosterDrawOutcome } from "../../src/sidebar/draw-timing.js";

let capture: LogCapture;
/** One push, and no local cause. */
const PUSH = { pushes: 1, local: [] };
/** What a draw that changed one row answers. */
const ONE_ROW: RosterDrawCounts = { changed_streams: ["workspaces"], sections_drawn: 0, rows_drawn: 1, rows_removed: 0, rows_moved: 0, repainted: 0 };
/** Run STEP, then answer that the draw changed one row. */
const drew = (step: () => void) => (): RosterDrawOutcome => {
  step();
  return { drew: ONE_ROW };
};
let clock: number;
let frames: FrameQueue;
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
    .map((record) => ({ level: record.level.case ?? "", context: record.context ?? {} }))
    .filter((record) => record.level === level);
}

beforeEach(() => {
  capture = captureLogRecords("debug");
  clock = 0;
  frames = frameQueue();
  env = { now: () => clock, frame: frames.frame };
});

describe("timeRosterDraw", () => {
  it("records a draw that reaches a frame's budget at INFO, with the rail's size", async () => {
    // Arrange
    const body = rail(3);

    // Act
    timeRosterDraw(body, "webhost", PUSH, drew(() => void (clock += 10)), env);
    clock += ROSTER_DRAW_SLOW_MS - 10;
    frames.run();

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
    timeRosterDraw(body, "page", PUSH, drew(() => void (clock += 5)), env);
    clock += ROSTER_DRAW_SLOW_MS - 6;
    frames.run();

    // Assert
    expect(await drawRecords("info")).toEqual([]);
    expect(await drawRecords("debug")).toHaveLength(1);
  });

  it("runs the draw before the record waits for the next frame", () => {
    // Arrange
    let ran = false;

    // Act
    timeRosterDraw(rail(0), "page", PUSH, () => {
      ran = true;
      return { drew: ONE_ROW };
    }, env);

    // Assert
    expect(ran).toBe(true);
    expect(frames.pending()).toBe(1);
  });

  it("records each phase the draw marks as the time since the mark before it", async () => {
    // Arrange
    const steps = { build: 30, mount: 4, layout: 20, hooks: 6 } as const;

    // Act
    timeRosterDraw(
      rail(2),
      "webhost",
      PUSH,
      (mark) => {
        for (const [phase, ms] of Object.entries(steps)) {
          clock += ms;
          mark(phase as keyof typeof steps);
        }
        return { drew: ONE_ROW };
      },
      env,
    );
    frames.run();

    // Assert
    const records = await drawRecords("info");
    expect(records[0]?.context).toEqual(
      expect.objectContaining({ build_ms: 30, mount_ms: 4, layout_ms: 20, hooks_ms: 6, draw_ms: 60 }),
    );
  });

  it("names the draw's cause and what it changed", async () => {
    // Arrange
    const cause = { pushes: 3, local: ["merged_pages"] };

    // Act
    timeRosterDraw(rail(1), "webhost", cause, drew(() => void (clock += ROSTER_DRAW_SLOW_MS)), env);
    frames.run();

    // Assert
    const records = await drawRecords("info");
    expect(records[0]?.context).toEqual(
      expect.objectContaining({
        pushes: 3,
        local_causes: ["merged_pages"],
        changed_streams: ["workspaces"],
        sections_drawn: 0,
        rows_drawn: 1,
        rows_removed: 0,
        rows_moved: 0,
        repainted: 0,
      }),
    );
  });

  it("states a whole draw as such, with no counts", async () => {
    // Act
    timeRosterDraw(rail(1), "webhost", PUSH, () => {
      clock += ROSTER_DRAW_SLOW_MS;
      return { drew: "whole" };
    }, env);
    frames.run();

    // Assert
    const context = (await drawRecords("info"))[0]?.context;
    expect([context?.whole, context?.rows_drawn]).toEqual([true, undefined]);
  });

  it("records a draw that changed nothing at DEBUG as skipped, waiting for no frame", async () => {
    // Act
    timeRosterDraw(rail(4), "webhost", PUSH, () => {
      clock += ROSTER_DRAW_SLOW_MS * 2;
      return "nothing";
    }, env);

    // Assert
    expect(frames.pending()).toBe(0);
    capture.logger.flush();
    await Promise.resolve();
    const records = capture.sent.filter((record) => record.operation.startsWith("sidebar.roster-draw"));
    expect(records.map((record) => [record.operation, record.level.case, record.context?.pushes])).toEqual([
      ["sidebar.roster-draw-skipped", "debug", 1],
    ]);
  });
});
