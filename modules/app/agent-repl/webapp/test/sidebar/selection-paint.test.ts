// @vitest-environment jsdom
// When the sidebar first painted a select's selection. See src/sidebar/selection-paint.ts.
import { create } from "@bufbuild/protobuf";
import { beforeEach, describe, expect, it } from "vitest";

import { ReportedLatencyKind } from "../../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { RosterCurrentWorkspaceSchema, type RosterCurrentWorkspace } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { WorkspaceRefSchema, type WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { frameQueue, type FrameQueue } from "../frame-queue.js";
import { captureLogRecords, forwardedRecords, type LogCapture } from "../log-capture.js";
import { createSelectionPaint, type SelectionPaint } from "../../src/sidebar/selection-paint.js";

const W1 = create(WorkspaceRefSchema, { id: "w1", dir: "/w/1" });

let capture: LogCapture;
let wall: number;
let frames: FrameQueue;
let recorded: Array<[string, ReportedLatencyKind, number]>;
let paint: SelectionPaint;

/** The roster's selection of WORKSPACE (W1; null for none) under SELECTION, begun at STARTED when given. */
function current(selection: string, started?: number, workspace: WorkspaceRef | null = W1): RosterCurrentWorkspace {
  return create(RosterCurrentWorkspaceSchema, {
    ...(workspace === null ? {} : { workspace }),
    selectionId: selection,
    ...(started === undefined ? {} : { switchStartedAtMs: BigInt(started) }),
  });
}

/** Every `sidebar.selection-painted` record's level and context. */
async function paints(): Promise<Array<{ level: string; context: Record<string, unknown> }>> {
  return (await forwardedRecords(capture))
    .filter((record) => record.operation === "sidebar.selection-painted")
    .map((record) => {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars -- the page's own connection rides every record; the paint's context is the rest
      const { connection_id, ...context } = record.context ?? {};
      return { level: record.level.case ?? "", context };
    });
}

beforeEach(() => {
  capture = captureLogRecords("debug");
  wall = 1_700_000_000_000;
  frames = frameQueue();
  recorded = [];
  paint = createSelectionPaint({ wallNow: () => wall, frame: frames.frame }, (workspace, kind, seconds) =>
    recorded.push([workspace.id, kind, seconds]),
  );
});

describe("createSelectionPaint", () => {
  it("records a select's first paint on the frame after its draw, with the switch's latency", async () => {
    // Arrange
    paint.drawn(current("sel-1", wall - 60));
    wall += 15;

    // Act
    frames.run();

    // Assert
    expect(await paints()).toEqual([
      { level: "info", context: { workspace: "w1", selection_id: "sel-1", ms_since_switch: 75 } },
    ]);
    expect(recorded).toEqual([["w1", ReportedLatencyKind.SWITCH_SIDEBAR, 0.075]]);
  });

  it("records nothing before the frame after the draw has run", async () => {
    // Act
    paint.drawn(current("sel-1", wall - 60));

    // Assert
    expect(await paints()).toEqual([]);
    expect(recorded).toEqual([]);
  });

  it("records each select once, however many rosters redraw it", async () => {
    // Arrange
    paint.drawn(current("sel-1", wall));
    frames.drain();

    // Act
    paint.drawn(current("sel-1", wall));
    frames.drain();

    // Assert
    expect(await paints()).toHaveLength(1);
    expect(recorded).toHaveLength(1);
  });

  it("records a new select of the same workspace again", async () => {
    // Arrange
    paint.drawn(current("sel-1", wall));
    frames.drain();

    // Act
    paint.drawn(current("sel-2", wall));
    frames.drain();

    // Assert
    expect((await paints()).map((record) => record.context.selection_id)).toEqual(["sel-1", "sel-2"]);
  });

  it("records nothing for a selection no select named", async () => {
    // Act
    paint.drawn(current(""));
    paint.drawn(undefined);
    frames.drain();

    // Assert
    expect(await paints()).toEqual([]);
    expect(frames.asked()).toBe(0);
  });

  it("records the paint with no latency for a select that carried no switch instant", async () => {
    // Act
    paint.drawn(current("sel-1"));
    frames.drain();

    // Assert
    expect(await paints()).toEqual([{ level: "info", context: { workspace: "w1", selection_id: "sel-1" } }]);
    expect(recorded).toEqual([]);
  });

  it("measures a paint but reports nothing when no reporter was given", async () => {
    // Arrange
    paint = createSelectionPaint({ wallNow: () => wall, frame: frames.frame });

    // Act
    paint.drawn(current("sel-1", wall - 10));
    frames.drain();

    // Assert
    expect(await paints()).toHaveLength(1);
  });

  it("reports no latency when the wall clock stepped back past the switch", async () => {
    // Act
    paint.drawn(current("sel-1", wall + 500));
    frames.drain();

    // Assert
    expect(recorded).toEqual([]);
    expect((await forwardedRecords(capture)).filter((record) => record.operation === "latency.clock-stepped")).toHaveLength(1);
  });

  it("throws on a selection that names no workspace", () => {
    // Act, Assert
    expect(() => paint.drawn(current("sel-1", wall, null))).toThrow(/names no workspace/);
  });
});
