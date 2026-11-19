// @vitest-environment jsdom
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { RosterRowViewDwellSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  VIEW_DWELL_CLASS,
  VIEW_DWELL_DELAY_PROPERTY,
  VIEW_DWELL_PROPERTY,
  applyViewDwell,
  drawRosterRowViewDwell,
  viewDwellElapsedMs,
} from "../../src/sidebar/view-dwell.js";

describe("drawRosterRowViewDwell", () => {
  it("reads the dwell's start and length", () => {
    // Arrange
    const wire = create(RosterRowViewDwellSchema, { startedAtMs: 9_000n, lengthMs: 5_000n });

    // Act
    const got = drawRosterRowViewDwell(wire, "R.view_dwell");

    // Assert
    expect(got).toEqual({ startedAtMs: 9_000, lengthMs: 5_000 });
  });

  it.each([
    { name: "refuses a dwell of no length", lengthMs: 0n },
    { name: "refuses a dwell of negative length", lengthMs: -1n },
  ])("$name", ({ lengthMs }) => {
    // Arrange
    const wire = create(RosterRowViewDwellSchema, { startedAtMs: 9_000n, lengthMs });

    // Act, Assert
    expect(() => drawRosterRowViewDwell(wire, "R.view_dwell")).toThrow(MalformedView);
  });
});

describe("viewDwellElapsedMs", () => {
  it.each([
    { name: "reads the time dwelt so far", now: 12_000, want: 2_000 },
    { name: "never reads a dwell that starts later as negative time", now: 9_000, want: 0 },
    { name: "never reads past the dwell's end", now: 30_000, want: 5_000 },
  ])("$name", ({ now, want }) => {
    // Arrange / Act
    const got = viewDwellElapsedMs({ startedAtMs: 10_000, lengthMs: 5_000 }, now);

    // Assert
    expect(got).toBe(want);
  });
});

describe("applyViewDwell", () => {
  it("marks the name and places it in the fade", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyViewDwell(label, { startedAtMs: 10_000, lengthMs: 5_000 }, 11_250);

    // Assert
    expect([
      label.classList.contains(VIEW_DWELL_CLASS),
      label.style.getPropertyValue(VIEW_DWELL_PROPERTY),
      label.style.getPropertyValue(VIEW_DWELL_DELAY_PROPERTY),
    ]).toEqual([true, "5000ms", "-1250ms"]);
  });
});
