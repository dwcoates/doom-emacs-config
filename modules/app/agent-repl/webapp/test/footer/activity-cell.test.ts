// @vitest-environment jsdom
/**
 * activity-cell — the strip's one activity cell, and the replacement every
 * line standing in for the daemon's goes through.
 */
import { describe, expect, it } from "vitest";
import { ACTIVITY_CELL_CLASS, activityCell, replaceActivityCell } from "../../src/footer/activity-cell.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** A strip holding the given cells' classes, in order. */
function stripOf(...classes: string[]): HTMLElement {
  const strip = document.createElement("div");
  for (const className of classes) {
    const cell = document.createElement("div");
    cell.className = className;
    strip.append(cell);
  }
  return strip;
}

/** A cell standing in for the activity, marked so a test can find it. */
function standIn(): HTMLElement {
  const cell = activityCell();
  cell.setAttribute("data-arm", "stand-in");
  return cell;
}

describe("activityCell", () => {
  it("is an elastic cell wearing the activity class", () => {
    // Act
    const cell = activityCell();
    // Assert
    expect(cell.className).toBe(`pfooter-cell pfooter-grow ${ACTIVITY_CELL_CLASS}`);
  });

  it("is empty", () => {
    // Act / Assert
    expect(activityCell().childNodes.length).toBe(0);
  });
});

describe("replaceActivityCell", () => {
  it("puts the cell in the strip's activity cell's place", () => {
    // Arrange
    const strip = stripOf("footer-status", ACTIVITY_CELL_CLASS, "footer-clock");
    // Act
    replaceActivityCell(strip, standIn(), "test.no-cell", {});
    // Assert
    expect([...strip.children].map((c) => c.getAttribute("data-arm") ?? c.className)).toEqual([
      "footer-status",
      "stand-in",
      "footer-clock",
    ]);
  });

  it.each([
    ["no activity cell", stripOf("footer-status")],
    ["two activity cells", stripOf(ACTIVITY_CELL_CLASS, ACTIVITY_CELL_CLASS)],
  ])("refuses a strip with %s", (_name, strip) => {
    // Act / Assert
    expect(() => replaceActivityCell(strip, standIn(), "test.no-cell", {})).toThrow(/activity cells, not one/);
  });

  it("records the refusal at error under the caller's operation, with its context and the count", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => replaceActivityCell(stripOf("footer-status"), standIn(), "test.no-cell", { line: "x" })).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "test.no-cell");
    expect([record.level.case, record.context?.cells, record.context?.line]).toEqual(["error", 0, "x"]);
  });

  it("leaves the strip as it was when it refuses", () => {
    // Arrange
    const strip = stripOf("footer-status");
    const before = strip.innerHTML;
    // Act
    expect(() => replaceActivityCell(strip, standIn(), "test.no-cell", {})).toThrow();
    // Assert
    expect(strip.innerHTML).toBe(before);
  });
});
