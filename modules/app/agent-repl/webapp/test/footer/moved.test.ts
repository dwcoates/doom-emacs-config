// @vitest-environment jsdom
/**
 * moved — the workspace's move, held for the footer and drawn as its
 * activity line.
 */
import { describe, expect, it } from "vitest";
import {
  MOVED_ARM,
  MOVED_TIER,
  createWorkspaceMove,
  drawMovedActivity,
  overlayMoved,
} from "../../src/footer/moved.js";
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

describe("createWorkspaceMove", () => {
  it("holds no move until one is recorded", () => {
    // Act / Assert
    expect(createWorkspaceMove().current()).toBeNull();
  });

  it("holds the successor's address once the move is recorded", () => {
    // Arrange
    const move = createWorkspaceMove();
    // Act
    move.moved("127.0.0.1:8123");
    // Assert
    expect(move.current()).toBe("127.0.0.1:8123");
  });

  it("tells every subscriber of the move", () => {
    // Arrange
    const move = createWorkspaceMove();
    const seen: Array<string | null> = [];
    move.subscribe(() => seen.push(move.current()));
    // Act
    move.moved("127.0.0.1:8123");
    // Assert
    expect(seen).toEqual(["127.0.0.1:8123"]);
  });

  it("tells no subscriber that unsubscribed", () => {
    // Arrange
    const move = createWorkspaceMove();
    let told = 0;
    const unsubscribe = move.subscribe(() => (told += 1));
    unsubscribe();
    // Act
    move.moved("127.0.0.1:8123");
    // Assert
    expect(told).toBe(0);
  });

  it("records the move at info with the address", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    createWorkspaceMove().moved("127.0.0.1:8123");
    // Assert
    const record = await forwardedRecord(capture, "footer.moved");
    expect([record.level.case, record.message]).toEqual(["info", "the workspace moved to 127.0.0.1:8123; drawing it as the footer's activity line"]);
  });
});

describe("drawMovedActivity", () => {
  it("names the successor's address, the one fact the reader may need", () => {
    // Act / Assert
    expect(drawMovedActivity("127.0.0.1:8123").textContent).toBe("workspace moved to 127.0.0.1:8123");
  });

  it("is an activity cell of its own tier and kind", () => {
    // Act
    const cell = drawMovedActivity("127.0.0.1:8123");
    // Assert
    expect([cell.classList.contains("footer-activity"), cell.getAttribute("data-tier"), cell.getAttribute("data-arm")]).toEqual([
      true,
      MOVED_TIER,
      MOVED_ARM,
    ]);
  });

  it("carries the address as a hook", () => {
    // Act / Assert
    expect(drawMovedActivity("127.0.0.1:8123").getAttribute("data-moved")).toBe("127.0.0.1:8123");
  });

  it("holds the whole line as its hover, since the cell ellipsizes", () => {
    // Act / Assert
    expect(drawMovedActivity("127.0.0.1:8123").title).toBe("workspace moved to 127.0.0.1:8123");
  });
});

describe("overlayMoved", () => {
  it("puts the move in the strip's activity cell, in its place", () => {
    // Arrange
    const strip = stripOf("footer-status", "pfooter-cell pfooter-grow footer-activity", "footer-clock");
    // Act
    overlayMoved(strip, "127.0.0.1:8123");
    // Assert
    expect([...strip.children].map((c) => c.getAttribute("data-arm") ?? c.className)).toEqual([
      "footer-status",
      MOVED_ARM,
      "footer-clock",
    ]);
  });

  it("refuses a strip with no activity cell, recording it at error with the address", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => overlayMoved(stripOf("footer-status"), "127.0.0.1:8123")).toThrow(/activity cells, not one/);
    // Assert
    const record = await forwardedRecord(capture, "footer.moved.no-cell");
    expect([record.level.case, record.context?.address, record.context?.cells]).toEqual(["error", "127.0.0.1:8123", 0]);
  });
});
