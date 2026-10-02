// @vitest-environment jsdom
// The page's presence log: hide/show, focus/blur, coalesced resizes, and the
// repaint measured after each show or resize. See src/page-presence.ts.
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { captureLogRecords, type LogCapture } from "./log-capture.js";
import { installPagePresenceLog, type PagePresence } from "../src/page-presence.js";

let capture: LogCapture;
let presence: PagePresence;
let clock: number;
let frames: (() => void)[];

/** Run every queued animation frame, including ones queued while running. */
function runFrames(): void {
  while (frames.length > 0) frames.shift()?.();
}

function setVisibility(state: "visible" | "hidden"): void {
  Object.defineProperty(document, "visibilityState", { configurable: true, get: () => state });
  document.dispatchEvent(new Event("visibilitychange"));
}

function setSize(width: number, height: number): void {
  Object.defineProperty(window, "innerWidth", { configurable: true, value: width });
  Object.defineProperty(window, "innerHeight", { configurable: true, value: height });
}

async function operations(): Promise<string[]> {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent.map((record) => record.operation);
}

beforeEach(() => {
  capture = captureLogRecords();
  clock = 0;
  frames = [];
  setSize(800, 600);
  presence = installPagePresenceLog({
    doc: document,
    win: window,
    now: () => clock,
    frame: (callback) => {
      frames.push(callback);
    },
  });
});

afterEach(() => {
  presence.stop();
});

describe("page presence log", () => {
  it("records the page becoming hidden", async () => {
    // Arrange / Act
    setVisibility("hidden");

    // Assert
    expect(await operations()).toEqual(["page.visibility"]);
  });

  it("measures the repaint after the page becomes visible", async () => {
    // Arrange
    setVisibility("visible");
    clock = 120;

    // Act
    runFrames();

    // Assert
    expect(await operations()).toEqual(["page.visibility", "page.repaint"]);
    const repaint = capture.sent.find((record) => record.operation === "page.repaint");
    expect(repaint?.context?.paint_ms).toBe(120);
    expect(repaint?.context?.cause).toBe("visible");
  });

  it("records focus and blur", async () => {
    // Arrange / Act
    window.dispatchEvent(new Event("focus"));
    window.dispatchEvent(new Event("blur"));

    // Assert
    expect(await operations()).toEqual(["page.focus", "page.blur"]);
  });

  it("coalesces the resizes of one frame into one record with the settled size", async () => {
    // Arrange
    setSize(400, 300);
    window.dispatchEvent(new Event("resize"));
    setSize(500, 300);
    window.dispatchEvent(new Event("resize"));

    // Act
    runFrames();

    // Assert
    await operations();
    const resizes = capture.sent.filter((record) => record.operation === "page.resize");
    expect(resizes).toHaveLength(1);
    expect(resizes[0]?.context?.width).toBe(500);
    expect(resizes[0]?.context?.before_width).toBe(800);
  });

  it("records nothing for a resize event that leaves the size unchanged", async () => {
    // Arrange / Act
    window.dispatchEvent(new Event("resize"));
    runFrames();

    // Assert
    expect(await operations()).toEqual([]);
  });

  it("counts the page's feed rows in the repaint record", async () => {
    // Arrange
    const row = document.createElement("div");
    row.setAttribute("data-feed-row", "r1");
    document.body.append(row);
    setVisibility("visible");

    // Act
    runFrames();

    // Assert
    await operations();
    const repaint = capture.sent.find((record) => record.operation === "page.repaint");
    expect(repaint?.context?.feed_rows).toBe(1);
    row.remove();
  });

  it("stops recording once stopped", async () => {
    // Arrange
    presence.stop();

    // Act
    setVisibility("hidden");

    // Assert
    expect(await operations()).toEqual([]);
  });
});
