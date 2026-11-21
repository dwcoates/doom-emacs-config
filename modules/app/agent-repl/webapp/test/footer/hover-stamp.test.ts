// @vitest-environment jsdom
/**
 * hover-stamp — the hovered bubble's stamp, drawn in the strip's activity
 * cell in place of the daemon's activity line.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import type { HoverStamp } from "../../src/feed/hover-stamp.js";
import {
  HOVER_STAMP_ARM,
  HOVER_STAMP_TIER,
  drawHoverStampActivity,
  hoverDuration,
  overlayHoverStamp,
} from "../../src/footer/hover-stamp.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

const NOW = 1_000_000;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
});
afterEach(() => {
  vi.useRealTimers();
});

/**
 * A response's stamp settled SETTLED_AGO_MS before now, or arriving (null),
 * whose span took TOOK_MS, or has no known start (null).
 */
function usage(settledAgoMs: number | null, tookMs: number | null = null): HoverStamp {
  const atMs = settledAgoMs === null ? null : NOW - settledAgoMs;
  const startedAtMs = tookMs === null ? null : (atMs ?? NOW) - tookMs;
  return { label: "Response", tokens: { text: "2.1k", color: "rgb(1, 2, 3)" }, atMs, startedAtMs };
}

/** A stamp labelled LABEL, with no figure, sent or settled AT_MS, begun at STARTED_AT_MS. */
function stamp(label: string, atMs: number | null, startedAtMs: number | null = null): HoverStamp {
  return { label, tokens: null, atMs, startedAtMs };
}

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

describe("drawHoverStampActivity", () => {
  it("is an activity cell of its own tier and kind", () => {
    // Act
    const cell = drawHoverStampActivity(usage(0), createTicker(1000));
    // Assert
    expect([
      cell.classList.contains("footer-activity"),
      cell.classList.contains("pfooter-grow"),
      cell.getAttribute("data-tier"),
      cell.getAttribute("data-arm"),
    ]).toEqual([true, true, HOVER_STAMP_TIER, HOVER_STAMP_ARM]);
  });

  it("draws the figure in its heat's color", () => {
    // Act
    const tokens = drawHoverStampActivity(usage(0), createTicker(1000)).querySelector<HTMLElement>(".footer-hover-tokens");
    // Assert
    expect([tokens?.textContent, tokens?.style.color]).toEqual([" 2.1k tokens", "rgb(1, 2, 3)"]);
  });

  it("opens the line with its label", () => {
    // Act
    const line = drawHoverStampActivity(usage(0), createTicker(1000)).querySelector(".footer-hover-stamp");
    // Assert
    expect(line?.firstChild?.textContent).toBe("Response");
  });

  it("draws how long ago a settled response settled", () => {
    // Act
    const cell = drawHoverStampActivity(usage(330_000), createTicker(1000));
    // Assert
    expect(cell.textContent).toBe("Response 2.1k tokens · 5m 30s ago");
  });

  it("draws how long a settled response took, between its figure and its age", () => {
    // Act
    const cell = drawHoverStampActivity(usage(330_000, 90_000), createTicker(1000));
    // Assert
    expect(cell.textContent).toBe("Response 2.1k tokens, 1m 30s · 5m 30s ago");
  });

  it("draws how long ago a prompt was sent, after its label alone", () => {
    // Act
    const cell = drawHoverStampActivity(stamp("Prompt", NOW - 600_000), createTicker(1000));
    // Assert
    expect([cell.textContent, cell.querySelector(".footer-hover-tokens")]).toEqual(["Prompt · 10m ago", null]);
  });

  it("draws how long a settled tool call took and how long ago it settled", () => {
    // Act
    const cell = drawHoverStampActivity(stamp("Tool call Bash", NOW - 120_000, NOW - 132_000), createTicker(1000));
    // Assert
    expect(cell.textContent).toBe("Tool call Bash, 12s · 2m ago");
  });

  it("draws a unit still under way with its span ticking up and no age", () => {
    // Act
    const cell = drawHoverStampActivity(stamp("Testing the fix", null, NOW - 250_000), createTicker(1000));
    // Assert
    expect([cell.textContent, cell.querySelector("[data-age]"), cell.querySelector("[data-elapsed]") !== null]).toEqual([
      "Testing the fix, 4m 10s",
      null,
      true,
    ]);
  });

  it("keeps a running span current on the shared clock", () => {
    // Arrange
    const cell = drawHoverStampActivity(stamp("Merge", null, NOW - 250_000), createTicker(1000));
    // Act
    vi.advanceTimersByTime(5_000);
    // Assert
    expect(cell.textContent).toBe("Merge, 4m 15s");
  });

  it("draws the label alone for a unit with no instant at all", () => {
    // Act
    const cell = drawHoverStampActivity(stamp("Hook", null), createTicker(1000));
    // Assert
    expect(cell.textContent).toBe("Hook");
  });

  it("draws the figure alone after the label while an arriving response has no known start", () => {
    // Act
    const cell = drawHoverStampActivity(usage(null), createTicker(1000));
    // Assert
    expect([cell.textContent, cell.querySelector("[data-age]")]).toEqual(["Response 2.1k tokens", null]);
  });

  it("keeps the age current on the shared clock", () => {
    // Arrange
    const cell = drawHoverStampActivity(usage(330_000), createTicker(1000));
    // Act
    vi.advanceTimersByTime(30_000);
    // Assert
    expect(cell.querySelector(".footer-activity-age")?.textContent).toBe(" · 6m ago");
  });

  it("records what it draws, naming the label and whether it runs", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    drawHoverStampActivity(stamp("Testing the fix", null, NOW - 1_000), createTicker(1000));
    // Assert
    const record = await forwardedRecord(capture, "footer.hover-stamp");
    expect([record.context?.label, record.context?.running, record.context?.settled]).toEqual(["Testing the fix", true, false]);
  });
});

describe("hoverDuration", () => {
  it.each([
    ["a settled span", usage(0, 90_000), "1m 30s"],
    ["a part second, rounded up", usage(0, 5_984), "6s"],
    ["a span that settled where it began", usage(0, 0), "0s"],
    ["a unit still under way", usage(null, 90_000), null],
    ["a span with no known start", usage(0), null],
  ])("states %s", (_name, u, want) => {
    // Act / Assert
    expect(hoverDuration(u)).toBe(want);
  });
});

describe("overlayHoverStamp", () => {
  it("puts the stamp in the strip's activity cell, in its place", () => {
    // Arrange
    const strip = stripOf("footer-status", "pfooter-cell pfooter-grow footer-activity", "footer-clock");
    // Act
    overlayHoverStamp(strip, usage(0), createTicker(1000));
    // Assert
    expect([...strip.children].map((c) => c.getAttribute("data-arm") ?? c.className)).toEqual([
      "footer-status",
      HOVER_STAMP_ARM,
      "footer-clock",
    ]);
  });

  it.each([
    ["no activity cell", stripOf("footer-status")],
    ["two activity cells", stripOf("footer-activity", "footer-activity")],
  ])("refuses a strip with %s", (_name, strip) => {
    // Act / Assert
    expect(() => overlayHoverStamp(strip, usage(0), createTicker(1000))).toThrow(/activity cells, not one/);
  });

  it("records the refusal at error, with how many cells it found", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => overlayHoverStamp(stripOf("footer-status"), usage(0), createTicker(1000))).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "footer.hover-stamp.no-cell");
    expect([record.level.case, record.context?.cells]).toEqual(["error", 0]);
  });

  it("leaves the strip as it was when it refuses", () => {
    // Arrange
    const strip = stripOf("footer-status");
    const before = strip.innerHTML;
    // Act
    expect(() => overlayHoverStamp(strip, usage(0), createTicker(1000))).toThrow();
    // Assert
    expect(strip.innerHTML).toBe(before);
  });
});
