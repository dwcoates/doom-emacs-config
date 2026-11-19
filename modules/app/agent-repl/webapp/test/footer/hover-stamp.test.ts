// @vitest-environment jsdom
/**
 * response-usage — the hovered response's usage, drawn in the strip's
 * activity cell in place of the daemon's activity line.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import type { ResponseUsage } from "../../src/feed/response-usage.js";
import {
  RESPONSE_USAGE_ARM,
  RESPONSE_USAGE_LABEL,
  RESPONSE_USAGE_TIER,
  drawResponseUsageActivity,
  overlayResponseUsage,
  responseDuration,
} from "../../src/footer/response-usage.js";
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
 * A usage settled SETTLED_AGO_MS before now, or arriving (null), whose span
 * took TOOK_MS, or has no known start (null).
 */
function usage(settledAgoMs: number | null, tookMs: number | null = null): ResponseUsage {
  const settledAtMs = settledAgoMs === null ? null : NOW - settledAgoMs;
  const startedAtMs = tookMs === null ? null : (settledAtMs ?? NOW) - tookMs;
  return { text: "2.1k", color: "rgb(1, 2, 3)", settledAtMs, startedAtMs };
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

describe("drawResponseUsageActivity", () => {
  it("is an activity cell of its own tier and kind", () => {
    // Act
    const cell = drawResponseUsageActivity(usage(0), createTicker(1000));
    // Assert
    expect([
      cell.classList.contains("footer-activity"),
      cell.classList.contains("pfooter-grow"),
      cell.getAttribute("data-tier"),
      cell.getAttribute("data-arm"),
    ]).toEqual([true, true, RESPONSE_USAGE_TIER, RESPONSE_USAGE_ARM]);
  });

  it("draws the figure in its heat's color", () => {
    // Act
    const tokens = drawResponseUsageActivity(usage(0), createTicker(1000)).querySelector<HTMLElement>(".footer-response-tokens");
    // Assert
    expect([tokens?.textContent, tokens?.style.color]).toEqual(["2.1k tokens", "rgb(1, 2, 3)"]);
  });

  it("opens the line naming it a response", () => {
    // Act
    const line = drawResponseUsageActivity(usage(0), createTicker(1000)).querySelector(".footer-response-usage");
    // Assert
    expect(line?.firstChild?.textContent).toBe(RESPONSE_USAGE_LABEL);
  });

  it("draws how long ago a settled response settled", () => {
    // Act
    const cell = drawResponseUsageActivity(usage(330_000), createTicker(1000));
    // Assert
    expect(cell.textContent).toBe("Response 2.1k tokens · 5m 30s ago");
  });

  it("draws how long a settled response took, between its figure and its age", () => {
    // Act
    const cell = drawResponseUsageActivity(usage(330_000, 90_000), createTicker(1000));
    // Assert
    expect(cell.textContent).toBe("Response 2.1k tokens, 1m 30s · 5m 30s ago");
  });

  it("draws no duration for an arriving response, even with a known start", () => {
    // Act
    const cell = drawResponseUsageActivity(usage(null, 90_000), createTicker(1000));
    // Assert
    expect([cell.textContent, cell.querySelector(".footer-response-duration")]).toEqual(["Response 2.1k tokens", null]);
  });

  it("keeps the age current on the shared clock", () => {
    // Arrange
    const cell = drawResponseUsageActivity(usage(330_000), createTicker(1000));
    // Act
    vi.advanceTimersByTime(30_000);
    // Assert
    expect(cell.querySelector(".footer-activity-age")?.textContent).toBe(" · 6m ago");
  });

  it("draws the figure alone while the response is arriving", () => {
    // Act
    const cell = drawResponseUsageActivity(usage(null), createTicker(1000));
    // Assert
    expect([cell.textContent, cell.querySelector("[data-age]")]).toEqual(["Response 2.1k tokens", null]);
  });
});

describe("responseDuration", () => {
  it.each([
    ["a settled span", usage(0, 90_000), "1m 30s"],
    ["a part second, rounded up", usage(0, 5_984), "6s"],
    ["a span that settled where it began", usage(0, 0), "0s"],
    ["an arriving response", usage(null, 90_000), null],
    ["a span with no known start", usage(0), null],
  ])("states %s", (_name, u, want) => {
    // Act / Assert
    expect(responseDuration(u)).toBe(want);
  });
});

describe("overlayResponseUsage", () => {
  it("puts the usage in the strip's activity cell, in its place", () => {
    // Arrange
    const strip = stripOf("footer-status", "pfooter-cell pfooter-grow footer-activity", "footer-clock");
    // Act
    overlayResponseUsage(strip, usage(0), createTicker(1000));
    // Assert
    expect([...strip.children].map((c) => c.getAttribute("data-arm") ?? c.className)).toEqual([
      "footer-status",
      RESPONSE_USAGE_ARM,
      "footer-clock",
    ]);
  });

  it.each([
    ["no activity cell", stripOf("footer-status")],
    ["two activity cells", stripOf("footer-activity", "footer-activity")],
  ])("refuses a strip with %s", (_name, strip) => {
    // Act / Assert
    expect(() => overlayResponseUsage(strip, usage(0), createTicker(1000))).toThrow(/activity cells, not one/);
  });

  it("records the refusal at error, with how many cells it found", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => overlayResponseUsage(stripOf("footer-status"), usage(0), createTicker(1000))).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "footer.response-usage.no-cell");
    expect([record.level.case, record.context?.cells]).toEqual(["error", 0]);
  });

  it("leaves the strip as it was when it refuses", () => {
    // Arrange
    const strip = stripOf("footer-status");
    const before = strip.innerHTML;
    // Act
    expect(() => overlayResponseUsage(strip, usage(0), createTicker(1000))).toThrow();
    // Assert
    expect(strip.innerHTML).toBe(before);
  });
});
