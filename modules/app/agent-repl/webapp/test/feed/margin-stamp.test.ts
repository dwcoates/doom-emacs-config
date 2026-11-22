// @vitest-environment jsdom
/**
 * margin-stamp — the hovered row's time and age, faded into the feed's right margin.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import type { HoverStamp } from "../../src/feed/hover-stamp.js";
import {
  MARGIN_STAMP_CLASS,
  MARGIN_STAMP_SHOWN,
  marginClockTime,
  marginStampText,
  mountMarginStamp,
} from "../../src/feed/margin-stamp.js";

const NOW = 1_000_000_000;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
  document.body.replaceChildren();
});
afterEach(() => {
  vi.useRealTimers();
});

function stamp(atMs: number | null): HoverStamp {
  return { label: "Response", tokens: null, atMs, startedAtMs: null };
}

function mounted() {
  const parent = document.createElement("div");
  document.body.append(parent);
  const row = document.createElement("div");
  parent.append(row);
  const margin = mountMarginStamp(parent, createTicker(1000));
  const el = parent.querySelector<HTMLElement>(`.${MARGIN_STAMP_CLASS}`)!;
  return { parent, row, margin, el };
}

describe("marginStampText", () => {
  it("reads the clock time and how long ago", () => {
    // Act
    const text = marginStampText(NOW - 330_000, NOW);
    // Assert
    expect(text).toBe(`${marginClockTime(NOW - 330_000)} · 5m 30s ago`);
  });
});

describe("mountMarginStamp", () => {
  it("fades a settled row's stamp in with its time and age", () => {
    // Arrange
    const { row, margin, el } = mounted();
    // Act
    margin.show(stamp(NOW - 330_000), row);
    // Assert
    expect([el.hasAttribute(MARGIN_STAMP_SHOWN), el.textContent]).toEqual([true, marginStampText(NOW - 330_000, NOW)]);
  });

  it("keeps the age current on the shared clock", () => {
    // Arrange
    const { row, margin, el } = mounted();
    margin.show(stamp(NOW - 330_000), row);
    // Act
    vi.advanceTimersByTime(30_000);
    // Assert
    expect(el.textContent).toBe(marginStampText(NOW - 330_000, NOW + 30_000));
  });

  it.each([
    ["a null stamp", null, true],
    ["a stamp with no instant", stamp(null), true],
    ["no row", stamp(NOW - 1000), false],
  ])("fades out for %s", (_name, value, hasRow) => {
    // Arrange
    const { row, margin, el } = mounted();
    margin.show(stamp(NOW - 1000), row);
    // Act
    margin.show(value, hasRow ? row : null);
    // Assert
    expect(el.hasAttribute(MARGIN_STAMP_SHOWN)).toBe(false);
  });

  it("stops counting once faded out", () => {
    // Arrange
    const { row, margin, el } = mounted();
    margin.show(stamp(NOW - 5000), row);
    margin.show(null, null);
    const before = el.textContent;
    // Act
    vi.advanceTimersByTime(30_000);
    // Assert
    expect(el.textContent).toBe(before);
  });

  it("removes its element on dispose", () => {
    // Arrange
    const { parent, margin } = mounted();
    // Act
    margin.dispose();
    // Assert
    expect(parent.querySelector(`.${MARGIN_STAMP_CLASS}`)).toBeNull();
  });
});
