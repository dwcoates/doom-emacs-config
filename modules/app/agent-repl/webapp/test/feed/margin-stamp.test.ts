// @vitest-environment jsdom
/**
 * margin-stamp — the hovered row's time and age, faded into the feed's right margin.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import { fireResize } from "../resize-observer.js";
import type { HoverStamp } from "../../src/feed/hover-stamp.js";
import {
  MARGIN_STAMP_CLASS,
  MARGIN_STAMP_LINES_CLASS,
  MARGIN_STAMP_SHOWN,
  marginClockTime,
  marginStampCenter,
  marginStampLines,
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
  vi.unstubAllGlobals();
});

function stamp(atMs: number | null): HoverStamp {
  return { label: "Response", tokens: null, atMs, startedAtMs: null };
}

/** Give EL a viewport extent from TOP to BOTTOM, its left edge at LEFT. */
function placeAt(el: Element, top: number, bottom: number, left = 0): void {
  el.getBoundingClientRect = () => ({ top, bottom, left }) as DOMRect;
}

/**
 * Feed geometry: the scroll box shows 100..500 of the viewport from x 30,
 * 600 wide, scrolled 1000 down; the stamp's slot stands at the box's bottom
 * edge from x 20; and the stamp is 40 tall.
 */
function mounted() {
  const parent = document.createElement("div");
  const slot = document.createElement("div");
  document.body.append(parent, slot);
  placeAt(parent, 100, 500, 30);
  Object.defineProperty(parent, "clientHeight", { value: 400 });
  Object.defineProperty(parent, "clientWidth", { value: 600 });
  parent.scrollTop = 1000;
  placeAt(slot, 500, 500, 20);
  const row = document.createElement("div");
  parent.append(row);
  placeAt(row, 200, 300);
  const margin = mountMarginStamp(parent, slot, createTicker(1000));
  const el = slot.querySelector<HTMLElement>(`.${MARGIN_STAMP_CLASS}`)!;
  Object.defineProperty(el, "offsetHeight", { value: 40 });
  return { parent, slot, row, margin, el };
}

function lines(el: HTMLElement): string[] {
  const box = el.querySelector(`.${MARGIN_STAMP_LINES_CLASS}`)!;
  return [...box.children].map((c) => c.textContent ?? "");
}

describe("marginStampCenter", () => {
  const view = { top: 0, bottom: 400 };
  it.each([
    ["a row wholly in view stands level with its middle", { top: 100, bottom: 200 }, 150],
    ["a row whose bottom half shows stands at its bottom quarter mark", { top: -200, bottom: 200 }, 100],
    ["a row whose top half shows stands at its top quarter mark", { top: 200, bottom: 600 }, 300],
    ["a row taller than the view stands at the view's middle", { top: -1000, bottom: 1000 }, 200],
    ["a sliver at the top edge keeps the whole stamp in view", { top: -100, bottom: 10 }, 20],
    ["a sliver at the bottom edge keeps the whole stamp in view", { top: 390, bottom: 900 }, 380],
  ])("%s", (_name, row, center) => {
    // Act
    const placed = marginStampCenter(row, view, 40);
    // Assert
    expect(placed).toBe(center);
  });

  it.each([
    ["wholly above the view", { top: -300, bottom: -10 }],
    ["wholly below the view", { top: 410, bottom: 700 }],
    ["ending on the view's top edge", { top: -300, bottom: 0 }],
  ])("is null for a row %s", (_name, row) => {
    // Act
    const placed = marginStampCenter(row, view, 40);
    // Assert
    expect(placed).toBeNull();
  });
});

describe("marginStampLines", () => {
  it("reads the clock time, then how long ago", () => {
    // Act
    const text = marginStampLines(NOW - 330_000, NOW);
    // Assert
    expect(text).toEqual([marginClockTime(NOW - 330_000), "5m 30s ago"]);
  });
});

describe("mountMarginStamp", () => {
  it("fades a settled row's stamp in with its time and age", () => {
    // Arrange
    const { row, margin, el } = mounted();
    // Act
    margin.show(stamp(NOW - 330_000), row);
    // Assert
    expect([el.hasAttribute(MARGIN_STAMP_SHOWN), lines(el)]).toEqual([true, marginStampLines(NOW - 330_000, NOW)]);
  });

  it("keeps the age current on the shared clock", () => {
    // Arrange
    const { row, margin, el } = mounted();
    margin.show(stamp(NOW - 330_000), row);
    // Act
    vi.advanceTimersByTime(30_000);
    // Assert
    expect(lines(el)).toEqual(marginStampLines(NOW - 330_000, NOW + 30_000));
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
    const before = lines(el);
    // Act
    vi.advanceTimersByTime(30_000);
    // Assert
    expect(lines(el)).toEqual(before);
  });

  it("stands at the middle of the row's visible part, in its slot's terms", () => {
    // Arrange
    const { row, margin, el } = mounted();
    // Act
    margin.show(stamp(NOW - 1000), row);
    // Assert
    expect(el.style.top).toBe(`${250 - 500}px`);
  });

  it("is mounted in its slot, never in the scroll box, so it adds nothing to the feed's height", () => {
    // Arrange
    const { parent, slot, row, margin, el } = mounted();
    // Act
    margin.show(stamp(NOW - 1000), row);
    // Assert
    expect([el.parentElement, parent.contains(el)]).toEqual([slot, false]);
  });

  it("spans the scroll box's client area across", () => {
    // Arrange
    const { row, margin, el } = mounted();
    // Act
    margin.show(stamp(NOW - 1000), row);
    // Assert
    expect([el.style.left, el.style.width]).toEqual([`${30 - 20}px`, "600px"]);
  });

  it("stands at the same place however far the feed is scrolled, while the row stands still on screen", () => {
    // Arrange
    const { parent, row, margin, el } = mounted();
    margin.show(stamp(NOW - 1000), row);
    const top = el.style.top;
    parent.scrollTop = 4000;
    // Act
    parent.dispatchEvent(new Event("scroll"));
    // Assert
    expect(el.style.top).toBe(top);
  });

  it("places itself again when the feed scrolls", () => {
    // Arrange
    const { parent, row, margin, el } = mounted();
    margin.show(stamp(NOW - 1000), row);
    placeAt(row, -300, 300);
    parent.scrollTop = 1500;
    // Act
    parent.dispatchEvent(new Event("scroll"));
    // Assert
    expect(el.style.top).toBe(`${200 - 500}px`);
  });

  it("places itself again when the row or the feed resizes", () => {
    // Arrange
    const { row, margin, el } = mounted();
    margin.show(stamp(NOW - 1000), row);
    placeAt(row, 200, 400);
    // Act
    fireResize(row);
    // Assert
    expect(el.style.top).toBe(`${300 - 500}px`);
  });

  it("fades out when a scroll takes its row wholly out of view", () => {
    // Arrange
    const { parent, row, margin, el } = mounted();
    margin.show(stamp(NOW - 1000), row);
    placeAt(row, 600, 700);
    // Act
    parent.dispatchEvent(new Event("scroll"));
    // Assert
    expect(el.hasAttribute(MARGIN_STAMP_SHOWN)).toBe(false);
  });

  it("stops watching a row once faded out, and is not placed again by a scroll", () => {
    // Arrange
    const { parent, row, margin, el } = mounted();
    margin.show(stamp(NOW - 1000), row);
    margin.show(null, null);
    const top = el.style.top;
    placeAt(row, 300, 400);
    // Act
    parent.dispatchEvent(new Event("scroll"));
    // Assert
    expect([el.hasAttribute(MARGIN_STAMP_SHOWN), el.style.top]).toEqual([false, top]);
    expect(() => fireResize(row)).toThrow("no ResizeObserver is watching");
  });

  it("refuses a page with no ResizeObserver", () => {
    // Arrange
    vi.stubGlobal("ResizeObserver", undefined);
    const parent = document.createElement("div");
    document.body.append(parent);
    // Act
    const mount = (): unknown => mountMarginStamp(parent, parent, createTicker(1000));
    // Assert
    expect(mount).toThrow("invariant: the feed's page has a ResizeObserver");
  });

  it("stops observing and listening on dispose", () => {
    // Arrange
    const { parent, row, margin, el } = mounted();
    margin.show(stamp(NOW - 1000), row);
    margin.dispose();
    const top = el.style.top;
    placeAt(row, 300, 400);
    // Act
    parent.dispatchEvent(new Event("scroll"));
    // Assert
    expect(el.style.top).toBe(top);
    expect(() => fireResize(parent)).toThrow("no ResizeObserver is watching");
  });

  it("removes its element on dispose", () => {
    // Arrange
    const { slot, margin } = mounted();
    // Act
    margin.dispose();
    // Assert
    expect(slot.querySelector(`.${MARGIN_STAMP_CLASS}`)).toBeNull();
  });
});
