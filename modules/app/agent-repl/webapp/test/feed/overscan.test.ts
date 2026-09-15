// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  OVERSCAN_CLASS,
  OVERSCAN_PAGES,
  createOverscan,
  overscanRootMargin,
} from "../../src/feed/overscan.js";
import { fireIntersection, intersectionObservers } from "../intersection-observer.js";

/** A scroll box holding one feed row, both fresh for each test. */
function boxWithRow(): { box: HTMLElement; row: HTMLElement } {
  const box = document.createElement("div");
  const row = document.createElement("article");
  row.className = "feed-item";
  box.append(row);
  document.body.replaceChildren(box);
  return { box, row };
}

describe("overscanRootMargin", () => {
  it("grows the root by five viewport heights top and bottom for five pages", () => {
    // Arrange / Act / Assert.
    expect(overscanRootMargin(5)).toBe("500% 0px");
  });

  it("grows the root by nothing on the sides", () => {
    // The band is vertical only, so the horizontal component stays 0px.
    expect(overscanRootMargin(OVERSCAN_PAGES).endsWith(" 0px")).toBe(true);
  });
});

describe("createOverscan: the guard", () => {
  const saved = (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver;
  afterEach(() => {
    (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver = saved;
  });

  it("is a no-op (null) when the environment ships no IntersectionObserver", () => {
    // Arrange: an environment without the class, like a bare jsdom.
    const { box } = boxWithRow();
    delete (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver;
    // Act.
    const overscan = createOverscan(box);
    // Assert.
    expect(overscan).toBeNull();
  });
});

describe("createOverscan: rooting the observer", () => {
  it("roots the observer on the scroll box", () => {
    // Arrange.
    const { box } = boxWithRow();
    // Act.
    createOverscan(box);
    // Assert.
    const registration = intersectionObservers().find((r) => r.root === box);
    expect(registration).toBeDefined();
  });

  it("expands the root by the five-page band", () => {
    // Arrange.
    const { box } = boxWithRow();
    // Act.
    createOverscan(box);
    // Assert.
    const registration = intersectionObservers().find((r) => r.root === box);
    expect(registration?.rootMargin).toBe(overscanRootMargin(OVERSCAN_PAGES));
  });
});

describe("createOverscan: pre-rendering within the band", () => {
  it("marks a row that enters the band pre-rendered", () => {
    // Arrange.
    const { box, row } = boxWithRow();
    const overscan = createOverscan(box);
    overscan?.observe(row);
    // Act: the row crosses into the band.
    fireIntersection(row, true);
    // Assert.
    expect(row.classList.contains(OVERSCAN_CLASS)).toBe(true);
  });

  it("reverts a row that leaves the band to the skippable default", () => {
    // Arrange: a row already in the band.
    const { box, row } = boxWithRow();
    const overscan = createOverscan(box);
    overscan?.observe(row);
    fireIntersection(row, true);
    // Act: the row scrolls out past the band.
    fireIntersection(row, false);
    // Assert.
    expect(row.classList.contains(OVERSCAN_CLASS)).toBe(false);
  });
});

describe("createOverscan: lifecycle", () => {
  it("stops watching a row it is told to unobserve", () => {
    // Arrange.
    const { box, row } = boxWithRow();
    const overscan = createOverscan(box);
    overscan?.observe(row);
    // Act.
    overscan?.unobserve(row);
    // Assert: nothing watches it, so a fire finds no observer.
    expect(() => fireIntersection(row, true)).toThrow(/no IntersectionObserver/);
  });

  it("tears the observer down on dispose", () => {
    // Arrange.
    const { box, row } = boxWithRow();
    const overscan = createOverscan(box);
    overscan?.observe(row);
    // Act.
    overscan?.dispose();
    // Assert: the disconnected observer is gone from the registry.
    expect(intersectionObservers().some((r) => r.root === box)).toBe(false);
  });
});
