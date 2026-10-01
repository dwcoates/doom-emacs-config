// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import { createLeftViewWatch, type LeftViewWatch } from "../../src/feed/left-view.js";
import { fireIntersection, intersectionObservers } from "../intersection-observer.js";

/** A scroll box holding two rows, and a detector over it. */
function watching(): { box: HTMLElement; r1: HTMLElement; r2: HTMLElement; detector: LeftViewWatch } {
  const box = document.createElement("div");
  const r1 = document.createElement("article");
  const r2 = document.createElement("article");
  box.append(r1, r2);
  document.body.replaceChildren(box);
  const detector = createLeftViewWatch(box);
  if (detector === null) throw new Error("the jsdom setup installs an IntersectionObserver");
  return { box, r1, r2, detector };
}

/** Handlers recording what they heard under NAME. */
function recorder(heard: string[], name: string) {
  return {
    onLeft: () => heard.push(`${name}:left`),
    onDetached: () => heard.push(`${name}:detached`),
  };
}

describe("createLeftViewWatch: the guard", () => {
  const saved = (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver;
  afterEach(() => {
    (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver = saved;
  });

  it("is absent (null) when the environment ships no IntersectionObserver", () => {
    // Arrange
    delete (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver;
    // Act
    const detector = createLeftViewWatch(document.createElement("div"));
    // Assert
    expect(detector).toBeNull();
  });
});

describe("createLeftViewWatch", () => {
  it("roots the observer on the scroll box at threshold 0", () => {
    // Arrange / Act
    const w = watching();
    // Assert
    expect(intersectionObservers().some((r) => r.root === w.box)).toBe(true);
  });

  it("reports an element that was seen and then left the viewport", () => {
    // Arrange
    const w = watching();
    const heard: string[] = [];
    w.detector.watch(w.r1, recorder(heard, "r1"));
    fireIntersection(w.r1, true);
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(heard).toEqual(["r1:left"]);
  });

  it("does not report an element never seen in the viewport", () => {
    // Arrange
    const w = watching();
    const heard: string[] = [];
    w.detector.watch(w.r1, recorder(heard, "r1"));
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(heard).toEqual([]);
  });

  it("reports an element that keeps leaving the viewport only once", () => {
    // Arrange
    const w = watching();
    const heard: string[] = [];
    w.detector.watch(w.r1, recorder(heard, "r1"));
    fireIntersection(w.r1, true);
    fireIntersection(w.r1, false);
    fireIntersection(w.r1, true);
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(heard).toEqual(["r1:left"]);
  });

  it("hears a detached element as detached, never as left", () => {
    // Arrange
    const w = watching();
    const heard: string[] = [];
    w.detector.watch(w.r1, recorder(heard, "r1"));
    fireIntersection(w.r1, true);
    w.r1.remove();
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(heard).toEqual(["r1:detached"]);
  });

  it("watches two elements independently", () => {
    // Arrange
    const w = watching();
    const heard: string[] = [];
    w.detector.watch(w.r1, recorder(heard, "r1"));
    w.detector.watch(w.r2, recorder(heard, "r2"));
    fireIntersection(w.r1, true);
    fireIntersection(w.r2, true);
    // Act
    fireIntersection(w.r2, false);
    // Assert
    expect(heard).toEqual(["r2:left"]);
  });

  it("stops observing an element once unwatched", () => {
    // Arrange
    const w = watching();
    const unwatch = w.detector.watch(w.r1, recorder([], "r1"));
    // Act
    unwatch();
    // Assert
    expect(intersectionObservers().some((r) => r.targets.has(w.r1))).toBe(false);
  });

  it("restarts an element watched again from unseen", () => {
    // Arrange
    const w = watching();
    const heard: string[] = [];
    w.detector.watch(w.r1, recorder(heard, "first"));
    fireIntersection(w.r1, true);
    w.detector.watch(w.r1, recorder(heard, "second"));
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(heard).toEqual([]);
  });

  it("leaves a later watch of the element in place when an earlier unwatch runs", () => {
    // Arrange
    const w = watching();
    const heard: string[] = [];
    const stale = w.detector.watch(w.r1, recorder(heard, "first"));
    w.detector.watch(w.r1, recorder(heard, "second"));
    stale();
    fireIntersection(w.r1, true);
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(heard).toEqual(["second:left"]);
  });

  it("observes nothing once disposed", () => {
    // Arrange
    const w = watching();
    w.detector.watch(w.r1, recorder([], "r1"));
    // Act
    w.detector.dispose();
    // Assert
    expect(intersectionObservers().some((r) => r.targets.has(w.r1))).toBe(false);
  });
});

/**
 * THE ONE DETECTOR: both "left the view" consumers go through it, and neither
 * builds an observer of its own, so the two cannot disagree about what "left"
 * means.
 */
describe("left-view's call sites", () => {
  /** The source of the src module at PATH. */
  const source = (path: string): string => readFileSync(join(process.cwd(), path), "utf8");

  it.each([["src/feed/selection-visibility.ts"], ["src/feed/jump-collapse.ts"]])(
    "%s detects departures through createLeftViewWatch and no observer of its own",
    (path) => {
      // Arrange
      const text = source(path);
      // Act
      const uses = [text.includes("createLeftViewWatch("), /IntersectionObserver\s*\(|new Ctor\(/.test(text)];
      // Assert
      expect(uses).toEqual([true, false]);
    },
  );
});
