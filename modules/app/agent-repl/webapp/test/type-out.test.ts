// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { installAnimationFrames, type AnimationFrames } from "./animation-frames.js";
import { readFileSync } from "node:fs";
import { createTicker } from "../src/clock.js";
import type { RevealPace } from "../src/reveal-pace.js";
import { typeOut, type TypeOutSurface } from "../src/type-out.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";
import { fakePageSeen, type FakePageSeen } from "./page-seen-fake.js";

/** A pace slow enough that one frame shows only part of TOTAL. */
const PACE: RevealPace = { minCps: 10, maxCps: 20, bankLimitSeconds: 100 };
const TOTAL = 400;

/** What the loop did to a surface, in order. */
interface Recorded {
  shown: number[];
  whole: number;
}

/** A surface whose text is TOTAL long, recording every reveal. */
function surface(
  over: Partial<TypeOutSurface> & { seen?: FakePageSeen } = {},
): { surface: TypeOutSurface; recorded: Recorded; element: HTMLElement } {
  const recorded: Recorded = { shown: [], whole: 0 };
  const element = document.createElement("div");
  return {
    element,
    recorded,
    surface: {
      ctx: { seen: over.seen ?? fakePageSeen(), ticker: createTicker(1000) },
      pace: PACE,
      resumed: 0,
      operation: "test.type-out",
      logContext: { row: "row-1" },
      element,
      reowned: () => false,
      total: () => TOTAL,
      whenRendered: (fn) => fn(),
      reveal: (shown) => recorded.shown.push(shown),
      showWhole: () => {
        recorded.whole += 1;
      },
      full: () => false,
      ...over,
    },
  };
}

/** The page's frames, run by hand. */
let animation: AnimationFrames;

/** Run COUNT animation frames. */
function frames(count: number): void {
  animation.run(count);
}

beforeEach(() => {
  animation = installAnimationFrames();
});
afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
  document.body.replaceChildren();
});

describe("the type-out loop", () => {
  it("shows a growing prefix frame by frame", () => {
    // Arrange
    const { surface: s, recorded } = surface();
    // Act
    typeOut(s);
    frames(3);
    // Assert
    expect([recorded.shown.length > 1, recorded.shown.every((n, i) => i === 0 || n >= recorded.shown[i - 1])]).toEqual([true, true]);
  });

  it("resumes from the length an earlier draw showed", () => {
    // Arrange
    const { surface: s, recorded } = surface({ resumed: 200 });
    // Act
    typeOut(s);
    frames(1);
    // Assert
    expect(recorded.shown[0]).toBeGreaterThanOrEqual(200);
  });

  it("shows the whole once the prefix reaches the end", () => {
    // Arrange
    const { surface: s, recorded } = surface({ resumed: TOTAL - 1 });
    // Act
    typeOut(s);
    frames(20);
    // Assert
    expect(recorded.whole).toBe(1);
  });

  it("shows the whole on the first frame a collapsed box is full", () => {
    // Arrange
    const { surface: s, recorded } = surface({ full: () => true });
    // Act
    typeOut(s);
    frames(3);
    // Assert
    expect([recorded.shown.length, recorded.whole]).toEqual([1, 1]);
  });

  it("shows the whole at once on an unseen page", () => {
    // Arrange
    const { surface: s, recorded } = surface({ seen: fakePageSeen(false) });
    // Act
    typeOut(s);
    frames(3);
    // Assert
    expect([recorded.shown, recorded.whole]).toEqual([[], 1]);
  });

  it("shows the rest whole the moment the page turns unseen", () => {
    // Arrange
    const seen = fakePageSeen(true);
    const { surface: s, recorded } = surface({ seen });
    typeOut(s);
    frames(1);
    // Act
    seen.set(false);
    frames(3);
    // Assert
    expect([recorded.shown.length, recorded.whole]).toEqual([1, 1]);
  });

  it("does nothing more once a later draw owns the text", () => {
    // Arrange
    let reowned = false;
    const seen = fakePageSeen(true);
    const { surface: s, recorded } = surface({ seen, reowned: () => reowned });
    typeOut(s);
    frames(1);
    // Act
    reowned = true;
    frames(3);
    seen.set(false);
    // Assert
    expect([recorded.shown.length, recorded.whole]).toEqual([1, 0]);
  });

  it("stops once its mounted element is replaced", () => {
    // Arrange
    const { surface: s, recorded, element } = surface();
    document.body.append(element);
    typeOut(s);
    frames(1);
    // Act
    element.remove();
    frames(3);
    // Assert
    expect([recorded.shown.length, recorded.whole]).toEqual([1, 0]);
  });

  it("waits for the text to render before typing", () => {
    // Arrange
    let rendered: (() => void) | undefined;
    let total: number | undefined = undefined;
    const { surface: s, recorded } = surface({
      total: () => total,
      whenRendered: (fn) => {
        rendered = fn;
      },
    });
    typeOut(s);
    frames(1);
    // Act
    total = TOTAL;
    rendered?.();
    frames(1);
    // Assert
    expect(recorded.shown.length).toBe(1);
  });

  it("shows the whole on a host with no animation frames", () => {
    // Arrange
    vi.stubGlobal("requestAnimationFrame", undefined);
    const { surface: s, recorded } = surface();
    // Act
    typeOut(s);
    // Assert
    expect([recorded.shown, recorded.whole]).toEqual([[], 1]);
  });

  it.each([
    { name: "an unseen page", over: { seen: fakePageSeen(false) }, suffix: "unseen" },
    { name: "a full collapsed box", over: { full: () => true }, suffix: "collapsed-full" },
  ])("records drawing whole for $name under the surface's operation", async ({ over, suffix }) => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { surface: s } = surface(over);
    // Act
    typeOut(s);
    frames(1);
    // Assert
    const record = await forwardedRecord(capture, `test.type-out.${suffix}`);
    expect([record.level.case, record.context?.row]).toEqual(["debug", "row-1"]);
  });

  it("is the loop every typed-out surface runs, none hand-rolling its own", () => {
    // Arrange: the pacing step is the loop's alone.
    const sources = ["../src/feed/cards/response.ts", "../src/feed/title-reveal.ts"].map((path) =>
      readFileSync(new URL(path, import.meta.url), "utf8"),
    );
    // Act
    const shapes = sources.map((source) => [source.includes("typeOut("), source.includes("advanceReveal(")]);
    // Assert
    expect(shapes).toEqual([
      [true, false],
      [true, false],
    ]);
  });
});
