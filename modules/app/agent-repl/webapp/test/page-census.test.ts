// @vitest-environment jsdom
// Each page's own statement of its size. See src/page-census.ts.
import { beforeEach, describe, expect, it } from "vitest";

import { captureLogRecords, type LogCapture } from "./log-capture.js";
import { animationCensus, CENSUS_ANIMATION_NAMES, CENSUS_EVERY_MS, installPageCensus } from "../src/page-census.js";
import type { After } from "../src/every.js";

let capture: LogCapture;
let fire: () => void;
let scheduledMs: number[];

const after: After = (fn, ms) => {
  scheduledMs.push(ms);
  fire = fn;
  return () => undefined;
};

async function censusRecords(): Promise<Record<string, unknown>[]> {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent
    .filter((record) => record.operation === "page.census")
    .map((record) => ({ level: record.level.case, ...(record.context ?? {}) }));
}

/** Put an element running each of NAMES in the page, as its computed style states it. */
function withAnimations(doc: Document, names: string[]): void {
  for (const name of names) {
    const el = doc.createElement("span");
    el.style.animationName = name;
    doc.body.append(el);
  }
}

beforeEach(() => {
  capture = captureLogRecords();
  scheduledMs = [];
  fire = () => undefined;
  document.body.replaceChildren();
});

describe("installPageCensus", () => {
  it("states the page's size and visibility at INFO each census interval", async () => {
    // Arrange
    const row = document.createElement("div");
    row.setAttribute("data-feed-row", "0");
    document.body.append(row);
    installPageCensus(document, after);

    // Act
    fire();

    // Assert
    expect(scheduledMs[0]).toBe(CENSUS_EVERY_MS);
    expect(await censusRecords()).toEqual([
      expect.objectContaining({ level: "info", feed_rows: 1, visibility: document.visibilityState }),
    ]);
  });

  it("states nothing before the first interval", async () => {
    // Act
    installPageCensus(document, after);

    // Assert
    expect(await censusRecords()).toEqual([]);
  });
});

describe("animationCensus", () => {
  it("counts the live animations by name", () => {
    // Arrange
    withAnimations(document, ["pulse", "pulse", "pfooter-status-wave"]);

    // Act
    const census = animationCensus(document);

    // Assert
    expect(census).toEqual({ animations: 3, animation_names: { pulse: 2, "pfooter-status-wave": 1 } });
  });

  it("counts each animation of an element's list", () => {
    // Arrange
    withAnimations(document, ["pulse, shimmer"]);

    // Act
    const census = animationCensus(document);

    // Assert
    expect(census).toEqual({ animations: 2, animation_names: { pulse: 1, shimmer: 1 } });
  });

  it("names only the most numerous names, though it counts every animation", () => {
    // Arrange
    const names = ["a", "b", "c", "d", "e", "f", "f"];
    withAnimations(document, names);

    // Act
    const census = animationCensus(document);

    // Assert
    expect([census.animations, Object.keys(census.animation_names).length, census.animation_names.f]).toEqual([
      names.length,
      CENSUS_ANIMATION_NAMES,
      2,
    ]);
  });

  it("states the animations in the census record", async () => {
    // Arrange
    withAnimations(document, ["pulse"]);
    installPageCensus(document, after);

    // Act
    fire();

    // Assert
    expect(await censusRecords()).toEqual([
      expect.objectContaining({ animations: 1, animation_names: { pulse: 1 } }),
    ]);
  });
});
