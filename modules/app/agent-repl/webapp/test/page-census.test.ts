// @vitest-environment jsdom
// Each page's own statement of its size. See src/page-census.ts.
import { beforeEach, describe, expect, it } from "vitest";

import { captureLogRecords, type LogCapture } from "./log-capture.js";
import { CENSUS_EVERY_MS, installPageCensus } from "../src/page-census.js";
import type { BusyProbe, BusyReport } from "../src/busy-probe.js";
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

  it("states the busy probe's tally beside the size when the page runs one", async () => {
    // Arrange
    const report: BusyReport = {
      busy_ms: 7,
      kinds: { timer: { count: 2, ms: 7 }, frame: { count: 0, ms: 0 }, mutation: { count: 0, ms: 0 }, resize: { count: 0, ms: 0 } },
      top: [{ kind: "timer", label: "step", count: 2, ms: 7 }],
    };
    const busy: BusyProbe = { take: () => report, stop: () => undefined };
    installPageCensus(document, after, busy);

    // Act
    fire();

    // Assert
    expect(await censusRecords()).toEqual([expect.objectContaining({ busy: report })]);
  });
});
