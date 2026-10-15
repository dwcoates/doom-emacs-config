// @vitest-environment jsdom
// The webhost's load census and main-thread stall probe. See
// src/host/load-census.ts.
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { captureLogRecords, type LogCapture } from "../log-capture.js";
import {
  CENSUS_EVERY_MS,
  STALL_PROBE_MS,
  STALL_THRESHOLD_MS,
  installLoadCensus,
  summarize,
  type LoadCensus,
} from "../../src/host/load-census.js";
import { FRAME_SHOWN_ATTRIBUTE, FRAME_WORKSPACE_ATTRIBUTE } from "../../src/host/host.js";

let capture: LogCapture;
let census: LoadCensus | null;
let clock: number;
let timers: { fn: () => void; at: number }[];

/** Add a workspace page holding ROWS feed rows. */
function addPage(workspace: string, rows: number, shown = false): HTMLIFrameElement {
  const iframe = document.createElement("iframe");
  iframe.setAttribute(FRAME_WORKSPACE_ATTRIBUTE, workspace);
  if (shown) iframe.setAttribute(FRAME_SHOWN_ATTRIBUTE, "");
  document.body.append(iframe);
  const doc = iframe.contentDocument;
  if (doc === null) throw new Error("jsdom gave the iframe no document");
  for (let i = 0; i < rows; i++) {
    const row = doc.createElement("div");
    row.setAttribute("data-feed-row", String(i));
    doc.body.append(row);
  }
  return iframe;
}

/** Advance the clock by MS, firing every timer due, in order. */
function advance(ms: number): void {
  const until = clock + ms;
  for (;;) {
    timers.sort((a, b) => a.at - b.at);
    const next = timers[0];
    if (next === undefined || next.at > until) break;
    timers.shift();
    // An overdue timer fires NOW: the clock never runs backwards to its due time.
    clock = Math.max(clock, next.at);
    next.fn();
  }
  clock = until;
}

/** Hold the main thread for MS: the clock moves, no timer fires. */
function block(ms: number): void {
  clock += ms;
}

function setVisibility(state: "visible" | "hidden"): void {
  Object.defineProperty(document, "visibilityState", { configurable: true, get: () => state });
  document.dispatchEvent(new Event("visibilitychange"));
}

async function records(operation: string): Promise<Record<string, unknown>[]> {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent
    .filter((record) => record.operation === operation)
    .map((record) => ({ level: record.level.case, message: record.message, ...(record.context ?? {}) }));
}

function install(): void {
  census = installLoadCensus(document, {
    doc: document,
    now: () => clock,
    after: (fn, ms) => {
      const timer = { fn, at: clock + ms };
      timers.push(timer);
      return () => {
        timers = timers.filter((t) => t !== timer);
      };
    },
  });
}

beforeEach(() => {
  capture = captureLogRecords();
  clock = 0;
  timers = [];
  census = null;
  document.body.replaceChildren();
  setVisibility("visible");
});

afterEach(() => {
  census?.stop();
});

describe("summarize", () => {
  it("totals every page and names the largest first", () => {
    // Arrange
    addPage("small", 1);
    addPage("big", 30, true);
    addPage("middle", 10);

    // Act
    const summary = summarize(document);

    // Assert
    expect(summary.pages).toBe(3);
    expect(summary.total_feed_rows).toBe(41);
    expect(summary.shown_workspace).toBe("big");
    expect(summary.largest.map((page) => page.workspace)).toEqual(["big", "middle", "small"]);
  });

  it("names at most three pages", () => {
    // Arrange
    for (const name of ["a", "b", "c", "d"]) addPage(name, 1);

    // Act
    const summary = summarize(document);

    // Assert
    expect(summary.largest).toHaveLength(3);
  });

  it("counts a page whose document is not loaded apart, never as an empty page", () => {
    // Arrange
    addPage("loaded", 2);
    const unloaded = document.createElement("iframe");
    unloaded.setAttribute(FRAME_WORKSPACE_ATTRIBUTE, "unloaded");
    Object.defineProperty(unloaded, "contentDocument", { value: null });
    document.body.append(unloaded);

    // Act
    const summary = summarize(document);

    // Assert
    expect(summary.pages).toBe(1);
    expect(summary.pages_unloaded).toBe(1);
  });

  it("states no shown workspace when no page is shown", () => {
    // Arrange
    addPage("a", 1);

    // Act
    const summary = summarize(document);

    // Assert
    expect(summary.shown_workspace).toBe("");
  });
});

describe("the stall probe", () => {
  it("records a probe that fired at the threshold late, with the census", async () => {
    // Arrange
    addPage("heavy", 5, true);
    install();

    // Act
    block(STALL_PROBE_MS + STALL_THRESHOLD_MS);
    advance(0);

    // Assert
    const stalls = await records("webhost.stall");
    expect(stalls).toHaveLength(1);
    expect(stalls[0]?.level).toBe("info");
    expect(stalls[0]?.blocked_ms).toBe(STALL_THRESHOLD_MS);
    expect(stalls[0]?.shown_workspace).toBe("heavy");
  });

  it("records nothing for a probe just under the threshold", async () => {
    // Arrange
    install();

    // Act
    block(STALL_PROBE_MS + STALL_THRESHOLD_MS - 1);
    advance(0);

    // Assert
    expect(await records("webhost.stall")).toEqual([]);
  });

  it("keeps probing after a stall", async () => {
    // Arrange
    install();
    block(STALL_PROBE_MS + STALL_THRESHOLD_MS);
    advance(0);

    // Act
    block(STALL_PROBE_MS + STALL_THRESHOLD_MS);
    advance(0);

    // Assert
    expect(await records("webhost.stall")).toHaveLength(2);
  });

  it("never reports a probe that spanned a hidden stretch", async () => {
    // Arrange
    install();

    // Act
    setVisibility("hidden");
    setVisibility("visible");
    block(STALL_PROBE_MS + 5_000);
    advance(0);

    // Assert
    expect(await records("webhost.stall")).toEqual([]);
  });

  it("reports again on the probe after a hidden stretch", async () => {
    // Arrange
    install();
    setVisibility("hidden");
    setVisibility("visible");
    block(STALL_PROBE_MS + 5_000);
    advance(0);

    // Act
    block(STALL_PROBE_MS + STALL_THRESHOLD_MS);
    advance(0);

    // Assert
    expect(await records("webhost.stall")).toHaveLength(1);
  });

  it("never reports while the webhost is hidden", async () => {
    // Arrange
    install();
    setVisibility("hidden");

    // Act
    block(STALL_PROBE_MS + 5_000);
    advance(0);

    // Assert
    expect(await records("webhost.stall")).toEqual([]);
  });
});

describe("the census", () => {
  it("records the pages it holds every census interval", async () => {
    // Arrange
    addPage("a", 2, true);
    addPage("b", 3);
    install();

    // Act
    advance(CENSUS_EVERY_MS);

    // Assert
    const counts = await records("webhost.census");
    expect(counts).toHaveLength(1);
    expect(counts[0]?.level).toBe("info");
    expect(counts[0]?.pages).toBe(2);
    expect(counts[0]?.total_feed_rows).toBe(5);
  });

  it("records nothing before the first interval has passed", async () => {
    // Arrange
    install();

    // Act
    advance(CENSUS_EVERY_MS - 1);

    // Assert
    expect(await records("webhost.census")).toEqual([]);
  });

  it("stops both loops when stopped", async () => {
    // Arrange
    install();

    // Act
    census?.stop();
    census = null;
    block(STALL_PROBE_MS + STALL_THRESHOLD_MS);
    advance(CENSUS_EVERY_MS);

    // Assert
    expect(timers).toEqual([]);
    expect(await records("webhost.census")).toEqual([]);
  });
});
