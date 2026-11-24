// @vitest-environment jsdom
// The webhost's load census and main-thread stall probe. See
// src/host/load-census.ts.
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { captureLogRecords, type LogCapture } from "../log-capture.js";
import {
  CENSUS_EVERY_MS,
  STALL_PROBE_MS,
  STALL_THRESHOLD_MS,
  heldFrames,
  installLoadCensus,
  summarize,
  type LoadCensus,
} from "../../src/host/load-census.js";
import { FRAME_SHOWN_ATTRIBUTE, FRAME_WORKSPACE_ATTRIBUTE } from "../../src/host/host.js";

let capture: LogCapture;
let census: LoadCensus | null;
let clock: number;
let timers: { fn: () => void; at: number }[];
let beat: ((sentAt: number) => void) | null;

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

/** A beat the worker SENT at SENT_AT, delivered now. */
function deliverBeat(sentAt: number): void {
  if (beat === null) throw new Error("no heartbeat installed");
  beat(sentAt);
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

/** Let the frames observer's batch run. */
async function observed(): Promise<void> {
  await Promise.resolve();
}

function install(reportsFrames = false): void {
  census = installLoadCensus(document, reportsFrames, {
    doc: document,
    now: () => clock,
    after: (fn, ms) => {
      const timer = { fn, at: clock + ms };
      timers.push(timer);
      return () => {
        timers = timers.filter((t) => t !== timer);
      };
    },
    heartbeat: (_ms, onBeat) => {
      beat = onBeat;
      return () => {
        beat = null;
      };
    },
  });
}

beforeEach(() => {
  capture = captureLogRecords();
  clock = 0;
  timers = [];
  beat = null;
  census = null;
  document.body.replaceChildren();
  setVisibility("visible");
});

afterEach(() => {
  census?.stop();
});

describe("summarize", () => {
  it("counts the pages and names the shown one", () => {
    // Arrange
    addPage("a", 0);
    addPage("b", 0, true);

    // Act
    const summary = summarize(document);

    // Assert
    expect(summary.pages).toBe(2);
    expect(summary.shown_workspace).toBe("b");
  });

  it("states no shown workspace when no page is shown", () => {
    // Arrange
    addPage("a", 0);

    // Act
    const summary = summarize(document);

    // Assert
    expect(summary.shown_workspace).toBe("");
  });

  it("states the size of its own document, never a page's", () => {
    // Arrange
    addPage("a", 40);
    const rail = document.createElement("nav");
    rail.append(document.createElement("div"));
    document.body.append(rail);

    // Act
    const summary = summarize(document);

    // Assert
    expect(summary.host).toEqual({ dom_nodes: document.getElementsByTagName("*").length, feed_rows: 0 });
  });
});

describe("the stall probe", () => {
  it("records a beat delivered at the threshold late, with the census", async () => {
    // Arrange
    addPage("heavy", 5, true);
    install();

    // Act
    block(STALL_THRESHOLD_MS);
    deliverBeat(0);

    // Assert
    const stalls = await records("webhost.stall");
    expect(stalls).toHaveLength(1);
    expect(stalls[0]?.level).toBe("info");
    expect(stalls[0]?.blocked_ms).toBe(STALL_THRESHOLD_MS);
    expect(stalls[0]?.shown_workspace).toBe("heavy");
  });

  it("records nothing for a beat just under the threshold", async () => {
    // Arrange
    install();

    // Act
    block(STALL_THRESHOLD_MS - 1);
    deliverBeat(0);

    // Assert
    expect(await records("webhost.stall")).toEqual([]);
  });

  it("never records throttled timers: beats arrive on time while the main-thread timer lands 750ms late", async () => {
    // Arrange
    install();

    // Act
    for (let i = 0; i < 4; i++) {
      clock += STALL_PROBE_MS;
      deliverBeat(clock);
      // An occluded view's timers land about 750ms late; the census timer is one.
      clock += 750;
      advance(0);
    }

    // Assert
    expect(await records("webhost.stall")).toEqual([]);
  });

  it("records a block once, though the beats queued behind it arrive after it", async () => {
    // Arrange
    install();
    block(STALL_PROBE_MS + 700);

    // Act
    deliverBeat(0);
    deliverBeat(STALL_PROBE_MS);
    deliverBeat(2 * STALL_PROBE_MS);

    // Assert
    const stalls = await records("webhost.stall");
    expect(stalls).toHaveLength(1);
    expect(stalls[0]?.blocked_ms).toBe(STALL_PROBE_MS + 700);
  });

  it("keeps probing after a stall", async () => {
    // Arrange
    install();
    block(STALL_THRESHOLD_MS);
    deliverBeat(0);
    const sentAt = clock;

    // Act
    block(STALL_THRESHOLD_MS);
    deliverBeat(sentAt);

    // Assert
    expect(await records("webhost.stall")).toHaveLength(2);
  });

  it("never reports a beat that spanned a hidden stretch", async () => {
    // Arrange
    install();

    // Act
    setVisibility("hidden");
    setVisibility("visible");
    block(5_000);
    deliverBeat(0);

    // Assert
    expect(await records("webhost.stall")).toEqual([]);
  });

  it("reports again on the beat after a hidden stretch", async () => {
    // Arrange
    install();
    setVisibility("hidden");
    setVisibility("visible");
    block(5_000);
    deliverBeat(0);
    const sentAt = clock;

    // Act
    block(STALL_THRESHOLD_MS);
    deliverBeat(sentAt);

    // Assert
    expect(await records("webhost.stall")).toHaveLength(1);
  });

  it("never reports while the webhost is hidden", async () => {
    // Arrange
    install();
    setVisibility("hidden");

    // Act
    block(5_000);
    deliverBeat(0);

    // Assert
    expect(await records("webhost.stall")).toEqual([]);
  });

  it("stops the heartbeat when stopped", () => {
    // Arrange
    install();

    // Act
    census?.stop();
    census = null;

    // Assert
    expect(beat).toBeNull();
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
    expect(counts[0]?.shown_workspace).toBe("a");
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
    advance(CENSUS_EVERY_MS);

    // Assert
    expect(timers).toEqual([]);
    expect(await records("webhost.census")).toEqual([]);
  });

  it("names no frames in the sidebar document's census", async () => {
    // Arrange
    addPage("a", 0);
    install(false);

    // Act
    advance(CENSUS_EVERY_MS);

    // Assert
    const counts = await records("webhost.census");
    expect(counts[0]).not.toHaveProperty("frames");
    expect(counts[0]).not.toHaveProperty("trigger");
  });
});

describe("heldFrames", () => {
  it("names each workspace once, sorted, though a replaced page holds two frames", () => {
    // Arrange
    addPage("b", 0);
    addPage("a", 0);
    addPage("b", 0);

    // Act
    const frames = heldFrames(document);

    // Assert
    expect(frames).toEqual(["a", "b"]);
  });

  it("names none when no frame stands", () => {
    // Act
    const frames = heldFrames(document);

    // Assert
    expect(frames).toEqual([]);
  });
});

describe("the host's frames", () => {
  it("names the frames it holds in every census interval's record, as a tick", async () => {
    // Arrange
    addPage("b", 0);
    addPage("a", 0, true);
    install(true);

    // Act
    advance(CENSUS_EVERY_MS);

    // Assert
    const counts = await records("webhost.census");
    expect(counts).toHaveLength(1);
    expect(counts[0]?.frames).toEqual(["a", "b"]);
    expect(counts[0]?.trigger).toBe("tick");
  });

  it("takes a census of its own when a frame is added", async () => {
    // Arrange
    install(true);

    // Act
    addPage("a", 0);
    await observed();

    // Assert
    const counts = await records("webhost.census");
    expect(counts).toHaveLength(1);
    expect(counts[0]?.frames).toEqual(["a"]);
    expect(counts[0]?.trigger).toBe("change");
  });

  it("takes a census of its own when a frame is removed", async () => {
    // Arrange
    const frame = addPage("a", 0);
    install(true);

    // Act
    frame.remove();
    await observed();

    // Assert
    const counts = await records("webhost.census");
    expect(counts).toHaveLength(1);
    expect(counts[0]?.frames).toEqual([]);
    expect(counts[0]?.trigger).toBe("change");
  });

  it("takes one census for a batch of frames added in one task", async () => {
    // Arrange
    install(true);

    // Act
    addPage("a", 0);
    addPage("b", 0);
    await observed();

    // Assert
    const counts = await records("webhost.census");
    expect(counts).toHaveLength(1);
    expect(counts[0]?.frames).toEqual(["a", "b"]);
  });

  it("takes no census for a change that leaves the same workspaces held", async () => {
    // Arrange
    addPage("a", 0);
    install(true);

    // Act
    addPage("a", 0);
    document.body.append(document.createElement("div"));
    await observed();

    // Assert
    expect(await records("webhost.census")).toEqual([]);
  });

  it("takes no census of a change in the sidebar document", async () => {
    // Arrange
    install(false);

    // Act
    addPage("a", 0);
    await observed();

    // Assert
    expect(await records("webhost.census")).toEqual([]);
  });

  it("stops watching the frames when stopped", async () => {
    // Arrange
    install(true);

    // Act
    census?.stop();
    census = null;
    addPage("a", 0);
    await observed();

    // Assert
    expect(await records("webhost.census")).toEqual([]);
  });
});
