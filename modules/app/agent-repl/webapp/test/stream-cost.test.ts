// @vitest-environment jsdom
// How much main-thread work each of a page's streams costs, one record per
// window. See src/stream-cost.ts.
import { afterEach, describe, expect, it } from "vitest";

import { STREAM_COST_WINDOW_MS, createStreamCostMeter, regionNodes, type StreamCostEnv } from "../src/stream-cost.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

afterEach(() => {
  document.body.replaceChildren();
});

/** An environment on a hand-driven clock whose window timer the test fires. */
function env(): StreamCostEnv & { at: { ms: number }; fire(): void; armedMs: number | null; stopped: boolean } {
  const at = { ms: 1000 };
  let armed: (() => void) | null = null;
  const e = {
    at,
    armedMs: null as number | null,
    stopped: false,
    now: () => at.ms,
    every: (fn: () => void, ms: number) => {
      armed = fn;
      e.armedMs = ms;
      return () => {
        e.stopped = true;
      };
    },
    nodes: () => ({ page: 40, sidebar: 30, topbar: 4, feed: 5, footer: 1 }),
    presence: () => ({ shown: false, warm: true }),
    fire: () => armed?.(),
  };
  return e;
}

describe("the stream cost meter", () => {
  it("states a window's totals per stream at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    const e = env();
    const meter = createStreamCostMeter(e);
    meter.record("WatchWorkspaceRoster", 18);
    meter.record("WatchWorkspaceRoster", 22.04);
    meter.record("WatchFooter", 1.26);
    e.at.ms += STREAM_COST_WINDOW_MS;

    // Act
    e.fire();

    // Assert
    const record = await forwardedRecord(capture, "page.stream-cost");
    expect(record.level.case).toBe("info");
    expect(record.context).toMatchObject({
      window_ms: STREAM_COST_WINDOW_MS,
      total_ms: 41.3,
      streams: {
        WatchWorkspaceRoster: { pushes: 2, ms: 40, max_ms: 22 },
        WatchFooter: { pushes: 1, ms: 1.3, max_ms: 1.3 },
      },
    });
  });

  it("states the DOM size of each component and whether the page is shown and warm", async () => {
    // Arrange
    const capture = captureLogRecords();
    const e = env();
    const meter = createStreamCostMeter(e);
    meter.record("WatchFooter", 1);

    // Act
    meter.flush();

    // Assert
    const record = await forwardedRecord(capture, "page.stream-cost");
    expect(record.context).toMatchObject({
      nodes: { page: 40, sidebar: 30, topbar: 4, feed: 5, footer: 1 },
      shown: false,
      warm: true,
    });
  });

  it("states its first window even when nothing arrived", async () => {
    // Arrange
    const capture = captureLogRecords();
    const meter = createStreamCostMeter(env());

    // Act
    meter.flush();

    // Assert
    const record = await forwardedRecord(capture, "page.stream-cost");
    expect(record.context).toMatchObject({ total_ms: 0, streams: {} });
  });

  it("says nothing for a later window in which nothing arrived", async () => {
    // Arrange
    const capture = captureLogRecords();
    const meter = createStreamCostMeter(env());
    meter.flush();
    capture.logger.flush();
    await Promise.resolve();
    capture.sent.length = 0;

    // Act
    meter.flush();

    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.map((r) => r.operation)).not.toContain("page.stream-cost");
  });

  it("starts each window from nothing", async () => {
    // Arrange
    const capture = captureLogRecords();
    const meter = createStreamCostMeter(env());
    meter.record("WatchFooter", 5);
    meter.flush();
    capture.logger.flush();
    await Promise.resolve();
    capture.sent.length = 0;
    meter.record("WatchTopbar", 2);

    // Act
    meter.flush();

    // Assert
    const record = await forwardedRecord(capture, "page.stream-cost");
    expect(record.context?.["streams"]).toEqual({ WatchTopbar: { pushes: 1, ms: 2, max_ms: 2 } });
  });

  it("arms one window timer of the window's length", () => {
    // Arrange
    const e = env();

    // Act
    createStreamCostMeter(e);

    // Assert
    expect(e.armedMs).toBe(STREAM_COST_WINDOW_MS);
  });

  it("stops its window timer when disposed", () => {
    // Arrange
    const e = env();
    const meter = createStreamCostMeter(e);

    // Act
    meter.dispose();

    // Assert
    expect(e.stopped).toBe(true);
  });
});

describe("regionNodes", () => {
  it("counts the elements in each component's region and in the page", () => {
    // Arrange
    document.body.innerHTML =
      '<nav id="ws-sidebar"><div><span></span></div></nav><header id="topbar"><b></b></header>' +
      '<div id="feed-scroll"><main id="feed"></main></div>';

    // Act
    const counts = regionNodes(document);

    // Assert
    expect(counts).toMatchObject({ sidebar: 2, topbar: 1, feed: 1, footer: 0 });
    expect(counts["page"]).toBe(document.getElementsByTagName("*").length);
  });
});
