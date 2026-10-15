// @vitest-environment jsdom
// A page in the webhost is switched to when the webhost shows it. See
// src/host/shown-edge.ts.
import { afterEach, describe, expect, it } from "vitest";

import { watchShownEdge } from "../../src/host/shown-edge.js";
import { createPresence, type PresenceSource } from "../../src/presence.js";
import { frameQueue } from "../frame-queue.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

let presence: PresenceSource | null = null;

afterEach(() => {
  presence?.dispose();
  presence = null;
});

/** An embedded page's presence and how many switches the edge reported. */
function watched() {
  presence = createPresence(document, true);
  const counted = { switches: 0 };
  watchShownEdge(presence, () => {
    counted.switches += 1;
  });
  return { p: presence, counted };
}

describe("watchShownEdge", () => {
  it("reports a switch when the webhost shows the page", () => {
    // Arrange
    const { p, counted } = watched();

    // Act
    p.setShown(true);

    // Assert
    expect(counted.switches).toBe(1);
  });

  it("reports nothing when the webhost hides the page", () => {
    // Arrange
    const { p, counted } = watched();
    p.setShown(true);

    // Act
    p.setShown(false);

    // Assert
    expect(counted.switches).toBe(1);
  });

  it("reports nothing for a change that leaves the page shown", () => {
    // Arrange
    const { p, counted } = watched();
    p.setShown(true);

    // Act: the document's visibility changes while the webhost still shows it.
    document.dispatchEvent(new Event("visibilitychange"));

    // Assert
    expect(counted.switches).toBe(1);
  });

  it("reports each switch back to the page", () => {
    // Arrange
    const { p, counted } = watched();
    p.setShown(true);
    p.setShown(false);

    // Act
    p.setShown(true);

    // Assert
    expect(counted.switches).toBe(2);
  });

  it("records the switch at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { p } = watched();

    // Act
    p.setShown(true);

    // Assert
    const record = await forwardedRecord(capture, "page.switched-to");
    expect(record.level.case).toBe("info");
  });

  it("stops reporting once stopped", () => {
    // Arrange
    presence = createPresence(document, true);
    let switches = 0;
    const stopWatching = watchShownEdge(presence, () => {
      switches += 1;
    });

    // Act
    stopWatching();
    presence.setShown(true);

    // Assert
    expect(switches).toBe(0);
  });
});

describe("what a show costs in the page", () => {
  /** An edge timed on a hand clock whose frames run when the test says. */
  function timed() {
    presence = createPresence(document, true);
    const clock = { ms: 100 };
    const frames = frameQueue();
    watchShownEdge(presence, () => undefined, {
      now: () => clock.ms,
      frame: frames.frame,
    });
    return { p: presence, clock, runFrame: frames.run, pending: frames.pending };
  }

  it("records the time to the frame after the first painted one, at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { p, clock, runFrame } = timed();
    p.setShown(true);
    clock.ms += 16;
    runFrame();
    clock.ms += 17.25;

    // Act
    runFrame();

    // Assert
    const record = await forwardedRecord(capture, "page.shown-frame");
    expect([record.level.case, record.context?.["ms"]]).toEqual(["info", 33.3]);
  });

  it("records nothing until that second frame runs", () => {
    // Arrange
    const capture = captureLogRecords();
    const { p, runFrame } = timed();
    p.setShown(true);

    // Act
    runFrame();

    // Assert
    capture.logger.flush();
    expect(capture.sent.some((r) => r.operation === "page.shown-frame")).toBe(false);
  });

  it("times nothing for a hide", () => {
    // Arrange
    const { p, pending } = timed();
    p.setShown(true);
    const afterShow = pending();

    // Act
    p.setShown(false);

    // Assert
    expect(pending()).toBe(afterShow);
  });
});
