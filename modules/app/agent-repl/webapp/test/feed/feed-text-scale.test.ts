// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  FEED_TEXT_SCALE_APPLIED,
  FEED_TEXT_SCALE_PROPERTY,
  applyFeedTextScale,
  createFeedTextScale,
} from "../../src/feed/feed-text-scale.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import type { PresenceCause } from "../../src/presence.js";

/** The current value of the feed-text-scale custom property. */
function currentScale(): string {
  return document.documentElement.style.getPropertyValue(FEED_TEXT_SCALE_PROPERTY);
}

describe("applyFeedTextScale", () => {
  afterEach(() => {
    document.documentElement.style.removeProperty(FEED_TEXT_SCALE_PROPERTY);
  });

  it("writes the scale onto the document element so all feed text re-scales", () => {
    // Act.
    applyFeedTextScale(1.5);

    // Assert.
    expect(currentScale()).toBe("1.5");
  });

  it("keeps the prior scale when handed a non-positive value", () => {
    // Arrange.
    applyFeedTextScale(1.2);

    // Act.
    applyFeedTextScale(0);

    // Assert.
    expect(currentScale()).toBe("1.2");
  });

  it("keeps the prior scale when handed a non-finite value", () => {
    // Arrange.
    applyFeedTextScale(1.2);

    // Act.
    applyFeedTextScale(Number.NaN);

    // Assert.
    expect(currentScale()).toBe("1.2");
  });
});

/** A page's presence the test moves: shown or not, embedded or not. */
function fakePresence(embedded: boolean, shown: boolean) {
  const listeners: ((seen: boolean, cause: PresenceCause) => void)[] = [];
  return {
    embedded,
    shown: () => shown,
    onChange(fn: (seen: boolean, cause: PresenceCause) => void) {
      listeners.push(fn);
      return () => {};
    },
    show() {
      shown = true;
      for (const fn of listeners) fn(true, "host");
    },
  };
}

/** Turns the test grants by hand, counting the asks. */
function fakeTurns() {
  let waiting: (() => void) | null = null;
  let asks = 0;
  return {
    request(run: () => void) {
      asks++;
      waiting = run;
    },
    grant() {
      const run = waiting;
      waiting = null;
      run?.();
    },
    asks: () => asks,
  };
}

describe("createFeedTextScale", () => {
  afterEach(() => {
    document.documentElement.style.removeProperty(FEED_TEXT_SCALE_PROPERTY);
  });

  /** A scale over the given presence and turns, with a clock that moves 7ms a read. */
  function scaleOver(presence: ReturnType<typeof fakePresence>, turns: ReturnType<typeof fakeTurns>) {
    let clock = 0;
    return createFeedTextScale({ doc: document, presence, turns, now: () => (clock += 7) });
  }

  it("lands a scale at once on a page the host shows", () => {
    // Arrange
    const scale = scaleOver(fakePresence(true, true), fakeTurns());
    // Act
    scale.apply(1.2);
    // Assert
    expect(currentScale()).toBe("1.2");
  });

  it("lands a scale at once on a page in no host", () => {
    // Arrange
    const scale = scaleOver(fakePresence(false, false), fakeTurns());
    // Act
    scale.apply(1.2);
    // Assert
    expect(currentScale()).toBe("1.2");
  });

  it("holds a hidden page's scale until its turn", () => {
    // Arrange
    const turns = fakeTurns();
    const scale = scaleOver(fakePresence(true, false), turns);
    // Act
    scale.apply(1.2);
    const before = currentScale();
    turns.grant();
    // Assert
    expect([before, currentScale()]).toEqual(["", "1.2"]);
  });

  it("lands only the latest of a hidden page's scales in its turn", () => {
    // Arrange
    const turns = fakeTurns();
    const scale = scaleOver(fakePresence(true, false), turns);
    const landed: string[] = [];
    scale.onApplied(() => landed.push(currentScale()));
    scale.apply(1.2);
    scale.apply(1.3);
    // Act
    turns.grant();
    // Assert
    expect(landed).toEqual(["1.3"]);
  });

  it("lands a hidden page's waiting scale the moment the page is shown", () => {
    // Arrange
    const presence = fakePresence(true, false);
    const turns = fakeTurns();
    const scale = scaleOver(presence, turns);
    scale.apply(1.2);
    // Act
    presence.show();
    const onShow = currentScale();
    turns.grant();
    // Assert — the later turn finds nothing left to land.
    expect([onShow, currentScale()]).toEqual(["1.2", "1.2"]);
  });

  it("lands nothing for the scale the page already shows", () => {
    // Arrange
    const scale = scaleOver(fakePresence(true, true), fakeTurns());
    const landed: string[] = [];
    scale.apply(1.2);
    scale.onApplied(() => landed.push(currentScale()));
    // Act — every sub-feed watch pushes the same value.
    scale.apply(1.2);
    // Assert
    expect(landed).toEqual([]);
  });

  it("asks a hidden page no turn for the scale it already shows", () => {
    // Arrange
    document.documentElement.style.setProperty(FEED_TEXT_SCALE_PROPERTY, "1.2");
    const turns = fakeTurns();
    const scale = scaleOver(fakePresence(true, false), turns);
    // Act
    scale.apply(1.2);
    // Assert
    expect(turns.asks()).toBe(0);
  });

  it("tells a listener after the scale lands", () => {
    // Arrange
    const scale = scaleOver(fakePresence(true, true), fakeTurns());
    const seen: string[] = [];
    scale.onApplied(() => seen.push(currentScale()));
    // Act
    scale.apply(1.4);
    // Assert
    expect(seen).toEqual(["1.4"]);
  });

  it("records a shown page's landing at info with how long it took", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const scale = scaleOver(fakePresence(true, true), fakeTurns());
    // Act
    scale.apply(1.2);
    // Assert
    const record = await forwardedRecord(capture, FEED_TEXT_SCALE_APPLIED);
    expect(record).toMatchObject({ level: { case: "info" }, context: { scale: 1.2, when: "shown", restyle_ms: 7 } });
  });

  it("records a hidden page's landing in its turn at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const turns = fakeTurns();
    const scale = scaleOver(fakePresence(true, false), turns);
    scale.apply(1.2);
    // Act
    turns.grant();
    // Assert
    const record = await forwardedRecord(capture, FEED_TEXT_SCALE_APPLIED);
    expect(record).toMatchObject({ level: { case: "debug" }, context: { scale: 1.2, when: "turn" } });
  });

  it("refuses a non-positive scale, recording it at warn and changing nothing", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const turns = fakeTurns();
    const scale = scaleOver(fakePresence(true, false), turns);
    // Act
    scale.apply(-1);
    // Assert
    const record = await forwardedRecord(capture, "feed.text-scale.invalid");
    expect([record.level.case, currentScale(), turns.asks()]).toEqual(["warn", "", 0]);
  });
});
