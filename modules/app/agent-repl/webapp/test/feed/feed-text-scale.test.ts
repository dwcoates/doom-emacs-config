// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  FEED_TEXT_SCALE_DEFAULT,
  FEED_TEXT_SCALE_PROPERTY,
  applyFeedTextScale,
  feedTextScale,
} from "../../src/feed/feed-text-scale.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

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

describe("feedTextScale", () => {
  afterEach(() => {
    document.documentElement.style.removeProperty(FEED_TEXT_SCALE_PROPERTY);
  });

  it("reads the stylesheet's 100% before any scale is applied", () => {
    // Act / Assert.
    expect(feedTextScale(document)).toBe(FEED_TEXT_SCALE_DEFAULT);
  });

  it("reads the scale applyFeedTextScale last wrote", () => {
    // Arrange.
    applyFeedTextScale(1.3);

    // Act / Assert.
    expect(feedTextScale(document)).toBe(1.3);
  });

  it("refuses a scale only a defect could have written, and records it", async () => {
    // Arrange.
    const capture = captureLogRecords("error");
    document.documentElement.style.setProperty(FEED_TEXT_SCALE_PROPERTY, "-2");

    // Act / Assert.
    expect(() => feedTextScale(document)).toThrow(/feed text scale unreadable/);
    const record = await forwardedRecord(capture, "feed.text-scale.unreadable");
    expect(record.context).toMatchObject({ scale: "-2" });
  });
});
