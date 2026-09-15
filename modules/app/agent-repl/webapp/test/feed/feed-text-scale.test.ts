// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  FEED_TEXT_SCALE_PROPERTY,
  applyFeedTextScale,
} from "../../src/feed/feed-text-scale.js";

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
