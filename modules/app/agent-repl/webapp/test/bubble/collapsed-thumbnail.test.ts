// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { BUBBLE_COLLAPSED_THUMBNAIL_CLASS, collapsedThumbnail } from "../../src/bubble/collapsed-thumbnail.js";

describe("collapsedThumbnail", () => {
  it("marks the element with the collapsed-thumbnail class, keeping its own", () => {
    // Arrange
    const el = document.createElement("img");
    el.className = "prompt-block";

    // Act
    collapsedThumbnail(el);

    // Assert
    expect([...el.classList]).toEqual(["prompt-block", BUBBLE_COLLAPSED_THUMBNAIL_CLASS]);
  });

  it("answers the element it marked", () => {
    const el = document.createElement("img");
    expect(collapsedThumbnail(el)).toBe(el);
  });
});
