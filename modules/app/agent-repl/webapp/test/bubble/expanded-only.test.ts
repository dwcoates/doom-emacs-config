// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { BUBBLE_EXPANDED_ONLY_CLASS, expandedOnly } from "../../src/bubble/expanded-only.js";

describe("expandedOnly", () => {
  it("marks the element with the expanded-only class, keeping its own", () => {
    // Arrange
    const el = document.createElement("img");
    el.className = "prompt-block";

    // Act
    expandedOnly(el);

    // Assert
    expect([...el.classList]).toEqual(["prompt-block", BUBBLE_EXPANDED_ONLY_CLASS]);
  });

  it("answers the element it marked", () => {
    const el = document.createElement("div");
    expect(expandedOnly(el)).toBe(el);
  });
});
