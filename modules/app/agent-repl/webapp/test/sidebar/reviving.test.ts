// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { markReviving } from "../../src/sidebar/reviving.js";

describe("marking a row reviving", () => {
  it("adds the class the shimmer keys on to the name", () => {
    // Arrange.
    const row = document.createElement("div");
    const name = document.createElement("span");

    // Act.
    markReviving(row, name);

    // Assert.
    expect(name.classList.contains("reviving")).toBe(true);
  });

  it("leaves the pass's phase to the rail's wall-clock seek", () => {
    // Arrange
    const name = document.createElement("span");

    // Act
    markReviving(document.createElement("div"), name);

    // Assert
    expect(name.style.animationDelay).toBe("");
  });

  it("stamps the row's hook attribute", () => {
    // Arrange.
    const row = document.createElement("div");

    // Act.
    markReviving(row, document.createElement("span"));

    // Assert.
    expect(row.getAttribute("data-reviving")).toBe("true");
  });
});
