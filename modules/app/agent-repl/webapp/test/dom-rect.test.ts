// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { boxAt, boxRect, settableScrollTop } from "./dom-rect.js";

describe("boxRect", () => {
  it("derives every edge from the box it is stated by", () => {
    // Arrange
    const init = { left: 10, top: 20, width: 30, height: 40 };
    // Act
    const rect = boxRect(init);
    // Assert
    expect({ ...rect, toJSON: undefined }).toEqual({
      left: 10,
      top: 20,
      width: 30,
      height: 40,
      right: 40,
      bottom: 60,
      x: 10,
      y: 20,
      toJSON: undefined,
    });
  });

  it("states an unstated length as 0", () => {
    // Arrange
    const init = { top: 5, height: 15 };
    // Act
    const rect = boxRect(init);
    // Assert
    expect([rect.left, rect.width, rect.right, rect.bottom, rect.x, rect.y]).toEqual([0, 0, 0, 20, 0, 5]);
  });
});

describe("boxAt", () => {
  it("gives the element the box from its top down by its height, 200 wide", () => {
    // Arrange
    const el = document.createElement("div");

    // Act
    boxAt(el, 40, 20);

    // Assert
    const { top, bottom, left, width } = el.getBoundingClientRect();
    expect([top, bottom, left, width]).toEqual([40, 60, 0, 200]);
  });

  it("replaces a box given earlier", () => {
    // Arrange
    const el = document.createElement("div");
    boxAt(el, 0, 10);

    // Act
    boxAt(el, 5, 15);

    // Assert
    expect(el.getBoundingClientRect().bottom).toBe(20);
  });
});

describe("settableScrollTop", () => {
  it("starts at the value it is given", () => {
    // Arrange
    const el = document.createElement("div");
    // Act
    settableScrollTop(el, 40);
    // Assert
    expect(el.scrollTop).toBe(40);
  });

  it("keeps what is written to it, unclamped", () => {
    // Arrange
    const el = document.createElement("div");
    settableScrollTop(el);
    // Act
    el.scrollTop = 9_999;
    // Assert
    expect(el.scrollTop).toBe(9_999);
  });
});
