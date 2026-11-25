// @vitest-environment jsdom
// The pointer every drag test drives. See test/drag-pointer.ts.
import { describe, expect, it, vi } from "vitest";
import { swallowTheNextClick } from "../src/drag.js";
import { primaryPointer, settleOwedDragClick } from "./drag-pointer.js";

describe("primaryPointer", () => {
  it("dispatches a bubbling primary left-button pointer event at the height asked", () => {
    // Arrange
    const parent = document.createElement("div");
    const target = document.createElement("span");
    parent.appendChild(target);
    const seen: PointerEvent[] = [];
    parent.addEventListener("pointerdown", (event) => seen.push(event));

    // Act
    primaryPointer("pointerdown", target, 42);

    // Assert
    expect(seen.map((e) => [e.type, e.isPrimary, e.button, e.pointerId, e.clientY])).toEqual([["pointerdown", true, 0, 1, 42]]);
  });
});

describe("settleOwedDragClick", () => {
  it("takes down a guard a drag left owed, so the next test's click goes through", () => {
    // Arrange
    swallowTheNextClick(window);

    // Act
    settleOwedDragClick();

    // Assert
    const reached = vi.fn();
    document.body.addEventListener("click", reached);
    document.body.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    document.body.removeEventListener("click", reached);
    expect(reached).toHaveBeenCalledTimes(1);
  });
});
