// @vitest-environment jsdom
// The pointer every drag test drives. See test/drag-pointer.ts.
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it, vi } from "vitest";
import { swallowTheNextClick } from "../src/drag.js";
import { primaryPointer, settleOwedDragClick } from "./drag-pointer.js";
import { codeOf } from "./source-text.js";

/** The test directories whose suites drag: the sidebar's and the feed's merge bubble's. */
const DRAGGING_DIRS = [join(__dirname, "sidebar"), join(__dirname, "feed", "merge")];

/** The TypeScript files directly in DIR. */
function tsFilesIn(dir: string): string[] {
  return readdirSync(dir)
    .filter((name) => name.endsWith(".ts"))
    .map((name) => join(dir, name));
}

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

  it("is the one primary pointer the drag tests build", () => {
    // Arrange
    const sources = DRAGGING_DIRS.flatMap((dir) => tsFilesIn(dir));

    // Act
    const handRolled = sources.filter((file) => /new PointerEvent\([^)]*isPrimary: true/.test(readFileSync(file, "utf8")));

    // Assert
    expect(handRolled).toEqual([]);
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

  it("is called after each test by every test file that drags", () => {
    // A file that drags without it hands its owed click to whatever test runs next.
    const offenders = DRAGGING_DIRS.flatMap((dir) => tsFilesIn(dir))
      .filter((file) => file.endsWith(".test.ts"))
      .filter((file) => {
        const code = codeOf(readFileSync(file, "utf8"));
        return /"pointermove"/.test(code) && !code.includes("settleOwedDragClick");
      });
    expect(offenders).toEqual([]);
  });
});
