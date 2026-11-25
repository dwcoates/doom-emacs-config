/**
 * The drag tests' pointer (test/drag-pointer.ts) is the only one: no drag suite
 * builds its own primary pointer, and every one settles an owed drag click.
 */
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeOf } from "../test/source-text.js";
import { testDir } from "./files-under.js";

/** The test directories whose suites drag: the sidebar's and the feed's merge bubble's. */
const DRAGGING_DIRS = [join(testDir, "sidebar"), join(testDir, "feed", "merge")];

/** The TypeScript files directly in DIR. */
function tsFilesIn(dir: string): string[] {
  return readdirSync(dir)
    .filter((name) => name.endsWith(".ts"))
    .map((name) => join(dir, name));
}

describe("primaryPointer", () => {
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
