/**
 * THE REAL STYLESHEET'S MOUNTS ARE THE ONLY ONES (`test/stylesheet.ts`): no
 * test hand-rolls the mount-and-teardown, the child-in-parent cascade probe or
 * the sidebar rail they replaced.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import STYLES_TEST from "./styles.rules.test.ts?raw";
import STYLESHEET from "../test/stylesheet.ts?raw";
import { testCodeFiles, testDir, webappRoot } from "./files-under.js";

describe("inDocument", () => {
  it("is the one mount-and-teardown the stylesheet suites share", () => {
    // Arrange: the hand-rolled shape inDocument replaced, an element removed and then the sheet torn down.
    const shape = /\.remove\(\);\s*teardown\(\);/g;

    // Act
    const sites = [STYLES_TEST.match(shape)?.length ?? 0, STYLESHEET.match(shape)?.length ?? 0];

    // Assert
    expect(sites).toEqual([0, 1]);
  });
});

describe("cascadedValueIn", () => {
  it("is the one child-in-parent cascade probe the stylesheet suite shares", () => {
    // Arrange: the hand-rolled shape cascadedValueIn replaced.
    const shape = /document\.body\.replaceChildren\(parent\)/g;

    // Act
    const sites = STYLES_TEST.match(shape)?.length ?? 0;

    // Assert
    expect(sites).toBe(0);
  });
});

describe("inRail", () => {
  it("is the only place a test builds the jsdom sidebar rail", () => {
    // Arrange: the hand-rolled shape inRail replaced.
    const shape = 'rail.id = "ws-sidebar"';
    const owners = new Set([path.join(testDir, "stylesheet.ts"), fileURLToPath(import.meta.url)]);

    // Act
    const handRolled = testCodeFiles().filter((file) => !owners.has(file) && readFileSync(file, "utf8").includes(shape));

    // Assert
    expect(handRolled.map((file) => path.relative(webappRoot, file))).toEqual([]);
  });
});
