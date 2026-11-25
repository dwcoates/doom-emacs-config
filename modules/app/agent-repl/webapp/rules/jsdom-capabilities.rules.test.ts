/** Every jsdom project's setup file installs the capabilities through installJsdomCapabilities (test/jsdom-capabilities.ts) alone. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { testDir } from "./files-under.js";

/** Every jsdom project's setup file. */
const SETUP_FILES = ["setup.ts", "webapp-layer/setup.ts"];

/** The single-capability installers only the shared installer may call. */
const SINGLE_INSTALLERS = [
  "installResizeObserver",
  "installIntersectionObserver",
  "installRangeClientRects",
  "installPseudoElementStyle",
];

describe("the jsdom capabilities", () => {
  it.each(SETUP_FILES)("is what %s installs", (file) => {
    // Arrange
    const source = readFileSync(path.join(testDir, file), "utf8");

    // Act
    const called = SINGLE_INSTALLERS.filter((name) => source.includes(name));

    // Assert
    expect(source).toContain("installJsdomCapabilities();");
    expect(called).toEqual([]);
  });

});
