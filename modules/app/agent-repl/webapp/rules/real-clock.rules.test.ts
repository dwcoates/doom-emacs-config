/** realNow and realSetImmediate (test/real-clock.ts) are the only real clock and setImmediate the suites capture. */
import { readFileSync } from "node:fs";
import { relative } from "node:path";
import { describe, expect, it } from "vitest";
import { testCodeFiles, webappRoot } from "./files-under.js";

describe("realNow", () => {
  it("is the only real clock the suites capture", () => {
    // Act: every capture of the real clock outside this module.
    const captures = testCodeFiles()
      .filter((path) => !path.endsWith("real-clock.ts") && !path.endsWith("real-clock.test.ts") && !path.endsWith("real-clock.rules.test.ts"))
      .filter((path) => readFileSync(path, "utf8").includes("performance.now.bind("))
      .map((path) => relative(webappRoot, path));

    // Assert: a second hand-rolled capture would drift from this one.
    expect(captures).toEqual([]);
  });
});

describe("realSetImmediate", () => {
  it("is the only real setImmediate the suites capture", () => {
    // Act
    const captures = testCodeFiles()
      .filter((path) => !path.endsWith("real-clock.ts") && !path.endsWith("real-clock.test.ts") && !path.endsWith("real-clock.rules.test.ts"))
      .filter((path) => /=\s*globalThis\.setImmediate\b/.test(readFileSync(path, "utf8")))
      .map((path) => relative(webappRoot, path));

    // Assert
    expect(captures).toEqual([]);
  });
});
