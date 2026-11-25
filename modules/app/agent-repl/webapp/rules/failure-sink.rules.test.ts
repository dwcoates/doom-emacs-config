/** The tests' recording failure sinks are all recordingFailures (test/failure-sink.ts), never a hand-rolled push. */
import { readFileSync } from "node:fs";
import { relative } from "node:path";
import { describe, expect, it } from "vitest";
import { testCodeFiles, webappRoot } from "./files-under.js";

describe("the tests' recording failure sinks", () => {
  it("are all recordingFailures rather than a hand-rolled push", () => {
    // Arrange
    const handRolled = /report: \(kind\) => (?:void )?\w+\.push\(/;

    // Act
    const offenders = testCodeFiles()
      .filter((path) => handRolled.test(readFileSync(path, "utf8")))
      .map((path) => relative(webappRoot, path));

    // Assert
    expect(offenders).toEqual([]);
  });
});

