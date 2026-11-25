/** The tests' frame seams all take frameQueue (test/frame-queue.ts), never a hand-rolled array of callbacks. */
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import { testCodeFiles } from "./files-under.js";

describe("the tests' frame seams", () => {
  it("all take frameQueue rather than a hand-rolled array of callbacks", () => {
    // Arrange
    const handRolled = /frames\s*(?:\.push\(|:\s*(?:Array<\(\) => void>|\(\(\) => void\)\[\]))/;

    // Act
    const offenders = testCodeFiles().filter((path) => handRolled.test(readFileSync(path, "utf8")));

    // Assert
    expect(offenders).toEqual([]);
  });
});

