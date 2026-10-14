import { readFileSync, readdirSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { transcriptPath } from "../src/engine/vendor-paths.js";
import { testTempDir } from "./temp-dir.js";
import { sequentialIds, writeTranscript } from "./vendor-transcript.js";

describe("writeTranscript", () => {
  it("writes one JSON line per record at the vendor's transcript path", () => {
    // Arrange
    const configDir = testTempDir("vendor-transcript-");
    const lines = [{ type: "user", uuid: "u1" }, { type: "assistant", uuid: "a1" }];

    // Act
    const file = writeTranscript(configDir, "/work/space", "session-1", lines);

    // Assert
    expect(file).toBe(transcriptPath(configDir, "/work/space", "session-1"));
    expect(readFileSync(file, "utf8")).toBe(`${JSON.stringify(lines[0])}\n${JSON.stringify(lines[1])}\n`);
  });
});

describe("sequentialIds", () => {
  it("answers id-1, id-2, ... in order", () => {
    // Arrange
    const next = sequentialIds();

    // Act / Assert
    expect([next(), next(), next()]).toEqual(["id-1", "id-2", "id-3"]);
  });

  it("starts every minter afresh", () => {
    // Arrange
    sequentialIds()();

    // Act / Assert
    expect(sequentialIds()()).toBe("id-1");
  });
});

describe("the fork suites", () => {
  it("write their transcripts and mint their ids through the shared helpers", () => {
    // Arrange: every test file, read for a hand-rolled minter or a hand-rolled
    // "make the project directory, write JSON lines" transcript write.
    const root = path.resolve(import.meta.dirname);
    const files = readdirSync(root, { recursive: true, encoding: "utf8" }).filter(
      (file) => file.endsWith(".test.ts") && file !== "vendor-transcript.test.ts",
    );

    // Act
    const handRolled = files.filter((file) => {
      const source = readFileSync(path.join(root, file), "utf8");
      return (
        /function sequentialIds\(/.test(source) ||
        /transcriptPath\([^)]*\);\s*\n\s*mkdirSync\([\s\S]{0,200}?JSON\.stringify\(line\)\)\.join/.test(source)
      );
    });

    // Assert
    expect(handRolled).toEqual([]);
  });
});
