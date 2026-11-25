/**
 * THE LAYER'S DRIVER IS ITS ONLY ONE (`test/webapp-layer/drive.ts`): no layer
 * file hand-rolls a budgeted settle loop or a ClientLog record of its own.
 */
import { readFileSync, readdirSync } from "node:fs";
import { resolve } from "node:path";
import { describe, expect, it } from "vitest";
import { testDir } from "../files-under.js";

describe("settleUntil", () => {
  it("is the only budgeted settle loop in the layer", () => {
    // Arrange
    const dir = resolve(process.cwd(), "test/webapp-layer");

    // Act: every hand-rolled loop that settles the page inside it.
    const loops = readdirSync(dir)
      .filter((name) => name.endsWith(".ts") && name !== "drive.ts" && name !== "drive.test.ts")
      .filter((name) => /for \(;;\) \{\s*await app\.settle\(/.test(readFileSync(resolve(dir, name), "utf8")));

    // Assert: a second loop is a second place for the budget's clock to drift.
    expect(loops).toEqual([]);
  });
});

describe("layerRecord", () => {
  it("is how every layer file writes a ClientLog record of its own", () => {
    // Arrange: every layer source file.
    const dir = resolve(testDir, "webapp-layer");
    const files = readdirSync(dir).filter((name) => name.endsWith(".ts") && name !== "drive.test.ts");
    // Act: a clientLog call whose record is an object literal is hand-rolled.
    const handRolled = files.filter((name) => /clientLog\(\{[\s\S]*?record:\s*\{/.test(readFileSync(resolve(dir, name), "utf8")));
    // Assert
    expect(handRolled).toEqual([]);
  });
});
