/**
 * THE SHARED SCREENSHOT DECODER (`test/webkit/pixels.ts`) TAKES THE ONLY
 * SCREENSHOT any webkit suite takes. Read off the suites' text, so it runs in
 * node with no browser.
 */
import path from "node:path";
import { readdir, readFile } from "node:fs/promises";
import { describe, expect, it } from "vitest";
import { testDir } from "../files-under.js";

const here = path.join(testDir, "webkit");

describe("screenshotPixels", () => {
  it("is the only screenshot any webkit suite takes", async () => {
    // Arrange — every webkit source but the helper itself.
    const files = (await readdir(here)).filter((f) => f.endsWith(".ts") && f !== "pixels.ts");

    // Act
    const shooting: string[] = [];
    for (const f of files) {
      if (/\.screenshot\(/.test(await readFile(path.join(here, f), "utf8"))) shooting.push(f);
    }

    // Assert
    expect(shooting).toEqual([]);
  });
});
