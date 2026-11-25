/**
 * THE HOOK-SIDE REJECTION CAPTURE (`test/webkit/rejection.ts`) IS THE ONE SHAPE
 * every webkit suite records a refusal through: none turns a rejection into a
 * value by hand. Read off the suites' text, so it runs in node with no browser.
 */
import path from "node:path";
import { readFile, readdir } from "node:fs/promises";
import { beforeAll, describe, expect, it } from "vitest";
import { testDir } from "../files-under.js";

const here = path.join(testDir, "webkit");

describe("rejectionOf", () => {
  let hand: string[];

  beforeAll(async () => {
    const names = (await readdir(here)).filter((name) => name.endsWith(".ts") && name !== "rejection.ts");
    hand = [];
    for (const name of names) {
      const src = await readFile(path.join(here, name), "utf8");
      if (/\(\) => null,\s*\(err: unknown\) => err/.test(src)) hand.push(name);
    }
  });

  it("is the only place a webkit suite turns a rejection into a value by hand", () => {
    // Arrange + Act: the hook's read of every webkit file. Assert
    expect(hand).toEqual([]);
  });
});
