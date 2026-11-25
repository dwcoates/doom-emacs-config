/**
 * THE ONE WEBKIT PAGE BOOT (`test/webkit/boot-page.ts`): no WebKit suite boots
 * its page by hand. Read off the suites' text, so it runs in node with no
 * browser.
 */
import path from "node:path";
import { readFile, readdir } from "node:fs/promises";
import { beforeAll, describe, expect, it } from "vitest";
import { testDir } from "../files-under.js";

const here = path.join(testDir, "webkit");

describe("the one boot", () => {
  let suites: string[];
  const own: string[] = [];

  beforeAll(async () => {
    // This suite names the boot's own failure, so it is not one of them.
    suites = (await readdir(here)).filter(
      (name) => name.endsWith(".webkit.test.ts") && name !== "boot-page.webkit.test.ts",
    );
    for (const name of suites) {
      const src = await readFile(path.join(here, name), "utf8");
      if (src.includes("the page failed to boot")) own.push(name);
    }
  });

  it("is the only place a WebKit suite boots its page by hand", () => {
    // Arrange + Act: the hook's read of every suite. Assert
    expect([suites.length > 5, own]).toEqual([true, []]);
  });
});
