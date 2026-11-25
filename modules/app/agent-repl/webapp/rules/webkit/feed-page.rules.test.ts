/**
 * THE ONE FEED SHELL (`test/webkit/feed-page.ts`): no webkit suite spells its
 * own empty feed. Read off the suites' text, so it runs in node with no
 * browser.
 */
import path from "node:path";
import { readdir, readFile } from "node:fs/promises";
import { describe, expect, it } from "vitest";
import { testDir } from "../files-under.js";

const here = path.join(testDir, "webkit");

/** The shell's empty feed, as a suite spelling its own shell would write it. */
const EMPTY_FEED = '<main id="feed" data-feed="root"></main>';

describe("the shared feed page in WebKit", () => {
  it("is the only shell: no webkit suite spells its own empty feed", async () => {
    // Arrange
    const suites = (await readdir(here)).filter((f) => f.endsWith(".webkit.test.ts"));

    // Act
    const sources = await Promise.all(suites.map(async (f) => [f, await readFile(path.join(here, f), "utf8")] as const));
    const handRolled = sources.filter(([f, src]) => f !== "feed-page.webkit.test.ts" && src.includes(EMPTY_FEED)).map(([f]) => f);

    // Assert
    expect(handRolled).toEqual([]);
  });
});
