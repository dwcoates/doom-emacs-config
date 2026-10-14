/**
 * THE ONE FEED SHELL (feed-page.ts): the empty feed column over the real
 * stylesheet, which every webkit suite that draws into `#feed` opens rather
 * than spelling its own.
 */
import path from "node:path";
import { readdir, readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import type { Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { openFeedPage } from "./feed-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The shell's empty feed, as a suite spelling its own shell would write it. */
const EMPTY_FEED = '<main id="feed" data-feed="root"></main>';

/** What the opened page reported. */
interface Opened {
  /** The ids and classes from `#feed` up to `body`. */
  nesting: string[];
  /** A drawn tool card's corner radius: the real stylesheet's 8px. */
  cardRadius: string;
  /** A drawn tool card's padding, which the extra sheet overrides. */
  cardPadding: string;
  devicePixelRatio: number;
}

describe("the shared feed page in WebKit", () => {
  let browser: Browser;
  // THE PAGE IS THE beforeAll'S, AND THE TESTS ONLY READ IT, as every webkit
  // suite does (vitest.webkit.config.ts).
  let opened: Opened;

  beforeAll(async () => {
    let page;
    ({ browser, page } = await openFeedPage({
      width: 640,
      height: 480,
      deviceScaleFactor: 2,
      styles: [".tool-card { padding: 3px; }"],
    }));
    opened = await page.evaluate(() => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = '<div class="tool-card">card</div>';
      const card = feed.firstElementChild;
      if (card === null) throw new Error("the card did not draw");
      const nesting: string[] = [];
      for (let el: Element | null = feed; el !== null && el !== document.body; el = el.parentElement) {
        nesting.push([el.id, ...el.classList].join("."));
      }
      const cs = getComputedStyle(card);
      return { nesting, cardRadius: cs.borderTopLeftRadius, cardPadding: cs.paddingTop, devicePixelRatio: window.devicePixelRatio };
    });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("nests the feed in the app's own column", () => {
    // Arrange / Act
    const { nesting } = opened;

    // Assert
    expect(nesting).toEqual(["feed", "feed-scroll.scroll-zone", "main-col"]);
  });

  it("lays the feed out under the real stylesheet", () => {
    // Arrange / Act
    const { cardRadius } = opened;

    // Assert
    expect(cardRadius).toBe("8px");
  });

  it("adds the extra sheets after the real one, so they override it", () => {
    // Arrange / Act
    const { cardPadding } = opened;

    // Assert
    expect(cardPadding).toBe("3px");
  });

  it("opens the page at the asked device scale", () => {
    // Arrange / Act
    const { devicePixelRatio } = opened;

    // Assert
    expect(devicePixelRatio).toBe(2);
  });

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
