/**
 * THE FEED'S BOTTOM STAYS PUT WHILE THE PINNED MERGE BUBBLE GROWS, IN WEBKIT.
 *
 * A running merge's bubble is the row pinned to the root feed's bottom
 * (FeedRowOrder.pinned). Its growth is growth at the tail, which a standing
 * follow keeps in view (owner ruling, 2026-10-10): a reader following the tail
 * never sees the merge bubble's bottom slide under the footer, in any painted
 * frame. While a merge bubble's growth was exempt from the follow, the tail
 * slid under the footer until the next unrelated push snapped it back.
 *
 * This runs the real stylesheet and the real scroll module in real headless
 * WebKit (pinned-tail-page.ts).
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import type { GrowthResult } from "./pinned-tail-page";
import { bootPage, pageAssets } from "./boot-page";

/** Painted frames measured after each growth: the correction lands in the first. */
const FRAMES = 5;

describe("the pinned merge bubble's growth in WebKit", () => {
  let browser: Browser;
  let page: Page;
  let merge: GrowthResult;
  let footer: GrowthResult;

  beforeAll(async () => {
    const assets = await pageAssets("pinned-tail-page.ts", "pinnedTailPage");
    browser = await webkit.launch();
    ({ page } = await bootPage(browser, { ...assets, global: "pinnedTail", viewport: { width: 900, height: 800 } }));
    await page.evaluate(() => window.pinnedTail.park());
    merge = await page.evaluate(([frames]) => window.pinnedTail.growMerge(200, frames), [FRAMES] as const);
    footer = await page.evaluate(([frames]) => window.pinnedTail.growFooter(60, frames), [FRAMES] as const);
  });

  afterAll(async () => {
    await browser.close();
  });

  it("keeps the merge bubble's bottom where it stood in every painted frame as it grows", () => {
    expect(merge.after).toEqual(Array<number>(FRAMES).fill(merge.before));
  });

  it("keeps the merge bubble's bottom where it stood in every painted frame as the footer grows", () => {
    expect(footer.after).toEqual(Array<number>(FRAMES).fill(footer.before));
  });
});
