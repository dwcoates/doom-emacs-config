/**
 * A SENT PROMPT GLIDES UP FROM BEHIND THE FOOTER, IN WEBKIT (owner request,
 * 2026-10-20; src/feed/send-glide.ts). The bubble starts one row-height below
 * its row and rises into place, and the row's clip keeps the translated bubble
 * out of the scroll box's `scrollHeight`, so a follow during the glide never
 * lands at an inflated tail. jsdom lays nothing out and runs no animation, so
 * this measures the real engine over the real stylesheet.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { SEND_GLIDE_MS } from "../../src/feed/send-glide.js";
import type { GlideSample, SendGlidePage } from "./send-glide-page";
import { bundlePage } from "./bundle";
import { openFeedPage } from "./feed-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/** Rows enough to scroll, then the sent prompt at the tail. */
const ROWS = `${'<article class="feed-item"><div style="height: 120px"></div></article>'.repeat(6)}
  <article class="feed-item" data-feed-row="sent"><div class="bubble" style="height: 48px">sent</div></article>`;

/** How tall the sent prompt's row stands. */
const ROW_HEIGHT = 48;

describe("the sent prompt's glide in WebKit", () => {
  let browser: Browser;
  let resting: number;
  let samples: GlideSample[];

  beforeAll(async () => {
    const script = await bundlePage(path.join(here, "send-glide-page.ts"), "sendGlidePage");
    const opened = await openFeedPage({ width: 600, height: 300 });
    browser = opened.browser;
    const { page } = opened;
    await page.evaluate((rows) => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the feed shell has no #feed");
      feed.innerHTML = rows;
    }, ROWS);
    await page.addScriptTag({ content: script });
    await page.waitForFunction(() => (window as { sendGlide?: SendGlidePage }).sendGlide !== undefined);
    resting = await page.evaluate(() => window.sendGlide.restingScrollHeight());
    samples = await page.evaluate((at) => window.sendGlide.sampleDuring(at), [0, SEND_GLIDE_MS / 2, SEND_GLIDE_MS]);
  });

  afterAll(async () => {
    await browser.close();
  });

  it("starts the bubble one row-height below its row's top", () => {
    expect(samples[0]?.bubbleBelowRow).toBe(ROW_HEIGHT);
  });

  it("is partway up while it glides", () => {
    expect(samples[1]?.bubbleBelowRow).toBeGreaterThan(0);
    expect(samples[1]?.bubbleBelowRow).toBeLessThan(ROW_HEIGHT);
  });

  it("lands the bubble in its resting spot", () => {
    expect(samples[2]?.bubbleBelowRow).toBe(0);
  });

  it("never grows the scroll box's scrollHeight while it glides", () => {
    expect(samples.map((s) => s.scrollHeight)).toEqual([resting, resting, resting]);
  });
});
