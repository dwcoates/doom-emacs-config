/**
 * THE MERGE BUBBLE'S TAB STRIP STAYS IN VIEW WHILE ITS SUB-FEED SCROLLS, IN
 * WEBKIT (owner request, 2026-10-01).
 *
 * An expanded bubble scrolls its `.bubble-subfeed` panel, and the merge body
 * draws its tab strip as the first thing inside that panel. jsdom lays nothing
 * out, so this draws an expanded merge bubble over a tab body far taller than
 * the bubble's ceiling, in real headless WebKit over the real stylesheet,
 * scrolls the panel, and measures the strip against the panel's top edge.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

/** An expanded merge bubble whose selected tab holds far more than fits. */
const BUBBLE = `
  <div class="tool-card bubble-fold" data-expanded="true">
    <div class="bubble-head">merge</div>
    <div class="agent-panel bubble-subfeed" data-probe-scroller>
      <div class="feed-body">
        <div class="merge-strip" data-probe-strip>
          <ar-button role="button" class="merge-tab is-active">queue</ar-button>
          <ar-button role="button" class="merge-tab">rebasing</ar-button>
          <ar-button role="button" class="merge-tab">tests</ar-button>
        </div>
        <div class="merge-tab-summary"></div>
        <div class="merge-tab-panel"><div style="height: 4000px">content</div></div>
        <div class="merge-loose-rows"></div>
      </div>
    </div>
  </div>`;

/** The strip's and the scroller's top edges after scrolling the panel by SCROLL px. */
async function measure(page: Page, scroll: number): Promise<{ strip: number; scroller: number; scrolled: number }> {
  return page.evaluate(
    ([html, by]) => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = `<div class="feed-item" data-feed-row="r1">${html}</div>`;
      const scroller = feed.querySelector<HTMLElement>("[data-probe-scroller]");
      const strip = feed.querySelector<HTMLElement>("[data-probe-strip]");
      if (scroller === null || strip === null) throw new Error("the bubble drew no probe");
      scroller.scrollTop = by;
      return {
        strip: strip.getBoundingClientRect().top,
        scroller: scroller.getBoundingClientRect().top,
        scrolled: scroller.scrollTop,
      };
    },
    [BUBBLE, scroll] as const,
  );
}

describe("the merge tab strip in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 800, height: 800 } });
    await page.setContent(`<!doctype html><html><head></head><body>
      <div id="main-col">
        <div id="feed-scroll" class="scroll-zone">
          <main id="feed" data-feed="root"></main>
        </div>
      </div></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("sits at the sub-feed's top edge before any scroll", async () => {
    // Arrange / Act.
    const { strip, scroller } = await measure(page, 0);
    // Assert.
    expect(Math.abs(strip - scroller)).toBeLessThanOrEqual(1);
  });

  it("stays at the sub-feed's top edge after the sub-feed scrolls", async () => {
    // Arrange / Act.
    const { strip, scroller, scrolled } = await measure(page, 1500);
    // Assert.
    expect(scrolled).toBeGreaterThan(0);
    expect(Math.abs(strip - scroller)).toBeLessThanOrEqual(1);
  });
});
