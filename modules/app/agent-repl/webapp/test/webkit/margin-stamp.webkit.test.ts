/**
 * THE MARGIN STAMP CENTERS IN THE FEED'S RIGHT MARGIN, in WebKit (owner
 * request, 2026-11-23). The hovered row's time and age (src/feed/margin-stamp.ts)
 * stand in the margin between the column's right rail and the feed's right
 * edge, centered across it, and centered down on the `top` the script sets.
 */
import type { Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { openFeedPage } from "./feed-page";

const ROWS = `<article class="feed-item"><div class="clear-divider" id="rail"></div></article>`;
const STAMP = `<div class="feed-margin-stamp" id="stamp" data-shown style="top: 300px"><div class="feed-margin-stamp-lines" id="lines"><div id="clock">14:03:22</div><div>5m 30s ago</div></div></div>`;

interface Measured {
  railRight: number;
  feedRight: number;
  linesLeft: number;
  linesRight: number;
  clockCenter: number;
  stampMiddle: number;
  feedTop: number;
}

describe("the margin stamp, in WebKit", () => {
  let browser: Browser;
  let measured: Measured;

  beforeAll(async () => {
    let page;
    ({ browser, page } = await openFeedPage({ width: 900, height: 700 }));
    measured = await page.evaluate(
      ([rows, stamp]) => {
        const feed = document.getElementById("feed");
        const scroll = document.getElementById("feed-scroll");
        if (feed === null || scroll === null) throw new Error("the page's shell did not mount");
        feed.innerHTML = rows;
        scroll.insertAdjacentHTML("beforeend", stamp);
        const rect = (id: string): DOMRect => {
          const el = document.getElementById(id);
          if (el === null) throw new Error(`no #${id}`);
          return el.getBoundingClientRect();
        };
        const clock = document.getElementById("clock")!;
        const range = document.createRange();
        range.selectNodeContents(clock);
        const text = range.getBoundingClientRect();
        const box = scroll.getBoundingClientRect();
        const stampRect = rect("stamp");
        return {
          railRight: rect("rail").right,
          feedRight: box.left + scroll.clientLeft + scroll.clientWidth,
          linesLeft: rect("lines").left,
          linesRight: rect("lines").right,
          clockCenter: (text.left + text.right) / 2,
          stampMiddle: (stampRect.top + stampRect.bottom) / 2,
          feedTop: box.top,
        };
      },
      [ROWS, STAMP],
    );
  });

  afterAll(async () => {
    await browser.close();
  });

  it("starts its lines' box at the column's right rail", () => {
    expect(measured.linesLeft).toBeCloseTo(measured.railRight, 0);
  });

  it("ends its lines' box at the feed's right edge", () => {
    expect(measured.linesRight).toBeCloseTo(measured.feedRight, 0);
  });

  it("centers its text across the margin", () => {
    expect(measured.clockCenter).toBeCloseTo((measured.railRight + measured.feedRight) / 2, 0);
  });

  it("centers itself down on the top the script sets", () => {
    expect(measured.stampMiddle).toBeCloseTo(measured.feedTop + 300, 0);
  });
});
